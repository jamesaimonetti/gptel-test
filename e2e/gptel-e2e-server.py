#!/usr/bin/env python3
"""gptel retry/backoff e2e fake server.

Behavior selected by PORT:

8899  url-retrieve 429 x2 then 200 (OpenAI-shaped non-stream response)
8900  curl non-stream 429 (Retry-After: 1) x2 then 200
8901  curl STREAM: attempt 1 emits partial content deltas then an Anthropic
      `event: error` (overloaded_error, HTTP 200), attempt 2 fully streams.
8902  curl non-stream, SLOW (0.4s) 200s so concurrency limiting can be
      observed; logs `request #N start/finish`.
Each request increments a global counter logged to stderr as `request #N`.
"""
import http.server
import json as _json
import socketserver
import sys
import threading
import time

ENV = {"count": 0}
LOCK = threading.Lock()

# Prompt-cache simulation (port 8903): a dict mapping the serialized
# cached prefix (system + tools + messages up to the breakpoint) to its
# byte length.  A new request reads however much of its prefix was
# already cached by an identical earlier prefix, and writes the rest
# (Anthropic's cache_read / cache_creation split).
CACHE = {}


def next_count():
    with LOCK:
        ENV["count"] += 1
        return ENV["count"]


class H(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def handle_one_request(self):  # force non-persistent: sends Connection: close
        super().handle_one_request()

    def send_bytes(self, status, headers, body):
        self.send_response(status)
        for k, v in headers:
            self.send_header(k, v)
        self.send_header("Connection", "close")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_POST(self):
        n = next_count()
        port = self.server.server_address[1]
        sys.stderr.write("request #%d (port %d)\n" % (n, port))
        sys.stderr.flush()
        try:
            self.server.handler(n, self)
        except BrokenPipeError:
            pass
        except Exception as e:  # noqa: BLE001
            sys.stderr.write("handler error: %r\n" % e)
            sys.stderr.flush()
            raise

    def log_message(self, *a):  # quiet
        pass

    @staticmethod
    def openai_429():
        body = b'{"error":{"type":"rate_limit_error","message":"slow down"}}'
        return (429, [("Content-Type", "application/json"), ("Retry-After", "1")], body)

    @staticmethod
    def openai_ok(text):
        body = ('{"choices":[{"message":{"role":"assistant","content":"%s"}}]}' % text).encode()
        return (200, [("Content-Type", "application/json")], body)


def handle_8899(n, srv):
    if n <= 2:
        st, hd, body = H.openai_429()
    else:
        st, hd, body = H.openai_ok("URL FINAL OK")
    srv.send_bytes(st, hd, body)


def handle_8900(n, srv):
    if n <= 2:
        st, hd, body = H.openai_429()
    else:
        st, hd, body = H.openai_ok("CURL FINAL OK")
    srv.send_bytes(st, hd, body)


ANTH_STREAM_FULL = (
    b'event: message_start\n'
    b'data: {"type":"message_start","message":{"id":"m","type":"message","role":"assistant"}}\n\n'
    b'event: content_block_start\n'
    b'data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}\n\n'
    b'event: content_block_delta\n'
    b'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"ANTH "}}\n\n'
    b'event: content_block_delta\n'
    b'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"FINAL OK"}}\n\n'
    b'event: content_block_stop\n'
    b'data: {"type":"content_block_stop","index":0}\n\n'
    b'event: message_delta\n'
    b'data: {"type":"message_delta","delta":{"stop_reason":"end_turn","stop_sequence":null}}\n\n'
    b'event: message_stop\n'
    b'data: {"type":"message_stop"}\n\n'
)


def handle_8901(n, srv):
    if n == 1:
        # Mid-stream error (HTTP 200): some deltas, then `event: error`.
        part = (
            b'event: message_start\n'
            b'data: {"type":"message_start","message":{"id":"m","type":"message","role":"assistant"}}\n\n'
            b'event: content_block_start\n'
            b'data: {"type":"content_block_start","index":0,"content_block":{"type":"text","text":""}}\n\n'
            b'event: content_block_delta\n'
            b'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"PARTIAL "}}\n\n'
            b'event: content_block_delta\n'
            b'data: {"type":"content_block_delta","index":0,"delta":{"type":"text_delta","text":"DIRTY"}}\n\n'
            b'event: error\n'
            b'data: {"type":"error","error":{"type":"overloaded_error","message":"overloaded"}}\n\n'
        )
        # Stream with a small delay so the client's filter notices chunks,
        # then terminate the chunked encoding properly (a bare close would
        # make curl exit 18 = partial file, which must not be retried).
        srv.send_response(200)
        srv.send_header("Content-Type", "text/event-stream")
        srv.send_header("Connection", "close")
        srv.send_header("Transfer-Encoding", "chunked")
        srv.end_headers()
        srv.wfile.write(b"%x\r\n%s\r\n0\r\n\r\n" % (len(part), part))
        srv.wfile.flush()
        time.sleep(0.15)   # let the filter run on the partial data before EOF
    else:
        srv.send_bytes(200, [("Content-Type", "text/event-stream")], ANTH_STREAM_FULL)


def handle_8902(n, srv):
    import time
    sys.stderr.write("  request #%d start\n" % n)
    sys.stderr.flush()
    time.sleep(0.4)   # hold the connection; lets us observe concurrency
    st, hd, body = H.openai_ok("RESP-%d" % n)
    srv.send_bytes(st, hd, body)
    sys.stderr.write("  request #%d finish\n" % n)
    sys.stderr.flush()


def handle_8903(n, srv):
    # Anthropic-shaped non-streaming response.  Compute usage from the
    # simulated cache table: reused prefix bytes come back as
    # cache_read_input_tokens, and every byte of the request that was
    # not already cached (including fresh input after the breakpoint)
    # is billed as input_tokens / cache_creation_input_tokens.
    import json as _json
    length = int(srv.headers.get("Content-Length", 0))
    body = b""
    while len(body) < length:
        chunk = srv.rfile.read(length - len(body))
        if not chunk:
            break
        body += chunk
    try:
        req = _json.loads(body.decode("utf-8"))
    except Exception as e:  # noqa: BLE001
        sys.stderr.write("bad request json: %r\n" % e)
        st, hd, body_err = (400, [("Content-Type", "application/json")], b'{"error":{"type":"invalid_request_error"}}')
        srv.send_bytes(st, hd, body_err)
        return
    tokens = simulate_cache(req)
    msgs = req.get("messages", [])
    cc = []
    for i, m in enumerate(msgs):
        blk = (m.get("content") or [{}])[0]
        if isinstance(blk, dict) and blk.get("cache_control"):
            cc.append("m%d:%s" % (i, blk["cache_control"].get("ttl", "?")))
    resp = _json.dumps({
        "id": "msg_%d" % n, "type": "message", "role": "assistant",
        "content": [{"type": "text", "text": "CACHE OK #%d" % n}],
        "model": req.get("model", "claude-test"),
        "stop_reason": "end_turn",
        "x_cache_layout": ",".join(cc) or "none",
        "usage": tokens,
    }).encode()
    sys.stderr.write("  layout: %s\n" % (",".join(cc) or "none"))
    sys.stderr.flush()
    log_line = _json.dumps({"messages": req.get("messages"),
                            "system": req.get("system")}, sort_keys=True)
    sys.stderr.write("  payload: %s\n" % log_line)
    sys.stderr.flush()
    hdr = "x-cache-layout: " + ",".join(cc) if cc else "x-cache-layout: none"
    srv.send_bytes(200, [("Content-Type", "application/json"),
                         (hdr.split(":", 1)[0], hdr.split(":", 1)[1].strip())],
                   resp)


def log_media_usage(msgs, sys_blocks):
    return None


def simulate_cache(req):
    """Return an Anthropic-shaped usage dict for REQ against CACHE.

    The cached prefix is system + tools + messages up to and including
    the last cache_control breakpoint.  Each request can READ the
    longest previously-written prefix that matches the start of its own
    prefix (Anthropic's multi-turn pattern: reads don't need the
    breakpoint to be in the same place, the breakpoint only determines
    what gets written), and WRITES its own breakpoint prefix if that
    exact key has not been seen before."""
    msgs = req.get("messages", [])
    sys = req.get("system")
    tools = req.get("tools", [])

    def key(end):
        # cache_control markers are not part of the content hash --
        # Anthropic's docs show a block marked one turn and unmarked the
        # next still hitting.  Strip them so the key reflects content only.
        def strip(obj):
            if isinstance(obj, dict):
                return {k: strip(v) for k, v in obj.items() if k != "cache_control"}
            if isinstance(obj, list):
                return [strip(x) for x in obj]
            return obj

        parts = []
        if sys:
            parts.append(_json.dumps(strip(sys), sort_keys=True))
        if tools:
            parts.append(_json.dumps(strip(tools), sort_keys=True))
        if end is not None:
            parts.append(_json.dumps(strip(msgs[:end]), sort_keys=True))
        return "|".join(parts)

    break_idx = None
    for i, m in enumerate(msgs):
        blk = (m.get("content") or [{}])[0]
        if isinstance(blk, dict) and blk.get("cache_control"):
            break_idx = i

    # Candidates that could be read from cache, longest first.
    candidates = [key(0)]                       # system + tools only
    if break_idx is not None:
        for k in range(1, break_idx + 2):
            candidates.append(key(k))
    read = 0
    for c in candidates:
        if c in CACHE and len(c) > read:
            read = len(c)

    # Write this request's breakpoint prefix (everything read + fresh),
    # and always ensure the system prefix is also cached so a growing
    # conversation can still hit the stable head.
    write_end = (break_idx + 1) if break_idx is not None else len(msgs)
    write_key = key(write_end)
    created = 0
    if write_key not in CACHE:
        created += len(write_key)
        CACHE[write_key] = len(write_key)
    if key(0) not in CACHE:
        created += len(key(0))
        CACHE[key(0)] = len(key(0))

    # input_tokens is what the model actually processes: everything
    # except cache reads (Anthropic counts cache creation in input).
    input_tokens = max(1, 512 + created + (len(body_bytes(req)) - read))
    return {
        "input_tokens": input_tokens,
        "output_tokens": 13,
        "cache_creation_input_tokens": created,
        "cache_read_input_tokens": read,
    }


def body_bytes(req):
    """Total request body length approximation (bytes of JSON)."""
    try:
        return _json.dumps(req).encode("utf-8")
    except Exception:  # noqa: BLE001
        return b""


if __name__ == "__main__":
    port = int(sys.argv[1]) if len(sys.argv) > 1 else 8899
    handler = globals().get("handle_%d" % port)
    if handler is None:
        sys.exit("no handler for port %d" % port)
    socketserver.TCPServer.allow_reuse_address = True
    with socketserver.TCPServer(("127.0.0.1", port), H) as httpd:
        httpd.allow_reuse_address = True
        httpd.handler = staticmethod(handler)
        httpd.serve_forever()
