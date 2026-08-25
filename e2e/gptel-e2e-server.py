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
import socketserver
import sys
import threading
import time

ENV = {"count": 0}
LOCK = threading.Lock()


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
