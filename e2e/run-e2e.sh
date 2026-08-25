#!/usr/bin/env bash
# Driver for the gptel backoff e2e smoke tests.
# Starts the fake servers, runs each elisp harness, reports results.
set -u
cd "$(dirname "$0")/../.." || exit 1
SRV="$(dirname "$0")/gptel-e2e-server.py"
LOGS="$(dirname "$0")/../.e2e-logs"
mkdir -p "$LOGS"
pkill -f "[g]ptel-e2e-server.py" 2>/dev/null
sleep 0.2
PIDS=()
for port in 8899 8900 8901 8902; do
  python3 "$SRV" "$port" > "$LOGS/server-$port.log" 2>&1 &
  PIDS+=($!)
done
sleep 0.6
fail=0

run_harness() {
  local name="$1" file="$2" want="$3"
  local out
  out=$(emacs -Q --batch -L . -l "$file" 2>&1)
  echo "=== $name ==="
  echo "$out" | grep -E "^$name" || echo "$out" | tail -20
  if echo "$out" | grep -qE "$want"; then
    echo "PASS ($name)"
  else
    echo "FAIL ($name)"
    fail=1
  fi
  echo "--- server log ---"
  local srvport="${4:-}"
  if [ -n "$srvport" ]; then
    cat "$LOGS/server-$srvport.log" 2>/dev/null | tail -8
  fi
}

run_harness "E2E-STREAM" "$(dirname "$0")/e2e-stream.el" "E2E-STREAM-CHECKS: has-final=t no-partial=t final-once=t" 8901

run_harness "E2E-URL" "$(dirname "$0")/e2e-url.el" "E2E-URL-CHECKS: final-ok=t no-error-call=t attempts-ok=t" 8899

# Limiter: queue + resume, then abort-while-queued.  Requires all three
# E2E-LIM-* lines to match (a single grep would only check the LAST one).
{
  out=$(emacs -Q --batch -L . -l "$(dirname "$0")/e2e-limiter.el" 2>&1)
  echo "$out" | grep -E "^E2E-LIM" || echo "$out" | tail -20
  if echo "$out" | grep -qE "E2E-LIM-MID: f1=WAIT f2=QUEUE active=1 queued=1" \
      && echo "$out" | grep -qE "E2E-LIM-DONE: f1=DONE f2=DONE active=0 queued=0" \
      && echo "$out" | grep -qE "E2E-LIM-ABORT-DONE: f1=DONE f2=ABRT sem-queue=0"; then
    echo "PASS (E2E-LIMITER)"
  else
    echo "FAIL (E2E-LIMITER)"
    fail=1
  fi
  echo "--- server log (8902) ---"
  tail -12 "$LOGS/server-8902.log" 2>/dev/null
}

pkill -f "[g]ptel-e2e-server.py" 2>/dev/null
exit $fail
