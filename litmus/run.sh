#!/usr/bin/env bash
# Run the Litmus WebDAV compliance test suite against SQLiteDAV.
#
# Requirements: docker, stack, sqlite3, curl.
#
# Steps:
#   1. Build (or reuse) the litmus container image.
#   2. Create an empty sqlar scratch database.
#   3. Start sqlitedav in the background pointing at the scratch database.
#   4. Run litmus inside the container against the host's sqlitedav.
#   5. Clean up the background server and scratch files on exit.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "$SCRIPT_DIR/.." && pwd)"
IMAGE_TAG="${LITMUS_IMAGE:-sqlitedav-litmus}"
PORT="${LITMUS_PORT:-1234}"
DB="$ROOT_DIR/test/litmus_scratch.sqlar"
LOG="$(mktemp -t sqlitedav-litmus.XXXXXX.log)"

SERVER_PID=""
cleanup() {
  if [ -n "$SERVER_PID" ] && kill -0 "$SERVER_PID" 2>/dev/null; then
    kill "$SERVER_PID" 2>/dev/null || true
    wait "$SERVER_PID" 2>/dev/null || true
  fi
  rm -f "$DB" "$DB-journal" "$DB-shm" "$DB-wal"
}
trap cleanup EXIT INT TERM

echo "==> Building litmus container image '$IMAGE_TAG'"
docker build --quiet --tag "$IMAGE_TAG" "$SCRIPT_DIR" >/dev/null

echo "==> Creating empty sqlar scratch database at $DB"
rm -f "$DB"
sqlite3 "$DB" \
  'CREATE TABLE sqlar(
     name TEXT PRIMARY KEY,
     mode INT,
     mtime INT,
     sz   INT,
     data BLOB
   );'

echo "==> Building sqlitedav"
( cd "$ROOT_DIR" && stack build --silent )

echo "==> Starting sqlitedav on port $PORT (log: $LOG)"
( cd "$ROOT_DIR" \
    && stack exec -- sqlitedav --port "$PORT" "$DB" \
) >"$LOG" 2>&1 &
SERVER_PID=$!

echo "==> Waiting for sqlitedav to accept connections"
for _ in $(seq 1 60); do
  # Accept any HTTP response (including 404) — the server is up the moment
  # it responds with status bytes, regardless of the status code.
  if curl -sS -o /dev/null --max-time 1 "http://localhost:$PORT/" \
       >/dev/null 2>&1; then
    break
  fi
  if ! kill -0 "$SERVER_PID" 2>/dev/null; then
    echo "!! sqlitedav exited before becoming ready. Log:" >&2
    cat "$LOG" >&2
    exit 1
  fi
  sleep 0.5
done

URL="http://host.docker.internal:$PORT/sqlar/"
echo "==> Running litmus against $URL"
# --keep-going so a single failing suite does not mask later ones; SQLiteDAV
# is not yet fully WebDAV-compliant, so the run is expected to surface
# failures.
docker run --rm \
  --add-host=host.docker.internal:host-gateway \
  "$IMAGE_TAG" \
  --keep-going \
  "$URL"
