#!/usr/bin/env bash
# Live preview while editing: http://localhost:8080
#
#   - `dune build --watch` recompiles the site tool when OCaml changes;
#   - the preview server is restarted when content (data/), layout/,
#     site.toml or the rebuilt binary change;
#   - open pages reload themselves (they poll /__live), including on CSS/JS
#     edits under assets/, which need no restart.
#
# Ctrl-C stops everything.
set -euo pipefail
cd "$(dirname "$0")"
eval "$(opam env --switch=webpage --set-switch)"

dune build ./site/main.exe
dune build --watch ./site/main.exe >/dev/null 2>&1 &
watch_pid=$!

server_pid=""
start() { ./_build/default/site/main.exe serve "$@" & server_pid=$!; }
stop() { [ -n "$server_pid" ] && kill "$server_pid" 2>/dev/null && wait "$server_pid" 2>/dev/null || true; }
trap 'stop; kill $watch_pid 2>/dev/null; exit 0' INT TERM

fingerprint() {
  # (main.exe briefly disappears while dune relinks it; that must not stop us)
  find data layout site.toml _build/default/site/main.exe -type f -exec stat -f '%m %N' {} + 2>/dev/null | sort | shasum || true
}

start "$@"
last=$(fingerprint)
while sleep 0.7; do
  now=$(fingerprint)
  if [ "$now" != "$last" ]; then
    last=$now
    echo "dev.sh: change detected, restarting server"
    stop
    start "$@"
  fi
done
