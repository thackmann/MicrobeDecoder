#!/usr/bin/env bash
set -euo pipefail

# Stop launcher-managed containers older than MAX_AGE_SECONDS.
MAX_AGE_SECONDS="${MAX_AGE_SECONDS:-86400}"
NOW=$(date +%s)

count=0
stopped=0

echo "[md-sweep] Starting age sweep: threshold=${MAX_AGE_SECONDS}s now=${NOW}"

while read -r cid; do
  [ -z "$cid" ] && continue
  count=$((count + 1))

  created=$(docker inspect -f '{{.Created}}' "$cid" 2>/dev/null || true)
  if [ -z "$created" ]; then
    echo "[md-sweep] WARN: could not inspect $cid"
    continue
  fi

  created_ts=$(date -d "$created" +%s 2>/dev/null || echo 0)
  age=$((NOW - created_ts))

  if [ "$age" -ge "$MAX_AGE_SECONDS" ]; then
    echo "[md-sweep] Removing $cid (age=${age}s created=${created})"
    docker rm -f "$cid" >/dev/null 2>&1 || true
    stopped=$((stopped + 1))
  else
    echo "[md-sweep] Keeping  $cid (age=${age}s)"
  fi
done < <(docker ps -q --filter "label=md.managed=1")

echo "[md-sweep] Done. examined=${count} removed=${stopped}"