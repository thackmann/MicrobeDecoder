#!/usr/bin/env bash
set -euo pipefail

echo "== curl / (expect 302 to /inst-.../ ) =="
curl -sS -I http://127.0.0.1/ | head -n 20

echo
echo "== nginx md timing log tail (if exists) =="
sudo tail -n 20 /var/log/nginx/md_timing.log 2>/dev/null || true

echo
echo "== docker containers (md-*) =="
sudo docker ps --format "table {{.Names}}\t{{.Ports}}\t{{.Status}}" | sed -n '1p;/^md-/p' || true
