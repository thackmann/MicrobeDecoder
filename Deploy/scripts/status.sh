#!/usr/bin/env bash
set -euo pipefail

echo "== nginx =="
sudo systemctl status nginx --no-pager -l || true

echo
echo "== launcher port 3000 =="
sudo ss -ltnp | grep ':3000' || echo "(nothing listening on 3000)"

echo
echo "== md-* containers =="
sudo docker ps --format "table {{.Names}}\t{{.Ports}}\t{{.Status}}" | sed -n '1p;/^md-/p' || true

echo
echo "== nginx md_timing tail =="
sudo tail -n 10 /var/log/nginx/md_timing.log 2>/dev/null || true
