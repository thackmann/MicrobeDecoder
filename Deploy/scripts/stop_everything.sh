#!/usr/bin/env bash
set -euo pipefail

echo "Stopping age sweeper (systemd timer/service)..."
sudo systemctl stop md-age-sweep.timer >/dev/null 2>&1 || true
sudo systemctl stop md-age-sweep.service >/dev/null 2>&1 || true

echo "Stopping launcher (node)..."
sudo fuser -k 3000/tcp >/dev/null 2>&1 || true
sudo pkill -f "node .*server\.js" >/dev/null 2>&1 || true

echo "Removing md-* containers..."
sudo sh -c "docker ps --format '{{.Names}}' | grep '^md-' | xargs -r docker rm -f" || true

echo "Done."
