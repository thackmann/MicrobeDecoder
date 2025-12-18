#!/usr/bin/env bash
set -euo pipefail

# Stop launcher
sudo systemctl stop microbe-launcher || true

# Kill existing containers
sudo sh -c "docker ps --format '{{.Names}}' | grep '^md-' | xargs -r docker rm -f" || true

# Restart nginx
sudo systemctl restart nginx

# Start launcher
sudo systemctl daemon-reload
sudo systemctl start microbe-launcher
sudo systemctl status microbe-launcher --no-pager -l || true