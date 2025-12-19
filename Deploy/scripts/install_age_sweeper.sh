#!/usr/bin/env bash
set -euo pipefail

DEPLOY_DIR="/srv/microbedecoder-deploy"

echo "[age-sweeper] Installing sweep script and systemd units (not starting)"

# Install sweep script
sudo install -m 0755 \
  "$DEPLOY_DIR/scripts/md-age-sweep.sh" \
  /usr/local/bin/md-age-sweep.sh

# Install systemd service
sudo install -m 0644 \
  "$DEPLOY_DIR/systemd/md-age-sweep.service" \
  /etc/systemd/system/md-age-sweep.service

# Install systemd timer
sudo install -m 0644 \
  "$DEPLOY_DIR/systemd/md-age-sweep.timer" \
  /etc/systemd/system/md-age-sweep.timer

# Reload systemd to recognize new units
sudo systemctl daemon-reload

echo "[age-sweeper] Installed. Timer is NOT enabled or started."
