#!/usr/bin/env bash
set -euo pipefail

DEPLOY_DIR="/srv/microbedecoder-deploy"

sudo install -m 0644 \
  "$DEPLOY_DIR/systemd/microbe-launcher.service" \
  /etc/systemd/system/microbe-launcher.service

sudo systemctl daemon-reload
sudo systemctl enable microbe-launcher