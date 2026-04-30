#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DEPLOY_DIR="/srv/microbedecoder-deploy"

echo "[age-sweeper] Installing sweep script and systemd units (not starting)"

# Install sweep script
install -m 0755 \
  "$DEPLOY_DIR/scripts/md-age-sweep.sh" \
  /usr/local/bin/md-age-sweep.sh

# Install systemd service
install -m 0644 \
  "$DEPLOY_DIR/systemd/md-age-sweep.service" \
  /etc/systemd/system/md-age-sweep.service

# Install systemd timer
install -m 0644 \
  "$DEPLOY_DIR/systemd/md-age-sweep.timer" \
  /etc/systemd/system/md-age-sweep.timer

# Reload systemd to recognize new units
systemctl daemon-reload

echo "[age-sweeper] Installed. Timer is NOT enabled or started."
