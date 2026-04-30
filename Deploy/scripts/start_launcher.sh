#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh / restart.sh we are
# already root, so this is a no-op (no extra pam_aad/IMDS call). When invoked
# directly, this triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

# Stop launcher
systemctl stop microbe-launcher || true

# Kill existing containers
docker ps --format '{{.Names}}' | grep '^md-' | xargs -r docker rm -f || true

# Restart nginx
systemctl restart nginx

# Start launcher
systemctl daemon-reload
systemctl start microbe-launcher
systemctl status microbe-launcher --no-pager -l || true
