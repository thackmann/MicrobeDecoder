#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DEPLOY_DIR="/srv/microbedecoder-deploy"

install -m 0644 \
  "$DEPLOY_DIR/systemd/microbe-launcher.service" \
  /etc/systemd/system/microbe-launcher.service

systemctl daemon-reload
systemctl enable microbe-launcher
