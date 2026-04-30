#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh / restart.sh we are
# already root, so this is a no-op (no extra pam_aad/IMDS call). When invoked
# directly, this triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

echo "[age-sweeper] Enabling and starting md-age-sweep.timer"

systemctl enable --now md-age-sweep.timer

echo "[age-sweeper] Timer active"
systemctl list-timers --no-pager | grep md-age-sweep || true
