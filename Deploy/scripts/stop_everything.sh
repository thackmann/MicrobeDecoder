#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh / restart.sh we are
# already root, so this is a no-op (no extra pam_aad/IMDS call). When invoked
# directly, this triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

echo "Stopping age sweeper (systemd timer/service)..."
systemctl stop md-age-sweep.timer >/dev/null 2>&1 || true
systemctl stop md-age-sweep.service >/dev/null 2>&1 || true

echo "Stopping launcher (node)..."
fuser -k 3000/tcp >/dev/null 2>&1 || true
pkill -f "node .*server\.js" >/dev/null 2>&1 || true

echo "Removing md-* containers..."
docker ps --format '{{.Names}}' | grep '^md-' | xargs -r docker rm -f || true

echo "Done."
