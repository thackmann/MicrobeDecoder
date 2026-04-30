#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

apt update
apt install -y docker.io

systemctl enable docker
systemctl start docker
systemctl status docker --no-pager || true

# Optional: allow the invoking user to run docker without sudo (takes effect
# after re-login). DEPLOY_USER is set by deploy.sh to the original (non-root)
# user.
TARGET_USER="${DEPLOY_USER:-${SUDO_USER:-}}"
if [[ -n "${TARGET_USER}" && "${TARGET_USER}" != "root" ]]; then
  if ! id -nG "${TARGET_USER}" 2>/dev/null | tr ' ' '\n' | grep -qx 'docker'; then
    usermod -aG docker "${TARGET_USER}" || true
    echo "NOTE: Added ${TARGET_USER} to docker group. Log out/in for it to take effect."
  fi
fi
