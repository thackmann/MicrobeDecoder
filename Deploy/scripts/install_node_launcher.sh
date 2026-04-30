#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DEPLOY_DIR="/srv/microbedecoder-deploy"
LAUNCHER_DIR="$DEPLOY_DIR/launcher"

# Node 20 — running as root already, no need for `sudo -E` inside the pipe
curl -fsSL https://deb.nodesource.com/setup_20.x | bash -
apt install -y nodejs

node -v
npm -v

# Install launcher deps. Run npm as the original (non-root) user so that
# node_modules and package-lock.json end up owned by them, not root.
TARGET_USER="${DEPLOY_USER:-${SUDO_USER:-root}}"

mkdir -p "$LAUNCHER_DIR"
chown -R "${TARGET_USER}:${TARGET_USER}" "$LAUNCHER_DIR"

run_as_user() {
  if [[ "${TARGET_USER}" == "root" ]]; then
    ( cd "$LAUNCHER_DIR" && "$@" )
  else
    sudo -u "${TARGET_USER}" -H bash -c "cd '$LAUNCHER_DIR' && $*"
  fi
}

if [[ ! -f "$LAUNCHER_DIR/package.json" ]]; then
  run_as_user npm init -y
fi

run_as_user npm install express dockerode find-free-port
