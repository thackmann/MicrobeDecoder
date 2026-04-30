#!/usr/bin/env bash
set -euo pipefail

# Elevate ONCE at the top to avoid pam_aad/IMDS rate-limiting (see deploy.sh
# for full explanation).
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DIR="/srv/microbedecoder-deploy/scripts"

bash "$DIR/stop_everything.sh"
bash "$DIR/start_launcher.sh"
bash "$DIR/start_age_sweep.sh"

echo
echo "Restart complete."
