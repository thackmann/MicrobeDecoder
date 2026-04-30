#!/usr/bin/env bash
set -euo pipefail

# This host uses pam_aad for sudo, which calls Azure IMDS on EVERY sudo
# invocation. A long chain of small sudo calls gets rate-limited (HTTP 429),
# which causes pam_aad to fail open mid-deploy with
# "PAM account management error: Unknown error -1".
#
# Fix: elevate ONCE at the top, then run everything as root. Sub-scripts no
# longer call sudo internally.
if [[ "${EUID}" -ne 0 ]]; then
  # Preserve the invoking user so install_node_launcher can drop back down
  # for `npm install` (root-owned node_modules is a footgun).
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DIR="/srv/microbedecoder-deploy/scripts"

bash "$DIR/install_docker.sh"
bash "$DIR/install_nginx.sh"
bash "$DIR/install_node_launcher.sh"
bash "$DIR/deploy_nginx_config.sh"
bash "$DIR/install_systemd_launcher.sh"
bash "$DIR/install_journald_limits.sh"
bash "$DIR/install_age_sweeper.sh"

# Jobs directory for containers
mkdir -p /srv/microbedecoder/jobs
chmod -R 777 /srv/microbedecoder/jobs

# Pull image (so launcher can immediately run containers / warm pool)
docker pull tjhackmann/microbedecoder:latest

# Start (or restart) services
bash "$DIR/stop_everything.sh"
bash "$DIR/start_launcher.sh"
bash "$DIR/start_age_sweep.sh"

echo
echo "Deploy complete."
echo "Open:"
echo "  http://localhost/"
