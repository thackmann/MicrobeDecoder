#!/usr/bin/env bash
set -euo pipefail

DIR="/srv/microbedecoder-deploy/scripts"

bash "$DIR/install_docker.sh"
bash "$DIR/install_nginx.sh"
bash "$DIR/install_node_launcher.sh"
bash "$DIR/deploy_nginx_config.sh"
bash "$DIR/install_systemd_launcher.sh"
bash "$DIR/install_journald_limits.sh"
bash "$DIR/install_age_sweeper.sh"

# Jobs directory for containers
sudo mkdir -p /srv/microbedecoder/jobs
sudo chmod -R 777 /srv/microbedecoder/jobs

# Pull image (so launcher can immediately run containers / warm pool)
sudo docker pull tjhackmann/microbedecoder:latest

# Start (or restart) services
bash "$DIR/stop_everything.sh"
bash "$DIR/start_launcher.sh"
bash "$DIR/start_age_sweep.sh"

echo
echo "Deploy complete."
echo "Open:"
echo "  http://localhost/"