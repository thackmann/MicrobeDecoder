#!/usr/bin/env bash
set -euo pipefail

DEPLOY_DIR="/srv/microbedecoder-deploy"
CFG_DIR="$DEPLOY_DIR/configs/nginx"

# Install configs
sudo install -m 0644 "$CFG_DIR/websocket-map.conf" /etc/nginx/conf.d/websocket-map.conf
sudo install -m 0644 "$CFG_DIR/md-debug-log.conf" /etc/nginx/conf.d/md-debug-log.conf
sudo install -m 0644 "$CFG_DIR/nginx.conf" /etc/nginx/nginx.conf

sudo install -m 0644 "$CFG_DIR/microbedecoder.site" /etc/nginx/sites-available/microbedecoder

# Enable site
sudo ln -sf /etc/nginx/sites-available/microbedecoder /etc/nginx/sites-enabled/microbedecoder
sudo rm -f /etc/nginx/sites-enabled/default

# Cache dir for launcher cache zone
sudo mkdir -p /var/cache/nginx/md_launcher
sudo chown -R www-data:www-data /var/cache/nginx

# Validate and restart
sudo nginx -t
sudo systemctl restart nginx
