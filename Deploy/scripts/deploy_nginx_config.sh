#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

DEPLOY_DIR="/srv/microbedecoder-deploy"
CFG_DIR="$DEPLOY_DIR/configs/nginx"
STATIC_DIR="$DEPLOY_DIR/configs/nginx/static"

# Install static assets
mkdir -p /var/www/microbedecoder/static
install -m 0644 "$STATIC_DIR/disconnected.html" /var/www/microbedecoder/disconnected.html
install -m 0644 "$STATIC_DIR/favicon.svg" /var/www/microbedecoder/static/favicon.svg
install -m 0644 "$STATIC_DIR/MicrobeDecoderLogo.svg" /var/www/microbedecoder/static/MicrobeDecoderLogo.svg
chown -R root:root /var/www/microbedecoder

# Install configs
install -m 0644 "$CFG_DIR/websocket-map.conf" /etc/nginx/conf.d/websocket-map.conf
install -m 0644 "$CFG_DIR/md-debug-log.conf" /etc/nginx/conf.d/md-debug-log.conf
install -m 0644 "$CFG_DIR/nginx.conf" /etc/nginx/nginx.conf

install -m 0644 "$CFG_DIR/microbedecoder.site" /etc/nginx/sites-available/microbedecoder

# Enable site
ln -sf /etc/nginx/sites-available/microbedecoder /etc/nginx/sites-enabled/microbedecoder
rm -f /etc/nginx/sites-enabled/default

# Cache dir for launcher cache zone
mkdir -p /var/cache/nginx/md_launcher
chown -R www-data:www-data /var/cache/nginx

# Validate and restart
nginx -t
systemctl restart nginx
