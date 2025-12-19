#!/usr/bin/env bash
set -euo pipefail

# Microbe Decoder - TLS / HTTPS setup using Let's Encrypt + certbot
# Requires:
#   - nginx already installed and running
#   - DNS A record(s) pointing domain(s) -> this server's public IP
#
# Usage:
#   sudo bash 07_install_tls_cert.sh dev.microbe-decoder.org admin@microbe-decoder.org

if [[ "${EUID}" -ne 0 ]]; then
  echo "ERROR: Please run as root (use: sudo bash $0 DOMAIN EMAIL)"
  exit 1
fi

DOMAIN="${1:-}"
EMAIL="${2:-}"

if [[ -z "$DOMAIN" || -z "$EMAIL" ]]; then
  echo "Usage: sudo bash $0 <domain> <email>"
  echo "Example:"
  echo "  sudo bash $0 dev.microbe-decoder.org admin@microbe-decoder.org"
  exit 1
fi

# If user passes "www.dev.example.org", make base "dev.example.org"
BASE_DOMAIN="${DOMAIN#www.}"

echo "==> Installing certbot and nginx plugin"
apt-get update -y
apt-get install -y certbot python3-certbot-nginx

echo "==> Verifying nginx is running"
systemctl is-active --quiet nginx || {
  echo "ERROR: nginx is not running"
  exit 1
}

echo "==> Requesting TLS certificate for ${BASE_DOMAIN} and www.${BASE_DOMAIN}"
certbot --nginx \
  -d "${BASE_DOMAIN}" \
  -d "www.${BASE_DOMAIN}" \
  --non-interactive \
  --agree-tos \
  --email "${EMAIL}" \
  --redirect \
  --expand

echo "==> Enabling automatic certificate renewal"
systemctl enable certbot.timer
systemctl start certbot.timer

echo "==> Testing renewal (dry run)"
certbot renew --dry-run

echo "==> TLS setup complete"