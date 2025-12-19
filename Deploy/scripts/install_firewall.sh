#!/usr/bin/env bash
set -euo pipefail

# Microbe Decoder - UFW firewall setup (production)
# Opens: SSH (OpenSSH), HTTP (80), HTTPS (443)
# Default: deny incoming, allow outgoing

if [[ "${EUID}" -ne 0 ]]; then
  echo "ERROR: Please run as root (use: sudo bash $0)"
  exit 1
fi

echo "==> Installing ufw (if needed)"
apt-get update -y
apt-get install -y ufw

echo "==> Setting default policies"
ufw default deny incoming
ufw default allow outgoing

echo "==> Allowing required ports/services"
ufw allow OpenSSH
ufw allow 80/tcp
ufw allow 443/tcp

echo "==> Enabling ufw (non-interactive)"
ufw --force enable

echo "==> Firewall status"
ufw status verbose
