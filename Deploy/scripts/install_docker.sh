#!/usr/bin/env bash
set -euo pipefail

sudo apt update
sudo apt install -y docker.io

sudo systemctl enable docker
sudo systemctl start docker
sudo systemctl status docker --no-pager || true

# Optional: allow your user to run docker without sudo (takes effect after re-login)
if ! groups "$USER" | grep -q '\bdocker\b'; then
  sudo usermod -aG docker "$USER" || true
  echo "NOTE: Added $USER to docker group. Log out/in for it to take effect."
fi
