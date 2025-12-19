#!/usr/bin/env bash
set -euo pipefail

DEPLOY_DIR="/srv/microbedecoder-deploy"
LAUNCHER_DIR="$DEPLOY_DIR/launcher"

# Node 20
curl -fsSL https://deb.nodesource.com/setup_20.x | sudo -E bash -
sudo apt install -y nodejs

node -v
npm -v

# Install launcher deps
mkdir -p "$LAUNCHER_DIR"
cd "$LAUNCHER_DIR"

if [ ! -f package.json ]; then
  npm init -y
fi

npm install express dockerode find-free-port
