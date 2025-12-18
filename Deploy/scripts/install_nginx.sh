#!/usr/bin/env bash
set -euo pipefail

sudo apt update
sudo apt install -y nginx

sudo systemctl enable nginx
sudo systemctl start nginx
sudo systemctl status nginx --no-pager || true