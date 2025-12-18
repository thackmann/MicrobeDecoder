#!/usr/bin/env bash
set -euo pipefail

echo "[age-sweeper] Enabling and starting md-age-sweep.timer"

sudo systemctl enable --now md-age-sweep.timer

echo "[age-sweeper] Timer active"
systemctl list-timers --no-pager | grep md-age-sweep || true