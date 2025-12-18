#!/usr/bin/env bash
set -euo pipefail

DIR="/srv/microbedecoder-deploy/scripts"

bash "$DIR/stop_everything.sh"
bash "$DIR/start_launcher.sh"
bash "$DIR/start_age_sweep.sh"

echo
echo "Restart complete."