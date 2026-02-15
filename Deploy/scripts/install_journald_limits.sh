#!/usr/bin/env bash
set -euo pipefail

# Cap systemd journal disk usage (persistent storage under /var/log/journal)
MAX_USE="500M"
KEEP_FREE="2G"

sudo mkdir -p /etc/systemd/journald.conf.d

sudo tee /etc/systemd/journald.conf.d/microbedecoder.conf >/dev/null <<EOF
[Journal]
SystemMaxUse=${MAX_USE}
SystemKeepFree=${KEEP_FREE}
# Optional: keep logs persistent but smaller.
# Storage=persistent
EOF

# Apply new limits
sudo systemctl restart systemd-journald

# Optional: immediately shrink current journal to the cap
sudo journalctl --vacuum-size="${MAX_USE}" >/dev/null || true

echo "journald limits installed: SystemMaxUse=${MAX_USE}, SystemKeepFree=${KEEP_FREE}"
