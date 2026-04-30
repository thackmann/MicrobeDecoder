#!/usr/bin/env bash
set -euo pipefail

# Self-elevate if not root. When invoked from deploy.sh we are already root,
# so this is a no-op (no extra pam_aad/IMDS call). When invoked directly, this
# triggers a single sudo prompt and re-execs.
if [[ "${EUID}" -ne 0 ]]; then
  exec sudo -E env "DEPLOY_USER=${SUDO_USER:-$USER}" bash "$0" "$@"
fi

# Cap systemd journal disk usage (persistent storage under /var/log/journal)
MAX_USE="500M"
KEEP_FREE="2G"

mkdir -p /etc/systemd/journald.conf.d

tee /etc/systemd/journald.conf.d/microbedecoder.conf >/dev/null <<EOF
[Journal]
SystemMaxUse=${MAX_USE}
SystemKeepFree=${KEEP_FREE}
# Optional: keep logs persistent but smaller.
# Storage=persistent
EOF

# Apply new limits
systemctl restart systemd-journald

# Optional: immediately shrink current journal to the cap
journalctl --vacuum-size="${MAX_USE}" >/dev/null || true

echo "journald limits installed: SystemMaxUse=${MAX_USE}, SystemKeepFree=${KEEP_FREE}"
