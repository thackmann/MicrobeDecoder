#!/usr/bin/env sh
set -eu

# Define configuration paths
CONF_IN="/etc/shiny-server/shiny-server.conf"
CONF_OUT="/tmp/shiny-server.conf"

# Capture current execution context
UID_NOW="$(id -u)"
GID_NOW="$(id -g)"

# Use SHINY_RUN_AS or default to 'shiny'. 
# This name is mapped to the current UID via nss_wrapper.
USER_NAME="${SHINY_RUN_AS:-shiny}"

PASSWD_FILE="/tmp/passwd"
GROUP_FILE="/tmp/group"

# Initialize nss_wrapper for non-root environments.
# If running as root (UID 0), remapping is skipped to prevent Shiny workers from aborting.
if [ "${UID_NOW}" -ne 0 ]; then
  # Create synthetic passwd and group files based on current UID/GID
  echo "${USER_NAME}:x:${UID_NOW}:${GID_NOW}:Shiny User:/tmp:/sbin/nologin" > "${PASSWD_FILE}"
  echo "${USER_NAME}:x:${GID_NOW}:" > "${GROUP_FILE}"

  # Locate and enable nss_wrapper library
  if [ -f /usr/lib/x86_64-linux-gnu/libnss_wrapper.so ]; then
    export LD_PRELOAD="/usr/lib/x86_64-linux-gnu/libnss_wrapper.so"
  elif [ -f /usr/lib64/libnss_wrapper.so ]; then
    export LD_PRELOAD="/usr/lib64/libnss_wrapper.so"
  else
    echo "ERROR: libnss_wrapper.so not found. Please install nss_wrapper." >&2
    exit 1
  fi
  
  export NSS_WRAPPER_PASSWD="${PASSWD_FILE}"
  export NSS_WRAPPER_GROUP="${GROUP_FILE}"
fi

# Ensure the log directory exists and is writable
LOG_DIR="${SHINY_LOG_DIR:-/tmp/shiny-server-logs}"
mkdir -p "${LOG_DIR}" || true

# Generate the final configuration:
# 1. Force IPv4 (0.0.0.0) to prevent errors on IPv6-disabled clusters.
# 2. Update 'run_as' to use the mapped USER_NAME.
# 3. Direct logs to the designated writable LOG_DIR.
if [ -f "${CONF_IN}" ]; then
  sed \
    -e "s/^[[:space:]]*run_as[[:space:]].*;/run_as ${USER_NAME};/" \
    -e "s/listen \[::\]:3838;/listen 3838 0.0.0.0;/" \
    -e "s/listen 3838;/listen 3838 0.0.0.0;/" \
    -e "s/listen 0\.0\.0\.0:3838;/listen 3838 0.0.0.0;/" \
    -e "s|^[[:space:]]*log_dir[[:space:]].*;|log_dir ${LOG_DIR};|" \
    "${CONF_IN}" > "${CONF_OUT}"
else
  # Fallback to a default configuration if no base config is found
  cat > "${CONF_OUT}" <<EOF
run_as ${USER_NAME};
server {
  listen 3838 0.0.0.0;
  location / {
  location / {
    site_dir /srv/shiny-server;
    log_dir ${LOG_DIR};
    directory_index on;
  }
}
EOF
fi

# Launch xtail to pipe app logs to stdout
if [ "${APPLICATION_LOGS_TO_STDOUT:-true}" != "false" ]; then
    xtail "${LOG_DIR}" &
fi

# Launch Shiny Server
exec /usr/bin/shiny-server "${CONF_OUT}"