#!/bin/bash
# install-services.sh — Install systemd user services for The Fold
#
# This script installs all systemd services and timers to ~/.config/systemd/user/
# and enables them.
#
# Usage:
#   ./install-services.sh           # Install and enable all
#   ./install-services.sh --dry-run # Show what would be done

set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
SYSTEMD_USER_DIR="${HOME}/.config/systemd/user"
DRY_RUN=0

if [[ "${1:-}" == "--dry-run" ]]; then
    DRY_RUN=1
fi

GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() {
    echo -e "${GREEN}[INSTALL]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

do_cmd() {
    if [[ $DRY_RUN -eq 1 ]]; then
        echo "  [DRY-RUN] $*"
    else
        "$@"
    fi
}

echo ""
echo "╔════════════════════════════════════════════════════╗"
echo "║     THE FOLD SYSTEMD SERVICE INSTALLER             ║"
echo "╚════════════════════════════════════════════════════╝"
echo ""

if [[ $DRY_RUN -eq 1 ]]; then
    warn "DRY-RUN MODE - No changes will be made"
    echo ""
fi

# Create systemd user directory if it doesn't exist
log "Creating systemd user directory..."
do_cmd mkdir -p "$SYSTEMD_USER_DIR"

# Install service files
log "Installing service files..."
for service_file in "${FOLD_ROOT}"/ops/systemd/user/*.service; do
    if [[ -f "$service_file" ]]; then
        SERVICE_NAME=$(basename "$service_file")
        log "  Installing $SERVICE_NAME"
        do_cmd cp "$service_file" "$SYSTEMD_USER_DIR/"
    fi
done

# Install timer files
log "Installing timer files..."
for timer_file in "${FOLD_ROOT}"/ops/systemd/user/*.timer; do
    if [[ -f "$timer_file" ]]; then
        TIMER_NAME=$(basename "$timer_file")
        log "  Installing $TIMER_NAME"
        do_cmd cp "$timer_file" "$SYSTEMD_USER_DIR/"
    fi
done

# Reload systemd
log "Reloading systemd daemon..."
do_cmd systemctl --user daemon-reload

# Enable and start services
log "Enabling the-fold-daemon.service..."
do_cmd systemctl --user enable the-fold-daemon.service

if systemctl --user is-active the-fold-daemon.service > /dev/null 2>&1; then
    log "  Daemon already running, restarting..."
    do_cmd systemctl --user restart the-fold-daemon.service
else
    log "  Starting daemon..."
    do_cmd systemctl --user start the-fold-daemon.service
fi

# Enable timers
log "Enabling timers..."
for timer in the-fold-tests.timer the-fold-cleanup.timer; do
    log "  Enabling $timer"
    do_cmd systemctl --user enable "$timer"
    do_cmd systemctl --user start "$timer"
done

# Show status
echo ""
log "Installation complete!"
echo ""

if [[ $DRY_RUN -eq 0 ]]; then
    log "Service status:"
    systemctl --user status the-fold-daemon.service --no-pager -l || true
    echo ""

    log "Timer status:"
    systemctl --user list-timers 'the-fold*' --no-pager || true
    echo ""

    log "To view logs:"
    echo "  journalctl --user -u the-fold-daemon.service -f"
    echo ""

    log "To restart daemon:"
    echo "  systemctl --user restart the-fold-daemon.service"
fi

echo ""
