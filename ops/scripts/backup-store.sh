#!/bin/bash
# backup-store.sh — Backup .store to git (homoiconic backup strategy)
#
# This backs up the content-addressed store using git itself.
# Since blocks are immutable, git is perfect for this.
#
# Usage:
#   ./backup-store.sh           # Interactive backup
#   ./backup-store.sh --auto    # Automated (for cron/systemd)

set -euo pipefail

FOLD_ROOT="${FOLD_ROOT:-$(cd "$(dirname "$0")/../.." && pwd)}"
cd "$FOLD_ROOT"

AUTO_MODE=0
if [[ "${1:-}" == "--auto" ]]; then
    AUTO_MODE=1
fi

# Colors for interactive mode
if [[ $AUTO_MODE -eq 0 ]]; then
    GREEN='\033[0;32m'
    YELLOW='\033[1;33m'
    NC='\033[0m'
else
    GREEN=''
    YELLOW=''
    NC=''
fi

log() {
    echo -e "${GREEN}[BACKUP]${NC} $*"
}

warn() {
    echo -e "${YELLOW}[WARN]${NC} $*"
}

# Check if we're in a git repo
if ! git rev-parse --git-dir > /dev/null 2>&1; then
    warn "Not in a git repository"
    exit 1
fi

# Get stats before backup
STORE_SIZE=$(du -sh .store 2>/dev/null | cut -f1)
OBJECT_COUNT=$(find .store/objects -type f 2>/dev/null | wc -l)

log "Store size: $STORE_SIZE, Objects: $OBJECT_COUNT"

# Check if there are changes
if git diff --quiet .store/ 2>/dev/null && \
   ! git ls-files --others --exclude-standard .store/ | grep -q .; then
    log "No changes to backup"
    exit 0
fi

# Stage the store
log "Staging .store/ for backup..."
git add .store/

# Create commit message
TIMESTAMP=$(date -u +"%Y-%m-%d %H:%M:%S UTC")
COMMIT_MSG="Backup .store: $OBJECT_COUNT objects, $STORE_SIZE

Automated backup of content-addressed store.
Timestamp: $TIMESTAMP
Objects: $OBJECT_COUNT
Size: $STORE_SIZE

🤖 Automated backup"

# Commit
log "Creating backup commit..."
if git commit -m "$COMMIT_MSG" --no-verify; then
    COMMIT_HASH=$(git rev-parse --short HEAD)
    log "Backup committed: $COMMIT_HASH"

    # Push to remote (if configured)
    if git remote get-url origin > /dev/null 2>&1; then
        if [[ $AUTO_MODE -eq 1 ]]; then
            log "Pushing to remote..."
            if git push origin main 2>&1; then
                log "Backup pushed successfully"
            else
                warn "Push failed (will retry next time)"
            fi
        else
            echo ""
            echo "To push backup to remote, run:"
            echo "  git push origin main"
        fi
    fi
else
    warn "Commit failed (maybe nothing to commit?)"
    exit 1
fi

log "Backup complete"
