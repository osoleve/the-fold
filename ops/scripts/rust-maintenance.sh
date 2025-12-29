#!/bin/bash

# Rust maintenance cron job script
# This script runs cargo udeps, cargo audit, and cargo update for the fold-rs project

LOG_FILE="/home/oso/the-fold/logs/cargo-maintenance.log"
PROJECT_DIR="/home/oso/the-fold/fold-rs"

# Create logs directory if it doesn't exist
mkdir -p "$(dirname "$LOG_FILE")"

# Function to log with timestamp
log() {
    echo "[$(date '+%Y-%m-%d %H:%M:%S')] $1" >> "$LOG_FILE"
}

log "Starting Rust maintenance tasks"

# Change to project directory
cd "$PROJECT_DIR" || {
    log "ERROR: Failed to change to project directory $PROJECT_DIR"
    exit 1
}

# Run cargo udeps
log "Running cargo udeps..."
if cargo udeps >> "$LOG_FILE" 2>&1; then
    log "cargo udeps completed successfully"
else
    log "ERROR: cargo udeps failed"
fi

# Run cargo audit
log "Running cargo audit..."
if cargo audit >> "$LOG_FILE" 2>&1; then
    log "cargo audit completed successfully"
else
    log "ERROR: cargo audit failed"
fi

# Run cargo update
log "Running cargo update..."
if cargo update >> "$LOG_FILE" 2>&1; then
    log "cargo update completed successfully"
else
    log "ERROR: cargo update failed"
fi

log "Rust maintenance tasks completed"
echo "----------------------------------------" >> "$LOG_FILE"