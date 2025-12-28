#!/bin/bash
# alert.sh — Send alerts for The Fold monitoring
#
# Currently supports:
#   - stdout (development)
#   - email via sendmail/mailx (if available)
#   - webhook (future: Discord/Slack)
#
# Usage:
#   ./alert.sh "Subject" "Message body"
#
# Configuration:
#   FOLD_ALERT_EMAIL - Email address to send alerts to
#   FOLD_ALERT_WEBHOOK - Webhook URL for alerts

set -euo pipefail

SUBJECT="${1:-Alert from The Fold}"
MESSAGE="${2:-An alert was triggered but no message was provided.}"

HOSTNAME=$(hostname)
TIMESTAMP=$(date -u +"%Y-%m-%d %H:%M:%S UTC")

# Load config if it exists
if [[ -f ~/.fold-env ]]; then
    # shellcheck disable=SC1090
    source ~/.fold-env
fi

ALERT_EMAIL="${FOLD_ALERT_EMAIL:-}"
ALERT_WEBHOOK="${FOLD_ALERT_WEBHOOK:-}"

# Format the alert
FULL_MESSAGE="
╔════════════════════════════════════════════════════╗
║          THE FOLD ALERT                            ║
╚════════════════════════════════════════════════════╝

Subject: $SUBJECT
Host:    $HOSTNAME
Time:    $TIMESTAMP

$MESSAGE

---
This is an automated alert from The Fold monitoring system.
"

# Always log to stdout
echo "$FULL_MESSAGE"

# Send email if configured
if [[ -n "$ALERT_EMAIL" ]]; then
    if command -v mail > /dev/null 2>&1; then
        echo "$FULL_MESSAGE" | mail -s "[The Fold] $SUBJECT" "$ALERT_EMAIL"
        echo "✓ Alert sent to $ALERT_EMAIL"
    elif command -v sendmail > /dev/null 2>&1; then
        echo "$FULL_MESSAGE" | sendmail "$ALERT_EMAIL"
        echo "✓ Alert sent to $ALERT_EMAIL via sendmail"
    else
        echo "⚠ Email configured but no mail command found"
    fi
fi

# Send webhook if configured
if [[ -n "$ALERT_WEBHOOK" ]]; then
    if command -v curl > /dev/null 2>&1; then
        # Generic webhook format (adjust for Discord/Slack as needed)
        PAYLOAD=$(cat <<EOF
{
  "text": "$SUBJECT",
  "attachments": [
    {
      "text": "$MESSAGE",
      "color": "danger",
      "fields": [
        {
          "title": "Host",
          "value": "$HOSTNAME",
          "short": true
        },
        {
          "title": "Time",
          "value": "$TIMESTAMP",
          "short": true
        }
      ]
    }
  ]
}
EOF
)
        curl -X POST -H 'Content-Type: application/json' \
             -d "$PAYLOAD" \
             "$ALERT_WEBHOOK" 2>/dev/null
        echo "✓ Alert sent to webhook"
    else
        echo "⚠ Webhook configured but curl not found"
    fi
fi

# If nothing is configured, just log
if [[ -z "$ALERT_EMAIL" ]] && [[ -z "$ALERT_WEBHOOK" ]]; then
    echo ""
    echo "ℹ  No alert destinations configured."
    echo "   Set FOLD_ALERT_EMAIL or FOLD_ALERT_WEBHOOK in ~/.fold-env"
fi
