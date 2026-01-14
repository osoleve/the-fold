#!/bin/bash

# tail-chat.sh — Watch The Fold chat log
# Usage: ./tail-chat.sh [lines] [--watch]
# Examples:
#   ./tail-chat.sh           # Show last 10 messages
#   ./tail-chat.sh 20        # Show last 20 messages  
#   ./tail-chat.sh 10 --watch # Show last 10 and watch for new
#   ./tail-chat.sh --watch   # Show last 10 and watch for new

set -e

# Show help
if [[ "$1" == "--help" || "$1" == "-h" ]]; then
    echo "The Fold Chat Monitor"
    echo ""
    echo "Usage: ./tail-chat.sh [lines] [--watch]"
    echo ""
    echo "Options:"
    echo "  lines       Number of recent messages to show (default: 10)"
    echo "  --watch, -w Watch for new messages continuously"
    echo "  --help, -h  Show this help message"
    echo ""
    echo "Examples:"
    echo "  ./tail-chat.sh           # Show last 10 messages"
    echo "  ./tail-chat.sh 20        # Show last 20 messages"
    echo "  ./tail-chat.sh 5 --watch # Show last 5, then watch for new"
    echo "  ./tail-chat.sh --watch   # Show last 10, then watch for new"
    echo ""
    echo "Requirements:"
    echo "  - Run from The Fold project root directory"
    echo "  - REPL daemon should be running: ./daemon.sh start"
    exit 0
fi

# Parse arguments
WATCH_MODE=false
LINES=10

for arg in "$@"; do
    case "$arg" in
        --watch|-w)
            WATCH_MODE=true
            ;;
        [0-9]*)
            LINES="$arg"
            ;;
    esac
done

# Colors for different tiers
COLOR_RESET="\033[0m"
COLOR_TIME="\033[90m"

# File to track the last chat head (to detect actual new messages)
LAST_HEAD_FILE="/tmp/tail-chat-head-$$"

# Cleanup function
cleanup() {
    rm -f "$LAST_HEAD_FILE"
}
trap cleanup EXIT

# Function to get current chat head
get_chat_head() {
    if [[ -f ".store/heads/chat.head" ]]; then
        cat ".store/heads/chat.head" | tr -d '[:space:]'
    else
        echo ""
    fi
}

# Function to display a chat message with colors
display_message() {
    local line="$1"
    
    # Parse the line format: [HH:MM] author (badge) [hash]: message
    if [[ "$line" =~ ^\s*\[([0-9:]+)\]\ ([^\ ]+)\ \(([🐑🔨🎮])\)\ \[([a-f0-9]+)\]:\ (.*) ]]; then
        local time="${BASH_REMATCH[1]}"
        local author="${BASH_REMATCH[2]}"
        local badge="${BASH_REMATCH[3]}"
        local hash="${BASH_REMATCH[4]}"
        local message="${BASH_REMATCH[5]}"
        
        # Determine color from badge
        local color="\033[1;32m"  # Default green for player
        case "$badge" in
            🐑) color="\033[1;35m" ;;  # Magenta for shepherd
            🔨) color="\033[1;34m" ;;  # Blue for builder
            🎮) color="\033[1;32m" ;;  # Green for player
        esac
        
        echo -e "${COLOR_TIME}[${time}]${COLOR_RESET} ${color}${author} ${badge}${COLOR_RESET} [${hash}]: ${message}"
        return 0
    else
        # If parsing fails, just display the line as-is
        echo "$line"
        return 0
    fi
}

# Function to get recent chat messages using direct scheme execution
get_recent_chat_direct() {
    local lines="$1"
    
    # Try direct scheme execution first (faster, no daemon overhead)
    local result
    result=$(scheme -q <<EOF 2>/dev/null
(load "thimble/repl.ss")
(display-recent-chat (mint-fs-capability ".store") $lines)
EOF
)
    
    # Check if we got output (even if there was an error, we might have chat messages)
    if [[ -n "$result" ]]; then
        # Extract just the chat lines and display them
        local chat_lines="$(echo "$result" | grep -E "^\s*\[.*\]" || true)"
        if [[ -n "$chat_lines" ]]; then
            while IFS= read -r line; do
                display_message "$line"
            done <<< "$chat_lines"
            return 0
        fi
    fi
    
    # If we got here, no chat messages were found
    return 1
}

# Function to get recent chat messages using daemon
get_recent_chat_daemon() {
    local lines="$1"
    
    # Use fold.sh to get recent chat messages
    local result
    result=$(timeout 5 ./fold.sh "(display-recent-chat (mint-fs-capability \".store\") $lines)" 2>/dev/null)
    if [[ $? -eq 0 && -n "$result" ]]; then
        # Extract just the chat lines and display them
        local chat_lines="$(echo "$result" | grep -E "^\s*\[.*\]" || true)"
        if [[ -n "$chat_lines" ]]; then
            while IFS= read -r line; do
                display_message "$line"
            done <<< "$chat_lines"
            return 0
        fi
    else
        return 1
    fi
}

# Function to get recent chat messages (tries direct first, then daemon)
get_recent_chat() {
    local lines="$1"
    
    # Try direct execution first
    if get_recent_chat_direct "$lines"; then
        return 0
    fi
    
    # Fall back to daemon
    if get_recent_chat_daemon "$lines"; then
        return 0
    fi
    
    # Both failed
    echo "❌ Error: Could not fetch chat messages"
    echo "   - Direct scheme execution failed"
    echo "   - REPL daemon not responding (try: ./daemon.sh start)"
    echo "   - Make sure you're in The Fold project root directory"
    return 1
}

# Function to watch for new messages  
watch_chat() {
    local lines="$1"
    
    echo "👁  Watching The Fold chat (press Ctrl+C to exit)..."
    echo ""
    
    # Initial display
    get_recent_chat "$lines"
    
    # Save the initial chat head
    local last_head="$(get_chat_head)"
    if [[ -n "$last_head" ]]; then
        echo "$last_head" > "$LAST_HEAD_FILE"
    fi
    
    # Watch for changes by polling the chat head
    while true; do
        sleep 3
        local current_head="$(get_chat_head)"
        
        if [[ "$current_head" != "$last_head" && -n "$current_head" ]]; then
            # New message(s) available - show just the newest ones
            echo "📝 New messages detected..."
            get_recent_chat 3  # Show recent messages including new ones
            last_head="$current_head"
            echo "$last_head" > "$LAST_HEAD_FILE"
        fi
    done
}

# Main execution
echo "🌟 The Fold Chat"
echo "===="

# Check if we're in the right directory
if [[ ! -d ".store" ]]; then
    echo "❌ Error: Must run from The Fold project root directory"
    echo "   Current directory: $(pwd)"
    exit 1
fi

# Check if scheme is available
if ! command -v scheme >/dev/null 2>&1; then
    echo "❌ Error: scheme interpreter not found"
    echo "   Make sure Chez Scheme is installed"
    exit 1
fi

# Test connection (try direct first, then daemon)
echo "Testing connection..."
if scheme -q <<EOF >/dev/null 2>&1
(load "thimble/repl.ss")
EOF
then
    echo "✅ Using direct scheme execution"
else
    echo "✅ Using REPL daemon"
fi

echo ""

if [[ "$WATCH_MODE" == true ]]; then
    watch_chat "$LINES"
else
    get_recent_chat "$LINES"
fi