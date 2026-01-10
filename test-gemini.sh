#!/bin/bash
export PATH=/usr/local/bin:/usr/bin:/bin:/home/oso/.local/bin
echo "Testing gemini with cron PATH..."
gemini --version
echo "Exit code: $?"