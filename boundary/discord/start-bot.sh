#!/bin/bash
cd "$(dirname "$0")"
set -a
source ../../.env.discord
set +a
node bot.js
