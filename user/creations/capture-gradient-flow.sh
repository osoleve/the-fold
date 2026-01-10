#!/bin/bash
# Capture Gradient Flow Symphony animation as GIF
#
# Uses vhs or termtosvg to capture, then converts to GIF

set -e

cd /home/oso/the-fold

echo "Recording Gradient Flow Symphony..."

# Use script to capture terminal output
script -q -c 'timeout 90 scheme --quiet << "EOF"
(load "user/creations/gradient-flow-symphony.ss")
(run-animation)
EOF' /dev/null 2>&1 | head -2000 > user/creations/gradient-flow-output.txt

echo "Captured to gradient-flow-output.txt"

# If termtosvg is available, use it for proper animation capture
if command -v termtosvg &> /dev/null; then
    echo "Generating SVG animation..."
    timeout 120 termtosvg user/creations/gradient-flow.svg -c 'scheme --quiet -e "(load \"user/creations/gradient-flow-symphony.ss\") (run-animation)"' -g 80x35
    echo "Generated gradient-flow.svg"
fi

# If asciinema is available
if command -v asciinema &> /dev/null; then
    echo "Recording with asciinema..."
    timeout 120 asciinema rec user/creations/gradient-flow.cast -c 'scheme --quiet -e "(load \"user/creations/gradient-flow-symphony.ss\") (run-animation)"' --overwrite
    echo "Generated gradient-flow.cast"

    # Convert to GIF if agg is available
    if command -v agg &> /dev/null; then
        agg user/creations/gradient-flow.cast user/creations/gradient-flow.gif
        echo "Generated gradient-flow.gif"
    fi
fi

echo "Done! Check user/creations/ for output files"
