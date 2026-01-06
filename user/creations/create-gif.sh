#!/bin/bash
# Create animated GIF from text frames

set -e

FRAMES_DIR="user/creations/frames"
OUTPUT="user/creations/mountain-dreams.gif"

echo "Converting text frames to images..."

# Create a temporary directory for PNG frames
TMP_DIR=$(mktemp -d)
trap "rm -rf $TMP_DIR" EXIT

# Convert each text frame to a PNG image
for frame in $FRAMES_DIR/frame-*.txt; do
    basename=$(basename "$frame" .txt)
    png_file="$TMP_DIR/${basename}.png"

    # Read the frame content and render it as an image
    # Use a monospace font and render as text
    convert -size 900x300 xc:black \
            -font "DejaVu-Sans-Mono" -pointsize 10 \
            -fill white \
            -annotate +10+15 "$(cat "$frame")" \
            "$png_file"
done

echo "Creating animated GIF..."

# Combine all PNGs into an animated GIF
# -delay 8 = 80ms between frames (12.5 FPS)
# -loop 0 = loop forever
convert -delay 8 -loop 0 $TMP_DIR/frame-*.png "$OUTPUT"

echo "Created: $OUTPUT"
ls -lh "$OUTPUT"
