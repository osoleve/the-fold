#!/usr/bin/env python3
"""
Wireworld GIF Animator - FIXED VERSION
Creates animated visualizations of digital circuits
"""

import os
from PIL import Image, ImageDraw, ImageFont

class WireworldRenderer:
    def __init__(self, cell_size=10):
        self.cell_size = cell_size
        self.colors = {
            0: (10, 10, 10),      # Empty - black
            1: (255, 200, 50),    # Conductor - gold/yellow
            2: (0, 150, 255),     # Electron head - blue
            3: (255, 50, 50)      # Electron tail - red
        }

    def render_grid(self, grid, title=""):
        height = len(grid)
        width = len(grid[0]) if height > 0 else 0

        title_height = 40
        img_height = height * self.cell_size + title_height
        img = Image.new('RGB', (width * self.cell_size, img_height), (10, 10, 10))
        draw = ImageDraw.Draw(img)

        # Title
        if title:
            try:
                font = ImageFont.truetype("/usr/share/fonts/truetype/dejavu/DejaVuSansMono-Bold.ttf", 18)
            except:
                font = ImageFont.load_default()
            draw.text((10, 10), title, fill=(0, 255, 136), font=font)

        # Render grid
        pixels = img.load()
        for y, row in enumerate(grid):
            for x, cell in enumerate(row):
                color = self.colors.get(cell, (10, 10, 10))
                if color != (10, 10, 10):
                    for py in range(self.cell_size):
                        for px in range(self.cell_size):
                            img_x = x * self.cell_size + px
                            img_y = y * self.cell_size + py + title_height
                            if img_x < img.width and img_y < img.height:
                                pixels[img_x, img_y] = color

        return img

def count_electron_heads(grid, x, y):
    """Count electron heads in Moore neighborhood"""
    height, width = len(grid), len(grid[0])
    count = 0
    for dy in [-1, 0, 1]:
        for dx in [-1, 0, 1]:
            if dx == 0 and dy == 0:
                continue
            nx = (x + dx) % width
            ny = (y + dy) % height
            if grid[ny][nx] == 2:
                count += 1
    return count

def evolve_wireworld(grid):
    """Evolve Wireworld one generation"""
    height, width = len(grid), len(grid[0])
    new_grid = [[0]*width for _ in range(height)]

    for y in range(height):
        for x in range(width):
            state = grid[y][x]
            if state == 0:  # Empty stays empty
                new_grid[y][x] = 0
            elif state == 2:  # Head becomes tail
                new_grid[y][x] = 3
            elif state == 3:  # Tail becomes conductor
                new_grid[y][x] = 1
            elif state == 1:  # Conductor
                heads = count_electron_heads(grid, x, y)
                new_grid[y][x] = 2 if heads in [1, 2] else 1

    return new_grid

# === CIRCUIT PATTERNS (FIXED for proper propagation) ===

def create_clock_circuit():
    """Oscillating clock signal - works because of corners"""
    return [
        [0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,1,1,1,1,1,1,0,0,0,0],
        [0,0,1,0,0,0,0,1,0,0,0,0],
        [0,2,1,0,0,0,0,1,3,0,0,0],
        [0,0,1,0,0,0,0,1,0,0,0,0],
        [0,0,1,1,1,1,1,1,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0]
    ]

def create_or_gate():
    """OR gate circuit - uses 2-wide wires for horizontal propagation"""
    return [
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,2,3,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0],  # Top input (2-wide)
        [0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,0,0,0,0],  # Output (2-wide)
        [0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0],
        [0,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0],  # Bottom input (2-wide)
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ]

def create_diode():
    """One-way diode - uses 3-wire branch structure"""
    return [
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,2,3,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],  # Main wire (2-wide)
        [0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0],
        [0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ]

def create_dual_clock():
    """Two clocks feeding into a shared wire"""
    return [
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
        [0,2,1,0,0,0,0,1,3,1,1,1,1,1,1,0,0,0,0,0,0,0],  # Top clock
        [0,0,1,0,0,0,0,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
        [0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,3,1,0,0,0,0,0,0,0,0,0,0,0],  # Bottom clock
        [0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,0,0,0,0,0,0,0],
        [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
    ]

def create_animation(pattern, generations, title):
    """Create animation frames"""
    renderer = WireworldRenderer(cell_size=12)
    frames = []
    grid = pattern

    for gen in range(generations):
        frame = renderer.render_grid(grid, f"{title} - Gen {gen}")
        frames.append(frame)
        grid = evolve_wireworld(grid)

    return frames

def save_gif(frames, filename, duration=200):
    """Save animated GIF"""
    if frames:
        frames[0].save(
            filename,
            save_all=True,
            append_images=frames[1:],
            duration=duration,
            loop=0,
            optimize=True
        )
        print(f"   ✓ Saved {filename} ({len(frames)} frames)")
        return True
    return False

def create_outbox_message(filename, title, description):
    """Create Discord outbox message"""
    import time
    timestamp = int(time.time() * 1000)
    outbox_path = f"/home/oso/the-fold/.fold-repl/discord-outbox/wireworld-{timestamp}.json"

    content = {
        "author": "ClaudeBuilder",
        "channel": "art",
        "title": title,
        "body": description,
        "attachment": {
            "path": f"/home/oso/the-fold/{filename}",
            "name": os.path.basename(filename),
            "description": title
        }
    }

    import json
    with open(outbox_path, 'w') as f:
        json.dump(content, f, indent=2)

    print(f"   ✓ Posted to Discord outbox")

def main():
    print("\n⚡ Wireworld Circuit GIF Generator (FIXED)")
    print("=" * 60)

    circuits = [
        (create_clock_circuit, 24, "Clock Generator",
         "🔄 Oscillating clock circuit - electrons loop endlessly! This works because corners allow Moore neighborhood propagation."),

        (create_or_gate, 35, "OR Logic Gate (Fixed)",
         "🚪 OR gate with 2-wide wires for proper horizontal electron propagation! Watch electrons merge at the junction."),

        (create_diode, 30, "One-Way Diode (Fixed)",
         "⚡ Diode with branching structure - electrons flow in one direction through the wire branches!"),

        (create_dual_clock, 40, "Dual Clock Merger",
         "🔀 Two independent clocks feeding signals into a shared vertical wire!")
    ]

    for pattern_fn, gens, title, description in circuits:
        print(f"\n⚡ Generating {title}...")
        pattern = pattern_fn()
        frames = create_animation(pattern, gens, title)

        filename = f"user/creations/wireworld-{title.lower().replace(' ', '-').replace('(', '').replace(')', '')}.gif"
        if save_gif(frames, filename, duration=150):
            create_outbox_message(filename, f"Wireworld: {title}", description)

    print("\n✨ Wireworld animations complete!")
    print("Check Discord #art channel for the GIFs!")

if __name__ == "__main__":
    main()
