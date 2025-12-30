#!/usr/bin/env python3
"""Remove duplicate set extension function definitions from missing.ss."""

import re
from pathlib import Path

# Functions now canonical in set-ext.ss
SET_EXT_FUNCTIONS = {
    'union', 'intersection', 'difference', 'symmetric-difference',
    'subset?', 'disjoint?', 'set-union', 'set-intersection',
    'set-difference', 'set-symmetric-difference', 'set-subset?',
    'set-equal?', 'set-superset?'
}

def find_function_blocks(content):
    """Find all function definition blocks and their positions."""
    lines = content.split('\n')
    blocks = []
    i = 0

    while i < len(lines):
        line = lines[i]
        # Match function definitions (including aliases)
        match = re.match(r'^\(([a-z][a-z0-9?!-]*)\s+(\(fn|\(fix|[a-z])', line)
        if match:
            funcname = match.group(1)
            if funcname in SET_EXT_FUNCTIONS:
                # Find the closing paren for this definition
                start = i
                paren_depth = 0
                for char in line:
                    if char == '(':
                        paren_depth += 1
                    elif char == ')':
                        paren_depth -= 1

                end = i
                while paren_depth > 0 and end < len(lines) - 1:
                    end += 1
                    for char in lines[end]:
                        if char == '(':
                            paren_depth += 1
                        elif char == ')':
                            paren_depth -= 1
                        if paren_depth == 0:
                            break

                blocks.append({
                    'name': funcname,
                    'start': start,
                    'end': end,
                    'lines': lines[start:end+1]
                })
                i = end + 1
                continue
        i += 1

    return blocks

def main():
    prelude_dir = Path(__file__).parent
    missing_path = prelude_dir / 'missing.ss'

    with open(missing_path, 'r', encoding='utf-8') as f:
        content = f.read()

    blocks = find_function_blocks(content)

    # Group by function name
    by_name = {}
    for block in blocks:
        name = block['name']
        if name not in by_name:
            by_name[name] = []
        by_name[name].append(block)

    # Identify all occurrences to remove
    to_remove = []
    for name, occurrences in by_name.items():
        print(f"Found {len(occurrences)} definitions of {name}")
        # Remove ALL occurrences (they're now in set-ext.ss)
        for block in occurrences:
            to_remove.append(block)

    if not to_remove:
        print("No duplicates found!")
        return

    # Sort by start line (descending) so we can remove from end to beginning
    to_remove.sort(key=lambda b: b['start'], reverse=True)

    lines = content.split('\n')
    removed_count = 0

    for block in to_remove:
        start = block['start']
        end = block['end']

        # Check if there are comment lines immediately before
        comment_start = start
        while comment_start > 0 and lines[comment_start - 1].strip().startswith(';'):
            comment_start -= 1

        # Remove the block (including preceding comments)
        print(f"Removing {block['name']} at lines {comment_start}-{end}")
        del lines[comment_start:end+1]
        removed_count += 1

        # Remove any blank lines left behind
        while comment_start < len(lines) and lines[comment_start].strip() == '':
            del lines[comment_start]

    # Write back
    new_content = '\n'.join(lines)
    with open(missing_path, 'w', encoding='utf-8') as f:
        f.write(new_content)

    print(f"\nRemoved {removed_count} set extension function definitions")
    print(f"Cleaned up {len(by_name)} unique functions")

if __name__ == '__main__':
    main()
