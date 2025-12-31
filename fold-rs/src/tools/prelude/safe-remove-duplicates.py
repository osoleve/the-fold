#!/usr/bin/env python3
"""
Safely remove definitions from missing.ss that:
1. Exist in other prelude files (loaded before missing.ss)
2. Are NOT used by other definitions within missing.ss

This avoids breaking internal dependencies.
"""

import os
import re
from pathlib import Path
from collections import defaultdict

def find_top_level_defs(filepath):
    """
    Find all top-level binding names and their spans in a .ss file.
    Returns list of (name, start_line, end_line, body_text).
    """
    definitions = []

    with open(filepath, 'r') as f:
        content = f.read()
        lines = content.split('\n')

    line_idx = 0
    while line_idx < len(lines):
        line = lines[line_idx]
        stripped = line.strip()
        if stripped.startswith(';') or not stripped:
            line_idx += 1
            continue

        # Match: (name (fn ...)) or (name (fix ...)) or (name expr)
        match = re.match(r'^\s*\(([a-zA-Z_][a-zA-Z0-9_?!*+\-/<>=]*)\s+', line)
        if match:
            name = match.group(1)
            if name in ['let', 'let*', 'letrec', 'if', 'cond', 'begin', 'lambda',
                       'fn', 'fix', 'define', 'quote', 'quasiquote', 'and', 'or',
                       'module-exports', 'when', 'unless', 'case']:
                line_idx += 1
                continue

            # Find end of this definition
            start_line = line_idx
            paren_count = 0
            in_string = False
            escape_next = False
            end_line = start_line

            for i in range(start_line, len(lines)):
                l = lines[i]
                for c in l:
                    if escape_next:
                        escape_next = False
                        continue
                    if c == '\\':
                        escape_next = True
                        continue
                    if c == '"':
                        in_string = not in_string
                        continue
                    if in_string:
                        continue
                    if c == ';':
                        break
                    if c == '(':
                        paren_count += 1
                    elif c == ')':
                        paren_count -= 1
                        if paren_count == 0:
                            end_line = i
                            break
                if paren_count == 0 and end_line == i:
                    break

            body_text = '\n'.join(lines[start_line:end_line+1])
            definitions.append((name, start_line + 1, end_line + 1, body_text))
            line_idx = end_line + 1
        else:
            line_idx += 1

    return definitions

def find_used_names(body_text, all_defined_names):
    """
    Find which defined names are used in a body.
    """
    used = set()
    # Remove strings and comments
    cleaned = re.sub(r'"[^"\\]*(?:\\.[^"\\]*)*"', '', body_text)
    cleaned = re.sub(r';.*$', '', cleaned, flags=re.MULTILINE)

    for name in all_defined_names:
        # Match as identifier (not part of larger word)
        pattern = r'(?<![a-zA-Z0-9_?!*+\-/<>=])' + re.escape(name) + r'(?![a-zA-Z0-9_?!*+\-/<>=])'
        if re.search(pattern, cleaned):
            used.add(name)

    return used

def main():
    prelude_dir = Path(__file__).parent
    missing_path = prelude_dir / 'missing.ss'

    # Collect all definitions from non-missing.ss files
    other_defs = set()

    for ss_file in sorted(prelude_dir.glob('*.ss')):
        if ss_file.name == 'missing.ss':
            continue
        for name, _, _, _ in find_top_level_defs(ss_file):
            other_defs.add(name)

    print(f"Found {len(other_defs)} unique definitions in other files")

    # Get all definitions in missing.ss with their bodies
    missing_defs = find_top_level_defs(missing_path)
    missing_names = set(name for name, _, _, _ in missing_defs)

    print(f"Found {len(missing_defs)} definitions in missing.ss")

    # Find duplicates (exist in both missing.ss and other files)
    duplicates = missing_names & other_defs
    print(f"Duplicates: {len(duplicates)}")

    # Build dependency graph within missing.ss
    # For each definition, find which other missing.ss definitions it uses
    internal_deps = {}  # name -> set of names it depends on (within missing.ss)

    for name, start, end, body in missing_defs:
        deps = find_used_names(body, missing_names)
        deps.discard(name)  # Remove self-reference
        internal_deps[name] = deps

    # Find which duplicates are "safe" to remove:
    # A duplicate is safe if no non-duplicate depends on it
    non_duplicates = missing_names - duplicates

    # Which duplicates are depended on by non-duplicates?
    needed_by_non_dups = set()
    for name in non_duplicates:
        for dep in internal_deps.get(name, set()):
            if dep in duplicates:
                needed_by_non_dups.add(dep)

    # Also, which duplicates are depended on by other duplicates we can't remove?
    # This requires iterative analysis
    definitely_keep = needed_by_non_dups.copy()
    changed = True
    while changed:
        changed = False
        for name in duplicates:
            if name in definitely_keep:
                continue
            # If this duplicate depends on another duplicate we're keeping
            for dep in internal_deps.get(name, set()):
                if dep in definitely_keep:
                    # Actually, that's fine - we're keeping the dep
                    pass

            # If any definition we're keeping depends on this one
            for other_name in definitely_keep | non_duplicates:
                if name in internal_deps.get(other_name, set()):
                    if name not in definitely_keep:
                        definitely_keep.add(name)
                        changed = True
                        break

    safe_to_remove = duplicates - definitely_keep

    print(f"\nAnalysis:")
    print(f"  Duplicates needed by non-duplicates: {len(needed_by_non_dups)}")
    print(f"  Total duplicates that must stay: {len(definitely_keep)}")
    print(f"  Safe to remove: {len(safe_to_remove)}")

    if not safe_to_remove:
        print("\nNo definitions can be safely removed!")
        return

    # Read missing.ss and remove safe duplicates
    with open(missing_path, 'r') as f:
        lines = f.readlines()

    # Find line ranges to remove
    ranges_to_remove = []
    for name, start, end, _ in missing_defs:
        if name in safe_to_remove:
            ranges_to_remove.append((start - 1, end - 1))  # Convert to 0-indexed

    ranges_to_remove.sort()

    # Expand to include preceding comments
    expanded = []
    for start, end in ranges_to_remove:
        new_start = start
        while new_start > 0:
            prev = lines[new_start - 1].strip()
            if prev.startswith(';') or prev == '':
                new_start -= 1
            else:
                break
        expanded.append((new_start, end))

    # Build set of lines to remove
    lines_to_remove = set()
    for start, end in expanded:
        for i in range(start, end + 1):
            lines_to_remove.add(i)

    # Create new content
    new_lines = []
    prev_blank = False
    for i, line in enumerate(lines):
        if i in lines_to_remove:
            continue
        is_blank = line.strip() == ''
        if is_blank and prev_blank:
            continue
        new_lines.append(line)
        prev_blank = is_blank

    # Backup and write
    backup_path = prelude_dir / 'missing.ss.bak2'
    with open(backup_path, 'w') as f:
        f.writelines(lines)
    print(f"\nBackup written to {backup_path}")

    with open(missing_path, 'w') as f:
        f.writelines(new_lines)

    print(f"Original: {len(lines)} lines")
    print(f"New: {len(new_lines)} lines")
    print(f"Removed: {len(lines) - len(new_lines)} lines")

    print(f"\nRemoved {len(safe_to_remove)} definitions:")
    for name in sorted(safe_to_remove)[:50]:
        print(f"  - {name}")
    if len(safe_to_remove) > 50:
        print(f"  ... and {len(safe_to_remove) - 50} more")

if __name__ == '__main__':
    main()
