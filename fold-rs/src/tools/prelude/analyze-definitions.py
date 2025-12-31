#!/usr/bin/env python3
"""
Analyze all prelude .ss files to find top-level definitions.
Reports duplicates and helps plan the missing.ss refactoring.
"""

import os
import re
import sys
from collections import defaultdict
from pathlib import Path

# Pattern to match top-level definitions in let* format: (name ...)
# Must be at start of line (possibly after whitespace/comments)
LET_BINDING_PATTERN = re.compile(r'^\s*\(([a-zA-Z_][a-zA-Z0-9_?!*+-/<>=]*)\s+(?:\(fn|fix|\S)', re.MULTILINE)

# Pattern to match (define ...) style definitions
DEFINE_PATTERN = re.compile(r'^\s*\(define\s+(?:\(([a-zA-Z_][a-zA-Z0-9_?!*+-/<>=]*)|([a-zA-Z_][a-zA-Z0-9_?!*+-/<>=]*))', re.MULTILINE)

def find_definitions(filepath):
    """Find all top-level definitions in a file."""
    definitions = []

    with open(filepath, 'r') as f:
        content = f.read()
        lines = content.split('\n')

    # Track parenthesis depth to identify top-level definitions
    depth = 0
    in_comment = False
    current_line = 0

    for line_num, line in enumerate(lines, 1):
        # Skip comment lines
        stripped = line.strip()
        if stripped.startswith(';'):
            continue

        # Check for let* binding format at start of logical form
        # This is tricky - we need to be at depth 1 (inside the let* bindings)
        # For simplicity, look for patterns that match definition starts

        # Match let* binding format: (name (fn ...)) or (name expr)
        match = re.match(r'^\s*\(([a-zA-Z_][a-zA-Z0-9_?!*+-/<>=]*)\s+', line)
        if match:
            name = match.group(1)
            # Skip keywords and special forms
            if name not in ['let', 'let*', 'letrec', 'if', 'cond', 'begin', 'lambda',
                           'fn', 'fix', 'define', 'quote', 'quasiquote', 'and', 'or',
                           'module-exports']:
                definitions.append((name, line_num))

        # Also check for define style (shouldn't be present after our fixes, but check anyway)
        match = DEFINE_PATTERN.match(line)
        if match:
            name = match.group(1) or match.group(2)
            if name:
                definitions.append((name, line_num))

    return definitions

def analyze_prelude_dir(prelude_dir):
    """Analyze all .ss files in prelude directory."""
    all_definitions = defaultdict(list)  # name -> [(file, line), ...]
    files_definitions = {}  # file -> [(name, line), ...]

    ss_files = sorted(Path(prelude_dir).glob('*.ss'))

    for filepath in ss_files:
        filename = filepath.name
        defs = find_definitions(filepath)
        files_definitions[filename] = defs

        for name, line_num in defs:
            all_definitions[name].append((filename, line_num))

    return all_definitions, files_definitions

def print_report(all_definitions, files_definitions):
    """Print analysis report."""

    # Find duplicates
    duplicates = {name: locs for name, locs in all_definitions.items() if len(locs) > 1}

    # Find definitions only in missing.ss
    missing_only = {name: locs for name, locs in all_definitions.items()
                   if len(locs) == 1 and locs[0][0] == 'missing.ss'}

    # Find definitions in missing.ss that are also elsewhere
    missing_duplicates = {name: locs for name, locs in duplicates.items()
                         if any(loc[0] == 'missing.ss' for loc in locs)}

    print("=" * 80)
    print("PRELUDE DEFINITION ANALYSIS")
    print("=" * 80)

    print(f"\nTotal unique definitions: {len(all_definitions)}")
    print(f"Definitions with duplicates: {len(duplicates)}")
    print(f"Definitions only in missing.ss: {len(missing_only)}")
    print(f"Duplicates involving missing.ss: {len(missing_duplicates)}")

    print("\n" + "=" * 80)
    print("DEFINITIONS IN MISSING.SS THAT EXIST ELSEWHERE (can be removed)")
    print("=" * 80)

    for name in sorted(missing_duplicates.keys()):
        locs = missing_duplicates[name]
        print(f"\n{name}:")
        for filename, line_num in sorted(locs):
            marker = " <-- REMOVE" if filename == 'missing.ss' else ""
            print(f"  {filename}:{line_num}{marker}")

    print("\n" + "=" * 80)
    print("ALL DUPLICATES (full list)")
    print("=" * 80)

    for name in sorted(duplicates.keys()):
        locs = duplicates[name]
        print(f"\n{name}:")
        for filename, line_num in sorted(locs):
            print(f"  {filename}:{line_num}")

    print("\n" + "=" * 80)
    print("DEFINITIONS PER FILE")
    print("=" * 80)

    for filename in sorted(files_definitions.keys()):
        defs = files_definitions[filename]
        print(f"\n{filename}: {len(defs)} definitions")

    print("\n" + "=" * 80)
    print("DEFINITIONS ONLY IN MISSING.SS (need to be moved to other modules)")
    print("=" * 80)

    # Group by likely module destination
    categories = {
        'list': [],
        'string': [],
        'numeric': [],
        'monad': [],
        'validation': [],
        'collection': [],
        'graph': [],
        'tree': [],
        'function': [],
        'control': [],
        'comparison': [],
        'other': []
    }

    for name in sorted(missing_only.keys()):
        # Try to categorize based on name
        name_lower = name.lower()
        if any(x in name_lower for x in ['list', 'cons', 'car', 'cdr', 'append', 'reverse', 'take', 'drop', 'filter', 'map', 'fold', 'zip', 'partition', 'group', 'chunk', 'split', 'interleave', 'flatten']):
            categories['list'].append(name)
        elif any(x in name_lower for x in ['string', 'char', 'str-', 'word', 'line', 'text', 'parse']):
            categories['string'].append(name)
        elif any(x in name_lower for x in ['num', 'math', 'sum', 'prod', 'avg', 'mean', 'median', 'variance', 'std', 'min', 'max', 'abs', 'floor', 'ceil', 'round', 'gcd', 'lcm', 'prime', 'factor', 'fib', 'range', 'seq']):
            categories['numeric'].append(name)
        elif any(x in name_lower for x in ['monad', 'bind', 'return', 'maybe', 'either', 'just', 'nothing', 'left', 'right', 'state', 'reader', 'writer', 'io']):
            categories['monad'].append(name)
        elif any(x in name_lower for x in ['valid', 'assert', 'check', 'test', 'expect', 'should']):
            categories['validation'].append(name)
        elif any(x in name_lower for x in ['dict', 'hash', 'set', 'alist', 'assoc', 'map-', 'bag', 'multi']):
            categories['collection'].append(name)
        elif any(x in name_lower for x in ['graph', 'node', 'edge', 'vertex', 'path', 'bfs', 'dfs', 'dijkstra', 'topo']):
            categories['graph'].append(name)
        elif any(x in name_lower for x in ['tree', 'node', 'leaf', 'branch', 'traverse', 'walk', 'depth', 'breadth']):
            categories['tree'].append(name)
        elif any(x in name_lower for x in ['fn', 'func', 'compose', 'pipe', 'curry', 'partial', 'apply', 'call', 'flip', 'identity', 'const']):
            categories['function'].append(name)
        elif any(x in name_lower for x in ['if', 'when', 'unless', 'cond', 'case', 'match', 'loop', 'while', 'for', 'do', 'iterate']):
            categories['control'].append(name)
        elif any(x in name_lower for x in ['sort', 'compare', 'order', 'equal', 'less', 'greater', 'between']):
            categories['comparison'].append(name)
        else:
            categories['other'].append(name)

    for category, names in categories.items():
        if names:
            print(f"\n{category.upper()} ({len(names)}):")
            for name in sorted(names):
                loc = missing_only[name][0]
                print(f"  {name} (line {loc[1]})")

    return duplicates, missing_only, missing_duplicates

if __name__ == '__main__':
    prelude_dir = os.path.dirname(os.path.abspath(__file__))
    all_defs, files_defs = analyze_prelude_dir(prelude_dir)
    print_report(all_defs, files_defs)
