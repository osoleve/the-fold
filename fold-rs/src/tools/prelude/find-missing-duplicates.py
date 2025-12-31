#!/usr/bin/env python3
"""
Find definitions in missing.ss that also exist in other prelude files.
These can be safely removed from missing.ss.
"""

import os
import re
from collections import defaultdict
from pathlib import Path

def find_top_level_names(filepath):
    """
    Find all top-level binding names in a .ss file.
    Returns list of (name, line_number, line_content).
    """
    definitions = []

    with open(filepath, 'r') as f:
        lines = f.readlines()

    for line_num, line in enumerate(lines, 1):
        # Skip pure comments
        stripped = line.strip()
        if stripped.startswith(';') or not stripped:
            continue

        # Match: (name (fn ...)) or (name (fix ...)) or (name expr) or (name other-name)
        # Must start at beginning of line (after optional whitespace)
        match = re.match(r'^\s*\(([a-zA-Z_][a-zA-Z0-9_?!*+\-/<>=]*)\s+', line)
        if match:
            name = match.group(1)
            # Skip keywords and special forms
            if name in ['let', 'let*', 'letrec', 'if', 'cond', 'begin', 'lambda',
                       'fn', 'fix', 'define', 'quote', 'quasiquote', 'and', 'or',
                       'module-exports', 'when', 'unless', 'case']:
                continue
            definitions.append((name, line_num, line.rstrip()))

    return definitions

def main():
    prelude_dir = Path(__file__).parent

    # Collect all definitions from non-missing.ss files
    other_defs = defaultdict(list)  # name -> [(file, line_num), ...]

    # Also collect from missing.ss separately
    missing_defs = []  # [(name, line_num, line_content), ...]

    for ss_file in sorted(prelude_dir.glob('*.ss')):
        filename = ss_file.name
        defs = find_top_level_names(ss_file)

        if filename == 'missing.ss':
            missing_defs = defs
        else:
            for name, line_num, _ in defs:
                other_defs[name].append((filename, line_num))

    # Find missing.ss definitions that exist elsewhere
    can_remove = []
    unique_to_missing = []

    for name, line_num, line_content in missing_defs:
        if name in other_defs:
            can_remove.append((name, line_num, line_content, other_defs[name]))
        else:
            unique_to_missing.append((name, line_num, line_content))

    # Report
    print("=" * 80)
    print(f"DEFINITIONS IN missing.ss THAT EXIST ELSEWHERE: {len(can_remove)}")
    print("These can be REMOVED from missing.ss")
    print("=" * 80)

    # Sort by line number
    can_remove.sort(key=lambda x: x[1])

    for name, line_num, line_content, other_locs in can_remove:
        other_files = ', '.join(f"{f}:{ln}" for f, ln in other_locs[:3])
        if len(other_locs) > 3:
            other_files += f" (+{len(other_locs)-3} more)"
        print(f"L{line_num}: {name} -- also in: {other_files}")

    print("\n" + "=" * 80)
    print(f"DEFINITIONS UNIQUE TO missing.ss: {len(unique_to_missing)}")
    print("These need to stay or be moved to appropriate modules")
    print("=" * 80)

    # Categorize unique definitions
    categories = {
        'list': [], 'string': [], 'numeric': [], 'monad': [],
        'validation': [], 'collection': [], 'graph': [], 'tree': [],
        'function': [], 'control': [], 'comparison': [], 'stream': [],
        'parser': [], 'matrix': [], 'lens': [], 'other': []
    }

    for name, line_num, line_content in unique_to_missing:
        name_lower = name.lower()
        categorized = False

        if any(x in name_lower for x in ['list', 'cons', 'car', 'cdr', 'append', 'reverse',
                                          'take', 'drop', 'filter', 'fold', 'zip', 'partition',
                                          'group', 'chunk', 'split', 'interleave', 'flatten',
                                          'pairs', 'nth', 'last', 'init', 'head', 'tail']):
            categories['list'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['string', 'char', 'str-', 'word', 'line', 'text',
                                            'parse', 'format', 'trim', 'pad', 'replace', 'join']):
            categories['string'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['matrix', 'vec-', 'vector', 'dot', 'cross']):
            categories['matrix'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['num', 'math', 'sum', 'prod', 'avg', 'mean', 'median',
                                            'variance', 'std', 'min', 'max', 'abs', 'floor',
                                            'ceil', 'round', 'gcd', 'lcm', 'prime', 'factor',
                                            'fib', 'range', 'seq', 'div', 'mod', 'pow', 'sqrt',
                                            'log', 'exp', 'sin', 'cos', 'tan']):
            categories['numeric'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['monad', 'bind', 'return', 'maybe', 'either',
                                            'just', 'nothing', 'left', 'right', 'state',
                                            'reader', 'writer', 'io', 'applicative', 'functor']):
            categories['monad'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['stream', 'lazy', 'delay', 'force', 'thunk', 'memo']):
            categories['stream'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['lens', 'view', 'over', 'set-']):
            categories['lens'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['p-', 'parse', 'parser', 'token', 'lex']):
            categories['parser'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['valid', 'assert', 'check', 'test', 'expect', 'should']):
            categories['validation'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['dict', 'hash', 'set-', 'alist', 'assoc', 'map-',
                                            'bag', 'multi', 'plist', 'trie', 'heap', 'queue',
                                            'stack', 'deque', 'ring', 'buffer', 'finger']):
            categories['collection'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['graph', 'node', 'edge', 'vertex', 'path',
                                            'bfs', 'dfs', 'dijkstra', 'topo', 'uf-']):
            categories['graph'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['tree', 'rose', 'sexp', 'nested', 'deep']):
            categories['tree'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['fn', 'func', 'compose', 'pipe', 'curry',
                                            'partial', 'apply', 'call', 'flip', 'fix',
                                            'identity', 'const', 'memoize', 'thunk', 'trampoline']):
            categories['function'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['if', 'when', 'unless', 'cond', 'case', 'match',
                                            'loop', 'while', 'for', 'do', 'iterate', 'repeat',
                                            'until', 'times', 'collect']):
            categories['control'].append((name, line_num))
            categorized = True
        elif any(x in name_lower for x in ['sort', 'compare', 'order', 'equal', 'less',
                                            'greater', 'between', 'cmp', 'merge']):
            categories['comparison'].append((name, line_num))
            categorized = True

        if not categorized:
            categories['other'].append((name, line_num))

    for category, items in sorted(categories.items()):
        if items:
            print(f"\n{category.upper()} ({len(items)}):")
            for name, line_num in sorted(items, key=lambda x: x[1]):
                print(f"  L{line_num}: {name}")

    # Generate removal commands
    print("\n" + "=" * 80)
    print("LINE RANGES TO CONSIDER FOR REMOVAL")
    print("=" * 80)

    # Group consecutive removable lines
    removable_lines = sorted(set(x[1] for x in can_remove))
    if removable_lines:
        ranges = []
        start = removable_lines[0]
        end = start

        for line_num in removable_lines[1:]:
            if line_num <= end + 5:  # Allow small gaps (comments between definitions)
                end = line_num
            else:
                ranges.append((start, end))
                start = line_num
                end = line_num
        ranges.append((start, end))

        print(f"\nTotal removable definitions: {len(can_remove)}")
        print(f"Across {len(ranges)} line ranges")
        print("\nTop 20 largest ranges:")

        sorted_ranges = sorted(ranges, key=lambda x: x[1] - x[0], reverse=True)
        for start, end in sorted_ranges[:20]:
            print(f"  Lines {start}-{end} ({end - start + 1} lines)")

if __name__ == '__main__':
    main()
