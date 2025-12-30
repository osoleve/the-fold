#!/usr/bin/env python3
"""Detect shadowing conflicts in prelude modules."""

import re
from pathlib import Path
from collections import defaultdict

# Module load order from mod.rs
MODULES = [
    "core.ss", "function.ss", "list.ss", "equality.ss",
    "maybe.ss", "either.ss", "numeric.ss", "comparison.ss",
    "string.ss", "collection.ss", "control.ss", "validation.ss",
    "stream.ss", "datastructures.ss", "graph.ss", "tree.ss",
    "missing.ss", "path.ss", "encoding.ss", "compat.ss"
]

def extract_functions(filepath):
    """Extract function definitions from a Scheme file."""
    pattern = re.compile(r'^\(([a-z][a-z0-9?!-]*)\s+\((fn|fix)')
    functions = []

    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            for line_num, line in enumerate(f, 1):
                match = pattern.match(line)
                if match:
                    functions.append((match.group(1), line_num))
    except FileNotFoundError:
        pass

    return functions

def main():
    prelude_dir = Path(__file__).parent

    all_defs = {}  # funcname -> (module, line_num)
    def_counts = defaultdict(int)
    shadows = []  # (funcname, first_module, shadow_module)

    print("=== Prelude Shadowing Detector ===\n")
    print("Scanning modules in load order...\n")

    total_defs = 0

    for module in MODULES:
        filepath = prelude_dir / module
        if not filepath.exists():
            print(f"⚠ Skipping missing module: {module}")
            continue

        functions = extract_functions(filepath)

        for funcname, line_num in functions:
            total_defs += 1
            def_counts[funcname] += 1

            if funcname in all_defs:
                first_module, first_line = all_defs[funcname]
                print(f"X SHADOW: {funcname}")
                print(f"    First:  {first_module}:{first_line}")
                print(f"    Shadow: {module}:{line_num}")
                shadows.append((funcname, first_module, module))
            else:
                all_defs[funcname] = (module, line_num)

    print(f"\n=== Summary ===")
    print(f"Total definitions: {total_defs}")
    print(f"Unique functions: {len(all_defs)}")
    print(f"Shadowed: {len(shadows)}")

    if shadows:
        print(f"\nFunctions defined multiple times:")
        multi_defs = [(name, count) for name, count in def_counts.items() if count > 1]
        multi_defs.sort(key=lambda x: -x[1])

        for name, count in multi_defs[:30]:
            print(f"  {name}: {count} times")

        return 1
    else:
        print("\nOK: No shadowing conflicts detected!")
        return 0

if __name__ == "__main__":
    exit(main())
