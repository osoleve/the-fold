#!/usr/bin/env python3
"""Remove duplicate statistical function definitions from missing.ss"""

import re

STATS_FUNCTIONS = [
    'mean', 'median', 'mode', 'variance', 'std-dev',
    'percentile', 'z-score', 'z-scores', 'correlation',
    'covariance', 'normalize-list', 'standardize',
    'quartiles', 'interquartile-range', 'majority'
]

def main():
    with open('missing.ss', 'r', encoding='utf-8') as f:
        lines = f.readlines()

    # Track which lines to comment
    skip_until = -1
    output = []
    removed_count = 0

    for i, line in enumerate(lines):
        if i < skip_until:
            continue  # Skip lines in removed sections

        # Check if this line defines a stats function
        for func in STATS_FUNCTIONS:
            pattern = f"^\\({func}\\s+\\(fn"
            if re.match(pattern, line):
                # Find the end of this definition
                depth = 0
                start = i
                end = i

                for j in range(i, len(lines)):
                    for char in lines[j]:
                        if char == '(':
                            depth += 1
                        elif char == ')':
                            depth -= 1
                            if depth == 0:
                                end = j
                                break
                    if depth == 0:
                        break

                # Mark this section as removed
                output.append(f"; REMOVED: {func} (duplicate, now in stats.ss)\n")
                skip_until = end + 1
                removed_count += 1
                print(f"Removed {func} at lines {start+1}-{end+1}")
                break
        else:
            # Not a stats function, keep the line
            if i >= skip_until or skip_until == -1:
                output.append(line)

    # Write back
    with open('missing.ss', 'w', encoding='utf-8') as f:
        f.writelines(output)

    print(f"\nRemoved {removed_count} duplicate definitions")

if __name__ == "__main__":
    main()
