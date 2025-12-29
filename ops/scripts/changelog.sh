#!/bin/bash
# Generate changelog with git-cliff
#
# Usage:
#   ./changelog.sh              # Generate changelog for all tags
#   ./changelog.sh --tag v1.0.0 # Generate and tag release
#   ./changelog.sh --unreleased # Show unreleased changes

set -e

cd "$(dirname "$0")/../.." || exit 1

# Check if git-cliff is installed
if ! command -v git-cliff >/dev/null 2>&1; then
    echo "Error: git-cliff not found" >&2
    echo "Install with: cargo install git-cliff" >&2
    exit 1
fi

# Create default config if it doesn't exist
if [ ! -f "cliff.toml" ]; then
    echo "Creating default cliff.toml configuration..."
    cat > cliff.toml << 'EOF'
# git-cliff configuration for The Fold

[changelog]
header = """
# Changelog\n
All notable changes to this project will be documented in this file.\n
"""
body = """
{% for group, commits in commits | group_by(attribute="group") %}
    ### {{ group | upper_first }}
    {% for commit in commits %}
        - {{ commit.message | upper_first }} ({{ commit.id | truncate(length=7, end="") }})\
    {% endfor %}
{% endfor %}\n
"""
trim = true

[git]
conventional_commits = true
filter_unconventional = true
commit_parsers = [
    { message = "^feat", group = "Features"},
    { message = "^fix", group = "Bug Fixes"},
    { message = "^doc", group = "Documentation"},
    { message = "^perf", group = "Performance"},
    { message = "^refactor", group = "Refactoring"},
    { message = "^style", group = "Styling"},
    { message = "^test", group = "Testing"},
    { message = "^chore\\(release\\)", skip = true},
    { message = "^chore", group = "Miscellaneous"},
]
EOF
fi

echo "Generating changelog with git-cliff..."
git-cliff "$@"
