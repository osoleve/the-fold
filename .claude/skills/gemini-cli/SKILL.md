---
name: gemini-cli
description: Invoke Google's Gemini AI via CLI in headless mode for AI-powered analysis, code generation, and reasoning tasks. Use when you need a second opinion, the user mentions Gemini, needs Google AI capabilities, or requests multi-modal analysis beyond Claude's scope.
allowed-tools: Bash(gemini:*), Read, Write, Grep, Glob
---

# Gemini CLI Skill

## Overview

This skill provides guidance for invoking Google's Gemini AI models via the command-line interface in headless (non-interactive) mode. Use Gemini for:

- Comparative analysis or second opinions
- Scenarios where the user explicitly requests Gemini
- Multi-modal analysis (images, audio, video)

Consider gemini-3-pro-preview to be a peer of Opus, and gemini-3-flash-preview to be a peer of Sonnet.

## Instructions

### Basic Headless Invocation

The Gemini CLI supports headless mode through standard input and command-line arguments:

```bash
# Basic prompt via stdin
echo "Your prompt here" | gemini -p

# Prompt via command-line argument
gemini -p "Your prompt here"

# With specific model selection
gemini -m gemini-3-pro-preview -p "Your prompt here"
gemini -m gemini-3-flash-preview -p "Your prompt here"
```

### Output Control

```bash
# JSON output for parsing
gemini --output-format json -p "Your prompt here"

# Stream responses (for long outputs)
gemini --output-format stream-json -p "Your prompt here"

# Save output to file
gemini -p "Your prompt here" > output.txt
```

### Configuration Options

| Option | Description | Example |
|--------|-------------|---------|
| `--prompt, -p` | Run in headless mode | `gemini -p "query"` |
| `--output-format` | Specify output format (text, json, stream-json) | `gemini -p "query" --output-format json` |
| `--model, -m` | Specify the Gemini model | `gemini -p "query" -m gemini-3-pro-preview` |
| `--debug, -d` | Enable debug mode | `gemini -p "query" --debug` |
| `--include-directories` | Include additional directories in context | `gemini -p "query" --include-directories src,docs` |
| `--yolo, -y` | Auto-approve all actions | `gemini -p "query" --yolo` |
| `--approval-mode` | Set approval mode (auto_edit, manual, etc) | `gemini -p "query" --approval-mode auto_edit` |

For complete details on all available configuration options, settings files, and environment variables, see the Gemini CLI Configuration Guide.

## Examples

### Example 1: Batch Code Analysis

```bash
for file in src/*.py; do
    echo "Analyzing $file..."
    result=$(cat "$file" | gemini -p "Find potential bugs and suggest improvements" --output-format json)
    echo "$result" | jq -r '.response' > "reports/$(basename "$file").analysis"
    echo "Completed analysis for $(basename "$file")" >> reports/progress.log
done
```

### Example 2: QA

```bash
gemini -p "Review the newly implemented higher-kinded types (HKTs) for ..."
```

### Example 3: User Simulation and Feedback

```bash
# Simulate user perspective on new feature
gemini -m gemini-3-flash-preview -p "Act as a new user encountering this interface for the first time. What's confusing? What delights you?" \
  --include-directories shell,docs \
  --output-format json > user/feedback/gemini-ux-review-$(date +%Y%m%d).json

# Quick usability check on CLI help text
cat shell/commands.ss | gemini -p "Is this help text clear to a beginner? Suggest improvements."
```

### Example 4: Second Opinion/Adversarial Review

```bash
# Challenge architectural decisions
gemini -m gemini-3-pro-preview -p "Review this type system design. What edge cases might break it? What performance issues do you foresee?" \
  --include-directories core/types \
  > docs/peer-review/type-system-critique-$(date +%Y%m%d).md

# Adversarial review of security-sensitive code
gemini -p "You are a security auditor. Find vulnerabilities in this validation logic. Consider injection attacks, bypasses, and edge cases." \
  --include-directories shell/validate.ss \
  --output-format json | jq -r '.findings[]' > reports/security-review.txt
```

### Example 5: Tech Debt Report

```bash
# Generate comprehensive tech debt inventory
gemini -m gemini-3-pro-preview -p "Analyze this codebase for technical debt. Identify: duplicated code, missing tests, complex functions, outdated patterns, performance bottlenecks, and documentation gaps. Prioritize by impact." \
  --include-directories core,shell \
  --output-format json > reports/tech-debt-$(date +%Y%m%d).json

# Focus on specific subsystem
gemini -p "Identify refactoring opportunities in this module. What would make it more maintainable?" \
  --include-directories core/types \
  | tee reports/types-refactor-suggestions.md
```

## Best Practices

1. **Escape Properly**: Quote prompts containing special characters or multiple lines
2. **Use Pipes**: Leverage stdin for complex prompts from files or command output
3. **Model Selection**: Generally trust "auto", but if you're not satisfied try gemini-3-pro-preview
4. **Error Handling**: Check exit codes and stderr for API errors
5. **Rate Limiting**: Be mindful of API quotas in automated scripts

## Piping and Composition

Combine with shell tools for powerful workflows:

```bash
# Process file through Gemini, save result
cat input.txt | gemini -p "Summarize this" > summary.txt

# Chain with other commands
gemini -p "Generate test data in JSON" --output-format json | jq '.items[]' | while read item; do ...; done

# Use heredoc for multi-line prompts
gemini -p << 'EOF'
Your multi-line
prompt here
EOF
```

## Integration with The Fold

When using Gemini within The Fold context:

1. **Complement, don't replace**: Use Gemini Pro for second opinions on designs, or as a subagent to handle parallel workstreams
2. **Document results**: Save Gemini review outputs to `docs/peer-review/` for reference
3. **Forum posts**: Share interesting Gemini insights via `(msg 'channel ...)`
4. **Validation**: Cross-check critical Gemini outputs with core validation logic

