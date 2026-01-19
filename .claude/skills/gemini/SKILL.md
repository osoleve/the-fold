---
name: gemini
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

The Gemini CLI supports headless mode through positional arguments and standard input:

```bash
# Prompt via positional argument (preferred)
gemini "Your prompt here"

# With specific model selection
gemini -m gemini-3-pro-preview "Your prompt here"
gemini -m gemini-3-flash-preview "Your prompt here"

# Prompt via stdin
echo "Your prompt here" | gemini
```

### Output Control

```bash
# JSON output for parsing
gemini --output-format json "Your prompt here"

# Stream responses (for long outputs)
gemini --output-format stream-json "Your prompt here"

# Save output to file
gemini "Your prompt here" > output.txt
```

### Configuration Options

| Option | Description | Example |
|----|----|----|
| `[query..]` | Positional prompt (preferred) | `gemini "query"` |
| `--output-format, -o` | Output format (text, json, stream-json) | `gemini --output-format json "query"` |
| `--model, -m` | Specify the Gemini model | `gemini -m gemini-3-pro-preview "query"` |
| `--debug, -d` | Enable debug mode | `gemini --debug "query"` |
| `--include-directories` | Additional directories for context (array) | `gemini --include-directories src docs "query"` |
| `--yolo, -y` | Auto-approve all actions | `gemini --yolo "query"` |
| `--approval-mode` | Approval mode (default, auto_edit, yolo) | `gemini --approval-mode auto_edit "query"` |
| `--sandbox, -s` | Run in sandbox mode | `gemini --sandbox "query"` |
| `--resume, -r` | Resume previous session | `gemini --resume latest` |

For complete details on all available configuration options, settings files, and environment variables, see the Gemini CLI Configuration Guide.

## Examples

### Example 1: Batch Code Analysis

```bash
for file in src/*.py; do
    echo "Analyzing $file..."
    result=$(cat "$file" | gemini --output-format json "Find potential bugs and suggest improvements")
    echo "$result" | jq -r '.response' > "reports/$(basename "$file").analysis"
    echo "Completed analysis for $(basename "$file")" >> reports/progress.log
done
```

### Example 2: QA

```bash
gemini "Review the newly implemented higher-kinded types (HKTs) for ..."
```

### Example 3: User Simulation and Feedback

```bash
# Simulate user perspective on new feature
gemini -m gemini-3-flash-preview \
  --include-directories boundary docs \
  --output-format json \
  "Act as a new user encountering this interface for the first time. What's confusing? What delights you?" \
  > user/feedback/gemini-ux-review-$(date +%Y%m%d).json

# Quick usability check on CLI help text
cat boundary/commands.ss | gemini "Is this help text clear to a beginner? Suggest improvements."
```

### Example 4: Second Opinion/Adversarial Review

```bash
# Challenge architectural decisions
gemini -m gemini-3-pro-preview \
  --include-directories core/types \
  "Review this type system design. What edge cases might break it? What performance issues do you foresee?" \
  > docs/peer-review/type-system-critique-$(date +%Y%m%d).md

# Adversarial review of security-sensitive code
gemini --include-directories boundary \
  --output-format json \
  "You are a security auditor. Find vulnerabilities in validate.ss. Consider injection attacks, bypasses, and edge cases." \
  | jq -r '.findings[]' > reports/security-review.txt
```

### Example 5: Tech Debt Report

```bash
# Generate comprehensive tech debt inventory
gemini -m gemini-3-pro-preview \
  --include-directories core boundary \
  --output-format json \
  "Analyze this codebase for technical debt. Identify: duplicated code, missing tests, complex functions, outdated patterns, performance bottlenecks, and documentation gaps. Prioritize by impact." \
  > reports/tech-debt-$(date +%Y%m%d).json

# Focus on specific subsystem
gemini --include-directories core/types \
  "Identify refactoring opportunities in this module. What would make it more maintainable?" \
  | tee reports/types-refactor-suggestions.md
```

## Best Practices

1. **Use Positional Args**: Use `gemini "prompt"` not the deprecated `-p` flag
2. **Escape Properly**: Quote prompts containing special characters or multiple lines
3. **Use Pipes**: Leverage stdin for complex prompts from files or command output
4. **Model Selection by Use Case**:
   - **QA/code review/flashmob**: Use `gemini-3-flash-preview` (faster, cheaper for bulk tasks)
   - **Deep analysis/architecture**: Use `gemini-3-pro-preview` (more thorough reasoning)
   - **General/uncertain**: Trust "auto" mode
5. **Error Handling**: Check exit codes (non-zero = failure) and stderr for API errors
6. **Rate Limiting**: Be mindful of API quotas in automated scripts
7. **JSON Output**: When using `--output-format json`, the response is in the `.response` field

## Piping and Composition

Combine with shell tools for powerful workflows:

```bash
# Process file through Gemini, save result
cat input.txt | gemini "Summarize this" > summary.txt

# Chain with other commands
gemini --output-format json "Generate test data in JSON" | jq '.items[]' | while read item; do ...; done

# Use heredoc for multi-line prompts
gemini "$(cat << 'EOF'
Your multi-line
prompt here
EOF
)"
```

## QA and Flashmob Integration

When running flashmob QA reviews, **always use `gemini-3-flash-preview`**:

```bash
# Flashmob QA review - use flash model
gemini -m gemini-3-flash-preview \
  --include-directories <target-dir> \
  "Review this code for bugs, logic errors, edge cases. Report findings with file:line format."

# Multiple files in parallel (batch mode)
for file in boundary/tools/*.ss; do
  gemini -m gemini-3-flash-preview \
    "Review $file for correctness issues" \
    > "reports/$(basename "$file").review" &
done
wait
```

The flash model is preferred for QA because:
- **Faster**: Lower latency for bulk file reviews
- **Cheaper**: More cost-effective for many small reviews
- **Sufficient**: Code review doesn't require pro-level reasoning depth

Reserve `gemini-3-pro-preview` for architectural design reviews or complex analysis.

## Integration with The Fold

When using Gemini within The Fold context:

1. **Complement, don't replace**: Use Gemini Pro for second opinions on designs, or as a subagent to handle parallel workstreams
2. **Document results**: Save Gemini review outputs to `docs/peer-review/` for reference
3. **Forum posts**: Share interesting Gemini insights via `(msg 'channel ...)`
4. **Validation**: Cross-check critical Gemini outputs with core validation logic

