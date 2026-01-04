# Flashmob Skill

## Overview

Flashmob is a parallel QA swarm utility that orchestrates multiple Gemini Flash agents to review code for bugs, security issues, and performance problems. It includes a triage phase where second agents validate findings and discover gaps.

Use flashmob when:
- Starting work on a new area of the codebase
- After significant refactoring
- For periodic code quality audits
- When the user asks for QA review or bug hunting

## Quick Start

```bash
# Standard review of a directory
flashmob core/fp

# Specialized agents (security + performance + correctness)
flashmob --specialized core/

# Cross-validation mode (consensus only)
flashmob --cross-validate -n 3 core/

# Create beads from confirmed issues
flashmob --create-beads core/

# Dry run to see file assignments
flashmob --dry-run core/
```

## CLI Reference

```
flashmob [options] [directories...]

Options:
  -n NUM              Number of agents (default: 3)
  -k NUM              Files per agent (default: 4)
  -m MODE             Mode: random|clustered|coverage (default: coverage)
  -s, --specialized   Use specialized agents (security/perf/correctness)
  -x, --cross-validate  Cross-validate with multiple agents per file
  --create-beads      Create beads from confirmed issues
  --bead-threshold    Minimum severity for beads: high|medium|low (default: medium)
  --no-line-correct   Skip line number correction
  --no-triage         Skip triage validation phase
  --no-stream         Disable streaming output
  --history FILE      History file (default: .flashmob-history)
  --no-history        Don't track history
  --extensions EXT    File extensions (default: ss)
  --dry-run           Show assignments only
  -h, --help          Show help
```

## Modes

### Standard Mode (default)
Each agent reviews a unique subset of files. Good for coverage.
```bash
flashmob -n 3 -k 4 core/  # 3 agents × 4 files = 12 file reviews
```

### Specialized Mode (`--specialized`)
Three focused agents review ALL files, each looking for specific issue types:
- **Security agent**: Injection, auth bypass, secrets, input validation
- **Performance agent**: O(N²), memory leaks, unnecessary allocations
- **Correctness agent**: Off-by-one, edge cases, type errors, logic bugs

Best for: Deep review of critical code paths.
```bash
flashmob --specialized core/security/
```

### Cross-Validation Mode (`--cross-validate`)
Multiple agents independently review the SAME files. Only issues reported by 2+ agents are kept.

Best for: High-confidence findings, reducing false positives.
```bash
flashmob --cross-validate -n 3 -k 5 core/  # 3 agents all review same 5 files
```

### Coverage Mode (`-m coverage`)
Prioritizes files not recently reviewed (tracked in `.flashmob-history`).

Best for: Systematic codebase coverage over time.

### Clustered Mode (`-m clustered`)
Groups files by directory so agents review related files together.

Best for: Context-aware review of module internals.

## Phases

### Phase 1: QA Review
Agents analyze assigned files and produce JSON reports with:
- Issue severity (high/medium/low)
- Line numbers
- Code snippets
- Suggested fixes
- Confidence scores (0.0-1.0)
- Category (security/performance/correctness/style)

### Phase 2: Line Correction
Fuzzy-matches code snippets to find actual line numbers (agents often get these wrong).

### Phase 3: Triage Validation
Second agents validate each finding:
- **confirmed**: Real bug, should fix
- **rejected**: False positive
- **uncertain**: Needs human review

Triage agents also discover **gaps** - issues the QA agents missed.

### Phase 4: Bead Creation (optional)
With `--create-beads`, automatically creates beads for confirmed issues:
```bash
flashmob --create-beads --bead-threshold high core/  # Only P1 bugs
flashmob --create-beads --bead-threshold medium core/  # P1 and P2
```

## Best Practices

### 1. Always Use Triage
Without triage, expect ~40% false positive rate. With triage, ~20%.
```bash
# Good
flashmob core/

# Risky (more noise)
flashmob --no-triage core/
```

### 2. Use Specialized Mode for Critical Code
Generic review misses domain-specific issues. Specialized agents go deeper.
```bash
flashmob --specialized core/auth/
flashmob --specialized core/crypto/
```

### 3. Use Cross-Validation for High Stakes
When you need high confidence (e.g., security audit):
```bash
flashmob --cross-validate -n 3 core/security/
```

### 4. Start Small, Then Expand
Don't review the entire codebase at once. Start with focused areas:
```bash
flashmob -n 2 -k 3 core/base/  # Small test run
flashmob core/fp/control/      # One module
flashmob core/                 # Full review (later)
```

### 5. Review Reports Before Acting
Always read the triage reports before fixing:
```bash
cat reports/triage-agent-*.json | jq '.validated_issues[] | select(.validation=="confirmed")'
```

### 6. Track Coverage Over Time
Use coverage mode to systematically review the codebase:
```bash
# First run reviews random files, saves to history
flashmob -m coverage core/

# Subsequent runs prioritize unreviewed files
flashmob -m coverage core/  # Different files
```

### 7. Create Beads for Tracking
For large reviews, create beads to track fixes:
```bash
flashmob --create-beads core/
bd list --status=open | grep flashmob
```

## Interpreting Results

### QA Summary Table
```
| Category | High | Medium | Low | Total |
|----------|------|--------|-----|-------|
| Security | 1    | 2      | 1   | 4     |
| Perf     | 3    | 4      | 1   | 8     |
| Correct  | 3    | 1      | 1   | 5     |
```

### Triage Statistics
```
| Metric | Count |
|--------|-------|
| Confirmed | 76 (real bugs) |
| Rejected | 20 (false positives) |
| Gaps Found | 41 (missed by QA) |
```

### Confidence Levels
- **0.9+**: Definitely a bug - prioritize
- **0.7-0.9**: Likely a bug - review
- **0.5-0.7**: Possible issue - verify
- **<0.5**: Not reported

## Common Issues Found

Based on experience, flashmob frequently finds:

**Performance:**
- O(N²) algorithms (nested loops, list-ref in loops)
- Unnecessary allocations in hot paths
- Missing memoization

**Security:**
- Injection vulnerabilities (SQL, command, path traversal)
- Unescaped output (XSS, DOT injection)
- Unbounded resource consumption (DoS)

**Correctness:**
- Off-by-one errors
- Missing edge case handling (empty, null, negative)
- State management bugs (leaks, incorrect restoration)
- Incomplete implementations (documented but not implemented)

## Workflow Integration

### After Flashmob Run
```bash
# 1. Review high-severity confirmed issues
jq '.validated_issues[] | select(.validation=="confirmed" and .adjusted_severity=="high")' reports/triage-*.json

# 2. Create beads if not using --create-beads
bd create --title="[flashmob] ..." --type=bug --priority=1

# 3. Fix issues (spawn parallel agents)
# Claude can spawn agents to fix multiple issues in parallel

# 4. Verify fixes
scheme --script core/run-tests.ss

# 5. Commit and close beads
git add . && git commit -m "fix: Address flashmob findings"
bd close fold-xxx fold-yyy --reason="Fixed"
bd sync && git push
```

### Parallel Fix Pattern
When Claude fixes flashmob findings, it spawns parallel agents:
```
# User: "fix the P1s"
# Claude spawns N agents in parallel, one per bug
# Each agent: reads file, fixes bug, adds tests, closes bead
```

## Output Files

```
reports/
├── gemini-flashmob-agent-*.json    # QA phase reports
├── gemini-flashmob-agent-security.json   # Specialized mode
├── gemini-flashmob-agent-performance.json
├── gemini-flashmob-agent-correctness.json
├── triage-agent-*.json             # Triage validations
└── triage-final-summary.json       # Aggregate statistics
```

## Lessons Learned

1. **Line numbers are hints, not facts** - Agents often report wrong line numbers. Use code snippets to find actual location.

2. **Triage is essential** - 40% of initial findings are false positives without triage.

3. **Gaps are valuable** - Triage agents find ~50% as many new issues as the original QA.

4. **Specialized > Generic** - Domain-focused agents find issues generic review misses.

5. **Cross-validation for confidence** - When multiple agents agree, confidence is much higher.

6. **Coverage mode for systematic review** - Prevents re-reviewing the same files.

7. **Small batches, iterate** - Better to run flashmob frequently on small areas than rarely on everything.
