# Development Tooling Reference

This document catalogs all development quality-of-life tools for The Fold, including currently integrated tools and candidates for future integration.

## Currently Integrated

| Tool | Language | Primary Purpose | Automation Trigger | Integrated |
|------|----------|-----------------|-------------------|------------|
| cargo fmt | Rust | Code formatting | Pre-commit hook | ✅ |
| cargo clippy | Rust | Linting & best practices | Pre-commit hook | ✅ |
| scmindent | Scheme | Indentation formatting | Pre-commit hook (staged files) | ✅ |
| cargo-llvm-cov | Rust | Test coverage reporting | Manual (`./ops/scripts/coverage.sh`) | ✅ |
| cargo-audit | Rust | Security vulnerability scanning | Manual (`./ops/scripts/audit.sh`) | ✅ |
| cargo-watch | Rust | Auto-rebuild on file changes | Manual (`./ops/scripts/watch.sh`) | ✅ |
| bd (beads) | All | Issue tracking & git integration | Pre-commit hook | ✅ |

**Usage:**
- Pre-commit hook: Automatically runs on `git commit` (install: `./ops/scripts/install-git-hooks.sh`)
- Coverage reports: `./ops/scripts/coverage.sh [--html|--lcov|--open]`
- Scheme formatting: `./ops/scripts/check-scheme-format.sh [--fix]`

## High-Priority Candidates (Integrated)

| Tool | Language | Primary Purpose | Automation Trigger | Status |
|------|----------|-----------------|-------------------|--------|
| typos | All | Spell checking source code | Pre-commit hook + CI | ✅ Integrated |
| cargo-deny | Rust | Blocks bad licenses/duplicates | CI Pipeline | ✅ Integrated |
| nextest | Rust | Faster, structured testing | CI (replaces cargo test) | ✅ Integrated |
| cargo-mutants | Rust | Verifies tests aren't tautological | Manual (./ops/scripts/mutants.sh) | ✅ Installed |
| cargo-semver-checks | Rust | Prevents API breakage | Manual (./ops/scripts/semver-checks.sh) | ✅ Installed |

## Medium-Priority Candidates

| Tool | Language | Primary Purpose | Automation Trigger | Status |
|------|----------|-----------------|-------------------|--------|
| git-cliff | Git | Generates CHANGELOG.md | Manual (./ops/scripts/changelog.sh) | ✅ Installed |
| cargo-benchcmp | Rust | Benchmark regression detection | CI (on benchmark changes) | 📋 Candidate |

## Scheme-Specific Candidates

| Tool | Language | Primary Purpose | Automation Trigger | Status |
|------|----------|-----------------|-------------------|--------|
| Scheme test runner | Scheme | Integration with standard test framework | CI / Manual | 📋 Candidate |
| Scheme linter | Scheme | Code quality checks beyond formatting | Pre-commit | 📋 Candidate |

## The Fold-Specific Tools

| Tool | Language | Primary Purpose | Automation Trigger | Status |
|------|----------|-----------------|-------------------|--------|
| Block store validator | Rust | Verify .store/ integrity | Manual (`./ops/scripts/store-validator.sh`) | ✅ Integrated |

## The Fold-Specific Candidates

| Tool | Language | Primary Purpose | Automation Trigger | Status |
|------|----------|-----------------|-------------------|--------|
| Schema validator | Scheme | Validate block schema compliance | CI | 📋 Candidate |
| Primitive cost verifier | Scheme | Ensure fuel costs match complexity | CI | 📋 Candidate |

---

## Tool Details

### cargo-mutants
**Purpose:** Mutation testing - modifies code to verify tests actually catch bugs
**Why useful:** Ensures test suite isn't just checking happy paths or tautologies
**Tradeoff:** Very slow (can take hours on large codebases)
**Integration point:** Run on PRs for critical modules, not every commit

### cargo-deny
**Purpose:** Dependency auditing for licenses, security advisories, and duplicate dependencies
**Why useful:** Prevents legal issues and bloated dependency trees
**Tradeoff:** Requires configuration and maintenance of allow/deny lists
**Integration point:** CI pipeline (fast check)

### cargo-semver-checks
**Purpose:** Verifies public API changes don't break semver guarantees
**Why useful:** Critical for library code; prevents accidental breaking changes
**Tradeoff:** Only useful for public APIs (less relevant for binaries)
**Integration point:** Pre-release checks, tag/release workflow

### typos
**Purpose:** Fast source code spell checker (catches common typos in code/comments)
**Why useful:** Catches embarrassing typos before they ship
**Tradeoff:** False positives for domain-specific terms (needs config)
**Integration point:** Pre-commit or CI (very fast)

### git-cliff
**Purpose:** Automatic CHANGELOG generation from conventional commits
**Why useful:** Eliminates manual changelog maintenance
**Tradeoff:** Requires conventional commit format
**Integration point:** Release tagging workflow

### nextest
**Purpose:** Modern Rust test runner with better UX, parallelization, and output
**Why useful:** Faster test runs, better failure reporting, test retries
**Tradeoff:** Replaces `cargo test` (new tool to learn)
**Integration point:** CI pipeline, local dev workflow

### cargo-audit
**Purpose:** Security vulnerability scanner for Rust dependencies
**Why useful:** Catches known security vulnerabilities in dependencies before they reach production
**Tradeoff:** Requires periodic database updates; may flag vulnerabilities in transitive deps you can't easily fix
**Integration point:** Manual checks before releases (`./ops/scripts/audit.sh`)

### cargo-watch
**Purpose:** Auto-rebuild and re-run tests/commands when files change
**Why useful:** Immediate feedback during development; catches errors as you type
**Tradeoff:** Can be resource-intensive on large projects; may trigger rebuilds too frequently
**Integration point:** Local development workflow (`./ops/scripts/watch.sh`)

### Block Store Validator
**Purpose:** Verifies integrity of The Fold's content-addressed block store
**Why useful:** Detects corruption, missing references, orphaned blocks, and decode errors
**Tradeoff:** Currently reports many decode errors on existing store (may indicate format evolution)
**Integration point:** Manual health checks (`./ops/scripts/store-validator.sh`)
**Checks performed:**
- Hash integrity: Block content matches address
- Reference integrity: All refs point to existing blocks
- Decode integrity: All blocks can be decoded
- Orphan detection: Blocks not reachable from pins/heads
- Pin/head consistency: Referenced blocks exist

---

## Current Coverage Baseline

As of commit `0f12d5a` (2025-12-29):

```
Filename                      Regions    Missed Regions     Cover   Functions  Missed Functions  Executed       Lines      Missed Lines     Cover
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
fold-bench.rs                      27                27     0.00%           3                 3     0.00%          51                51     0.00%
fold-daemon.rs                     50                50     0.00%           4                 4     0.00%         118               118     0.00%
fold-repl.rs                       26                26     0.00%           2                 2     0.00%          50                50     0.00%
block.rs                           88                13    85.23%          20                 2    90.00%         200                26    87.00%
cas.rs                             73                 3    95.89%          14                 0   100.00%         150                 4    97.33%
eval.rs                           241                46    80.91%          25                 3    88.00%         424                45    89.39%
mod.rs                              5                 2    60.00%           4                 1    75.00%          17                 5    70.59%
sha256.rs                          70                 0   100.00%          10                 0   100.00%         185                 0   100.00%
prim.rs                          1026               510    50.29%          89                49    44.94%        1461               727    50.24%
fold_load.rs                      103                79    23.30%          13                 9    30.77%         190               149    21.58%
fold_lower.rs                      47                 4    91.49%           7                 1    85.71%          73                 5    93.15%
fold_parse.rs                     181                24    86.74%          25                 2    92.00%         311                47    84.89%
fold_print.rs                      84                77     8.33%          18                15    16.67%         170               162     4.71%
fold_run.rs                        76                11    85.53%          12                 1    91.67%         121                16    86.78%
mod.rs                              2                 0   100.00%           2                 0   100.00%           6                 0   100.00%
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
TOTAL                            2099               872    58.46%         248               92    62.90%        3527              1405    60.16%
```

**Overall: 50.48% line coverage** (per summary output)

Low-coverage modules needing attention:
- `fold_print.rs`: 4.71%
- `fold_load.rs`: 21.57%
- `prim.rs`: 50.24% (but largest module - highest impact)
- `fold-bench.rs`, `fold-daemon.rs`, `fold-repl.rs`: 0% (binaries, not tested)

---

## Installation & Usage

### Installing Tools

```bash
# Already integrated
./ops/scripts/install-git-hooks.sh    # Install pre-commit hooks
sudo npm install -g scmindent          # Scheme formatter
cargo install cargo-llvm-cov           # Coverage tool
cargo install cargo-audit              # Security vulnerability scanner
cargo install cargo-watch              # Auto-rebuild on file changes

# Additional tools
cargo install cargo-mutants            # Mutation testing
cargo install cargo-deny               # Dependency auditing
cargo install cargo-semver-checks      # API compatibility
cargo install typos-cli                # Spell checker
cargo install git-cliff                # Changelog generator
cargo install cargo-nextest            # Modern test runner
```

### Running Tools

```bash
# Integrated tools (run automatically)
git commit                                      # Triggers: bd sync, typos, scmindent, cargo fmt, clippy

# Testing
./ops/scripts/nextest.sh                        # Run tests with nextest (faster)
cargo test                                       # Traditional test runner (slower)
./ops/scripts/coverage.sh                       # Generate coverage summary
./ops/scripts/coverage.sh --html                # Generate HTML coverage report

# Code Quality
typos                                           # Check spelling across all files
typos --write-changes                           # Auto-fix spelling errors
./ops/scripts/check-scheme-format.sh            # Check Scheme indentation
./ops/scripts/check-scheme-format.sh --fix      # Auto-fix Scheme indentation
./ops/scripts/deny.sh                           # Check dependencies/licenses/security
cargo fmt                                        # Format Rust code
cargo clippy                                     # Lint Rust code

# Security & Development
./ops/scripts/audit.sh                          # Check for security vulnerabilities
./ops/scripts/watch.sh                          # Auto-rebuild on file changes
./ops/scripts/watch.sh -x "run --bin fold-repl" # Watch and run specific binary

# The Fold-Specific
./ops/scripts/store-validator.sh                # Validate .store/ integrity
./ops/scripts/store-validator.sh /path/to/store # Validate specific store

# Release Tools
./ops/scripts/changelog.sh                      # Generate CHANGELOG.md
./ops/scripts/changelog.sh --unreleased         # Show unreleased changes
./ops/scripts/changelog.sh --tag v1.0.0         # Generate changelog and tag
./ops/scripts/semver-checks.sh                  # Check for API breaking changes

# Advanced Testing (slow)
./ops/scripts/mutants.sh                        # Mutation testing (VERY SLOW!)
./ops/scripts/mutants.sh --list                 # List potential mutants
./ops/scripts/mutants.sh --file <path>          # Test specific file
```

---

## Next Steps

To integrate additional tools:

1. **Immediate win**: `typos` - Fast, low false-positive rate with config
2. **CI pipeline**: `cargo-deny` + `nextest` - Better CI reliability and speed
3. **Release workflow**: `cargo-semver-checks` + `git-cliff` - Professional release process
4. **Quality gate**: `cargo-mutants` - For critical modules only (too slow for everything)

See integration scripts in `ops/scripts/` for examples.
