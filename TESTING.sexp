;;; TESTING.sexp — Testing Guide for The Fold
;;;
;;; How to write, run, and organize tests in The Fold.

((title . "Testing in The Fold")
 (description . "
The Fold has a comprehensive testing infrastructure spanning
the pure core (fabric/) and the shell (thimble/).

Tests are first-class citizens. Every module has a corresponding
test file. Tests run fast, provide clear output, and catch regressions.
")
 (structure . (
   ((layer . "Core Tests")
    (location . "fabric/stitches/")
    (pattern . "test-<module>.ss")
    (runner . "fabric/stitches/run-tests.ss")
    (framework . "fabric/stitches/test-framework.ss")
    (documentation . "fabric/stitches/TEST-FRAMEWORK.md")
    (examples . (
      "test-block.ss       - Block construction & serialization"
      "test-types.ss       - Type system"
      "test-infer.ss       - Type inference"
      "test-eval.ss        - Evaluator"))
    (commands . (
      "Run all core tests:"
      "  scheme --script fabric/stitches/run-tests.ss"
      ""
      "Run single test file:"
      "  scheme --script fabric/stitches/test-block.ss")))
   ((layer . "Shell Tests")
    (location . "thimble/")
    (pattern . "test-<module>.ss")
    (runner . "thimble/run-tests.ss")
    (examples . (
      "test-fs.ss          - Filesystem operations"
      "test-text.ss        - Text encoding/normalization"
      "test-validate.ss    - Input validation"))
    (commands . (
      "Run all thimble tests:"
      "  scheme --script thimble/run-tests.ss")))
   ((layer . "Integration Tests")
    (location . "tests/integration/")
    (pattern . "test-<feature>.ss")
    (description . "End-to-end tests spanning fabric and thimble"))
   ((layer . "Unified Runner")
    (file . "test-all.ss")
    (description . "Runs core + shell + integration tests")
    (command . "scheme --script test-all.ss"))))
 (writing-tests . "
## Test Framework

The Fold uses a unified test framework with clear assertions:

```scheme
;;; Load framework
(load \"fabric/stitches/test-framework.ss\")

;;; Define test groups
(test-group arithmetic
  (define-test addition
    (assert-equal 3 (+ 1 2)))

  (define-test subtraction
    (assert-equal 1 (- 3 2)))

  (define-test division-by-zero
    (assert-error (/ 1 0))))

;;; Tests run immediately when defined
```

## Available Assertions

- (assert-equal expected actual)      - Values must be equal
- (assert-true expr)                  - Expression must be true
- (assert-false expr)                 - Expression must be false
- (assert-error expr)                 - Expression must raise error
- (assert-predicate pred val)         - Predicate must hold

See fabric/stitches/TEST-FRAMEWORK.md for complete reference.

## Test File Template

```scheme
;;; fabric/stitches/test-mymodule.ss
;;;
;;; Tests for mymodule.ss

;;; Load framework (if not in core tests)
(unless (top-level-bound? 'define-test)
  (load \"test-framework.ss\"))

;;; Load module under test
(load \"mymodule.ss\")

(display \"My Module Tests\\n\")
(display \"===============\\n\\n\")

;;; Test groups
(test-group construction
  (define-test basic-case
    (assert-equal expected (my-function input))))

(test-group edge-cases
  (define-test empty-input
    (assert-equal default (my-function '()))))
```

## Legacy Pattern (Still Supported)

Some tests use a manual pattern:

```scheme
(define (test name expected actual)
  (display \"  \")
  (display name)
  (display \": \")
  (if (equal? expected actual)
      (display \"\u2713\")
      (begin
        (display \"\u2717\\n    expected: \")
        (display expected)
        (display \"\\n    got: \")
        (display actual)))
  (newline))

(test \"addition\" 3 (+ 1 2))
```

This works but the framework is preferred for new tests.
")
 (running-tests . "
## Run Everything

```bash
scheme --script test-all.ss
```

This runs:
1. Core tests (fabric/stitches/)
2. Shell tests (thimble/)
3. Integration tests (tests/integration/)

## Run Core Tests Only

```bash
scheme --script fabric/stitches/run-tests.ss
```

Runs 16+ test files in dependency order:
- Layer 0: prelude, sha256
- Layer 1: block, cas
- Layer 2: normalize, prim, parse
- Layer 3: types, kinds
- Layer 4: infer, resolve, annotate
- Layer 5: eval, typed-eval
- Layer 6: compile
- Layer 7: error

## Run Shell Tests Only

```bash
scheme --script thimble/run-tests.ss
```

## Run Single Test File

```bash
scheme --script fabric/stitches/test-types.ss
scheme --script thimble/test-fs.ss
```

## Run Specific Test Group

Not currently supported - file granularity only.
Feature request: Add test filtering to framework.
")
 (test-output . "
## Successful Test

```
  \u2713 addition
  \u2713 subtraction
  \u2713 multiplication
```

## Failed Test

```
  \u2717 division
    expected: 2
    got: 3
```

## Summary

```
\u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557
\u2551       TEST SUMMARY        \u2551
\u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d
  Total:  16 test files
  Passed: 16
  Failed: 0

\u2713 All core tests passed!
```
")
 (for-players . "
Players don't need to write tests for playpen creations.

If you want to test your creation:

```bash
# Quick REPL test
scheme -q << 'EOF'
(load \"playpen/creations/my-game.ss\")
(play-game)
EOF

# Interactive test
scheme
> (load \"playpen/creations/my-game.ss\")
> (help-commands)
> (test-function arg)
```

That's it! No framework needed for play.
")
 (for-builders . "
Builders MUST write tests for thimble/ utilities.

## When Adding a Utility

1. Create: thimble/my-utility.ss
2. Create: thimble/test-my-utility.ss
3. Run: scheme --script thimble/test-my-utility.ss
4. Verify it passes
5. Add to thimble/run-tests.ss (if appropriate)

## Testing IO Code

Use temporary files and cleanup:

```scheme
(define test-dir \".test-tmp/\")

(define (setup)
  (make-directory test-dir))

(define (teardown)
  (delete-directory test-dir))

(test-group filesystem
  (setup)

  (define-test write-and-read
    (write-file (string-append test-dir \"test.txt\") \"content\")
    (assert-equal \"content\" (read-file (string-append test-dir \"test.txt\"))))

  (teardown))
```

## Testing REPL Commands

Mock user input with string ports:

```scheme
(define (test-command input expected-output)
  (let* ([in-port (open-input-string input)]
         [out-port (open-output-string)]
         [result (with-input-from-port in-port
                   (lambda ()
                     (with-output-to-port out-port
                       (lambda () (command-function)))))])
    (assert-equal expected-output (get-output-string out-port))))
```
")
 (for-shepherds . "
Shepherds maintain the test infrastructure.

## Test Philosophy

- Tests are documentation
- Tests run fast (< 5 seconds for full suite)
- Tests are deterministic (no random failures)
- Tests catch regressions
- Tests guide design

## Adding to Core

When adding to fabric/stitches/:

1. Write test first (TDD encouraged)
2. Implement module
3. Run tests: scheme --script fabric/stitches/test-<module>.ss
4. Add to run-tests.ss in correct dependency layer
5. Verify full suite: scheme --script fabric/stitches/run-tests.ss

## Continuous Integration

Tests run automatically on:
- Every PR (via GitHub Actions)
- Pre-commit hook (local)
- Manual: ./test-all.ss

CI enforces:
- All tests pass
- No uncommitted test files
- Test coverage (future: coverage threshold)

## Test-Driven Refactoring

When refactoring:
1. Run tests before changes (establish baseline)
2. Make changes
3. Run tests after changes
4. If failures: fix or update tests
5. Commit only when all tests pass

Tests are the contract. If tests pass, refactoring is safe.
")
 (ci-integration . "
## What CI Runs

GitHub Actions (.github/workflows/test.yml):

```yaml
- Run: scheme --script test-all.ss
- Check: Exit code 0 (all pass)
- Report: Test summary
```

## Running Locally What CI Runs

```bash
# Same command CI uses
scheme --script test-all.ss

# Exit code check
echo $?  # Should be 0
```

## Pre-commit Hook

Install:
```bash
cp .git/hooks/pre-commit.sample .git/hooks/pre-commit
# Edit to run: scheme --script test-all.ss
chmod +x .git/hooks/pre-commit
```

Now tests run before every commit.
")
 (coverage . "
Test coverage tracking is available via:

```bash
scheme --script thimble/coverage.ss
```

This generates a report showing:
- Which functions are tested
- Which branches are covered
- Uncovered code paths

Future: CI will enforce minimum coverage threshold.
")
 (benchmarking . "
Performance regression testing:

```bash
scheme --script thimble/benchmark.ss
```

Tracks:
- Fuel consumption (core operations)
- Memory allocation
- Execution time

Prevents performance regressions.
")
 (faq . (
   ((q . "Do I need tests for playpen creations?")
    (a . "No, playpen is for exploration. Tests optional."))
   ((q . "Can I run just one test in a file?")
    (a . "Not yet. File granularity only. Feature request welcome!"))
   ((q . "What if a test needs the daemon?")
    (a . "Mock the daemon or use integration tests in tests/integration/"))
   ((q . "How do I test randomness?")
    (a . "Use a fixed seed: (random-seed 42)"))
   ((q . "Can tests modify files?")
    (a . "Yes, but cleanup after! Use .test-tmp/ and delete it."))
   ((q . "What if tests are slow?")
    (a . "Core tests should be < 5s total. If slow, consider mocking IO.")))))
