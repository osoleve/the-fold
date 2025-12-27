# Unified Test Framework

## Overview

The unified test framework (`test-framework.ss`) provides a consistent API for writing tests across all core modules.

## Quick Start

### Basic Test Example

```scheme
(load "block.ss")  ; Load module to test

(display "Block System Tests\n")
(display "===================\n\n")

(test-group block-construction
  (define-test basic-block
    (let ([blk (make-block 'greeting (string->utf8 "hello") (vector))])
      (assert-equal 'greeting (block-tag blk))
      (assert-equal 5 (bytevector-length (block-payload blk))))))
```

## API Reference

### Test Definition

- `(define-test name body ...)` - Define and run a test immediately
- `(test-group name tests ...)` - Group related tests together

### Assertions

- `(assert-equal expected actual)` - Check equality
- `(assert-true expr)` - Check truthfulness
- `(assert-false expr)` - Check falseness
- `(assert-error thunk)` - Verify expression raises error
- `(assert-ok result)` - Verify result is `(ok ...)`

### Test Runners

- `(run-all-tests)` - Run all registered tests (prints summary)
- `(run-tests 'group-name)` - Run specific test group

### Legacy Compatibility

- `(test name expected actual)` - Legacy test function (still supported)

## Test Execution

Tests defined with `define-test` run immediately when defined. This matches the original pattern where tests execute inline as the file loads.

## Features

- ✓ Clear pass/fail reporting with checkmarks
- ✓ Expected vs actual output on failure
- ✓ Exception handling and reporting
- ✓ Group organization
- ✓ Summary statistics
- ✓ Backward compatible with existing tests

## Example Output

```
Block System Tests
===================

    ✓ basic-block
    ✓ round-trip-simple
    ✓ round-trip-with-refs
```

## Migration Guide

To migrate an existing test file:

1. Add display header:
   ```scheme
   (display "Module Tests\n")
   (display "=============\n\n")
   ```

2. Wrap tests in groups:
   ```scheme
   (test-group feature-name
     (define-test test-case-1
       (assert-equal expected actual))
     ...)
   ```

3. Replace manual assertions with framework assertions:
   - `(if (equal? ...) ...)` → `(assert-equal ...)`
   - Manual error checking → `(assert-error ...)`

See `test-block.ss` for a complete example.
