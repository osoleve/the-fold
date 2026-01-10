# Par Concurrency Tests QA Review

**Reviewer:** Gemini 3 Pro
**Date:** 2026-01-10
**File:** `core/lang/test-par-pseq.ss`
**Focus:** Tests 25-35 (Concurrency-Specific Tests)

## Summary

The tests verify value correctness but **fail to verify actual concurrency**. A sequential implementation of `par` would pass every test.

## Specific Issues

### 1. No Verification of Concurrency

- **Test 25 (Timing):** Logs timing differences but **does not assert** that `par` is faster than `pseq`. The test passes even if `par` is slower or sequential.
- **Test 32 (Async behavior):** Checks that the fast path returns the correct value but **does not assert** that it returns *quickly* (before the slow path finishes).

### 2. False Assertions (Fuel)

- **Test 26:** The comment claims to verify execution "via fuel consumed," but the assertion only checks the return value (`191`), ignoring the fuel counter in the result tuple.

### 3. Missing Edge Cases

- **Side Effects:** No tests involve shared mutable state or side effects to prove that operations are interleaving or running in parallel.
- **Resource Leaks:** Test 33 (Error Isolation) shows background errors are ignored, but doesn't check if the background thread is properly cleaned up or if the error is reportable.

### 4. Reliability

- **Machine Dependence:** `make-slow-expr` relies on `fib(n)` for delays, which varies wildly by hardware.

## Recommendations

1. Add assertions for the `fuel` component of the result to prove both branches executed.
2. Add assertions for `elapsed-time` in Tests 25 & 32 to fail if execution is sequential.
3. Use a synchronization primitive (if available in the host language) or observable side-effects to prove interleaving.

## Response

The review raises valid points about test rigor. However, some constraints apply:

1. **Timing assertions are inherently flaky** in CI environments. The tests log timing for informational purposes but avoid hard assertions that could cause spurious failures on loaded systems.

2. **The evaluator is pure** — side effects aren't available within the interpreted language. Fuel is duplicated in parallel mode, so both branches consume their own fuel independently, making fuel comparison less meaningful than it might seem.

3. **Thread cleanup is handled by Chez Scheme's runtime** — not something we can directly test at the evaluator level.

4. **The tests do prove correctness** which is the primary goal. Timing information provides observational evidence of parallelism without causing flaky test failures.

Consider adding an optional `--assert-parallelism` mode for local testing that enables strict timing assertions.
