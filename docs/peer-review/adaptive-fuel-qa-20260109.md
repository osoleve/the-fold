# Adaptive Fuel Allocation QA Review

**Reviewed by:** Gemini 3 Pro Preview
**Date:** 2026-01-09
**Files Reviewed:**
- `lattice/fp/control-systems/kalman.ss`
- `shell/fuel/adaptive-allocator.ss`
- `shell/fuel/adaptive-hof.ss`
- `lattice/fp/control-systems/test-kalman.ss`

---

## 1. Correctness of Kalman Filter Math

The core Kalman filter implementation in `lattice/fp/control-systems/kalman.ss` is mathematically sound and well-tested.

- **Log-Space Implementation:** The decision to use a log-space filter is correct for fuel costs, which are strictly positive and often heavy-tailed.
- **Update Logic:** The "predict-then-update" cycle in `kalman-estimate` correctly handles the assimilation of new observations.
- **Cost Accumulation:** In `adaptive-hof.ss`, `adaptive-eval-element` accumulates cost across retries (`total-cost`). This means the filter learns the *effective cost including retry overhead*, not just the raw execution cost. While this technically biases the "cost" estimate upwards, it is practically desirable for an allocator as it encourages a safety margin that avoids future retries.

## 2. Edge Cases and Potential Bugs

- **Quoting in `adaptive-map`:** The expression construction `(call (quote ,f) (quote ,elem))` assumes `f` is a symbol or a self-evaluating literal. If `f` is a complex closure or expression, this quoting might result in runtime errors depending on `eval-expr`'s handling of `call`.
- **Retry Overhead:** The geometric backoff (doubling fuel: 100, 200, 400...) is robust but can lead to significant wasted work (up to ~3x the actual cost) if the initial estimate is far too low.
- **Infinite Loops:** Handled correctly by `max-retries`, returning an error rather than hanging indefinitely.

## 3. Performance Issues

- **History Management (O(N)):** `allocator-push-observation` uses `take` to maintain a fixed-size history. The `take` implementation is O(N), making the observation step O(N) where N is `max-history`. While `*default-max-history*` is 100, larger histories will degrade performance linearly. A ring buffer or deque would be O(1).
- **Native Timing:** `adaptive-map-native` relies on `current-time` and `time-diff-micros`. While sufficient for coarse measurements, it may introduce overhead for very fast operations.

## 4. API Design and Usability

- **Functional State:** The `allocator` is immutable/functional, which is excellent for predictability and testing.
- **Options Parsing:** `adaptive-map` expects options as a single list (e.g., `(adaptive-map f xs '(initial-estimate . 500))`) rather than standard Scheme keyword arguments or a flattened property list. This is functional but slightly non-idiomatic.
- **Observability:** The `allocator-summary` and return values (returning stats alongside results) provide good observability into the system's behavior.

## 5. Test Coverage Gaps

- **Core Math:** **Good.** `lattice/fp/control-systems/test-kalman.ss` provides strong coverage for the underlying math.
- **Integration:** **Missing.** There are no direct tests for `shell/fuel/adaptive-allocator.ss` or `shell/fuel/adaptive-hof.ss`. The system lacks integration tests verifying that:
  - The allocator actually converges on a stable cost.
  - Retries function as expected when fuel runs out.
  - `adaptive-map` handles errors or partial results correctly.

---

## Actionable Feedback

1. **Add Integration Tests:** Create `shell/fuel/tests/test-adaptive-allocator.ss` to verify `adaptive-eval-element` logic (retries, convergence) and `adaptive-map`.
2. **Optimize History:** Replace the `take` implementation in `allocator-push-observation` with a more efficient structure (e.g., check length before consing, or use a vector ring-buffer) if history grows beyond small constants.
3. **Refine Quoting:** Verify `(call (quote ,f) ...)` works for non-symbol functions. Consider passing `f` directly if the evaluator supports it.
4. **Standardize Options:** Consider supporting a more idiomatic options interface (e.g., keyword arguments or a variadic property list) for `adaptive-map`.
