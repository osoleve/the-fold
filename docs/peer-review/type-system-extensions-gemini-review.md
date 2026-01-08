# Type System Extensions Peer Review

**Reviewer:** Gemini 3 Pro
**Date:** 2026-01-08
**Files Reviewed:**
- `core/types/rank-n.ss` (Rank-N Polymorphism)
- `core/types/type-families.ss` (Associated Type Families)
- `core/types/dep-match.ss` (Dependent Pattern Matching)

---

## Executive Summary

The implementation demonstrates a sophisticated understanding of advanced type systems, particularly in the structural handling of Rank-N types and the grammar for type families. However, there are **critical soundness issues** in the dependent pattern matching (unification consistency) and **correctness bugs** in the evaluation strategies for type families (stuck term handling) and pattern compilation (ignoring subsequent clauses).

---

## File 1: `core/types/rank-n.ss` (Rank-N Polymorphism)

### 1. Correctness

The implementation follows the Dunfield & Krishnaswami bidirectional approach reasonably well. The rank calculation and polarity tracking in `type-rank-with` correctly identify the "negative" positions for quantifiers.

- **Logic Check:** The `subsumes-with` function correctly handles the standard Rank-N rules:
  - `forall-L`: Instantiates left side with fresh unification variables.
  - `forall-R`: Skolemizes right side (checking that skolems don't escape).
  - **Contra-variance:** Correctly swaps order for function arguments `(subsumes-with (car ps2) (car ps1) ...)`.

### 2. Potential Bugs & Soundness

#### CRITICAL: Naive Substitution (Capture Avoidance)

- **Location:** `apply-subst-rankn` (Line 270)
- **Issue:** The substitution logic `(filter (lambda (p) (not (memq (car p) vars))) s)` handles shadowing but **does not rename bound variables** to avoid capturing free variables in the substituted range.
- **Example:** Substituting `[b/a]` into `forall b. a -> b` results in `forall b. b -> b` (variable capture), whereas it should be `forall c. b -> c`. This breaks preservation.

#### Impredicative Unification

- **Location:** `impredicative-unify` (Line 501)
- **Issue:** Impredicative unification is generally undecidable. The provided algorithm is a structural heuristic. It lacks a rigorous "occurs check" that works across quantifier boundaries, potentially leading to infinite loops or unsound equivalence (e.g., `a ~ forall x. a`).

### 3. Suggestions

- **Implement proper capture-avoiding substitution** (using De Bruijn indices or nominal sets) before relying on this for core type checking.
- **Restrict Impredicativity:** Unless full impredicativity is strictly required, restrict instantiation to monotypes by default and require explicit type applications for polytypes, which is the standard solution in languages like Haskell (GHC) to maintain decidability.

---

## File 2: `core/types/type-families.ss` (Associated Type Families)

### 1. Correctness

The definition of families and instances is syntactically sound. The "fuel" approach for reduction (`reduce-tf-fuel`) is a practical safeguard against non-terminating type families.

### 2. Edge Cases & Bugs

#### Stuck Families Block Argument Reduction

- **Location:** `reduce-tf-fuel` (Line 272)
- **Issue:** If `type-family-app?` is true, but no instance matches (the `else` branch of the inner `if`), the function returns `type` **without reducing the arguments**.
- **Impact:** If you have `Elem (List Int)` and `Elem` is defined, it works. But if you have `Elem (AliasForList Int)` where `AliasForList` reduces to `List`, the current logic will see `Elem` is a family, fail to match `AliasForList` (because it hasn't been reduced yet), and return the stuck term.
- **Fix:** In the "no instance found" branch, you must recursively call `reduce-tf-fuel` on the arguments (similar to the `else` branch at line 303).

#### Instance Overlap

- **Location:** `tf-registry-lookup-instance` (Line 219)
- **Issue:** It returns the first match found using `assoc-with`. The system does not check for overlapping instances or specificity. This makes type checking order-dependent on how files were loaded.

### 3. Suggestions

- **Normalize arguments first:** Change the reduction strategy to "outside-in" or "inside-out" consistently. Typically, you want to reduce arguments to head-normal form *before* attempting to match a type family instance.
- **Add Overlap Check:** When adding an instance, check if it overlaps with existing ones and either reject it or implement specific selection rules (e.g., "most specific instance").

---

## File 3: `core/types/dep-match.ss` (Dependent Pattern Matching)

### 1. Correctness

This file contains the most significant issues. While the grammar and parsing are fine, the core logic for unification and compilation is critically flawed.

### 2. Potential Bugs & Soundness

#### CRITICAL: Unification Inconsistency

- **Location:** `unify-types` (Line 261)
- **Issue:** The unifier returns a list of equations but **never checks them for consistency**.
- **Example:** Unifying `(Vec n n)` against `(Vec 0 1)`. The loop will produce `((n . 0) (n . 1))`. This is a contradiction, but `unify-types` returns it as valid. `apply-refinement` will likely just use the first binding, effectively proving `0 = 1`.
- **Fix:** The unifier must maintain a current substitution `theta`. Every new equation must be solved *under* `theta`, and the result composed. If a conflict arises (e.g., `0 ~ 1`), it must return `'mismatch`.

#### CRITICAL: Compilation Ignores Clauses

- **Location:** `compile-dep-match` (Line 500)
- **Issue:** `(let* ([first-clause (car clauses)] ...)`
- **Impact:** The compiler **only looks at the first clause** of the match expression. It ignores all subsequent clauses. A match expression with multiple cases will simply fail or generate wrong code if the first case doesn't match. It needs to generate a proper decision tree handling fall-throughs.

#### Incomplete Pattern Coverage

- **Location:** `coverage-check` (Line 333)
- **Issue:** It only checks the top-level constructor. It does not verify nested patterns. `(match (x : List (List A)) [[cons (cons a as) bs] ...])` would pass coverage despite missing `(cons nil ...)` and `nil` cases.

### 3. Suggestions

- **Rewrite `unify-types`:** It needs to be a proper unification algorithm (e.g., Robinson's) that propagates substitutions and detects conflicts immediately.
- **Fix `compile-dep-match`:** It must iterate through all clauses, grouping them by constructor to form a proper case tree (switch statement).
- **Implement Dot Pattern Checking:** The comment "In a real implementation, we'd check..." (Line 181) acknowledges a missing feature. For dependent matching to be sound, you *must* verify that the value provided matches the forced index.

---

## Summary of Recommendations

| Priority | Issue | File | Action Required |
|----------|-------|------|-----------------|
| **P1 (Soundness)** | Inconsistent unification | `dep-match.ss` | Fix `unify-types` to prevent deriving falsehoods from inconsistent indices |
| **P1 (Functionality)** | Single clause compilation | `dep-match.ss` | Fix `compile-dep-match` to handle more than one match clause |
| **P2 (Correctness)** | Stuck family reduction | `type-families.ss` | Fix `reduce-tf-fuel` to reduce arguments when the head family is stuck |
| **P2 (Safety)** | Variable capture | `rank-n.ss` | Implement capture-avoiding substitution |
| **P3 (Robustness)** | Instance overlap | `type-families.ss` | Add overlap detection or specificity-based selection |
| **P3 (Completeness)** | Nested coverage | `dep-match.ss` | Extend coverage checking to nested patterns |

---

*Review generated by Gemini 3 Pro on 2026-01-08*
