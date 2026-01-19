# Lessons Learned: Expression Construction for DSL Evaluation

**Date:** 2026-01-10
**Project:** Adaptive fuel HOFs (fold-vkoc, fold-y7gk)

## Executive Summary

Fixed two issues in `boundary/fuel/adaptive-hof.ss` related to constructing expressions for Core DSL evaluation. Key insight: the boundary between Scheme orchestration code and Core DSL evaluation requires careful handling of quoting, environments, and API ergonomics.

---

## Quoting vs Direct Evaluation

### The Problem

The original code used:
```scheme
(define (make-call elem)
  `(call (quote ,f) (quote ,elem)))
```

This assumes `f` is a quotable datum (symbol or self-evaluating literal). But if `f` is a Core DSL expression like `(fn (x) body)`, quoting it produces a *list*, not a closure.

### How Core Evaluation Works

```scheme
;; (quote X) → returns X as data
(eval-expr '(quote (fn (x) x)) env fuel)
;; Returns: (fn (x) x)  ← a LIST, not a closure

;; (fn ...) → creates a closure
(eval-expr '(fn (x) x) env fuel)
;; Returns: (closure (x) x env)  ← a CLOSURE
```

The `closure?` predicate checks for `(and (pair? v) (eq? (car v) 'closure))`, so quoted lambdas fail.

### The Fix

Pass `f` directly (unquoted) so it gets evaluated:
```scheme
(define (make-call elem)
  `(call ,f (quote ,elem)))  ; f evaluated, elem quoted
```

Now:
- Lambda expressions like `(fn (x) body)` → evaluated to closures
- Variable names like `my-func` → looked up in environment
- Only runtime values (elements) are quoted

### Rule

> When constructing expressions for DSL evaluation: **quote data, don't quote code**.

---

## Environment Threading

### The Problem

The original code always used `empty-env`:
```scheme
(adaptive-eval-element alloc (make-call elem) empty-env max-retries)
```

If `f` is a variable name bound elsewhere, lookup fails.

### The Fix

Add an `env` option and thread it through:
```scheme
[env (get-opt opts 'env empty-env)]
;; ...
(adaptive-eval-element alloc (make-call elem) env max-retries)
```

### Rule

> When evaluating expressions that may reference variables, always provide a way to pass the environment.

---

## Options API Design

### The Problem

Original API required verbose alist syntax:
```scheme
(adaptive-map f xs '((initial-estimate . 500) (confidence . 3.0)))
```

This is syntactically heavy compared to flat plist:
```scheme
(adaptive-map f xs '(initial-estimate 500 confidence 3.0))
```

### The Fix

Detect format and normalize:
```scheme
(define (normalize-opts opts)
  (cond
   [(null? opts) '()]
   ;; Alist: first element is (symbol . value)
   [(and (pair? (car opts)) (symbol? (caar opts)))
    opts]
   ;; Plist: first element is a symbol
   [(symbol? (car opts))
    (plist->alist opts)]
   [else
    (error 'normalize-opts "invalid options format" opts)]))
```

### Detection Heuristic

| First Element | Format |
|----|----|
| `(key . val)` | Alist |
| `symbol` | Plist |

This works because:
- Alist entries are pairs with symbol cars
- Plist entries alternate symbol keys and values
- A symbol followed by a value can't be confused with `(symbol . value)`

### Backward Compatibility

Both formats now work:
```scheme
;; Old style (still works)
(adaptive-map f xs '((initial-estimate . 500)))

;; New style (now supported)
(adaptive-map f xs '(initial-estimate 500))
```

### Rule

> When designing options APIs, support multiple idiomatic formats while normalizing internally to a canonical form.

---

## External QA Value

### How These Issues Were Found

The Gemini QA review (documented in `docs/peer-review/adaptive-fuel-qa-20260109.md`) systematically analyzed the code and identified:

1. **fold-vkoc**: Potential runtime errors from quoting complex closures
2. **fold-y7gk**: Non-idiomatic API requiring verbose alist syntax

These were logged as beads issues for later resolution.

### The Pattern

```
Code written → External QA review → Issues logged → Prioritized → Fixed
```

This is more effective than trying to catch all issues during initial development.

### Rule

> External review (human or AI) catches issues that authors miss. The review → issue → fix cycle is valuable.

---

## Testing Strategy

### Quick Validation

After making changes, a quick inline test validates the fix:
```scheme
;; Test plist normalization
(equal? (normalize-opts '(initial-estimate 500 confidence 3.0))
        '((initial-estimate . 500) (confidence . 3.0)))  ; → #t

;; Test alist passthrough
(equal? (normalize-opts '((initial-estimate . 500)))
        '((initial-estimate . 500)))  ; → #t
```

### Integration Testing

The existing test suite (`boundary/fuel/tests/test-adaptive-allocator.ss`) validates that the native functions still work correctly after refactoring.

### Rule

> Quick inline tests + existing integration tests = confidence without test-writing overhead.

---

## Key Takeaways

1. **Quote data, not code** - When building expressions for DSL evaluation, only quote runtime values; let code expressions be evaluated.

2. **Thread environments** - Provide options for passing evaluation environments when building expressions with variable references.

3. **Normalize options internally** - Support multiple input formats (alist, plist) and convert to canonical form early.

4. **External QA finds real issues** - Systematic review catches subtle problems; logging them as issues ensures they get fixed.

5. **Detect format, don't require it** - Heuristic detection of input formats improves API ergonomics without breaking backward compatibility.
