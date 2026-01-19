# The Fold's New Superpowers: Proof Sketching & Auto-Fusion

*A hands-on tour of fold-y2f and fold-lzr*

---

## Part 1: The Interactive Proof Sketcher (fold-y2f)

Ever wanted to prove that your monoid is actually a monoid? Or verify that
functor composition really does fuse? Now you can - interactively!

### Getting Started

```scheme
;; Load the proof REPL
(load "boundary/tools/proof-repl.ss")

;; Let's prove a classic: the monad left identity law
;; bind (pure x) f = f x

(sketch '(= (bind (pure x) f) (f x)))
```

You'll see:
```
╔══════════════════════════════════════════════════════════════════╗
║                     PROOF SKETCHER                               ║
╚══════════════════════════════════════════════════════════════════╝

  Goal 1 [open]: (bind (pure x) f) = (f x)

  Type (hint) for suggestions, (proof-help) for commands.
```

### Following the Hints

```scheme
(hint)
```

Output:
```
  Applicable laws for current goal:

    [1] monad-left-id
        (bind (pure (?a)) (?f)) → ((?f) (?a))
        Preview: (f x)
```

The sketcher found exactly the law we need! Apply it:

```scheme
(proof-apply 'monad-left-id)
```

```
  ✓ Applied monad-left-id

  Goal 1 [open]: (f x) = (f x)
```

Now we just need reflexivity:

```scheme
(proof-apply 'refl)
```

```
  ✓ Goal discharged by reflexivity

  No remaining goals!
```

Finalize the proof:

```scheme
(qed)
```

```
  ╔═══════════════════════════════════════╗
  ║         PROOF COMPLETE!               ║
  ╚═══════════════════════════════════════╝

  Theorem: (bind (pure x) f) = (f x)
  Steps: 2
  Laws used: monad-left-id, reflexivity
```

### A More Interesting Proof: Functor Composition

```scheme
(sketch '(= (fmap f (fmap g fa)) (fmap (compose f g) fa)))

(hint)
;; Suggests: functor-fuse (reverse direction)

;; But wait - we want to go the other way!
;; Use symmetry to flip the goal:
(proof-apply 'sym)

;; Now: (fmap (compose f g) fa) = (fmap f (fmap g fa))
(hint)
;; Suggests: functor-comp

(proof-apply 'functor-comp)
(qed)
```

### Available Tactics

| Tactic | What it does |
|----|----|
| `(proof-apply 'law)` | Apply a named law |
| `(simplify-goal)` | Auto-simplify with all laws |
| `(undo)` | Take back the last step |
| `(hint)` | Get suggestions |
| `(show)` | Display current state |
| `(proof-trace)` | Show proof history |

---

## Part 2: Auto-Fusion & Parallelization (fold-lzr)

Writing `(map f (map g xs))`? That's two list traversals when you only
need one! Let the LZR analyzer find these opportunities automatically.

### Detecting Fusion Opportunities

```scheme
(load "boundary/lzr.ss")

;; Analyze some code
(analyze-fusion '(map square (map double xs)))
```

Output:
```
  Found 1 fusion opportunity:

  [map-map-fuse] at root
    Before: (map square (map double xs))
    After:  (map (compose square double) xs)
    Savings: 50%
    Confidence: likely-pure
```

### The Full Report

```scheme
(lzr-report '(let ([doubled (map double xs)]
                   [squared (map square ys)])
               (append doubled squared)))
```

```
╔════════════════════════════════════════════════════════════════╗
║           FOLD-LZR: AUTO-PARALLELIZATION & FUSION             ║
╚════════════════════════════════════════════════════════════════╝

── FUSION OPPORTUNITIES ──

  No fusion opportunities detected.

── PARALLELIZATION HINTS ──

  [independent-let] at (0)
    Branches: (map double xs), (map square ys)
    Estimated speedup: ~1.8x

    These computations are independent and could run in parallel:
      (par (map double xs) (map square ys))

── COST ANALYSIS ──

  Total estimated work: 400 units
  Parallelizable: 360 units (90%)
```

### Automatic Optimization

```scheme
;; Before
(define slow-pipeline
  '(filter even? (map square (map (lambda (x) (+ x 1)) xs))))

;; Optimize it!
(optimize slow-pipeline)
```

Result:
```scheme
;; After - single traversal!
(filter-map
  (compose even? square)
  (compose square (lambda (x) (+ x 1)))
  xs)
```

### The Fusion Rules

LZR knows 19 fusion rules:

| Pattern | Becomes |
|----|----|
| `(map f (map g xs))` | `(map (compose f g) xs)` |
| `(filter p (map f xs))` | `(filter-map (compose p f) f xs)` |
| `(foldl f z (map g xs))` | `(foldl (fn (a x) (f a (g x))) z xs)` |
| `(flatten (map f xs))` | `(flatMap f xs)` |
| `(length (map f xs))` | `(length xs)` |
| ... and 14 more! |

### Stream Fusion Too!

```scheme
(analyze-fusion '(stream-map f (stream-map g (stream-filter p s))))
```

```
  Found 2 fusion opportunities:

  [stream-map-map-fuse]
    (stream-map f (stream-map g ...)) → (stream-map (compose f g) ...)

  [stream-filter-map-fuse]
    (stream-map g (stream-filter p s)) → (stream-filter-map p g s)
```

---

## Part 3: Combining Powers

The proof sketcher and fusion analyzer work together. Want to prove
that your optimization is correct?

```scheme
;; Prove map-map fusion is semantically valid
(sketch '(= (map f (map g xs)) (map (compose f g) xs)))

(hint)
;; Suggests: list-map-fuse or functor-fuse

(proof-apply 'functor-fuse)
(qed)

;; Now you KNOW your optimization is sound!
```

---

## Quick Reference

### Proof Sketcher Commands
```scheme
(sketch '(= lhs rhs))    ; Start proof
(goals)                  ; List goals
(hint)                   ; Get suggestions
(proof-apply 'law)       ; Apply law
(undo)                   ; Undo step
(qed)                    ; Finalize
(proof-help)             ; Full help
```

### LZR Commands
```scheme
(analyze-fusion expr)    ; Find fusion opportunities
(suggest-parallel expr)  ; Find parallelization hints
(optimize expr)          ; Apply safe rewrites
(lzr-report expr)        ; Full analysis
```

---

## Test Coverage

Both features are thoroughly tested:

| Module | Tests |
|----|----|
| Proof sketcher (goals, sketch, tactics) | 107 |
| Fusion detection | 31 |
| Parallel detection | 35 |
| Cost analysis | 48 |
| Fused operations | 66 |
| LZR integration | 29 |
| **Total** | **316** |

---

*Happy proving and optimizing!*
