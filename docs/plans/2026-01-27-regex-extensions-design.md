# Regex Extensions: Lookahead, Anchors, Quantifier Ranges

**Issue:** fold-zxub
**Date:** 2026-01-27
**Status:** Design complete, ready for implementation

## Overview

Extend the regex engine (`lattice/fp/parsing/regex.ss`) and FSM library (`lattice/fp/parsing/fsm.ss`) with:

1. **Quantifier ranges** — `{n}`, `{n,}`, `{,m}`, `{n,m}`
2. **Anchors** — `^` (start), `$` (end)
3. **Lookahead** — `(?=...)` (positive), `(?!...)` (negative)

## AST Extensions

New node types in regex.ss:

```scheme
;; Quantifier ranges
(regex-repeat expr min max)
;; min : Nat, max : (Option Nat) where #f = unbounded
;; {3}   → (regex-repeat e 3 3)
;; {2,4} → (regex-repeat e 2 4)
;; {2,}  → (regex-repeat e 2 #f)
;; {,4}  → (regex-repeat e 0 4)

;; Anchors
(regex-anchor type)
;; type : Symbol ('start | 'end)
;; ^  → (regex-anchor 'start)
;; $  → (regex-anchor 'end)

;; Lookahead assertions
(regex-lookahead expr positive?)
;; expr : RegexAST, positive? : Boolean
;; (?=pat) → (regex-lookahead pat #t)
;; (?!pat) → (regex-lookahead pat #f)
```

Each gets standard constructor, predicate, and accessor functions.

## Parser Extensions

### Updated metacharacter exclusions

```scheme
;; In parse-literal and parse-escape
(not (member c '(#\* #\+ #\? #\. #\| #\[ #\] #\( #\) #\\ #\^ #\$ #\{)))
```

### New parsers

**parse-interval** — Parses `{...}` quantifier syntax:
- `{n}` → exact count
- `{n,}` → at least n
- `{,m}` → at most m (0 to m)
- `{n,m}` → range
- `{}` → parse error
- Validates min ≤ max when both present

**parse-anchor** — Parses `^` and `$` as zero-width atoms.

**parse-lookahead** — Parses `(?=...)` and `(?!...)`:
- Distinguishes from regular groups by `(?` prefix followed by `=` or `!`
- Contains recursive call to parse-regex for inner pattern
- Must be tried before parse-group in parse-atom

### Grammar changes

```
postfix  → atom postfix-op*
postfix-op → '*' | '+' | '?' | interval
interval → '{' nat '}'
         | '{' nat ',' nat? '}'
         | '{' ',' nat '}'

atom → lookahead | group | class | anchor | dot | literal
lookahead → '(?' ('=' | '!') regex ')'
anchor → '^' | '$'
```

## FSM Extensions

### New transition type: Assertions

Add assertion transitions to the FSM model — transitions that check a condition without consuming input.

```scheme
;; Assertion transition entry in fsm-assertions list
(cons state (list 'assertion inner-fsm positive? target-state))
```

**FSM structure change:**
```scheme
(make-fsm states alphabet transitions start accepting epsilon assertions)
;;                                                            ^^^^^^^^^^
;; New 8th element: list of assertion transitions
```

For backward compatibility, assertions default to `'()` if not provided.

### Anchor assertions

Anchors compile to assertion transitions with trivial "inner FSMs":
- `^` — assertion that succeeds only at position 0
- `$` — assertion that succeeds only at position = input length

These don't need inner NFAs; they check position directly. Represented as:
```scheme
(cons state (list 'anchor 'start target))  ; for ^
(cons state (list 'anchor 'end target))    ; for $
```

### Lookahead assertions

Lookahead compiles to assertion transitions with inner NFAs:
```scheme
(cons state (list 'lookahead inner-nfa positive? target))
```

During execution:
1. At assertion state, run inner-nfa on remaining input from current position
2. If positive? and inner accepts → take transition (position unchanged)
3. If !positive? and inner rejects → take transition (position unchanged)
4. Otherwise → assertion fails, don't take transition

## Compilation (AST → FSM)

### regex-repeat compilation

Expand at compile time:
```scheme
(define (compile-repeat expr min max universe)
  (let ([base (regex-compile expr universe)])
    (cond
      ;; {0,0} → empty string
      [(and (= min 0) (eqv? max 0))
       (fsm-epsilon-lang)]
      ;; {n} or {n,n} → concatenate n copies
      [(eqv? min max)
       (fold-left fsm-concat (fsm-epsilon-lang)
                  (make-list min base))]
      ;; {0,m} → optional chain
      [(= min 0)
       (fold-left (lambda (acc _) (fsm-optional (fsm-concat base acc)))
                  (fsm-epsilon-lang)
                  (iota max))]
      ;; {n,} → n copies then star
      [(not max)
       (fsm-concat (fold-left fsm-concat (fsm-epsilon-lang) (make-list min base))
                   (fsm-star base))]
      ;; {n,m} → n copies then (m-n) optionals
      [else
       (let ([required (fold-left fsm-concat (fsm-epsilon-lang) (make-list min base))]
             [optional (fold-left (lambda (acc _) (fsm-concat (fsm-optional base) acc))
                                  (fsm-epsilon-lang)
                                  (iota (- max min)))])
         (fsm-concat required optional))])))
```

### regex-anchor compilation

```scheme
(define (compile-anchor type)
  (let ([s0 (fsm-fresh-state "anc")]
        [s1 (fsm-fresh-state "anc")])
    (make-fsm (list s0 s1) '() '() s0 (list s1) '()
              (list (cons s0 (list 'anchor type s1))))))
```

### regex-lookahead compilation

```scheme
(define (compile-lookahead expr positive? universe)
  (let ([inner (regex-compile expr universe)]
        [s0 (fsm-fresh-state "la")]
        [s1 (fsm-fresh-state "la")])
    (make-fsm (list s0 s1) '() '() s0 (list s1) '()
              (list (cons s0 (list 'lookahead inner positive? s1))))))
```

## Execution Changes

### fsm-run modification

The execution loop needs to handle assertions:

```scheme
(define (fsm-run-with-assertions fsm input)
  (let* ([input-vec (list->vector (string->list input))]
         [len (vector-length input-vec)])
    (let run ([states (epsilon-closure-with-assertions fsm (fsm-start fsm) 0 input-vec len)]
              [pos 0])
      (if (= pos len)
          ;; At end: check for accepting state
          (if (exists (lambda (s) (member s (fsm-accepting fsm))) states)
              (just states)
              nothing)
          ;; Process next character
          (let ([next-states (fsm-move-with-assertions fsm states (vector-ref input-vec pos) (+ pos 1) input-vec len)])
            (if (null? next-states)
                nothing
                (run next-states (+ pos 1))))))))
```

**epsilon-closure-with-assertions** — Like epsilon-closure but also follows assertion transitions when they succeed:
- For anchor assertions: check position against 0 or len
- For lookahead assertions: run inner FSM on `(substring input pos)`

**fsm-move-with-assertions** — Like fsm-move but uses assertion-aware epsilon closure.

## Edge Cases

| Pattern | Interpretation |
|---------|----------------|
| `{0,0}` | Matches empty string only |
| `{0,}` | Same as `*` |
| `{1,}` | Same as `+` |
| `{0,1}` | Same as `?` |
| `{,0}` | Matches empty string only |
| `{}` | Parse error |
| `(?=)` | Lookahead with empty pattern (always succeeds) |
| `(?!)` | Negative lookahead with empty (always fails) |
| `^$` | Matches empty string only |
| `(?=a(?!b))` | Nested: matches if followed by 'a' not followed by 'b' |

## Testing Strategy

1. **Parser tests** — Verify AST construction for all new syntax
2. **Compilation tests** — Verify FSM structure for each node type
3. **Acceptance tests** — End-to-end regex-accepts? tests:
   - Quantifier ranges: `a{3}`, `a{2,4}`, `a{2,}`, `a{,3}`
   - Anchors: `^foo`, `foo$`, `^foo$`, `^$`
   - Lookahead: `a(?=b)`, `a(?!b)`, `(?=a)a`, password patterns
   - Combined: `^(?=.*[A-Z])(?=.*[0-9]).{8,}$`
4. **Edge cases** — `{0,0}`, empty lookahead, nested assertions

## Files Modified

- `lattice/fp/parsing/regex.ss` — AST types, parser, compilation
- `lattice/fp/parsing/fsm.ss` — Assertion transitions, execution
- `lattice/fp/parsing/test-regex.ss` — New test file

## Performance Notes

- Quantifier ranges expand at compile time → large `{n}` causes state explosion
- Lookahead runs subsidiary NFA at each assertion point → O(n·m) worst case
- No memoization of lookahead results (could add later if needed)
