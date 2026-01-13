# Template DSL v1: Grammar-Driven Code Construction

**Status:** Stable
**Version:** 1.0
**Location:** `lattice/dsl/template/` (core), `shell/tools/` (session, parser)

## Overview

The Template DSL enables AI agents to construct S-expressions without tracking parenthesis depth. Instead of writing nested Scheme directly, agents write linear EBNF-like production statements where holes (`$name`) act as non-terminals that get filled incrementally.

### The Problem

AI language models struggle with deeply nested parentheses:

```scheme
;; Tracking depth across )))) sequences is error-prone
(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))  ;; ← How many )'s?
```

Models frequently:
- Miscount closing parentheses
- Lose track of nesting depth mid-expression
- Generate syntactically invalid code that fails to parse

### The Solution

Write flat production statements. Let the DSL handle structure:

```
define $sig $body           → (define $sig $body)
$sig := factorial n         → (factorial n)
$body := if $c $t $e        → (if $c $t $e)
$c := = n 0                 → (= n 0)
$t := 1
$e := * n (factorial (- n 1))
```

Compile when all holes are filled → valid S-expression.

## Core Concepts

### Holes

Holes are symbols prefixed with `$`. They act as placeholders (non-terminals) in the grammar:

```scheme
(hole? '$name)      ; → #t
(hole? 'name)       ; → #f
(hole? '$x)         ; → #t (single char after $ is valid)
(hole? '$)          ; → #f (need at least one char after $)
```

### Implicit Parentheses

**Every multi-token statement gets wrapped in parentheses.** This is the key insight that eliminates most outer parens:

| Input | Output |
|-------|--------|
| `define $sig $body` | `(define $sig $body)` |
| `+ 1 2` | `(+ 1 2)` |
| `if $c $t $e` | `(if $c $t $e)` |
| `foo` | `foo` (single token, no wrap) |
| `(+ 1 2)` | `(+ 1 2)` (already wrapped) |

Explicit parentheses are only needed for **nested structure within** an expression:

```
$else := * n (factorial (- n 1))
         ↑   ↑          ↑
         These are explicit nested calls
```

### Hole Propagation

When you fill a hole with a value containing holes, those holes become active:

```scheme
(ts-start '($outer))           ; Holes: $outer
(ts-fill '$outer '(if $a $b))  ; Holes: $a, $b  ← propagated!
(ts-fill '$a '(= x 0))         ; Holes: $b
(ts-fill '$b 1)                ; Holes: (none) → ready to compile
```

This enables incremental refinement of structure.

## Usage Patterns

### Pattern 1: Batch Mode (Recommended)

Chain template + fills in one command using `---` separators:

```scheme
(load "shell/tools/template-parser.ss")

(tp-batch "
  define qs $params $body
  --- $params := lst
  --- $body := if $cond $then $else
  --- $cond := null? lst
  --- $then := '()
  --- $else := append (qs (filter $pred (cdr lst)))
                      (cons (car lst) (qs (filter $pred2 (cdr lst))))
  --- $pred := lambda (x) (< x (car lst))
  --- $pred2 := lambda (x) (>= x (car lst))
")
```

Returns a `begin` block with all definitions.

### Pattern 2: Complete Definitions

Use `tp-batch` to chain complete expressions without holes:

```scheme
(tp-batch "
  define (merge left right)
    (cond
      ((null? left) right)
      ((null? right) left)
      ((< (car left) (car right))
       (cons (car left) (merge (cdr left) right)))
      (else
       (cons (car right) (merge left (cdr right)))))
  ---
  define (merge-sort lst)
    (if (null? (cdr lst))
        lst
        (let ((halves (split lst)))
          (merge (merge-sort (car halves))
                 (merge-sort (cdr halves)))))
")
```

Still benefits from implicit outer parens and `begin` wrapping.

### Pattern 3: Interactive Mode

Build incrementally with feedback after each step:

```scheme
(tp-parse "define $sig $body")    ; "Holes: $sig, $body"
(tp-parse "$sig := factorial n")  ; "Holes: $body"
(tp-parse "$body := if $c $t $e") ; "Holes: $c, $t, $e"
(tp-parse "$c := = n 0")          ; "Holes: $t, $e"
(tp-parse "$t := 1")              ; "Holes: $e"
(tp-parse "$e := * n (factorial (- n 1))")  ; "Complete!"
(ts-compile)  ; → (define (factorial n) (if (= n 0) 1 (* n (factorial (- n 1)))))
```

### Pattern 4: Quick Templates

Convenience functions for common patterns:

```scheme
(ts-fn 'foo '(x y))     ; → (define (foo x y) $body)
(ts-lambda '(x y))      ; → (lambda (x y) $body)
(ts-let '((x 1) (y 2))) ; → (let ((x 1) (y 2)) $body)
(ts-if)                 ; → (if $cond $then $else)
(ts-cond 3)             ; → (cond ($cond1 $body1) ($cond2 $body2) ($cond3 $body3))
```

## API Reference

### Core Engine (`lattice/dsl/template/template.ss`)

```scheme
;; Hole detection
(hole? x)                    ; Any → Bool
(hole-name x)                ; Symbol → Symbol ($foo → foo)
(find-holes expr)            ; Expr → (List Symbol)

;; Template construction
(new-template expr)          ; Expr → Template
(template? x)                ; Any → Bool
(template-expr t)            ; Template → Expr
(template-holes t)           ; Template → (List Symbol)
(template-complete? t)       ; Template → Bool

;; Filling
(fill-hole t sym val)        ; Template × Symbol × Expr → Template
(fill-holes t alist)         ; Template × Alist → Template

;; Compilation
(compile-template t)         ; Template → Expr (error if incomplete)
(try-compile-template t)     ; Template → (Ok Expr) | (Err (List Symbol))

;; Display
(template-status t)          ; Template → String
```

### Session Manager (`shell/tools/template-session.ss`)

```scheme
;; Session lifecycle
(ts-start expr)              ; Start session with template
(ts-active?)                 ; Is session active?
(ts-reset)                   ; Clear session
(ts-compile)                 ; Compile and return result

;; Hole operations
(ts-holes)                   ; Get current holes
(ts-fill sym val)            ; Fill a hole
(ts-undo)                    ; Revert last fill

;; Display
(ts-show)                    ; Pretty-print template
(ts-status)                  ; Get status string

;; Multi-definition accumulation
(ts-next expr)               ; Compile current, start new
(ts-emit)                    ; Compile and save without starting new
(ts-all)                     ; Get all accumulated as begin block
(ts-count)                   ; Count accumulated definitions
(ts-defs exprs)              ; Add multiple complete exprs

;; Quick templates
(ts-fn name args)            ; (define (name args...) $body)
(ts-lambda args)             ; (lambda (args...) $body)
(ts-let bindings)            ; (let bindings $body)
(ts-if)                      ; (if $cond $then $else)
(ts-cond n)                  ; (cond (clause)...)
```

### Parser (`shell/tools/template-parser.ss`)

```scheme
;; Main entry points
(tp-batch str)               ; Parse chained defs with ---, return Expr
(tp-parse str)               ; Parse single line, execute operation
(tp-repl)                    ; Interactive REPL

;; Utilities
(tokenize str)               ; String → (List Sexpr)
(apply-implicit-parens toks) ; (List Sexpr) → Sexpr
```

## Integration with fold-agent.py

The Python agent client also applies implicit parentheses:

```bash
# These are equivalent:
./fold-agent.py "(+ 1 2)"
./fold-agent.py "+ 1 2"

# Multi-token → wrapped
./fold-agent.py "define x 10"  # → (define x 10)

# Single token → not wrapped
./fold-agent.py "x"            # → x
```

## Design Decisions

### Why `$` for holes?

- **Visually distinct** from Scheme syntax
- **Not used** in standard Scheme (unlike `?` which appears in predicates)
- **Easy to type** and recognize
- **Grep-friendly** for finding unfilled holes

Alternatives considered:
- `?name` — Conflicts with predicate conventions (`null?`, `pair?`)
- `<name>` — Conflicts with comparison operators
- `_name` — Could be confused with wildcards

### Why implicit parentheses?

The parenthesis problem is specifically about **outer wrapping**, not inner structure. Scheme's uniform syntax means every compound expression needs parens, but:

1. The **outermost** parens are pure boilerplate
2. **Nested** parens carry semantic meaning (grouping)

By auto-wrapping multi-token statements, we eliminate the boilerplate while preserving meaningful structure.

### Why `---` as separator?

- **Visually clear** delimiter
- **Not valid Scheme** syntax (won't be confused with code)
- **Three characters** — distinct from `--` (which could be a symbol)
- **Easy to parse** with simple string splitting

## Limitations

### v1 Limitations

1. **No hole typing** — Holes accept any value. Future versions might support `$name:expr` or `$name:symbol` constraints.

2. **No validation** — The DSL doesn't verify that filled values make semantic sense, only that they're syntactically valid S-expressions.

3. **Batch mode doesn't do hole-filling** — `tp-batch` treats each `---` section as complete. You can't start a template in section 1 and fill holes in section 2. Each section must be self-contained.

4. **Complex algorithms still need parens** — For deeply nested conditionals or complex recursion, you still write Scheme. The DSL helps most with structural boilerplate.

### When NOT to use the Template DSL

- **Simple expressions** — `(+ 1 2)` is fine as-is
- **Reading existing code** — The DSL is for writing, not reading
- **Performance-critical generation** — The indirection adds overhead

### When TO use the Template DSL

- **Multi-function definitions** — Chain with `tp-batch`
- **Boilerplate-heavy code** — `define`, `let`, `lambda` wrappers
- **Incremental construction** — Build structure before details
- **AI code generation** — Reduces parenthesis errors

## Testing

```bash
# Core engine tests (25 tests)
scheme --script lattice/dsl/template/test-template.ss

# Shell tools tests (30 tests)
scheme --script shell/tools/test-template-tools.ss
```

## Files

| File | Purpose |
|------|---------|
| `lattice/dsl/template/template.ss` | Core engine (pure) |
| `lattice/dsl/template/test-template.ss` | Core tests |
| `lattice/dsl/template/manifest.sexp` | Skill metadata |
| `lattice/dsl/template/README.sexp` | Module docs |
| `shell/tools/template-session.ss` | Session manager |
| `shell/tools/template-parser.ss` | Linear syntax parser |
| `shell/tools/test-template-tools.ss` | Shell tools tests |

## Future Directions (v2 ideas)

1. **Typed holes** — `$name:symbol`, `$body:expr`, `$n:number`
2. **Hole defaults** — `$timeout:=30` falls back if not filled
3. **Hole validation** — Custom predicates for valid fills
4. **Template macros** — Reusable template patterns
5. **Better batch mode** — Allow hole-filling across `---` sections
6. **IDE integration** — Highlight unfilled holes, autocomplete

## Changelog

### v1.0 (2025-01)

- Initial release
- Core template engine with hole detection, filling, compilation
- Session manager with undo support
- Linear syntax parser with implicit parentheses
- Batch mode with `---` separators
- Quick templates for common patterns
- Multi-definition accumulation
- 55 tests total (25 core + 30 shell)
