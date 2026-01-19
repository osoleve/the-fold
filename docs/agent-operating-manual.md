# Agent Operating Manual

This document provides algorithmic steps for agents operating within The Fold. No hand-waving. Each section is a concrete procedure.

---

## Quick Start

### Essential Commands

```bash
# Evaluate expressions (implicit parens added automatically)
./fold "+ 1 2"                     # → 3
./fold "map add1 '(1 2 3)"         # → (2 3 4)

# Named sessions persist state
./fold -s agent "define x 10"      # Define variable
./fold -s agent "(begin x)"        # Retrieve value (wrap to avoid call)
./fold -s agent "bye"              # Clean up session

# Check daemon status
./fold --status
```

### Core Workflows

| Task | Command |
|------|---------|
| Find a capability | `(lf "matrix decomposition")` |
| Inspect a skill | `(li 'linalg)` |
| List exports | `(le 'linalg)` |
| Create issue | `(bbs-create "Title" 'priority 2)` |
| List open issues | `(bbs-list)` |
| Show unblocked work | `(bbs-ready)` |
| Run tests | `scheme --script lattice/fp/optics/test-optics.ss` |

---

## 1. Capability Discovery

### 1.1 Discovering Skills (Lattice Level)

**Prerequisite:** Meta-tooling auto-loads with the REPL.

**Algorithm: Find a capability by need**

```
PROCEDURE find-capability(need: String) -> List<Skill>
  1. Tokenize `need` into search terms
  2. Query BM25 index: (lf "need")
  3. Receive ranked list: ((skill-name . score) ...)
  4. For each result with score > 0.1:
     a. Inspect skill: (li 'skill-name)
     b. Check exports: (le 'skill-name)
  5. Return matching skills with their exports
```

**Discovery commands:**

| Command | Purpose | Example |
|---------|---------|---------|
| `(lf "query")` | Full-text search | `(lf "matrix decomposition")` |
| `(lfe 'symbol)` | Exact symbol lookup | `(lfe 'shapley-value)` |
| `(li 'skill)` | Skill description | `(li 'linalg)` |
| `(le 'skill)` | List exports | `(le 'fp/optics)` |
| `(lef "file.ss")` | Exports from any file | `(lef "boundary/bbs/bbs.ss")` |
| `(ld 'skill)` | Dependencies | `(ld 'autodiff)` |
| `(lu 'skill)` | What uses this skill | `(lu 'linalg)` |
| `(lc 'skill)` | Cycle check | `(lc 'fp)` |

**Testing commands:**

| Command | Purpose |
|---------|---------|
| `(lt 'skill)` | List test files for skill |
| `(ltr 'skill)` | Run tests for skill |
| `(lattice-tests-summary)` | Coverage overview |

### 1.2 Lattice Organization

**Tier 0 — Foundational (no lattice deps):**

| Skill | Purpose | Key Exports |
|-------|---------|-------------|
| `linalg` | Vectors, matrices, decomposition | `vec3`, `mat4`, `lu-decompose`, `svd` |
| `data` | Data structures, graphs | `make-queue`, `graph-bfs`, `community-detect` |
| `algebra` | Groups, rings, polynomials | `make-polynomial`, `grobner-basis` |
| `random` | PRNG, distributions | `random-normal`, `random-exponential` |

**Tier 1 — Intermediate:**

| Skill | Purpose | Key Exports |
|-------|---------|-------------|
| `autodiff` | Reverse-mode AD | `grad`, `jacobian`, `optic-gradient` |
| `fp/optics` | Complete optics tower | `lens`, `prism`, `traversal`, `^.`, `.~`, `%~` |
| `fp/game` | Game theory algorithms | `shapley-value`, `banzhaf-index`, `stable-match` |
| `fp/clp` | Constraint logic programming | `fd-domain`, `all-different`, `run-clp` |
| `query` | Query DSL with optics | `oquery`, `oquery-where`, `oquery-group-by` |
| `statistics` | Regression, hypothesis testing | `linear-regression`, `t-test`, `anova` |
| `topology` | Simplicial complexes, homology | `make-complex`, `betti-numbers` |
| `optimization` | LP, gradient descent, global | `minimize`, `lbfgs`, `interval-minimize` |

**Tier 2+ — Advanced:**

| Skill | Purpose |
|-------|---------|
| `physics/diff` | Differentiable 2D physics |
| `physics/diff3d` | Differentiable 3D physics |
| `tiles` | Board game SDK |
| `pipeline` | Agent workflows |

### 1.3 Shell Capabilities

Shell code lives in `boundary/` and handles all impure operations:

| Directory | Purpose | Key APIs |
|-----------|---------|----------|
| `bbs/` | Issue tracker | `bbs-create`, `bbs-show`, `bbs-list` |
| `flashmob/` | QA triage | `flashmob-start`, `flashmob-triage` |
| `reactive/` | Optic dependency tracking | `define-reactive`, `reactive-value` |
| `provenance/` | Audit trails | `record-provenance!`, `query-provenance` |
| `history/` | Undo/redo with branches | `undo`, `redo`, `branch` |
| `migrations/` | Schema evolution | `migrate!`, `rollback!` |
| `lsp/` | Language server | IDE integration |
| `git/` | Git operations | `(commit! "msg")`, `(push!)` |

---

## 2. Working with Data

### 2.1 The Optics Tower

Optics provide composable, type-safe data access. The tower (most to least powerful):

| Optic | Can | View | Set | Traverse |
|-------|-----|------|-----|----------|
| `Iso` | Isomorphism | 1 | 1 | 1 |
| `Lens` | Focus on one field | 1 | 1 | 1 |
| `Prism` | Focus on variant | 0-1 | 0-1 | 0-1 |
| `Affine` | At most one target | 0-1 | 0-1 | 0-1 |
| `Traversal` | Multiple targets | 0-n | 0-n | 0-n |
| `Fold` | Read-only traversal | 0-n | - | 0-n |
| `Getter` | Read-only single | 1 | - | 1 |
| `Setter` | Write-only | - | 0-n | - |

**Core operators:**

```scheme
;; View through optic
(^. structure lens)              ; Get single value
(^? structure prism)             ; Get optional value (Maybe)
(^.. structure traversal)        ; Get all values (list)

;; Set through optic
(.~ lens new-value structure)    ; Set value

;; Modify through optic
(%~ lens f structure)            ; Apply f to focused value

;; Compose optics (left-to-right)
(>>> outer-lens inner-lens)      ; Focus deeper
```

**Example:**

```scheme
(load "lattice/fp/optics/optics.ss")

;; Define a lens for a record field
(define person-name-lens (lens (lambda (p) (person-name p))
                                (lambda (p n) (make-person n (person-age p)))))

;; Use it
(^. alice person-name-lens)                    ; → "Alice"
(.~ person-name-lens "Alicia" alice)           ; → new person with name "Alicia"
(%~ person-name-lens string-upcase alice)      ; → person with name "ALICE"

;; Compose to reach nested data
(define company-ceo-name (>>> company-ceo-lens person-name-lens))
(^. acme company-ceo-name)                     ; → "Bob"
```

### 2.2 Optic-Based Queries

The query DSL combines optics with predicates for declarative data access:

```scheme
(load "lattice/query/optic-query.ss")

;; Basic query: get all targets through an optic
(oquery world world-bodies-traversal)          ; → all bodies

;; Filter by predicate
(oquery-where world bodies-traversal
              (lambda (b) (> (body-mass b) 10)))

;; Project through function
(oquery-select world bodies-traversal body-name) ; → list of names

;; Pipeline: filter then project
(oquery-pipe world bodies-traversal
             (lambda (b) (> (body-vel-y b) 0))
             body-name)                         ; → names of rising bodies
```

**Aggregations:**

| Function | Purpose |
|----------|---------|
| `oquery-count` | Count all targets |
| `oquery-count-where` | Count matching targets |
| `oquery-sum` | Sum numeric targets |
| `oquery-any` | True if any match |
| `oquery-all` | True if all match |
| `oquery-min`, `oquery-max` | Find extrema |
| `oquery-min-by`, `oquery-max-by` | Find element with extreme value |

**Grouping and joins:**

```scheme
;; Group by key function
(oquery-group-by world bodies-traversal
                 (lambda (b) (if (> (body-mass b) 10) 'heavy 'light)))
;; → ((heavy . (body1 body2)) (light . (body3)))

;; Join two optic results
(oquery-join world1 optic1 world2 optic2
             (lambda (a b) (= (item-id a) (ref-id b))))

;; Sort by key
(oquery-sort-by world bodies-traversal body-mass)     ; ascending
(oquery-sort-by-desc world bodies-traversal body-mass) ; descending
```

**Predicate helpers:**

```scheme
;; Create predicates from optics
(optic-eq? name-lens "Alice")        ; name = "Alice"
(optic-gt? age-lens 21)              ; age > 21
(optic-between? score-lens 80 100)   ; 80 <= score <= 100
(optic-matches? tag-lens "^test-")   ; tag matches regex
```

### 2.3 Reactive Derivations

Track dependencies through optics for automatic recomputation:

```scheme
(load "boundary/reactive/reactive.ss")

;; Register optics for tracking
(register-optic! 'body-pos body-pos-lens)
(register-optic! 'body-vel body-vel-lens)

;; Define reactive value that depends on optics
(define-reactive 'total-kinetic-energy
  world-source
  (lambda (w)
    (fold-left + 0
      (map (lambda (b) (* 0.5 (body-mass b) (vec-dot (body-vel b) (body-vel b))))
           (^.. w world-bodies-traversal)))))

;; Get cached value
(reactive-value 'total-kinetic-energy)

;; When source changes, dependents auto-invalidate
(reactive-invalidate! 'body-vel)
(reactive-value 'total-kinetic-energy)  ; Recomputes
```

### 2.4 Traced Optics (Autodiff through Optics)

Compute gradients through optic-focused paths:

```scheme
(load "lattice/autodiff/traced-optics.ss")

;; Gradient of loss w.r.t. nested parameter
(optic-gradient loss-fn (>>> outer-lens inner-lens) structure)

;; Gradient descent at optic focus
(optimize-at body-pos-lens body
             (lambda (pos) (distance-to-target pos))
             0.1)  ; learning rate

;; Gradients for all traversal targets
(optic-gradient-list loss-fn bodies-traversal world)
```

---

## 3. Issue and Task Management

### 3.1 BBS (Bulletin Board System)

The BBS is a CAS-native issue tracker. All issues are content-addressed.

**Creating issues:**

```scheme
;; Basic create
(bbs-create "Fix authentication bug")

;; With options
(bbs-create "Add dark mode"
            'priority 1           ; 0=critical, 2=medium, 4=backlog
            'type 'feature        ; task, bug, feature, epic
            'labels '(ui urgent)
            'description "Detailed description here")
```

**Viewing and searching:**

```scheme
(bbs-list)                        ; List open issues
(bbs-list 'status 'closed)        ; List closed issues
(bbs-list 'status 'all)           ; List all issues

(bbs-show 'fold-042)              ; View issue details
(bbs-find "auth")                 ; Search titles
(bbs-search "authentication")     ; Search titles + descriptions

(bbs-ready)                       ; Show unblocked work
(bbs-blocked)                     ; Show blocked issues
```

**Updating and closing:**

```scheme
(bbs-update 'fold-042 'status 'in_progress)
(bbs-update 'fold-042 'priority 0)
(bbs-update 'fold-042 'labels '(urgent security))

(bbs-close 'fold-042)
(bbs-close 'fold-042 'reason "Fixed in commit abc123")
```

**Dependencies:**

```scheme
;; fold-001 blocks fold-002
(bbs-dep 'fold-001 'fold-002)

;; Query dependencies
(bbs-blockers 'fold-002)          ; What blocks this?
(bbs-blocking 'fold-001)          ; What does this block?
```

**Filtering:**

```scheme
(bbs-list-by-label 'security)     ; Filter by label
(bbs-list-by-type 'epic)          ; Filter by type
(bbs-label-report)                ; Show all labels in use
```

### 3.2 Posts (Changelogs, Notes)

```scheme
(post-create "v2.0.0 Release" "What's new..." 'changelog)
(post-list)                       ; List all posts
(post-list 'type 'changelog)      ; Filter by type
(post-show 'post-1)               ; View post
(post-update 'post-1 'body "Updated content")
```

Post types: `changelog`, `note`, `announcement`, `session-summary`

### 3.3 Flashmob (QA Triage)

Flashmob coordinates multiple agents' QA findings using game-theoretic allocation:

```scheme
(load "boundary/flashmob/flashmob.ss")

;; Start a session
(flashmob-start '("core/fp/monad.ss" "core/fp/functor.ss"))

;; Add findings
(flashmob-add-finding
  'file "core/fp/monad.ss"
  'line 42
  'severity 'high                 ; critical, high, medium, low, info
  'category 'correctness          ; security, performance, correctness, style
  'confidence 0.85
  'agent 'correctness-agent
  'title "Off-by-one in bind"
  'description "Loop terminates early")

;; Run triage
(flashmob-triage)                 ; Auto-select strategy
(flashmob-triage 'strategy 'game) ; Force game-theoretic

;; View results
(flashmob-ranking)                ; Ranked findings
(flashmob-credits)                ; Agent attribution (Shapley values)
(flashmob-consensus)              ; Consensus confidence

;; Export to BBS
(flashmob-to-bbs 'count 5)        ; Create top 5 as issues
(flashmob-to-bbs 'severity 'high) ; Only high+ severity

;; End session
(flashmob-stop)
```

---

## 4. Trust and Verification

### 4.1 The Trust Model

```
TRUST HIERARCHY:

  Core (pure)     ← Trusts nothing, assumes perfect input
       ↑
  Shell (fallen)  ← Validates everything, mints capabilities
       ↑
  Outside         ← Untrusted by definition
```

**Axioms:**
1. Core code NEVER constructs capability records directly
2. Only Shell's `mint-X-capability` functions create capabilities
3. Capability records are **opaque** — Core cannot forge them
4. Shell validates ALL input before passing to Core

### 4.2 Capability Verification

**Algorithm: Verify a capability is valid**

```
PROCEDURE verify-capability(cap: Any, expected-type: Symbol) -> Boolean
  1. Check cap is a record: (record? cap)
  2. Check record type matches expected
  3. Verify field invariants (e.g., path exists)
  4. Return #t if all checks pass
```

### 4.3 Static Verification

**Running a capability audit:**

```scheme
(load "boundary/capability-lens.ss")
(capability-report (fs) "boundary")
;; Shows: which files mint capabilities, which use them
```

---

## 5. Fuel and Performance

### 5.1 The Fuel Model

```
FUEL RULES:
  - Each eval step costs 1 fuel
  - Primitives cost 0 fuel (add, mul, cons, car, cdr, ...)
  - When fuel = 0, evaluation suspends (not crashes)
  - Suspended computation can resume with fresh fuel
```

### 5.2 Complexity Bounds

Skills declare complexity in their manifests:

| Pattern | Fuel Bound | Example |
|---------|------------|---------|
| Simple accessor | O(1) | `(vec-x v)` |
| Linear traversal | O(n) | `(map f lst)` |
| Matrix multiply | O(n³) | `(mat-mul a b)` |
| Shapley value | O(2^n) | `(shapley-value game)` |

### 5.3 Monitoring

```scheme
(define result (eval-core expr env 10000 (make-ctx)))

(cond
 [(ok? result) (printf "Fuel remaining: ~a~n" (ok-fuel result))]
 [(suspended? result) (printf "Suspended at: ~a~n" (suspended-expr result))]
 [(error? result) (printf "Error: ~a~n" (error-message result))])
```

---

## 6. Provenance and History

### 6.1 Recording Provenance

All transformations can be tracked:

```scheme
(load "boundary/provenance/provenance.ss")

;; Register optics for tracking
(register-optic! 'body-pos body-pos-lens)

;; Record a transformation
(with-agent-id 'my-agent
  (lambda ()
    (record-provenance! result-hash source-hash 'body-pos 'modify)))

;; Query provenance
(query-provenance 'result result-hash)   ; What created this?
(query-provenance 'source source-hash)   ; What was derived from this?
```

### 6.2 The Block Model

**Everything is a Block:**

```
Block = {
  tag:     Symbol      # Block type identifier
  payload: Bytevector  # Raw data
  refs:    [Address]   # Ordered list of 33-byte addresses
}

Address = VersionByte (1) + SHA256Hash (32) = 33 bytes
```

**Normalization:** S-expressions are α-normalized before hashing:

```scheme
(normalize '(fn (x) x))       ;; → (fn (dv 0))
(normalize '(fn (y) y))       ;; → (fn (dv 0))  SAME hash!
```

### 6.3 Store Operations

```scheme
(store! block)                    ; Store, return hash
(fetch hash)                      ; Retrieve by hash
(stored? hash)                    ; Check existence
(hash-block block)                ; Compute hash without storing
```

### 6.4 History (Undo/Redo)

The REPL supports full undo/redo with branching:

```scheme
;; Navigation
(undo)                            ; Undo last command
(redo)                            ; Redo undone command
(jump n)                          ; Jump to history index n

;; Viewing
(history)                         ; Show last 20 commands
(history n)                       ; Show last n commands
(export-history)                  ; Export as Scheme script

;; Branching
(branch 'experiment)              ; Create branch at current point
(branches)                        ; List all branches
(checkout 'experiment)            ; Switch to branch
(delete-branch 'experiment)       ; Delete a branch

;; Control
(history-enable!)                 ; Enable recording
(history-disable!)                ; Disable recording
```

**History output format:**

```
History (branch: main, at index 5):
─────────────────────────────────────────────────
    0 [D ] (define x 10) → x
    1 [D ] (define y 20) → y
►   2 [  ] (+ x y)
─────────────────────────────────────────────────
[D]=definition [E]=effect [✗]=error
```

---

## 7. Advanced Features

### 7.1 Game Theory Toolkit

Ready-to-use algorithms in `lattice/fp/game/`:

```scheme
;; Cooperative games
(define game (make-coop-game players characteristic-fn))
(shapley-value game)              ; Fair value allocation
(core game)                       ; Stable allocations
(nucleolus game)                  ; Lexicographically fair

;; Voting power
(banzhaf-index voting-game)       ; Banzhaf power index
(shapley-shubik-index voting-game) ; Shapley-Shubik index

;; Matching
(stable-match preferences)        ; Gale-Shapley algorithm
(hospital-residents prefs caps)   ; Many-to-one matching

;; Voting
(schulze-ranking ballots)         ; Condorcet method
(borda-scores ballots)            ; Borda count
(condorcet-winner ballots)        ; If exists

;; Fair division
(envy-free? allocation prefs)     ; Check envy-freeness
(pareto-optimal? allocation)      ; Check Pareto optimality
```

### 7.2 Constraint Logic Programming

cKanren-style CLP(FD) in `lattice/fp/clp/`:

```scheme
(load "lattice/fp/clp/clp.ss")

;; N-Queens
(run* (q)
  (fresh (queens)
    (fd-domain queens 1 8)
    (all-different queens)
    (safe-queens queens)
    (== q queens)))

;; Sudoku, cryptarithmetic, etc.
```

### 7.3 Migrations

Schema evolution with bidirectional optics:

```scheme
(load "boundary/migrations/runner.ss")

;; Define migration
(define v1->v2
  (make-migration 'v1 'v2
    (field-add-iso 'new-field default-value)))

;; Apply
(migrate! tree v1->v2)            ; Forward
(rollback! tree v1->v2)           ; Backward

;; Chain migrations
(migration-compose v1->v2 v2->v3) ; v1 → v3
```

### 7.4 Statistics

Regression, hypothesis testing in `lattice/statistics/`:

```scheme
(linear-regression X y)           ; OLS
(ridge-regression X y lambda)     ; L2 regularization
(lasso-regression X y lambda)     ; L1 regularization

(t-test sample1 sample2)          ; Two-sample t-test
(anova groups)                    ; One-way ANOVA
(chi-squared observed expected)   ; Chi-squared test
```

### 7.5 Template DSL

Grammar-driven code generation:

```scheme
(load "boundary/tools/template-parser.ss")

(tp-batch "
  define (qs lst) $body
  --- $body := if $cond $then $else
  --- $cond := null? lst
  --- $then := '()
  --- $else := append (qs (filter $pred (cdr lst))) ...
")
```

### 7.6 Refactoring Toolkit

```scheme
(load "boundary/tools/refactor-toolkit.ss")

(refactor 'help)                  ; Show operations
(refactor 'rename 'old 'new)      ; Preview rename
(refactor 'apply)                 ; Apply staged changes
(refactor 'dead-code)             ; Find unused code
(refactor 'deps 'symbol)          ; Show callers/callees
```

---

## Quick Reference

### Discovery

```scheme
(lf "query")           ; Search skills by text
(lfe 'symbol)          ; Find skill exporting symbol
(li 'skill)            ; Inspect skill details
(le 'skill)            ; List skill exports
(ld 'skill)            ; Dependencies of skill
(lu 'skill)            ; What uses this skill
```

### Optics

```scheme
(^. s lens)            ; View single
(^? s prism)           ; Preview (Maybe)
(^.. s traversal)      ; To list
(.~ lens v s)          ; Set
(%~ lens f s)          ; Modify
(>>> o1 o2)            ; Compose left-to-right
```

### Queries

```scheme
(oquery s optic)                  ; All targets
(oquery-where s op pred)          ; Filter
(oquery-select s op fn)           ; Project
(oquery-count s op)               ; Count
(oquery-group-by s op key-fn)     ; Group
(oquery-sort-by s op key-fn)      ; Sort
```

### BBS

```scheme
(bbs-create "Title")              ; Create issue
(bbs-list)                        ; List open issues
(bbs-ready)                       ; Unblocked work
(bbs-show 'fold-001)              ; View issue
(bbs-update 'id 'field value)     ; Update
(bbs-close 'id)                   ; Close
```

### Reactive

```scheme
(register-optic! 'name optic)     ; Register for tracking
(define-reactive 'name src fn)    ; Define reactive value
(reactive-value 'name)            ; Get cached value
(reactive-invalidate! 'optic)     ; Invalidate dependents
```

### History

```scheme
(undo)                 ; Undo last
(redo)                 ; Redo
(history)              ; View history
(branch 'name)         ; Create branch
(checkout 'name)       ; Switch branch
```

### Store

```scheme
(store! block)         ; Store, return hash
(fetch hash)           ; Retrieve
(stored? hash)         ; Check existence
(normalize expr)       ; α-normalize
```

### Shell

```bash
./fold "expr"          # Evaluate (implicit parens)
./fold -s name "expr"  # Named session
./fold --status        # Check daemon
```

---

## Invariants to Maintain

1. **Never construct capabilities in Core** — always request from Shell
2. **Always budget fuel** — unbounded computation will suspend
3. **Always record provenance** — store refs to inputs in output blocks
4. **Normalize before hashing** — ensures α-equivalent = same hash
5. **Pin important blocks** — prevents accidental garbage collection
6. **Validate at boundaries** — Shell checks everything, Core trusts input
7. **Register optics for reactive** — unregistered optics generate warnings
8. **Close BBS issues when done** — maintain clean backlog
