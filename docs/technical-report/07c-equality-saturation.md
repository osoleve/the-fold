## 7c. Equality Saturation and Optimization

This chapter covers pattern matching, saturation algorithms, cost models, extraction, and integration with CUDA codegen. The core e-graph data structures were covered in Chapter 7b.

### 7c.1 Pattern Matching and Rewrite Rules

Patterns use variables prefixed with `?`:

```scheme
;; Pattern: (+ ?x 0)  matches any addition with 0
;; ?x binds to the e-class ID of the first child

(define identity-rule (make-rule '(+ ?x 0) '?x))
```

**Key insight**: In an e-graph, a pattern can match *multiple ways* because each e-class may contain multiple e-nodes. The matcher returns *all* valid substitutions.

```scheme
(ematch eg '(+ ?x ?y) class-id)
;; Returns: list of substitutions like ((?x . 3) (?y . 4))
```

#### 7c.1.1 Pattern Syntax

Patterns support:
- **Variables**: `?x`, `?y`, `?foo` (bind to e-class IDs)
- **Literals**: `0`, `1`, `#t` (match exactly)
- **Symbols**: `+`, `cons` (match operator)
- **Nested patterns**: `(+ ?x (* ?y ?z))`

**Example patterns**:

```scheme
;; Arithmetic identities
'(+ ?x 0)              ; x + 0
'(* ?x 1)              ; x * 1
'(* ?x 0)              ; x * 0

;; Commutativity
'(+ ?x ?y)             ; x + y (also matches y + x after rewriting)
'(* ?x ?y)             ; x * y

;; Associativity
'(+ (+ ?x ?y) ?z)      ; (x + y) + z
'(+ ?x (+ ?y ?z))      ; x + (y + z)

;; Distributivity
'(* ?x (+ ?y ?z))      ; x * (y + z)
'(+ (* ?x ?y) (* ?x ?z))  ; x*y + x*z
```

#### 7c.1.2 Matching Algorithm

The matcher recursively traverses patterns and e-nodes:

```scheme
(define (ematch eg pattern class-id)
  (let ([root (egraph-find eg class-id)])
    (apply append
      (map (lambda (node) (match-node eg pattern node))
           (eclass-get-nodes (egraph-store eg) root)))))
```

**Match node**:

```scheme
(define (match-node eg pattern node)
  (cond
    ;; Variable: bind to this e-class
    [(variable? pattern)
     (list (list (cons pattern class-id)))]

    ;; Literal: must match exactly
    [(literal? pattern)
     (if (equal? pattern (enode-op node))
         '(())
         '())]

    ;; Constructor: operator must match, recurse on children
    [(pair? pattern)
     (if (equal? (car pattern) (enode-op node))
         (let ([child-matches
                 (map (lambda (pat child)
                        (ematch eg pat child))
                      (cdr pattern)
                      (vector->list (enode-children node)))])
           ;; Combine substitutions from all children
           (combine-substitutions child-matches))
         '())]))
```

**Combining substitutions**:

When matching multiple children, substitutions must be consistent (same variable can't bind to different IDs):

```scheme
(define (combine-substitutions substs-list)
  (fold-right
    (lambda (substs acc)
      (apply append
        (map (lambda (s1)
               (filter-map (lambda (s2) (merge-subst s1 s2))
                           substs))
             acc)))
    '(())
    substs-list))
```

### 7c.2 Equality Saturation

The saturation loop repeatedly applies rules until no new equivalences are found:

```scheme
(define (saturate eg rules config)
  (let loop ([iteration 0] [applied 0])
    (let ([new-applied (saturate-iteration eg rules)])
      (egraph-rebuild! eg)
      (cond
        [(zero? new-applied) 'saturated]      ; Fixpoint reached
        [(> applied (config-fuel config)) 'fuel-exhausted]
        [else (loop (+ iteration 1) (+ applied new-applied))]))))
```

**Resource limits** prevent unbounded growth:
- **Fuel**: Maximum total rule applications
- **Node limit**: Maximum e-graph size
- **Iteration limit**: Maximum applications per iteration

#### 7c.2.1 Saturation Iteration

One iteration applies all rules to all e-classes:

```scheme
(define (saturate-iteration eg rules)
  (let ([count 0])
    (for-each (lambda (rule)
                (set! count (+ count (apply-rule! eg rule))))
              rules)
    count))
```

**Applying a rule**:

```scheme
(define (apply-rule! eg rule)
  (let ([matches 0])
    (for-each (lambda (class-id)
                (let ([substs (ematch eg (rule-lhs rule) class-id)])
                  (for-each (lambda (subst)
                              (let ([rhs-id (instantiate eg (rule-rhs rule) subst)])
                                (egraph-merge! eg class-id rhs-id)
                                (set! matches (+ matches 1))))
                            substs)))
              (egraph-classes eg))
    matches))
```

**Instantiation**: Substitute variables in the right-hand side pattern:

```scheme
(define (instantiate eg rhs subst)
  (cond
    [(variable? rhs)
     (cdr (assq rhs subst))]  ; Look up variable
    [(literal? rhs)
     (egraph-add-term! eg rhs)]
    [(pair? rhs)
     (let ([children (map (lambda (child) (instantiate eg child subst))
                          (cdr rhs))])
       (egraph-add-expr! eg (car rhs) children))]))
```

#### 7c.2.2 Predefined Rule Sets

**Arithmetic identities**:

```scheme
(define arith-identity-rules
  (list
    (make-rule '(+ ?x 0) '?x)       ; x + 0 = x
    (make-rule '(+ 0 ?x) '?x)       ; 0 + x = x
    (make-rule '(* ?x 1) '?x)       ; x * 1 = x
    (make-rule '(* 1 ?x) '?x)       ; 1 * x = x
    (make-rule '(* ?x 0) '0)        ; x * 0 = 0
    (make-rule '(* 0 ?x) '0)))      ; 0 * x = 0
```

**Commutativity**:

```scheme
(define arith-comm-rules
  (list
    (make-rule '(+ ?x ?y) '(+ ?y ?x))  ; x + y = y + x
    (make-rule '(* ?x ?y) '(* ?y ?x))))  ; x * y = y * x
```

**Associativity**:

```scheme
(define arith-assoc-rules
  (list
    (make-rule '(+ (+ ?x ?y) ?z) '(+ ?x (+ ?y ?z)))
    (make-rule '(* (* ?x ?y) ?z) '(* ?x (* ?y ?z)))))
```

**Distributivity**:

```scheme
(define arith-distrib-rules
  (list
    (make-rule '(* ?x (+ ?y ?z)) '(+ (* ?x ?y) (* ?x ?z)))
    (make-rule '(+ (* ?x ?y) (* ?x ?z)) '(* ?x (+ ?y ?z)))))
```

### 7c.3 Cost Models

Cost models assign numeric costs to e-nodes, enabling extraction of optimal forms.

#### 7c.3.1 CUDA Cost Model

The CUDA cost model optimizes for GPU execution:

| Operation | Cost | Rationale |
|-----------|------|-----------|
| `+`, `-` | 1 | Fast ALU operations |
| `*` | 2 | Slightly more expensive |
| `/`, `mod` | 10 | Division is costly on GPU |
| `sqrt` | 15 | Special function unit |
| `rsqrt` | 8 | Optimized on NVIDIA hardware |
| `fma` | 3 | Fused multiply-add (preferred) |
| `load`, `store` | 100 | Memory bandwidth dominates |

```scheme
(optimize '(/ 1 (sqrt x))          ; Original
          cuda-rewrite-rules       ; Try rsqrt equivalence
          cuda-cost)               ; CUDA cost model
;; => (rsqrt x)                    ; Faster on GPU
```

**CUDA-specific rules**:

```scheme
(define cuda-intrinsic-rules
  (list
    (make-rule '(/ 1 (sqrt ?x)) '(rsqrt ?x))     ; Fast reciprocal sqrt
    (make-rule '(+ (* ?a ?b) ?c) '(fma ?a ?b ?c)) ; Fused multiply-add
    (make-rule '(* ?x ?x) '(sq ?x))))            ; Optimized squaring
```

#### 7c.3.2 CPU Cost Model

The CPU cost model has different priorities:

| Operation | Cost | Rationale |
|-----------|------|-----------|
| `+`, `-`, `*` | 1 | Modern CPUs have fast ALU/FPU |
| `/` | 5 | Division slower than multiplication |
| `sqrt` | 10 | Special instruction, pipelined |
| `fma` | 1 | Native FMA on modern x86 |
| Memory ops | Variable | Depends on cache hierarchy |

#### 7c.3.3 Size Cost Model

The size cost model minimizes AST size (useful for code generation):

```scheme
(define (size-cost eg node)
  (+ 1  ; This node
     (sum (map (lambda (child) (class-cost eg child))
               (vector->list (enode-children node))))))
```

#### 7c.3.4 Cost Computation

Costs are computed via dynamic programming:
1. Initialize all classes with infinite cost
2. Iterate until fixpoint:
   - For each e-class, compute minimum cost across all e-nodes
   - E-node cost = base cost + sum of child costs

```scheme
(define (compute-costs eg cost-model)
  (let ([costs (make-hashtable)])
    ;; Initialize to infinity
    (for-each (lambda (root) (hashtable-set! costs root +inf.0))
              (uf-roots (egraph-uf eg)))
    ;; Iterate until fixpoint
    (let loop ([changed #t])
      (when changed
        (set! changed #f)
        (for-each (lambda (root)
                    (for-each (lambda (node)
                                (let ([c (node-cost cost-model node costs)])
                                  (when (< c (hashtable-ref costs root +inf.0))
                                    (hashtable-set! costs root c)
                                    (set! changed #t))))
                              (eclass-get-nodes (egraph-store eg) root)))
                  (uf-roots (egraph-uf eg))))
        (loop changed)))
    costs))
```

**Node cost**:

```scheme
(define (node-cost cost-model node costs)
  (+ (cost-model-base-cost cost-model (enode-op node))
     (sum (map (lambda (child) (hashtable-ref costs child +inf.0))
               (vector->list (enode-children node))))))
```

### 7c.4 Extraction

Extraction recovers a concrete term from the e-graph by selecting the minimum-cost e-node from each e-class:

```scheme
(define (extract state class-id)
  (let ([best-node (hashtable-ref (state-best-nodes state) class-id)])
    (if (leaf? best-node)
        (enode-op best-node)
        (cons (enode-op best-node)
              (map (lambda (child) (extract state child))
                   (vector->list (enode-children best-node)))))))
```

**Finding best nodes**:

After cost computation, find the minimum-cost e-node in each e-class:

```scheme
(define (find-best-nodes eg costs)
  (let ([best (make-hashtable)])
    (for-each (lambda (root)
                (let ([nodes (eclass-get-nodes (egraph-store eg) root)])
                  (let loop ([nodes nodes] [min-node #f] [min-cost +inf.0])
                    (if (null? nodes)
                        (hashtable-set! best root min-node)
                        (let ([c (node-cost cost-model (car nodes) costs)])
                          (if (< c min-cost)
                              (loop (cdr nodes) (car nodes) c)
                              (loop (cdr nodes) min-node min-cost)))))))
              (uf-roots (egraph-uf eg)))
    best))
```

**High-level API**:

```scheme
(optimize term rules cost-model)
;; 1. Build e-graph from term
;; 2. Saturate with rules
;; 3. Extract minimum-cost equivalent
```

**Example**:

```scheme
(optimize '(+ (* a b) (* a c))
          arith-distrib-rules
          size-cost)
;; => (* a (+ b c))  ; Smaller AST
```

### 7c.5 Rule Scheduling

Naive saturation applies all rules to all classes every iteration—expensive for large e-graphs. **Scheduling strategies** improve performance:

#### 7c.5.1 Backoff Scheduler

Tracks per-rule statistics and reduces priority for unproductive rules:

```scheme
(define (update-stats! stats matches)
  (if (zero? matches)
      (begin
        (inc! (stats-zero-streak stats))
        (when (>= (stats-zero-streak stats) threshold)
          (set-priority! stats (* (get-priority stats) 0.5))))  ; Back off
      (begin
        (set-zero-streak! stats 0)
        (set-priority! stats (* (get-priority stats) 1.5)))))   ; Boost
```

**Usage**:

```scheme
(define scheduler (make-backoff-scheduler rules))
(saturate-with-scheduler eg scheduler config)
```

The scheduler:
1. Applies high-priority rules first
2. Tracks match counts per rule
3. Reduces priority after N consecutive zero-match iterations
4. Boosts priority when rule starts matching again

#### 7c.5.2 Priority Scheduler

Sorts rules by recent productivity, applying most-likely-to-match rules first:

```scheme
(define (priority-scheduler-step! scheduler eg)
  (let ([rules (sort-by-priority (scheduler-rules scheduler))])
    (for-each (lambda (rule)
                (let ([matches (apply-rule! eg rule)])
                  (update-priority! scheduler rule matches)))
              rules)))
```

#### 7c.5.3 Worklist Scheduler

Only processes e-classes that changed in the previous iteration—essential for large e-graphs where most classes are stable:

```scheme
(define (worklist-saturate eg rules config)
  (let ([worklist (all-classes eg)])
    (let loop ([fuel (config-fuel config)])
      (if (or (null? worklist) (zero? fuel))
          'done
          (let* ([class-id (pop! worklist)]
                 [new-merges (apply-rules-to-class! eg rules class-id)])
            (egraph-rebuild! eg)
            (for-each (lambda (merged) (push! worklist merged))
                      new-merges)
            (loop (- fuel 1)))))))
```

**Key insight**: After merging classes A and B, only e-classes that use A or B as children need to be re-checked. The worklist tracks these.

### 7c.6 Integration with CUDA Codegen

The e-graph system integrates with CUDA code generation:

```scheme
;; 1. Parse CUDA kernel expression
(define expr (parse-cuda-kernel source))

;; 2. Optimize using e-graph
(define optimized
  (optimize expr
            (append cuda-algebraic-rules
                    cuda-intrinsic-rules)  ; rsqrt, fma, etc.
            cuda-cost))

;; 3. Generate CUDA code from optimized form
(define cuda-code (emit-cuda optimized))
```

**Example pipeline**:

```scheme
;; Original: distance calculation
(define expr
  '(sqrt (+ (* (- x1 x2) (- x1 x2))
            (* (- y1 y2) (- y1 y2)))))

;; After algebraic simplification
;; => (sqrt (+ (sq (- x1 x2)) (sq (- y1 y2))))

;; After CUDA intrinsic optimization
;; => (norm2 (- x1 x2) (- y1 y2))  ; Uses built-in norm2 function
```

**CUDA codegen rules**:

```scheme
(define cuda-algebraic-rules
  (append arith-identity-rules
          arith-comm-rules
          arith-assoc-rules
          arith-distrib-rules))

(define cuda-intrinsic-rules
  (list
    (make-rule '(/ 1 (sqrt ?x)) '(rsqrt ?x))
    (make-rule '(+ (* ?a ?b) ?c) '(fma ?a ?b ?c))
    (make-rule '(* ?x ?x) '(sq ?x))
    (make-rule '(sqrt (+ (sq ?x) (sq ?y))) '(norm2 ?x ?y))
    (make-rule '(sqrt (+ (sq ?x) (+ (sq ?y) (sq ?z)))) '(norm3 ?x ?y ?z))))
```

### 7c.7 Design Decisions

**Why separate cost models?**

Different targets need different optimizations:
- CUDA: Minimize memory bandwidth, prefer FMA
- CPU: Minimize division, exploit caching
- Code size: Minimize instruction count

The cost model abstraction enables target-specific optimization without changing the saturation logic.

**Why scheduling?**

Without scheduling, saturation is O(rules × classes × matches) per iteration. For large e-graphs:
- Backoff prevents wasting time on dormant rules
- Worklist avoids redundant work on stable classes
- Priority focuses on productive rules first

**Why fuel limits?**

Equality saturation can run indefinitely on some rule sets (e.g., `(+ x 0) → x` and `x → (+ x 0)` form a cycle). Fuel limits ensure termination:
- Fuel per iteration: Prevents explosion in a single iteration
- Total fuel: Bounds overall work
- Node limit: Prevents memory exhaustion

### 7c.8 Performance Characteristics

| Operation | Complexity |
|-----------|------------|
| `ematch` | O(pattern size × class nodes) |
| `saturate` | O(fuel) bounded |
| `compute-costs` | O(classes × nodes) |
| `extract` | O(term size) |

**Saturation complexity**:

Worst-case saturation is O(fuel × rules × classes × pattern-size). In practice:
- Scheduling reduces the constant factor
- Most rules match rarely (backoff helps)
- Worklist limits redundant work

**Empirical results**:

On typical CUDA kernel expressions (50-100 AST nodes):
- Saturation converges in 5-10 iterations
- ~100-500 rule applications total
- E-graph size: 200-1000 e-classes
- Runtime: <100ms

### 7c.9 Limitations

**No conditional rewriting**: Rules are unconditional. We cannot express "apply this rule only if X is a constant" without extending the pattern language.

**No e-graph analysis**: Advanced egg features like "e-class analysis" (lattice-based data propagation) are not yet implemented. This would enable constant folding, sign analysis, etc.

**Single-threaded**: As noted in Chapter 7b, the e-graph is not thread-safe. Parallel saturation (partitioning the e-graph or rule set) is future work.

**No proof terms**: We don't track *why* two terms are equivalent (which rules were applied). This makes debugging harder and prevents proof-carrying code.

---
