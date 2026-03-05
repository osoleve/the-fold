;;; lattice/meta/analyzer-fuel.ss — Fuel cost inference analyzer
;;; @module meta/analyzer-fuel
;;; @requires prelude meta/lattice-walk meta/analyzer-effects

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/lattice-walk)
(require 'meta/analyzer-effects)

(doc 'module 'meta/analyzer-fuel)
(doc 'description "Fuel cost inference analyzer for the lattice walker.
Estimates per-call fuel consumption for lattice functions by walking expression
trees. Processes modules in dependency order so cross-module calls resolve
to known costs. Uses fixed-point iteration within each module to detect
recursive functions. Fuel semantics follow eval.ss: each expression evaluation
costs 1 fuel unit, primitives are free, branching takes worst-case path.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Fuel Cost Lattice
;;; ============================================================================

(doc 'section 'fuel-cost-lattice)

;;; FuelCost = Nat | 'recursive | 'unknown
;;; - Nat: deterministic cost per call
;;; - 'recursive: contains self/mutual recursion (unbounded)
;;; - 'unknown: can't determine (opaque call target)

(doc fuel-cost? 'export #t)
(define (fuel-cost? x)
  (or (and (integer? x) (>= x 0))
      (eq? x 'recursive)
      (eq? x 'unknown)))

;;; fuel-cost-add : FuelCost FuelCost -> FuelCost
;;; Sequential composition: costs add.
;;; Recursive dominates unknown (if we know it recurses, that's stronger
;;; than "can't determine"). Both absorb numeric values.
(doc fuel-cost-add 'export #t)
(define (fuel-cost-add a b)
  (cond
    [(or (eq? a 'recursive) (eq? b 'recursive)) 'recursive]
    [(or (eq? a 'unknown) (eq? b 'unknown)) 'unknown]
    [else (+ a b)]))

;;; fuel-cost-max : FuelCost FuelCost -> FuelCost
;;; Branching composition: worst-case path.
(doc fuel-cost-max 'export #t)
(define (fuel-cost-max a b)
  (cond
    [(or (eq? a 'recursive) (eq? b 'recursive)) 'recursive]
    [(or (eq? a 'unknown) (eq? b 'unknown)) 'unknown]
    [else (max a b)]))

;;; fuel-cost-sum : (List FuelCost) -> FuelCost
(doc fuel-cost-sum 'export #t)
(define (fuel-cost-sum costs)
  (fold-left fuel-cost-add 0 costs))

;;; ============================================================================
;;; Primitive Fuel Database
;;; ============================================================================

(doc 'section 'primitives)

;;; seed-fuel-primitives! : Hashtable -> Void
;;; Seed the fuel DB with known Chez Scheme primitive costs.
;;; Primitives are free (cost 0) — they don't consume eval fuel.
(define (seed-fuel-primitives! db)
  (for-each
   (lambda (name) (hashtable-set! db name 0))
   '(+ - * / < > = <= >= not
     car cdr cons null? pair? list? length append reverse
     list list-ref list-tail
     map filter fold-left fold-right for-each exists for-all
     apply values call-with-values
     string-length string-append substring string-ref string=? string<?
     string->symbol symbol->string
     number->string string->number
     vector vector-ref vector-length vector->list list->vector
     make-vector vector-map vector-for-each
     bytevector-length bytevector-u8-ref
     equal? eq? eqv?
     abs min max quotient remainder modulo gcd lcm
     floor ceiling round truncate exact->inexact inexact->exact
     sin cos tan asin acos atan sqrt exp log expt
     bitwise-and bitwise-or bitwise-xor bitwise-not
     bitwise-arithmetic-shift-left bitwise-arithmetic-shift-right
     char->integer integer->char char=? char<? char-alphabetic? char-numeric?
     boolean? symbol? string? number? integer? real? char? vector? bytevector?
     procedure? port? eof-object?
     assq assv assoc memq memv member
     caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr
     cadddr
     sort list-sort
     make-eq-hashtable make-eqv-hashtable
     hashtable? hashtable-ref hashtable-contains? hashtable-keys hashtable-size
     hashtable-set! hashtable-delete! hashtable-update! hashtable-clear!
     even? odd? zero? positive? negative?
     exact? inexact?
     make-string string-copy string-set!
     with-exception-handler condition?
     make-list iota
     append-map filter-map
     unique
     format
     display newline printf fprintf
     read write
     error assertion-violation raise raise-continuable
     guard
     set! set-car! set-cdr!
     vector-set! vector-fill!
     bytevector-u8-set!)))

;;; ============================================================================
;;; Fuel Cost Inference
;;; ============================================================================

(doc 'section 'inference)

;;; infer-fuel-cost : SExp Hashtable -> FuelCost
;;; Infer the fuel cost of an expression given a fuel cost database.
;;; Models eval.ss semantics: each expression evaluation costs 1 fuel,
;;; branching takes worst-case path, lambda bodies are latent.
(define (infer-fuel-cost expr db)
  (cond
    ;; Literals: 1 eval step
    [(or (number? expr) (boolean? expr) (string? expr) (char? expr))
     1]
    ;; Symbol reference: 1 eval step (variable lookup)
    [(symbol? expr) 1]
    ;; Non-pair non-atoms
    [(not (pair? expr)) 1]
    ;; Quote: 1 eval step
    [(eq? (car expr) 'quote) 1]
    ;; Quasiquote: 1 + cost of unquoted expressions
    [(eq? (car expr) 'quasiquote)
     (if (pair? (cdr expr))
         (fuel-cost-add 1 (infer-quasiquote-fuel-cost (cadr expr) db))
         1)]
    ;; Lambda: 1 eval step (closure creation, body is latent)
    [(memq (car expr) '(lambda case-lambda fn))
     1]
    ;; Let forms
    [(memq (car expr) '(let let* letrec letrec*))
     (infer-let-fuel-cost expr db)]
    ;; If: 1 + test + max(branches)
    [(eq? (car expr) 'if)
     (let* ([test-cost (infer-fuel-cost (cadr expr) db)]
            [then-cost (if (pair? (cddr expr))
                           (infer-fuel-cost (caddr expr) db)
                           0)]
            [else-cost (if (and (pair? (cddr expr)) (pair? (cdddr expr)))
                           (infer-fuel-cost (cadddr expr) db)
                           0)])
       (fuel-cost-add 1 (fuel-cost-add test-cost
                                        (fuel-cost-max then-cost else-cost))))]
    ;; Cond: 1 + worst-case clause
    [(eq? (car expr) 'cond)
     (fuel-cost-add
      1
      (fold-left fuel-cost-max 0
        (map (lambda (clause)
               (if (pair? clause)
                   (fuel-cost-sum (map (lambda (e) (infer-fuel-cost e db)) clause))
                   0))
             (cdr expr))))]
    ;; Begin: sum of all expressions
    [(eq? (car expr) 'begin)
     (fuel-cost-sum (map (lambda (e) (infer-fuel-cost e db)) (cdr expr)))]
    ;; When/unless: 1 + test + body
    [(memq (car expr) '(when unless))
     (if (and (pair? (cdr expr)) (pair? (cddr expr)))
         (fuel-cost-add
          1
          (fuel-cost-add
           (infer-fuel-cost (cadr expr) db)
           (fuel-cost-sum (map (lambda (e) (infer-fuel-cost e db)) (cddr expr)))))
         1)]
    ;; Set!: 1 + cost of value expression
    [(eq? (car expr) 'set!)
     (fuel-cost-add 1 (if (and (pair? (cdr expr)) (pair? (cddr expr)))
                          (infer-fuel-cost (caddr expr) db)
                          0))]
    ;; And/or: worst case evaluates all subexpressions
    [(memq (car expr) '(and or))
     (fuel-cost-add 1 (fuel-cost-sum (map (lambda (e) (infer-fuel-cost e db))
                                          (cdr expr))))]
    ;; Define: 0 (top-level metadata, not executed)
    [(eq? (car expr) 'define) 0]
    ;; Doc/require/load: 0 (metadata, not executed)
    [(memq (car expr) '(doc require load)) 0]
    ;; Case: 1 + key eval + worst-case clause
    [(eq? (car expr) 'case)
     (if (pair? (cdr expr))
         (fuel-cost-add
          1
          (fuel-cost-add
           (infer-fuel-cost (cadr expr) db)
           (fold-left fuel-cost-max 0
             (map (lambda (clause)
                    (if (and (pair? clause) (pair? (cdr clause)))
                        (fuel-cost-sum (map (lambda (e) (infer-fuel-cost e db))
                                            (cdr clause)))
                        0))
                  (cddr expr)))))
         1)]
    ;; Application: 1 (apply) + operator eval + callee body cost + argument costs
    ;; eval.ss evaluates the operator expression (1 fuel for symbol lookup),
    ;; then each argument, then applies (entering the callee body).
    [(pair? expr)
     (let* ([fn-name (car expr)]
            [operator-eval-cost (if (symbol? fn-name) 1 (infer-fuel-cost fn-name db))]
            [callee-cost (if (symbol? fn-name)
                             (hashtable-ref db fn-name 'unknown)
                             0)]  ; non-symbol operator: eval cost already covers it
            [arg-costs (fuel-cost-sum
                        (map (lambda (a) (infer-fuel-cost a db))
                             (cdr expr)))])
       (fuel-cost-add 1 (fuel-cost-add operator-eval-cost
                                        (fuel-cost-add callee-cost arg-costs))))]
    [else 'unknown]))

;;; infer-let-fuel-cost : SExp Hashtable -> FuelCost
;;; Handle let/let*/letrec/named-let forms.
(define (infer-let-fuel-cost expr db)
  (cond
    ;; Named let: (let name ([var init] ...) body ...)
    [(and (pair? (cdr expr))
          (symbol? (cadr expr))
          (pair? (cddr expr))
          (list? (caddr expr)))
     (let* ([bindings (caddr expr)]
            [body (cdddr expr)]
            [binding-costs (fuel-cost-sum
                            (map (lambda (b)
                                   (if (and (pair? b) (pair? (cdr b)))
                                       (infer-fuel-cost (cadr b) db)
                                       0))
                                 bindings))]
            [body-cost (fuel-cost-sum
                        (map (lambda (e) (infer-fuel-cost e db)) body))])
       (fuel-cost-add 1 (fuel-cost-add binding-costs body-cost)))]
    ;; Regular let: (let ([var init] ...) body ...)
    [(and (pair? (cdr expr)) (list? (cadr expr)))
     (let* ([bindings (cadr expr)]
            [body (cddr expr)]
            [binding-costs (fuel-cost-sum
                            (map (lambda (b)
                                   (if (and (pair? b) (pair? (cdr b)))
                                       (infer-fuel-cost (cadr b) db)
                                       0))
                                 bindings))]
            [body-cost (fuel-cost-sum
                        (map (lambda (e) (infer-fuel-cost e db)) body))])
       (fuel-cost-add 1 (fuel-cost-add binding-costs body-cost)))]
    [else 1]))

;;; infer-quasiquote-fuel-cost : SExp Hashtable -> FuelCost
;;; Walk quasiquoted data, only counting costs in unquote positions.
(define (infer-quasiquote-fuel-cost datum db)
  (cond
    [(not (pair? datum)) 0]
    [(and (eq? (car datum) 'unquote) (pair? (cdr datum)))
     (infer-fuel-cost (cadr datum) db)]
    [(and (eq? (car datum) 'unquote-splicing) (pair? (cdr datum)))
     (infer-fuel-cost (cadr datum) db)]
    [else
     (fuel-cost-add (infer-quasiquote-fuel-cost (car datum) db)
                    (infer-quasiquote-fuel-cost (cdr datum) db))]))

;;; ============================================================================
;;; Recursion Detection (Call Graph)
;;; ============================================================================

(doc 'section 'recursion-detection)

;;; collect-local-refs : SExp Hashtable -> (List Symbol)
;;; Collect all unquoted symbols in expr that are local definitions.
(define (collect-local-refs expr name-set)
  (let ([result '()])
    (let walk ([e expr])
      (cond
        [(and (symbol? e) (hashtable-ref name-set e #f))
         (unless (memq e result) (set! result (cons e result)))]
        [(not (pair? e)) (void)]
        [(eq? (car e) 'quote) (void)]
        [else (walk (car e)) (walk (cdr e))]))
    result))

;;; can-reach? : Symbol Symbol Hashtable Hashtable -> Boolean
;;; DFS reachability: can target be reached from start via the call graph?
(define (can-reach? start target graph visited)
  (let ([neighbors (hashtable-ref graph start '())])
    (exists
     (lambda (n)
       (cond
         [(eq? n target) #t]
         [(hashtable-ref visited n #f) #f]
         [else
          (hashtable-set! visited n #t)
          (can-reach? n target graph visited)]))
     neighbors)))

;;; find-recursive-defs : (List (Pair Symbol SExp)) -> (List Symbol)
;;; Build local call graph and return names involved in cycles
;;; (self-recursion or mutual recursion).
(define (find-recursive-defs defs)
  (let ([name-set (make-eq-hashtable)]
        [call-graph (make-eq-hashtable)]
        [names (map car defs)])
    ;; Register local names
    (for-each (lambda (n) (hashtable-set! name-set n #t)) names)
    ;; Build call graph: for each def, which local names does its body reference?
    (for-each
     (lambda (def)
       (hashtable-set! call-graph (car def)
                       (collect-local-refs (cdr def) name-set)))
     defs)
    ;; Find names that can reach themselves via the call graph
    (filter
     (lambda (name)
       (can-reach? name name call-graph (make-eq-hashtable)))
     names)))

;;; ============================================================================
;;; Fixed-Point Inference
;;; ============================================================================

(doc 'section 'fixpoint)

;;; infer-module-fuel-costs : (List (Pair Symbol SExp)) Hashtable -> (List (Pair Symbol FuelCost))
;;; Run fuel cost inference on a module's definitions to fixed point.
;;; First detects recursive functions structurally via call-graph cycle detection,
;;; then iterates to propagate costs for non-recursive functions.
(define (infer-module-fuel-costs defs db)
  (let* ([names (map car defs)]
         ;; Structural recursion detection — find names in call-graph cycles
         [recursive-names (find-recursive-defs defs)]
         ;; Scale max-iters with def count for deep acyclic chains
         [max-iters (max 20 (length defs))])
    ;; Pre-mark recursive functions
    (for-each (lambda (name) (hashtable-set! db name 'recursive)) recursive-names)
    ;; Seed remaining defs as cost 0 (optimistic)
    (for-each
     (lambda (name)
       (unless (memq name recursive-names)
         (hashtable-set! db name 0)))
     names)
    ;; Iterate to fixed point for non-recursive functions
    (let loop ([iter 0])
      (let* ([results (map (lambda (def)
                             (let ([name (car def)])
                               (if (memq name recursive-names)
                                   (cons name 'recursive)
                                   (cons name (infer-fuel-cost (cdr def) db)))))
                           defs)]
             ;; Check for changes among non-recursive defs
             [changed? (exists (lambda (r)
                                 (and (not (eq? (cdr r) 'recursive))
                                      (let ([prev (hashtable-ref db (car r) 'unknown)])
                                        (not (equal? (cdr r) prev)))))
                               results)])
        ;; Update DB
        (for-each (lambda (r) (hashtable-set! db (car r) (cdr r))) results)
        (if (and changed? (< iter max-iters))
            (loop (+ iter 1))
            results)))))

;;; ============================================================================
;;; Analyzer
;;; ============================================================================

(doc 'section 'analyzer)

;;; make-fuel-analyzer : -> Analyzer
;;; Creates a fuel cost inference analyzer.
;;; State: eq-hashtable mapping Symbol -> FuelCost.
;;; Seeded with Chez primitives (cost 0). Costs persist across modules
;;; so downstream modules see resolved callee costs.
(doc make-fuel-analyzer 'export #t)
(doc make-fuel-analyzer 'type '(-> Analyzer))
(define (make-fuel-analyzer)
  (make-analyzer
   'fuel
   ;; init: seed with Chez primitives
   (lambda ()
     (let ([db (make-eq-hashtable)])
       (seed-fuel-primitives! db)
       db))
   ;; step: analyze module, persist results
   (lambda (mr acc state)
     (let* ([sexps (module-record-sexps mr)]
            [defs (extract-definitions sexps)])
       ;; Run fixed-point inference (modifies state in-place)
       (infer-module-fuel-costs defs state)
       ;; Return the same hashtable (modified)
       state))))

;;; ============================================================================
;;; Query Helpers
;;; ============================================================================

(doc 'section 'queries)

;;; fuel-db-lookup : Hashtable Symbol -> FuelCost
(doc fuel-db-lookup 'export #t)
(define (fuel-db-lookup db name)
  (hashtable-ref db name 'unknown))

;;; fuel-db-module-summary : Hashtable (List Symbol) -> (List (Pair Symbol FuelCost))
;;; Get fuel costs for a list of function names.
(doc fuel-db-module-summary 'export #t)
(define (fuel-db-module-summary db names)
  (map (lambda (n) (cons n (hashtable-ref db n 'unknown))) names))

;;; fuel-db-hot-functions : Hashtable Nat -> (List (Pair Symbol Nat))
;;; Return functions whose cost exceeds the given threshold, sorted by cost descending.
(doc fuel-db-hot-functions 'export #t)
(define (fuel-db-hot-functions db threshold)
  (let* ([keys (vector->list (hashtable-keys db))]
         [hot (filter (lambda (k)
                        (let ([c (hashtable-ref db k 0)])
                          (and (integer? c) (> c threshold))))
                      keys)]
         [pairs (map (lambda (k) (cons k (hashtable-ref db k 0))) hot)])
    (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs)))

;;; fuel-db-recursive-functions : Hashtable -> (List Symbol)
;;; Return all functions detected as recursive.
(doc fuel-db-recursive-functions 'export #t)
(define (fuel-db-recursive-functions db)
  (let ([keys (vector->list (hashtable-keys db))])
    (filter (lambda (k) (eq? 'recursive (hashtable-ref db k 'unknown))) keys)))
