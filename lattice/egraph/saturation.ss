;;; @module egraph/saturation
;;; @requires prelude egraph/match
;;; @description Equality saturation loop with resource limits
;;; @purity partial
;;; @stability experimental
;;; lattice/egraph/saturation.ss — Equality Saturation Loop
;;;
;;; Equality saturation repeatedly applies rewrite rules until no new
;;; equivalences are discovered (fixpoint) or a resource limit is reached.
;;;
;;; The algorithm:
;;; 1. Match all rules against all e-classes
;;; 2. Apply matches (instantiate RHS, merge with matched class)
;;; 3. Rebuild e-graph to restore congruence invariants
;;; 4. Repeat until fixpoint or fuel exhausted
;;;
;;; Resource management:
;;; - Fuel: Maximum number of saturation iterations
;;; - Node limit: Maximum e-graph size (prevents explosion)
;;; - Time limit: Optional wall-clock timeout
;;;
;;; This is Lattice code: pure saturation logic with controlled effects.

(require 'prelude)
(require 'egraph/match)

(doc 'module 'egraph/saturation)
(doc 'description "Equality saturation loop with resource management")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'partial)  ; Mutation for e-graph updates

;;; ============================================================
;;; Saturation Configuration
;;; ============================================================

(doc 'section 'config)

;;; Configuration record for saturation
;;; - fuel: Maximum iterations (0 = unlimited)
;;; - node-limit: Maximum e-nodes (0 = unlimited)
;;; - iter-limit: Maximum rules applied per iteration (0 = unlimited)

(define saturation-config-tag 'saturation-config)

(define (make-saturation-config fuel node-limit iter-limit)
  (doc 'type (-> Nat Nat Nat SaturationConfig))
  (doc 'description "Create saturation configuration.")
  (doc 'export #t)
  (vector saturation-config-tag fuel node-limit iter-limit))

(define (saturation-config? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a saturation configuration.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 4)
       (eq? (vector-ref x 0) saturation-config-tag)))

(define (config-fuel cfg)
  (doc 'export #t)
  (vector-ref cfg 1))
(define (config-node-limit cfg)
  (doc 'export #t)
  (vector-ref cfg 2))
(define (config-iter-limit cfg)
  (doc 'export #t)
  (vector-ref cfg 3))

;;; Default configuration
(define (default-saturation-config)
  (doc 'type (-> SaturationConfig))
  (doc 'description "Default saturation config: 100 iterations, 10000 nodes.")
  (doc 'export #t)
  (make-saturation-config 100 10000 0))

;;; ============================================================
;;; Saturation Result
;;; ============================================================

(doc 'section 'result)

;;; Result of saturation
;;; - status: 'saturated, 'fuel-exhausted, 'node-limit, 'iter-limit
;;; - iterations: Number of iterations performed
;;; - rules-applied: Total rule applications
;;; - final-classes: Number of e-classes at end
;;; - final-nodes: Number of e-nodes at end

(define saturation-result-tag 'saturation-result)

(define (make-saturation-result status iterations rules-applied final-classes final-nodes)
  (doc 'type (-> Symbol Nat Nat Nat Nat SaturationResult))
  (doc 'description "Create saturation result.")
  (vector saturation-result-tag status iterations rules-applied final-classes final-nodes))

(define (saturation-result? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if x is a saturation result.")
  (doc 'export #t)
  (and (vector? x)
       (>= (vector-length x) 6)
       (eq? (vector-ref x 0) saturation-result-tag)))

(define (result-status r)
  (doc 'export #t)
  (vector-ref r 1))
(define (result-iterations r)
  (doc 'export #t)
  (vector-ref r 2))
(define (result-rules-applied r) (vector-ref r 3))
(define (result-final-classes r)
  (doc 'export #t)
  (vector-ref r 4))
(define (result-final-nodes r)
  (doc 'export #t)
  (vector-ref r 5))

(define (result-saturated? r)
  (doc 'type (-> SaturationResult Boolean))
  (doc 'description "Check if saturation reached fixpoint.")
  (doc 'export #t)
  (eq? (result-status r) 'saturated))

;;; ============================================================
;;; Saturation Loop
;;; ============================================================

(doc 'section 'loop)

;;; saturate : EGraph × (List Rule) × SaturationConfig → SaturationResult
;;; Run equality saturation until fixpoint or limit reached.
(define (saturate eg rules config)
  (doc 'type (-> EGraph (List Rule) SaturationConfig SaturationResult))
  (doc 'description "Run equality saturation. Returns result with status.")
  (doc 'export #t)
  (let ([fuel (config-fuel config)]
        [node-limit (config-node-limit config)]
        [iter-limit (config-iter-limit config)])
    (let loop ([iteration 0]
               [total-applied 0])
      (cond
        ;; Check fuel limit
        [(and (> fuel 0) (>= iteration fuel))
         (make-saturation-result 'fuel-exhausted iteration total-applied
                                (egraph-class-count eg)
                                (egraph-node-count eg))]

        ;; Check node limit
        [(and (> node-limit 0) (> (egraph-node-count eg) node-limit))
         (make-saturation-result 'node-limit iteration total-applied
                                (egraph-class-count eg)
                                (egraph-node-count eg))]

        ;; Run one iteration
        [else
         (let ([applied (saturate-iteration eg rules iter-limit)])
           ;; Rebuild after rule applications
           (egraph-saturate-rebuild! eg)

           (cond
             ;; No rules applied - reached fixpoint
             [(zero? applied)
              (make-saturation-result 'saturated iteration total-applied
                                     (egraph-class-count eg)
                                     (egraph-node-count eg))]

             ;; Hit iteration limit
             [(and (> iter-limit 0) (>= applied iter-limit))
              (make-saturation-result 'iter-limit (+ iteration 1)
                                     (+ total-applied applied)
                                     (egraph-class-count eg)
                                     (egraph-node-count eg))]

             ;; Continue to next iteration
             [else
              (loop (+ iteration 1) (+ total-applied applied))]))]))))

;;; saturate-iteration : EGraph × (List Rule) × Nat → Nat
;;; Apply all rules once. Returns number of NEW rule applications.
;;; Only counts applications that actually change the e-graph.
;;; If limit > 0, stop after that many applications.
(define (saturate-iteration eg rules limit)
  (doc 'type (-> EGraph (List Rule) Nat Nat))
  (doc 'description "Run one saturation iteration. Returns applications count.")
  (let ([uf (egraph-uf eg)]
        [count 0]
        [stop #f])
    (for-each
     (lambda (root)
       (unless stop
         (for-each
          (lambda (rule)
            (unless stop
              (let ([matches (ematch eg (rule-lhs rule) root)])
                (for-each
                 (lambda (subst)
                   (unless stop
                     ;; Apply the rule
                     (let* ([rhs (rule-rhs rule)]
                            [rhs-term (pattern-apply subst rhs)]
                            [rhs-id (add-instantiated-term eg rhs-term)]
                            ;; Only count if this is a NEW equivalence
                            [root-class (egraph-find eg root)]
                            [rhs-class (egraph-find eg rhs-id)])
                       (unless (= root-class rhs-class)
                         (egraph-merge! eg root rhs-id)
                         (set! count (+ count 1))
                         ;; Check limit
                         (when (and (> limit 0) (>= count limit))
                           (set! stop #t))))))
                 matches))))
          rules)))
     (uf-roots uf))
    count))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

;;; saturate-simple : EGraph × (List Rule) → SaturationResult
;;; Saturate with default configuration.
(define (saturate-simple eg rules)
  (doc 'type (-> EGraph (List Rule) SaturationResult))
  (doc 'description "Run saturation with default configuration.")
  (doc 'export #t)
  (saturate eg rules (default-saturation-config)))

;;; saturate-with-fuel : EGraph × (List Rule) × Nat → SaturationResult
;;; Saturate with custom fuel limit.
(define (saturate-with-fuel eg rules fuel)
  (doc 'type (-> EGraph (List Rule) Nat SaturationResult))
  (doc 'description "Run saturation with custom fuel limit.")
  (doc 'export #t)
  (saturate eg rules (make-saturation-config fuel 10000 0)))

;;; ============================================================
;;; Rule Sets (Common Algebraic Rules)
;;; ============================================================

(doc 'section 'rulesets)

;;; Arithmetic identity rules: x+0=x, x*1=x, x*0=0
(define arith-identity-rules
  (list
   (make-rule '(+ ?x 0) '?x)
   (make-rule '(+ 0 ?x) '?x)
   (make-rule '(* ?x 1) '?x)
   (make-rule '(* 1 ?x) '?x)
   (make-rule '(* ?x 0) '0)
   (make-rule '(* 0 ?x) '0)))

;;; Arithmetic commutativity: x+y=y+x, x*y=y*x
(define arith-comm-rules
  (list
   (make-rule '(+ ?x ?y) '(+ ?y ?x))
   (make-rule '(* ?x ?y) '(* ?y ?x))))

;;; Arithmetic associativity: (x+y)+z=x+(y+z)
(define arith-assoc-rules
  (list
   (make-rule '(+ (+ ?x ?y) ?z) '(+ ?x (+ ?y ?z)))
   (make-rule '(+ ?x (+ ?y ?z)) '(+ (+ ?x ?y) ?z))
   (make-rule '(* (* ?x ?y) ?z) '(* ?x (* ?y ?z)))
   (make-rule '(* ?x (* ?y ?z)) '(* (* ?x ?y) ?z))))

;;; Distributivity: x*(y+z)=x*y+x*z
(define arith-distrib-rules
  (list
   (make-rule '(* ?x (+ ?y ?z)) '(+ (* ?x ?y) (* ?x ?z)))
   (make-rule '(+ (* ?x ?y) (* ?x ?z)) '(* ?x (+ ?y ?z)))))

;;; Basic arithmetic: identities + commutativity
(define basic-arith-rules
  (append arith-identity-rules arith-comm-rules))

;;; Full arithmetic rules (careful: associativity can explode!)
(define full-arith-rules
  (append arith-identity-rules
          arith-comm-rules
          arith-assoc-rules
          arith-distrib-rules))

;;; ============================================================
;;; Resumable Saturation
;;; ============================================================

(doc 'section 'resumable)

;;; Saturation suspension — captures loop state for later resumption.
;;; A suspension wraps a SaturationResult plus the continuation state
;;; needed to restart the loop: egraph, rules, iteration count, total applied.

(define saturation-suspension-tag 'saturation-suspension)

(define (make-saturation-suspension eg rules iteration total-applied config result)
  (vector saturation-suspension-tag eg rules iteration total-applied config result))

(doc 'type '(-> Any Boolean))
(doc 'description "Check if a value is a saturation suspension (resumable).")
(define (saturation-suspended? x)
  (and (vector? x)
       (>= (vector-length x) 7)
       (eq? (vector-ref x 0) saturation-suspension-tag)))

(define (suspension-egraph s) (vector-ref s 1))
(define (suspension-rules s) (vector-ref s 2))
(define (suspension-iteration s) (vector-ref s 3))
(define (suspension-total-applied s) (vector-ref s 4))
(define (suspension-config s) (vector-ref s 5))
(define (suspension-result s) (vector-ref s 6))

;;; Inner loop shared by saturate-resumable and saturate-resume.
;;; start-iter/start-total are the accumulated counts from prior runs.
;;; Config fuel is treated as RELATIVE: fuel=5 means 5 more iterations.
(define (saturate-resumable-loop eg rules config start-iter start-total)
  (let ([fuel (config-fuel config)]
        [node-limit (config-node-limit config)]
        [iter-limit (config-iter-limit config)])
    (let loop ([rel-iter 0]
               [total-applied start-total])
      (let ([abs-iter (+ start-iter rel-iter)])
        (cond
          ;; Fuel exhausted (relative to this run's budget)
          [(and (> fuel 0) (>= rel-iter fuel))
           (let ([result (make-saturation-result 'fuel-exhausted abs-iter total-applied
                           (egraph-class-count eg) (egraph-node-count eg))])
             (make-saturation-suspension eg rules abs-iter total-applied config result))]

          ;; Node limit
          [(and (> node-limit 0) (> (egraph-node-count eg) node-limit))
           (let ([result (make-saturation-result 'node-limit abs-iter total-applied
                           (egraph-class-count eg) (egraph-node-count eg))])
             (make-saturation-suspension eg rules abs-iter total-applied config result))]

          ;; Run one iteration
          [else
           (let ([applied (saturate-iteration eg rules iter-limit)])
             (egraph-saturate-rebuild! eg)
             (cond
               ;; Fixpoint
               [(zero? applied)
                (make-saturation-result 'saturated abs-iter total-applied
                  (egraph-class-count eg) (egraph-node-count eg))]

               ;; Iteration limit — suspend
               [(and (> iter-limit 0) (>= applied iter-limit))
                (let ([result (make-saturation-result 'iter-limit (+ abs-iter 1)
                                (+ total-applied applied)
                                (egraph-class-count eg) (egraph-node-count eg))])
                  (make-saturation-suspension eg rules (+ abs-iter 1)
                    (+ total-applied applied) config result))]

               ;; Continue
               [else (loop (+ rel-iter 1) (+ total-applied applied))]))])))))

(doc 'type '(-> EGraph (List Rule) SaturationConfig (Or SaturationResult SaturationSuspension)))
(doc 'description "Run equality saturation, returning a suspension on resource limits.
Returns a SaturationResult on fixpoint, or a SaturationSuspension when a limit is hit.
Use saturate-resume to continue from a suspension.")
(define (saturate-resumable eg rules config)
  (saturate-resumable-loop eg rules config 0 0))

(doc 'type '(-> SaturationSuspension (Or SaturationResult SaturationSuspension)))
(doc 'description "Resume saturation from a suspension. Uses the same config as the original.
Returns a result on fixpoint, or a new suspension if limits are hit again.")
(define (saturate-resume suspension)
  (let ([eg (suspension-egraph suspension)]
        [rules (suspension-rules suspension)]
        [iter (suspension-iteration suspension)]
        [total (suspension-total-applied suspension)]
        [config (suspension-config suspension)])
    (saturate-resumable-loop eg rules config iter total)))

(doc 'type '(-> SaturationSuspension SaturationConfig (Or SaturationResult SaturationSuspension)))
(doc 'description "Resume saturation with a new configuration. Useful for granting more fuel
or adjusting node limits after inspecting a suspension.")
(define (saturate-resume-with suspension new-config)
  (let ([eg (suspension-egraph suspension)]
        [rules (suspension-rules suspension)]
        [iter (suspension-iteration suspension)]
        [total (suspension-total-applied suspension)])
    (saturate-resumable-loop eg rules new-config iter total)))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

(define (result->string r)
  (doc 'type (-> SaturationResult String))
  (doc 'description "Convert saturation result to string.")
  (doc 'export #t)
  (format "SaturationResult(~a, iters=~a, applied=~a, classes=~a, nodes=~a)"
          (result-status r)
          (result-iterations r)
          (result-rules-applied r)
          (result-final-classes r)
          (result-final-nodes r)))

(define (saturation-debug eg rules config)
  (doc 'type (-> EGraph (List Rule) SaturationConfig Void))
  (doc 'description "Run saturation with debug output.")
  (doc 'export #t)
  (printf "Starting saturation with ~a rules~n" (length rules))
  (printf "Config: fuel=~a, node-limit=~a, iter-limit=~a~n"
          (config-fuel config)
          (config-node-limit config)
          (config-iter-limit config))
  (printf "Initial: ~a classes, ~a nodes~n"
          (egraph-class-count eg)
          (egraph-node-count eg))
  (let ([result (saturate eg rules config)])
    (printf "Result: ~a~n" (result->string result))
    result))

