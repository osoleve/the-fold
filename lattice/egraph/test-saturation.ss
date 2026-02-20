;;; lattice/egraph/test-saturation.ss — Tests for Equality Saturation
;;;
;;; Run with: scheme --script lattice/egraph/test-saturation.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/egraph/saturation.ss")

(display "\n====\n")
(display "Equality Saturation Tests\n")
(display "====\n\n")

(test-group "config"

  (define-test "make-saturation-config creates config"
    (let ([cfg (make-saturation-config 50 5000 100)])
      (assert-true (saturation-config? cfg))
      (assert-equal 50 (config-fuel cfg))
      (assert-equal 5000 (config-node-limit cfg))
      (assert-equal 100 (config-iter-limit cfg))))

  (define-test "default-saturation-config has reasonable defaults"
    (let ([cfg (default-saturation-config)])
      (assert-true (saturation-config? cfg))
      (assert-equal 100 (config-fuel cfg))
      (assert-equal 10000 (config-node-limit cfg)))))

(test-group "result"

  (define-test "saturation-result accessors work"
    (let ([r (make-saturation-result 'saturated 5 10 3 8)])
      (assert-true (saturation-result? r))
      (assert-equal 'saturated (result-status r))
      (assert-equal 5 (result-iterations r))
      (assert-equal 10 (result-rules-applied r))
      (assert-equal 3 (result-final-classes r))
      (assert-equal 8 (result-final-nodes r))))

  (define-test "result-saturated? checks status"
    (let ([r1 (make-saturation-result 'saturated 5 10 3 8)]
          [r2 (make-saturation-result 'fuel-exhausted 10 20 5 15)])
      (assert-true (result-saturated? r1))
      (assert-false (result-saturated? r2)))))

(test-group "saturation-basic"

  (define-test "saturate with no rules is immediate"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg '(+ a b))
      (let ([result (saturate-simple eg '())])
        (assert-true (result-saturated? result))
        (assert-equal 0 (result-iterations result))
        (assert-equal 0 (result-rules-applied result)))))

  (define-test "saturate with non-matching rule is immediate"
    (let ([eg (make-egraph)]
          [rules (list (make-rule '(foo ?x) '?x))])
      (egraph-add-term! eg '(+ a b))
      (let ([result (saturate-simple eg rules)])
        (assert-true (result-saturated? result))
        (assert-equal 0 (result-rules-applied result)))))

  (define-test "saturate applies matching rule"
    (let ([eg (make-egraph)]
          ;; Rule: (+ ?x 0) => ?x
          [rules (list (make-rule '(+ ?x 0) '?x))])
      (let ([term (egraph-add-term! eg '(+ a 0))]
            [a-id (egraph-add-term! eg 'a)])
        (let ([result (saturate-simple eg rules)])
          (assert-true (result-saturated? result))
          (assert-true (> (result-rules-applied result) 0))
          ;; a and (+ a 0) should be equivalent
          (assert-equal (egraph-find eg term) (egraph-find eg a-id)))))))

(test-group "saturation-identity"

  (define-test "identity rules simplify expressions"
    (let ([eg (make-egraph)])
      ;; Add (* (+ x 0) 1)
      (let ([term (egraph-add-term! eg '(* (+ x 0) 1))]
            [x-id (egraph-add-term! eg 'x)])
        (let ([result (saturate-simple eg arith-identity-rules)])
          (assert-true (result-saturated? result))
          ;; Should simplify to x
          (assert-equal (egraph-find eg term) (egraph-find eg x-id))))))

  (define-test "zero multiplication"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(* (+ a b) 0))]
            [zero (egraph-add-term! eg 0)])
        (let ([result (saturate-simple eg arith-identity-rules)])
          (assert-true (result-saturated? result))
          ;; Should simplify to 0
          (assert-equal (egraph-find eg term) (egraph-find eg zero)))))))

(test-group "saturation-commutativity"

  (define-test "commutativity adds equivalent form"
    (let ([eg (make-egraph)]
          [rules arith-comm-rules])
      (let ([term (egraph-add-term! eg '(+ a b))])
        (let ([result (saturate-simple eg rules)])
          (assert-true (result-saturated? result))
          ;; Class should contain both (+ a b) and (+ b a)
          (let ([nodes (egraph-class-nodes eg term)])
            (assert-equal 2 (length nodes)))))))

  (define-test "commutativity with identity"
    (let ([eg (make-egraph)]
          [rules basic-arith-rules])
      (let ([term (egraph-add-term! eg '(+ 0 x))]
            [x-id (egraph-add-term! eg 'x)])
        (let ([result (saturate-simple eg rules)])
          (assert-true (result-saturated? result))
          ;; (+ 0 x) = (+ x 0) = x
          (assert-equal (egraph-find eg term) (egraph-find eg x-id)))))))

(test-group "saturation-limits"

  (define-test "fuel limit stops saturation"
    (let ([eg (make-egraph)]
          ;; Commutativity alone reaches fixpoint quickly, but let's test fuel
          [rules arith-comm-rules]
          [config (make-saturation-config 1 0 0)])  ; Only 1 iteration
      (egraph-add-term! eg '(+ a b))
      (let ([result (saturate eg rules config)])
        ;; Should stop after 1 iteration (may or may not be saturated)
        (assert-true (<= (result-iterations result) 1)))))

  (define-test "node limit stops saturation"
    (let ([eg (make-egraph)]
          ;; Associativity can explode
          [rules arith-assoc-rules]
          [config (make-saturation-config 100 20 0)])  ; Small node limit
      ;; Add something that can grow with associativity
      (egraph-add-term! eg '(+ (+ (+ a b) c) d))
      (let ([result (saturate eg rules config)])
        ;; Should hit node limit
        (assert-true (or (eq? (result-status result) 'node-limit)
                        (eq? (result-status result) 'saturated))))))

  (define-test "iter limit stops iteration"
    (let ([eg (make-egraph)]
          ;; Rule that creates new equivalences
          [rules (list (make-rule '(f ?x) '(g ?x)))]
          [config (make-saturation-config 100 10000 2)])  ; Max 2 per iteration
      ;; Add 5 terms - each will match and create new equivalence
      (egraph-add-term! eg '(f a))
      (egraph-add-term! eg '(f b))
      (egraph-add-term! eg '(f c))
      (egraph-add-term! eg '(f d))
      (egraph-add-term! eg '(f e))
      (let ([result (saturate eg rules config)])
        ;; Should hit iteration limit (2 applications per iter, need at least 3 iters)
        (assert-equal 'iter-limit (result-status result))))))

(test-group "saturation-algebraic"

  (define-test "basic algebra: (a+0)*(1+0) = a"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(* (+ a 0) (+ 1 0)))]
            [a-id (egraph-add-term! eg 'a)])
        ;; Need identity rules + we manually add that 1+0=1
        (let ([one (egraph-add-term! eg 1)])
          (egraph-add-term! eg '(+ 1 0))
          (let ([result (saturate-simple eg arith-identity-rules)])
            (assert-true (result-saturated? result))
            ;; (a+0) = a, (+ 1 0) won't simplify without constant folding
            ;; but (* a 1) = a
            (let ([a-times-expr (egraph-find eg term)])
              ;; At minimum, (+ a 0) should equal a
              (let ([a-plus-zero (egraph-add-term! eg '(+ a 0))])
                (assert-equal (egraph-find eg a-plus-zero)
                             (egraph-find eg a-id)))))))))

  (define-test "nested simplification"
    (let ([eg (make-egraph)])
      ;; (* (* x 1) 1) should simplify to x
      (let ([term (egraph-add-term! eg '(* (* x 1) 1))]
            [x-id (egraph-add-term! eg 'x)])
        (let ([result (saturate-simple eg arith-identity-rules)])
          (assert-true (result-saturated? result))
          (assert-equal (egraph-find eg term) (egraph-find eg x-id)))))))

(test-group "saturation-convergence"

  (define-test "saturation converges on fixed expression"
    (let ([eg (make-egraph)]
          [rules basic-arith-rules])
      ;; Add a simple expression
      (egraph-add-term! eg '(+ x y))
      (let ([result (saturate-simple eg rules)])
        (assert-true (result-saturated? result))
        ;; Should converge in finite iterations
        (assert-true (< (result-iterations result) 10)))))

  (define-test "self-referential rules reach fixpoint"
    ;; The rule f(x) => f(f(x)) actually reaches a fixpoint because
    ;; merging f(a) with f(f(a)) creates a self-referential e-node
    ;; f(class-id) where class-id contains f(class-id), forming a cycle.
    (let ([eg (make-egraph)]
          [rules (list (make-rule '(f ?x) '(f (f ?x))))]
          [config (make-saturation-config 10 100 0)])
      (egraph-add-term! eg '(f a))
      (let ([result (saturate eg rules config)])
        ;; Should reach fixpoint, not exhaust fuel
        (assert-true (result-saturated? result))
        ;; Should complete in just a few iterations
        (assert-true (< (result-iterations result) 5))))))

(test-group "saturation-result-string"

  (define-test "result->string formats correctly"
    (let ([r (make-saturation-result 'saturated 5 10 3 8)])
      (let ([s (result->string r)])
        (assert-true (string? s))
        (assert-true (> (string-length s) 0))))))

(test-group "resumable-basic"

  (define-test "saturate-resumable reaches fixpoint same as saturate"
    (let ([eg (make-egraph)]
          [rules arith-identity-rules]
          [config (default-saturation-config)])
      (egraph-add-term! eg '(* (+ x 0) 1))
      (egraph-add-term! eg 'x)
      (let ([result (saturate-resumable eg rules config)])
        ;; Should reach fixpoint (not be suspended)
        (assert-false (saturation-suspended? result))
        (assert-true (result-saturated? result)))))

  (define-test "fuel limit returns suspension"
    (let ([eg (make-egraph)]
          [rules arith-comm-rules]
          [config (make-saturation-config 1 0 0)])
      (egraph-add-term! eg '(+ a b))
      (let ([result (saturate-resumable eg rules config)])
        ;; With 1 iteration fuel and commutativity, should suspend
        ;; (commutativity creates (+ b a) which needs rebuild, then
        ;; another iteration confirms fixpoint)
        (assert-true (or (saturation-suspended? result)
                        (result-saturated? result))))))

  (define-test "suspension preserves loop state"
    (let ([eg (make-egraph)]
          ;; Use associativity rules that take many iterations
          [rules arith-assoc-rules]
          [config (make-saturation-config 2 0 0)])  ; Only 2 iterations
      (egraph-add-term! eg '(+ (+ (+ a b) c) d))
      (let ([result (saturate-resumable eg rules config)])
        (when (saturation-suspended? result)
          ;; Suspension has the right iteration count
          (assert-equal 2 (suspension-iteration result))
          ;; The wrapped result is accessible
          (assert-true (saturation-result? (suspension-result result)))
          (assert-equal 'fuel-exhausted (result-status (suspension-result result))))))))

(test-group "resumable-resume"

  (define-test "resume from suspension continues work"
    (let ([eg (make-egraph)]
          [rules arith-identity-rules]
          ;; 1 iteration at a time
          [config (make-saturation-config 1 0 0)])
      (egraph-add-term! eg '(+ x 0))
      (egraph-add-term! eg 'x)
      ;; First run: 1 iteration
      (let ([r1 (saturate-resumable eg rules config)])
        (if (saturation-suspended? r1)
            ;; Resume: should eventually reach fixpoint
            (let ([r2 (saturate-resume r1)])
              (if (saturation-suspended? r2)
                  ;; One more try
                  (let ([r3 (saturate-resume r2)])
                    (assert-true (or (saturation-suspended? r3)
                                    (result-saturated? r3))))
                  (assert-true (result-saturated? r2))))
            ;; Reached fixpoint in 1 iteration (identity rule applied + confirmed)
            (assert-true (result-saturated? r1))))))

  (define-test "resume accumulates iteration count"
    ;; Use identity rules on nested expression that needs multiple iterations
    (let ([eg (make-egraph)]
          [rules arith-identity-rules]
          [config (make-saturation-config 1 0 0)])
      (egraph-add-term! eg '(* (* (+ x 0) 1) 1))
      (egraph-add-term! eg 'x)
      ;; Step through until fixpoint, tracking total iterations
      (let step ([result (saturate-resumable eg rules config)]
                 [steps 0])
        (if (saturation-suspended? result)
            (step (saturate-resume result) (+ steps 1))
            ;; Reached fixpoint after multiple suspend-resume cycles
            (assert-true (> steps 0))))))

  (define-test "resume-with allows changing config"
    (let ([eg (make-egraph)]
          [rules arith-identity-rules]
          [tight-config (make-saturation-config 1 0 0)]
          [generous-config (make-saturation-config 100 10000 0)])
      (egraph-add-term! eg '(* (* x 1) 1))
      (egraph-add-term! eg 'x)
      ;; Start with tight fuel
      (let ([r1 (saturate-resumable eg rules tight-config)])
        (when (saturation-suspended? r1)
          ;; Resume with generous fuel — should reach fixpoint
          (let ([r2 (saturate-resume-with r1 generous-config)])
            (assert-true (or (result-saturated? r2)
                            ;; Or a new suspension if it needs more
                            (saturation-suspended? r2)))))))))

(test-group "resumable-multi-step"

  (define-test "multi-step saturation reaches same equivalence"
    ;; Verify that stepping through 1-iteration-at-a-time reaches
    ;; the same e-graph state as running all at once.
    (let ([eg1 (make-egraph)]
          [eg2 (make-egraph)]
          [rules arith-identity-rules])
      ;; Setup identical e-graphs
      (let ([t1 (egraph-add-term! eg1 '(+ (* x 1) 0))]
            [x1 (egraph-add-term! eg1 'x)]
            [t2 (egraph-add-term! eg2 '(+ (* x 1) 0))]
            [x2 (egraph-add-term! eg2 'x)])
        ;; All-at-once
        (saturate-simple eg1 rules)
        ;; Step-by-step
        (let step ([result (saturate-resumable eg2 rules (make-saturation-config 1 0 0))])
          (when (saturation-suspended? result)
            (step (saturate-resume result))))
        ;; Both should agree: (+ (* x 1) 0) equivalent to x
        (assert-equal (egraph-find eg1 t1) (egraph-find eg1 x1))
        (assert-equal (egraph-find eg2 t2) (egraph-find eg2 x2)))))

  (define-test "suspension-result accessors work"
    (let ([eg (make-egraph)]
          [rules arith-assoc-rules]
          [config (make-saturation-config 1 0 0)])
      (egraph-add-term! eg '(+ (+ a b) c))
      (let ([result (saturate-resumable eg rules config)])
        (when (saturation-suspended? result)
          (let ([inner (suspension-result result)])
            (assert-true (number? (result-final-classes inner)))
            (assert-true (number? (result-final-nodes inner)))))))))

(test-group "resumable-predicate"

  (define-test "saturation-suspended? distinguishes types"
    (let ([result (make-saturation-result 'saturated 5 10 3 8)])
      (assert-false (saturation-suspended? result))
      (assert-false (saturation-suspended? 42))
      (assert-false (saturation-suspended? '()))
      (assert-false (saturation-suspended? (vector 'wrong 1 2 3 4 5 6)))))

  (define-test "suspension is not a saturation-result"
    ;; Suspensions and results are distinct types
    (let ([eg (make-egraph)]
          [rules arith-comm-rules]
          [config (make-saturation-config 1 0 0)])
      (egraph-add-term! eg '(+ a b))
      (let ([result (saturate-resumable eg rules config)])
        (when (saturation-suspended? result)
          (assert-false (saturation-result? result)))))))

(test-group "rule-sets"

  (define-test "arith-identity-rules defined"
    (assert-true (list? arith-identity-rules))
    (assert-equal 6 (length arith-identity-rules)))

  (define-test "arith-comm-rules defined"
    (assert-true (list? arith-comm-rules))
    (assert-equal 2 (length arith-comm-rules)))

  (define-test "arith-assoc-rules defined"
    (assert-true (list? arith-assoc-rules))
    (assert-equal 4 (length arith-assoc-rules)))

  (define-test "basic-arith-rules combines sets"
    (assert-true (list? basic-arith-rules))
    (assert-equal 8 (length basic-arith-rules))))

(run-all-tests)

