;;; lattice/egraph/test-extract.ss — Tests for E-Graph Extraction
;;;
;;; Run with: scheme --script lattice/egraph/test-extract.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/egraph/extract.ss")

(display "\n====\n")
(display "E-Graph Extraction Tests\n")
(display "====\n\n")

(test-group "extraction-state"

  (define-test "make-extraction-state creates state"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg '(+ x y))
      (let ([state (make-extraction-state eg ast-size-cost)])
        (assert-true (extraction-state? state)))))

  (define-test "state contains egraph"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg 'x)
      (let ([state (make-extraction-state eg ast-size-cost)])
        (assert-equal eg (state-egraph state)))))

  (define-test "state has costs table"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg 'x)
      (let ([state (make-extraction-state eg ast-size-cost)])
        (assert-false (hamt-empty? (state-costs state)))))))

(test-group "extract-basic"

  (define-test "extract leaf"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 'x)])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal 'x term)))))

  (define-test "extract number"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 42)])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal 42 term)))))

  (define-test "extract simple application"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(neg x))])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal '(neg x) term)))))

  (define-test "extract binary operation"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(+ a b))])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal '(+ a b) term)))))

  (define-test "extract nested expression"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(* (+ a b) c))])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal '(* (+ a b) c) term))))))

(test-group "extract-with-equivalences"

  (define-test "extract chooses simpler form"
    (let ([eg (make-egraph)])
      ;; Add two equivalent expressions: x and (+ x 0)
      (let ([x-id (egraph-add-term! eg 'x)]
            [expr-id (egraph-add-term! eg '(+ x 0))])
        ;; Merge them
        (egraph-merge! eg x-id expr-id)
        (egraph-rebuild! eg)
        ;; Extract from the merged class should give x (simpler)
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state (egraph-find eg expr-id))])
          (assert-equal 'x term)))))

  (define-test "extract respects cost model"
    (let ([eg (make-egraph)])
      ;; Add (sqrt x) and (/ 1 (rsqrt x)) as equivalent
      ;; With CUDA cost model, rsqrt-based should be cheaper
      (let ([sqrt-id (egraph-add-term! eg '(sqrt x))]
            [rsqrt-id (egraph-add-term! eg '(/ 1 (rsqrt x)))])
        ;; Don't merge - different structures, different costs
        ;; With ast-size, (sqrt x) is cheaper (size 2 vs 4)
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [sqrt-term (extract state sqrt-id)])
          (assert-equal '(sqrt x) sqrt-term))))))

(test-group "extract-term"

  (define-test "extract-term on simple expression"
    (let ([eg (make-egraph)])
      (let ([term (extract-term eg '(+ a b) ast-size-cost)])
        (assert-equal '(+ a b) term))))

  (define-test "extract-term preserves structure"
    (let ([eg (make-egraph)])
      (let ([term (extract-term eg '(* (+ x y) (- a b)) ast-size-cost)])
        (assert-equal '(* (+ x y) (- a b)) term)))))

(test-group "extract-all"

  (define-test "extract-all returns all roots"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg 'a)
      (egraph-add-term! eg 'b)
      (egraph-add-term! eg '(+ a b))
      (let* ([state (make-extraction-state eg ast-size-cost)]
             [all (extract-all state)])
        ;; Should have 3 roots (a, b, (+ a b))
        (assert-equal 3 (length all)))))

  (define-test "extract-all pairs class with term"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg 'x)])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [all (extract-all state)])
          ;; Should find (0 . x) pair
          (let ([entry (find (lambda (p) (= (car p) id)) all)])
            (assert-true (pair? entry))
            (assert-equal 'x (cdr entry))))))))

(test-group "optimize"

  (define-test "optimize with identity rules"
    (let ([result (optimize '(+ x 0) arith-identity-rules ast-size-cost)])
      ;; Should simplify to x
      (assert-equal 'x result)))

  (define-test "optimize (* x 1) -> x"
    (let ([result (optimize '(* x 1) arith-identity-rules ast-size-cost)])
      (assert-equal 'x result)))

  (define-test "optimize (* x 0) -> 0"
    (let ([result (optimize '(* x 0) arith-identity-rules ast-size-cost)])
      (assert-equal '0 result)))

  (define-test "optimize nested identity"
    (let ([result (optimize '(+ (* x 1) 0) arith-identity-rules ast-size-cost)])
      ;; Should simplify to x
      (assert-equal 'x result)))

  (define-test "optimize with commutativity chooses canonical form"
    ;; Commutativity rules alone don't simplify, just add (+ b a)
    ;; Extract should return one of the equivalent forms
    (let ([result (optimize '(+ a b) arith-comm-rules ast-size-cost)])
      ;; Both (+ a b) and (+ b a) have same cost, either is valid
      (assert-true (or (equal? result '(+ a b))
                      (equal? result '(+ b a))))))

  (define-test "optimize complex expression"
    ;; (+ (+ x 0) (* y 1)) should simplify to (+ x y)
    (let ([result (optimize '(+ (+ x 0) (* y 1)) basic-arith-rules ast-size-cost)])
      ;; With commutativity, could be (+ x y) or (+ y x)
      (assert-true (or (equal? result '(+ x y))
                      (equal? result '(+ y x)))))))

(test-group "optimize-with-config"

  (define-test "optimize-with-config with low fuel"
    (let* ([config (make-saturation-config 1 10000 0)]
           [result (optimize-with-config '(+ x 0)
                                         arith-identity-rules
                                         ast-size-cost
                                         config)])
      ;; Even with 1 iteration, should simplify this
      (assert-equal 'x result))))

(test-group "compare-extractions"

  (define-test "compare-extractions with different models"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(+ x y))])
        (let ([results (compare-extractions eg id
                         (list ast-size-cost ast-depth-cost))])
          (assert-equal 2 (length results))
          ;; Both should extract (+ x y) since there's only one option
          (assert-equal '(+ x y) (cadr (car results)))
          (assert-equal '(+ x y) (cadr (cadr results))))))))

(test-group "extraction-report"

  (define-test "extraction-report returns string"
    (let ([eg (make-egraph)])
      (egraph-add-term! eg '(+ x y))
      (let* ([state (make-extraction-state eg ast-size-cost)]
             [report (extraction-report state 2)])
        (assert-true (string? report))
        (assert-true (> (string-length report) 0))))))

(test-group "extract-edge-cases"

  (define-test "extract deeply nested"
    (let ([eg (make-egraph)])
      (let ([id (egraph-add-term! eg '(f (g (h x))))])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal '(f (g (h x))) term)))))

  (define-test "extract with shared subexpressions"
    (let ([eg (make-egraph)])
      ;; (+ x x) has shared child
      (let ([id (egraph-add-term! eg '(+ x x))])
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state id)])
          (assert-equal '(+ x x) term)))))

  (define-test "extract after multiple merges"
    (let ([eg (make-egraph)])
      (let ([a (egraph-add-term! eg 'a)]
            [b (egraph-add-term! eg '(id a))]
            [c (egraph-add-term! eg '(id (id a)))])
        ;; Merge a = (id a) = (id (id a))
        (egraph-merge! eg a b)
        (egraph-merge! eg b c)
        (egraph-rebuild! eg)
        ;; Should extract simplest form: a
        (let* ([state (make-extraction-state eg ast-size-cost)]
               [term (extract state (egraph-find eg c))])
          (assert-equal 'a term))))))

(run-all-tests)

