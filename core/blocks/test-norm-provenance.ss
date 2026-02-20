(load "core/testing/test-framework.ss")
(load "core/blocks/norm-provenance.ss")

;;; ====
;;; Phase Records
;;; ====

(test-group "phase-records"

  (define-test "create phase record"
    (let ([p (make-phase-record 'alpha '(fn (x) x) '(fn (dv 0)) '(de-bruijn-index))])
      (assert-true (phase-record? p))
      (assert-equal 'alpha (phase-field p 'name))
      (assert-true (phase-field p 'changed?))
      (assert-equal '(de-bruijn-index) (phase-field p 'rules))))

  (define-test "unchanged phase"
    (let ([p (make-phase-record 'eta-reduce '(+ a b) '(+ a b) '())])
      (assert-false (phase-field p 'changed?))
      (assert-equal '() (phase-field p 'rules))))

  (define-test "phase-field returns #f for missing key"
    (let ([p (make-phase-record 'test 'a 'b '())])
      (assert-false (phase-field p 'nonexistent))))
)

;;; ====
;;; V1 Provenance
;;; ====

(test-group "v1-provenance"

  (define-test "identity function trace"
    (let ([trace (normalize-v1-provenance '(fn (x) x))])
      (assert-true (norm-trace? trace))
      (assert-equal 'v1 (norm-trace-field trace 'version))
      (assert-equal '(fn (x) x) (norm-trace-field trace 'input))
      (assert-equal '(fn (dv 0)) (norm-trace-field trace 'output))
      (assert-true (norm-trace-field trace 'changed?))
      (assert-equal 2 (norm-trace-field trace 'phase-count))))

  (define-test "already normalized expression"
    (let ([trace (normalize-v1-provenance 42)])
      (assert-false (norm-trace-field trace 'changed?))
      (assert-equal 0 (norm-trace-field trace 'active-phases))))

  (define-test "commutative sort detected"
    (let* ([trace (normalize-v1-provenance '(+ b a))]
           [phases (norm-trace-field trace 'phases)]
           [alg-phase (car phases)])
      (assert-true (phase-field alg-phase 'changed?))
      (assert-true (pair? (memq 'commutative-sort (phase-field alg-phase 'rules))))))

  (define-test "alpha rename detected"
    (let* ([trace (normalize-v1-provenance '(fn (x) x))]
           [phases (norm-trace-field trace 'phases)]
           [alpha-phase (cadr phases)])
      (assert-true (phase-field alpha-phase 'changed?))
      (assert-true (pair? (memq 'de-bruijn-index (phase-field alpha-phase 'rules))))))

  (define-test "associative flatten detected"
    (let* ([trace (normalize-v1-provenance '(+ (+ a b) c))]
           [phases (norm-trace-field trace 'phases)]
           [alg-phase (car phases)])
      (assert-true (phase-field alg-phase 'changed?))
      (assert-true (pair? (memq 'associative-flatten (phase-field alg-phase 'rules))))))
)

;;; ====
;;; V2 Provenance
;;; ====

(test-group "v2-provenance"

  (define-test "v2 has 6 phases"
    (let ([trace (normalize-v2-provenance '(fn (x) (f x)))])
      (assert-equal 'v2 (norm-trace-field trace 'version))
      (assert-equal 6 (norm-trace-field trace 'phase-count))))

  (define-test "eta reduction captured"
    (let* ([trace (normalize-v2-provenance '(fn (x) (f x)))]
           [phases (norm-trace-field trace 'phases)]
           [eta-phase (car phases)])
      (assert-equal 'eta-reduce (phase-field eta-phase 'name))
      (assert-true (phase-field eta-phase 'changed?))
      (assert-true (pair? (memq 'eta-reduce (phase-field eta-phase 'rules))))))

  (define-test "identity elimination phase present"
    ;; Note: (+ x 0) is handled by poly-canon before identity-elim runs.
    ;; We verify the phase structure is correct and the output is right.
    (let* ([trace (normalize-v2-provenance '(+ x 0))]
           [phases (norm-trace-field trace 'phases)]
           [ident-phase (list-ref phases 3)])
      (assert-equal 'identity-elim (phase-field ident-phase 'name))
      ;; The overall trace should show the expression was simplified
      (assert-true (norm-trace-field trace 'changed?))))

  (define-test "identity elimination fires on non-polynomial"
    ;; (* 1 (fn (x) x)) — fn is not an arithmetic leaf, so poly-canon skips it.
    ;; But identity-elim sees (* ... 1 ...) and removes the identity element.
    (let* ([trace (normalize-v2-provenance '(* 1 (fn (x) x)))]
           [phases (norm-trace-field trace 'phases)]
           [ident-phase (list-ref phases 3)])
      (assert-equal 'identity-elim (phase-field ident-phase 'name))
      (assert-true (phase-field ident-phase 'changed?))))

  (define-test "no-op expression has zero active phases"
    (let ([trace (normalize-v2-provenance 42)])
      (assert-equal 0 (norm-trace-field trace 'active-phases))))
)

;;; ====
;;; V3 Provenance
;;; ====

(test-group "v3-provenance"

  (define-test "v3 has 7 phases"
    (let ([trace (normalize-v3-provenance '(+ a b))])
      (assert-equal 'v3 (norm-trace-field trace 'version))
      (assert-equal 7 (norm-trace-field trace 'phase-count))))

  (define-test "nbe beta reduction captured"
    (let* ([trace (normalize-v3-provenance '((fn (x) x) y))]
           [phases (norm-trace-field trace 'phases)]
           [nbe-phase (car phases)])
      (assert-equal 'nbe (phase-field nbe-phase 'name))
      (assert-true (phase-field nbe-phase 'changed?))
      (assert-true (pair? (memq 'beta-reduce (phase-field nbe-phase 'rules))))))

  (define-test "nbe pair projection captured"
    (let* ([trace (normalize-v3-provenance '(fst (pair a b)))]
           [phases (norm-trace-field trace 'phases)]
           [nbe-phase (car phases)])
      (assert-true (phase-field nbe-phase 'changed?))
      (assert-true (pair? (memq 'pair-project (phase-field nbe-phase 'rules))))))
)

;;; ====
;;; Version Dispatch
;;; ====

(test-group "version-dispatch"

  (define-test "normalize-with-provenance v1"
    (let ([trace (normalize-with-provenance '(+ b a) 'v1)])
      (assert-equal 'v1 (norm-trace-field trace 'version))))

  (define-test "normalize-with-provenance v2"
    (let ([trace (normalize-with-provenance '(+ b a) 'v2)])
      (assert-equal 'v2 (norm-trace-field trace 'version))))

  (define-test "normalize-with-provenance v3"
    (let ([trace (normalize-with-provenance '(+ b a) 'v3)])
      (assert-equal 'v3 (norm-trace-field trace 'version))))
)

;;; ====
;;; Trace Analysis
;;; ====

(test-group "trace-analysis"

  (define-test "trace-all-rules collects across phases"
    (let* ([trace (normalize-v1-provenance '(+ b a))]
           [rules (trace-all-rules trace)])
      (assert-true (pair? rules))))

  (define-test "trace-active-phases filters unchanged"
    (let* ([trace (normalize-v1-provenance 42)]
           [active (trace-active-phases trace)])
      (assert-equal 0 (length active))))

  (define-test "trace-active-phases includes changed"
    (let* ([trace (normalize-v1-provenance '(fn (x) (+ b x a)))]
           [active (trace-active-phases trace)])
      (assert-true (> (length active) 0))))

  (define-test "trace-summary structure"
    (let* ([trace (normalize-v1-provenance '(+ b a))]
           [summary (trace-summary trace)])
      (assert-true (pair? summary))
      (assert-equal 'norm-summary (car summary))))
)

;;; ====
;;; Trace Comparison
;;; ====

(test-group "trace-comparison"

  (define-test "same expression same output"
    (let* ([t1 (normalize-v1-provenance '(+ a b))]
           [t2 (normalize-v1-provenance '(+ b a))]
           [diff (trace-compare t1 t2)])
      ;; Different inputs, same output (commutative)
      (assert-false (cadr (assq 'same-input? (cdr diff))))
      (assert-true (cadr (assq 'same-output? (cdr diff))))))

  (define-test "different expressions different output"
    (let* ([t1 (normalize-v1-provenance '(+ a b))]
           [t2 (normalize-v1-provenance '(* a b))]
           [diff (trace-compare t1 t2)])
      (assert-false (cadr (assq 'same-output? (cdr diff))))))

  (define-test "divergence point detected"
    (let* ([t1 (normalize-v1-provenance '(+ a b))]
           [t2 (normalize-v1-provenance '(* a b))]
           [diff (trace-compare t1 t2)]
           [divergence (cadr (assq 'divergence (cdr diff)))])
      ;; Should diverge at the algebraic phase
      (assert-true (pair? divergence))))
)

(run-all-tests)
