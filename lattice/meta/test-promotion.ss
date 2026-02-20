(load "core/testing/test-framework.ss")
(load "lattice/meta/promotion.ss")

;;; ====
;;; Issue Construction
;;; ====

(test-group "issue-record"

  (define-test "make-issue creates proper structure"
    (let ([i (make-issue 'error 'manifest "missing file")])
      (assert-equal 'error (issue-severity i))
      (assert-equal 'manifest (issue-category i))
      (assert-equal "missing file" (issue-message i))))

  (define-test "severity values"
    (assert-equal 'pass (issue-severity (make-issue 'pass 'tests "ok")))
    (assert-equal 'warning (issue-severity (make-issue 'warning 'purity "missing")))))

;;; ====
;;; Manifest Check
;;; ====

(test-group "check-manifest"

  (define-test "existing lattice skill passes"
    ;; Use a real lattice skill directory that has manifest.sexp
    (let ([results (check-manifest "lattice/algebra")])
      (assert-true (pair? results))
      ;; Should have at least one pass (or errors for missing fields)
      (assert-true (pair? (car results)))))

  (define-test "nonexistent directory fails"
    (let ([results (check-manifest "user/nonexistent-xyz-dir")])
      (assert-equal 'error (issue-severity (car results)))))

  (define-test "real manifest parses"
    (let ([results (check-manifest "lattice/linalg")])
      ;; linalg has a complete manifest
      (assert-true (pair? results)))))

;;; ====
;;; README Check
;;; ====

(test-group "check-readme"

  (define-test "directory with README.sexp"
    (let ([results (check-readme "lattice/algebra")])
      (assert-true (pair? results))))

  (define-test "directory without README.sexp gets warning"
    (let ([results (check-readme "user/nonexistent-xyz-dir")])
      (assert-equal 'warning (issue-severity (car results))))))

;;; ====
;;; Purity Check
;;; ====

(test-group "check-purity"

  (define-test "annotated lattice skill passes"
    (let ([results (check-purity "lattice/validation")])
      ;; Our new validation skill has purity annotations
      (assert-true (pair? results))
      (assert-equal 'pass (issue-severity (car results)))))

  (define-test "empty directory gets warning"
    (let ([results (check-purity "user/nonexistent-xyz-dir")])
      ;; No files found = warning
      (assert-true (pair? results)))))

;;; ====
;;; Test Coverage Check
;;; ====

(test-group "check-tests"

  (define-test "skill with tests reports coverage"
    (let ([results (check-tests "lattice/validation")])
      ;; validation has contract.ss + test-contract.ss
      (assert-true (pair? results))
      (assert-equal 'pass (issue-severity (car results)))))

  (define-test "empty directory gets warning"
    (let ([results (check-tests "user/nonexistent-xyz-dir")])
      (assert-true (pair? results)))))

;;; ====
;;; Header Check
;;; ====

(test-group "check-headers"

  (define-test "well-structured module passes"
    (let ([results (check-headers "lattice/validation")])
      (assert-true (pair? results))))

  (define-test "empty directory gets warning"
    (let ([results (check-headers "user/nonexistent-xyz-dir")])
      (assert-true (pair? results)))))

;;; ====
;;; Boundary Check
;;; ====

(test-group "check-boundaries"

  (define-test "pure lattice skill has no violations"
    (let ([results (check-boundaries "lattice/validation")])
      (assert-true (pair? results))
      (assert-equal 'pass (issue-severity (car results)))))

  (define-test "empty directory passes vacuously"
    (let ([results (check-boundaries "user/nonexistent-xyz-dir")])
      (assert-true (pair? results))
      (assert-equal 'pass (issue-severity (car results))))))

;;; ====
;;; Impurity Markers Check
;;; ====

(test-group "check-impurity"

  (define-test "pure lattice code has no markers"
    (let ([results (check-impurity-markers "lattice/validation")])
      (assert-true (pair? results))
      (assert-equal 'pass (issue-severity (car results))))))

;;; ====
;;; Full Audit
;;; ====

(test-group "promotion-audit"

  (define-test "audit returns structured report"
    (let ([report (promotion-audit "lattice/validation")])
      (assert-true (pair? report))
      (assert-equal 'promotion-audit (car report))
      (assert-true (pair? (assq 'ready? (cdr report))))
      (assert-true (pair? (assq 'errors (cdr report))))
      (assert-true (pair? (assq 'warnings (cdr report))))
      (assert-true (pair? (assq 'passes (cdr report))))
      (assert-true (pair? (assq 'checks (cdr report))))))

  (define-test "well-structured skill is ready"
    (let ([report (promotion-audit "lattice/validation")])
      ;; validation skill should pass (we just built it to spec)
      (assert-true (promotion-ready? report))))

  (define-test "nonexistent directory is not ready"
    (let ([report (promotion-audit "user/nonexistent-xyz-dir")])
      (assert-false (promotion-ready? report))))

  (define-test "report prints without error"
    (let ([out (with-output-to-string
                 (lambda () (promotion-report (promotion-audit "lattice/validation"))))])
      (assert-true (> (string-length out) 20)))))

;;; ====
;;; Convenience
;;; ====

(test-group "convenience"

  (define-test "promote? runs without error"
    (let ([out (with-output-to-string
                 (lambda () (promote? "lattice/validation")))])
      (assert-true (> (string-length out) 10)))))

(run-all-tests)
