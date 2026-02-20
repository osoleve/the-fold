(load "core/testing/test-framework.ss")
(load "lattice/meta/readme-audit.ss")

;;; ====
;;; Field Extraction
;;; ====

(test-group "readme-field-extraction"

  (define-test "flat-list field"
    (let ([entries '((name "base") (purpose "Foundation"))])
      (assert-equal "base" (readme-field entries 'name))
      (assert-equal "Foundation" (readme-field entries 'purpose))))

  (define-test "cons-pair field"
    (let ([entries '((name . "repl") (purpose . "REPL infra"))])
      (assert-equal "repl" (readme-field entries 'name))
      (assert-equal "REPL infra" (readme-field entries 'purpose))))

  (define-test "missing field returns #f"
    (let ([entries '((name "base"))])
      (assert-false (readme-field entries 'purpose))))

  (define-test "nested list value"
    (let ([entries '((name "test") (modules ((a.ss "desc1") (b.ss "desc2"))))])
      (assert-true (pair? (readme-field entries 'modules)))))

  (define-test "symbol value"
    (let ([entries '((name "test") (layer lattice))])
      (assert-equal 'lattice (readme-field entries 'layer))))
)

;;; ====
;;; Style Detection
;;; ====

(test-group "readme-style-detection"

  (define-test "flat-list style"
    (let ([entries '((name "x") (purpose "y") (layer lattice))])
      (assert-equal 'flat-list (readme-detect-style entries))))

  (define-test "cons-pair style"
    (let ([entries '((name . "x") (purpose . "y") (tier-access . boundary))])
      (assert-equal 'cons-pair (readme-detect-style entries))))

  (define-test "mixed style"
    (let ([entries '((name "flat-value") (purpose . "cons-value"))])
      (assert-equal 'mixed (readme-detect-style entries))))

  (define-test "empty style"
    (assert-equal 'empty (readme-detect-style '())))
)

;;; ====
;;; Validation
;;; ====

(test-group "readme-validation"

  (define-test "valid minimal"
    (let* ([entries '((name "test") (purpose "Testing"))]
           [issues (readme-validate entries)])
      (assert-true (readme-valid? issues))
      (assert-equal 0 (readme-issue-count issues 'error))))

  (define-test "valid full"
    (let* ([entries '((name "test") (purpose "Testing")
                      (description "A test module")
                      (modules ((a.ss "A")))
                      (layer lattice))]
           [issues (readme-validate entries)])
      (assert-true (readme-valid? issues))
      (assert-equal 0 (readme-issue-count issues 'error))
      (assert-equal 0 (readme-issue-count issues 'warning))))

  (define-test "missing required field is error"
    (let* ([entries '((purpose "Testing"))]
           [issues (readme-validate entries)])
      (assert-false (readme-valid? issues))
      (assert-true (> (readme-issue-count issues 'error) 0))))

  (define-test "missing recommended field is warning"
    (let* ([entries '((name "test") (purpose "Testing"))]
           [issues (readme-validate entries)])
      (assert-true (> (readme-issue-count issues 'warning) 0))))

  (define-test "cons-pair style triggers warning"
    (let* ([entries '((name . "test") (purpose . "Testing"))]
           [issues (readme-validate entries)])
      ;; cons-pair is a warning, not error
      (assert-true (> (readme-issue-count issues 'warning) 0))))

  (define-test "unknown field is info"
    (let* ([entries '((name "test") (purpose "Testing") (foobar "baz"))]
           [issues (readme-validate entries)])
      (assert-true (> (readme-issue-count issues 'info) 0))))
)

;;; ====
;;; Audit Report
;;; ====

(test-group "readme-audit-one"

  (define-test "produces valid report"
    (let* ([entries '((name "base") (purpose "Foundation")
                      (description "Core base modules")
                      (modules ((prelude.ss "Utils")))
                      (layer core))]
           [report (readme-audit-one "core/base" entries)])
      (assert-true (readme-audit? report))
      (assert-equal "core/base" (readme-audit-field report 'path))
      (assert-equal "base" (readme-audit-field report 'name))
      (assert-equal 'flat-list (readme-audit-field report 'style))
      (assert-equal 0 (readme-audit-field report 'errors))
      (assert-true (readme-audit-field report 'valid))))

  (define-test "cons-pair style detected"
    (let* ([entries '((name . "repl") (purpose . "REPL"))]
           [report (readme-audit-one "boundary/repl" entries)])
      (assert-equal 'cons-pair (readme-audit-field report 'style))
      (assert-true (> (readme-audit-field report 'warnings) 0))))

  (define-test "incomplete file flagged"
    (let* ([entries '((purpose "Just purpose"))]
           [report (readme-audit-one "some/dir" entries)])
      (assert-false (readme-audit-field report 'valid))
      (assert-true (> (readme-audit-field report 'errors) 0))))
)

;;; ====
;;; Batch Summary
;;; ====

(test-group "readme-audit-summary"

  (define-test "summary aggregates correctly"
    (let* ([r1 (readme-audit-one "a" '((name "a") (purpose "A")
                                        (description "A") (modules ()) (layer lattice)))]
           [r2 (readme-audit-one "b" '((name . "b") (purpose . "B")))]
           [r3 (readme-audit-one "c" '((purpose "Only purpose")))]
           [summary (readme-audit-summary (list r1 r2 r3))]
           [field (lambda (k)
                    (let loop ([fields (cdr summary)])
                      (cond [(null? fields) #f]
                            [(eq? (caar fields) k) (cadar fields)]
                            [else (loop (cdr fields))])))])
      (assert-equal 3 (field 'total))
      (assert-equal 2 (field 'valid))     ; r1 and r2 are valid (no errors)
      (assert-equal 1 (field 'invalid))   ; r3 missing name
      (assert-equal 2 (field 'flat-list-style))  ; r1 + r3 (r3 has flat-list style despite missing name)
      (assert-equal 1 (field 'cons-pair-style))))
)

(run-all-tests)
