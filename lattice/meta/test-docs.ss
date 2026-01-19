;;; lattice/meta/test-docs.ss — Tests for Doc Form Extraction
;;;
;;; Run with: scheme --script lattice/meta/test-docs.ss

(load "core/testing/test-framework.ss")

;;; Suppress module load messages during tests
(define *meta-quiet* #t)

(load "lattice/meta/docs.ss")

(display "\n====\n")
(display "Doc Extraction Tests\n")
(display "====\n\n")

;;; ====
;;; doc-form? Tests
;;; ====

(test-group doc-form-detection

  (define-test test-doc-form-basic
    (assert-true (doc-form? '(doc 'type Int))))

  (define-test test-doc-form-targeted
    (assert-true (doc-form? '(doc add 'type (-> Int Int Int)))))

  (define-test test-doc-form-empty
    (assert-false (doc-form? '(doc))))

  (define-test test-non-doc-forms
    (assert-false (doc-form? '(define x 1)))
    (assert-false (doc-form? '(+ 1 2)))
    (assert-false (doc-form? 'symbol))
    (assert-false (doc-form? 42)))

  )

;;; ====
;;; parse-doc-form Tests
;;; ====

(test-group parse-doc-form-tests

  (define-test test-parse-contextual
    ;; (doc 'tag content...) -> (tag content #f)
    (let ([parsed (parse-doc-form '(doc 'type Int))])
      (assert-true (list? parsed))
      (assert-equal 'type (car parsed))
      (assert-equal '(Int) (cadr parsed))
      (assert-false (caddr parsed))))  ; no target

  (define-test test-parse-contextual-multi-content
    (let ([parsed (parse-doc-form '(doc 'param x "An integer"))])
      (assert-equal 'param (car parsed))
      (assert-equal '(x "An integer") (cadr parsed))
      (assert-false (caddr parsed))))

  (define-test test-parse-targeted
    ;; (doc target 'tag content...) -> (tag content target)
    (let ([parsed (parse-doc-form '(doc factorial 'type (-> Int Int)))])
      (assert-true (list? parsed))
      (assert-equal 'type (car parsed))
      (assert-equal '((-> Int Int)) (cadr parsed))
      (assert-equal 'factorial (caddr parsed))))

  (define-test test-parse-targeted-no-content
    (let ([parsed (parse-doc-form '(doc deprecated-fn 'deprecated))])
      (assert-equal 'deprecated (car parsed))
      (assert-equal '() (cadr parsed))
      (assert-equal 'deprecated-fn (caddr parsed))))

  (define-test test-parse-empty-returns-false
    (assert-false (parse-doc-form '(doc))))

  (define-test test-parse-non-doc-returns-false
    (assert-false (parse-doc-form '(define x 1))))

  )

;;; ====
;;; extract-docs-from-sexp Tests
;;; ====

(test-group extract-docs-tests

  (define-test test-extract-simple
    (let ([docs (extract-docs-from-sexp '(doc 'todo "Fix this") 10)])
      (assert-equal 1 (length docs))
      (let ([doc (car docs)])
        (assert-equal 10 (car doc))   ; line
        (assert-equal 'todo (cadr doc)))))  ; tag

  (define-test test-extract-nested
    ;; Doc form nested in a define
    (let ([docs (extract-docs-from-sexp
                  '(define (add x y)
                     (doc 'type (-> Int Int Int))
                     (+ x y))
                  5)])
      (assert-equal 1 (length docs))))

  (define-test test-extract-multiple
    ;; Multiple doc forms in one expression
    (let ([docs (extract-docs-from-sexp
                  '(begin
                     (doc 'description "Math utilities")
                     (doc 'since "1.0")
                     (define x 1))
                  1)])
      (assert-equal 2 (length docs))))

  (define-test test-extract-non-doc
    (let ([docs (extract-docs-from-sexp '(+ 1 2 3) 1)])
      (assert-equal 0 (length docs))))

  (define-test test-extract-atom
    (let ([docs (extract-docs-from-sexp 'symbol 1)])
      (assert-equal 0 (length docs))))

  )

;;; ====
;;; Initialization Flag Tests
;;; ====

(test-group init-flag-tests

  (define-test test-flag-initially-false
    ;; Reset state
    (set! *doc-index* '())
    (set! *doc-index-built?* #f)
    (assert-false *doc-index-built?*))

  (define-test test-flag-set-after-build
    (set! *doc-index* '())
    (set! *doc-index-built?* #f)
    (build-doc-index!)
    (assert-true *doc-index-built?*))

  (define-test test-ensure-doesnt-rebuild
    ;; Build once
    (set! *doc-index* '())
    (set! *doc-index-built?* #f)
    (build-doc-index!)
    (let ([count1 (length *doc-index*)])
      ;; Clear index but keep flag
      (set! *doc-index* '())
      ;; ensure-doc-index! should NOT rebuild because flag is set
      (ensure-doc-index!)
      ;; Index should still be empty
      (assert-equal 0 (length *doc-index*))))

  (define-test test-reindex-resets-flag
    (set! *doc-index-built?* #t)
    (doc-reindex!)
    ;; Flag should still be true (was reset then set again)
    (assert-true *doc-index-built?*))

  )

;;; ====
;;; Search Function Tests
;;; ====

(test-group search-tests

  ;; Ensure we have an index for search tests
  (define-test test-lf-docs-returns-list
    (doc-reindex!)
    (let ([todos (lf-docs 'todo)])
      (assert-true (list? todos))))

  (define-test test-lf-docs-nonexistent-tag
    (let ([results (lf-docs 'nonexistent-tag-xyz)])
      (assert-equal '() results)))

  (define-test test-docs-for-returns-list
    (let ([results (docs-for 'some-symbol)])
      (assert-true (list? results))))

  (define-test test-doc-stats-returns-alist
    (let ([stats (doc-stats)])
      (assert-true (list? stats))
      ;; Each entry should be (tag . count)
      (for-each
        (lambda (entry)
          (assert-true (pair? entry))
          (assert-true (number? (cdr entry))))
        stats)))

  )

;;; ====
;;; Performance Regression Test
;;; ====

(test-group performance-tests

  ;; This test ensures we don't regress to O(N^2) behavior
  ;; by checking that build completes in reasonable time
  (define-test test-build-completes
    (set! *doc-index* '())
    (set! *doc-index-built?* #f)
    ;; Just verify it completes without hanging
    (build-doc-index!)
    (assert-true *doc-index-built?*))

  )

;;; ====
;;; Summary
;;; ====

(display "\n====\n")
(display (format "Tests: ~a run, ~a passed, ~a failed\n"
                 *tests-run* *tests-passed* *tests-failed*))
(display "====\n")

(when (> *tests-failed* 0)
  (exit 1))
