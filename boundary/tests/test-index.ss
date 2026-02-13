;;; boundary/tests/test-index.ss — Tests for symbol index
;;; Validates @defines annotation parsing and index building.

(load "core/testing/test-framework.ss")
(load "boundary/tools/index.ss")

(test-group split-defines-names
  (define-test "basic space-separated names"
    (assert-equal (split-defines-names "foo bar baz")
                  '("foo" "bar" "baz")))

  (define-test "handles leading/trailing whitespace"
    (assert-equal (split-defines-names "  vec2-dot  vec2-mag  ")
                  '("vec2-dot" "vec2-mag")))

  (define-test "single name"
    (assert-equal (split-defines-names "vec2")
                  '("vec2")))

  (define-test "empty string returns empty list"
    (assert-equal (split-defines-names "") '()))

  (define-test "names with special characters"
    (assert-equal (split-defines-names "vec2? vec2->list list->vec2")
                  '("vec2?" "vec2->list" "list->vec2"))))

(test-group parse-defines-annotation
  (define-test "creates entries for each name"
    (let ([entries (parse-defines-annotation "test.ss" 42 ";;; @defines foo bar baz?")])
      (assert-equal (length entries) 3)))

  (define-test "entries have correct file and line"
    (let* ([entries (parse-defines-annotation "test.ss" 42 ";;; @defines foo")]
           [entry (car entries)])
      (assert-equal (cdr (assq 'file entry)) "test.ss")
      (assert-equal (cdr (assq 'line entry)) 42)))

  (define-test "entries have correct names"
    (let ([entries (parse-defines-annotation "test.ss" 1 ";;; @defines vec2 vec2?")])
      (assert-equal (cdr (assq 'name (car entries))) 'vec2)
      (assert-equal (cdr (assq 'name (cadr entries))) 'vec2?)))

  (define-test "entries are tagged as define kind"
    (let* ([entries (parse-defines-annotation "test.ss" 1 ";;; @defines foo")]
           [entry (car entries)])
      (assert-equal (cdr (assq 'kind entry)) 'define))))

(test-group index-extract-definitions-with-annotations
  (define-test "@defines lines produce entries"
    (let ([results (index-extract-definitions "test.ss"
                     '(";;; @defines alpha beta"
                       "(generate-something alpha beta)"))])
      (assert-equal (length results) 2)
      (assert-equal (cdr (assq 'name (car results))) 'alpha)
      (assert-equal (cdr (assq 'name (cadr results))) 'beta)))

  (define-test "@defines mixed with regular defines"
    (let ([results (index-extract-definitions "test.ss"
                     '("(define (regular-fn x) x)"
                       ";;; @defines macro-gen"
                       "(generate-foo macro-gen)"))])
      (assert-equal (length results) 2)
      (assert-equal (cdr (assq 'name (car results))) 'regular-fn)
      (assert-equal (cdr (assq 'name (cadr results))) 'macro-gen)))

  (define-test "@defines clears comment accumulator"
    (let ([results (index-extract-definitions "test.ss"
                     '(";;; some docstring"
                       ";;; @defines alpha"
                       "(generate-alpha alpha)"))])
      ;; Should get 1 entry (alpha), not confuse the docstring
      (assert-equal (length results) 1)
      (assert-equal (cdr (assq 'name (car results))) 'alpha))))

(test-group index-lookup-macro-generated
  (define-test "vec2-dot found after index-refresh"
    (index-refresh!)
    (let ([entry (index-lookup 'vec2-dot)])
      (assert-true (pair? entry))
      (assert-true (string-contains? (cdr (assq 'file entry)) "vec2.ss"))))

  (define-test "vec3-rotate-axis found after index-refresh"
    (let ([entry (index-lookup 'vec3-rotate-axis)])
      (assert-true (pair? entry))
      (assert-true (string-contains? (cdr (assq 'file entry)) "vec3.ss"))))

  (define-test "vec2-lerp found after index-refresh"
    (let ([entry (index-lookup 'vec2-lerp)])
      (assert-true (pair? entry))
      (assert-true (string-contains? (cdr (assq 'file entry)) "vec2.ss")))))

(run-all-tests)
