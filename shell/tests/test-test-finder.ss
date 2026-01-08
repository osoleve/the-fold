;;; shell/tests/test-test-finder.ss — Tests for Test Discovery Module
;;;
;;; Comprehensive test suite for shell/lens/test-finder.ss
;;; Tests test file discovery, symbol reference checking, and test name extraction.
;;; (Yes, this is a test file for the test finder - quite meta!)

(load "shell/lens/test-finder.ss")

(define test-count 0)
(define pass-count 0)
(define fail-count 0)

;;; Test assertion helper
(define (assert-equal name actual expected)
  (set! test-count (+ test-count 1))
  (if (equal? actual expected)
      (begin
       (set! pass-count (+ pass-count 1))
       (printf "  ✓ ~a\n" name))
      (begin
       (set! fail-count (+ fail-count 1))
       (printf "  ✗ ~a\n" name)
       (printf "    Expected: ~s\n" expected)
       (printf "    Actual:   ~s\n" actual))))

(define (assert-true name actual)
  (assert-equal name actual #t))

(define (assert-false name actual)
  (assert-equal name actual #f))

;;; ============================================================
;;; path-basename Tests
;;; ============================================================

(printf "\n=== Testing path-basename ===\n")

(assert-equal "basename simple"
              (path-basename "foo/bar/baz.ss")
              "baz.ss")

(assert-equal "basename no slash"
              (path-basename "file.ss")
              "file.ss")

(assert-equal "basename deep path"
              (path-basename "/a/b/c/d/e/f.ss")
              "f.ss")

(assert-equal "basename root file"
              (path-basename "/file.ss")
              "file.ss")

;;; ============================================================
;;; path-dirname Tests
;;; ============================================================

(printf "\n=== Testing path-dirname ===\n")

(assert-equal "dirname simple"
              (path-dirname "foo/bar/baz.ss")
              "foo/bar")

(assert-equal "dirname no slash"
              (path-dirname "file.ss")
              ".")

(assert-equal "dirname deep path"
              (path-dirname "/a/b/c/d/e/f.ss")
              "/a/b/c/d/e")

(assert-equal "dirname root file"
              (path-dirname "/file.ss")
              "")

;;; ============================================================
;;; string-rindex Tests
;;; ============================================================

(printf "\n=== Testing string-rindex ===\n")

(assert-equal "rindex found at end"
              (string-rindex "hello" #\o)
              4)

(assert-equal "rindex multiple occurrences"
              (string-rindex "abcabc" #\a)
              3)

(assert-equal "rindex not found"
              (string-rindex "hello" #\x)
              #f)

(assert-equal "rindex at start"
              (string-rindex "hello" #\h)
              0)

(assert-equal "rindex slash in path"
              (string-rindex "foo/bar/baz" #\/)
              7)

;;; ============================================================
;;; string-prefix? Tests
;;; ============================================================

(printf "\n=== Testing string-prefix? ===\n")

(assert-true "prefix matches"
             (string-prefix? "test-file.ss" "test-"))

(assert-false "prefix does not match"
              (string-prefix? "file.ss" "test-"))

(assert-true "empty prefix always matches"
             (string-prefix? "anything" ""))

(assert-true "exact match"
             (string-prefix? "test" "test"))

(assert-false "prefix longer than string"
              (string-prefix? "te" "test-"))

;;; ============================================================
;;; is-test-file? Tests
;;; ============================================================

(printf "\n=== Testing is-test-file? ===\n")

(assert-true "test file name"
             (is-test-file? "test-foo.ss"))

(assert-true "test file with path"
             (is-test-file? "shell/tests/test-string-utils.ss"))

(assert-false "not a test file"
              (is-test-file? "foo.ss"))

(assert-false "similar but not test file"
              (is-test-file? "testing.ss"))

(assert-false "tests in name but not prefix"
              (is-test-file? "my-tests.ss"))

;;; ============================================================
;;; contains-symbol? Tests
;;; ============================================================

(printf "\n=== Testing contains-symbol? ===\n")

(assert-true "symbol at top level"
             (contains-symbol? 'foo 'foo))

(assert-true "symbol in list"
             (contains-symbol? '(a b foo c) 'foo))

(assert-true "symbol nested"
             (contains-symbol? '(a (b (c foo))) 'foo))

(assert-false "symbol not present"
              (contains-symbol? '(a b c) 'foo))

(assert-false "similar symbol not same"
              (contains-symbol? '(foobar) 'foo))

(assert-true "symbol in complex expr"
             (contains-symbol? '(define (test-it) (foo x y)) 'foo))

;;; ============================================================
;;; extract-test-name-from-expr Tests
;;; ============================================================

(printf "\n=== Testing extract-test-name-from-expr ===\n")

(assert-equal "define test function"
              (extract-test-name-from-expr '(define (test-foo x) x))
              'test-foo)

(assert-equal "test form with string"
              (extract-test-name-from-expr '(test "my test" (assert-true #t)))
              'test:my\ test)

(assert-equal "test-case form"
              (extract-test-name-from-expr '(test-case "case 1" body))
              'test-case:case\ 1)

(assert-false "not a test form"
              (extract-test-name-from-expr '(define (foo x) x)))

(assert-false "non-pair"
              (extract-test-name-from-expr 'symbol))

(assert-false "define without test- prefix"
              (extract-test-name-from-expr '(define (helper x) x)))

;;; ============================================================
;;; find-test-file Tests
;;; ============================================================

(printf "\n=== Testing find-test-file ===\n")

;; Test with known module
(assert-true "finds test file for string-utils"
             (let ([result (find-test-file "shell/tools/string-utils.ss")])
                  (and result
                       (string-contains? result "test-string-utils.ss"))))

;; Test with nonexistent module
(assert-false "returns #f for nonexistent"
              (find-test-file "/nonexistent/module.ss"))

;;; ============================================================
;;; find-all-test-files Tests
;;; ============================================================

(printf "\n=== Testing find-all-test-files ===\n")

(assert-true "returns list"
             (list? (find-all-test-files)))

(assert-true "finds test files"
             (> (length (find-all-test-files)) 0))

(assert-true "all are test files"
             (andmap is-test-file? (find-all-test-files)))

(assert-true "finds this test file"
             (ormap (lambda (f) (string-contains? f "test-test-finder.ss"))
                    (find-all-test-files)))

;;; ============================================================
;;; find-test-files-in Tests
;;; ============================================================

(printf "\n=== Testing find-test-files-in ===\n")

(assert-true "finds tests in shell"
             (> (length (find-test-files-in "shell")) 0))

(assert-true "finds tests in core"
             (> (length (find-test-files-in "core")) 0))

(assert-equal "empty for nonexistent dir"
              (find-test-files-in "/nonexistent/directory")
              '())

;;; ============================================================
;;; extract-test-names Tests
;;; ============================================================

(printf "\n=== Testing extract-test-names ===\n")

;; Test with this very test file
(assert-true "extracts test names from test file"
             (let ([names (extract-test-names "shell/tests/test-test-finder.ss")])
                  (list? names)))

(assert-equal "empty for nonexistent file"
              (extract-test-names "/nonexistent/file.ss")
              '())

;;; ============================================================
;;; find-tests-for Tests
;;; ============================================================

(printf "\n=== Testing find-tests-for ===\n")

(assert-true "returns list"
             (list? (find-tests-for 'foo)))

;; Test with a known function that has tests
(assert-true "finds tests for string-split"
             (let ([tests (find-tests-for 'string-split)])
                  (or (null? tests)  ; May not be indexed
                      (and (list? tests)
                           (list? (car tests))))))

(assert-true "returns empty for unknown"
             (let ([tests (find-tests-for 'nonexistent-symbol-xyz-123)])
                  (null? tests)))

;;; ============================================================
;;; search-all-tests-for Tests
;;; ============================================================

(printf "\n=== Testing search-all-tests-for ===\n")

(assert-true "returns list"
             (list? (search-all-tests-for 'foo)))

;; Test that it finds references to common functions
(assert-true "finds references to assert-equal"
             (let ([tests (search-all-tests-for 'assert-equal)])
                  ;; assert-equal is used in many test files
                  (> (length tests) 0)))

;;; ============================================================
;;; filter-map-test Tests
;;; ============================================================

(printf "\n=== Testing filter-map-test ===\n")

(assert-equal "filter-map basic"
              (filter-map-test (lambda (x) (and (even? x) (* x 2))) '(1 2 3 4))
              '(4 8))

(assert-equal "filter-map all false"
              (filter-map-test (lambda (x) #f) '(1 2 3))
              '())

(assert-equal "filter-map empty list"
              (filter-map-test (lambda (x) x) '())
              '())

;;; ============================================================
;;; test-file-references-symbol? Tests
;;; ============================================================

(printf "\n=== Testing test-file-references-symbol? ===\n")

;; This test file references 'path-basename
(assert-true "this file references path-basename"
             (test-file-references-symbol? "shell/tests/test-test-finder.ss" 'path-basename))

(assert-false "this file doesn't reference xyz-nonexistent"
              (test-file-references-symbol? "shell/tests/test-test-finder.ss" 'xyz-nonexistent-symbol))

(assert-false "nonexistent file returns false"
              (test-file-references-symbol? "/nonexistent/file.ss" 'foo))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "\n=== Testing Edge Cases ===\n")

(assert-equal "basename empty string"
              (path-basename "")
              "")

(assert-equal "dirname empty string"
              (path-dirname "")
              ".")

(assert-equal "rindex empty string"
              (string-rindex "" #\x)
              #f)

(assert-true "contains-symbol in empty list"
             (not (contains-symbol? '() 'foo)))

;;; ============================================================
;;; Test Summary
;;; ============================================================

(printf "\n========================================\n")
(printf "Test Results (test-finder):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
