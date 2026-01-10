;;; shell/tests/test-complexity.ss — Tests for Complexity Analysis
;;;
;;; Comprehensive test suite for shell/introspect/complexity.ss
;;; Tests file analysis, complexity scoring, coverage, and dependency graphs.

(load "shell/introspect/complexity.ss")

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
;;; line-type Tests
;;; ============================================================

(printf "\n=== Testing line-type ===\n")

(assert-equal "blank line"
              (line-type "")
              'blank)

(assert-equal "whitespace only is blank"
              (line-type "   ")
              'blank)

(assert-equal "comment line"
              (line-type ";;; This is a comment")
              'comment)

(assert-equal "comment with leading space"
              (line-type "  ; comment")
              'comment)

(assert-equal "code line"
              (line-type "(define foo 42)")
              'code)

(assert-equal "code with leading whitespace"
              (line-type "  (define foo 42)")
              'code)

;;; ============================================================
;;; string-count-char Tests
;;; ============================================================

(printf "\n=== Testing string-count-char ===\n")

(assert-equal "count single occurrence"
              (string-count-char "hello" #\l)
              2)

(assert-equal "count no occurrences"
              (string-count-char "hello" #\x)
              0)

(assert-equal "count all same"
              (string-count-char "aaaa" #\a)
              4)

(assert-equal "count empty string"
              (string-count-char "" #\a)
              0)

(assert-equal "count parens"
              (string-count-char "((()))" #\()
              3)

;;; ============================================================
;;; sexp-size Tests
;;; ============================================================

(printf "\n=== Testing sexp-size ===\n")

(assert-equal "size of atom"
              (sexp-size 'foo)
              1)

(assert-equal "size of number"
              (sexp-size 42)
              1)

(assert-equal "size of simple list"
              (sexp-size '(a b c))
              7)  ; 3 pairs + 3 atoms + nil

(assert-equal "size of nested list"
              (sexp-size '((a)))
              5)  ; pair(pair(a . nil) . nil)

(assert-equal "size of empty list"
              (sexp-size '())
              0)

;;; ============================================================
;;; sexp-depth Tests
;;; ============================================================

(printf "\n=== Testing sexp-depth ===\n")

(assert-equal "depth of atom"
              (sexp-depth 'foo)
              0)

(assert-equal "depth of flat list"
              (sexp-depth '(a b c))
              2)

(assert-equal "depth of nested list"
              (sexp-depth '((a)))
              3)

(assert-equal "depth of empty list"
              (sexp-depth '())
              0)

(assert-equal "depth of deeply nested"
              (sexp-depth '((((a)))))
              5)

;;; ============================================================
;;; extract-references Tests
;;; ============================================================

(printf "\n=== Testing extract-references ===\n")

(assert-equal "refs from atom"
              (extract-references 'foo)
              '(foo))

(assert-true "refs from list"
             (let ([refs (extract-references '(foo bar baz))])
                  (and (member 'foo refs)
                       (member 'bar refs)
                       (member 'baz refs))))

(assert-true "refs from nested"
             (let ([refs (extract-references '(define (f x) (g x)))])
                  (and (member 'define refs)
                       (member 'f refs)
                       (member 'g refs)
                       (member 'x refs))))

(assert-equal "refs from empty list"
              (extract-references '())
              '())

;;; ============================================================
;;; definition? Tests
;;; ============================================================

(printf "\n=== Testing definition? ===\n")

(assert-true "define is definition"
             (definition? '(define foo 42)))

(assert-true "define-record-type is definition"
             (definition? '(define-record-type point (fields x y))))

(assert-true "define-syntax is definition"
             (definition? '(define-syntax mac (syntax-rules () ()))))

(assert-false "if is not definition"
              (definition? '(if a b c)))

(assert-false "let is not definition"
              (definition? '(let ([x 1]) x)))

(assert-false "atom is not definition"
              (definition? 'foo))

;;; ============================================================
;;; extract-definition-name Tests
;;; ============================================================

(printf "\n=== Testing extract-definition-name ===\n")

(assert-equal "simple define"
              (extract-definition-name '(define foo 42))
              'foo)

(assert-equal "function define"
              (extract-definition-name '(define (bar x) x))
              'bar)

(assert-equal "define-record-type"
              (extract-definition-name '(define-record-type point (fields x y)))
              'point)

(assert-equal "define-syntax"
              (extract-definition-name '(define-syntax mac body))
              'mac)

(assert-false "non-definition"
              (extract-definition-name '(if a b c)))

;;; ============================================================
;;; load-form? Tests
;;; ============================================================

(printf "\n=== Testing load-form? ===\n")

(assert-true "load with string path"
             (load-form? '(load "core/base/prelude.ss")))

(assert-false "load without string"
              (load-form? '(load path)))

(assert-false "not a load form"
              (load-form? '(define foo 42)))

;;; ============================================================
;;; extract-load-path Tests
;;; ============================================================

(printf "\n=== Testing extract-load-path ===\n")

(assert-equal "extract path from load"
              (extract-load-path '(load "core/base/prelude.ss"))
              "core/base/prelude.ss")

(assert-false "non-load returns #f"
              (extract-load-path '(define foo 42)))

;;; ============================================================
;;; scheme-file? Tests
;;; ============================================================

(printf "\n=== Testing scheme-file? ===\n")

(assert-true "scheme file"
             (scheme-file? "foo.ss"))

(assert-true "scheme file with path"
             (scheme-file? "core/base/prelude.ss"))

(assert-false "not scheme file"
              (scheme-file? "foo.txt"))

(assert-false "short name"
              (scheme-file? "ab"))

;;; ============================================================
;;; count-paren-delta Tests
;;; ============================================================

(printf "\n=== Testing count-paren-delta ===\n")

(assert-equal "balanced parens"
              (count-paren-delta "(foo)")
              0)

(assert-equal "opening parens"
              (count-paren-delta "(define (foo")
              2)

(assert-equal "closing parens"
              (count-paren-delta "  x))))")
              -4)

(assert-equal "ignores parens in comment"
              (count-paren-delta "(foo ; ))) comment")
              1)

(assert-equal "ignores parens in string"
              (count-paren-delta "(foo \")))\" bar)")
              0)

(assert-equal "handles escaped quotes in string"
              (count-paren-delta "(foo \"a\\\"b\" baz)")
              0)

;;; ============================================================
;;; analyze-file Tests
;;; ============================================================

(printf "\n=== Testing analyze-file ===\n")

;; Test with a known file
(let ([metrics (analyze-file "shell/introspect/complexity.ss")])
     (assert-true "returns file-metrics"
                  (file-metrics? metrics))
     
     (assert-equal "path matches"
                   (file-metrics-path metrics)
                   "shell/introspect/complexity.ss")
     
     (assert-true "has positive lines"
                  (> (file-metrics-total-lines metrics) 0))
     
     (assert-true "has definitions"
                  (> (length (file-metrics-definitions metrics)) 0))
     
     (assert-true "has load deps"
                  (list? (file-metrics-load-deps metrics))))

;;; ============================================================
;;; complexity-score Tests
;;; ============================================================

(printf "\n=== Testing complexity-score ===\n")

(let ([simple-def (make-definition-info 'foo 'define 1 5 2 '(bar))])
     (assert-true "complexity score is positive"
                  (> (complexity-score simple-def) 0))
     
     (assert-equal "score formula"
                   (complexity-score simple-def)
                   (+ (* 5 1)    ; size * 1
                      (* 2 5)    ; depth * 5
                      (* 1 2)))) ; refs * 2

;;; ============================================================
;;; high-complexity? Tests
;;; ============================================================

(printf "\n=== Testing high-complexity? ===\n")

(let ([simple-def (make-definition-info 'foo 'define 1 5 2 '(bar))]
      [complex-def (make-definition-info 'bar 'define 1 100 10 '(a b c d e))])
     
     (assert-false "simple def below threshold"
                   (high-complexity? simple-def 50))
     
     (assert-true "complex def above threshold"
                  (high-complexity? complex-def 50)))

;;; ============================================================
;;; filter-map Tests
;;; ============================================================

(printf "\n=== Testing filter-map ===\n")

(assert-equal "filter-map basic"
              (filter-map (lambda (x) (and (even? x) (* x 2))) '(1 2 3 4))
              '(4 8))

(assert-equal "filter-map all false"
              (filter-map (lambda (x) #f) '(1 2 3))
              '())

(assert-equal "filter-map empty list"
              (filter-map (lambda (x) x) '())
              '())

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(printf "\n=== Testing Edge Cases ===\n")

(assert-equal "line-type tabs only"
              (line-type "\t\t")
              'blank)

(assert-equal "sexp-size string"
              (sexp-size "hello")
              1)

(assert-true "analyze-file with nonexistent fails gracefully"
             (guard (e [else #t])
                    (analyze-file "/nonexistent/file.ss")
                    #f))

;;; ============================================================
;;; Test Summary
;;; ============================================================

(printf "\n========================================\n")
(printf "Test Results (complexity):\n")
(printf "  Total:  ~a\n" test-count)
(printf "  Passed: ~a\n" pass-count)
(printf "  Failed: ~a\n" fail-count)

(if (= fail-count 0)
    (printf "\n✓ All tests passed!\n\n")
    (printf "\n✗ Some tests failed\n\n"))

;;; Return success/failure
(= fail-count 0)
