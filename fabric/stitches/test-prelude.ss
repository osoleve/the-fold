;;; core/test-prelude.ss — Tests for prelude.ss
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "prelude.ss")

(define tests-passed 0)
(define tests-failed 0)

(define (test name expected actual)
  (if (equal? expected actual)
      (begin
        (set! tests-passed (+ tests-passed 1))
        (display "  ✓ ") (display name) (newline))
      (begin
        (set! tests-failed (+ tests-failed 1))
        (display "  ✗ ") (display name)
        (display " — expected ") (write expected)
        (display ", got ") (write actual)
        (newline))))

(display "Testing prelude.ss\n")
(display "==================\n\n")

;;; andmap tests
(display "andmap:\n")
(test "empty list" #t (andmap number? '()))
(test "all pass" #t (andmap number? '(1 2 3)))
(test "one fails" #f (andmap number? '(1 "x" 3)))
(test "first fails" #f (andmap number? '("x" 2 3)))

;;; ormap tests
(display "\normap:\n")
(test "empty list" #f (ormap number? '()))
(test "one passes" #t (ormap number? '("a" 2 "c")))
(test "all fail" #f (ormap number? '("a" "b" "c")))

;;; unique tests
(display "\nunique:\n")
(test "no duplicates" '(a b c) (unique '(a b c)))
(test "with duplicates" '(a b c) (unique '(a b a c b)))
(test "empty list" '() (unique '()))
(test "all same" '(x) (unique '(x x x)))

;;; filter tests
(display "\nfilter:\n")
(test "keep evens" '(2 4) (filter even? '(1 2 3 4 5)))
(test "keep none" '() (filter even? '(1 3 5)))
(test "keep all" '(2 4 6) (filter even? '(2 4 6)))

;;; fold-left tests
(display "\nfold-left:\n")
(test "sum" 10 (fold-left + 0 '(1 2 3 4)))
(test "empty" 0 (fold-left + 0 '()))
(test "left assoc" '(((0 1) 2) 3) (fold-left (lambda (acc x) (list acc x)) 0 '(1 2 3)))

;;; fold-right tests
(display "\nfold-right:\n")
(test "sum" 10 (fold-right + 0 '(1 2 3 4)))
(test "right assoc" '(1 (2 (3 0))) (fold-right (lambda (x acc) (list x acc)) 0 '(1 2 3)))

;;; zip tests
(display "\nzip:\n")
(test "equal length" '((1 . a) (2 . b)) (zip '(1 2) '(a b)))
(test "first shorter" '((1 . a)) (zip '(1) '(a b)))
(test "empty" '() (zip '() '(a b)))

;;; iota tests
(display "\niota:\n")
(test "iota 0" '() (iota 0))
(test "iota 5" '(0 1 2 3 4) (iota 5))

;;; take/drop tests
(display "\ntake/drop:\n")
(test "take 2" '(a b) (take 2 '(a b c d)))
(test "take 0" '() (take 0 '(a b c)))
(test "drop 2" '(c d) (drop 2 '(a b c d)))
(test "drop 0" '(a b c) (drop 0 '(a b c)))

;;; Result type tests
(display "\nResult type:\n")
(test "ok? true" #t (ok? '(ok 42)))
(test "ok? false" #f (ok? '(error bad)))
(test "error? true" #t (error? '(error bad)))
(test "error? false" #f (error? '(ok 42)))
(test "unwrap-ok" 42 (unwrap-ok '(ok 42)))
(test "unwrap-error" '(bad thing) (unwrap-error '(error bad thing)))

;;; result-map tests
(display "\nresult-map:\n")
(test "map ok" '(ok 10) (result-map (lambda (x) (* x 2)) '(ok 5)))
(test "map error" '(error oops) (result-map (lambda (x) (* x 2)) '(error oops)))

;;; result-bind tests
(display "\nresult-bind:\n")
(test "bind ok" '(ok 10) (result-bind '(ok 5) (lambda (x) `(ok ,(* x 2)))))
(test "bind error" '(error oops) (result-bind '(error oops) (lambda (x) `(ok ,(* x 2)))))

;;; result-sequence tests
(display "\nresult-sequence:\n")
(test "all ok" '(ok (1 2 3)) (result-sequence '((ok 1) (ok 2) (ok 3))))
(test "one error" '(error bad) (result-sequence '((ok 1) (error bad) (ok 3))))
(test "empty" '(ok ()) (result-sequence '()))

;;; string-join tests
(display "\nstring-join:\n")
(test "join words" "a,b,c" (string-join '("a" "b" "c") ","))
(test "join empty" "" (string-join '() ","))
(test "join one" "hello" (string-join '("hello") ","))

;;; Summary
(newline)
(display "==================\n")
(display "Passed: ") (display tests-passed) (newline)
(display "Failed: ") (display tests-failed) (newline)

(if (> tests-failed 0)
    (exit 1)
    (display "\n✓ All prelude tests passed!\n"))
