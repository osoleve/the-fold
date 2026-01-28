;;; Test harness for core/lang/eval.ss — The Evaluator
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/blocks/block.ss")
(load "core/lang/prim.ss")
(load "core/lang/eval.ss")

;;; ============================================================================
;;; Eval-specific assertion helpers
;;; These integrate with the test framework's failure counting while
;;; understanding the evaluator's result types: (ok value fuel) | (error msg) | (suspended ...)
;;; ============================================================================

(define (assert-eval-ok expected result)
  (doc 'description "Assert that result is (ok expected ...). Integrates with test framework.")
  (unless (and (pair? result)
               (eq? (car result) 'ok)
               (equal? expected (cadr result)))
    (inc-failed!)
    (display "    ✗ ")
    (display (ctx-name))
    (newline)
    (display "      Expected: (ok ")
    (write expected)
    (display " ...)")
    (newline)
    (display "      Got:      ")
    (write result)
    (newline)))

(define (assert-eval-error result)
  (doc 'description "Assert that result is (error ...). Integrates with test framework.")
  (unless (and (pair? result) (eq? (car result) 'error))
    (inc-failed!)
    (display "    ✗ ")
    (display (ctx-name))
    (newline)
    (display "      Expected error, got: ")
    (write result)
    (newline)))

(define (assert-eval-suspended result)
  (doc 'description "Assert that result is (suspended ...). Integrates with test framework.")
  (unless (and (pair? result) (eq? (car result) 'suspended))
    (inc-failed!)
    (display "    ✗ ")
    (display (ctx-name))
    (newline)
    (display "      Expected suspended, got: ")
    (write result)
    (newline)))

(define (assert-fuel-remaining expected result)
  (doc 'description "Assert remaining fuel in an (ok value fuel) result.")
  (unless (and (pair? result)
               (eq? (car result) 'ok)
               (= expected (caddr result)))
    (inc-failed!)
    (display "    ✗ ")
    (display (ctx-name))
    (newline)
    (display "      Expected remaining fuel: ")
    (write expected)
    (newline)
    (display "      Got result: ")
    (write result)
    (newline)))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group literals-and-quote
  (define-test "number"
    (assert-eval-ok 42 (run '42 100)))

  (define-test "string"
    (assert-eval-ok "hello" (run '"hello" 100)))

  (define-test "boolean true"
    (assert-eval-ok #t (run '#t 100)))

  (define-test "boolean false"
    (assert-eval-ok #f (run '#f 100)))

  (define-test "quoted symbol"
    (assert-eval-ok 'foo (run ''foo 100)))

  (define-test "quoted list"
    (assert-eval-ok '(1 2 3) (run ''(1 2 3) 100))))

(test-group lambda-and-application
  (define-test "identity is closure"
    (let ([result (run '(fn (x) x) 100)])
      (assert-true (and (eq? (car result) 'ok)
                        (closure? (cadr result))))))

  (define-test "apply identity"
    (assert-eval-ok 42 (run '((fn (x) x) 42) 100)))

  (define-test "apply const"
    (assert-eval-ok 1 (run '((fn (x) ((fn (y) x) 2)) 1) 100)))

  (define-test "multi-arg"
    (assert-eval-ok 3 (run '((fn (x y) (prim 'add x y)) 1 2) 100))))

(test-group let-bindings
  (define-test "simple let"
    (assert-eval-ok 42 (run '(let ((x 42)) x) 100)))

  (define-test "let with computation"
    (assert-eval-ok 10 (run '(let ((x 3) (y 7)) (prim 'add x y)) 100)))

  (define-test "nested let"
    (assert-eval-ok 6
      (run '(let ((x 1))
              (let ((y 2))
                (let ((z 3))
                  (prim 'add x (prim 'add y z)))))
           100)))

  (define-test "let shadowing"
    (assert-eval-ok 2
      (run '(let ((x 1))
              (let ((x 2))
                x))
           100))))

(test-group fix-recursion
  (define-test "factorial 0"
    (assert-eval-ok 1
      (run '(let ((fact (fix fact (fn (n)
                                    (if (prim 'zero? n)
                                        1
                                        (prim 'mul n (fact (prim 'sub n 1))))))))
              (fact 0))
           100)))

  (define-test "factorial 5"
    (assert-eval-ok 120
      (run '(let ((fact (fix fact (fn (n)
                                    (if (prim 'zero? n)
                                        1
                                        (prim 'mul n (fact (prim 'sub n 1))))))))
              (fact 5))
           500)))

  (define-test "fibonacci 6"
    (assert-eval-ok 8
      (run '(let ((fib (fix fib (fn (n)
                                  (if (prim 'lt? n 2)
                                      n
                                      (prim 'add (fib (prim 'sub n 1))
                                            (fib (prim 'sub n 2))))))))
              (fib 6))
           1000))))

(test-group if-conditional
  (define-test "if true"
    (assert-eval-ok 1 (run '(if #t 1 2) 100)))

  (define-test "if false"
    (assert-eval-ok 2 (run '(if #f 1 2) 100)))

  (define-test "if computed"
    (assert-eval-ok 'yes (run '(if (prim 'lt? 1 2) 'yes 'no) 100)))

  (define-test "nested if"
    (assert-eval-ok 3
      (run '(if (prim 'gt? 5 10)
                1
                (if (prim 'gt? 5 3)
                    3
                    2))
           100))))

(test-group primitives
  (define-test "add"
    (assert-eval-ok 5 (run '(prim 'add 2 3) 100)))

  (define-test "sub"
    (assert-eval-ok 7 (run '(prim 'sub 10 3) 100)))

  (define-test "mul"
    (assert-eval-ok 12 (run '(prim 'mul 3 4) 100)))

  (define-test "div"
    (assert-eval-ok 3 (run '(prim 'div 10 3) 100)))

  (define-test "neg"
    (assert-eval-ok -5 (run '(prim 'neg 5) 100)))

  (define-test "eq? true"
    (assert-eval-ok #t (run '(prim 'eq? 1 1) 100)))

  (define-test "eq? false"
    (assert-eval-ok #f (run '(prim 'eq? 1 2) 100)))

  (define-test "lt?"
    (assert-eval-ok #t (run '(prim 'lt? 1 2) 100)))

  (define-test "not"
    (assert-eval-ok #t (run '(prim 'not #f) 100)))

  (define-test "cons"
    (assert-eval-ok '(1 . 2) (run '(prim 'cons 1 2) 100)))

  (define-test "car"
    (assert-eval-ok 1 (run '(prim 'car (prim 'cons 1 2)) 100)))

  (define-test "cdr"
    (assert-eval-ok 2 (run '(prim 'cdr (prim 'cons 1 2)) 100)))

  (define-test "null?"
    (assert-eval-ok #t (run '(prim 'null? '()) 100)))

  (define-test "list"
    (assert-eval-ok '(1 2 3) (run '(prim 'list 1 2 3) 100))))

(test-group fuel-and-suspension
  (define-test "enough fuel"
    (assert-eval-ok 42 (run '((fn (x) x) 42) 10)))

  (define-test "zero fuel"
    (assert-eval-suspended (run '((fn (x) x) 42) 0)))

  (define-test "limited fuel suspends"
    (assert-eval-suspended
      (run '(let ((fact (fix fact (fn (n)
                                    (if (prim 'zero? n)
                                        1
                                        (prim 'mul n (fact (prim 'sub n 1))))))))
              (fact 10))
           50))))

(test-group blocks-and-case
  (define-test "make-block ok"
    (let ([result (run '(prim 'make-block 'Just "hello" (prim 'vec-empty)) 100)])
      (assert-equal 'ok (car result))))

  (define-test "case match"
    (assert-eval-ok "found it"
      (run '(let ((b (prim 'make-block 'Just "payload" (prim 'vec-empty))))
              (case b
                ((Nothing) "nothing")
                ((Just) "found it")))
           100))))

(test-group error-cases
  (define-test "unbound variable"
    (assert-eval-error (run 'undefined 100)))

  (define-test "apply non-function"
    (assert-eval-error (run '(42 1 2) 100)))

  (define-test "arity mismatch"
    (assert-eval-error (run '((fn (x y) x) 1) 100)))

  (define-test "div by zero returns error value"
    (assert-eval-ok '(error div-by-zero) (run '(prim 'div 1 0) 100))))

(test-group complex-expressions
  (define-test "map double"
    (assert-eval-ok '(2 4 6)
      (run '(let ((map (fix map (fn (f xs)
                                  (if (prim 'null? xs)
                                      '()
                                      (prim 'cons (f (prim 'car xs))
                                            (map f (prim 'cdr xs))))))))
              (map (fn (x) (prim 'mul x 2)) '(1 2 3)))
           500)))

  (define-test "fold sum"
    (assert-eval-ok 10
      (run '(let ((fold (fix fold (fn (f acc xs)
                                    (if (prim 'null? xs)
                                        acc
                                        (fold f (f acc (prim 'car xs)) (prim 'cdr xs)))))))
              (fold (fn (a b) (prim 'add a b)) 0 '(1 2 3 4)))
           500)))

  (define-test "compose"
    (assert-eval-ok 7
      (run '(let ((compose (fn (f g) (fn (x) (f (g x))))))
              (let ((add1 (fn (x) (prim 'add x 1))))
                (let ((double (fn (x) (prim 'mul x 2))))
                  ((compose add1 double) 3))))
           100))))

(test-group prelude
  (define-test "prelude id"
    (assert-eval-ok 42 (run-prelude '(call id 42) 100)))

  (define-test "prelude const"
    (assert-eval-ok 1 (run-prelude '(call (call const 1) 2) 100)))

  (define-test "prelude compose"
    (assert-eval-ok 7
      (run-prelude '(let ((add1 (fn (x) (prim 'add x 1)))
                          (double (fn (x) (prim 'mul x 2))))
                      (((compose add1) double) 3))
                   200)))

  (define-test "prelude flip"
    (assert-eval-ok 2
      (run-prelude '(let ((sub (fn (a) (fn (b) (prim 'sub a b)))))
                      (((flip sub) 3) 5))
                   200)))

  (define-test "prelude fst"
    (assert-eval-ok 1 (run-prelude '(fst '(1 2)) 100)))

  (define-test "prelude snd"
    (assert-eval-ok 2 (run-prelude '(snd '(1 2)) 100)))

  (define-test "prelude pair"
    (assert-eval-ok '(3 4) (run-prelude '((pair 3) 4) 200)))

  (define-test "prelude bool-not"
    (assert-eval-ok #f (run-prelude '(bool-not #t) 100)))

  (define-test "prelude bool-and"
    (assert-eval-ok #t (run-prelude '((bool-and #t) #t) 200)))

  (define-test "prelude bool-or"
    (assert-eval-ok #t (run-prelude '((bool-or #f) #t) 200)))

  (define-test "prelude map"
    (assert-eval-ok '(2 4 6)
      (run-prelude '(map (fn (x) (prim 'mul x 2)) '(1 2 3)) 500)))

  (define-test "prelude filter"
    (assert-eval-ok '(2 4)
      (run-prelude '(filter (fn (x) (prim 'eq? 0 (prim 'mod x 2))) '(1 2 3 4 5)) 500)))

  (define-test "prelude foldl"
    (assert-eval-ok 10
      (run-prelude '(foldl (fn (acc x) (prim 'add acc x)) 0 '(1 2 3 4)) 500)))

  (define-test "prelude foldr"
    (assert-eval-ok '(1 2 3)
      (run-prelude '(foldr (fn (x acc) (prim 'cons x acc)) '() '(1 2 3)) 500)))

  (define-test "prelude scanl"
    (assert-eval-ok '(0 1 3 6 10)
      (run-prelude '(scanl (fn (acc x) (prim 'add acc x)) 0 '(1 2 3 4)) 500)))

  (define-test "prelude scanl empty"
    (assert-eval-ok '(0)
      (run-prelude '(scanl (fn (acc x) (prim 'add acc x)) 0 '()) 500)))

  (define-test "prelude scanr"
    (assert-eval-ok '(10 9 7 4 0)
      (run-prelude '(scanr (fn (x acc) (prim 'add x acc)) 0 '(1 2 3 4)) 500)))

  (define-test "prelude scanr empty"
    (assert-eval-ok '(0)
      (run-prelude '(scanr (fn (x acc) (prim 'add x acc)) 0 '()) 500)))

  (define-test "prelude take"
    (assert-eval-ok '(1 2 3)
      (run-prelude '(take 3 '(1 2 3 4 5)) 300)))

  (define-test "prelude drop"
    (assert-eval-ok '(4 5)
      (run-prelude '(drop 3 '(1 2 3 4 5)) 300)))

  (define-test "prelude zip"
    (assert-eval-ok '((1 4) (2 5) (3 6))
      (run-prelude '(zip '(1 2 3) '(4 5 6)) 500)))

  (define-test "prelude range"
    (assert-eval-ok '(1 2 3 4)
      (run-prelude '(range 1 5) 300)))

  (define-test "prelude sum"
    (assert-eval-ok 15
      (run-prelude '(sum '(1 2 3 4 5)) 300)))

  (define-test "prelude product"
    (assert-eval-ok 120
      (run-prelude '(product '(1 2 3 4 5)) 300)))

  (define-test "prelude flatten"
    (assert-eval-ok '(1 2 3 4 5 6)
      (run-prelude '(flatten '((1 2) (3 4) (5 6))) 500)))

  (define-test "prelude flatten empty"
    (assert-eval-ok '()
      (run-prelude '(flatten '()) 300)))

  (define-test "prelude flatMap"
    (assert-eval-ok '(1 1 2 2 3 3)
      (run-prelude '((flatMap (fn (x) (prim 'list x x))) '(1 2 3)) 800)))

  (define-test "prelude any true"
    (assert-eval-ok #t
      (run-prelude '(any (fn (x) (prim 'eq? x 3)) '(1 2 3 4)) 500)))

  (define-test "prelude any false"
    (assert-eval-ok #f
      (run-prelude '(any (fn (x) (prim 'eq? x 5)) '(1 2 3 4)) 500)))

  (define-test "prelude all true"
    (assert-eval-ok #t
      (run-prelude '(all (fn (x) (prim 'positive? x)) '(1 2 3 4)) 500)))

  (define-test "prelude all false"
    (assert-eval-ok #f
      (run-prelude '(all (fn (x) (prim 'positive? x)) '(1 -2 3 4)) 500)))

  (define-test "prelude elem true"
    (assert-eval-ok #t
      (run-prelude '(elem 3 '(1 2 3 4)) 500)))

  (define-test "prelude elem false"
    (assert-eval-ok #f
      (run-prelude '(elem 5 '(1 2 3 4)) 500)))

  (define-test "prelude replicate"
    (assert-eval-ok '(x x x x x)
      (run-prelude '(replicate 5 'x) 500)))

  (define-test "prelude takeWhile"
    (assert-eval-ok '(1 2 3)
      (run-prelude '(takeWhile (fn (x) (prim 'lt? x 4)) '(1 2 3 4 5 6)) 800)))

  (define-test "prelude dropWhile"
    (assert-eval-ok '(4 5 6)
      (run-prelude '(dropWhile (fn (x) (prim 'lt? x 4)) '(1 2 3 4 5 6)) 800)))

  (define-test "prelude span"
    (assert-eval-ok '((1 2 3) (4 5 6))
      (run-prelude '(span (fn (x) (prim 'lt? x 4)) '(1 2 3 4 5 6)) 1000)))

  (define-test "prelude break"
    (assert-eval-ok '((1 2 3) (4 5 6))
      (run-prelude '((break (fn (x) (prim 'ge? x 4))) '(1 2 3 4 5 6)) 1000)))

  (define-test "prelude partition"
    (assert-eval-ok '((2 4 6) (1 3 5))
      (run-prelude '(partition (fn (x) (prim 'eq? 0 (prim 'mod x 2))) '(1 2 3 4 5 6)) 1500)))

  (define-test "prelude zipWith"
    (assert-eval-ok '(5 7 9)
      (run-prelude '(zipWith (fn (x y) (prim 'add x y)) '(1 2 3) '(4 5 6)) 800)))

  (define-test "prelude unzip"
    (assert-eval-ok '((1 2 3) (4 5 6))
      (run-prelude '(unzip '((1 4) (2 5) (3 6))) 800)))

  (define-test "prelude intersperse"
    (assert-eval-ok '(a x b x c)
      (run-prelude '(intersperse 'x '(a b c)) 800)))

  (define-test "prelude group"
    (assert-eval-ok '((1 1) (2 2 2) (3) (4 4))
      (run-prelude '(group '(1 1 2 2 2 3 4 4)) 1500)))

  (define-test "prelude nub"
    (assert-eval-ok '(1 2 3 4)
      (run-prelude '(nub '(1 2 2 3 1 4 3)) 1500)))

  (define-test "prelude find some"
    (assert-eval-ok '(some . 3)
      (run-prelude '(find (fn (x) (prim 'eq? 0 (prim 'mod x 3))) '(1 2 3 4)) 800)))

  (define-test "prelude find none"
    (assert-eval-ok 'none
      (run-prelude '(find (fn (x) (prim 'eq? 0 (prim 'mod x 7))) '(1 2 3 4)) 800)))

  (define-test "prelude splitAt"
    (assert-eval-ok '((1 2 3) (4 5 6))
      (run-prelude '(splitAt 3 '(1 2 3 4 5 6)) 800)))

  (define-test "sum of squares"
    (assert-eval-ok 55
      (run-prelude '(sum (map (fn (x) (prim 'mul x x)) (range 1 6))) 1000))))

(test-group environment-api
  (define-test "custom env"
    (let ([custom-env (env-extend empty-env 'x 100)])
      (assert-eval-ok 100 (eval-with-env 'x custom-env 10))))

  (define-test "custom env computation"
    (let ([custom-env (env-extend empty-env 'x 100)])
      (assert-eval-ok 105
        (eval-with-env '(prim 'add x 5) custom-env 100)))))

(test-group fuel-semantics
  (define-test "literal consumes 1 fuel"
    (let ([result (eval-expr 42 empty-env 1)])
      (assert-fuel-remaining 0 result)))

  (define-test "literal suspends with 0 fuel"
    (assert-eval-suspended (eval-expr 42 empty-env 0)))

  (define-test "prim with no args succeeds"
    (let ([result (eval-expr '(prim 'vec-empty) empty-env 1)])
      (assert-eval-ok (vector) result)))

  (define-test "prim with no args remaining fuel"
    (let ([result (eval-expr '(prim 'vec-empty) empty-env 1)])
      (assert-fuel-remaining 0 result)))

  (define-test "prim with no args suspends with 0 fuel"
    (assert-eval-suspended (eval-expr '(prim 'vec-empty) empty-env 0)))

  (define-test "prim add succeeds with exact fuel"
    (let ([result (eval-expr '(prim 'add 2 3) empty-env 3)])
      (assert-eval-ok 5 result)))

  (define-test "prim add remaining fuel"
    (let ([result (eval-expr '(prim 'add 2 3) empty-env 3)])
      (assert-fuel-remaining 0 result)))

  (define-test "prim add suspends when fuel short"
    (assert-eval-suspended (eval-expr '(prim 'add 2 3) empty-env 2)))

  (define-test "deterministic suspension"
    (let ([suspend-a (eval-expr '(prim 'add 2 3) empty-env 2)]
          [suspend-b (eval-expr '(prim 'add 2 3) empty-env 2)])
      (assert-equal suspend-a suspend-b))))

(test-group doc-special-form
  (define-test "doc returns ok"
    (let ([result (eval-expr '(doc 'todo "test") empty-env 10)])
      (assert-equal 'ok (car result))))

  (define-test "doc consumes 1 fuel"
    (let ([result (eval-expr '(doc 'todo "test") empty-env 10)])
      (assert-equal 9 (caddr result))))

  (define-test "doc doesn't evaluate args"
    (let ([result (eval-expr '(doc 'note (error "should not run")) empty-env 10)])
      (assert-equal 'ok (car result))))

  (define-test "doc in pseq sequence"
    (assert-eval-ok 42
      (eval-expr '(pseq (doc 'type Int) 42) empty-env 20)))

  (define-test "multiple docs in pseq"
    (assert-eval-ok 100
      (eval-expr '(pseq (doc 'a) (pseq (doc 'b) 100)) empty-env 30))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
