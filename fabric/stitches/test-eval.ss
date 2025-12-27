;;; Test harness for core/eval.ss — The Evaluator

(load "block.ss")
(load "prim.ss")
(load "eval.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
        (display "✗\n    expected: ")
        (display expected)
        (display "\n    got: ")
        (display actual)))
  (newline))

(define (test-ok name expected result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result)
           (eq? (car result) 'ok)
           (equal? expected (cadr result)))
      (display "✓")
      (begin
        (display "✗\n    expected: (ok ")
        (display expected)
        (display ")\n    got: ")
        (display result)))
  (newline))

(define (test-error name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'error))
      (display "✓")
      (begin
        (display "✗\n    expected error, got: ")
        (display result)))
  (newline))

(define (test-suspended name result)
  (display "  ")
  (display name)
  (display ": ")
  (if (and (pair? result) (eq? (car result) 'suspended))
      (display "✓")
      (begin
        (display "✗\n    expected suspended, got: ")
        (display result)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ============================================================
;;; Literals and Quote
;;; ============================================================
(test-section "Literals and Quote")

(test-ok "number" 42 (run '42 100))
(test-ok "string" "hello" (run '"hello" 100))
(test-ok "boolean true" #t (run '#t 100))
(test-ok "boolean false" #f (run '#f 100))
(test-ok "quoted symbol" 'foo (run ''foo 100))
(test-ok "quoted list" '(1 2 3) (run ''(1 2 3) 100))

;;; ============================================================
;;; Lambda and Application
;;; ============================================================
(test-section "Lambda and Application")

;; Identity function
(define id-result (run '(fn (x) x) 100))
(test "identity is closure" #t (and (eq? (car id-result) 'ok)
                                    (closure? (cadr id-result))))

;; Application
(test-ok "apply identity" 42 (run '((fn (x) x) 42) 100))
(test-ok "apply const" 1 (run '((fn (x) ((fn (y) x) 2)) 1) 100))

;; Multi-argument
(test-ok "multi-arg" 3 (run '((fn (x y) (prim 'add x y)) 1 2) 100))

;;; ============================================================
;;; Let Bindings
;;; ============================================================
(test-section "Let Bindings")

(test-ok "simple let" 42 (run '(let ((x 42)) x) 100))
(test-ok "let with computation" 10
  (run '(let ((x 3) (y 7)) (prim 'add x y)) 100))
(test-ok "nested let" 6
  (run '(let ((x 1))
          (let ((y 2))
            (let ((z 3))
              (prim 'add x (prim 'add y z)))))
       100))
(test-ok "let shadowing" 2
  (run '(let ((x 1))
          (let ((x 2))
            x))
       100))

;;; ============================================================
;;; Fix (Recursion)
;;; ============================================================
(test-section "Fix (Recursion)")

;; Factorial
(test-ok "factorial 0" 1
  (run '(let ((fact (fix fact (fn (n)
                                (if (prim 'zero? n)
                                    1
                                    (prim 'mul n (fact (prim 'sub n 1))))))))
          (fact 0))
       100))

(test-ok "factorial 5" 120
  (run '(let ((fact (fix fact (fn (n)
                                (if (prim 'zero? n)
                                    1
                                    (prim 'mul n (fact (prim 'sub n 1))))))))
          (fact 5))
       500))

;; Fibonacci (costs more fuel)
(test-ok "fibonacci 6" 8
  (run '(let ((fib (fix fib (fn (n)
                              (if (prim 'lt? n 2)
                                  n
                                  (prim 'add (fib (prim 'sub n 1))
                                             (fib (prim 'sub n 2))))))))
          (fib 6))
       1000))

;;; ============================================================
;;; If Conditional
;;; ============================================================
(test-section "If Conditional")

(test-ok "if true" 1 (run '(if #t 1 2) 100))
(test-ok "if false" 2 (run '(if #f 1 2) 100))
(test-ok "if computed" 'yes
  (run '(if (prim 'lt? 1 2) 'yes 'no) 100))
(test-ok "nested if" 3
  (run '(if (prim 'gt? 5 10)
            1
            (if (prim 'gt? 5 3)
                3
                2))
       100))

;;; ============================================================
;;; Primitives
;;; ============================================================
(test-section "Primitives")

(test-ok "add" 5 (run '(prim 'add 2 3) 100))
(test-ok "sub" 7 (run '(prim 'sub 10 3) 100))
(test-ok "mul" 12 (run '(prim 'mul 3 4) 100))
(test-ok "div" 3 (run '(prim 'div 10 3) 100))
(test-ok "neg" -5 (run '(prim 'neg 5) 100))
(test-ok "eq? true" #t (run '(prim 'eq? 1 1) 100))
(test-ok "eq? false" #f (run '(prim 'eq? 1 2) 100))
(test-ok "lt?" #t (run '(prim 'lt? 1 2) 100))
(test-ok "not" #t (run '(prim 'not #f) 100))

;; List primitives
(test-ok "cons" '(1 . 2) (run '(prim 'cons 1 2) 100))
(test-ok "car" 1 (run '(prim 'car (prim 'cons 1 2)) 100))
(test-ok "cdr" 2 (run '(prim 'cdr (prim 'cons 1 2)) 100))
(test-ok "null?" #t (run '(prim 'null? '()) 100))
(test-ok "list" '(1 2 3) (run '(prim 'list 1 2 3) 100))

;;; ============================================================
;;; Fuel and Suspension
;;; ============================================================
(test-section "Fuel and Suspension")

;; With enough fuel, complete
(test-ok "enough fuel" 42 (run '((fn (x) x) 42) 10))

;; With zero fuel, suspend immediately
(test-suspended "zero fuel" (run '((fn (x) x) 42) 0))

;; Run factorial with limited fuel — should suspend
(define limited-result
  (run '(let ((fact (fix fact (fn (n)
                                (if (prim 'zero? n)
                                    1
                                    (prim 'mul n (fact (prim 'sub n 1))))))))
          (fact 10))
       50))
(test-suspended "limited fuel suspends" limited-result)

;;; ============================================================
;;; Blocks and Case
;;; ============================================================
(test-section "Blocks and Case")

;; Create a simple block
(define test-block
  (run '(prim 'make-block 'Just "hello" (prim 'vec-empty)) 100))
(test "make-block ok" 'ok (car test-block))

;; Case on a block (with no refs, pattern has no vars)
(test-ok "case match" "found it"
  (run '(let ((b (prim 'make-block 'Just "payload" (prim 'vec-empty))))
          (case b
            ((Nothing) "nothing")
            ((Just) "found it")))
       100))

;;; ============================================================
;;; Error Cases
;;; ============================================================
(test-section "Error Cases")

(test-error "unbound variable" (run 'undefined 100))
(test-error "apply non-function" (run '(42 1 2) 100))
(test-error "arity mismatch" (run '((fn (x y) x) 1) 100))
;; div by zero returns an error *value*, not an evaluation error
(test-ok "div by zero" '(error div-by-zero) (run '(prim 'div 1 0) 100))

;;; ============================================================
;;; Complex Expressions
;;; ============================================================
(test-section "Complex Expressions")

;; Map over a list
(test-ok "map double" '(2 4 6)
  (run '(let ((map (fix map (fn (f xs)
                              (if (prim 'null? xs)
                                  '()
                                  (prim 'cons (f (prim 'car xs))
                                              (map f (prim 'cdr xs))))))))
          (map (fn (x) (prim 'mul x 2)) '(1 2 3)))
       500))

;; Fold (reduce)
(test-ok "fold sum" 10
  (run '(let ((fold (fix fold (fn (f acc xs)
                                (if (prim 'null? xs)
                                    acc
                                    (fold f (f acc (prim 'car xs)) (prim 'cdr xs)))))))
          (fold (fn (a b) (prim 'add a b)) 0 '(1 2 3 4)))
       500))

;; Compose
(test-ok "compose" 7
  (run '(let ((compose (fn (f g) (fn (x) (f (g x))))))
          (let ((add1 (fn (x) (prim 'add x 1))))
            (let ((double (fn (x) (prim 'mul x 2))))
              ((compose add1 double) 3))))
       100))

;;; ============================================================
;;; Prelude
;;; ============================================================
(test-section "Prelude")

;; Basic combinators
(test-ok "prelude id" 42 (run-prelude '(call id 42) 100))
(test-ok "prelude const" 1 (run-prelude '(call (call const 1) 2) 100))
(test-ok "prelude compose" 7
  (run-prelude '(let ((add1 (fn (x) (prim 'add x 1)))
                      (double (fn (x) (prim 'mul x 2))))
                  (((compose add1) double) 3))
               200))
(test-ok "prelude flip" 2
  (run-prelude '(let ((sub (fn (a) (fn (b) (prim 'sub a b)))))
                  (((flip sub) 3) 5))
               200))

;; Pair operations
(test-ok "prelude fst" 1 (run-prelude '(fst '(1 2)) 100))
(test-ok "prelude snd" 2 (run-prelude '(snd '(1 2)) 100))
(test-ok "prelude pair" '(3 4) (run-prelude '((pair 3) 4) 200))

;; Boolean combinators
(test-ok "prelude bool-not" #f (run-prelude '(bool-not #t) 100))
(test-ok "prelude bool-and" #t (run-prelude '((bool-and #t) #t) 200))
(test-ok "prelude bool-or" #t (run-prelude '((bool-or #f) #t) 200))

;; List operations
(test-ok "prelude map" '(2 4 6)
  (run-prelude '(map (fn (x) (prim 'mul x 2)) '(1 2 3)) 500))
(test-ok "prelude filter" '(2 4)
  (run-prelude '(filter (fn (x) (prim 'eq? 0 (prim 'mod x 2))) '(1 2 3 4 5)) 500))
(test-ok "prelude foldl" 10
  (run-prelude '(foldl (fn (acc x) (prim 'add acc x)) 0 '(1 2 3 4)) 500))
(test-ok "prelude foldr" '(1 2 3)
  (run-prelude '(foldr (fn (x acc) (prim 'cons x acc)) '() '(1 2 3)) 500))
(test-ok "prelude take" '(1 2 3)
  (run-prelude '(take 3 '(1 2 3 4 5)) 300))
(test-ok "prelude drop" '(4 5)
  (run-prelude '(drop 3 '(1 2 3 4 5)) 300))
(test-ok "prelude zip" '((1 4) (2 5) (3 6))
  (run-prelude '(zip '(1 2 3) '(4 5 6)) 500))
(test-ok "prelude range" '(1 2 3 4)
  (run-prelude '(range 1 5) 300))
(test-ok "prelude sum" 15
  (run-prelude '(sum '(1 2 3 4 5)) 300))
(test-ok "prelude product" 120
  (run-prelude '(product '(1 2 3 4 5)) 300))

;; Combining prelude functions
(test-ok "sum of squares" 55
  (run-prelude '(sum (map (fn (x) (prim 'mul x x)) (range 1 6))) 1000))

;;; ============================================================
;;; Environment API
;;; ============================================================
(test-section "Environment API")

(define custom-env (env-extend empty-env 'x 100))
(test-ok "custom env" 100 (eval-with-env 'x custom-env 10))
(test-ok "custom env computation" 105
  (eval-with-env '(prim 'add x 5) custom-env 100))

(newline)
(display "✓ All evaluator tests complete.\n")
