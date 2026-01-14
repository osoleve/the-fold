;;; pattern-matching.ss --- Destructuring Data Structures
;;;
;;; Scheme doesn't have built-in pattern matching like Haskell or ML,
;;; but you can achieve the same effect with predicates and destructuring.
;;;
;;; Run with: scheme --script docs/examples/patterns/pattern-matching.ss

;;; ====
;;; The Pattern: Test + Destructure
;;; ====

(display "=== Pattern Matching Style ===\n")

(display "
Pattern matching is: test what you have, then take it apart.

  1. Check the SHAPE (is it a pair? a list? what tag?)
  2. Extract the PARTS (car, cdr, accessors)
  3. Process each case

This is how functional programs handle algebraic data types.
")

;;; ====
;;; Matching with cond
;;; ====

(display "=== Matching with cond ===\n")

;;; Describe a value based on its type
(define (describe x)
  (cond
    [(null? x) "empty list"]
    [(pair? x) (format #f "pair starting with ~a" (car x))]
    [(number? x) (format #f "number: ~a" x)]
    [(symbol? x) (format #f "symbol: ~a" x)]
    [(string? x) (format #f "string: ~s" x)]
    [else "something else"]))

(format #t "describe '() = ~a~%" (describe '()))
(format #t "describe '(1 2) = ~a~%" (describe '(1 2)))
(format #t "describe 42 = ~a~%" (describe 42))
(format #t "describe 'hello = ~a~%" (describe 'hello))
(format #t "describe \"hi\" = ~a~%" (describe "hi"))

;;; ====
;;; Matching List Shapes
;;; ====

(display "\n=== List Shape Matching ===\n")

(define (list-shape lst)
  (cond
    [(null? lst) "empty"]
    [(null? (cdr lst)) "one element"]
    [(null? (cddr lst)) "two elements"]
    [else "three or more elements"]))

(format #t "() -> ~a~%" (list-shape '()))
(format #t "(a) -> ~a~%" (list-shape '(a)))
(format #t "(a b) -> ~a~%" (list-shape '(a b)))
(format #t "(a b c d) -> ~a~%" (list-shape '(a b c d)))

;;; ====
;;; Tagged Data (Algebraic Data Types)
;;; ====

(display "\n=== Tagged Data Types ===\n")

(display "
In Scheme, we represent variants with tagged lists:
  '(some value)     ; Maybe's Some
  '(none)           ; Maybe's None
  '(left err)       ; Either's Left
  '(right val)      ; Either's Right
")

;;; Maybe type: (some value) or (none)
(define (some x) (list 'some x))
(define (none) '(none))

(define (maybe? x)
  (and (pair? x)
       (or (eq? (car x) 'some)
           (eq? (car x) 'none))))

(define (some? x) (and (pair? x) (eq? (car x) 'some)))
(define (none? x) (equal? x '(none)))
(define (some-value x) (cadr x))

;;; Pattern match on Maybe
(define (maybe-map f mx)
  (cond
    [(none? mx) (none)]
    [(some? mx) (some (f (some-value mx)))]))

(format #t "maybe-map square (some 5) = ~a~%"
        (maybe-map (lambda (x) (* x x)) (some 5)))
(format #t "maybe-map square (none) = ~a~%"
        (maybe-map (lambda (x) (* x x)) (none)))

;;; ====
;;; case for Literal Matching
;;; ====

(display "\n=== case for Literals ===\n")

;;; case matches against literal values (symbols, numbers)
(define (day-type day)
  (case day
    [(saturday sunday) 'weekend]
    [(monday tuesday wednesday thursday friday) 'weekday]
    [else 'unknown]))

(format #t "saturday -> ~a~%" (day-type 'saturday))
(format #t "monday -> ~a~%" (day-type 'monday))
(format #t "holiday -> ~a~%" (day-type 'holiday))

;;; ====
;;; Recursive Pattern Matching
;;; ====

(display "\n=== Recursive Matching ===\n")

;;; Sum a list by matching on structure
(define (sum lst)
  (if (null? lst)
      0                          ; base case: empty
      (+ (car lst)               ; recursive case: head + sum of tail
         (sum (cdr lst)))))

(format #t "sum (1 2 3 4 5) = ~a~%" (sum '(1 2 3 4 5)))

;;; Map with pattern matching style
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))

(format #t "map square (1 2 3) = ~a~%"
        (my-map (lambda (x) (* x x)) '(1 2 3)))

;;; ====
;;; Matching Nested Structures
;;; ====

(display "\n=== Nested Structure Matching ===\n")

;;; Expression type: (lit n) | (add e1 e2) | (mul e1 e2)
(define (eval-expr expr)
  (let ([tag (car expr)])
    (case tag
      [(lit) (cadr expr)]
      [(add) (+ (eval-expr (cadr expr))
                (eval-expr (caddr expr)))]
      [(mul) (* (eval-expr (cadr expr))
                (eval-expr (caddr expr)))]
      [else (error 'eval-expr "unknown expression" expr)])))

(format #t "(lit 5) = ~a~%" (eval-expr '(lit 5)))
(format #t "(add (lit 1) (lit 2)) = ~a~%" (eval-expr '(add (lit 1) (lit 2))))
(format #t "(mul (lit 3) (add (lit 1) (lit 2))) = ~a~%"
        (eval-expr '(mul (lit 3) (add (lit 1) (lit 2)))))

;;; ====
;;; Binary Trees
;;; ====

(display "\n=== Example: Binary Trees ===\n")

;;; Trees: 'empty or (node value left right)
(define empty-tree 'empty)
(define (make-node v l r) (list 'node v l r))
(define (node? t) (and (pair? t) (eq? (car t) 'node)))
(define (node-value t) (cadr t))
(define (node-left t) (caddr t))
(define (node-right t) (cadddr t))

(define tree
  (make-node 5
             (make-node 3
                        (make-node 1 empty-tree empty-tree)
                        (make-node 4 empty-tree empty-tree))
             (make-node 8
                        (make-node 7 empty-tree empty-tree)
                        (make-node 9 empty-tree empty-tree))))

;;; Tree operations using pattern matching style
(define (tree-sum t)
  (if (eq? t 'empty)
      0
      (+ (node-value t)
         (tree-sum (node-left t))
         (tree-sum (node-right t)))))

(define (tree-height t)
  (if (eq? t 'empty)
      0
      (+ 1 (max (tree-height (node-left t))
                (tree-height (node-right t))))))

(define (tree-contains? t x)
  (cond
    [(eq? t 'empty) #f]
    [(= x (node-value t)) #t]
    [(< x (node-value t)) (tree-contains? (node-left t) x)]
    [else (tree-contains? (node-right t) x)]))

(format #t "tree sum: ~a~%" (tree-sum tree))
(format #t "tree height: ~a~%" (tree-height tree))
(format #t "contains 7? ~a~%" (tree-contains? tree 7))
(format #t "contains 6? ~a~%" (tree-contains? tree 6))

;;; ====
;;; The Pattern Matching Mindset
;;; ====

(display "\n=== Summary ===\n")

(display "
Pattern matching in Scheme:

1. USE PREDICATES to test shape
   - null?, pair?, number?, symbol?
   - Custom predicates: node?, some?, none?

2. USE ACCESSORS to extract parts
   - car, cdr, cadr, caddr
   - Custom accessors: node-value, node-left

3. USE cond/case to dispatch
   - cond for arbitrary predicates
   - case for literal symbol/number matching

4. USE RECURSION for nested structures
   - Base case for 'empty' / '() / leaf
   - Recursive case for compound structures

This is exactly what pattern matching does - just explicit.
")

(display "\nDone!\n")
