(load "core/base/prelude.ss")
(load "core/lang/module.ss")  ; Needed for 'require' in CAS examples

(doc 'module 'examples)
(doc 'description "Curated gallery of idiomatic Fold examples.
Each example includes commented code, expected output, and variations.")
(doc 'layer 'user)
(doc 'purity 'partial)

;;; ============================================================
;;; Example Registry
;;; ============================================================

(define *examples* (make-eq-hashtable))

;; Helper for hashtable iteration (not in prelude)
(define (hashtable->alist ht)
  (let-values ([(keys vals) (hashtable-entries ht)])
    (let loop ([i 0] [result '()])
      (if (>= i (vector-length keys))
          result
          (loop (+ i 1) (cons (cons (vector-ref keys i)
                                     (vector-ref vals i))
                              result))))))

(define (register-example! name category code description expected)
  "Register an example in the gallery."
  (hashtable-set! *examples* name
    `((category . ,category)
      (code . ,code)
      (description . ,description)
      (expected . ,expected))))

(define (example name)
  "Run an example and show its code and output."
  (let ([entry (hashtable-ref *examples* name #f)])
    (if entry
        (let ([code (cdr (assq 'code entry))]
              [desc (cdr (assq 'description entry))]
              [expected (cdr (assq 'expected entry))])
          (printf "~n=== ~a ===~n" name)
          (printf "~a~n~n" desc)
          (printf "Code:~n")
          (pretty-print code)
          (printf "~nOutput:~n")
          (let ([result (eval code)])
            (pretty-print result)
            (printf "~nExpected: ~a~n" expected)
            result))
        (printf "Example not found: ~a~nUse (examples) to list available examples.~n" name))))

(define (examples)
  "List all available examples by category."
  (let ([entries (hashtable->alist *examples*)])
    (printf "~n=== Examples Gallery ===~n~n")
    (for-each
     (lambda (cat)
       (let ([cat-examples (filter (lambda (e)
                                     (eq? (cdr (assq 'category (cdr e))) cat))
                                   entries)])
         (when (pair? cat-examples)
           (printf "~a:~n" cat)
           (for-each
            (lambda (e)
              (printf "  (example '~a)  - ~a~n"
                      (car e)
                      (cdr (assq 'description (cdr e)))))
            cat-examples)
           (printf "~n"))))
     '(getting-started recursion higher-order data-structures content-addressing dsl-building))))

(define (example-code name)
  "Get just the code for an example (for copying)."
  (let ([entry (hashtable-ref *examples* name #f)])
    (if entry
        (cdr (assq 'code entry))
        #f)))

;;; ============================================================
;;; Getting Started
;;; ============================================================

(register-example! 'hello-world 'getting-started
  '(display "Hello, Fold!\n")
  "The classic first program"
  "Hello, Fold!")

(register-example! 'basic-math 'getting-started
  '(+ (* 3 4) (- 10 5))
  "Arithmetic with prefix notation"
  "17")

(register-example! 'define-and-use 'getting-started
  '(let ([x 10]
         [y 20])
     (+ x y))
  "Local bindings with let"
  "30")

;;; ============================================================
;;; Recursion Patterns
;;; ============================================================

(register-example! 'factorial 'recursion
  '(letrec ([fact (lambda (n)
                    (if (<= n 1)
                        1
                        (* n (fact (- n 1)))))])
     (fact 5))
  "Classic recursive factorial"
  "120")

(register-example! 'fibonacci 'recursion
  '(letrec ([fib (lambda (n)
                   (cond
                     [(< n 2) n]
                     [else (+ (fib (- n 1))
                              (fib (- n 2)))]))])
     (map fib '(0 1 2 3 4 5 6 7 8 9)))
  "Fibonacci sequence (naive recursive)"
  "(0 1 1 2 3 5 8 13 21 34)")

(register-example! 'tail-recursive-sum 'recursion
  '(letrec ([sum (lambda (lst acc)
                   (if (null? lst)
                       acc
                       (sum (cdr lst) (+ acc (car lst)))))])
     (sum '(1 2 3 4 5) 0))
  "Tail-recursive list sum with accumulator"
  "15")

;;; ============================================================
;;; Higher-Order Functions
;;; ============================================================

(register-example! 'map-double 'higher-order
  '(map (lambda (x) (* x 2)) '(1 2 3 4 5))
  "Double each element with map"
  "(2 4 6 8 10)")

(register-example! 'filter-even 'higher-order
  '(filter even? '(1 2 3 4 5 6 7 8 9 10))
  "Keep only even numbers"
  "(2 4 6 8 10)")

(register-example! 'fold-sum 'higher-order
  '(fold-left + 0 '(1 2 3 4 5))
  "Sum a list with fold-left"
  "15")

(register-example! 'compose-functions 'higher-order
  '(let* ([add1 (lambda (x) (+ x 1))]
          [double (lambda (x) (* x 2))]
          [composed (lambda (x) (add1 (double x)))])
     (map composed '(1 2 3 4 5)))
  "Function composition: add1 after double"
  "(3 5 7 9 11)")

(register-example! 'currying 'higher-order
  '(let ([add (lambda (a)
                (lambda (b) (+ a b)))]
         [add5 ((lambda (a) (lambda (b) (+ a b))) 5)])
     (map add5 '(1 2 3 4 5)))
  "Curried addition"
  "(6 7 8 9 10)")

;;; ============================================================
;;; Data Structures
;;; ============================================================

(register-example! 'list-operations 'data-structures
  '(let ([lst '(a b c d e)])
     (list (car lst)           ; first element
           (cadr lst)          ; second element
           (length lst)        ; length
           (reverse lst)       ; reversed
           (append lst '(f g)))) ; appended
  "Basic list operations"
  "(a b 5 (e d c b a) (a b c d e f g))")

(register-example! 'association-list 'data-structures
  '(let ([alist '((name . alice)
                  (age . 30)
                  (city . boston))])
     (list (assq 'name alist)
           (assq 'age alist)
           (cdr (assq 'city alist))))
  "Association lists for key-value pairs"
  "((name . alice) (age . 30) boston)")

(register-example! 'vector-basics 'data-structures
  '(let ([v (vector 10 20 30 40 50)])
     (list (vector-ref v 0)
           (vector-ref v 2)
           (vector-length v)))
  "Vector creation and access"
  "(10 30 5)")

;;; ============================================================
;;; Content Addressing (requires CAS)
;;; ============================================================

(register-example! 'store-and-fetch 'content-addressing
  '(begin
     (require 'blocks)
     (let* ([data '(hello world)]
            [hash (store data)]
            [retrieved (fetch hash)])
       (equal? data retrieved)))
  "Store data, get hash, fetch by hash"
  "#t")

;;; ============================================================
;;; DSL Building (mini-languages and interpreters)
;;; ============================================================

(register-example! 'arithmetic-interpreter 'dsl-building
  '(letrec ([eval-expr
             (lambda (expr env)
               (cond
                 [(number? expr) expr]
                 [(symbol? expr) (cdr (assq expr env))]
                 [(eq? (car expr) '+)
                  (+ (eval-expr (cadr expr) env)
                     (eval-expr (caddr expr) env))]
                 [(eq? (car expr) '*)
                  (* (eval-expr (cadr expr) env)
                     (eval-expr (caddr expr) env))]
                 [(eq? (car expr) 'let)
                  (let* ([binding (cadr expr)]
                         [var (car binding)]
                         [val (eval-expr (cadr binding) env)]
                         [new-env (cons (cons var val) env)])
                    (eval-expr (caddr expr) new-env))]))])
     (eval-expr '(let (x 10) (+ x (* x 2))) '()))
  "Mini interpreter for arithmetic with let bindings"
  "30")

(register-example! 'pattern-matcher 'dsl-building
  '(letrec ([match
             (lambda (pattern data)
               (cond
                 [(eq? pattern '_) #t]              ; wildcard
                 [(symbol? pattern) #t]             ; variable (always matches)
                 [(and (null? pattern) (null? data)) #t]
                 [(or (null? pattern) (null? data)) #f]
                 [(and (pair? pattern) (pair? data))
                  (and (match (car pattern) (car data))
                       (match (cdr pattern) (cdr data)))]
                 [else (equal? pattern data)]))])
     (list (match '(a _ c) '(a b c))      ; wildcard match
           (match '(1 2 3) '(1 2 3))      ; exact match
           (match '(x y) '(hello world))  ; variable match
           (match '(a b) '(a b c))))      ; length mismatch
  "Simple pattern matcher with wildcards"
  "(#t #t #t #f)")

(register-example! 'state-machine 'dsl-building
  '(letrec ([make-machine
             (lambda (transitions initial)
               (lambda (inputs)
                 (let loop ([state initial] [inputs inputs])
                   (if (null? inputs)
                       state
                       (let* ([input (car inputs)]
                              [key (cons state input)]
                              [next (cdr (assoc key transitions))])
                         (loop next (cdr inputs)))))))])
     ;; Traffic light: green -> yellow -> red -> green
     (let ([light (make-machine
                    '(((green . tick) . yellow)
                      ((yellow . tick) . red)
                      ((red . tick) . green))
                    'green)])
       (list (light '())
             (light '(tick))
             (light '(tick tick))
             (light '(tick tick tick)))))
  "State machine DSL for traffic light"
  "(green yellow red green)")

(register-example! 'query-dsl 'dsl-building
  '(let* ([data '((name . alice) (age . 30) (city . boston))]
          [select (lambda (keys record)
                    (filter (lambda (pair) (memq (car pair) keys))
                            record))]
          [where (lambda (pred records)
                   (filter pred records))]
          [records '(((name . alice) (age . 30))
                     ((name . bob) (age . 25))
                     ((name . carol) (age . 35)))])
     ;; SELECT name FROM records WHERE age > 28
     (map (lambda (r) (cdr (assq 'name r)))
          (where (lambda (r) (> (cdr (assq 'age r)) 28))
                 records)))
  "Simple query DSL combinators"
  "(alice carol)")

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "Examples gallery loaded.~n")
(printf "  (examples)           - List all examples~n")
(printf "  (example 'name)      - Run an example~n")
(printf "  (example-code 'name) - Get code for copying~n")
