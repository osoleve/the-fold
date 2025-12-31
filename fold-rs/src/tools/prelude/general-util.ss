; general-util.ss - General utility functions and aliases
; Part of The Fold prelude

; ============================================
; Basic utility functions
; ============================================

; id: Identity function
; (id 5) => 5
(define (id x) x)

; identity: Alias for id
(define identity id)

; const: Constant function (returns x regardless of input)
; ((const 5) 10) => 5
(define (const x)
  (fn (_) x))

; constantly: Alias for const
(define constantly const)

; ============================================
; Mathematical shortcuts
; ============================================

; inc: Increment by 1
; (inc 5) => 6
(define (inc x) (+ x 1))

; dec: Decrement by 1
; (dec 5) => 4
(define (dec x) (- x 1))

; succ: Successor (alias for inc)
(define succ inc)

; pred-fn: Predecessor (alias for dec)
(define pred-fn dec)

; square: Square a number
; (square 5) => 25
(define (square x) (* x x))

; cube: Cube a number
; (cube 3) => 27
(define (cube x) (* x x x))

; double: Double a number
; (double 5) => 10
(define (double x) (* x 2))

; halve: Halve a number
; (halve 10) => 5
(define (halve x) (/ x 2))

; ============================================
; Sign and range utilities
; ============================================

; signum: Sign of number (-1, 0, or 1)
; (signum -5) => -1
(define (signum x)
  (if (> x 0)
      1
      (if (< x 0)
          -1
          0)))

; sign: Alias for signum
(define sign signum)

; in-range?: Check if value is in range [lo, hi]
; (in-range? 0 10 5) => #t
(define (in-range? lo hi x)
  (and (>= x lo) (<= x hi)))

; ============================================
; List aliases and utilities
; ============================================

; singleton?: Check if list has exactly one element
; (singleton? '(1)) => #t
(define (singleton? lst)
  (and (pair? lst) (null? (cdr lst))))

; has-pair?: Check if value is a pair
; (has-pair? '(1 2)) => #t
(define has-pair? pair?)

; list-tail: Alias for drop-n
(define list-tail drop-n)

; list-head: Alias for take-n
(define list-head take-n)

; every?: Alias for all
(define every? all)

; some?: Alias for any
(define some? any)

; filter-not: Filter elements NOT matching predicate
; (filter-not even? '(1 2 3 4)) => (1 3)
(define (filter-not p lst)
  (filter (fn (x) (not (p x))) lst))

; remove-all: Remove all occurrences of element
; (remove-all 2 '(1 2 3 2 4)) => (1 3 4)
(define (remove-all x lst)
  (filter (fn (y) (not (equal? x y))) lst))

; append-map: Alias for flat-map
(define append-map flat-map)

; ============================================
; List construction
; ============================================

; cons*: Cons multiple elements
; (cons* 1 2 3 '(4)) => (1 2 3 4)
(define (cons* . args)
  (if (null? args)
      '()
      (if (null? (cdr args))
          (car args)
          (cons (car args) (apply cons* (cdr args))))))

; list*: Alias for cons*
(define list* cons*)

; build-list: Build list by applying function to indices
; (build-list 5 square) => (0 1 4 9 16)
(define (build-list n f)
  (map f (range 0 n)))

; tabulate: Alias for build-list
(define tabulate build-list)

; make-list-fn: Create list of n elements using function
(define make-list-fn build-list)

; ============================================
; Comparison utilities
; ============================================

; max-of: Maximum element by comparison function
; (max-of > '(3 1 4 1 5)) => 5
(define (max-of compare lst)
  (if (null? lst)
      '()
      (foldl (fn (acc x) (if (compare x acc) x acc))
             (car lst)
             (cdr lst))))

; min-of: Minimum element by comparison function
; (min-of < '(3 1 4 1 5)) => 1
(define (min-of compare lst)
  (if (null? lst)
      '()
      (foldl (fn (acc x) (if (compare x acc) x acc))
             (car lst)
             (cdr lst))))

; argmax: Element that maximizes function
; (argmax abs '(-5 3 -7 2)) => -7
(define (argmax f lst)
  (if (null? lst)
      '()
      (foldl (fn (acc x) (if (> (f x) (f acc)) x acc))
             (car lst)
             (cdr lst))))

; argmin: Element that minimizes function
; (argmin abs '(-5 3 -7 2)) => 2
(define (argmin f lst)
  (if (null? lst)
      '()
      (foldl (fn (acc x) (if (< (f x) (f acc)) x acc))
             (car lst)
             (cdr lst))))

; ============================================
; Module exports
; ============================================

(module-exports
  id identity
  const constantly
  inc dec succ pred-fn
  square cube double halve
  signum sign
  in-range?
  singleton? has-pair?
  list-tail list-head
  every? some?
  filter-not remove-all
  append-map
  cons* list*
  build-list tabulate make-list-fn
  max-of min-of
  argmax argmin)
