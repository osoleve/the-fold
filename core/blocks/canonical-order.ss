;;; core/blocks/canonical-order.ss — Canonical ordering for expression sorting
;;; @module canonical-order
;;; @requires prelude
;;;
;;; Defines a total order over S-expressions for deterministic sorting
;;; during algebraic normalization. The order must be:
;;;   - Total: Every pair of expressions is comparable
;;;   - Stable: Same expressions always compare the same way
;;;   - Deterministic: No randomness or external state
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Expression Class Ordering
;;; ====

;;; Expressions are grouped into classes for comparison.
;;; Within the same class, type-specific comparison is used.
;;; Across classes, the class number determines order.
;;;
;;; Class ordering (lower = comes first):
;;;   0: Numbers (sorted numerically)
;;;   1: Booleans (#f < #t)
;;;   2: Characters (by char code)
;;;   3: Strings (lexicographic)
;;;   4: Symbols (alphabetic by name)
;;;   5: De Bruijn variables (by index)
;;;   6: Quoted data (by structure)
;;;   7: Compound expressions (recursive lexicographic)
;;;   8: Other (fallback)

;;; expr-order-class : S-expr → Nat
;;; Returns the ordering class of an expression.
(define (expr-order-class expr)
  (cond
    [(number? expr)  0]
    [(boolean? expr) 1]
    [(char? expr)    2]
    [(string? expr)  3]
    [(symbol? expr)  4]
    [(and (pair? expr) (eq? (car expr) 'dv)
          (pair? (cdr expr)) (number? (cadr expr)))
     5]  ; de Bruijn variable
    [(and (pair? expr) (eq? (car expr) 'quote))
     6]  ; quoted data
    [(pair? expr)    7]  ; compound expression
    [else            8]))

;;; ====
;;; Main Comparison Functions
;;; ====

;;; canonical<? : S-expr × S-expr → Bool
;;; Returns #t if a should come before b in canonical order.
;;; This is a strict total order: exactly one of a<b, a=b, b<a holds.
(define (canonical<? a b)
  (let ([ca (expr-order-class a)]
        [cb (expr-order-class b)])
    (cond
      [(< ca cb) #t]
      [(> ca cb) #f]
      ;; Same class: use type-specific comparison
      [(= ca 0) (number<? a b)]
      [(= ca 1) (boolean<? a b)]
      [(= ca 2) (char<? a b)]
      [(= ca 3) (string<? a b)]
      [(= ca 4) (symbol<? a b)]
      [(= ca 5) (dv<? a b)]
      [(= ca 6) (quoted<? a b)]
      [(= ca 7) (compound<? a b)]
      [else #f])))  ; class 8: equal by default

;;; canonical=? : S-expr × S-expr → Bool
;;; Returns #t if a and b are equal in canonical ordering.
(define (canonical=? a b)
  (and (not (canonical<? a b))
       (not (canonical<? b a))))

;;; canonical<=? : S-expr × S-expr → Bool
(define (canonical<=? a b)
  (or (canonical<? a b) (canonical=? a b)))

;;; ====
;;; Type-Specific Comparisons
;;; ====

;;; number<? : Number × Number → Bool
;;; Numeric comparison with special handling for exact vs inexact.
;;; Exact numbers come before inexact numbers of the same value.
(define (number<? a b)
  (cond
    [(< a b) #t]
    [(> a b) #f]
    ;; Equal numeric value: prefer exact over inexact
    [(and (exact? a) (not (exact? b))) #t]
    [(and (not (exact? a)) (exact? b)) #f]
    [else #f]))

;;; boolean<? : Bool × Bool → Bool
;;; #f < #t (false before true)
(define (boolean<? a b)
  (and (not a) b))

;;; symbol<? : Symbol × Symbol → Bool
;;; Alphabetic comparison by symbol name.
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; dv<? : (dv N) × (dv M) → Bool
;;; De Bruijn variable comparison by index.
(define (dv<? a b)
  (< (cadr a) (cadr b)))

;;; quoted<? : (quote X) × (quote Y) → Bool
;;; Quoted data comparison by comparing the quoted values.
(define (quoted<? a b)
  (datum<? (cadr a) (cadr b)))

;;; datum<? : Datum × Datum → Bool
;;; Comparison for arbitrary quoted data (not expressions).
(define (datum<? a b)
  (cond
    [(and (null? a) (null? b)) #f]
    [(null? a) #t]
    [(null? b) #f]
    [(and (pair? a) (pair? b))
     (cond
       [(datum<? (car a) (car b)) #t]
       [(datum<? (car b) (car a)) #f]
       [else (datum<? (cdr a) (cdr b))])]
    [(pair? a) #f]  ; non-pair < pair
    [(pair? b) #t]
    ;; Atoms: use canonical<? for uniform handling
    [else (canonical<? a b)]))

;;; compound<? : Pair × Pair → Bool
;;; Lexicographic comparison of compound expressions.
(define (compound<? a b)
  (cond
    [(and (null? a) (null? b)) #f]
    [(null? a) #t]
    [(null? b) #f]
    [(canonical<? (car a) (car b)) #t]
    [(canonical<? (car b) (car a)) #f]
    [else (compound<? (cdr a) (cdr b))]))

;;; ====
;;; Sorting Support
;;; ====

;;; canonical-sort : (List S-expr) → (List S-expr)
;;; Sort a list of expressions in canonical order.
(define (canonical-sort exprs)
  (list-sort canonical<? exprs))

;;; canonical-merge : (List S-expr) × (List S-expr) → (List S-expr)
;;; Merge two sorted lists maintaining canonical order.
(define (canonical-merge as bs)
  (cond
    [(null? as) bs]
    [(null? bs) as]
    [(canonical<? (car as) (car bs))
     (cons (car as) (canonical-merge (cdr as) bs))]
    [else
     (cons (car bs) (canonical-merge as (cdr bs)))]))
