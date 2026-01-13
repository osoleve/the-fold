;;; core/blocks/poly-canon.ss — Polynomial Canonicalization for Normalization
;;; @module poly-canon
;;; @requires prelude
;;;
;;; Canonicalizes arithmetic expressions by lifting to polynomial representation
;;; and lowering back to a canonical S-expression form (sum-of-products).
;;;
;;; Enables semantic equivalence detection:
;;;   (+ x x)              ≡  (* 2 x)
;;;   (+ (* a b) (* b a))  =  (+ (* a b) (* a b))  [after sorting]  = (* 2 a b)
;;;
;;; CONSTRAINTS:
;;;   - Only handles exact numbers (integers, rationals) — floats are opaque
;;;   - Size/depth limits prevent expensive canonicalization on huge expressions
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

;;; Maximum expression depth for polynomial canonicalization.
(define *poly-canon-max-depth* 10)

;;; Maximum number of terms after expansion.
(define *poly-canon-max-terms* 100)

;;; ============================================================
;;; Arithmetic Expression Detection
;;; ============================================================

;;; exact-number? : Any → Bool
;;; True for integers and exact rationals, false for floats.
(define (exact-number? x)
  (and (number? x)
       (or (integer? x)
           (and (rational? x) (exact? x)))))

;;; arithmetic-expr? : S-expr → Bool
(define (arithmetic-expr? expr)
  (arithmetic-expr-depth? expr *poly-canon-max-depth*))

;;; arithmetic-expr-depth? : S-expr × Nat → Bool
(define (arithmetic-expr-depth? expr depth)
  (cond
    [(<= depth 0) #f]
    [(exact-number? expr) #t]
    [(symbol? expr) #t]
    [(not (pair? expr)) #f]
    [(eq? (car expr) 'quote) #f]
    [(eq? (car expr) 'dv) #t]
    [(memq (car expr) '(+ * -))
     (and (not (null? (cdr expr)))
          (andmap (lambda (a) (arithmetic-expr-depth? a (- depth 1)))
                  (cdr expr)))]
    [else #f]))

;;; ============================================================
;;; Internal Polynomial Representation
;;; ============================================================
;;;
;;; A polynomial is a list of terms.
;;; A term is a list: (coefficient monomial)
;;; A monomial is a sorted alist of (symbol . exponent) pairs.
;;;
;;; Example: 2*x*y + 3*z is:
;;;   ((2 ((x . 1) (y . 1))) (3 ((z . 1))))

;;; term-coeff : Term → Number
(define (term-coeff term) (car term))

;;; term-mono : Term → Monomial
(define (term-mono term) (cadr term))

;;; make-term : Number × Monomial → Term
(define (make-term coeff mono) (list coeff mono))

;;; ============================================================
;;; S-expression → Polynomial Conversion
;;; ============================================================

;;; sexpr->poly : S-expr → Poly
(define (sexpr->poly expr)
  (cond
    [(exact-number? expr)
     (if (= expr 0)
         '()
         (list (make-term expr '())))]

    [(symbol? expr)
     (list (make-term 1 (list (cons expr 1))))]

    [(and (pair? expr) (eq? (car expr) 'dv))
     (let ([dv-sym (string->symbol (string-append "#dv" (number->string (cadr expr))))])
       (list (make-term 1 (list (cons dv-sym 1)))))]

    [(and (pair? expr) (eq? (car expr) '+))
     (poly-add-all (map sexpr->poly (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '*))
     (poly-mul-all (map sexpr->poly (cdr expr)))]

    [(and (pair? expr) (eq? (car expr) '-))
     (if (= (length (cdr expr)) 1)
         (poly-negate (sexpr->poly (cadr expr)))
         (poly-sub (sexpr->poly (cadr expr))
                   (poly-add-all (map sexpr->poly (cddr expr)))))]

    [else (error 'sexpr->poly "not an arithmetic expression" expr)]))

;;; ============================================================
;;; Polynomial Operations
;;; ============================================================

(define (poly-add-all polys)
  (fold-left poly-add '() polys))

(define (poly-add p1 p2)
  (poly-simplify (append p1 p2)))

(define (poly-sub p1 p2)
  (poly-add p1 (poly-negate p2)))

(define (poly-negate p)
  (map (lambda (term)
         (make-term (- (term-coeff term)) (term-mono term)))
       p))

(define (poly-mul-all polys)
  (if (null? polys)
      (list (make-term 1 '()))
      (fold-left poly-mul (car polys) (cdr polys))))

(define (poly-mul p1 p2)
  (poly-simplify
   (apply append
          (map (lambda (t1)
                 (map (lambda (t2)
                        (term-mul t1 t2))
                      p2))
               p1))))

(define (term-mul t1 t2)
  (make-term (* (term-coeff t1) (term-coeff t2))
             (mono-mul (term-mono t1) (term-mono t2))))

;;; ============================================================
;;; Monomial Operations
;;; ============================================================

;;; mono-mul : Monomial × Monomial → Monomial
;;; Multiply monomials (add exponents of like variables).
(define (mono-mul m1 m2)
  (mono-merge m1 m2))

(define (mono-merge m1 m2)
  (cond
    [(null? m1) m2]
    [(null? m2) m1]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(string<? (symbol->string v1) (symbol->string v2))
          (cons (car m1) (mono-merge (cdr m1) m2))]
         [(string<? (symbol->string v2) (symbol->string v1))
          (cons (car m2) (mono-merge m1 (cdr m2)))]
         [else
          (cons (cons v1 (+ e1 e2)) (mono-merge (cdr m1) (cdr m2)))]))]))

;;; ============================================================
;;; Polynomial Simplification
;;; ============================================================

;;; poly-simplify : Poly → Poly
;;; Sort terms, combine like monomials, remove zeros.
(define (poly-simplify terms)
  (let* ([sorted (list-sort term<? terms)]
         [combined (combine-terms sorted)]
         [nonzero (filter (lambda (t) (not (= (term-coeff t) 0))) combined)])
    nonzero))

;;; term<? : Term × Term → Bool
(define (term<? t1 t2)
  (mono<? (term-mono t1) (term-mono t2)))

;;; mono<? : Monomial × Monomial → Bool
(define (mono<? m1 m2)
  (cond
    [(and (null? m1) (null? m2)) #f]
    [(null? m1) #t]
    [(null? m2) #f]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(string<? (symbol->string v1) (symbol->string v2)) #t]
         [(string<? (symbol->string v2) (symbol->string v1)) #f]
         [(< e1 e2) #t]
         [(> e1 e2) #f]
         [else (mono<? (cdr m1) (cdr m2))]))]))

;;; combine-terms : (Sorted Poly) → Poly
;;; Combine adjacent terms with same monomial.
(define (combine-terms terms)
  (if (null? terms)
      '()
      (let loop ([acc (list (car terms))] [rest (cdr terms)])
        (cond
          [(null? rest) (reverse acc)]
          [(equal? (term-mono (car acc)) (term-mono (car rest)))
           (let ([new-coeff (+ (term-coeff (car acc)) (term-coeff (car rest)))])
             (loop (cons (make-term new-coeff (term-mono (car acc))) (cdr acc))
                   (cdr rest)))]
          [else
           (loop (cons (car rest) acc) (cdr rest))]))))

;;; ============================================================
;;; Polynomial → S-expression Conversion
;;; ============================================================

;;; poly->sexpr : Poly → S-expr
(define (poly->sexpr p)
  (cond
    [(null? p) 0]
    [(null? (cdr p)) (term->sexpr (car p))]
    [else (cons '+ (map term->sexpr p))]))

;;; term->sexpr : Term → S-expr
(define (term->sexpr term)
  (let ([coeff (term-coeff term)]
        [mono (term-mono term)])
    (cond
      [(null? mono) coeff]
      [(= coeff 1) (mono->sexpr mono)]
      [(= coeff -1)
       (let ([m (mono->sexpr mono)])
         (if (and (pair? m) (eq? (car m) '*))
             `(* -1 ,@(cdr m))
             `(* -1 ,m)))]
      [else
       (let ([m (mono->sexpr mono)])
         (if (and (pair? m) (eq? (car m) '*))
             `(* ,coeff ,@(cdr m))
             `(* ,coeff ,m)))])))

;;; mono->sexpr : Monomial → S-expr
(define (mono->sexpr mono)
  (cond
    [(null? mono) 1]
    [(null? (cdr mono)) (var->sexpr (caar mono) (cdar mono))]
    [else (cons '* (map (lambda (ve) (var->sexpr (car ve) (cdr ve))) mono))]))

;;; var->sexpr : Symbol × Nat → S-expr
(define (var->sexpr v e)
  (let ([vstr (symbol->string v)])
    (cond
      [(and (> (string-length vstr) 3)
            (string=? "#dv" (substring vstr 0 3)))
       (let ([idx (string->number (substring vstr 3 (string-length vstr)))])
         (if (= e 1)
             `(dv ,idx)
             (cons '* (make-list e `(dv ,idx)))))]
      [(= e 1) v]
      [else (cons '* (make-list e v))])))

;;; make-list : Nat × Any → List
(define (make-list n x)
  (if (<= n 0) '() (cons x (make-list (- n 1) x))))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

;;; poly-canonicalize : S-expr → S-expr
(define (poly-canonicalize expr)
  (if (arithmetic-expr? expr)
      (let ([p (sexpr->poly expr)])
        (if (> (length p) *poly-canon-max-terms*)
            expr
            (poly->sexpr p)))
      expr))

;;; try-poly-canonicalize : S-expr → S-expr
(define (try-poly-canonicalize expr)
  (guard (ex [else expr])
    (poly-canonicalize expr)))
