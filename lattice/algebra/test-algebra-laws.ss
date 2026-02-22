;;; lattice/algebra/test-algebra-laws.ss — QuickCheck property tests for algebraic structures

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'qc-laws)
(require 'field)
(require 'algebra/polynomial)
(require 'group)

;;; ============================================================================
;;; Polynomial generators
;;; ============================================================================

;; Generate polynomials over Q (rationals) with small coefficients
(define gen-q-coeffs
  (gen-bind (gen-int-range 0 5)
    (lambda (deg)
      (gen-list-of (+ deg 1)
        (gen-map (lambda (n) (if (= n 0) 0 n))
                 (gen-int-range -5 5))))))

(define gen-q-poly
  (gen-map (lambda (coeffs) (make-polynomial Q-field coeffs))
           gen-q-coeffs))

;; Polynomials over Z_7 (small finite field)
(define Z7 (make-field-zp 7))

(define gen-z7-coeffs
  (gen-bind (gen-int-range 0 4)
    (lambda (deg)
      (gen-list-of (+ deg 1) (gen-int-range 0 6)))))

(define gen-z7-poly
  (gen-map (lambda (coeffs) (make-polynomial Z7 coeffs))
           gen-z7-coeffs))

;;; ============================================================================
;;; Polynomial equality
;;; ============================================================================

(define (poly-equal? p1 p2)
  (equal? (poly-coeffs p1) (poly-coeffs p2)))

;;; ============================================================================
;;; Polynomial ring laws over Q
;;; ============================================================================

(test-group poly-q-ring-laws

  (define-test "Q[x] additive abelian group"
    (assert-laws "Q[x] add"
      (check-abelian-group-laws
        gen-q-poly poly-add (poly-zero-over Q-field) poly-neg poly-equal?)))

  (define-test "Q[x] multiplicative monoid"
    (assert-laws "Q[x] mul monoid"
      (check-monoid-laws
        gen-q-poly poly-mul (poly-one-over Q-field) poly-equal?)))

  (define-property "Q[x] left distributivity"
    (gen-bind gen-q-poly (lambda (a)
      (gen-bind gen-q-poly (lambda (b)
        (gen-map (lambda (c) (list a b c)) gen-q-poly)))))
    (lambda (abc)
      (let ([a (car abc)] [b (cadr abc)] [c (caddr abc)])
        (poly-equal? (poly-mul a (poly-add b c))
                     (poly-add (poly-mul a b) (poly-mul a c)))))
    'tests 200)

  (define-property "Q[x] right distributivity"
    (gen-bind gen-q-poly (lambda (a)
      (gen-bind gen-q-poly (lambda (b)
        (gen-map (lambda (c) (list a b c)) gen-q-poly)))))
    (lambda (abc)
      (let ([a (car abc)] [b (cadr abc)] [c (caddr abc)])
        (poly-equal? (poly-mul (poly-add a b) c)
                     (poly-add (poly-mul a c) (poly-mul b c)))))
    'tests 200)
)

;;; ============================================================================
;;; Polynomial ring laws over Z_7
;;; ============================================================================

(test-group poly-z7-ring-laws

  (define-test "Z7[x] additive abelian group"
    (assert-laws "Z7[x] add"
      (check-abelian-group-laws
        gen-z7-poly poly-add (poly-zero-over Z7) poly-neg poly-equal?)))

  (define-test "Z7[x] multiplicative monoid"
    (assert-laws "Z7[x] mul monoid"
      (check-monoid-laws
        gen-z7-poly poly-mul (poly-one-over Z7) poly-equal?)))
)

;;; ============================================================================
;;; Polynomial degree properties
;;; ============================================================================

(test-group poly-degree-properties

  (define-property "degree of sum <= max of degrees"
    (gen-pair gen-q-poly gen-q-poly)
    (lambda (pair)
      (let* ([p (car pair)]
             [q (cdr pair)]
             [sum-deg (poly-degree (poly-add p q))]
             [max-deg (max (poly-degree p) (poly-degree q))])
        (<= sum-deg max-deg)))
    'tests 200)

  (define-property "degree of product <= sum of degrees (for non-zero)"
    (gen-pair gen-q-poly gen-q-poly)
    (lambda (pair)
      (let* ([p (car pair)]
             [q (cdr pair)]
             [prod (poly-mul p q)])
        (if (or (poly-zero? p) (poly-zero? q))
            #t  ; zero polynomial handled separately
            (<= (poly-degree prod)
                (+ (poly-degree p) (poly-degree q))))))
    'tests 200)

  (define-property "p + (-p) = zero"
    gen-q-poly
    (lambda (p)
      (poly-zero? (poly-add p (poly-neg p))))
    'tests 200)
)

;;; ============================================================================
;;; Polynomial evaluation properties
;;; ============================================================================

(test-group poly-eval-properties

  (define-property "eval at 0 gives constant term"
    gen-q-poly
    (lambda (p)
      (let* ([coeffs (poly-coeffs p)]
             [constant (if (null? coeffs) 0 (car coeffs))])
        (= (poly-eval p 0) constant)))
    'tests 200)

  (define-property "eval (p + q) at x = eval p at x + eval q at x"
    (gen-bind gen-q-poly (lambda (p)
      (gen-bind gen-q-poly (lambda (q)
        (gen-map (lambda (x) (list p q x))
                 (gen-int-range -3 3))))))
    (lambda (args)
      (let ([p (car args)] [q (cadr args)] [x (caddr args)])
        (= (poly-eval (poly-add p q) x)
           (+ (poly-eval p x) (poly-eval q x)))))
    'tests 200)

  (define-property "eval (p * q) at x = (eval p at x) * (eval q at x)"
    (gen-bind gen-q-poly (lambda (p)
      (gen-bind gen-q-poly (lambda (q)
        (gen-map (lambda (x) (list p q x))
                 (gen-int-range -3 3))))))
    (lambda (args)
      (let ([p (car args)] [q (cadr args)] [x (caddr args)])
        (= (poly-eval (poly-mul p q) x)
           (* (poly-eval p x) (poly-eval q x)))))
    'tests 200)
)

;;; ============================================================================
;;; Group module laws (if available)
;;; ============================================================================

(test-group integer-additive-group

  ;; Integers under addition form an abelian group
  (define-test "Z additive abelian group"
    (assert-laws "Z add"
      (check-abelian-group-laws
        gen-int + 0 (lambda (x) (- 0 x)) =)))
)

(test-group integer-multiplicative-monoid

  ;; Integers under multiplication form a monoid (not a group!)
  (define-test "Z multiplicative monoid"
    (assert-laws "Z mul monoid"
      (check-monoid-laws
        gen-int * 1 =)))
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
