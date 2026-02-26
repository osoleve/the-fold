;;; lattice/algebra/test-multivariate-groebner-properties.ss — QuickCheck properties for multivariate and Groebner primitives

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'groebner)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define F Q-field)
(define vars '(x y))
(define ord (make-ordering 'grlex vars))

(define gen-c
  (gen-int-range -4 4))

(define gen-six
  (gen-bind gen-c
    (lambda (a)
      (gen-bind gen-c
        (lambda (b)
          (gen-bind gen-c
            (lambda (c)
              (gen-bind gen-c
                (lambda (d)
                  (gen-bind gen-c
                    (lambda (e)
                      (gen-map (lambda (f) (list a b c d e f)) gen-c))))))))))))

(define gen-exp
  (gen-int-range 0 4))

(define (lin-poly a b c)
  (make-mpoly F vars ord
    (list (cons a '((x . 1)))
          (cons b '((y . 1)))
          (cons c '()))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group multivariate-groebner-properties

  (define-property "mpoly-add is commutative for linear polynomials"
    gen-six
    (lambda (vals)
      (let* ([a (list-ref vals 0)]
             [b (list-ref vals 1)]
             [c (list-ref vals 2)]
             [d (list-ref vals 3)]
             [e (list-ref vals 4)]
             [f (list-ref vals 5)]
             [p (lin-poly a b c)]
             [q (lin-poly d e f)])
        (mpoly-equal? (mpoly-add p q)
                      (mpoly-add q p))))
    'tests 220)

  (define-property "mpoly multiplication distributes over addition"
    (gen-bind gen-six
      (lambda (vals1)
        (gen-map (lambda (vals2) (list vals1 vals2)) gen-six)))
    (lambda (args)
      (let* ([v1 (car args)]
             [v2 (cadr args)]
             [p (lin-poly (list-ref v1 0) (list-ref v1 1) (list-ref v1 2))]
             [q (lin-poly (list-ref v1 3) (list-ref v1 4) (list-ref v1 5))]
             [r (lin-poly (list-ref v2 0) (list-ref v2 1) (list-ref v2 2))]
             [lhs (mpoly-mul p (mpoly-add q r))]
             [rhs (mpoly-add (mpoly-mul p q)
                             (mpoly-mul p r))])
        (mpoly-equal? lhs rhs)))
    'tests 170)

  (define-property "monomial degree is additive under multiplication"
    (gen-bind gen-exp
      (lambda (ax)
        (gen-bind gen-exp
          (lambda (ay)
            (gen-bind gen-exp
              (lambda (bx)
                (gen-map (lambda (by) (list ax ay bx by)) gen-exp)))))))
    (lambda (args)
      (let* ([ax (car args)]
             [ay (cadr args)]
             [bx (caddr args)]
             [by (cadddr args)]
             [m1 (make-monomial (list (cons 'x ax) (cons 'y ay)))]
             [m2 (make-monomial (list (cons 'x bx) (cons 'y by)))]
             [prod (mono-mul m1 m2)])
        (= (mono-degree prod)
           (+ (mono-degree m1) (mono-degree m2)))))
    'tests 240)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
