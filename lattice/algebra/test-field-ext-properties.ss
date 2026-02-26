;;; lattice/algebra/test-field-ext-properties.ss — QuickCheck properties for field extensions

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'field-ext)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define Q-sqrt2 (make-Q-sqrt 2))
(define conj (make-conjugation Q-sqrt2))
(define add-op (field-add-op Q-sqrt2))
(define mul-op (field-mul-op Q-sqrt2))

(define gen-coeff
  (gen-int-range -20 20))

(define gen-pair
  (gen-bind gen-coeff
    (lambda (a)
      (gen-map (lambda (b) (cons a b)) gen-coeff))))

(define (mk a b)
  (Q-sqrt-element Q-sqrt2 a b))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group field-ext-properties

  (define-property "quadratic conjugation is an involution"
    gen-pair
    (lambda (ab)
      (let* ([a (car ab)]
             [b (cdr ab)]
             [x (mk a b)]
             [y (conj (conj x))])
        (and (= (Q-sqrt-real-part x) (Q-sqrt-real-part y))
             (= (Q-sqrt-sqrt-part x) (Q-sqrt-sqrt-part y)))))
    'tests 220)

  (define-property "norm is multiplicative in Q(sqrt(2))"
    (gen-bind gen-pair
      (lambda (ab)
        (gen-map (lambda (cd) (list ab cd)) gen-pair)))
    (lambda (args)
      (let* ([ab (car args)]
             [cd (cadr args)]
             [x (mk (car ab) (cdr ab))]
             [y (mk (car cd) (cdr cd))]
             [xy (mul-op x y)])
        (= (Q-sqrt-norm xy 2)
           (* (Q-sqrt-norm x 2)
              (Q-sqrt-norm y 2)))))
    'tests 200)

  (define-property "trace is additive in Q(sqrt(2))"
    (gen-bind gen-pair
      (lambda (ab)
        (gen-map (lambda (cd) (list ab cd)) gen-pair)))
    (lambda (args)
      (let* ([ab (car args)]
             [cd (cadr args)]
             [x (mk (car ab) (cdr ab))]
             [y (mk (car cd) (cdr cd))]
             [sum (add-op x y)])
        (= (Q-sqrt-trace sum)
           (+ (Q-sqrt-trace x)
              (Q-sqrt-trace y)))))
    'tests 200)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
