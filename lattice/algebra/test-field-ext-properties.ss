;;; lattice/algebra/test-field-ext-properties.ss — QuickCheck properties for field extensions

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'field-ext)
(require 'algebra/polynomial)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define Q-sqrt2 (make-Q-sqrt 2))
(define Q-sqrt-i (make-Q-i))
(define conj (make-conjugation Q-sqrt2))
(define add-op (field-add-op Q-sqrt2))
(define mul-op (field-mul-op Q-sqrt2))
(define min-poly-x2-2 (make-polynomial Q-field (list -2 0 1)))
(define ext-a (make-algebraic-extension Q-field min-poly-x2-2 'a))
(define ext-a-elem (make-alg-ext-element (list 1 1) min-poly-x2-2 Q-field 'a))

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

  (define-property "field extension constructors and utilities are coherent on representative values"
    (gen-pure #t)
    (lambda (_)
      (let* ([autos (galois-group-quadratic Q-sqrt2)]
             [gen (ext-generator ext-a)]
             [emb (ext-embed ext-a 5)]
             [sf (splitting-field-quadratic Q-field min-poly-x2-2)])
        (and (field? Q-sqrt-i)
             (alg-ext-element? ext-a-elem)
             (= (length (alg-ext-coeffs ext-a-elem)) 2)
             (= (poly-degree (alg-ext-min-poly ext-a-elem)) 2)
             (equal? (alg-ext-base-field ext-a-elem) Q-field)
             (eq? (alg-ext-symbol ext-a-elem) 'a)
             (field? ext-a)
             (= (ext-degree ext-a) 2)
             (alg-ext-element? gen)
             (alg-ext-element? emb)
             (= (length autos) 2)
             (alg-ext-element? ((car autos) ext-a-elem))
             (alg-ext-element? ((cadr autos) ext-a-elem))
             (field? sf)
             (verify-minimal-poly ext-a ext-a-elem)
             (string? (alg-ext->string ext-a-elem)))))
    'tests 8)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
