;;; lattice/number-theory/test-fast-multiply-properties.ss — QuickCheck properties for fast multiplication

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'fast-multiply)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (mul-via-limbs mul-fn a b base)
  (limbs->integer
    (mul-fn (integer->limbs a base)
            (integer->limbs b base)
            base)
    base))

(define (all-satisfy? pred xs)
  (or (null? xs)
      (and (pred (car xs))
           (all-satisfy? pred (cdr xs)))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-base
  (gen-int-range 2 200))

(define gen-roundtrip-args
  (gen-bind gen-base
    (lambda (base)
      (gen-map (lambda (n) (list n base))
               (gen-int-range 0 2000000)))))

(define gen-mul-args
  (gen-bind gen-base
    (lambda (base)
      (gen-bind (gen-int-range 0 200000)
        (lambda (a)
          (gen-map (lambda (b) (list a b base))
                   (gen-int-range 0 200000)))))))

(define gen-toom3-args
  (gen-bind (gen-int-range 100000 999999)
    (lambda (a)
      (gen-map (lambda (b) (list a b))
               (gen-int-range 100000 999999)))))

(define gen-square-args
  (gen-bind gen-base
    (lambda (base)
      (gen-map (lambda (n) (list n base))
               (gen-int-range 0 200000)))))

;;; ============================================================================
;;; Representation properties
;;; ============================================================================

(test-group limb-representation-properties

  (define-property "integer->limbs->integer roundtrip"
    gen-roundtrip-args
    (lambda (args)
      (let ([n (car args)] [base (cadr args)])
        (= n (limbs->integer (integer->limbs n base) base))))
    'tests 300)

  (define-property "limbs-normalize is idempotent"
    gen-roundtrip-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [limbs (integer->limbs n base)]
             [padded (append limbs '(0 0 0))]
             [once (limbs-normalize padded)]
             [twice (limbs-normalize once)])
        (equal? once twice)))
    'tests 250)

  (define-property "limbs->integer of normalized limbs is unchanged"
    gen-roundtrip-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [limbs (integer->limbs n base)]
             [padded (append limbs '(0 0 0 0))]
             [norm (limbs-normalize padded)])
        (= (limbs->integer padded base)
           (limbs->integer norm base))))
    'tests 250)
)

;;; ============================================================================
;;; Multiplication correctness properties
;;; ============================================================================

(test-group multiplication-properties

  (define-property "schoolbook multiplication agrees with native *"
    gen-mul-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [base (caddr args)])
        (= (* a b)
           (mul-via-limbs limbs-multiply-schoolbook a b base))))
    'tests 250)

  (define-property "Karatsuba multiplication agrees with native *"
    gen-mul-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [base (caddr args)])
        (= (* a b)
           (mul-via-limbs limbs-multiply-karatsuba a b base))))
    'tests 250)

  (define-property "hybrid limbs-multiply agrees with native *"
    gen-mul-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [base (caddr args)])
        (= (* a b)
           (mul-via-limbs limbs-multiply a b base))))
    'tests 250)

  (define-property "Toom-3 multiplication agrees with native *"
    gen-toom3-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)])
        (= (* a b)
           (mul-via-limbs limbs-multiply-toom3 a b 10))))
    'tests 150
    'max-size 12)

  (define-property "limbs-multiply is commutative"
    gen-mul-args
    (lambda (args)
      (let ([a (car args)] [b (cadr args)] [base (caddr args)])
        (= (mul-via-limbs limbs-multiply a b base)
           (mul-via-limbs limbs-multiply b a base))))
    'tests 200)
)

;;; ============================================================================
;;; Integer interface and squaring
;;; ============================================================================

(test-group integer-interface-properties

  (define-property "fast-multiply agrees with native *"
    (gen-pair (gen-int-range 0 10000000) (gen-int-range 0 10000000))
    (lambda (pair)
      (let ([a (car pair)] [b (cdr pair)])
        (= (* a b)
           (fast-multiply a b))))
    'tests 220)

  (define-property "karatsuba-multiply agrees with native *"
    (gen-pair (gen-int-range 0 1000000) (gen-int-range 0 1000000))
    (lambda (pair)
      (let ([a (car pair)] [b (cdr pair)])
        (= (* a b)
           (karatsuba-multiply a b))))
    'tests 220)

  (define-property "toom3-multiply agrees with native *"
    (gen-pair (gen-int-range 0 1000000) (gen-int-range 0 1000000))
    (lambda (pair)
      (let ([a (car pair)] [b (cdr pair)])
        (= (* a b)
           (toom3-multiply a b))))
    'tests 180
    'max-size 10)

  (define-property "limbs-square matches limbs-multiply x x"
    gen-square-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [x (integer->limbs n base)])
        (equal? (limbs-square x base)
                (limbs-multiply x x base))))
    'tests 220)

  (define-property "fast-square agrees with native squaring"
    (gen-int-range 0 10000000)
    (lambda (n)
      (= (* n n)
         (fast-square n)))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
