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

(define (signed->integer sl base)
  (* (car sl) (limbs->integer (cdr sl) base)))

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
;;; Limb operation coverage gaps
;;; ============================================================================

(test-group limb-ops-coverage

  (define-property "limbs-pad-to preserves prefix and reaches target length"
    gen-roundtrip-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [limbs (integer->limbs n base)]
             [target (+ (length limbs) 3)]
             [padded (limbs-pad-to limbs target)])
        (and (= target (length padded))
             (equal? limbs (take (length limbs) padded)))))
    'tests 220)

  (define-property "limbs-split reconstructs original integer"
    (gen-bind gen-roundtrip-args
      (lambda (args)
        (gen-map (lambda (k) (list (car args) (cadr args) k))
                 (gen-int-range 0 12))))
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [k (caddr args)]
             [limbs (integer->limbs n base)]
             [parts (limbs-split limbs k)]
             [lo (car parts)]
             [hi (cadr parts)]
             [recon (limbs-add lo (limbs-shift hi k) base)])
        (= n (limbs->integer recon base))))
    'tests 220)

  (define-property "limbs-sub inverts limbs-add when subtracting second addend"
    gen-mul-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [base (caddr args)]
             [la (integer->limbs a base)]
             [lb (integer->limbs b base)]
             [sum (limbs-add la lb base)]
             [back (limbs-sub sum lb base)])
        (equal? (limbs-normalize la) back)))
    'tests 220)

  (define-property "limb-scale agrees with integer multiplication"
    (gen-bind gen-base
      (lambda (base)
        (gen-bind (gen-int-range 0 200000)
          (lambda (n)
            (gen-map (lambda (k) (list n k base))
                     (gen-int-range 0 50))))))
    (lambda (args)
      (let* ([n (car args)]
             [k (cadr args)]
             [base (caddr args)]
             [scaled (limb-scale (integer->limbs n base) k base)])
        (= (* n k) (limbs->integer scaled base))))
    'tests 220)

  (define-property "limbs-split3 reconstructs original integer"
    (gen-bind gen-roundtrip-args
      (lambda (args)
        (gen-map (lambda (m) (list (car args) (cadr args) m))
                 (gen-int-range 1 4))))
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [m (caddr args)]
             [limbs (integer->limbs n base)]
             [parts (limbs-split3 limbs m)]
             [a0 (car parts)]
             [a1 (cadr parts)]
             [a2 (caddr parts)]
             [recon (limbs-add
                     (limbs-add a0 (limbs-shift a1 m) base)
                     (limbs-shift a2 (* 2 m))
                     base)])
        (= n (limbs->integer recon base))))
    'tests 200)

  (define-property "limbs-compare matches integer ordering"
    gen-mul-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [base (caddr args)]
             [cmp (limbs-compare (integer->limbs a base)
                                 (integer->limbs b base))])
        (cond
         [(< a b) (= cmp -1)]
         [(> a b) (= cmp 1)]
         [else (= cmp 0)])))
    'tests 220)

  (define-property "signed-add cancels equal opposite magnitudes"
    gen-roundtrip-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [mag (integer->limbs n base)]
             [sa (cons 1 mag)]
             [sb (cons -1 mag)]
             [sum (signed-add sa sb base)])
        (= 0 (signed->integer sum base))))
    'tests 220)

  (define-property "limbs-div-small exact division roundtrip"
    (gen-bind gen-base
      (lambda (base)
        (gen-bind (gen-int-range 0 200000)
          (lambda (q)
            (gen-map (lambda (d) (list q d base))
                     (gen-int-range 1 25))))))
    (lambda (args)
      (let* ([q (car args)]
             [d (cadr args)]
             [base (caddr args)]
             [n (* q d)]
             [limbs (integer->limbs n base)]
             [quot-limbs (limbs-div-small limbs d base)])
        (= q (limbs->integer quot-limbs base))))
    'tests 220)

  (define-property "limbs-square-schoolbook matches schoolbook multiplication"
    gen-square-args
    (lambda (args)
      (let* ([n (car args)]
             [base (cadr args)]
             [x (integer->limbs n base)])
        (equal? (limbs-square-schoolbook x base)
                (limbs-multiply-schoolbook x x base))))
    'tests 220)

  (define-test "threshold setters/getter roundtrip"
    (let* ([old (get-multiply-thresholds)]
           [old-k (car old)]
           [old-t (cadr old)])
      (set-karatsuba-threshold! 9)
      (set-toom3-threshold! 27)
      (let ([now (get-multiply-thresholds)])
        (assert-equal 9 (car now))
        (assert-equal 27 (cadr now)))
      ;; Restore original values to avoid affecting other tests.
      (set-karatsuba-threshold! old-k)
      (set-toom3-threshold! old-t)))
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
