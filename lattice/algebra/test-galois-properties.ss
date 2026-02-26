;;; lattice/algebra/test-galois-properties.ss — QuickCheck properties for finite fields

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'galois)

;;; ============================================================================
;;; Setup
;;; ============================================================================

(define Fp (gf-prime 7))
(define F2 (make-gf2-8-aes))

(define gen-z7
  (gen-int-range 0 6))

(define gen-z7-nonzero
  (gen-int-range 1 6))

(define gen-byte
  (gen-int-range 0 255))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group galois-properties

  (define-property "GF(7) addition matches modulo arithmetic"
    (gen-bind gen-z7
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-z7)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)])
        (= (field-add Fp a b) (modulo (+ a b) 7))))
    'tests 260)

  (define-property "GF(7) multiplication matches modulo arithmetic"
    (gen-bind gen-z7
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-z7)))
    (lambda (ab)
      (let ([a (car ab)]
            [b (cdr ab)])
        (= (field-mul Fp a b) (modulo (* a b) 7))))
    'tests 240)

  (define-property "GF(7) nonzero elements have multiplicative inverse"
    gen-z7-nonzero
    (lambda (a)
      (= 1 (field-mul Fp a (field-inv Fp a))))
    'tests 220)

  (define-property "GF(2^8) addition is XOR"
    (gen-bind gen-byte
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-byte)))
    (lambda (ab)
      (let* ([a (car ab)]
             [b (cdr ab)]
             [ea (gf2n-from-int F2 a)]
             [eb (gf2n-from-int F2 b)]
             [sum (field-add F2 ea eb)])
        (= (gf2n-to-int sum) (bitwise-xor a b))))
    'tests 240)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
