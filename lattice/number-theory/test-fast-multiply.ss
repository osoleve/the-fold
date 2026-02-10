;;; lattice/number-theory/test-fast-multiply.ss — Tests for Fast Multiplication
;;;
;;; Tests for Karatsuba, Toom-Cook, and hybrid multiplication algorithms.
;;; Verifies correctness against native multiplication for various sizes.
;;;
;;; Run with: scheme --script lattice/number-theory/test-fast-multiply.ss

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'fast-multiply)

;;; ============================================================================
;;; Test Configuration
;;; ============================================================================

;; Lower thresholds for testing (so we actually test the algorithms)
(set-karatsuba-threshold! 4)
(set-toom3-threshold! 10)

(define test-base (expt 2 16))  ; Smaller base for readable test cases

;;; ============================================================================
;;; Part 1: Limb Conversion Tests
;;; ============================================================================

(test-group "Limb Conversion"

  (define-test "integer->limbs basic"
    (assert-equal '(0) (integer->limbs 0 10))
    (assert-equal '(5) (integer->limbs 5 10))
    (assert-equal '(3 2 1) (integer->limbs 123 10)))  ; 123 = 3 + 2*10 + 1*100

  (define-test "limbs->integer basic"
    (assert-equal 0 (limbs->integer '(0) 10))
    (assert-equal 5 (limbs->integer '(5) 10))
    (assert-equal 123 (limbs->integer '(3 2 1) 10)))

  (define-test "roundtrip integer->limbs->integer"
    (let ([base 1000])
      (assert-equal 0 (limbs->integer (integer->limbs 0 base) base))
      (assert-equal 1 (limbs->integer (integer->limbs 1 base) base))
      (assert-equal 999 (limbs->integer (integer->limbs 999 base) base))
      (assert-equal 1000 (limbs->integer (integer->limbs 1000 base) base))
      (assert-equal 123456789 (limbs->integer (integer->limbs 123456789 base) base))))

  (define-test "limbs-normalize removes trailing zeros"
    (assert-equal '(1 2 3) (limbs-normalize '(1 2 3 0 0 0)))
    (assert-equal '(0) (limbs-normalize '(0 0 0)))
    (assert-equal '(5) (limbs-normalize '(5 0))))

  (define-test "limbs-pad-to adds zeros"
    (assert-equal '(1 2 0 0) (limbs-pad-to '(1 2) 4))
    (assert-equal '(1 2 3) (limbs-pad-to '(1 2 3) 2))))

;;; ============================================================================
;;; Part 2: Limb Arithmetic Tests
;;; ============================================================================

(test-group "Limb Arithmetic"

  (define-test "limbs-add basic"
    (let ([base 10])
      (assert-equal '(5) (limbs-add '(2) '(3) base))
      (assert-equal '(0 1) (limbs-add '(7) '(3) base))  ; 7+3=10
      (assert-equal '(5 7 9) (limbs-add '(1 2 3) '(4 5 6) base))))

  (define-test "limbs-add with carry propagation"
    (let ([base 10])
      ;; 99 + 11 = 110
      (assert-equal '(0 1 1) (limbs-add '(9 9) '(1 1) base))))

  (define-test "limbs-sub basic"
    (let ([base 10])
      (assert-equal '(2) (limbs-sub '(5) '(3) base))
      (assert-equal '(1 2 3) (limbs-sub '(5 7 9) '(4 5 6) base))))

  (define-test "limbs-sub with borrow"
    (let ([base 10])
      ;; 100 - 1 = 99
      (assert-equal '(9 9) (limbs-sub '(0 0 1) '(1) base))))

  (define-test "limbs-shift multiplies by base^k"
    (assert-equal '(0 0 1 2 3) (limbs-shift '(1 2 3) 2))
    (assert-equal '(5) (limbs-shift '(5) 0))
    (assert-equal '(0) (limbs-shift '(0) 3)))  ; 0 stays 0

  (define-test "limb-scale multiplies by scalar"
    (let ([base 10])
      (assert-equal '(0 1) (limb-scale '(5) 2 base))  ; 5*2=10
      (assert-equal '(6 7 5) (limb-scale '(2 7) 8 base))))  ; 72*8=576 = 6 + 7*10 + 5*100

  (define-test "limbs-split divides at position"
    (assert-equal '((1 2) (3 4 5)) (limbs-split '(1 2 3 4 5) 2))
    (assert-equal '((1 2 3) (0)) (limbs-split '(1 2 3) 5))))

;;; ============================================================================
;;; Part 3: Schoolbook Multiplication Tests
;;; ============================================================================

(test-group "Schoolbook Multiplication"

  (define-test "schoolbook single limb"
    (let ([base 100])
      (assert-equal (integer->limbs (* 7 8) base)
                    (limbs-multiply-schoolbook '(7) '(8) base))))

  (define-test "schoolbook multi-limb"
    (let ([base 10])
      ;; 12 * 34 = 408
      (assert-equal '(8 0 4)
                    (limbs-multiply-schoolbook '(2 1) '(4 3) base))))

  (define-test "schoolbook matches native"
    (let ([base 1000])
      (let ([a 12345] [b 67890])
        (assert-equal (* a b)
                      (limbs->integer
                       (limbs-multiply-schoolbook
                        (integer->limbs a base)
                        (integer->limbs b base)
                        base)
                       base)))))

  (define-test "schoolbook with zero"
    (let ([base 10])
      (assert-equal '(0) (limbs-multiply-schoolbook '(0) '(5 6 7) base))
      (assert-equal '(0) (limbs-multiply-schoolbook '(1 2 3) '(0) base)))))

;;; ============================================================================
;;; Part 4: Karatsuba Multiplication Tests
;;; ============================================================================

(test-group "Karatsuba Multiplication"

  (define-test "karatsuba small inputs (uses schoolbook)"
    (let ([base 100])
      (assert-equal (integer->limbs (* 12 34) base)
                    (limbs-multiply-karatsuba
                     (integer->limbs 12 base)
                     (integer->limbs 34 base)
                     base))))

  (define-test "karatsuba medium inputs"
    ;; Force Karatsuba path with lower threshold
    (let ([base 10])
      ;; 1234 * 5678 = 7006652
      (let ([a-limbs (integer->limbs 1234 base)]
            [b-limbs (integer->limbs 5678 base)])
        (assert-equal (* 1234 5678)
                      (limbs->integer
                       (limbs-multiply-karatsuba a-limbs b-limbs base)
                       base)))))

  (define-test "karatsuba matches native for various sizes"
    (let ([base 100])
      (for-each
       (lambda (pair)
         (let ([a (car pair)] [b (cdr pair)])
           (assert-equal (* a b)
                         (limbs->integer
                          (limbs-multiply-karatsuba
                           (integer->limbs a base)
                           (integer->limbs b base)
                           base)
                          base))))
       '((123 . 456)
         (9999 . 9999)
         (12345678 . 87654321)
         (111111 . 222222)))))

  (define-test "karatsuba commutative"
    (let ([base 100])
      (let ([a 12345] [b 67890])
        (assert-equal (limbs->integer
                       (limbs-multiply-karatsuba
                        (integer->limbs a base)
                        (integer->limbs b base)
                        base)
                       base)
                      (limbs->integer
                       (limbs-multiply-karatsuba
                        (integer->limbs b base)
                        (integer->limbs a base)
                        base)
                       base))))))

;;; ============================================================================
;;; Part 5: Toom-3 Multiplication Tests
;;; ============================================================================

(test-group "Toom-3 Multiplication"

  (define-test "toom3 falls back to karatsuba for small inputs"
    (let ([base 100])
      (assert-equal (* 12 34)
                    (limbs->integer
                     (limbs-multiply-toom3
                      (integer->limbs 12 base)
                      (integer->limbs 34 base)
                      base)
                     base))))

  (define-test "toom3 medium inputs"
    (let ([base 10])
      (let ([a 123456789]
            [b 987654321])
        (assert-equal (* a b)
                      (limbs->integer
                       (limbs-multiply-toom3
                        (integer->limbs a base)
                        (integer->limbs b base)
                        base)
                       base)))))

  (define-test "toom3 matches native"
    (let ([base 100])
      (for-each
       (lambda (pair)
         (let ([a (car pair)] [b (cdr pair)])
           (assert-equal (* a b)
                         (limbs->integer
                          (limbs-multiply-toom3
                           (integer->limbs a base)
                           (integer->limbs b base)
                           base)
                          base))))
       '((12345 . 67890)
         (111111111 . 999999999)
         (123456789012 . 210987654321)))))

  (define-test "toom3 handles negative evaluation points (fold-zxug)"
    ;; Test case where p(-1) = a0 - a1 + a2 is negative
    ;; In base 10: 191 = (1 9 1), p(-1) = 1 - 9 + 1 = -7
    ;; 292 = (2 9 2), q(-1) = 2 - 9 + 2 = -5
    ;; r(-1) = 35 (positive, since -7 * -5 = 35)
    (let ([base 10])
      (assert-equal (* 191 292)
                    (limbs->integer
                     (limbs-multiply-toom3
                      (integer->limbs 191 base)
                      (integer->limbs 292 base)
                      base)
                     base))))

  (define-test "toom3 with mixed sign evaluation points"
    ;; One positive, one negative evaluation
    ;; 121 = (1 2 1), p(-1) = 1 - 2 + 1 = 0
    ;; 191 = (1 9 1), q(-1) = 1 - 9 + 1 = -7
    ;; r(-1) = 0
    (let ([base 10])
      (assert-equal (* 121 191)
                    (limbs->integer
                     (limbs-multiply-toom3
                      (integer->limbs 121 base)
                      (integer->limbs 191 base)
                      base)
                     base))))

  (define-test "toom3 larger negative evaluation case"
    ;; Larger numbers that force Toom-3 recursion
    (let ([base 10])
      (let ([a 19191919191919]  ; Pattern that gives negative p(-1)
            [b 29292929292929])
        (assert-equal (* a b)
                      (limbs->integer
                       (limbs-multiply-toom3
                        (integer->limbs a base)
                        (integer->limbs b base)
                        base)
                       base))))))

;;; ============================================================================
;;; Part 6: Hybrid Multiplier Tests
;;; ============================================================================

(test-group "Hybrid Multiplier"

  (define-test "hybrid selects appropriate algorithm"
    (let ([base 100])
      ;; Should work regardless of which algorithm is selected
      (for-each
       (lambda (pair)
         (let ([a (car pair)] [b (cdr pair)])
           (assert-equal (* a b)
                         (limbs->integer
                          (limbs-multiply
                           (integer->limbs a base)
                           (integer->limbs b base)
                           base)
                          base))))
       '((5 . 7)           ; Tiny - schoolbook
         (1234 . 5678)     ; Small - schoolbook/karatsuba boundary
         (123456 . 789012) ; Medium - karatsuba
         (123456789012 . 987654321098)))))  ; Large - may use toom3

  (define-test "fast-multiply integer interface"
    (assert-equal (* 12345 67890) (fast-multiply 12345 67890))
    (assert-equal (* 999999 888888) (fast-multiply 999999 888888)))

  (define-test "karatsuba-multiply forces karatsuba"
    (assert-equal (* 1234 5678) (karatsuba-multiply 1234 5678)))

  (define-test "toom3-multiply forces toom3"
    (assert-equal (* 12345678 87654321) (toom3-multiply 12345678 87654321))))

;;; ============================================================================
;;; Part 7: Squaring Tests
;;; ============================================================================

(test-group "Squaring"

  (define-test "fast-square basic"
    (assert-equal 0 (fast-square 0))
    (assert-equal 1 (fast-square 1))
    (assert-equal 100 (fast-square 10))
    (assert-equal 10000 (fast-square 100)))

  (define-test "fast-square large numbers"
    (assert-equal (* 12345 12345) (fast-square 12345))
    (assert-equal (* 999999 999999) (fast-square 999999)))

  (define-test "limbs-square-schoolbook matches multiplication (fold-zxui)"
    ;; Verify optimized schoolbook squaring produces same result as multiplication
    (let ([base 100])
         (for-each
          (lambda (n)
                  (let* ([limbs (integer->limbs n base)]
                         [sq-opt (limbs->integer (limbs-square-schoolbook limbs base) base)]
                         [sq-mul (limbs->integer (limbs-multiply-schoolbook limbs limbs base) base)])
                        (assert-equal sq-mul sq-opt
                                      (format "schoolbook squaring failed for ~a" n))))
          '(0 1 7 42 123 9999 12345 999999))))

  (define-test "limbs-square matches multiplication for various sizes (fold-zxui)"
    ;; Verify Karatsuba-style squaring matches general multiplication
    (let ([base 100])
         (for-each
          (lambda (n)
                  (let* ([limbs (integer->limbs n base)]
                         [sq-opt (limbs->integer (limbs-square limbs base) base)]
                         [sq-mul (limbs->integer (limbs-multiply limbs limbs base) base)])
                        (assert-equal sq-mul sq-opt
                                      (format "karatsuba squaring failed for ~a" n))))
          '(0 1 7 42 123 9999 12345 999999 123456789))))

  (define-test "fast-square matches fast-multiply (fold-zxui)"
    ;; Verify the top-level API produces correct results
    (for-each
     (lambda (n)
             (assert-equal (fast-multiply n n) (fast-square n)
                           (format "fast-square failed for ~a" n)))
     '(0 1 2 10 99 100 1000 12345 999999 123456789))))

;;; ============================================================================
;;; Part 8: Division Helper Tests
;;; ============================================================================

(test-group "Division Helper"

  (define-test "limbs-div-small exact division"
    (let ([base 10])
      ;; 246 / 2 = 123
      (assert-equal '(3 2 1) (limbs-div-small '(6 4 2) 2 base))
      ;; 369 / 3 = 123
      (assert-equal '(3 2 1) (limbs-div-small '(9 6 3) 3 base))))

  (define-test "limbs-div-small single limb"
    (let ([base 100])
      (assert-equal '(5) (limbs-div-small '(10) 2 base)))))

;;; ============================================================================
;;; Part 9: Edge Cases
;;; ============================================================================

(test-group "Edge Cases"

  (define-test "multiply by zero"
    (assert-equal 0 (fast-multiply 0 12345))
    (assert-equal 0 (fast-multiply 12345 0))
    (assert-equal 0 (karatsuba-multiply 0 999)))

  (define-test "multiply by one"
    (assert-equal 12345 (fast-multiply 1 12345))
    (assert-equal 12345 (fast-multiply 12345 1)))

  (define-test "multiply same number"
    (let ([n 12345])
      (assert-equal (* n n) (fast-multiply n n))
      (assert-equal (* n n) (karatsuba-multiply n n))))

  (define-test "asymmetric sizes"
    ;; One small, one large
    (assert-equal (* 7 123456789) (fast-multiply 7 123456789))
    (assert-equal (* 123456789 7) (fast-multiply 123456789 7)))

  (define-test "single-limb asymmetric optimization (fold-zxuh)"
    ;; Test the optimization that uses limb-scale for single-limb operands
    ;; This is O(n) instead of recursive Karatsuba/Toom-3
    (let ([base test-base]
          [large (integer->limbs (expt 2 256) test-base)]  ; Many limbs
          [small '(42)])                                     ; Single limb
      ;; Verify single-limb × multi-limb works correctly both ways
      (assert-equal (limbs-multiply-karatsuba small large base)
                    (limb-scale large 42 base))
      (assert-equal (limbs-multiply-karatsuba large small base)
                    (limb-scale large 42 base))
      ;; Verify through hybrid multiplier
      (assert-equal (* 42 (expt 2 256))
                    (limbs->integer (limbs-multiply small large base) base))))

  (define-test "asymmetric below-threshold optimization (fold-zxuh)"
    ;; Even when one operand is large and the other is moderately small
    ;; (below threshold but multi-limb), we should use schoolbook
    (let ([base test-base]
          [large (integer->limbs (expt 2 128) test-base)]
          [medium (integer->limbs 12345 test-base)])  ; 2-3 limbs, below threshold
      ;; Should still produce correct result via schoolbook fallback
      (assert-equal (* (expt 2 128) 12345)
                    (limbs->integer (limbs-multiply-karatsuba large medium base) base)))))

;;; ============================================================================
;;; Part 10: Property Tests
;;; ============================================================================

(test-group "Properties"

  (define-test "commutativity"
    (let ([a 123456] [b 789012])
      (assert-equal (fast-multiply a b) (fast-multiply b a))
      (assert-equal (karatsuba-multiply a b) (karatsuba-multiply b a))))

  (define-test "associativity via multiplication sequence"
    ;; (a * b) * c = a * (b * c)
    (let ([a 123] [b 456] [c 789])
      (assert-equal (fast-multiply (fast-multiply a b) c)
                    (fast-multiply a (fast-multiply b c)))))

  (define-test "distributivity"
    ;; a * (b + c) = a*b + a*c
    (let ([a 100] [b 23] [c 45])
      (assert-equal (fast-multiply a (+ b c))
                    (+ (fast-multiply a b) (fast-multiply a c))))))

;;; ============================================================================
;;; Part 11: Configuration Tests
;;; ============================================================================

(test-group "Configuration"

  (define-test "get-multiply-thresholds returns current values"
    (let ([thresholds (get-multiply-thresholds)])
      (assert-equal 2 (length thresholds))
      ;; Values should be the ones we set at top of file
      (assert-equal 4 (car thresholds))
      (assert-equal 10 (cadr thresholds))))

  (define-test "threshold changes affect behavior"
    ;; Save current
    (let ([old-thresholds (get-multiply-thresholds)])
      ;; Change to force schoolbook for everything
      (set-karatsuba-threshold! 1000)
      (set-toom3-threshold! 2000)
      ;; Result should still be correct
      (assert-equal (* 12345 67890) (fast-multiply 12345 67890))
      ;; Restore
      (set-karatsuba-threshold! (car old-thresholds))
      (set-toom3-threshold! (cadr old-thresholds)))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
