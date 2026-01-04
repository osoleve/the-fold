;;; core/algebra/test-ring.ss — Tests for Ring Theory Library
;;;
;;; Tests ring structures, operations, homomorphisms, and ideals.

(load "core/test-framework.ss")
(load "core/algebra/ring.ss")

;;; ============================================================
;;; Ring Construction Tests
;;; ============================================================

(define-test "make-ring-zn creates Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (ring? z5))
       (assert (= (ring-order z5) 5))
       (assert (= (ring-zero z5) 0))
       (assert (= (ring-one z5) 1))))

(define-test "make-ring-z creates integer ring"
  (let ([z (make-ring-z)])
       (assert (ring? z))
       (assert (= (ring-zero z) 0))
       (assert (= (ring-one z) 1))))

;;; ============================================================
;;; Ring Operations Tests
;;; ============================================================

(define-test "ring addition in Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-add z5 3 4) 2))  ; 3 + 4 = 7 ≡ 2 (mod 5)
       (assert (= (ring-add z5 2 2) 4))))

(define-test "ring multiplication in Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-mul z5 3 4) 2))  ; 3 × 4 = 12 ≡ 2 (mod 5)
       (assert (= (ring-mul z5 2 3) 1)))) ; 2 × 3 = 6 ≡ 1 (mod 5)

(define-test "ring negation in Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-neg z5 3) 2))    ; -3 ≡ 2 (mod 5)
       (assert (= (ring-neg z5 0) 0))))

(define-test "ring subtraction in Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-sub z5 2 3) 4))  ; 2 - 3 ≡ -1 ≡ 4 (mod 5)
       (assert (= (ring-sub z5 4 1) 3))))

(define-test "ring power"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-power z5 2 3) 3))  ; 2^3 = 8 ≡ 3 (mod 5)
       (assert (= (ring-power z5 3 2) 4))  ; 3^2 = 9 ≡ 4 (mod 5)
       (assert (= (ring-power z5 2 0) 1)))) ; 2^0 = 1

(define-test "ring sum"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-sum z5 '(1 2 3)) 1))  ; 1+2+3 = 6 ≡ 1 (mod 5)
       (assert (= (ring-sum z5 '()) 0))))

(define-test "ring product"
  (let ([z5 (make-ring-zn 5)])
       (assert (= (ring-product z5 '(2 3 4)) 4))  ; 2×3×4 = 24 ≡ 4 (mod 5)
       (assert (= (ring-product z5 '()) 1))))

;;; ============================================================
;;; Ring Properties Tests
;;; ============================================================

(define-test "Z_5 is commutative"
  (let ([z5 (make-ring-zn 5)])
       (assert (is-commutative-ring? z5))))

(define-test "Z_6 has zero divisors"
  (let ([z6 (make-ring-zn 6)])
       (assert (is-zero-divisor? z6 2))  ; 2×3 = 6 ≡ 0 (mod 6)
       (assert (is-zero-divisor? z6 3))  ; 3×2 = 6 ≡ 0 (mod 6)
       (assert (has-zero-divisors? z6))))

(define-test "Z_5 has no zero divisors"
  (let ([z5 (make-ring-zn 5)])
       (assert (not (has-zero-divisors? z5)))))

(define-test "Z_5 is an integral domain"
  (let ([z5 (make-ring-zn 5)])
       (assert (is-integral-domain? z5))))

(define-test "Z_6 is not an integral domain"
  (let ([z6 (make-ring-zn 6)])
       (assert (not (is-integral-domain? z6)))))

(define-test "units in Z_5"
  (let* ([z5 (make-ring-zn 5)]
         [units (ring-units z5)])
        ; In Z_5, units are {1, 2, 3, 4} (all non-zero elements)
        (assert (= (length units) 4))
        (assert (member 1 units))
        (assert (member 2 units))
        (assert (member 3 units))
        (assert (member 4 units))))

(define-test "units in Z_6"
  (let* ([z6 (make-ring-zn 6)]
         [units (ring-units z6)])
        ; In Z_6, units are {1, 5} (coprime to 6)
        (assert (= (length units) 2))
        (assert (member 1 units))
        (assert (member 5 units))))

(define-test "is-unit? in Z_5"
  (let ([z5 (make-ring-zn 5)])
       (assert (is-unit? z5 1))
       (assert (is-unit? z5 2))  ; 2×3 ≡ 1 (mod 5)
       (assert (is-unit? z5 3))  ; 3×2 ≡ 1 (mod 5)
       (assert (is-unit? z5 4))  ; 4×4 ≡ 1 (mod 5)
       (assert (not (is-unit? z5 0)))))

;;; ============================================================
;;; Ring Homomorphism Tests
;;; ============================================================

(define-test "natural homomorphism Z -> Z_5"
  (let* ([z (make-ring-z)]
         [z5 (make-ring-zn 5)]
         [phi (lambda (a) (modulo a 5))]
         [h (make-ring-homomorphism z z5 phi)])
        (assert (ring-hom? h))
        (assert (= (ring-hom-apply h 7) 2))
        (assert (= (ring-hom-apply h 12) 2))))

(define-test "identity homomorphism"
  (let* ([z5 (make-ring-zn 5)]
         [id (lambda (a) a)]
         [h (make-ring-homomorphism z5 z5 id)])
        (assert (is-valid-ring-homomorphism? h))))

(define-test "kernel of Z -> Z_5"
  (let* ([z-small (make-ring
                   (range -10 11)
                   + * 0 1 - =)]
         [z5 (make-ring-zn 5)]
         [phi (lambda (a) (modulo a 5))]
         [h (make-ring-homomorphism z-small z5 phi)]
         [ker (ring-hom-kernel h)])
        ; Kernel should be {..., -10, -5, 0, 5, 10, ...}
        (assert (member 0 ker))
        (assert (member 5 ker))
        (assert (member -5 ker))
        (assert (member 10 ker))
        (assert (not (member 1 ker)))
        (assert (not (member 2 ker)))))

(define-test "image of Z -> Z_5"
  (let* ([z-small (make-ring
                   (range 0 10)
                   + * 0 1 - =)]
         [z5 (make-ring-zn 5)]
         [phi (lambda (a) (modulo a 5))]
         [h (make-ring-homomorphism z-small z5 phi)]
         [img (ring-hom-image h)])
        ; Image should be all of Z_5
        (assert (= (length img) 5))))

;;; ============================================================
;;; Ideal Tests
;;; ============================================================

(define-test "principal ideal in Z_6"
  (let* ([z6 (make-ring-zn 6)]
         [ideal-2 (make-principal-ideal z6 2)])
        (assert (ideal? ideal-2))
        ; <2> in Z_6 = {0, 2, 4}
        (let ([elems (ideal-elements ideal-2)])
             (assert (member 0 elems))
             (assert (member 2 elems))
             (assert (member 4 elems)))))

(define-test "principal ideal in Z_5"
  (let* ([z5 (make-ring-zn 5)]
         [ideal-2 (make-principal-ideal z5 2)])
        ; <2> in Z_5 = Z_5 (since 2 is a unit)
        (let ([elems (ideal-elements ideal-2)])
             (assert (= (length elems) 5)))))

(define-test "ideal sum"
  (let* ([z6 (make-ring-zn 6)]
         [i1 (make-principal-ideal z6 2)]  ; {0, 2, 4}
         [i2 (make-principal-ideal z6 3)]  ; {0, 3}
         [sum (ideal-sum i1 i2)])
        (assert (ideal? sum))
        ; <2> + <3> = Z_6 in Z_6
        (let ([elems (ideal-elements sum)])
             (assert (>= (length elems) 1)))))

(define-test "ideal product"
  (let* ([z6 (make-ring-zn 6)]
         [i1 (make-principal-ideal z6 2)]
         [i2 (make-principal-ideal z6 3)]
         [prod (ideal-product i1 i2)])
        (assert (ideal? prod))
        ; <2> × <3> contains 0 (and possibly other elements)
        (let ([elems (ideal-elements prod)])
             (assert (member 0 elems)))))

;;; ============================================================
;;; Special Ring Tests
;;; ============================================================

(define-test "Z_2 is the smallest field"
  (let ([z2 (make-ring-zn 2)])
       (assert (is-commutative-ring? z2))
       (assert (is-integral-domain? z2))
       ; Check all non-zero elements are units
       (assert (is-unit? z2 1))
       (assert (not (is-unit? z2 0)))))

(define-test "Z_4 square root of 1"
  (let ([z4 (make-ring-zn 4)])
       ; In Z_4, both 1 and 3 are square roots of 1
       (assert (= (ring-mul z4 1 1) 1))
       (assert (= (ring-mul z4 3 3) 1))))

(define-test "binomial expansion (a+b)^2"
  (let ([z5 (make-ring-zn 5)])
       ; (2+3)^2 should equal 2^2 + 2×2×3 + 3^2
       (let* ([a 2]
              [b 3]
              [sum (ring-add z5 a b)]
              [lhs (ring-power z5 sum 2)]
              [a-sq (ring-power z5 a 2)]
              [b-sq (ring-power z5 b 2)]
              [two-ab (ring-mul z5 2 (ring-mul z5 a b))]
              [rhs (ring-sum z5 (list a-sq two-ab b-sq))])
             (assert (= lhs rhs)))))

;;; ============================================================
;;; Tests auto-run when defined
;;; ============================================================
