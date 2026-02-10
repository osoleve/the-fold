;;; lattice/number-theory/fast-multiply.ss — Fast Multiplication Algorithms
;;; @module fast-multiply
;;; @requires prelude
;;;
;;; Karatsuba and Toom-Cook multiplication for large integers.
;;; These algorithms work on "limb lists" - numbers represented as
;;; lists of base-B digits where B is typically 2^32 or similar.
;;;
;;; Complexity comparison for n-limb numbers:
;;;   Schoolbook:  O(n²)
;;;   Karatsuba:   O(n^1.585)  - crossover around 32-64 limbs
;;;   Toom-3:      O(n^1.465)  - crossover around 100+ limbs

(require 'prelude)

(doc 'module 'fast-multiply)
(doc 'description "Fast multiplication algorithms: Karatsuba, Toom-Cook, and hybrid strategies")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Limb List Representation
;;; ============================================================================
;;;
;;; Numbers are represented as lists of non-negative integers (limbs),
;;; least-significant first. For base B:
;;;   (l0 l1 l2 ...) represents l0 + l1*B + l2*B² + ...
;;;
;;; We use a configurable base for flexibility.

(doc 'section 'limb-operations)

(define *default-base* (expt 2 30))  ; 30-bit limbs for safe 64-bit intermediate products

(define (limbs->integer limbs base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat Nat))
  (doc 'description "Convert limb list to integer. Limbs are least-significant first.")
  (let loop ([ls limbs] [multiplier 1] [result 0])
    (if (null? ls)
        result
        (loop (cdr ls)
              (* multiplier base)
              (+ result (* (car ls) multiplier))))))

(define (integer->limbs n base)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat (List Nat)))
  (doc 'description "Convert integer to limb list. Returns least-significant limb first.")
  (if (= n 0)
      '(0)
      (let loop ([n n] [limbs '()])
        (if (= n 0)
            (reverse limbs)
            (loop (quotient n base)
                  (cons (modulo n base) limbs))))))

(define (limbs-normalize limbs)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat)))
  (doc 'description "Remove trailing zeros (high-order) from limb list.")
  (let loop ([ls (reverse limbs)])
    (cond
      [(null? ls) '(0)]
      [(and (= (car ls) 0) (not (null? (cdr ls))))
       (loop (cdr ls))]
      [else (reverse ls)])))

(define (limbs-pad-to limbs n)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Pad limb list with trailing zeros to length n.")
  (let ([len (length limbs)])
    (if (>= len n)
        limbs
        (append limbs (make-list (- n len) 0)))))

(define (limbs-split limbs k)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List (List Nat))))
  (doc 'description "Split limbs at position k: returns (low high) where low has k limbs.")
  (let loop ([ls limbs] [i 0] [low '()])
    (cond
      [(or (null? ls) (= i k))
       (list (reverse low) (if (null? ls) '(0) ls))]
      [else
       (loop (cdr ls) (+ i 1) (cons (car ls) low))])))

;;; ============================================================================
;;; Schoolbook Multiplication
;;; ============================================================================

(doc 'section 'schoolbook)

(define (limbs-add a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Add two limb lists with carry propagation.")
  (let loop ([a a] [b b] [carry 0] [result '()])
    (cond
      [(and (null? a) (null? b))
       (limbs-normalize
        (reverse (if (> carry 0) (cons carry result) result)))]
      [(null? a)
       (loop '() (cdr b)
             (quotient (+ (car b) carry) base)
             (cons (modulo (+ (car b) carry) base) result))]
      [(null? b)
       (loop (cdr a) '()
             (quotient (+ (car a) carry) base)
             (cons (modulo (+ (car a) carry) base) result))]
      [else
       (let ([sum (+ (car a) (car b) carry)])
         (loop (cdr a) (cdr b)
               (quotient sum base)
               (cons (modulo sum base) result)))])))

(define (limbs-sub a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Subtract b from a (assumes a >= b). Returns limb list.")
  (let loop ([a a] [b b] [borrow 0] [result '()])
    (cond
      [(and (null? a) (null? b))
       (limbs-normalize (reverse result))]
      [(null? b)
       (let ([diff (- (car a) borrow)])
         (if (< diff 0)
             (loop (cdr a) '() 1 (cons (+ diff base) result))
             (loop (cdr a) '() 0 (cons diff result))))]
      [else
       (let* ([av (if (null? a) 0 (car a))]
              [bv (car b)]
              [diff (- av bv borrow)])
         (if (< diff 0)
             (loop (if (null? a) '() (cdr a))
                   (cdr b)
                   1
                   (cons (+ diff base) result))
             (loop (if (null? a) '() (cdr a))
                   (cdr b)
                   0
                   (cons diff result))))])))

(define (limbs-shift limbs k)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Shift left by k limbs (multiply by base^k).")
  (if (and (= 1 (length limbs)) (= 0 (car limbs)))
      limbs  ; 0 shifted is still 0
      (append (make-list k 0) limbs)))

(define (limb-scale limbs scalar base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat Nat (List Nat)))
  (doc 'description "Multiply limb list by a single limb (scalar).")
  (let loop ([ls limbs] [carry 0] [result '()])
    (if (null? ls)
        (limbs-normalize
         (reverse (if (> carry 0) (cons carry result) result)))
        (let ([prod (+ (* (car ls) scalar) carry)])
          (loop (cdr ls)
                (quotient prod base)
                (cons (modulo prod base) result))))))

(define (limbs-multiply-schoolbook a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Schoolbook O(n²) multiplication. Foundation for small inputs.")
  (let loop ([b b] [shift 0] [result '(0)])
    (if (null? b)
        (limbs-normalize result)
        (let ([partial (limb-scale a (car b) base)])
          (loop (cdr b)
                (+ shift 1)
                (limbs-add result (limbs-shift partial shift) base))))))

;;; ============================================================================
;;; Karatsuba Multiplication
;;; ============================================================================
;;;
;;; Given n-digit numbers x and y, split each at n/2:
;;;   x = x1*B^m + x0
;;;   y = y1*B^m + y0
;;;
;;; Then xy = (x1*B^m + x0)(y1*B^m + y0)
;;;         = x1*y1*B^(2m) + (x1*y0 + x0*y1)*B^m + x0*y0
;;;
;;; Karatsuba's insight: compute 3 products instead of 4:
;;;   z2 = x1*y1
;;;   z0 = x0*y0
;;;   z1 = (x0+x1)*(y0+y1) - z0 - z2  (gives x1*y0 + x0*y1)
;;;
;;; Result: z2*B^(2m) + z1*B^m + z0

(doc 'section 'karatsuba)

(define *karatsuba-threshold* 32)  ; Below this, use schoolbook

(define (limbs-multiply-karatsuba a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Karatsuba O(n^1.585) multiplication using divide-and-conquer.
Optimized for asymmetric inputs: uses limb-scale when one operand is 1 limb,
falls back to schoolbook when smaller operand is below threshold.")
  (doc 'complexity "O(n^log2(3)) ≈ O(n^1.585)")
  (let ([na (length a)]
        [nb (length b)])
    ;; Asymmetric optimization: if one operand is single limb, use O(n) scaling
    (cond
      [(= na 1)
       (limb-scale b (car a) base)]
      [(= nb 1)
       (limb-scale a (car b) base)]
      ;; Base case: use schoolbook when smaller operand is below threshold
      ;; Using min ensures we don't waste Karatsuba overhead on asymmetric inputs
      [(<= (min na nb) *karatsuba-threshold*)
       (limbs-multiply-schoolbook a b base)]
      ;; Recursive case
      [else
       (let* ([m (quotient (max na nb) 2)]
               ;; Split a = a1*B^m + a0
               [split-a (limbs-split a m)]
               [a0 (car split-a)]
               [a1 (cadr split-a)]
               ;; Split b = b1*B^m + b0
               [split-b (limbs-split b m)]
               [b0 (car split-b)]
               [b1 (cadr split-b)]
               ;; Compute the three products
               [z0 (limbs-multiply-karatsuba a0 b0 base)]
               [z2 (limbs-multiply-karatsuba a1 b1 base)]
               [a0+a1 (limbs-add a0 a1 base)]
               [b0+b1 (limbs-add b0 b1 base)]
               [z1-raw (limbs-multiply-karatsuba a0+a1 b0+b1 base)]
               [z1 (limbs-sub (limbs-sub z1-raw z0 base) z2 base)])
          ;; Combine: z2*B^(2m) + z1*B^m + z0
          (limbs-add
           (limbs-add z0 (limbs-shift z1 m) base)
           (limbs-shift z2 (* 2 m))
           base))])))  ; close let*, else clause, cond, outer let

;;; ============================================================================
;;; Toom-Cook (Toom-3) Multiplication
;;; ============================================================================
;;;
;;; Split into 3 parts instead of 2:
;;;   x = x2*B^(2m) + x1*B^m + x0
;;;   y = y2*B^(2m) + y1*B^m + y0
;;;
;;; Treat as polynomials p(t) = x2*t² + x1*t + x0, q(t) = y2*t² + y1*t + y0
;;; Product r(t) = p(t)*q(t) is degree 4, has 5 coefficients.
;;;
;;; Evaluate at 5 points: t = 0, 1, -1, 2, ∞
;;; Multiply point values, then interpolate to get coefficients.

(doc 'section 'toom-cook)

(define *toom3-threshold* 100)  ; Below this, use Karatsuba

(define (limbs-split3 limbs m)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List (List Nat))))
  (doc 'description "Split limbs into three parts of size m (low, mid, high).")
  (let* ([split1 (limbs-split limbs m)]
         [low (car split1)]
         [rest (cadr split1)]
         [split2 (limbs-split rest m)]
         [mid (car split2)]
         [high (cadr split2)])
    (list low mid high)))

;;; Signed number representation for Toom-Cook
;;; A signed-limbs is (sign . limbs) where sign is 1 or -1 and limbs is unsigned

(define (make-signed sign limbs)
  (doc 'type '(-> Int (List Nat) SignedLimbs))
  (doc 'description "Create a signed limb representation.")
  (cons sign (limbs-normalize limbs)))

(define (signed-sign sl) (car sl))
(define (signed-limbs sl) (cdr sl))

(define (signed-zero? sl)
  (doc 'type '(-> SignedLimbs Boolean))
  (let ([limbs (signed-limbs sl)])
    (and (= 1 (length limbs)) (= 0 (car limbs)))))

(define (limbs-compare a b)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Int))
  (doc 'description "Compare two normalized limb lists. Returns -1, 0, or 1.")
  (let ([la (length a)]
        [lb (length b)])
    (cond
      [(> la lb) 1]
      [(< la lb) -1]
      [else
       ;; Same length - compare from high to low
       (let loop ([as (reverse a)] [bs (reverse b)])
         (cond
           [(null? as) 0]
           [(> (car as) (car bs)) 1]
           [(< (car as) (car bs)) -1]
           [else (loop (cdr as) (cdr bs))]))])))

(define (signed-add sa sb base)
  (doc 'export #t)
  (doc 'type '(-> SignedLimbs SignedLimbs Nat SignedLimbs))
  (doc 'description "Add two signed limb numbers.")
  (let ([sign-a (signed-sign sa)]
        [sign-b (signed-sign sb)]
        [a (signed-limbs sa)]
        [b (signed-limbs sb)])
    (if (= sign-a sign-b)
        ;; Same sign: add magnitudes, keep sign
        (make-signed sign-a (limbs-add a b base))
        ;; Different signs: subtract smaller from larger
        (let ([cmp (limbs-compare a b)])
          (cond
            [(= cmp 0) (make-signed 1 '(0))]  ; Equal magnitudes = zero
            [(> cmp 0) (make-signed sign-a (limbs-sub a b base))]
            [else (make-signed sign-b (limbs-sub b a base))])))))

(define (signed-negate sl)
  (doc 'type '(-> SignedLimbs SignedLimbs))
  (doc 'description "Negate a signed limb number.")
  (if (signed-zero? sl)
      sl
      (make-signed (- (signed-sign sl)) (signed-limbs sl))))

(define (signed-multiply sa sb base multiply-fn)
  (doc 'type '(-> SignedLimbs SignedLimbs Nat (-> (List Nat) (List Nat) Nat (List Nat)) SignedLimbs))
  (doc 'description "Multiply two signed limb numbers using provided multiplication function.")
  (let ([sign-a (signed-sign sa)]
        [sign-b (signed-sign sb)]
        [a (signed-limbs sa)]
        [b (signed-limbs sb)])
    (make-signed (* sign-a sign-b) (multiply-fn a b base))))

(define (unsigned->signed limbs)
  (doc 'type '(-> (List Nat) SignedLimbs))
  (make-signed 1 limbs))

(define (limbs-multiply-toom3 a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Toom-3 O(n^1.465) multiplication.
Optimized for asymmetric inputs: delegates to Karatsuba when smaller operand
is below threshold, which handles single-limb cases with O(n) scaling.")
  (doc 'complexity "O(n^log3(5)) ≈ O(n^1.465)")
  (doc 'note "Uses signed arithmetic for evaluation at t=-1 to handle negative intermediate values correctly.")
  (let ([na (length a)]
        [nb (length b)])
    ;; Use Karatsuba for inputs below threshold
    ;; Using min ensures we don't waste Toom-3 overhead on asymmetric inputs
    ;; Karatsuba will further optimize single-limb cases with O(n) scaling
    (if (<= (min na nb) *toom3-threshold*)
        (limbs-multiply-karatsuba a b base)
        ;; Split into thirds
        (let* ([m (quotient (max na nb) 3)]
               [split-a (limbs-split3 a m)]
               [a0 (car split-a)]
               [a1 (cadr split-a)]
               [a2 (caddr split-a)]
               [split-b (limbs-split3 b m)]
               [b0 (car split-b)]
               [b1 (cadr split-b)]
               [b2 (caddr split-b)]

               ;; Convert to signed for evaluation at -1
               [sa0 (unsigned->signed a0)]
               [sa1 (unsigned->signed a1)]
               [sa2 (unsigned->signed a2)]
               [sb0 (unsigned->signed b0)]
               [sb1 (unsigned->signed b1)]
               [sb2 (unsigned->signed b2)]

               ;; Evaluate p(t) at t = 0, 1, -1, 2, ∞
               ;; p(0) = a0
               ;; p(1) = a0 + a1 + a2
               ;; p(-1) = a0 - a1 + a2  (can be negative!)
               ;; p(2) = a0 + 2*a1 + 4*a2
               ;; p(∞) = a2

               [p0 a0]
               [p1 (limbs-add (limbs-add a0 a1 base) a2 base)]
               ;; p(-1) using signed arithmetic: a0 - a1 + a2
               [sp-1 (signed-add (signed-add sa0 (signed-negate sa1) base) sa2 base)]
               [p2 (limbs-add (limbs-add a0 (limb-scale a1 2 base) base)
                             (limb-scale a2 4 base) base)]
               [pinf a2]

               [q0 b0]
               [q1 (limbs-add (limbs-add b0 b1 base) b2 base)]
               ;; q(-1) using signed arithmetic: b0 - b1 + b2
               [sq-1 (signed-add (signed-add sb0 (signed-negate sb1) base) sb2 base)]
               [q2 (limbs-add (limbs-add b0 (limb-scale b1 2 base) base)
                             (limb-scale b2 4 base) base)]
               [qinf b2]

               ;; Multiply at evaluation points
               [r0 (limbs-multiply-toom3 p0 q0 base)]
               [r1 (limbs-multiply-toom3 p1 q1 base)]
               ;; r(-1) = p(-1) * q(-1) with proper sign tracking
               [sr-1 (signed-multiply sp-1 sq-1 base limbs-multiply-toom3)]
               [r2 (limbs-multiply-toom3 p2 q2 base)]
               [rinf (limbs-multiply-toom3 pinf qinf base)]

               ;; Convert unsigned results to signed for interpolation
               [sr0 (unsigned->signed r0)]
               [sr1 (unsigned->signed r1)]
               [sr2 (unsigned->signed r2)]
               [srinf (unsigned->signed rinf)]

               ;; Interpolation using signed arithmetic
               ;; Standard Toom-3 interpolation formulas:
               ;; c0 = r(0)
               ;; c4 = r(∞)
               ;; c1 = (r(1) - r(-1)) / 2 - c4
               ;; c2 = (r(1) + r(-1)) / 2 - c0
               ;; c3 = (r(2) - c0 - c2 - 4*c4) / 2 - c1  ... simplified

               ;; More stable formulas using the system:
               ;; r(0) = c0
               ;; r(1) = c0 + c1 + c2 + c3 + c4
               ;; r(-1) = c0 - c1 + c2 - c3 + c4
               ;; r(2) = c0 + 2c1 + 4c2 + 8c3 + 16c4
               ;; r(∞) = c4

               ;; Solve step by step:
               [c0 r0]
               [c4 rinf]
               [sc0 sr0]
               [sc4 srinf]

               ;; r(1) + r(-1) = 2(c0 + c2 + c4)
               ;; => c2 = (r(1) + r(-1))/2 - c0 - c4
               [sum-r1-rm1 (signed-add sr1 sr-1 base)]
               [half-sum (signed-div-small sum-r1-rm1 2 base)]
               [sc2 (signed-add (signed-add half-sum (signed-negate sc0) base)
                               (signed-negate sc4) base)]

               ;; r(1) - r(-1) = 2(c1 + c3)
               ;; => c1 + c3 = (r(1) - r(-1))/2
               [diff-r1-rm1 (signed-add sr1 (signed-negate sr-1) base)]
               [half-diff (signed-div-small diff-r1-rm1 2 base)]  ; c1 + c3

               ;; r(2) = c0 + 2c1 + 4c2 + 8c3 + 16c4
               ;; r(2) - c0 - 4c2 - 16c4 = 2c1 + 8c3 = 2(c1 + 4c3)
               ;; => c1 + 4c3 = (r(2) - c0 - 4c2 - 16c4)/2
               [sr2-terms (signed-add
                           (signed-add
                            (signed-add sr2 (signed-negate sc0) base)
                            (signed-negate (signed-scale sc2 4 base)) base)
                           (signed-negate (signed-scale sc4 16 base)) base)]
               [c1-plus-4c3 (signed-div-small sr2-terms 2 base)]

               ;; (c1 + 4c3) - (c1 + c3) = 3c3
               ;; => c3 = ((c1+4c3) - (c1+c3))/3
               [three-c3 (signed-add c1-plus-4c3 (signed-negate half-diff) base)]
               [sc3 (signed-div-small three-c3 3 base)]

               ;; c1 = (c1 + c3) - c3
               [sc1 (signed-add half-diff (signed-negate sc3) base)]

               ;; Extract unsigned coefficients (all should be non-negative for valid input)
               [c1 (signed-limbs sc1)]
               [c2 (signed-limbs sc2)]
               [c3 (signed-limbs sc3)])

          ;; Combine: c0 + c1*B^m + c2*B^(2m) + c3*B^(3m) + c4*B^(4m)
          (limbs-add
           (limbs-add
            (limbs-add
             (limbs-add c0 (limbs-shift c1 m) base)
             (limbs-shift c2 (* 2 m)) base)
            (limbs-shift c3 (* 3 m)) base)
           (limbs-shift c4 (* 4 m)) base)))))

(define (signed-scale sl n base)
  (doc 'type '(-> SignedLimbs Nat Nat SignedLimbs))
  (doc 'description "Scale a signed limb number by positive integer n.")
  (make-signed (signed-sign sl) (limb-scale (signed-limbs sl) n base)))

(define (signed-div-small sl d base)
  (doc 'type '(-> SignedLimbs Nat Nat SignedLimbs))
  (doc 'description "Divide signed limb number by small positive integer. Assumes exact division.")
  (make-signed (signed-sign sl) (limbs-div-small (signed-limbs sl) d base)))

(define (limbs-div-small limbs d base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat Nat (List Nat)))
  (doc 'description "Divide limb list by small integer d. Assumes exact division.")
  (let loop ([ls (reverse limbs)] [carry 0] [result '()])
    (if (null? ls)
        (limbs-normalize result)
        (let* ([val (+ (* carry base) (car ls))]
               [q (quotient val d)]
               [r (modulo val d)])
          (loop (cdr ls) r (cons q result))))))

;;; ============================================================================
;;; Hybrid Multiplier with Automatic Algorithm Selection
;;; ============================================================================

(doc 'section 'hybrid)

(define (limbs-multiply a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Multiply two limb lists, automatically selecting best algorithm.
Uses schoolbook for small inputs, Karatsuba for medium, Toom-3 for large.")
  (let ([n (max (length a) (length b))])
    (cond
      [(<= n *karatsuba-threshold*)
       (limbs-multiply-schoolbook a b base)]
      [(<= n *toom3-threshold*)
       (limbs-multiply-karatsuba a b base)]
      [else
       (limbs-multiply-toom3 a b base)])))

;;; ============================================================================
;;; Integer Interface
;;; ============================================================================
;;;
;;; Convenience functions that work directly on Scheme integers

(doc 'section 'integer-interface)

(define (fast-multiply x y)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "Multiply two integers using optimal algorithm selection.
For small inputs, uses native multiplication. For large inputs,
converts to limbs and uses Karatsuba/Toom-3.")
  (let ([base *default-base*])
    ;; For native integers, check if they're "large enough" to benefit
    ;; Rough heuristic: if either number > base^threshold, use fast multiply
    (if (or (< x (expt base 4))
            (< y (expt base 4)))
        (* x y)  ; Native is fine
        (let ([a (integer->limbs x base)]
              [b (integer->limbs y base)])
          (limbs->integer (limbs-multiply a b base) base)))))

(define (karatsuba-multiply x y)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "Multiply two integers using Karatsuba algorithm.")
  (let* ([base *default-base*]
         [a (integer->limbs x base)]
         [b (integer->limbs y base)])
    (limbs->integer (limbs-multiply-karatsuba a b base) base)))

(define (toom3-multiply x y)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "Multiply two integers using Toom-3 algorithm.")
  (let* ([base *default-base*]
         [a (integer->limbs x base)]
         [b (integer->limbs y base)])
    (limbs->integer (limbs-multiply-toom3 a b base) base)))

;;; ============================================================================
;;; Squaring Optimization
;;; ============================================================================
;;;
;;; Squaring can be done faster than general multiplication because
;;; we can take advantage of symmetry: (a+b)² = a² + 2ab + b²

(doc 'section 'squaring)

(define (limbs-square-schoolbook a base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Square a limb list using schoolbook method with optimization.
Exploits symmetry: diagonal terms + 2*(off-diagonal terms).
Reduces multiplications from n² to n(n+1)/2.")
  (let ([n (length a)])
       (if (< n 2)
           ;; Single limb: just square it
           (if (null? a)
               '(0)
               (let* ([val (car a)]
                      [sq (* val val)]
                      [lo (modulo sq base)]
                      [hi (quotient sq base)])
                     (if (> hi 0) (list lo hi) (list lo))))
           ;; Multi-limb: diagonal + 2*cross terms
           (let ([result (make-vector (+ (* 2 n) 1) 0)])
                ;; Diagonal terms: a[i]² at position 2i
                (do ([i 0 (+ i 1)])
                    ((= i n))
                    (let* ([ai (list-ref a i)]
                           [sq (* ai ai)]
                           [pos (* 2 i)])
                          (vector-set! result pos (+ (vector-ref result pos) sq))))
                ;; Cross terms: 2 * a[i] * a[j] at position i+j for i < j
                (do ([i 0 (+ i 1)])
                    ((= i n))
                    (do ([j (+ i 1) (+ j 1)])
                        ((= j n))
                        (let* ([ai (list-ref a i)]
                               [aj (list-ref a j)]
                               [cross (* 2 ai aj)]
                               [pos (+ i j)])
                              (vector-set! result pos (+ (vector-ref result pos) cross)))))
                ;; Propagate carries
                (let loop ([i 0] [carry 0])
                     (if (>= i (vector-length result))
                         (limbs-normalize (vector->list result))
                         (let* ([val (+ (vector-ref result i) carry)]
                                [lo (modulo val base)]
                                [hi (quotient val base)])
                               (vector-set! result i lo)
                               (loop (+ i 1) hi))))))))

(define (limbs-square a base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Square a limb list using Karatsuba-style optimization.
For x = x1*B^m + x0: x² = x1²*B^(2m) + 2*x1*x0*B^m + x0²
Uses 2 recursive squares + 1 multiplication (vs 3 multiplications for general Karatsuba).")
  (let ([n (length a)])
       (cond
        ;; Base case: single limb
        [(= n 1)
         (let* ([val (car a)]
                [sq (* val val)]
                [lo (modulo sq base)]
                [hi (quotient sq base)])
               (if (> hi 0) (list lo hi) (list lo)))]
        ;; Small input: use optimized schoolbook
        [(<= n *karatsuba-threshold*)
         (limbs-square-schoolbook a base)]
        ;; Karatsuba squaring
        [else
         (let* ([m (quotient n 2)]
                ;; Split a = a1*B^m + a0
                [split-a (limbs-split a m)]
                [a0 (car split-a)]
                [a1 (cadr split-a)]
                ;; Compute z0 = a0² and z2 = a1² (recursive squares)
                [z0 (limbs-square a0 base)]
                [z2 (limbs-square a1 base)]
                ;; Compute z1 = 2*a0*a1 (one multiplication, then double)
                [a0*a1 (limbs-multiply a0 a1 base)]
                [z1 (limbs-double a0*a1 base)]
                ;; Result: z2*B^(2m) + z1*B^m + z0
                [term2 (limbs-shift z2 (* 2 m))]
                [term1 (limbs-shift z1 m)])
               (limbs-add (limbs-add z0 term1 base) term2 base))])))

(define (limbs-double a base)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Double a limb list (multiply by 2).")
  (let loop ([ls a] [carry 0] [result '()])
       (if (null? ls)
           (limbs-normalize
            (reverse (if (> carry 0) (cons carry result) result)))
           (let ([doubled (+ (* 2 (car ls)) carry)])
                (loop (cdr ls)
                      (quotient doubled base)
                      (cons (modulo doubled base) result))))))

(define (fast-square x)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Square an integer using optimized squaring algorithm.
~1.5x faster than general multiplication due to symmetry exploitation.")
  (let* ([base *default-base*]
         [a (integer->limbs x base)])
        (limbs->integer (limbs-square a base) base)))

;;; ============================================================================
;;; Configuration
;;; ============================================================================

(doc 'section 'configuration)

(define (set-karatsuba-threshold! n)
  (doc 'export #t)
  (doc 'type '(-> Nat Void))
  (doc 'description "Set the threshold below which schoolbook multiplication is used.")
  (set! *karatsuba-threshold* n))

(define (set-toom3-threshold! n)
  (doc 'export #t)
  (doc 'type '(-> Nat Void))
  (doc 'description "Set the threshold below which Karatsuba is used instead of Toom-3.")
  (set! *toom3-threshold* n))

(define (get-multiply-thresholds)
  (doc 'export #t)
  (doc 'type '(-> (List Nat)))
  (doc 'description "Get current algorithm thresholds: (karatsuba-threshold toom3-threshold).")
  (list *karatsuba-threshold* *toom3-threshold*))

