;;; lattice/number-theory/fast-multiply.ss — Fast Multiplication Algorithms
;;; @module fast-multiply
;;; @requires ()
;;;
;;; Karatsuba and Toom-Cook multiplication for large integers.
;;; These algorithms work on "limb lists" - numbers represented as
;;; lists of base-B digits where B is typically 2^32 or similar.
;;;
;;; Complexity comparison for n-limb numbers:
;;;   Schoolbook:  O(n²)
;;;   Karatsuba:   O(n^1.585)  - crossover around 32-64 limbs
;;;   Toom-3:      O(n^1.465)  - crossover around 100+ limbs

(load "core/base/prelude.ss")

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
  (doc 'description "Karatsuba O(n^1.585) multiplication using divide-and-conquer.")
  (doc 'complexity "O(n^log2(3)) ≈ O(n^1.585)")
  (let ([na (length a)]
        [nb (length b)])
    ;; Base case: use schoolbook for small inputs
    (if (or (<= na *karatsuba-threshold*)
            (<= nb *karatsuba-threshold*))
        (limbs-multiply-schoolbook a b base)
        ;; Recursive case
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
           base)))))

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

(define (limbs-negate limbs)
  (doc 'export #t)
  (doc 'type '(-> (List Int) (List Int)))
  (doc 'description "Negate all limbs (for signed intermediate values).")
  (map - limbs))

(define (limbs-add-signed a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Int) (List Int) Nat (List Int)))
  (doc 'description "Add two signed limb lists.")
  ;; For Toom-Cook we need signed arithmetic
  ;; Simplified: just add the lists element-wise, handle carries at end
  (let* ([len (max (length a) (length b))]
         [a-pad (limbs-pad-to a len)]
         [b-pad (limbs-pad-to b len)])
    (let loop ([as a-pad] [bs b-pad] [result '()])
      (if (null? as)
          (reverse result)
          (loop (cdr as) (cdr bs)
                (cons (+ (car as) (car bs)) result))))))

(define (limbs-propagate-carries limbs base)
  (doc 'export #t)
  (doc 'type '(-> (List Int) Nat (List Nat)))
  (doc 'description "Propagate carries through signed limb list, normalizing to unsigned.")
  (let loop ([ls limbs] [carry 0] [result '()])
    (if (null? ls)
        (limbs-normalize
         (reverse (if (not (= carry 0)) (cons carry result) result)))
        (let* ([val (+ (car ls) carry)]
               [limb (modulo val base)]
               [new-carry (quotient val base)]
               ;; Handle negative modulo
               [limb (if (< limb 0) (+ limb base) limb)]
               [new-carry (if (< val 0) (- new-carry 1) new-carry)])
          (loop (cdr ls) new-carry (cons limb result))))))

(define (limbs-multiply-toom3 a b base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) (List Nat) Nat (List Nat)))
  (doc 'description "Toom-3 O(n^1.465) multiplication.")
  (doc 'complexity "O(n^log3(5)) ≈ O(n^1.465)")
  (let ([na (length a)]
        [nb (length b)])
    ;; Use Karatsuba for inputs below threshold
    (if (or (<= na *toom3-threshold*)
            (<= nb *toom3-threshold*))
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

               ;; Evaluate p(t) at t = 0, 1, -1, 2, ∞
               ;; p(0) = a0
               ;; p(1) = a0 + a1 + a2
               ;; p(-1) = a0 - a1 + a2
               ;; p(2) = a0 + 2*a1 + 4*a2
               ;; p(∞) = a2 (leading coefficient)

               [p0 a0]
               [p1 (limbs-add (limbs-add a0 a1 base) a2 base)]
               [p-1-parts (limbs-add-signed
                           (limbs-add-signed a0 (limbs-negate a1) base)
                           a2 base)]
               [p2 (limbs-add (limbs-add a0 (limb-scale a1 2 base) base)
                             (limb-scale a2 4 base) base)]
               [pinf a2]

               [q0 b0]
               [q1 (limbs-add (limbs-add b0 b1 base) b2 base)]
               [q-1-parts (limbs-add-signed
                           (limbs-add-signed b0 (limbs-negate b1) base)
                           b2 base)]
               [q2 (limbs-add (limbs-add b0 (limb-scale b1 2 base) base)
                             (limb-scale b2 4 base) base)]
               [qinf b2]

               ;; Multiply at evaluation points
               [r0 (limbs-multiply-toom3 p0 q0 base)]
               [r1 (limbs-multiply-toom3 p1 q1 base)]
               ;; For r(-1), handle signed multiplication
               [r-1-unsigned (limbs-multiply-toom3
                              (limbs-propagate-carries p-1-parts base)
                              (limbs-propagate-carries q-1-parts base)
                              base)]
               [r2 (limbs-multiply-toom3 p2 q2 base)]
               [rinf (limbs-multiply-toom3 pinf qinf base)]

               ;; Interpolation to recover coefficients c0..c4
               ;; r(t) = c4*t^4 + c3*t^3 + c2*t^2 + c1*t + c0
               ;;
               ;; c0 = r(0)
               ;; c4 = r(∞)
               ;; From the system of equations, solve for c1, c2, c3
               ;; This involves some divisions by small integers

               [c0 r0]
               [c4 rinf]

               ;; Intermediate values for interpolation
               ;; Using standard Toom-3 interpolation formulas
               [t1 (limbs-sub r1 r0 base)]         ; r(1) - r(0)
               [t2 (limbs-sub r-1-unsigned r0 base)] ; r(-1) - r(0), approximate
               [t3 (limbs-sub r2 r0 base)]         ; r(2) - r(0)

               ;; Simplified interpolation (exact for Toom-3)
               ;; c1 = (r(1) - r(-1))/2
               ;; c2 = r(-1) - r(0) + rinf  (approximate)
               ;; c3 = (r(2) - 2*r(1) + r(-1) - 6*rinf)/6

               ;; For simplicity, use a direct but less efficient interpolation
               ;; c1 + c2 + c3 = r(1) - r(0) - r(∞)
               ;; -c1 + c2 - c3 = r(-1) - r(0) - r(∞)
               ;; 2c1 + 4c2 + 8c3 = r(2) - r(0) - 16*r(∞)

               ;; Adding first two: 2c2 = r(1) + r(-1) - 2*r(0) - 2*r(∞)
               [two-c2 (limbs-sub (limbs-sub
                                   (limbs-add r1 r-1-unsigned base)
                                   (limb-scale r0 2 base)
                                   base)
                                  (limb-scale rinf 2 base)
                                  base)]
               ;; c2 = two-c2 / 2
               [c2 (limbs-div-small two-c2 2 base)]

               ;; Subtracting: 2c1 + 2c3 = r(1) - r(-1)
               [two-c1-c3 (limbs-sub r1 r-1-unsigned base)]

               ;; From third equation: 2c1 + 4c2 + 8c3 = r(2) - r(0) - 16*rinf
               [rhs3 (limbs-sub (limbs-sub r2 r0 base)
                               (limb-scale rinf 16 base)
                               base)]
               ;; 2c1 + 8c3 = rhs3 - 4*c2
               [two-c1-8c3 (limbs-sub rhs3 (limb-scale c2 4 base) base)]

               ;; Solve: 2c1 + 2c3 = two-c1-c3
               ;;        2c1 + 8c3 = two-c1-8c3
               ;; Subtracting: 6c3 = two-c1-8c3 - two-c1-c3
               [six-c3 (limbs-sub two-c1-8c3 two-c1-c3 base)]
               [c3 (limbs-div-small six-c3 6 base)]

               ;; c1 = (two-c1-c3 - 2*c3) / 2
               [c1 (limbs-div-small (limbs-sub two-c1-c3 (limb-scale c3 2 base) base)
                                   2 base)])

          ;; Combine: c0 + c1*B^m + c2*B^(2m) + c3*B^(3m) + c4*B^(4m)
          (limbs-add
           (limbs-add
            (limbs-add
             (limbs-add c0 (limbs-shift c1 m) base)
             (limbs-shift c2 (* 2 m)) base)
            (limbs-shift c3 (* 3 m)) base)
           (limbs-shift c4 (* 4 m)) base)))))

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
Exploits symmetry: diagonal terms + 2*(off-diagonal terms).")
  ;; For now, just use multiplication - optimization can be added later
  (limbs-multiply-schoolbook a a base))

(define (limbs-square a base)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Square a limb list using optimal algorithm.")
  ;; Squaring with Karatsuba: x² = (x1*B^m + x0)²
  ;;                            = x1²*B^(2m) + 2*x1*x0*B^m + x0²
  ;; Only 2 recursive squares + 1 multiplication
  (limbs-multiply a a base))

(define (fast-square x)
  (doc 'export #t)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Square an integer using optimal algorithm.")
  (fast-multiply x x))

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

(display "Loaded: lattice/number-theory/fast-multiply.ss\n")
