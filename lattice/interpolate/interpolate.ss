;;; lattice/interpolate/interpolate.ss — Numerical Interpolation
;;; @module interpolate
;;; @requires prelude matrix matrix-decomp matrix-solvers numeric/polynomial iteration

(require 'prelude)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-solvers)
(require 'numeric/polynomial)
(require 'iteration)

(doc 'module 'interpolate)
(doc 'description "Numerical interpolation, splines, Bezier curves, and curve fitting")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Binary Search Helper (for O(log n) segment lookup)
;;; ====

;;; binary-search-segment : (List Num) × Num → Int
;;; Find index i such that xs[i] <= x < xs[i+1].
;;; Returns 0 if x < xs[0], n-2 if x >= xs[n-1].
;;; Uses binary search: O(log n) instead of O(n).
(define (binary-search-segment xs x)
  (doc 'export #t)
  (let ([n (length xs)])
    (if (<= n 1)
        0
        (let loop ([lo 0] [hi (- n 1)])
          (if (<= (- hi lo) 1)
              lo
              (let* ([mid (quotient (+ lo hi) 2)]
                     [x-mid (list-ref xs mid)])
                (if (<= x x-mid)
                    (loop lo mid)
                    (loop mid hi))))))))

;;; binary-search-segment-vec : (Vector Num) × Num → Int
;;; Vector version of binary-search-segment for spline evaluation.
(define (binary-search-segment-vec xs-list x n)
  (if (<= n 1)
      0
      (let loop ([lo 0] [hi (- n 1)])
        (if (<= (- hi lo) 1)
            lo
            (let* ([mid (quotient (+ lo hi) 2)]
                   [x-mid (list-ref xs-list mid)])
              (if (<= x x-mid)
                  (loop lo mid)
                  (loop mid hi)))))))

;;; ====
;;; Linear Interpolation
;;; ====

;;; lerp : Num × Num × Num → Num
;;; Linear interpolation: lerp(a, b, t) = a + t×(b - a)
;;; When t=0 returns a, when t=1 returns b.
(define (lerp a b t)
  (doc 'export #t)
  (+ a (* t (- b a))))

;;; lerp-inverse : Num × Num × Num → Num
;;; Inverse linear interpolation: find t such that lerp(a, b, t) = v
;;; Returns (v - a) / (b - a).
(define (lerp-inverse a b v)
  (doc 'export #t)
  (if (= a b)
      0.0
      (/ (- v a) (- b a))))

;;; interp-linear : (List (Num × Num)) × Num → Num
;;; Piecewise linear interpolation.
;;; Points are (x, y) pairs, must be sorted by x.
;;; For x outside the range, uses the nearest endpoint value.
;;; Uses binary search for O(log n) segment lookup.
(define (interp-linear points x)
  (doc 'export #t)
  (cond
   [(null? points) 0]
   [(null? (cdr points)) (cdar points)]
   [else
    (let* ([xs (map car points)]
           [n (length xs)]
           [i (binary-search-segment xs x)])
      (cond
       [(<= x (car xs)) (cdar points)]  ; Before first point
       [(>= x (list-ref xs (- n 1))) (cdr (list-ref points (- n 1)))]  ; Past last point
       [else
        ;; Interpolate in segment [i, i+1]
        (let ([p0 (list-ref points i)]
              [p1 (list-ref points (+ i 1))])
          (let ([x0 (car p0)]
                [y0 (cdr p0)]
                [x1 (car p1)]
                [y1 (cdr p1)])
            (lerp y0 y1 (lerp-inverse x0 x1 x))))]))]))

;;; ====
;;; Polynomial Interpolation (Lagrange)
;;; ====

;;; lagrange-basis : (List Num) × Int × Num → Num
;;; Compute the i-th Lagrange basis polynomial L_i(x).
;;; L_i(x) = ∏_{j≠i} (x - x_j) / (x_i - x_j)
(define (lagrange-basis xs i x)
  (doc 'export #t)
  (let ([xi (list-ref xs i)]
        [n (length xs)])
    (let loop ([j 0] [result 1.0])
      (if (>= j n)
          result
          (if (= j i)
              (loop (+ j 1) result)
              (let ([xj (list-ref xs j)])
                (loop (+ j 1)
                      (* result (/ (- x xj) (- xi xj))))))))))

;;; interp-lagrange : (List Num) × (List Num) × Num → Num
;;; Lagrange polynomial interpolation.
;;; Given x-values xs and y-values ys, evaluate at x.
;;; P(x) = Σ y_i × L_i(x)
(define (interp-lagrange xs ys x)
  (doc 'export #t)
  (let ([n (length xs)])
    (let loop ([i 0] [result 0.0])
      (if (>= i n)
          result
          (loop (+ i 1)
                (+ result (* (list-ref ys i)
                            (lagrange-basis xs i x))))))))

;;; ====
;;; Polynomial Interpolation (Newton's Divided Differences)
;;; ====

;;; divided-differences : (List Num) × (List Num) → (List Num)
;;; Compute Newton's divided differences table (first column only).
;;; Returns coefficients for Newton polynomial form.
(define (divided-differences xs ys)
  (doc 'export #t)
  (let* ([n (length xs)]
         [table (make-vector n 0)])
    ;; Initialize with y values
    (do ([i 0 (+ i 1)]
         [ys ys (cdr ys)])
        ((= i n))
      (vector-set! table i (car ys)))
    ;; Build divided differences
    (do ([j 1 (+ j 1)])
        ((= j n))
      (do ([i (- n 1) (- i 1)])
          ((< i j))
        (let ([xi (list-ref xs i)]
              [xij (list-ref xs (- i j))])
          (vector-set! table i
                       (/ (- (vector-ref table i)
                             (vector-ref table (- i 1)))
                          (- xi xij))))))
    ;; Extract first column (diagonal)
    (let loop ([i 0] [result '()])
      (if (= i n)
          (reverse result)
          (loop (+ i 1) (cons (vector-ref table i) result))))))

;;; interp-newton : (List Num) × (List Num) × Num → Num
;;; Newton polynomial interpolation using divided differences.
;;; More efficient for evaluating at multiple points.
(define (interp-newton xs ys x)
  (doc 'export #t)
  (let ([coeffs (divided-differences xs ys)]
        [n (length xs)])
    ;; Horner-like evaluation of Newton form
    (let loop ([i (- n 1)] [result 0.0])
      (if (< i 0)
          result
          (loop (- i 1)
                (+ (list-ref coeffs i)
                   (* result (- x (list-ref xs i)))))))))

;;; ====
;;; Hermite Interpolation
;;; ====

;;; interp-hermite : Num × Num × Num × Num × Num → Num
;;; Cubic Hermite interpolation between two points.
;;; Given positions p0, p1 and tangents m0, m1, evaluate at t ∈ [0,1].
;;; Uses Hermite basis functions:
;;;   h00(t) = 2t³ - 3t² + 1
;;;   h10(t) = t³ - 2t² + t
;;;   h01(t) = -2t³ + 3t²
;;;   h11(t) = t³ - t²
(define (interp-hermite p0 p1 m0 m1 t)
  (doc 'export #t)
  (let* ([t2 (* t t)]
         [t3 (* t2 t)]
         [h00 (+ (* 2 t3) (* -3 t2) 1)]
         [h10 (+ t3 (* -2 t2) t)]
         [h01 (+ (* -2 t3) (* 3 t2))]
         [h11 (+ t3 (- t2))])
    (+ (* h00 p0)
       (* h10 m0)
       (* h01 p1)
       (* h11 m1))))

;;; hermite-tangent-estimate : Num × Num × Num × Num → Num
;;; Estimate tangent at middle point using Catmull-Rom formula.
;;; Given three consecutive y-values and x-spacing.
(define (hermite-tangent-estimate y-prev y-curr y-next dx)
  (doc 'export #t)
  (/ (- y-next y-prev) (* 2 dx)))

;;; ====
;;; Cubic Spline Interpolation
;;; ====

;;; thomas-algorithm : (Vector Num) × (Vector Num) × (Vector Num) × (Vector Num) → (Vector Num)
;;; Solve tridiagonal system using Thomas algorithm (TDMA).
;;; a: sub-diagonal (length n-1, a[0] unused conceptually but we use 0-indexed)
;;; b: main diagonal (length n)
;;; c: super-diagonal (length n-1)
;;; d: right-hand side (length n)
;;; Returns solution x of length n.
;;; Time complexity: O(n), Space: O(n)
(define (thomas-algorithm a-sub b-diag c-sup d-rhs)
  (doc 'export #t)
  (let* ([n (vector-length b-diag)]
         [c-prime (make-vector n 0.0)]
         [d-prime (make-vector n 0.0)]
         [x (make-vector n 0.0)])
    (if (= n 0)
        x
        (begin
          ;; Forward sweep
          (vector-set! c-prime 0 (/ (vector-ref c-sup 0) (vector-ref b-diag 0)))
          (vector-set! d-prime 0 (/ (vector-ref d-rhs 0) (vector-ref b-diag 0)))
          (do ([i 1 (+ i 1)])
              ((>= i n))
            (let* ([ai (if (> i 0) (vector-ref a-sub (- i 1)) 0)]
                   [bi (vector-ref b-diag i)]
                   [ci (if (< i (- n 1)) (vector-ref c-sup i) 0)]
                   [di (vector-ref d-rhs i)]
                   [denom (- bi (* ai (vector-ref c-prime (- i 1))))])
              (when (< i (- n 1))
                (vector-set! c-prime i (/ ci denom)))
              (vector-set! d-prime i (/ (- di (* ai (vector-ref d-prime (- i 1)))) denom))))
          ;; Back substitution
          (vector-set! x (- n 1) (vector-ref d-prime (- n 1)))
          (do ([i (- n 2) (- i 1)])
              ((< i 0))
            (vector-set! x i (- (vector-ref d-prime i)
                               (* (vector-ref c-prime i) (vector-ref x (+ i 1))))))
          x))))

;;; cubic-spline-natural : (List Num) × (List Num) → (Vector (Num Num Num Num))
;;; Compute natural cubic spline coefficients.
;;; Returns vector of (a, b, c, d) for each segment where
;;; S_i(x) = a_i + b_i(x-x_i) + c_i(x-x_i)² + d_i(x-x_i)³
;;; Natural boundary: S''(x_0) = S''(x_n) = 0
;;; Uses Thomas algorithm for O(n) time complexity.
(define (cubic-spline-natural xs ys)
  (doc 'export #t)
  (let* ([n (length xs)]
         [n-1 (- n 1)])
    (if (< n 2)
        (vector)
        (let* (;; Compute h_i = x_{i+1} - x_i
               [h (let loop ([xs xs] [result '()])
                    (if (null? (cdr xs))
                        (list->vector (reverse result))
                        (loop (cdr xs)
                              (cons (- (cadr xs) (car xs)) result))))])
          ;; Solve for M (second derivatives at interior points) using Thomas algorithm
          (let* ([m (- n 2)]  ; Number of interior points
                 [M-interior
                  (if (< n 3)
                      (vector)
                      ;; Build tridiagonal system vectors
                      (let* ([a-sub (make-vector (- m 1) 0.0)]   ; sub-diagonal
                             [b-diag (make-vector m 0.0)]         ; main diagonal
                             [c-sup (make-vector (- m 1) 0.0)]    ; super-diagonal
                             [d-rhs (make-vector m 0.0)])         ; right-hand side
                        ;; Fill tridiagonal vectors
                        (do ([i 1 (+ i 1)])
                            ((>= i n-1))
                          (let* ([idx (- i 1)]
                                 [hi-1 (vector-ref h (- i 1))]
                                 [hi (vector-ref h i)]
                                 [yi-1 (list-ref ys (- i 1))]
                                 [yi (list-ref ys i)]
                                 [yi+1 (list-ref ys (+ i 1))])
                            ;; Main diagonal: 2(h_{i-1} + h_i)
                            (vector-set! b-diag idx (* 2 (+ hi-1 hi)))
                            ;; Sub-diagonal: h_{i-1} (for rows after first)
                            (when (> idx 0)
                              (vector-set! a-sub (- idx 1) hi-1))
                            ;; Super-diagonal: h_i (for rows before last)
                            (when (< idx (- m 1))
                              (vector-set! c-sup idx hi))
                            ;; RHS
                            (vector-set! d-rhs idx
                                        (* 6 (- (/ (- yi+1 yi) hi)
                                                (/ (- yi yi-1) hi-1))))))
                        ;; Solve with Thomas algorithm O(n) instead of matrix-solve O(n³)
                        (thomas-algorithm a-sub b-diag c-sup d-rhs)))]
                 ;; Full M vector with M_0 = M_n = 0
                 [M (vec-tabulate n i
                      (if (and (> i 0) (< i n-1))
                          (vector-ref M-interior (- i 1))
                          0))]
                 ;; Build spline coefficients
                 [spline (vec-tabulate n-1 i
                           (let* ([hi (vector-ref h i)]
                                  [yi (list-ref ys i)]
                                  [yi+1 (list-ref ys (+ i 1))]
                                  [Mi (vector-ref M i)]
                                  [Mi+1 (vector-ref M (+ i 1))]
                                  [a yi]
                                  [b (- (/ (- yi+1 yi) hi)
                                        (/ (* hi (+ (* 2 Mi) Mi+1)) 6))]
                                  [c (/ Mi 2)]
                                  [d (/ (- Mi+1 Mi) (* 6 hi))])
                             (list a b c d)))])
            spline)))))

;;; Helper for matrix-set! (not in base matrix.ss)
(define (matrix-set! m i j val)
  (let ([data (matrix-data m)]
        [cols (matrix-cols m)])
    (vector-set! data (+ (* i cols) j) val)))

;;; spline-eval : (Vector (Num Num Num Num)) × (List Num) × Num → Num
;;; Evaluate cubic spline at point x.
;;; Uses binary search for O(log n) segment lookup.
(define (spline-eval spline xs x)
  (doc 'export #t)
  (let ([n (vector-length spline)])
    (if (= n 0)
        0
        ;; Binary search for segment
        (let* ([n-pts (length xs)]
               [i (binary-search-segment-vec xs x n-pts)]
               ;; Clamp to valid segment range
               [seg-idx (max 0 (min i (- n 1)))]
               [coeffs (vector-ref spline seg-idx)]
               [xi (list-ref xs seg-idx)]
               [dx (- x xi)]
               [a (car coeffs)]
               [b (cadr coeffs)]
               [c (caddr coeffs)]
               [d (cadddr coeffs)])
          (+ a (* b dx) (* c dx dx) (* d dx dx dx))))))

;;; interp-cubic-spline : (List Num) × (List Num) × Num → Num
;;; Cubic spline interpolation with natural boundary conditions.
(define (interp-cubic-spline xs ys x)
  (doc 'export #t)
  (let ([spline (cubic-spline-natural xs ys)])
    (spline-eval spline xs x)))

;;; ====
;;; Bezier Curves
;;; ====

;;; bezier-linear : (Num × Num) × (Num × Num) × Num → (Num × Num)
;;; Linear Bezier (line segment).
(define (bezier-linear p0 p1 t)
  (doc 'export #t)
  (cons (lerp (car p0) (car p1) t)
        (lerp (cdr p0) (cdr p1) t)))

;;; bezier-quadratic : (Num × Num) × (Num × Num) × (Num × Num) × Num → (Num × Num)
;;; Quadratic Bezier curve.
;;; B(t) = (1-t)²P₀ + 2(1-t)tP₁ + t²P₂
(define (bezier-quadratic p0 p1 p2 t)
  (doc 'export #t)
  (let* ([u (- 1 t)]
         [u2 (* u u)]
         [t2 (* t t)]
         [c0 u2]
         [c1 (* 2 u t)]
         [c2 t2])
    (cons (+ (* c0 (car p0)) (* c1 (car p1)) (* c2 (car p2)))
          (+ (* c0 (cdr p0)) (* c1 (cdr p1)) (* c2 (cdr p2))))))

;;; bezier-cubic : (Num × Num) × (Num × Num) × (Num × Num) × (Num × Num) × Num → (Num × Num)
;;; Cubic Bezier curve.
;;; B(t) = (1-t)³P₀ + 3(1-t)²tP₁ + 3(1-t)t²P₂ + t³P₃
(define (bezier-cubic p0 p1 p2 p3 t)
  (doc 'export #t)
  (let* ([u (- 1 t)]
         [u2 (* u u)]
         [u3 (* u2 u)]
         [t2 (* t t)]
         [t3 (* t2 t)]
         [c0 u3]
         [c1 (* 3 u2 t)]
         [c2 (* 3 u t2)]
         [c3 t3])
    (cons (+ (* c0 (car p0)) (* c1 (car p1)) (* c2 (car p2)) (* c3 (car p3)))
          (+ (* c0 (cdr p0)) (* c1 (cdr p1)) (* c2 (cdr p2)) (* c3 (cdr p3))))))

;;; bezier-general : (List (Num × Num)) × Num → (Num × Num)
;;; General Bezier curve of arbitrary degree using de Casteljau's algorithm.
(define (bezier-general control-points t)
  (doc 'export #t)
  (if (null? control-points)
      '(0 . 0)
      (let decasteljau ([points control-points])
        (if (null? (cdr points))
            (car points)
            (decasteljau
             (let loop ([ps points] [result '()])
               (if (null? (cdr ps))
                   (reverse result)
                   (loop (cdr ps)
                         (cons (bezier-linear (car ps) (cadr ps) t)
                               result)))))))))

;;; bezier-derivative : (List (Num × Num)) × Num → (Num × Num)
;;; Compute derivative (tangent vector) of Bezier curve at t.
(define (bezier-derivative control-points t)
  (doc 'export #t)
  (let ([n (- (length control-points) 1)])
    (if (< n 1)
        '(0 . 0)
        ;; Derivative control points: n(P_{i+1} - P_i)
        (let ([deriv-points
               (let loop ([ps control-points] [result '()])
                 (if (null? (cdr ps))
                     (reverse result)
                     (loop (cdr ps)
                           (cons (cons (* n (- (caadr ps) (caar ps)))
                                       (* n (- (cdadr ps) (cdar ps))))
                                 result))))])
          (bezier-general deriv-points t)))))

;;; ====
;;; Least Squares Fitting
;;; ====

;;; polyfit : (List Num) × (List Num) × Int → Poly
;;; Fit polynomial of degree n to data points using least squares.
;;; Returns polynomial in descending power order.
;;;
;;; WARNING: Uses normal equations (A^T A)c = A^T b which can be numerically
;;; unstable for high-degree polynomials (degree > 5-7). The condition number
;;; of the Vandermonde-like matrix grows exponentially. For high-degree fits,
;;; consider Chebyshev approximation or orthogonal polynomial bases instead.
(define (polyfit xs ys degree)
  (doc 'export #t)
  (let* ([n (length xs)]
         [m (+ degree 1)]
         ;; Build Vandermonde-like matrix A where A[i,j] = x_i^j
         [A (make-matrix n m 0)]
         [b (make-matrix n 1 0)])
    ;; Fill matrices
    (do ([i 0 (+ i 1)]
         [xs xs (cdr xs)]
         [ys ys (cdr ys)])
        ((= i n))
      (let ([xi (car xs)]
            [yi (car ys)])
        (matrix-set! b i 0 yi)
        (let loop ([j 0] [xij 1])
          (when (< j m)
            (matrix-set! A i (- m j 1) xij)
            (loop (+ j 1) (* xij xi))))))
    ;; Solve least squares: (A^T A) c = A^T b
    (let* ([AT (matrix-transpose A)]
           [ATA (matrix-mul AT A)]
           [ATb (matrix-mul AT b)]
           ;; Convert ATb column matrix to vector for matrix-solve
           [ATb-vec (matrix-col ATb 0)]
           [coeffs (matrix-solve ATA ATb-vec)])
      ;; coeffs is now a vector, convert to polynomial
      (make-poly coeffs))))

;;; linreg : (List Num) × (List Num) → (Num × Num)
;;; Simple linear regression: fit y = mx + b.
;;; Returns (slope . intercept).
(define (linreg xs ys)
  (doc 'export #t)
  (let* ([n (length xs)]
         [sum-x (apply + xs)]
         [sum-y (apply + ys)]
         [sum-xx (apply + (map (lambda (x) (* x x)) xs))]
         [sum-xy (apply + (map * xs ys))]
         [denom (- (* n sum-xx) (* sum-x sum-x))]
         [m (/ (- (* n sum-xy) (* sum-x sum-y)) denom)]
         [b (/ (- (* sum-xx sum-y) (* sum-x sum-xy)) denom)])
    (cons m b)))

;;; linreg-r2 : (List Num) × (List Num) → Num
;;; Compute R² (coefficient of determination) for linear regression.
(define (linreg-r2 xs ys)
  (doc 'export #t)
  (let* ([reg (linreg xs ys)]
         [m (car reg)]
         [b (cdr reg)]
         [y-mean (/ (apply + ys) (length ys))]
         [ss-tot (apply + (map (lambda (y) (expt (- y y-mean) 2)) ys))]
         [ss-res (apply + (map (lambda (x y)
                                 (expt (- y (+ (* m x) b)) 2))
                               xs ys))])
    (if (zero? ss-tot)
        1.0
        (- 1 (/ ss-res ss-tot)))))

;;; ====
;;; Chebyshev Approximation
;;; ====

;;; chebyshev-nodes : Int → (List Num)
;;; Generate n Chebyshev nodes in [-1, 1].
;;; x_k = cos((2k+1)π/(2n)) for k = 0, ..., n-1
(define (chebyshev-nodes n)
  (doc 'export #t)
  (let ([pi 3.141592653589793])
    (let loop ([k 0] [result '()])
      (if (= k n)
          (reverse result)
          (loop (+ k 1)
                (cons (cos (/ (* (+ (* 2 k) 1) pi) (* 2 n)))
                      result))))))

;;; chebyshev-nodes-interval : Int × Num × Num → (List Num)
;;; Generate n Chebyshev nodes in interval [a, b].
(define (chebyshev-nodes-interval n a b)
  (doc 'export #t)
  (let ([nodes (chebyshev-nodes n)])
    (map (lambda (x)
           (+ (* 0.5 (- b a) (+ x 1)) a))
         nodes)))

;;; chebyshev-t : Int × Num → Num
;;; Compute Chebyshev polynomial T_n(x) using recurrence.
(define (chebyshev-t n x)
  (doc 'export #t)
  (cond
   [(= n 0) 1]
   [(= n 1) x]
   [else
    (let loop ([i 2] [t0 1] [t1 x])
      (if (> i n)
          t1
          (loop (+ i 1) t1 (- (* 2 x t1) t0))))]))

;;; chebyshev-coeffs : (Num → Num) × Int → (List Num)
;;; Compute Chebyshev expansion coefficients for function f.
;;; Approximates f on [-1, 1] with n terms.
(define (chebyshev-coeffs f n)
  (doc 'export #t)
  (let ([nodes (chebyshev-nodes n)]
        [pi 3.141592653589793])
    (let loop ([k 0] [result '()])
      (if (= k n)
          (reverse result)
          (let ([sum (apply + (map (lambda (x)
                                     (* (f x) (chebyshev-t k x)))
                                   nodes))])
            (loop (+ k 1)
                  (cons (* (/ (if (= k 0) 1 2) n) sum)
                        result)))))))

;;; chebyshev-eval : (List Num) × Num → Num
;;; Evaluate Chebyshev series at point x using Clenshaw's algorithm.
(define (chebyshev-eval coeffs x)
  (doc 'export #t)
  (let ([n (length coeffs)])
    (if (= n 0)
        0
        (let loop ([i (- n 1)] [b1 0] [b2 0])
          (if (< i 0)
              (- b1 (* x b2))
              (loop (- i 1)
                    (+ (list-ref coeffs i) (* 2 x b1) (- b2))
                    b1))))))

;;; ====
;;; B-Spline Basis Functions
;;; ====

;;; bspline-basis : (Vector Num) × Int × Int × Num → Num
;;; Compute B-spline basis function N_{i,p}(x).
;;; knots: knot vector, i: index, p: degree, x: evaluation point.
;;; Uses Cox-de Boor recursion.
(define (bspline-basis knots i p x)
  (doc 'export #t)
  (let ([n (vector-length knots)])
    (cond
     [(= p 0)
      ;; Base case: characteristic function of [t_i, t_{i+1})
      (let ([ti (vector-ref knots i)]
            [ti+1 (if (< (+ i 1) n)
                      (vector-ref knots (+ i 1))
                      (+ ti 1))])  ; Extend if needed
        (if (and (>= x ti) (< x ti+1))
            1.0
            0.0))]
     [else
      ;; Recursive case
      (let* ([ti (vector-ref knots i)]
             [ti+1 (vector-ref knots (+ i 1))]
             [ti+p (if (< (+ i p) n)
                       (vector-ref knots (+ i p))
                       (+ ti+1 1))]
             [ti+p+1 (if (< (+ i p 1) n)
                         (vector-ref knots (+ i p 1))
                         (+ ti+p 1))]
             [denom1 (- ti+p ti)]
             [denom2 (- ti+p+1 ti+1)]
             [term1 (if (zero? denom1)
                        0.0
                        (* (/ (- x ti) denom1)
                           (bspline-basis knots i (- p 1) x)))]
             [term2 (if (zero? denom2)
                        0.0
                        (* (/ (- ti+p+1 x) denom2)
                           (bspline-basis knots (+ i 1) (- p 1) x)))])
        (+ term1 term2))])))

;;; bspline-curve : (Vector Num) × (List (Num × Num)) × Int × Num → (Num × Num)
;;; Evaluate B-spline curve at parameter t.
;;; knots: knot vector, control-points: control polygon, p: degree
(define (bspline-curve knots control-points p t)
  (doc 'export #t)
  (let ([n (length control-points)])
    (let loop ([i 0] [x 0.0] [y 0.0])
      (if (= i n)
          (cons x y)
          (let ([basis (bspline-basis knots i p t)]
                [pi (list-ref control-points i)])
            (loop (+ i 1)
                  (+ x (* basis (car pi)))
                  (+ y (* basis (cdr pi)))))))))

;;; ====
;;; Export List (for reference)
;;; ====

;;; Exports:
;;;   ;; Linear interpolation
;;;   lerp lerp-inverse interp-linear
;;;   ;; Polynomial interpolation
;;;   lagrange-basis interp-lagrange
;;;   divided-differences interp-newton
;;;   ;; Hermite interpolation
;;;   interp-hermite hermite-tangent-estimate
;;;   ;; Cubic splines
;;;   cubic-spline-natural spline-eval interp-cubic-spline
;;;   ;; Bezier curves
;;;   bezier-linear bezier-quadratic bezier-cubic
;;;   bezier-general bezier-derivative
;;;   ;; Least squares
;;;   polyfit linreg linreg-r2
;;;   ;; Chebyshev
;;;   chebyshev-nodes chebyshev-nodes-interval
;;;   chebyshev-t chebyshev-coeffs chebyshev-eval
;;;   ;; B-splines
;;;   bspline-basis bspline-curve
