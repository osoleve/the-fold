;;; lattice/numeric/interpolate.ss — Interpolation and Curve Fitting
;;; @module interpolate
;;; @requires prelude, vec, matrix, matrix-solvers, polynomial
;;;
;;; Numerical interpolation, splines, Bezier curves, and curve fitting.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/linalg/vec.ss
;;;   - lattice/linalg/matrix.ss
;;;   - lattice/linalg/matrix-solvers.ss
;;;   - lattice/numeric/polynomial.ss

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-decomp.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/numeric/polynomial.ss")

;;; ============================================================
;;; Linear Interpolation
;;; ============================================================

;;; lerp : Num × Num × Num → Num
;;; Linear interpolation: lerp(a, b, t) = a + t×(b - a)
;;; When t=0 returns a, when t=1 returns b.
(define (lerp a b t)
  (+ a (* t (- b a))))

;;; lerp-inverse : Num × Num × Num → Num
;;; Inverse linear interpolation: find t such that lerp(a, b, t) = v
;;; Returns (v - a) / (b - a).
(define (lerp-inverse a b v)
  (if (= a b)
      0.0
      (/ (- v a) (- b a))))

;;; interp-linear : (List (Num × Num)) × Num → Num
;;; Piecewise linear interpolation.
;;; Points are (x, y) pairs, must be sorted by x.
;;; For x outside the range, uses the nearest endpoint value.
(define (interp-linear points x)
  (cond
   [(null? points) 0]
   [(null? (cdr points)) (cdar points)]
   [else
    (let find-segment ([ps points])
      (cond
       [(null? (cdr ps)) (cdar ps)]  ; Past last point
       [(<= x (caar ps)) (cdar ps)]  ; Before first point
       [(< x (caadr ps))
        ;; Found segment
        (let ([x0 (caar ps)]
              [y0 (cdar ps)]
              [x1 (caadr ps)]
              [y1 (cdadr ps)])
          (lerp y0 y1 (lerp-inverse x0 x1 x)))]
       [else
        (find-segment (cdr ps))]))]))

;;; ============================================================
;;; Polynomial Interpolation (Lagrange)
;;; ============================================================

;;; lagrange-basis : (List Num) × Int × Num → Num
;;; Compute the i-th Lagrange basis polynomial L_i(x).
;;; L_i(x) = ∏_{j≠i} (x - x_j) / (x_i - x_j)
(define (lagrange-basis xs i x)
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
  (let ([n (length xs)])
    (let loop ([i 0] [result 0.0])
      (if (>= i n)
          result
          (loop (+ i 1)
                (+ result (* (list-ref ys i)
                            (lagrange-basis xs i x))))))))

;;; ============================================================
;;; Polynomial Interpolation (Newton's Divided Differences)
;;; ============================================================

;;; divided-differences : (List Num) × (List Num) → (List Num)
;;; Compute Newton's divided differences table (first column only).
;;; Returns coefficients for Newton polynomial form.
(define (divided-differences xs ys)
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
  (let ([coeffs (divided-differences xs ys)]
        [n (length xs)])
    ;; Horner-like evaluation of Newton form
    (let loop ([i (- n 1)] [result 0.0])
      (if (< i 0)
          result
          (loop (- i 1)
                (+ (list-ref coeffs i)
                   (* result (- x (list-ref xs i)))))))))

;;; ============================================================
;;; Hermite Interpolation
;;; ============================================================

;;; interp-hermite : Num × Num × Num × Num × Num → Num
;;; Cubic Hermite interpolation between two points.
;;; Given positions p0, p1 and tangents m0, m1, evaluate at t ∈ [0,1].
;;; Uses Hermite basis functions:
;;;   h00(t) = 2t³ - 3t² + 1
;;;   h10(t) = t³ - 2t² + t
;;;   h01(t) = -2t³ + 3t²
;;;   h11(t) = t³ - t²
(define (interp-hermite p0 p1 m0 m1 t)
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
  (/ (- y-next y-prev) (* 2 dx)))

;;; ============================================================
;;; Cubic Spline Interpolation
;;; ============================================================

;;; cubic-spline-natural : (List Num) × (List Num) → (Vector (Num Num Num Num))
;;; Compute natural cubic spline coefficients.
;;; Returns vector of (a, b, c, d) for each segment where
;;; S_i(x) = a_i + b_i(x-x_i) + c_i(x-x_i)² + d_i(x-x_i)³
;;; Natural boundary: S''(x_0) = S''(x_n) = 0
(define (cubic-spline-natural xs ys)
  (let* ([n (length xs)]
         [n-1 (- n 1)])
    (if (< n 2)
        (vector)
        (let* (;; Compute h_i = x_{i+1} - x_i
               [h (let loop ([xs xs] [result '()])
                    (if (null? (cdr xs))
                        (list->vector (reverse result))
                        (loop (cdr xs)
                              (cons (- (cadr xs) (car xs)) result))))]
               ;; Set up tridiagonal system for second derivatives M
               ;; Natural spline: M_0 = M_n = 0
               ;; For i=1..n-1: h_{i-1}M_{i-1} + 2(h_{i-1}+h_i)M_i + h_iM_{i+1} = 6(...)
               [a (make-matrix (- n 2) (- n 2) 0)]
               [b (make-vector (- n 2) 0)])
          ;; Build system
          (when (>= n 3)
            (do ([i 1 (+ i 1)])
                ((>= i n-1))
              (let ([hi-1 (vector-ref h (- i 1))]
                    [hi (vector-ref h i)]
                    [yi-1 (list-ref ys (- i 1))]
                    [yi (list-ref ys i)]
                    [yi+1 (list-ref ys (+ i 1))])
                ;; Diagonal
                (matrix-set! a (- i 1) (- i 1) (* 2 (+ hi-1 hi)))
                ;; Sub/super diagonal
                (when (> i 1)
                  (matrix-set! a (- i 1) (- i 2) hi-1))
                (when (< i (- n 2))
                  (matrix-set! a (- i 1) i hi))
                ;; RHS
                (vector-set! b (- i 1)
                             (* 6 (- (/ (- yi+1 yi) hi)
                                     (/ (- yi yi-1) hi-1)))))))
          ;; Solve for M (second derivatives at interior points)
          (let* ([M-interior (if (< n 3)
                                 (vector)
                                 (matrix-solve a b))]
                 ;; Full M vector with M_0 = M_n = 0
                 [M (let ([v (make-vector n 0)])
                      (do ([i 1 (+ i 1)])
                          ((>= i n-1))
                        (vector-set! v i (vector-ref M-interior (- i 1))))
                      v)]
                 ;; Build spline coefficients
                 [spline (make-vector n-1 #f)])
            (do ([i 0 (+ i 1)])
                ((>= i n-1))
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
                (vector-set! spline i (list a b c d))))
            spline)))))

;;; Helper for matrix-set! (not in base matrix.ss)
(define (matrix-set! m i j val)
  (let ([data (matrix-data m)]
        [cols (matrix-cols m)])
    (vector-set! data (+ (* i cols) j) val)))

;;; spline-eval : (Vector (Num Num Num Num)) × (List Num) × Num → Num
;;; Evaluate cubic spline at point x.
(define (spline-eval spline xs x)
  (let ([n (vector-length spline)])
    (if (= n 0)
        0
        ;; Find segment
        (let find ([i 0])
          (cond
           [(>= i (- n 1))
            ;; Use last segment
            (let* ([coeffs (vector-ref spline (- n 1))]
                   [xi (list-ref xs (- n 1))]
                   [dx (- x xi)]
                   [a (car coeffs)]
                   [b (cadr coeffs)]
                   [c (caddr coeffs)]
                   [d (cadddr coeffs)])
              (+ a (* b dx) (* c dx dx) (* d dx dx dx)))]
           [(< x (list-ref xs (+ i 1)))
            ;; Found segment
            (let* ([coeffs (vector-ref spline i)]
                   [xi (list-ref xs i)]
                   [dx (- x xi)]
                   [a (car coeffs)]
                   [b (cadr coeffs)]
                   [c (caddr coeffs)]
                   [d (cadddr coeffs)])
              (+ a (* b dx) (* c dx dx) (* d dx dx dx)))]
           [else
            (find (+ i 1))])))))

;;; interp-cubic-spline : (List Num) × (List Num) × Num → Num
;;; Cubic spline interpolation with natural boundary conditions.
(define (interp-cubic-spline xs ys x)
  (let ([spline (cubic-spline-natural xs ys)])
    (spline-eval spline xs x)))

;;; ============================================================
;;; Bezier Curves
;;; ============================================================

;;; bezier-linear : (Num × Num) × (Num × Num) × Num → (Num × Num)
;;; Linear Bezier (line segment).
(define (bezier-linear p0 p1 t)
  (cons (lerp (car p0) (car p1) t)
        (lerp (cdr p0) (cdr p1) t)))

;;; bezier-quadratic : (Num × Num) × (Num × Num) × (Num × Num) × Num → (Num × Num)
;;; Quadratic Bezier curve.
;;; B(t) = (1-t)²P₀ + 2(1-t)tP₁ + t²P₂
(define (bezier-quadratic p0 p1 p2 t)
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

;;; ============================================================
;;; Least Squares Fitting
;;; ============================================================

;;; polyfit : (List Num) × (List Num) × Int → Poly
;;; Fit polynomial of degree n to data points using least squares.
;;; Returns polynomial in descending power order.
(define (polyfit xs ys degree)
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

;;; ============================================================
;;; Chebyshev Approximation
;;; ============================================================

;;; chebyshev-nodes : Int → (List Num)
;;; Generate n Chebyshev nodes in [-1, 1].
;;; x_k = cos((2k+1)π/(2n)) for k = 0, ..., n-1
(define (chebyshev-nodes n)
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
  (let ([nodes (chebyshev-nodes n)])
    (map (lambda (x)
           (+ (* 0.5 (- b a) (+ x 1)) a))
         nodes)))

;;; chebyshev-t : Int × Num → Num
;;; Compute Chebyshev polynomial T_n(x) using recurrence.
(define (chebyshev-t n x)
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
  (let ([n (length coeffs)])
    (if (= n 0)
        0
        (let loop ([i (- n 1)] [b1 0] [b2 0])
          (if (< i 0)
              (- b1 (* x b2))
              (loop (- i 1)
                    (+ (list-ref coeffs i) (* 2 x b1) (- b2))
                    b1))))))

;;; ============================================================
;;; B-Spline Basis Functions
;;; ============================================================

;;; bspline-basis : (Vector Num) × Int × Int × Num → Num
;;; Compute B-spline basis function N_{i,p}(x).
;;; knots: knot vector, i: index, p: degree, x: evaluation point.
;;; Uses Cox-de Boor recursion.
(define (bspline-basis knots i p x)
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
  (let ([n (length control-points)])
    (let loop ([i 0] [x 0.0] [y 0.0])
      (if (= i n)
          (cons x y)
          (let ([basis (bspline-basis knots i p t)]
                [pi (list-ref control-points i)])
            (loop (+ i 1)
                  (+ x (* basis (car pi)))
                  (+ y (* basis (cdr pi)))))))))

;;; ============================================================
;;; Export List (for reference)
;;; ============================================================

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
