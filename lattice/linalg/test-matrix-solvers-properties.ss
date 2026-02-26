;;; lattice/linalg/test-matrix-solvers-properties.ss — QuickCheck properties for matrix solvers

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'matrix-decomp)
(require 'matrix-solvers)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (matrix-error? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (vector-approx=? v1 v2 tol)
  (and (= (vector-length v1) (vector-length v2))
       (let loop ([i 0] [n (vector-length v1)])
         (or (= i n)
             (and (approx= (vector-ref v1 i) (vector-ref v2 i) tol)
                  (loop (+ i 1) n))))))

(define (list-replace xs i val)
  (let loop ([rest xs] [k 0] [acc '()])
    (if (null? rest)
        (reverse acc)
        (loop (cdr rest)
              (+ k 1)
              (cons (if (= k i) val (car rest)) acc)))))

(define (strictly-diagonal-dominant-rows rows)
  (let loop ([rs rows] [i 0] [acc '()])
    (if (null? rs)
        (reverse acc)
        (let* ([row (car rs)]
               [offdiag-sum
                (let sum-loop ([cs row] [j 0] [s 0])
                  (if (null? cs)
                      s
                      (sum-loop (cdr cs)
                                (+ j 1)
                                (+ s (if (= j i) 0 (abs (car cs)))))))]
               [new-row (list-replace row i (+ offdiag-sum 1))])
          (loop (cdr rs) (+ i 1) (cons new-row acc))))))

(define (make-invertible-matrix rows)
  (matrix-from-lists (strictly-diagonal-dominant-rows rows)))

(define (toeplitz-from-vector r)
  (let* ([n (vector-length r)])
    (matrix-from-lists
     (let row-loop ([i 0] [acc '()])
       (if (= i n)
           (reverse acc)
           (row-loop (+ i 1)
                     (cons (let col-loop ([j 0] [row '()])
                             (if (= j n)
                                 (reverse row)
                                 (col-loop (+ j 1)
                                           (cons (vector-ref r (abs (- i j))) row))))
                           acc)))))))

(define (acf-vector len rho)
  (let ([v (make-vector len 0.0)])
    (do ([k 0 (+ k 1)])
        [(= k len) v]
        (vector-set! v k (expt rho k)))))

(define (vector-tail v start)
  (let* ([n (vector-length v)]
         [out (make-vector (- n start) 0)])
    (do ([i start (+ i 1)])
        [(= i n) out]
        (vector-set! out (- i start) (vector-ref v i)))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-size
  (gen-int-range 1 4))

(define gen-entry
  (gen-int-range -6 6))

(define (gen-square-lists n)
  (gen-list-of n (gen-list-of n gen-entry)))

(define gen-invertible-matrix
  (gen-bind gen-size
    (lambda (n)
      (gen-map make-invertible-matrix
               (gen-square-lists n)))))

(define gen-solve-args
  (gen-bind gen-size
    (lambda (n)
      (gen-bind (gen-square-lists n)
        (lambda (rows)
          (let ([a (make-invertible-matrix rows)])
            (gen-map (lambda (xs) (list a (list->vector xs)))
                     (gen-list-of n gen-entry))))))))

(define gen-rect-matrix
  (gen-bind (gen-int-range 0 5)
    (lambda (rows)
      (gen-bind (gen-int-range 0 5)
        (lambda (cols)
          (gen-map matrix-from-lists
                   (gen-list-of rows (gen-list-of cols gen-entry))))))))

(define gen-rho
  (gen-map (lambda (k) (/ k 100.0))
           (gen-int-range -80 80)))

(define gen-levinson-general-args
  (gen-bind (gen-int-range 1 4)
    (lambda (n)
      (gen-bind gen-rho
        (lambda (rho)
          (gen-map (lambda (xs) (list n rho (list->vector xs)))
                   (gen-list-of n gen-entry)))))))

(define gen-levinson-yw-args
  (gen-bind (gen-int-range 1 4)
    (lambda (p)
      (gen-map (lambda (rho) (list p rho))
               gen-rho))))

;;; ============================================================================
;;; Core solver properties
;;; ============================================================================

(test-group matrix-solver-core-properties

  (define-property "matrix-solve recovers known x for invertible A"
    gen-solve-args
    (lambda (args)
      (let* ([a (car args)]
             [x-true (cadr args)]
             [b (matrix-vec-mul a x-true)]
             [x (matrix-solve a b)])
        (and (vector? x)
             (vector-approx=? x x-true 1e-7))))
    'tests 200)

  (define-property "matrix-inverse satisfies A*A^-1 ≈ I and A^-1*A ≈ I"
    gen-invertible-matrix
    (lambda (a)
      (let* ([inv (matrix-inverse a)]
             [n (matrix-rows a)]
             [i (matrix-identity n)])
        (and (not (matrix-error? inv))
             (let ([left (matrix-mul a inv)]
                   [right (matrix-mul inv a)])
               (and (not (matrix-error? left))
                    (not (matrix-error? right))
                    (matrix-approx-equal? left i 1e-7)
                    (matrix-approx-equal? right i 1e-7))))))
    'tests 180)

  (define-property "matrix-determinant agrees with matrix-det"
    gen-invertible-matrix
    (lambda (a)
      (approx= (matrix-determinant a) (matrix-det a) 1e-7))
    'tests 180)

  (define-property "matrix-least-squares matches exact solutions when b = Ax"
    gen-solve-args
    (lambda (args)
      (let* ([a (car args)]
             [x-true (cadr args)]
             [b (matrix-vec-mul a x-true)]
             [x (matrix-least-squares a b)])
        (and (vector? x)
             (vector-approx=? x x-true 1e-6))))
    'tests 180)
)

;;; ============================================================================
;;; Rank and conditioning properties
;;; ============================================================================

(test-group matrix-rank-conditioning-properties

  (define-property "rank is bounded by matrix dimensions"
    gen-rect-matrix
    (lambda (a)
      (let* ([r (matrix-rank a)]
             [rows (matrix-rows a)]
             [cols (matrix-cols a)])
        (and (>= r 0)
             (<= r (min rows cols)))))
    'tests 220)

  (define-property "rank(identity n) = n"
    (gen-int-range 0 6)
    (lambda (n)
      (= (matrix-rank (matrix-identity n)) n))
    'tests 100)

  (define-property "condition number of invertible matrix is >= 1"
    gen-invertible-matrix
    (lambda (a)
      (let ([kappa (matrix-condition-number a)])
        (and (number? kappa)
             (>= kappa 1.0))))
    'tests 160)
)

;;; ============================================================================
;;; Toeplitz / Levinson-Durbin properties
;;; ============================================================================

(test-group levinson-durbin-properties

  (define-property "levinson-durbin-general matches matrix-solve on Toeplitz SPD systems"
    gen-levinson-general-args
    (lambda (args)
      (let* ([n (car args)]
             [rho (cadr args)]
             [x-true (caddr args)]
             [r (acf-vector n rho)]
             [t (toeplitz-from-vector r)]
             [b (matrix-vec-mul t x-true)]
             [x (levinson-durbin-general r b)])
        (and (vector? x)
             (vector-approx=? x x-true 1e-6))))
    'tests 160)

  (define-property "levinson-durbin matches Yule-Walker solve for AR coefficients"
    gen-levinson-yw-args
    (lambda (args)
      (let* ([p (car args)]
             [rho (cadr args)]
             [r-full (acf-vector (+ p 1) rho)]
             [r-top (let ([v (make-vector p 0.0)])
                      (do ([i 0 (+ i 1)])
                          [(= i p) v]
                          (vector-set! v i (vector-ref r-full i))))]
             [rhs (vector-tail r-full 1)]
             [t (toeplitz-from-vector r-top)]
             [phi1 (levinson-durbin r-full)]
             [phi2 (matrix-solve t rhs)])
        (and (vector? phi1)
             (vector? phi2)
             (vector-approx=? phi1 phi2 1e-6))))
    'tests 140)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
