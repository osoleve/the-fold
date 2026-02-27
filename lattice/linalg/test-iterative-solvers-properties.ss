;;; lattice/linalg/test-iterative-solvers-properties.ss — QuickCheck properties for iterative solvers

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'vec)
(require 'matrix)
(require 'iterative-solvers)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (diag-matrix ds)
  (let ([n (length ds)])
    (let loop-i ([i 0] [rows '()])
      (if (= i n)
          (matrix-from-lists (reverse rows))
          (let loop-j ([j 0] [row '()])
            (if (= j n)
                (loop-i (+ i 1) (cons (reverse row) rows))
                (loop-j (+ j 1)
                        (cons (if (= i j) (list-ref ds i) 0) row))))))))

(define (vec-approx=? x y tol)
  (let ([n (vector-length x)])
    (and (= n (vector-length y))
         (let loop ([i 0])
           (or (= i n)
               (and (approx= (vector-ref x i) (vector-ref y i) tol)
                    (loop (+ i 1))))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-diagonal-system
  (gen-bind (gen-int-range 1 6)
    (lambda (n)
      (gen-bind (gen-list-of n (gen-int-range 1 12))
        (lambda (diag)
          (gen-map (lambda (b-list)
                     (list (diag-matrix diag) diag (list->vector b-list)))
                   (gen-list-of n (gen-int-range -30 30))))))))

(define gen-diagonal-system-with-x
  (gen-bind gen-diagonal-system
    (lambda (sys)
      (let ([n (length (cadr sys))])
        (gen-map (lambda (x-list)
                   (list (car sys) (cadr sys) (caddr sys) (list->vector x-list)))
                 (gen-list-of n (gen-int-range -10 10)))))))

(define gen-spd-system
  (gen-bind (gen-int-range 1 6)
    (lambda (n)
      (gen-bind (gen-list-of n (gen-int-range 1 20))
        (lambda (diag)
          (gen-map (lambda (b-list)
                     (list (diag-matrix diag) diag (list->vector b-list)))
                   (gen-list-of n (gen-int-range -20 20))))))))

(define gen-givens-args
  (gen-bind (gen-int-range -50 50)
    (lambda (a)
      (gen-bind (gen-int-range -50 50)
        (lambda (b)
          (if (and (= a 0) (= b 0))
              (gen-pure (list 1 0))
              (gen-pure (list a b))))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group iterative-solver-properties

  (define-property "jacobi solves diagonal systems to near-zero residual"
    gen-diagonal-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [b (caddr sys)]
             [n (length diag)]
             [x0 (make-vector n 0.0)]
             [result (jacobi a b x0 60 1e-12)])
        (and (not (error-result? result))
             (let* ([x (car result)]
                    [res (caddr result)]
                    [expected (list->vector
                               (map (lambda (d bi) (/ bi d)) diag (vector->list b)))])
               (and (< res 1e-8)
                    (vec-approx=? x expected 1e-7))))))
    'tests 140)

  (define-property "gauss-seidel solves diagonal systems to near-zero residual"
    gen-diagonal-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [b (caddr sys)]
             [n (length diag)]
             [x0 (make-vector n 0.0)]
             [result (gauss-seidel a b x0 60 1e-12)])
        (and (not (error-result? result))
             (let* ([x (car result)]
                    [res (caddr result)]
                    [expected (list->vector
                               (map (lambda (d bi) (/ bi d)) diag (vector->list b)))])
               (and (< res 1e-8)
                    (vec-approx=? x expected 1e-7))))))
    'tests 140)

  (define-property "residual matches ||Ax-b|| definition"
    gen-diagonal-system-with-x
    (lambda (args)
      (let* ([a (car args)]
             [b (caddr args)]
             [x (cadddr args)]
             [lhs (residual a x b)]
             [rhs (vec-norm (vec-sub (matrix-vec-mul a x) b))])
        (approx= lhs rhs 1e-12)))
    'tests 180)

  (define-property "converged? matches residual < tolerance predicate"
    (gen-bind (gen-int-range 0 10000)
      (lambda (r-int)
        (gen-map (lambda (t-int)
                   (list (/ r-int 1000.0) (/ t-int 1000.0)))
                 (gen-int-range 0 10000))))
    (lambda (args)
      (let ([r (car args)]
            [tol (cadr args)])
        (equal? (converged? r tol) (< r tol))))
    'tests 220)

  (define-property "sor solves diagonal systems for omega in (0,2)"
    (gen-bind gen-diagonal-system
      (lambda (sys)
        (gen-map (lambda (omega-int)
                   (list (car sys) (cadr sys) (caddr sys) (/ omega-int 100.0)))
                 (gen-int-range 50 180))))
    (lambda (args)
      (let* ([a (car args)]
             [diag (cadr args)]
             [b (caddr args)]
             [omega (cadddr args)]
             [n (length diag)]
             [x0 (make-vector n 0.0)]
             [result (sor a b omega x0 120 1e-8)])
        (and (not (error-result? result))
             (< (caddr result) 1e-6))))
    'tests 120)

  (define-property "conjugate-gradient solves SPD diagonal systems"
    gen-spd-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [b (caddr sys)]
             [n (length diag)]
             [x0 (make-vector n 0.0)]
             [result (conjugate-gradient a b x0 n 1e-12)])
        (and (not (error-result? result))
             (let* ([x (car result)]
                    [expected (list->vector
                               (map (lambda (d bi) (/ bi d)) diag (vector->list b)))])
               (vec-approx=? x expected 1e-6)))))
    'tests 120)

  (define-property "pcg with Jacobi preconditioner solves SPD diagonal systems"
    gen-spd-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [b (caddr sys)]
             [n (length diag)]
             [solve-precond (make-jacobi-preconditioner a)]
             [result (pcg a b solve-precond (make-vector n 0.0) n 1e-12)])
        (and (not (error-result? result))
             (let* ([x (car result)]
                    [expected (list->vector
                               (map (lambda (d bi) (/ bi d)) diag (vector->list b)))])
               (vec-approx=? x expected 1e-6)))))
    'tests 120)

  (define-property "gmres returns well-formed result on diagonal systems"
    gen-diagonal-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [b (caddr sys)]
             [n (length diag)]
             [x0 (make-vector n 0.0)]
             [result (gmres a b n x0 (* 4 n) 1e-8)])
        (if (error-result? result)
            (not (null? result))
            (let ([x (car result)]
                  [res (caddr result)])
              (and (= (vector-length x) n)
                   (>= res 0.0))))))
    'tests 100)

  (define-property "make-jacobi-preconditioner inverts diagonal entries"
    gen-diagonal-system-with-x
    (lambda (args)
      (let* ([a (car args)]
             [diag (cadr args)]
             [r (cadddr args)]
             [solve-precond (make-jacobi-preconditioner a)]
             [z (solve-precond r)]
             [expected (list->vector
                        (map (lambda (ri di) (/ ri di))
                             (vector->list r)
                             diag))])
        (vec-approx=? z expected 1e-10)))
    'tests 150)

  (define-property "is-diagonally-dominant? holds for strictly positive diagonal matrices"
    gen-spd-system
    (lambda (sys)
      (is-diagonally-dominant? (car sys)))
    'tests 150)

  (define-property "spectral-radius-estimate stays within diagonal eigenvalue bounds"
    gen-spd-system
    (lambda (sys)
      (let* ([a (car sys)]
             [diag (cadr sys)]
             [rho (spectral-radius-estimate a 120)]
             [d-min (fold-left min (car diag) (cdr diag))]
             [d-max (fold-left max (car diag) (cdr diag))])
        (and (>= rho (- d-min 1e-6))
             (<= rho (+ d-max 1e-6)))))
    'tests 120)

  (define-property "compute-givens zeroes the second component"
    gen-givens-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [giv (compute-givens a b)]
             [c (car giv)]
             [s (cdr giv)]
             [y (+ (* (- s) a) (* c b))])
        (approx= y 0.0 1e-9)))
    'tests 180)

  (define-property "apply-givens-rotations matches explicit 2x2 rotation update"
    gen-givens-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [giv (compute-givens a b)]
             [c (car giv)]
             [s (cdr giv)]
             [h (make-vector 4 0.0)]   ; k=2 => (k+1) x k storage, only entries we use
             [cs (vector c 0.0)]
             [sn (vector s 0.0)])
        ;; Store column j=1: h[0,1]=a and h[1,1]=b
        (vector-set! h 1 a)
        (vector-set! h 3 b)
        (apply-givens-rotations h cs sn 1 2)
        (let ([h01 (vector-ref h 1)]
              [h11 (vector-ref h 3)])
          (and (approx= h01 (+ (* c a) (* s b)) 1e-10)
               (approx= h11 (+ (* (- s) a) (* c b)) 1e-10)))))
    'tests 160)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
