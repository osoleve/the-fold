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
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
