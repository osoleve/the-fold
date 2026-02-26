;;; lattice/linalg/test-matrix-eigen-properties.ss — QuickCheck properties for eigen computations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix-eigen)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (eigenpair-result? x)
  (and (pair? x)
       (not (eq? (car x) 'error))
       (number? (car x))
       (vector? (cdr x))))

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (list-max xs)
  (let loop ([rest (cdr xs)] [best (car xs)])
    (if (null? rest)
        best
        (loop (cdr rest) (max best (car rest))))))

(define (list-min xs)
  (let loop ([rest (cdr xs)] [best (car xs)])
    (if (null? rest)
        best
        (loop (cdr rest) (min best (car rest))))))

(define (diagonal-matrix entries)
  (let* ([n (length entries)]
         [data (make-vector (* n n) 0)])
    (let loop ([i 0] [xs entries])
      (if (= i n)
          (matrix-from-vec n n data)
          (begin
            (vector-set! data (+ (* i n) i) (car xs))
            (loop (+ i 1) (cdr xs)))))))

(define (descending-positive-entries n base)
  (let loop ([i 0] [acc '()])
    (if (= i n)
        (reverse acc)
        (loop (+ i 1)
              (cons (* (+ base 1)
                       (expt 2 (- n i)))
                    acc)))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-size
  (gen-int-range 1 4))

(define gen-entry
  (gen-int-range -5 5))

(define gen-positive-entry
  (gen-int-range 1 9))

(define (gen-square-lists n)
  (gen-list-of n (gen-list-of n gen-entry)))

(define gen-dominant-diagonal-case
  (gen-bind gen-size
    (lambda (n)
      (gen-map
       (lambda (base)
         (let* ([entries (descending-positive-entries n base)]
                [a (diagonal-matrix entries)])
           (list a (car entries))))
       (gen-int-range 1 10)))))

(define gen-positive-diagonal-case
  (gen-bind gen-size
    (lambda (n)
      (gen-map
       (lambda (entries)
         (list (diagonal-matrix entries) entries))
       (gen-list-of n gen-positive-entry)))))

(define gen-symmetric-matrix
  (gen-bind gen-size
    (lambda (n)
      (gen-map
       (lambda (rows)
         (let* ([b (matrix-from-lists rows)]
                [bt (matrix-transpose b)])
           (matrix-add b bt)))
       (gen-square-lists n)))))

(define gen-spd-matrix
  (gen-bind gen-size
    (lambda (n)
      (gen-map
       (lambda (rows)
         (let* ([b (matrix-from-lists rows)]
                [bt (matrix-transpose b)]
                [btb (matrix-mul bt b)]
                [i (matrix-identity n)])
           (matrix-add btb i)))
       (gen-square-lists n)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group matrix-eigen-power-properties

  (define-property "power-iteration finds dominant eigenpair on positive diagonal matrices"
    gen-dominant-diagonal-case
    (lambda (args)
      (let* ([a (car args)]
             [expected (cadr args)]
             [n (matrix-rows a)]
             [v0 (make-vector n 1.0)]
             [result (power-iteration a v0 160 1e-10)])
        (and (eigenpair-result? result)
             (approx= (car result) expected 1e-6)
             (verify-eigenvalue a (car result) (cdr result) 1e-4))))
    'tests 140)

  (define-property "spectral-radius matches max diagonal entry on positive diagonal matrices"
    gen-positive-diagonal-case
    (lambda (args)
      (let* ([a (car args)]
             [entries (cadr args)]
             [rho (spectral-radius a 120 1e-10)])
        (and (number? rho)
             (approx= rho (list-max entries) 1e-7))))
    'tests 140)

  (define-property "eigenvalue-condition matches max/min ratio on positive diagonal matrices"
    gen-positive-diagonal-case
    (lambda (args)
      (let* ([a (car args)]
             [entries (cadr args)]
             [expected (/ (list-max entries) (list-min entries))]
             [kappa (eigenvalue-condition a 160 1e-10)])
        (and (number? kappa)
             (approx= kappa expected 1e-7))))
    'tests 140)
)

(test-group matrix-eigen-qr-properties

  (define-property "qr-algorithm-shifted preserves trace on symmetric matrices"
    gen-symmetric-matrix
    (lambda (a)
      (let ([eigenvalues (qr-algorithm-shifted a 260 1e-10)])
        (and (vector? eigenvalues)
             (approx= (vec-sum eigenvalues) (matrix-trace a) 1e-4))))
    'tests 120)

  (define-property "symmetric-eigen returns orthonormal eigenvectors and valid eigenpairs on SPD matrices"
    gen-spd-matrix
    (lambda (a)
      (let ([result (symmetric-eigen a 260 1e-10)])
        (and (pair? result)
             (not (error-result? result))
             (let* ([eigenvalues (car result)]
                    [eigenvectors (cdr result)]
                    [n (matrix-rows a)]
                    [vtv (matrix-mul (matrix-transpose eigenvectors) eigenvectors)]
                    [id (matrix-identity n)])
               (and (vector? eigenvalues)
                    (matrix-approx-equal? vtv id 1e-5)
                    (let loop ([i 0])
                      (if (= i n)
                          #t
                          (and (verify-eigenvalue a
                                                  (vector-ref eigenvalues i)
                                                  (matrix-col eigenvectors i)
                                                  1e-5)
                               (loop (+ i 1))))))))))
    'tests 90)

  (define-property "eigen-decomposition verifies on positive diagonal matrices"
    gen-positive-diagonal-case
    (lambda (args)
      (let* ([a (car args)]
             [result (eigen-decomposition a 200 1e-10)])
        (and (pair? result)
             (not (error-result? result))
             (verify-decomposition a (car result) (cdr result) 1e-6))))
    'tests 110)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
