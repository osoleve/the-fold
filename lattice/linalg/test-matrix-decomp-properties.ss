;;; lattice/linalg/test-matrix-decomp-properties.ss — QuickCheck properties for matrix decompositions

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'matrix-decomp)

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

(define (matrix-upper-triangular? u tol)
  (let ([rows (matrix-rows u)]
        [cols (matrix-cols u)])
    (let row-loop ([i 0])
      (if (= i rows)
          #t
          (let col-loop ([j 0])
            (cond
              [(= j cols) (row-loop (+ i 1))]
              [(< j i)
               (and (<= (abs (matrix-ref u i j)) tol)
                    (col-loop (+ j 1)))]
              [else (col-loop (+ j 1))]))))))

(define (matrix-lower-unit-triangular? l tol)
  (let ([rows (matrix-rows l)]
        [cols (matrix-cols l)])
    (let row-loop ([i 0])
      (if (= i rows)
          #t
          (let col-loop ([j 0])
            (cond
              [(= j cols) (row-loop (+ i 1))]
              [(= j i)
               (and (approx= (matrix-ref l i j) 1.0 tol)
                    (col-loop (+ j 1)))]
              [(> j i)
               (and (<= (abs (matrix-ref l i j)) tol)
                    (col-loop (+ j 1)))]
              [else (col-loop (+ j 1))]))))))

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

(define gen-lu-solve-args
  (gen-bind gen-size
    (lambda (n)
      (gen-bind (gen-square-lists n)
        (lambda (rows)
          (let ([a (make-invertible-matrix rows)])
            (gen-map (lambda (xs) (list a (list->vector xs)))
                     (gen-list-of n gen-entry))))))))

(define gen-qr-matrix
  (gen-bind (gen-int-range 1 5)
    (lambda (m)
      (gen-bind (gen-int-range 1 m)
        (lambda (n)
          (gen-map matrix-from-lists
                   (gen-list-of m (gen-list-of n gen-entry))))))))

(define gen-spd-matrix
  (gen-bind gen-size
    (lambda (n)
      (gen-map (lambda (rows)
                 (let* ([b (matrix-from-lists rows)]
                        [bt (matrix-transpose b)]
                        [btb (matrix-mul bt b)]
                        [i (matrix-identity n)])
                   (matrix-add btb i)))
               (gen-square-lists n)))))

;;; ============================================================================
;;; LU properties
;;; ============================================================================

(test-group matrix-lu-properties

  (define-property "LU decomposition succeeds on strictly diagonally dominant matrices"
    gen-invertible-matrix
    (lambda (a)
      (let ([res (matrix-lu a)])
        (not (matrix-error? res))))
    'tests 180)

  (define-property "LU yields unit-lower L and upper U"
    gen-invertible-matrix
    (lambda (a)
      (let ([res (matrix-lu a)])
        (and (not (matrix-error? res))
             (let ([l (car res)]
                   [u (cadr res)])
               (and (matrix-lower-unit-triangular? l 1e-9)
                    (matrix-upper-triangular? u 1e-9))))))
    'tests 180)

  (define-property "LU solve recovers a known solution"
    gen-lu-solve-args
    (lambda (args)
      (let* ([a (car args)]
             [x-true (cadr args)]
             [b (matrix-vec-mul a x-true)]
             [lu (matrix-lu a)]
             [x (if (matrix-error? lu) lu (matrix-lu-solve lu b))])
        (and (not (matrix-error? x))
             (vector-approx=? x x-true 1e-7))))
    'tests 180)

  (define-property "matrix-det is non-zero on strictly diagonally dominant matrices"
    gen-invertible-matrix
    (lambda (a)
      (> (abs (matrix-det a)) 1e-9))
    'tests 160)

  (define-property "matrix-copy and permutation-sign helper surface is consistent"
    (gen-pure #t)
    (lambda (_)
      (let* ([a (matrix-from-lists '((1 2) (3 4)))]
             [copy (matrix-copy a)]
             [even-p (vector 0 1 2 3)]
             [odd-p (vector 1 0 2 3)])
        (and (matrix-equal? copy a)
             (= (permutation-sign even-p) 1)
             (= (permutation-sign odd-p) -1))))
    'tests 20)
)

;;; ============================================================================
;;; QR properties
;;; ============================================================================

(test-group matrix-qr-properties

  (define-property "QR produces orthonormal columns (Q^TQ ≈ I)"
    gen-qr-matrix
    (lambda (a)
      (let ([res (matrix-qr a)])
        (and (not (matrix-error? res))
             (let* ([q (car res)]
                    [qtq (matrix-mul (matrix-transpose q) q)]
                    [i (matrix-identity (matrix-cols a))])
               (and (not (matrix-error? qtq))
                    (matrix-approx-equal? qtq i 1e-6))))))
    'tests 180)

  (define-property "QR reconstructs A (Q*R ≈ A) and R is upper triangular"
    gen-qr-matrix
    (lambda (a)
      (let ([res (matrix-qr a)])
        (and (not (matrix-error? res))
             (let* ([q (car res)]
                    [r (cadr res)]
                    [qr (matrix-mul q r)])
               (and (not (matrix-error? qr))
                    (matrix-upper-triangular? r 1e-6)
                    (matrix-approx-equal? qr a 1e-6))))))
    'tests 180)

  (define-property "QR rejects underdetermined shapes (m < n)"
    (gen-bind (gen-int-range 1 4)
      (lambda (m)
        (gen-bind (gen-int-range (+ m 1) (+ m 4))
          (lambda (n)
            (gen-map matrix-from-lists
                     (gen-list-of m (gen-list-of n gen-entry)))))))
    (lambda (a)
      (let ([res (matrix-qr a)])
        (and (matrix-error? res)
             (eq? (cadr res) 'underdetermined))))
    'tests 120)
)

;;; ============================================================================
;;; Cholesky properties
;;; ============================================================================

(test-group matrix-cholesky-properties

  (define-property "Cholesky succeeds on SPD matrices and reconstructs A"
    gen-spd-matrix
    (lambda (a)
      (let ([res (matrix-cholesky a)])
        (and (not (matrix-error? res))
             (let* ([l (car res)]
                    [llt (matrix-mul l (matrix-transpose l))])
               (and (not (matrix-error? llt))
                    (matrix-upper-triangular? (matrix-transpose l) 1e-8)
                    (matrix-approx-equal? llt a 1e-6))))))
    'tests 180)

  (define-property "Cholesky rejects a known non-PD matrix"
    gen-int
    (lambda (_)
      (let ([res (matrix-cholesky (matrix-from-lists '((1 2) (2 1))))])
        (and (matrix-error? res)
             (eq? (cadr res) 'not-positive-definite))))
    'tests 40)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
