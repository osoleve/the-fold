;;; lattice/linalg/test-sparse-properties.ss — QuickCheck properties for sparse matrices

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'sparse)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (sparse-error? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (vector-approx=? v1 v2 tol)
  (and (= (vector-length v1) (vector-length v2))
       (let loop ([i 0] [n (vector-length v1)])
         (or (= i n)
             (and (< (abs (- (vector-ref v1 i)
                             (vector-ref v2 i)))
                     tol)
                  (loop (+ i 1) n))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-entry
  (gen-frequency
   (list (cons 6 (gen-pure 0))
         (cons 4 (gen-int-range -5 5)))))

(define gen-small-dim
  (gen-int-range 0 4))

(define gen-pos-dim
  (gen-int-range 1 4))

(define (gen-dense rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-entry))))

(define gen-any-dense
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-dense rows cols))))))

(define gen-square-dense
  (gen-bind gen-small-dim
    (lambda (n)
      (gen-dense n n))))

(define gen-same-shape-pair
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-bind (gen-dense rows cols)
            (lambda (a)
              (gen-map (lambda (b) (cons a b))
                       (gen-dense rows cols)))))))))

(define gen-matvec-args
  (gen-bind gen-small-dim
    (lambda (rows)
      (gen-bind gen-small-dim
        (lambda (cols)
          (gen-bind (gen-dense rows cols)
            (lambda (a)
              (let ([n (matrix-cols a)])
                (gen-map (lambda (xs) (list a (list->vector xs)))
                         (gen-list-of n gen-entry))))))))))

(define gen-matmul-args
  (gen-bind gen-pos-dim
    (lambda (m)
      (gen-bind gen-pos-dim
        (lambda (n)
          (gen-bind gen-pos-dim
            (lambda (p)
              (gen-bind (gen-dense m n)
                (lambda (a)
                  (gen-map (lambda (b) (list a b))
                           (gen-dense n p)))))))))))

;;; ============================================================================
;;; Conversion and structure properties
;;; ============================================================================

(test-group sparse-conversion-properties

  (define-property "dense->COO->dense roundtrip"
    gen-any-dense
    (lambda (a)
      (let* ([coo (dense->sparse-coo a)]
             [back (sparse-coo->dense coo)])
        (matrix-equal? back a)))
    'tests 220)

  (define-property "dense->CSR->dense roundtrip"
    gen-any-dense
    (lambda (a)
      (let* ([csr (dense->sparse-csr a)]
             [back (sparse-csr->dense csr)])
        (matrix-equal? back a)))
    'tests 220)

  (define-property "dense->CSC->dense roundtrip"
    gen-any-dense
    (lambda (a)
      (let* ([coo (dense->sparse-coo a)]
             [csc (coo->csc coo)]
             [back (sparse-csc->dense csc)])
        (matrix-equal? back a)))
    'tests 220)

  (define-property "COO->CSR->COO preserves values"
    gen-any-dense
    (lambda (a)
      (let* ([coo (dense->sparse-coo a)]
             [csr (coo->csr coo)]
             [coo2 (csr->coo csr)])
        (sparse-approx-equal? coo coo2 1e-12)))
    'tests 200)

  (define-property "COO transpose is involutive"
    gen-any-dense
    (lambda (a)
      (let* ([coo (dense->sparse-coo a)]
             [tt (sparse-coo-transpose (sparse-coo-transpose coo))])
        (sparse-approx-equal? coo tt 1e-12)))
    'tests 200)
)

;;; ============================================================================
;;; Algebraic properties
;;; ============================================================================

(test-group sparse-algebra-properties

  (define-property "sparse COO addition matches dense addition"
    gen-same-shape-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [coo-a (dense->sparse-coo a)]
             [coo-b (dense->sparse-coo b)]
             [sum (sparse-coo-add coo-a coo-b)]
             [dense-sum (matrix-add a b)])
        (and (not (sparse-error? sum))
             (matrix-equal? (sparse-coo->dense sum) dense-sum))))
    'tests 200)

  (define-property "sparse COO addition is commutative"
    gen-same-shape-pair
    (lambda (pair)
      (let* ([coo-a (dense->sparse-coo (car pair))]
             [coo-b (dense->sparse-coo (cdr pair))]
             [ab (sparse-coo-add coo-a coo-b)]
             [ba (sparse-coo-add coo-b coo-a)])
        (and (not (sparse-error? ab))
             (not (sparse-error? ba))
             (sparse-approx-equal? ab ba 1e-12))))
    'tests 200)

  (define-property "A + (-A) has zero nnz in COO"
    gen-any-dense
    (lambda (a)
      (let* ([coo (dense->sparse-coo a)]
             [neg (sparse-coo-scale -1 coo)]
             [sum (sparse-coo-add coo neg)])
        (and (not (sparse-error? sum))
             (= (sparse-coo-nnz sum) 0))))
    'tests 200)
)

;;; ============================================================================
;;; Multiplication properties
;;; ============================================================================

(test-group sparse-multiplication-properties

  (define-property "sparse matvec matches dense matvec (COO/CSR/CSC)"
    gen-matvec-args
    (lambda (args)
      (let* ([a (car args)]
             [x (cadr args)]
             [dense-y (matrix-vec-mul a x)]
             [coo (dense->sparse-coo a)]
             [csr (coo->csr coo)]
             [csc (coo->csc coo)]
             [y-coo (sparse-coo-vec-mul coo x)]
             [y-csr (sparse-csr-vec-mul csr x)]
             [y-csc (sparse-csc-vec-mul csc x)])
        (and (vector? y-coo)
             (vector? y-csr)
             (vector? y-csc)
             (vector-approx=? dense-y y-coo 1e-12)
             (vector-approx=? dense-y y-csr 1e-12)
             (vector-approx=? dense-y y-csc 1e-12))))
    'tests 200)

  (define-property "sparse CSR matmul matches dense matmul"
    gen-matmul-args
    (lambda (args)
      (let* ([a (car args)]
             [b (cadr args)]
             [csr-a (dense->sparse-csr a)]
             [csr-b (dense->sparse-csr b)]
             [csr-c (sparse-csr-mul csr-a csr-b)]
             [dense-c (matrix-mul a b)])
        (and (not (sparse-error? csr-c))
             (matrix-equal? (sparse-csr->dense csr-c) dense-c))))
    'tests 160)

  (define-property "sparse identity is multiplicative neutral element (CSR)"
    gen-square-dense
    (lambda (a)
      (let* ([csr-a (dense->sparse-csr a)]
             [n (matrix-rows a)]
             [i (sparse-identity n)]
             [left (sparse-csr-mul i csr-a)]
             [right (sparse-csr-mul csr-a i)])
        (and (not (sparse-error? left))
             (not (sparse-error? right))
             (sparse-approx-equal? left csr-a 1e-12)
             (sparse-approx-equal? right csr-a 1e-12))))
    'tests 160)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
