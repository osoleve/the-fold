;;; boundary/parallel/test-matrix-parallel.ss — Tests for Parallel Matrix Operations

(load "core/testing/test-framework.ss")
(load "boundary/parallel/matrix-parallel.ss")

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

;;; Helper: deterministic test matrix
(define (make-test-matrix rows cols)
  (let ([data (make-vector (* rows cols) 0)])
    (do ([i 0 (+ i 1)])
        ((= i rows))
      (do ([j 0 (+ j 1)])
          ((= j cols))
        (vector-set! data (+ (* i cols) j) (+ (* i cols) j 1.0))))
    (list 'matrix rows cols data)))

;;; Helper: apply permutation matrix to a matrix
;;; P is a permutation vector, A is the matrix.
;;; Returns P*A where P[i] gives the source row for result row i.
(define (apply-permutation p a)
  (let* ([n (matrix-rows a)]
         [cols (matrix-cols a)]
         [data (matrix-data a)]
         [result (make-vector (* n cols) 0)])
    (do ([i 0 (+ i 1)])
        ((= i n))
      (let ([src (vector-ref p i)])
        (do ([j 0 (+ j 1)])
            ((= j cols))
          (vector-set! result (+ (* i cols) j)
            (vector-ref data (+ (* src cols) j))))))
    (list 'matrix n cols result)))

;;; ============================================================================
;;; Parallel Multiplication Tests
;;; ============================================================================

(test-group parallel-mul

  (define-test parallel-mul-small-sequential-fallback
    (let* ([a (make-test-matrix 4 4)]
           [b (make-test-matrix 4 4)]
           [seq (matrix-mul a b)]
           [par (parallel-matrix-mul a b)])
      (assert-true (matrix-approx-equal? seq par))))

  (define-test parallel-mul-above-threshold
    (let* ([a (make-test-matrix 100 80)]
           [b (make-test-matrix 80 100)]
           [seq (matrix-mul-blocked a b)]
           [par (parallel-matrix-mul a b)])
      (assert-true (matrix-approx-equal? seq par 1e-4))))

  (define-test parallel-mul-square-128
    (let* ([a (make-test-matrix 128 128)]
           [b (make-test-matrix 128 128)]
           [seq (matrix-mul-blocked a b)]
           [par (parallel-matrix-mul a b)])
      (assert-true (matrix-approx-equal? seq par 1e-2))))

  (define-test parallel-mul-dimension-mismatch
    (let ([a (matrix-from-lists '((1 2 3)))]
          [b (matrix-from-lists '((1 2)))])
      (assert-true (error-result? (parallel-matrix-mul a b))))))

;;; ============================================================================
;;; Parallel Transpose Tests
;;; ============================================================================

(test-group parallel-transpose

  (define-test parallel-transpose-small-fallback
    (let* ([m (make-test-matrix 4 4)]
           [seq (matrix-transpose m)]
           [par (parallel-matrix-transpose m)])
      (assert-true (matrix-equal? seq par))))

  (define-test parallel-transpose-large
    (let* ([m (make-test-matrix 200 150)]
           [seq (matrix-transpose-blocked m)]
           [par (parallel-matrix-transpose m)])
      (assert-true (matrix-equal? seq par))))

  (define-test parallel-transpose-double-is-identity
    (let* ([m (make-test-matrix 200 200)]
           [result (parallel-matrix-transpose (parallel-matrix-transpose m))])
      (assert-true (matrix-equal? m result)))))

;;; ============================================================================
;;; Parallel Map Tests
;;; ============================================================================

(test-group parallel-map

  (define-test parallel-map-small-fallback
    (let* ([m (make-test-matrix 3 3)]
           [seq (matrix-map (lambda (x) (* x 2)) m)]
           [par (parallel-matrix-map (lambda (x) (* x 2)) m)])
      (assert-true (matrix-equal? seq par))))

  (define-test parallel-map-large
    (let* ([m (make-test-matrix 100 100)]
           [seq (matrix-map (lambda (x) (+ x 1.0)) m)]
           [par (parallel-matrix-map (lambda (x) (+ x 1.0)) m)])
      (assert-true (matrix-approx-equal? seq par)))))

;;; ============================================================================
;;; Parallel LU Tests
;;; ============================================================================

(test-group parallel-lu-decomp

  (define-test parallel-lu-3x3
    (let* ([a (matrix-from-lists '((2.0 1.0 1.0) (4.0 3.0 3.0) (8.0 7.0 9.0)))]
           [result (parallel-lu a)]
           [l (car result)]
           [u (cadr result)]
           [p (caddr result)]
           [pa (apply-permutation p a)]
           [lu-product (matrix-mul l u)])
      (assert-true (matrix-approx-equal? pa lu-product 1e-8))))

  (define-test parallel-lu-identity
    (let* ([a (matrix-identity 5)]
           ;; Convert to flonum
           [af (matrix-map (lambda (x) (* 1.0 x)) a)]
           [result (parallel-lu af)]
           [l (car result)]
           [u (cadr result)])
      ;; L should be identity, U should be identity
      (assert-true (matrix-approx-equal? l af 1e-10))
      (assert-true (matrix-approx-equal? u af 1e-10))))

  (define-test parallel-lu-large
    (let* ([n 64]
           ;; Create a diagonally dominant matrix (guaranteed non-singular)
           [data (make-vector (* n n) 0)]
           [_ (do ([i 0 (+ i 1)])
                  ((= i n))
                (do ([j 0 (+ j 1)])
                    ((= j n))
                  (vector-set! data (+ (* i n) j)
                    (if (= i j)
                        (+ n 1.0)  ; diagonal dominance
                        (/ 1.0 (+ 1.0 (abs (- i j))))))))]
           [a (list 'matrix n n data)]
           [result (parallel-lu a)]
           [l (car result)]
           [u (cadr result)]
           [p (caddr result)]
           [pa (apply-permutation p a)]
           [lu-product (matrix-mul l u)])
      (assert-true (matrix-approx-equal? pa lu-product 1e-6))))

  (define-test parallel-lu-singular
    (let ([a (matrix-from-lists '((1.0 2.0) (2.0 4.0)))])
      (assert-true (error-result? (parallel-lu a)))))

  (define-test parallel-lu-not-square
    (let ([a (matrix-from-lists '((1.0 2.0 3.0) (4.0 5.0 6.0)))])
      (assert-true (error-result? (parallel-lu a))))))

;;; ============================================================================
;;; Parallel QR Tests
;;; ============================================================================

(test-group parallel-qr-decomp

  (define-test parallel-qr-3x3
    (let* ([a (matrix-from-lists '((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 10.0)))]
           [result (parallel-qr a)]
           [q (car result)]
           [r (cadr result)]
           ;; Q*R should equal A
           [qr-product (matrix-mul q r)])
      (assert-true (matrix-approx-equal? a qr-product 1e-8))))

  (define-test parallel-qr-orthogonal
    (let* ([a (matrix-from-lists '((1.0 2.0 3.0) (4.0 5.0 6.0) (7.0 8.0 10.0)))]
           [result (parallel-qr a)]
           [q (car result)]
           [qt (matrix-transpose q)]
           [qtq (matrix-mul qt q)]
           [I (matrix-map (lambda (x) (* 1.0 x)) (matrix-identity 3))])
      ;; Q^T * Q should be identity
      (assert-true (matrix-approx-equal? qtq I 1e-8))))

  (define-test parallel-qr-tall
    (let* ([a (matrix-from-lists '((1.0 2.0) (3.0 4.0) (5.0 6.0) (7.0 8.0)))]
           [result (parallel-qr a)]
           [q (car result)]
           [r (cadr result)]
           [qr-product (matrix-mul q r)])
      (assert-true (matrix-approx-equal? a qr-product 1e-8))))

  (define-test parallel-qr-large
    (let* ([n 64]
           [data (make-vector (* n n) 0)]
           [_ (do ([i 0 (+ i 1)])
                  ((= i n))
                (do ([j 0 (+ j 1)])
                    ((= j n))
                  (vector-set! data (+ (* i n) j)
                    (+ (* i n) j 1.0))))]
           [a (list 'matrix n n data)]
           [result (parallel-qr a)]
           [q (car result)]
           [r (cadr result)]
           [qr-product (matrix-mul q r)]
           [qt (matrix-transpose q)]
           [qtq (matrix-mul qt q)]
           [I (matrix-map (lambda (x) (* 1.0 x)) (matrix-identity n))])
      (assert-true (matrix-approx-equal? a qr-product 1e-4))
      (assert-true (matrix-approx-equal? qtq I 1e-8))))

  (define-test parallel-qr-underdetermined
    (let ([a (matrix-from-lists '((1.0 2.0 3.0) (4.0 5.0 6.0)))])
      (assert-true (error-result? (parallel-qr a))))))

;;; ============================================================================
;;; Cleanup & Run
;;; ============================================================================

(run-all-tests)

;; Shutdown thread pool
(pool-shutdown-global!)
