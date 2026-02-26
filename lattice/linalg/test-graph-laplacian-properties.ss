;;; lattice/linalg/test-graph-laplacian-properties.ss — QuickCheck properties for graph Laplacians

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'graph-matrix)
(require 'graph-laplacian)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (path-edges n)
  (let loop ([i 0] [acc '()])
    (if (>= i (- n 1))
        (reverse acc)
        (loop (+ i 1) (cons (list i (+ i 1)) acc)))))

(define (matrix-row-sum m i)
  (let ([cols (matrix-cols m)])
    (let loop ([j 0] [sum 0])
      (if (= j cols)
          sum
          (loop (+ j 1) (+ sum (matrix-ref m i j)))))))

(define (all-rows-zero-sum? m)
  (let ([rows (matrix-rows m)])
    (let loop ([i 0])
      (or (= i rows)
          (and (= (matrix-row-sum m i) 0)
               (loop (+ i 1)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group graph-laplacian-properties

  (define-property "laplacian-from-edges matches laplacian(adjacency)"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [adj (edges->adjacency-matrix edges n #t)]
             [l1 (laplacian adj)]
             [l2 (laplacian-from-edges edges n)])
        (matrix-equal? l1 l2)))
    'tests 180)

  (define-property "Laplacian rows sum to zero for undirected path graphs"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [l (laplacian-from-edges edges n)])
        (all-rows-zero-sum? l)))
    'tests 180)

  (define-property "normalized Laplacian has unit diagonal on non-isolated path nodes"
    (gen-int-range 2 12)
    (lambda (n)
      (let* ([edges (path-edges n)]
             [adj (edges->adjacency-matrix edges n #t)]
             [l (laplacian-normalized adj)])
        (let loop ([i 0])
          (or (= i n)
              (and (approx= (matrix-ref l i i) 1.0 1e-10)
                   (loop (+ i 1)))))))
    'tests 150)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
