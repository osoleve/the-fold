;;; lattice/linalg/test-svd-properties.ss — QuickCheck properties for SVD helpers

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'svd)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (error-result? x)
  (and (pair? x) (eq? (car x) 'error)))

(define (diagonal-matrix diag)
  (let ([n (length diag)])
    (let loop-i ([i 0] [rows '()])
      (if (= i n)
          (matrix-from-lists (reverse rows))
          (let loop-j ([j 0] [row '()])
            (if (= j n)
                (loop-i (+ i 1) (cons (reverse row) rows))
                (loop-j (+ j 1)
                        (cons (if (= i j) (list-ref diag i) 0) row))))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-small-n
  (gen-int-range 1 4))

(define gen-diagonal-entries
  (gen-bind gen-small-n
    (lambda (n)
      (gen-list-of n (gen-int-range 1 12)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group svd-properties

  (define-property "identity singular-values are all ones"
    gen-small-n
    (lambda (n)
      (let ([sv (singular-values (matrix-identity n))])
        (and (vector? sv)
             (= (vector-length sv) n)
             (let loop ([i 0])
               (or (= i n)
                   (and (approx= (vector-ref sv i) 1.0 1e-8)
                        (loop (+ i 1))))))))
    'tests 120)

  (define-property "identity matrix rank equals dimension"
    gen-small-n
    (lambda (n)
      (= (matrix-rank (matrix-identity n)) n))
    'tests 120)

  (define-property "verify-svd succeeds on diagonal matrices"
    gen-diagonal-entries
    (lambda (diag)
      (let* ([a (diagonal-matrix diag)]
             [result (svd a)])
        (and (not (error-result? result))
             (let* ([u (car result)]
                    [sigma (cadr result)]
                    [v (caddr result)])
               (verify-svd a u sigma v 1e-8)))))
    'tests 80)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
