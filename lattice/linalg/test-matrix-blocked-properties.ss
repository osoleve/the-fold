;;; lattice/linalg/test-matrix-blocked-properties.ss — QuickCheck properties for blocked matrix algorithms

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'matrix)
(require 'matrix-blocked)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-entry
  (gen-int-range -8 8))

(define gen-dim
  (gen-int-range 1 4))

(define (gen-matrix rows cols)
  (gen-map matrix-from-lists
           (gen-list-of rows (gen-list-of cols gen-entry))))

(define gen-compatible-pair
  (gen-bind gen-dim
    (lambda (m)
      (gen-bind gen-dim
        (lambda (n)
          (gen-bind gen-dim
            (lambda (p)
              (gen-bind (gen-matrix m n)
                (lambda (a)
                  (gen-map (lambda (b) (cons a b))
                           (gen-matrix n p)))))))))))

(define gen-any-matrix
  (gen-bind (gen-int-range 0 5)
    (lambda (rows)
      (gen-bind (gen-int-range 0 5)
        (lambda (cols)
          (gen-matrix rows cols))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group matrix-blocked-properties

  (define-property "matrix-mul-blocked agrees with matrix-mul"
    gen-compatible-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [blocked (matrix-mul-blocked a b 2)]
             [plain (matrix-mul a b)])
        (matrix-equal? blocked plain)))
    'tests 160)

  (define-property "matrix-transpose-blocked agrees with matrix-transpose"
    gen-any-matrix
    (lambda (m)
      (matrix-equal? (matrix-transpose-blocked m 2)
                     (matrix-transpose m)))
    'tests 200)

  (define-property "matrix-mul-strassen agrees with matrix-mul"
    gen-compatible-pair
    (lambda (pair)
      (let* ([a (car pair)]
             [b (cdr pair)]
             [strassen (matrix-mul-strassen a b 2)]
             [plain (matrix-mul a b)])
        (matrix-equal? strassen plain)))
    'tests 120)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
