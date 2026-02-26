;;; lattice/optics/test-bidirectional-properties.ss — QuickCheck properties for bidirectional migrations

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'bidirectional)

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group bidirectional-properties

  (define-property "identity migration is a no-op"
    gen-int
    (lambda (x)
      (let ([m (make-identity-migration 'id 'v1)])
        (and (equal? x (migrate m x))
             (equal? x (rollback m x)))))
    'tests 220)

  (define-property "rollback undoes migrate for reversible function migrations"
    gen-int
    (lambda (x)
      (let* ([m (make-migration-from-functions
                 'int-shift 'v1 'v2
                 (lambda (n) (+ n 1))
                 (lambda (n) (- n 1)))]
             [y (migrate m x)])
        (equal? x (rollback m y))))
    'tests 220)

  (define-property "composed migration equals sequential application"
    gen-int
    (lambda (x)
      (let* ([m1 (make-migration-from-functions
                  'm1 'v1 'v2
                  (lambda (n) (+ n 3))
                  (lambda (n) (- n 3)))]
             [m2 (make-migration-from-functions
                  'm2 'v2 'v3
                  (lambda (n) (* n 2))
                  (lambda (n) (/ n 2)))]
             [mc (migration-compose m1 m2)])
        (and (equal? (migrate mc x)
                     (migrate m2 (migrate m1 x)))
             (equal? (rollback mc (migrate mc x)) x))))
    'tests 180)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
