;;; lattice/fp/test-protocol-properties.ss — QuickCheck properties for protocol dispatch

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'protocol)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (with-protocol-registry thunk)
  (let ([saved *protocol-registry*])
    (dynamic-wind
      (lambda () #f)
      thunk
      (lambda () (set! *protocol-registry* saved)))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group protocol-properties

  (define-property "protocol-dispatch routes by type tag"
    gen-int
    (lambda (x)
      (with-protocol-registry
       (lambda ()
         (implement-protocol! 'qc-proto 'qa (lambda (obj) (+ (cadr obj) 1)))
         (implement-protocol! 'qc-proto 'qb (lambda (obj) (* (cadr obj) 2)))
         (and (= (protocol-dispatch 'qc-proto (list 'qa x)) (+ x 1))
              (= (protocol-dispatch 'qc-proto (list 'qb x)) (* x 2))))))
    'tests 220)

  (define-property "protocol-dispatch/default falls back when missing"
    gen-int
    (lambda (x)
      (with-protocol-registry
       (lambda ()
         (implement-protocol! 'qc-proto2 'qa (lambda (obj) (+ (cadr obj) 10)))
         (and (= (protocol-dispatch/default 'qc-proto2 (list 'qa x) (lambda (_obj) -1)) (+ x 10))
              (= (protocol-dispatch/default 'qc-proto2 (list 'qx x) (lambda (_obj) -1)) -1)))))
    'tests 220)

  (define-property "get-type-tag recognizes tagged lists and rejects scalars"
    gen-int
    (lambda (x)
      (and (equal? 'qtag (get-type-tag (list 'qtag x)))
           (equal? #f (get-type-tag x))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
