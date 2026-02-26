;;; lattice/fp/test-protocol-bundle-properties.ss — QuickCheck properties for protocol bundles

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'protocol-bundle)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (with-protocol-bundle-state thunk)
  (let ([saved-proto *protocol-registry*]
        [saved-bundle *bundle-registry*])
    (dynamic-wind
      (lambda () #f)
      thunk
      (lambda ()
        (set! *protocol-registry* saved-proto)
        (set! *bundle-registry* saved-bundle)))))

(define (make-qc-bundle)
  (make-protocol-bundle 'qc-bundle
    (list (make-bundle-slot 'qc-get-a 'qc-set-a "a")
          (make-bundle-slot 'qc-get-b 'qc-set-b "b"))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group protocol-bundle-properties

  (define-property "bundle-protocols and slot count are coherent"
    gen-int
    (lambda (_x)
      (with-protocol-bundle-state
       (lambda ()
         (let ([b (make-qc-bundle)])
           (and (= (bundle-slot-count b) 2)
                (= (length (bundle-protocols b)) 4))))))
    'tests 180)

  (define-property "implements-bundle? true when all protocols are registered"
    gen-int
    (lambda (_x)
      (with-protocol-bundle-state
       (lambda ()
         (let ([b (make-qc-bundle)])
           (implement-protocol! 'qc-get-a 'qc-type (lambda (obj) (cadr obj)))
           (implement-protocol! 'qc-set-a 'qc-type (lambda (obj v) (list 'qc-type v (caddr obj))))
           (implement-protocol! 'qc-get-b 'qc-type (lambda (obj) (caddr obj)))
           (implement-protocol! 'qc-set-b 'qc-type (lambda (obj v) (list 'qc-type (cadr obj) v)))
           (implements-bundle? 'qc-type b)))))
    'tests 180)

  (define-property "missing-protocols reports unimplemented entries"
    gen-int
    (lambda (_x)
      (with-protocol-bundle-state
       (lambda ()
         (let ([b (make-qc-bundle)])
           (implement-protocol! 'qc-get-a 'qc-type (lambda (obj) (cadr obj)))
           (let ([missing (missing-protocols 'qc-type b)])
             (and (pair? (memq 'qc-set-a missing))
                  (pair? (memq 'qc-get-b missing))
                  (pair? (memq 'qc-set-b missing))))))))
    'tests 160)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
