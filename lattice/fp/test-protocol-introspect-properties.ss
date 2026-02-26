;;; lattice/fp/test-protocol-introspect-properties.ss — QuickCheck properties for protocol introspection

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'protocol-introspect)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define (with-protocol-introspect-state thunk)
  (let ([saved-proto *protocol-registry*]
        [saved-docs *protocol-docs*])
    (dynamic-wind
      (lambda () #f)
      thunk
      (lambda ()
        (set! *protocol-registry* saved-proto)
        (set! *protocol-docs* saved-docs)))))

(define (qc-sym base n)
  (string->symbol (string-append base (number->string n))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group protocol-introspect-properties

  (define-property "registered doc can be retrieved"
    (gen-int-range 0 200)
    (lambda (n)
      (with-protocol-introspect-state
       (lambda ()
         (let ([p (qc-sym "qc-doc-proto-" n)])
           (register-protocol-doc! p "doc-string" '(-> X Y) 'qc-mod)
           (let ([doc (get-protocol-doc p)])
             (and (protocol-doc? doc)
                  (equal? "doc-string" (protocol-docstring p))))))))
    'tests 180)

  (define-property "protocols-for-type mirrors registered implementations"
    (gen-int-range 0 200)
    (lambda (n)
      (with-protocol-introspect-state
       (lambda ()
         (let ([p1 (qc-sym "qc-p1-" n)]
               [p2 (qc-sym "qc-p2-" n)]
               [t (qc-sym "qc-t-" n)])
           (implement-protocol! p1 t (lambda (obj) obj))
           (implement-protocol! p2 t (lambda (obj) obj))
           (let ([ps (protocols-for-type t)])
             (and (pair? (memq p1 ps))
                  (pair? (memq p2 ps))))))))
    'tests 160)

  (define-property "protocol-stats totals are non-negative and coherent"
    (gen-int-range 0 200)
    (lambda (n)
      (with-protocol-introspect-state
       (lambda ()
         (let ([p (qc-sym "qc-stat-p-" n)]
               [t (qc-sym "qc-stat-t-" n)])
           (implement-protocol! p t (lambda (obj) obj))
           (let* ([stats (protocol-stats)]
                  [protocols (cdr (assq 'protocols stats))]
                  [types (cdr (assq 'types stats))]
                  [impls (cdr (assq 'total-implementations stats))])
             (and (>= protocols 1)
                  (>= types 1)
                  (>= impls 1)))))))
    'tests 140)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
