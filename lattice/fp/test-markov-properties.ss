;;; lattice/fp/test-markov-properties.ss — QuickCheck properties for Markov chains

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'markov)

;;; ============================================================================
;;; Helpers and generators
;;; ============================================================================

(define (approx= a b tol)
  (< (abs (- a b)) tol))

(define (vec-sum v)
  (let ([n (vector-length v)])
    (let loop ([i 0] [s 0.0])
      (if (= i n) s
          (loop (+ i 1) (+ s (vector-ref v i)))))))

(define gen-two-state-system
  (gen-bind (gen-int-range 0 100)
    (lambda (p-int)
      (gen-bind (gen-int-range 0 100)
        (lambda (q-int)
          (gen-map
           (lambda (r-int)
             (let* ([p (/ p-int 100.0)]
                    [q (/ q-int 100.0)]
                    [r (/ r-int 100.0)]
                    [P (matrix-from-lists
                        (list (list p (- 1.0 p))
                              (list q (- 1.0 q))))]
                    [pi (vector r (- 1.0 r))])
               (list P pi)))
           (gen-int-range 0 100)))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group markov-properties

  (define-property "markov-step preserves probability normalization"
    gen-two-state-system
    (lambda (sys)
      (let* ([P (car sys)]
             [pi (cadr sys)]
             [pi-next (markov-step pi P)])
        (and (approx= (vec-sum pi-next) 1.0 1e-9)
             (>= (vector-ref pi-next 0) -1e-10)
             (>= (vector-ref pi-next 1) -1e-10))))
    'tests 220)

  (define-property "markov-steps with n=0 is identity"
    gen-two-state-system
    (lambda (sys)
      (let* ([P (car sys)]
             [pi (cadr sys)]
             [result (markov-steps pi P 0)])
        (and (approx= (vector-ref result 0) (vector-ref pi 0) 1e-12)
             (approx= (vector-ref result 1) (vector-ref pi 1) 1e-12))))
    'tests 220)

  (define-property "stationary-distribution is an approximate fixed point"
    (gen-bind (gen-int-range 1 99)
      (lambda (p-int)
        (gen-map
         (lambda (q-int)
           (let* ([p (/ p-int 100.0)]
                  [q (/ q-int 100.0)])
             (matrix-from-lists
              (list (list p (- 1.0 p))
                    (list q (- 1.0 q))))))
         (gen-int-range 1 99))))
    (lambda (P)
      (let ([pi (stationary-distribution P 1e-10 5000)])
        (if (and (pair? pi) (eq? (car pi) 'error))
            #f
            (let ([next (vec-matrix-mul pi P)])
              (and (approx= (vector-ref pi 0) (vector-ref next 0) 1e-6)
                   (approx= (vector-ref pi 1) (vector-ref next 1) 1e-6)
                   (approx= (vec-sum pi) 1.0 1e-8))))))
    'tests 140)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
