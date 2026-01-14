;;; core/autodiff/test-profiling.ss --- Tests for Autodiff Profiling

(load "core/test-framework.ss")
(load "lattice/autodiff/profiling.ss")

(display "
====
         AUTODIFF PROFILING TESTS
====
")

;;; ====
;;; Tape Statistics Tests
;;; ====

(test-group tape-stats-tests
            (define-test tape-stats-empty
              ;; Empty tape
              (let* ([tape (make-reverse-tape)]
                     [stats (tape-stats tape)])
                    (assert-equal (cdr (assq 'size stats)) 0)
                    (assert-equal (cdr (assq 'ops stats)) '())))
            
            (define-test tape-stats-single-op
              ;; Single addition
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 2 tape)]
                     [y (make-traced-var 3 tape)]
                     [_ (traced-add x y)]
                     [stats (tape-stats tape)])
                    (assert-equal (cdr (assq 'size stats)) 1)
                    (assert-equal (assq 'add (cdr (assq 'ops stats))) '(add . 1))))
            
            (define-test tape-stats-multiple-ops
              ;; f(x) = x^2 + x -> sq, add
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 3 tape)]
                     [_ (traced-add (traced-sq x) x)]
                     [stats (tape-stats tape)])
                    (assert-equal (cdr (assq 'size stats)) 2)
                    (assert-true (pair? (assq 'sq (cdr (assq 'ops stats)))))
                    (assert-true (pair? (assq 'add (cdr (assq 'ops stats)))))))
            
            (define-test tape-size-basic
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 1 tape)]
                     [y (traced-mul x x)]
                     [z (traced-add y x)])
                    (assert-equal (tape-size tape) 2)))
            
            (define-test tape-op-count-basic
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 1 tape)]
                     [y (traced-mul x x)]
                     [z (traced-mul y x)])
                    (assert-equal (tape-op-count tape 'mul) 2)
                    (assert-equal (tape-op-count tape 'add) 0))))

;;; ====
;;; Graph Statistics Tests
;;; ====

(test-group graph-stats-tests
            (define-test graph-stats-empty
              (let* ([g (make-comp-graph)]
                     [stats (graph-stats g)])
                    (assert-equal (cdr (assq 'nodes stats)) 0)
                    (assert-equal (cdr (assq 'edges stats)) 0)
                    (assert-equal (cdr (assq 'depth stats)) 0)))
            
            (define-test graph-stats-single-var
              (let-values ([(g id) (graph-add-node (make-comp-graph)
                                                   (node-var 'x))])
                          (let ([stats (graph-stats g)])
                               (assert-equal (cdr (assq 'nodes stats)) 1)
                               (assert-equal (cdr (assq 'edges stats)) 0)
                               (assert-equal (cdr (assq 'variables stats)) '(x)))))
            
            (define-test graph-edge-count-basic
              (let* ([g0 (make-comp-graph)]
                     [g1 (let-values ([(g id) (graph-add-node g0 (node-var 'x))]) g)]
                     [g2 (let-values ([(g id) (graph-add-node g1 (node-var 'y))]) g)]
                     ;; Add an op node that references two inputs
                     [g3 (let-values ([(g id)
                                       (graph-add-node
                                        g2
                                        (node-op 'add (list (node-var 'x)
                                                            (node-var 'y))))])
                                     g)])
                    (assert-equal (graph-edge-count g3) 2)))
            
            (define-test graph-depth-flat
              ;; Just variables, depth = 0
              (let* ([g0 (make-comp-graph)]
                     [g1 (let-values ([(g id) (graph-add-node g0 (node-var 'x))]) g)]
                     [g2 (let-values ([(g id) (graph-add-node g1 (node-var 'y))]) g)])
                    (assert-equal (graph-depth g2) 0))))

;;; ====
;;; Timing Tests
;;; ====

(test-group timing-tests
            (define-test time-gradient-simple
              ;; f(x) = x^2
              (let* ([f (lambda (x) (traced-sq x))]
                     [result (time-gradient f '(3))])
                    ;; Check structure
                    (assert-true (pair? (assq 'value result)))
                    (assert-true (pair? (assq 'gradient result)))
                    (assert-true (pair? (assq 'forward-us result)))
                    (assert-true (pair? (assq 'backward-us result)))
                    (assert-true (pair? (assq 'total-us result)))
                    (assert-true (pair? (assq 'tape-size result)))
                    ;; Check values
                    (assert-equal (cdr (assq 'value result)) 9)
                    (assert-equal (cdr (assq 'gradient result)) '(6))))
            
            (define-test time-gradient-multivar
              ;; f(x,y) = x*y
              (let* ([f (lambda (x y) (traced-mul x y))]
                     [result (time-gradient f '(3 4))])
                    (assert-equal (cdr (assq 'value result)) 12)
                    (assert-equal (cdr (assq 'gradient result)) '(4 3))))
            
            (define-test time-thunk-basic
              (let-values ([(result us) (time-thunk (lambda () (+ 1 2)))])
                          (assert-equal result 3)
                          (assert-true (>= us 0))))
            
            (define-test timing-values-positive
              ;; Timings should be non-negative
              (let* ([f (lambda (x) (traced-exp (traced-sq x)))]
                     [result (time-gradient f '(1))])
                    (assert-true (>= (cdr (assq 'forward-us result)) 0))
                    (assert-true (>= (cdr (assq 'backward-us result)) 0))
                    (assert-true (>= (cdr (assq 'total-us result)) 0)))))

;;; ====
;;; Memory Estimation Tests
;;; ====

(test-group memory-tests
            (define-test tape-memory-empty
              (let ([tape (make-reverse-tape)])
                   (assert-equal (tape-memory-estimate tape) 0)))
            
            (define-test tape-memory-nonzero
              ;; Non-empty tape should have positive memory estimate
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [x (make-traced-var 1 tape)]
                     [_ (traced-sq x)]
                     [mem (tape-memory-estimate tape)])
                    (assert-true (> mem 0))))
            
            (define-test graph-memory-empty
              (let ([g (make-comp-graph)])
                   (assert-true (>= (graph-memory-estimate g) 0))))
            
            (define-test graph-memory-with-nodes
              (let-values ([(g id) (graph-add-node (make-comp-graph)
                                                   (node-var 'x))])
                          (assert-true (> (graph-memory-estimate g) 0))))
            
            (define-test traced-memory-estimate-plain
              ;; Plain number
              (assert-equal (traced-memory-estimate 42) *flonum-size*))
            
            (define-test traced-memory-estimate-traced
              ;; Traced value should be larger
              (reset-traced-ids!)
              (let* ([tape (make-reverse-tape)]
                     [t (make-traced-var 42 tape)])
                    (assert-true (> (traced-memory-estimate t)
                                    (traced-memory-estimate 42))))))

;;; ====
;;; Profile Report Tests
;;; ====

(test-group profile-tests
            (define-test profile-computation-structure
              ;; Check that profile returns expected structure
              (let* ([f (lambda (x) (traced-sq x))]
                     [report (profile-computation f '(3))])
                    (assert-true (pair? (assq 'value report)))
                    (assert-true (pair? (assq 'gradient report)))
                    (assert-true (pair? (assq 'timing report)))
                    (assert-true (pair? (assq 'tape report)))
                    (assert-true (pair? (assq 'memory-bytes report)))
                    (assert-true (pair? (assq 'num-inputs report)))))
            
            (define-test profile-computation-values
              (let* ([f (lambda (x y) (traced-add (traced-sq x) (traced-sq y)))]
                     [report (profile-computation f '(3 4))])
                    ;; f(3,4) = 9 + 16 = 25
                    (assert-equal (cdr (assq 'value report)) 25)
                    ;; df/dx = 2x = 6, df/dy = 2y = 8
                    (assert-equal (cdr (assq 'gradient report)) '(6 8))
                    (assert-equal (cdr (assq 'num-inputs report)) 2)))
            
            (define-test profile-constant-function
              ;; Constant function should have zero gradients
              (let* ([f (lambda (x) 42)]
                     [report (profile-computation f '(5))])
                    (assert-equal (cdr (assq 'value report)) 42)
                    (assert-equal (cdr (assq 'gradient report)) '(0)))))

;;; ====
;;; Benchmark Tests
;;; ====

(test-group benchmark-tests
            (define-test benchmark-gradient-structure
              (let* ([f (lambda (x) (traced-sq x))]
                     [result (benchmark-gradient f '(3) 5)])
                    (assert-true (pair? (assq 'iterations result)))
                    (assert-true (pair? (assq 'total-us result)))
                    (assert-true (pair? (assq 'avg-us result)))
                    (assert-true (pair? (assq 'min-us result)))
                    (assert-true (pair? (assq 'max-us result)))
                    (assert-true (pair? (assq 'median-us result)))))
            
            (define-test benchmark-iterations-match
              (let* ([f (lambda (x) (traced-sq x))]
                     [result (benchmark-gradient f '(3) 10)])
                    (assert-equal (cdr (assq 'iterations result)) 10)))
            
            (define-test benchmark-min-lte-max
              (let* ([f (lambda (x) (traced-mul x x))]
                     [result (benchmark-gradient f '(5) 5)])
                    (assert-true (<= (cdr (assq 'min-us result))
                                     (cdr (assq 'max-us result)))))))

;;; ====
;;; Integration Tests
;;; ====

(test-group integration-tests
            (define-test complex-function-profile
              ;; f(x,y,z) = sin(x*y) + exp(z)
              (let* ([f (lambda (x y z)
                                (traced-add
                                 (traced-sin (traced-mul x y))
                                 (traced-exp z)))]
                     [report (profile-computation f '(1 2 0))])
                    ;; Just check it runs and produces sensible output
                    (let* ([tape-info (cdr (assq 'tape report))]
                           [size (cdr (assq 'size tape-info))])
                          (assert-true (> size 0))
                          (assert-true (> (cdr (assq 'memory-bytes report)) 0)))))
            
            (define-test chain-rule-tape-growth
              ;; Longer chains should have larger tapes
              (reset-traced-ids!)
              (let* ([tape1 (make-reverse-tape)]
                     [x1 (make-traced-var 1 tape1)]
                     [_ (traced-sq x1)]
                     [size1 (tape-size tape1)])
                    (reset-traced-ids!)
                    (let* ([tape2 (make-reverse-tape)]
                           [x2 (make-traced-var 1 tape2)]
                           [_ (traced-sq (traced-sq (traced-sq x2)))]
                           [size2 (tape-size tape2)])
                          (assert-true (> size2 size1))))))

;;; ====
;;; Summary
;;; ====

(display "
====
")
(printf "Tests passed: ~a~n" *tests-passed*)
(printf "Tests failed: ~a~n" *tests-failed*)
(printf "Total tests:  ~a~n" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All autodiff profiling tests passed.
")
    (display "
[FAILURE] Some autodiff profiling tests failed.
"))
