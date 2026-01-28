;;; core/fp/control-systems/test-state-space.ss — Tests for State Space Models
;;;
;;; NOTE: Run from project root: scheme --script core/fp/control-systems/test-state-space.ss

(load "core/test-framework.ss")
(load "lattice/fp/control-systems/state-space.ss")

(display "\n")
(display "====\n")
(display "         STATE SPACE MODELS TESTS\n")
(display "====\n")

;;; ====
;;; Construction Tests
;;; ====

(test-group construction
            (define-test scalar-system
              ;; Simple first-order system: x' = -x + u, y = x
              (let ([sys (ss-scalar -1 1 1 0)])
                   (assert-true (ss? sys))
                   (assert-equal 1 (ss-order sys))
                   (assert-equal 1 (ss-inputs sys))
                   (assert-equal 1 (ss-outputs sys))))
            
            (define-test second-order-system
              ;; Mass-spring-damper: x' = Ax + Bu
              (let* ([A (matrix-from-lists '((0 1) (-2 -3)))]
                     [B (matrix-from-lists '((0) (1)))]
                     [C (matrix-from-lists '((1 0)))]
                     [D (matrix-from-lists '((0)))]
                     [sys (make-ss A B C D)])
                    (assert-true (ss? sys))
                    (assert-equal 2 (ss-order sys))
                    (assert-equal 1 (ss-inputs sys))
                    (assert-equal 1 (ss-outputs sys))))
            
            (define-test from-lists
              (let ([sys (ss-from-lists '((0 1) (-1 -1))
                                        '((0) (1))
                                        '((1 0))
                                        '((0)))])
                   (assert-true (ss? sys))
                   (assert-equal 2 (ss-order sys))))
            
            (define-test dimension-mismatch-A
              ;; A must be square
              (let* ([A (matrix-from-lists '((1 2 3) (4 5 6)))] ; 2x3
                     [B (matrix-from-lists '((1) (2)))]
                     [C (matrix-from-lists '((1 2)))]
                     [D (matrix-from-lists '((0)))]
                     [result (make-ss A B C D)])
                    (assert-true (pair? result))
                    (assert-equal 'error (car result))))
            
            (define-test integrator-chain
              (let ([sys (ss-integrator 3)])
                   (assert-true (ss? sys))
                   (assert-equal 3 (ss-order sys))
                   ;; Check A has correct structure (1s on superdiagonal)
                   (let ([A (ss-A sys)])
                        (assert-equal 1 (matrix-ref A 0 1))
                        (assert-equal 1 (matrix-ref A 1 2))
                        (assert-equal 0 (matrix-ref A 0 0))
                        (assert-equal 0 (matrix-ref A 2 2))))))

;;; ====
;;; State and Output Equation Tests
;;; ====

(test-group equations
            (define-test state-equation-scalar
              (let* ([sys (ss-scalar -2 3 1 0)]
                     [x (vector 5)]
                     [u (vector 1)]
                     [xdot (ss-state-equation sys x u)])
                    ;; x' = -2*5 + 3*1 = -10 + 3 = -7
                    (assert-equal -7 (vector-ref xdot 0))))
            
            (define-test output-equation-scalar
              (let* ([sys (ss-scalar -2 3 4 5)]
                     [x (vector 2)]
                     [u (vector 1)]
                     [y (ss-output-equation sys x u)])
                    ;; y = 4*2 + 5*1 = 8 + 5 = 13
                    (assert-equal 13 (vector-ref y 0))))
            
            (define-test equations-second-order
              (let* ([sys (ss-from-lists '((0 1) (-1 -2))
                                         '((0) (1))
                                         '((1 0))
                                         '((0)))]
                     [x (vector 1 2)]
                     [u (vector 3)]
                     [xdot (ss-state-equation sys x u)]
                     [y (ss-output-equation sys x u)])
                    ;; x' = [0 1; -1 -2] * [1; 2] + [0; 1] * 3
                    ;;    = [2; -1-4] + [0; 3] = [2; -2]
                    (assert-equal 2 (vector-ref xdot 0))
                    (assert-equal -2 (vector-ref xdot 1))
                    ;; y = [1 0] * [1; 2] = 1
                    (assert-equal 1 (vector-ref y 0)))))

;;; ====
;;; Controllability Matrix Tests
;;; ====

(test-group controllability
            (define-test controllability-matrix-scalar
              ;; Scalar system: C = [B] = [b]
              (let* ([sys (ss-scalar -1 2 1 0)]
                     [C-mat (ss-controllability-matrix sys)])
                    (assert-equal 1 (matrix-rows C-mat))
                    (assert-equal 1 (matrix-cols C-mat))
                    (assert-equal 2 (matrix-ref C-mat 0 0))))
            
            (define-test controllability-matrix-second-order
              ;; Controllability matrix for 2nd order system
              (let* ([sys (ss-from-lists '((0 1) (0 0))
                                         '((0) (1))
                                         '((1 0))
                                         '((0)))]
                     [C-mat (ss-controllability-matrix sys)])
                    ;; C = [B AB] = [[0;1] [1;0]]
                    (assert-equal 2 (matrix-rows C-mat))
                    (assert-equal 2 (matrix-cols C-mat))
                    (assert-equal 0 (matrix-ref C-mat 0 0))
                    (assert-equal 1 (matrix-ref C-mat 1 0))
                    (assert-equal 1 (matrix-ref C-mat 0 1))
                    (assert-equal 0 (matrix-ref C-mat 1 1))))
            
            (define-test controllable-system
              (let* ([sys (ss-from-lists '((0 1) (0 0))
                                         '((0) (1))
                                         '((1 0))
                                         '((0)))])
                    ;; Double integrator is controllable
                    (assert-true (ss-controllable? sys 1e-10))))
            
            (define-test uncontrollable-system
              (let* ([sys (ss-from-lists '((1 0) (0 2))  ; diagonal A
                                         '((1) (0))      ; B only affects first state
                                         '((1 1))
                                         '((0)))])
                    ;; Second state is not controllable
                    (assert-false (ss-controllable? sys 1e-10)))))

;;; ====
;;; Observability Matrix Tests
;;; ====

(test-group observability
            (define-test observability-matrix-scalar
              (let* ([sys (ss-scalar -1 1 3 0)]
                     [O-mat (ss-observability-matrix sys)])
                    (assert-equal 1 (matrix-rows O-mat))
                    (assert-equal 1 (matrix-cols O-mat))
                    (assert-equal 3 (matrix-ref O-mat 0 0))))
            
            (define-test observability-matrix-second-order
              (let* ([sys (ss-from-lists '((0 1) (0 0))
                                         '((0) (1))
                                         '((1 0))
                                         '((0)))]
                     [O-mat (ss-observability-matrix sys)])
                    ;; O = [C; CA] = [[1 0]; [0 1]]
                    (assert-equal 2 (matrix-rows O-mat))
                    (assert-equal 2 (matrix-cols O-mat))
                    (assert-equal 1 (matrix-ref O-mat 0 0))
                    (assert-equal 0 (matrix-ref O-mat 0 1))
                    (assert-equal 0 (matrix-ref O-mat 1 0))
                    (assert-equal 1 (matrix-ref O-mat 1 1))))
            
            (define-test observable-system
              (let* ([sys (ss-from-lists '((0 1) (0 0))
                                         '((0) (1))
                                         '((1 0))
                                         '((0)))])
                    (assert-true (ss-observable? sys 1e-10))))
            
            (define-test unobservable-system
              (let* ([sys (ss-from-lists '((1 0) (0 2))  ; diagonal A
                                         '((1) (1))
                                         '((1 0))       ; C only sees first state
                                         '((0)))])
                    ;; Second state is not observable
                    (assert-false (ss-observable? sys 1e-10)))))

;;; ====
;;; Gramian Tests
;;; ====

(test-group gramians
            (define-test controllability-gramian-scalar
              (let* ([sys (ss-scalar -1 1 1 0)]
                     [Wc (ss-controllability-gramian-finite sys 10)])
                    ;; Wc should be positive for stable controllable system
                    (assert-true (> (matrix-ref Wc 0 0) 0))))
            
            (define-test observability-gramian-scalar
              (let* ([sys (ss-scalar -1 1 1 0)]
                     [Wo (ss-observability-gramian-finite sys 10)])
                    ;; Wo should be positive for stable observable system
                    (assert-true (> (matrix-ref Wo 0 0) 0))))
            
            (define-test gramian-symmetry
              ;; Gramians should be symmetric
              (let* ([sys (ss-from-lists '((-1 0) (0 -2))
                                         '((1) (1))
                                         '((1 1))
                                         '((0)))]
                     [Wc (ss-controllability-gramian-finite sys 20)]
                     [Wo (ss-observability-gramian-finite sys 20)])
                    ;; Check symmetry Wc[0,1] ≈ Wc[1,0]
                    (assert-true (< (abs (- (matrix-ref Wc 0 1)
                                            (matrix-ref Wc 1 0)))
                                    1e-10))
                    (assert-true (< (abs (- (matrix-ref Wo 0 1)
                                            (matrix-ref Wo 1 0)))
                                    1e-10)))))

;;; ====
;;; Transformation Tests
;;; ====

(test-group transformation
            (define-test identity-transform
              (let* ([sys (ss-from-lists '((1 2) (3 4))
                                         '((1) (0))
                                         '((1 0))
                                         '((0)))]
                     [I (matrix-identity 2)]
                     [sys2 (ss-transform sys I I)])
                    ;; Identity transform should preserve system
                    (assert-true (ss? sys2))
                    (assert-equal (matrix-ref (ss-A sys) 0 0)
                                  (matrix-ref (ss-A sys2) 0 0)))))

;;; ====
;;; Display Tests
;;; ====

(test-group display-tests
            (define-test ss->string-test
              (let* ([sys (ss-integrator 3)]
                     [str (ss->string sys)])
                    (assert-true (string? str))
                    (assert-true (> (string-length str) 0)))))

;;; ====
;;; Summary
;;; ====

(display "\n")
(display "====\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All state space tests passed.\n")
    (display "\n[FAILURE] Some state space tests failed.\n"))
