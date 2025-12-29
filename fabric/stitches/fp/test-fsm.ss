;;; fabric/stitches/fp/test-fsm.ss — Tests for Finite State Machine Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/fsm.ss")

(display "
")
(display "==============================================================
")
(display "         FINITE STATE MACHINE TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Basic FSM Construction Tests
;;; ============================================================

(test-group fsm-construction
            (define-test make-fsm-test
              (let ([m (make-fsm '(q0 q1) '(a b) '() 'q0 '(q1))])
                   (assert-true (fsm? m))
                   (assert-equal '(q0 q1) (fsm-states m))
                   (assert-equal '(a b) (fsm-alphabet m))
                   (assert-equal 'q0 (fsm-start m))
                   (assert-equal '(q1) (fsm-accepting m))))
            
            (define-test dfa-construction-test
              (let ([m (dfa '(q0 q1 q2) '(a b)
                            '((q0 a q1) (q1 b q2))
                            'q0 '(q2))])
                   (assert-true (fsm? m))
                   (assert-true (fsm-deterministic? m))))
            
            (define-test nfa-construction-test
              (let ([m (nfa '(q0 q1 q2) '(a)
                            '(((q0 . a) q1 q2))  ; Multiple targets from q0 on 'a'
                            'q0 '(q1 q2))])
                   (assert-true (fsm? m))
                   (assert-false (fsm-deterministic? m))))
            
            (define-test epsilon-nfa-test
              (let ([m (epsilon-nfa '(q0 q1) '(a)
                                    '(((q0 . a) q1))
                                    'q0 '(q1)
                                    '((q0 q1)))])  ; ε-transition q0 -> q1
                   (assert-false (fsm-deterministic? m))
                   (assert-equal '((q0 q1)) (fsm-epsilon m)))))

;;; ============================================================
;;; FSM Execution Tests
;;; ============================================================

(test-group fsm-execution
            (define-test simple-dfa-accepts-test
              ;; DFA that accepts "ab"
              (let ([m (dfa '(q0 q1 q2) '(# #)
                            '((q0 # q1) (q1 # q2))
                            'q0 '(q2))])
                   (assert-true (fsm-accepts? m "ab"))
                   (assert-false (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "b"))
                   (assert-false (fsm-accepts? m "ba"))
                   (assert-false (fsm-accepts? m ""))))
            
            (define-test dfa-loop-test
              ;; DFA that accepts strings of 'a's
              (let ([m (dfa '(q0) '(#)
                            '((q0 # q0))
                            'q0 '(q0))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "aaa"))
                   (assert-false (fsm-accepts? m "b"))))
            
            (define-test epsilon-closure-test
              ;; ε-NFA: q0 --ε--> q1 --a--> q2
              (let ([m (epsilon-nfa '(q0 q1 q2) '(#)
                                    '(((q1 . #) q2))
                                    'q0 '(q2)
                                    '((q0 q1)))])
                   ;; From q0, ε-closure should include q0 and q1
                   (let ([closure (epsilon-closure m 'q0)])
                        (assert-true (not (not (member 'q0 closure))))
                        (assert-true (not (not (member 'q1 closure)))))
                   ;; Should accept "a" via ε-move
                   (assert-true (fsm-accepts? m "a"))))
            
            (define-test nfa-multiple-paths-test
              ;; NFA with multiple paths
              (let ([m (nfa '(q0 q1 q2 q3) '(#)
                            '(((q0 . #) q1 q2)   ; q0 --a--> q1 or q2
                              ((q1 . #) q3)       ; q1 --a--> q3
                              ((q2 . #) q3))      ; q2 --a--> q3
                            'q0 '(q3))])
                   (assert-true (fsm-accepts? m "aa"))
                   (assert-false (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "aaa")))))

;;; ============================================================
;;; FSM Builder Tests
;;; ============================================================

(test-group fsm-builders
            (define-test fsm-char-test
              (let ([m (fsm-char #\x)])
                   (assert-true (fsm-accepts? m "x"))
                   (assert-false (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "y"))
                   (assert-false (fsm-accepts? m "xx"))))
            
            (define-test fsm-epsilon-lang-test
              (let ([m (fsm-epsilon-lang)])
                   (assert-true (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "a"))))
            
            (define-test fsm-literal-test
              (let ([m (fsm-literal "hello")])
                   (assert-true (fsm-accepts? m "hello"))
                   (assert-false (fsm-accepts? m "hell"))
                   (assert-false (fsm-accepts? m "helloo"))
                   (assert-false (fsm-accepts? m ""))))
            
            (define-test fsm-any-of-test
              (let ([m (fsm-any-of '(# # #