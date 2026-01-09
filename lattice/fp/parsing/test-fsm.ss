;;; fabric/stitches/fp/test-fsm.ss — Tests for Finite State Machine Library
;;;
;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "lattice/fp/parsing/fsm.ss")

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
                                    '((q0 q1)))])  ; epsilon-transition q0 -> q1
                   (assert-false (fsm-deterministic? m))
                   (assert-equal '((q0 q1)) (fsm-epsilon m)))))

;;; ============================================================
;;; FSM Execution Tests
;;; ============================================================

(define char-a (string-ref "a" 0))
(define char-b (string-ref "b" 0))
(define char-c (string-ref "c" 0))
(define char-x (string-ref "x" 0))

(test-group fsm-execution
            (define-test simple-dfa-accepts-test
              ;; DFA that accepts "ab"
              (let ([m (dfa '(q0 q1 q2) (list char-a char-b)
                            (list (list 'q0 char-a 'q1) (list 'q1 char-b 'q2))
                            'q0 '(q2))])
                   (assert-true (fsm-accepts? m "ab"))
                   (assert-false (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "b"))
                   (assert-false (fsm-accepts? m "ba"))
                   (assert-false (fsm-accepts? m ""))))
            
            (define-test dfa-loop-test
              ;; DFA that accepts strings of 'a's
              (let ([m (dfa '(q0) (list char-a)
                            (list (list 'q0 char-a 'q0))
                            'q0 '(q0))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "aaa"))
                   (assert-false (fsm-accepts? m "b"))))
            
            (define-test epsilon-closure-test
              ;; epsilon-NFA: q0 --epsilon--> q1 --a--> q2
              (let ([m (epsilon-nfa '(q0 q1 q2) (list char-a)
                                    (list (cons (cons 'q1 char-a) '(q2)))
                                    'q0 '(q2)
                                    '((q0 q1)))])
                   ;; From q0, epsilon-closure should include q0 and q1
                   (let ([closure (epsilon-closure m 'q0)])
                        (assert-true (not (not (member 'q0 closure))))
                        (assert-true (not (not (member 'q1 closure)))))
                   ;; Should accept "a" via epsilon-move
                   (assert-true (fsm-accepts? m "a"))))
            
            (define-test nfa-multiple-paths-test
              ;; NFA with multiple paths
              (let ([m (nfa '(q0 q1 q2 q3) (list char-a)
                            (list (cons (cons 'q0 char-a) '(q1 q2))
                                  (cons (cons 'q1 char-a) '(q3))
                                  (cons (cons 'q2 char-a) '(q3)))
                            'q0 '(q3))])
                   (assert-true (fsm-accepts? m "aa"))
                   (assert-false (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "aaa")))))

;;; ============================================================
;;; FSM Builder Tests
;;; ============================================================

(test-group fsm-builders
            (define-test fsm-char-test
              (let ([m (fsm-char char-x)])
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
              (let ([m (fsm-any-of (list char-a char-b char-c))])
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "b"))
                   (assert-true (fsm-accepts? m "c"))
                   (assert-false (fsm-accepts? m "d"))
                   (assert-false (fsm-accepts? m "ab")))))

;;; ============================================================
;;; FSM Composition Tests
;;; ============================================================

(test-group fsm-composition
            (define-test fsm-union-test
              ;; Union of "a" and "b" should accept either
              (let ([m (fsm-union (fsm-char char-a) (fsm-char char-b))])
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "b"))
                   (assert-false (fsm-accepts? m "c"))
                   (assert-false (fsm-accepts? m "ab"))))
            
            (define-test fsm-concat-test
              ;; Concat of "a" and "b" should accept "ab"
              (let ([m (fsm-concat (fsm-char char-a) (fsm-char char-b))])
                   (assert-true (fsm-accepts? m "ab"))
                   (assert-false (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "b"))
                   (assert-false (fsm-accepts? m "ba"))))
            
            (define-test fsm-star-test
              ;; Star of "a" should accept "", "a", "aa", etc.
              (let ([m (fsm-star (fsm-char char-a))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "aa"))
                   (assert-true (fsm-accepts? m "aaa"))
                   (assert-false (fsm-accepts? m "b"))
                   (assert-false (fsm-accepts? m "ab"))))
            
            (define-test fsm-plus-test
              ;; Plus of "a" should accept "a", "aa", etc. but not ""
              (let ([m (fsm-plus (fsm-char char-a))])
                   (assert-false (fsm-accepts? m ""))
                   (assert-true (fsm-accepts? m "a"))
                   (assert-true (fsm-accepts? m "aa"))
                   (assert-true (fsm-accepts? m "aaa"))))
            
            (define-test fsm-optional-test
              ;; Optional of "a" should accept "" or "a"
              (let ([m (fsm-optional (fsm-char char-a))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-true (fsm-accepts? m "a"))
                   (assert-false (fsm-accepts? m "aa"))))
            
            (define-test complex-composition-test
              ;; (a|b)*c - strings of a's and b's followed by c
              (let* ([ab (fsm-union (fsm-char char-a) (fsm-char char-b))]
                     [ab-star (fsm-star ab)]
                     [m (fsm-concat ab-star (fsm-char char-c))])
                    (assert-true (fsm-accepts? m "c"))
                    (assert-true (fsm-accepts? m "ac"))
                    (assert-true (fsm-accepts? m "bc"))
                    (assert-true (fsm-accepts? m "abc"))
                    (assert-true (fsm-accepts? m "aabc"))
                    (assert-false (fsm-accepts? m ""))
                    (assert-false (fsm-accepts? m "ab")))))

;;; ============================================================
;;; NFA to DFA Conversion Tests
;;; ============================================================

(test-group nfa-to-dfa
            (define-test simple-nfa-to-dfa-test
              (let* ([nfa-m (nfa '(q0 q1 q2) (list char-a char-b)
                                 (list (cons (cons 'q0 char-a) '(q0 q1))
                                       (cons (cons 'q1 char-b) '(q2)))
                                 'q0 '(q2))]
                     [dfa-m (nfa->dfa nfa-m)])
                    (assert-true (fsm-deterministic? dfa-m))
                    ;; Should accept same language
                    (assert-true (fsm-accepts? dfa-m "ab"))
                    (assert-true (fsm-accepts? dfa-m "aab"))
                    (assert-true (fsm-accepts? dfa-m "aaab"))
                    (assert-false (fsm-accepts? dfa-m ""))
                    (assert-false (fsm-accepts? dfa-m "a"))
                    (assert-false (fsm-accepts? dfa-m "b"))))
            
            (define-test epsilon-nfa-to-dfa-test
              (let* ([enfa (epsilon-nfa '(q0 q1 q2) (list char-a)
                                        (list (cons (cons 'q1 char-a) '(q2)))
                                        'q0 '(q2)
                                        '((q0 q1)))]
                     [dfa-m (nfa->dfa enfa)])
                    (assert-true (fsm-deterministic? dfa-m))
                    (assert-true (fsm-accepts? dfa-m "a"))
                    (assert-false (fsm-accepts? dfa-m ""))))
            
            (define-test composed-nfa-to-dfa-test
              ;; Convert (a|b)* NFA to DFA
              (let* ([nfa-m (fsm-star (fsm-union (fsm-char char-a) (fsm-char char-b)))]
                     [dfa-m (nfa->dfa nfa-m)])
                    (assert-true (fsm-deterministic? dfa-m))
                    (assert-true (fsm-accepts? dfa-m ""))
                    (assert-true (fsm-accepts? dfa-m "a"))
                    (assert-true (fsm-accepts? dfa-m "b"))
                    (assert-true (fsm-accepts? dfa-m "ab"))
                    (assert-true (fsm-accepts? dfa-m "ba"))
                    (assert-true (fsm-accepts? dfa-m "aabb"))
                    (assert-false (fsm-accepts? dfa-m "c")))))

;;; ============================================================
;;; FSM Minimization Tests
;;; ============================================================

(test-group fsm-minimization
            (define-test minimize-redundant-states-test
              ;; DFA with redundant states
              (let* ([m (dfa '(q0 q1 q2 q3) (list char-a char-b)
                             (list (list 'q0 char-a 'q1)
                                   (list 'q0 char-b 'q2)
                                   (list 'q1 char-a 'q3)
                                   (list 'q2 char-a 'q3))  ; q1 and q2 are equivalent
                             'q0 '(q3))]
                     [min-m (fsm-minimize m)])
                    ;; Minimized should have fewer states
                    (assert-true (<= (length (fsm-states min-m)) (length (fsm-states m))))
                    ;; Should accept same language
                    (assert-true (fsm-accepts? min-m "aa"))
                    (assert-true (fsm-accepts? min-m "ba"))
                    (assert-false (fsm-accepts? min-m "a"))
                    (assert-false (fsm-accepts? min-m "ab"))))
            
            (define-test minimize-already-minimal-test
              ;; Already minimal DFA
              (let* ([m (dfa '(q0 q1) (list char-a)
                             (list (list 'q0 char-a 'q1))
                             'q0 '(q1))]
                     [min-m (fsm-minimize m)])
                    (assert-equal (length (fsm-states m)) (length (fsm-states min-m)))))
            
            (define-test minimize-nfa-via-conversion-test
              ;; Minimizing NFA should convert to DFA first
              (let* ([nfa-m (fsm-star (fsm-char char-a))]
                     [min-m (fsm-minimize nfa-m)])
                    (assert-true (fsm-deterministic? min-m))
                    (assert-true (fsm-accepts? min-m ""))
                    (assert-true (fsm-accepts? min-m "a"))
                    (assert-true (fsm-accepts? min-m "aaa")))))

;;; ============================================================
;;; Language Operation Tests
;;; ============================================================

(test-group language-operations
            (define-test fsm-reachable-test
              (let* ([m (dfa '(q0 q1 q2 unreachable) (list char-a)
                             (list (list 'q0 char-a 'q1) (list 'q1 char-a 'q2))
                             'q0 '(q2))]
                     [reachable (fsm-reachable m)])
                    (assert-true (not (not (member 'q0 reachable))))
                    (assert-true (not (not (member 'q1 reachable))))
                    (assert-true (not (not (member 'q2 reachable))))
                    (assert-false (not (not (member 'unreachable reachable))))))
            
            (define-test fsm-empty-test
              ;; FSM with no path to accepting state
              (let ([m (dfa '(q0 q1) (list char-a)
                            (list (list 'q0 char-a 'q0))  ; Loops on q0
                            'q0 '(q1))])    ; q1 unreachable
                   (assert-true (fsm-empty? m)))
              ;; FSM with path to accepting
              (let ([m (dfa '(q0 q1) (list char-a)
                            (list (list 'q0 char-a 'q1))
                            'q0 '(q1))])
                   (assert-false (fsm-empty? m))))
            
            (define-test fsm-intersect-test
              ;; Intersection: strings of a's with even length AND at least 2 a's
              ;; DFA1: even number of a's
              (let* ([even-as (dfa '(q0 q1) (list char-a)
                                   (list (list 'q0 char-a 'q1) (list 'q1 char-a 'q0))
                                   'q0 '(q0))]
                     ;; DFA2: at least 2 a's
                     [two-plus (dfa '(s0 s1 s2) (list char-a)
                                    (list (list 's0 char-a 's1) (list 's1 char-a 's2) (list 's2 char-a 's2))
                                    's0 '(s2))]
                     [inter (fsm-intersect even-as two-plus)])
                    (assert-true (fsm-accepts? inter "aa"))
                    (assert-true (fsm-accepts? inter "aaaa"))
                    (assert-false (fsm-accepts? inter ""))
                    (assert-false (fsm-accepts? inter "a"))
                    (assert-false (fsm-accepts? inter "aaa"))))
            
            (define-test fsm-complement-test
              ;; Complement of "accepts a" should reject "a" and accept others
              (let* ([accepts-a (dfa '(q0 q1 dead) (list char-a char-b)
                                     (list (list 'q0 char-a 'q1) (list 'q0 char-b 'dead)
                                           (list 'q1 char-a 'dead) (list 'q1 char-b 'dead)
                                           (list 'dead char-a 'dead) (list 'dead char-b 'dead))
                                     'q0 '(q1))]
                     [comp (fsm-complement accepts-a)])
                    (assert-false (fsm-accepts? comp "a"))
                    (assert-true (fsm-accepts? comp ""))
                    (assert-true (fsm-accepts? comp "b"))
                    (assert-true (fsm-accepts? comp "aa"))))
            
            (define-test fsm-complement-incomplete-dfa-test
              ;; Test complement of an incomplete DFA
              ;; DFA accepts only "a": state 0 -a-> state 1 (accept), no other transitions
              ;; This is incomplete: missing transitions for 'b' from q0, and both symbols from q1
              ;; Complement should accept everything EXCEPT "a"
              (let* ([accepts-only-a (dfa '(q0 q1) (list char-a char-b)
                                          (list (list 'q0 char-a 'q1))
                                          'q0 '(q1))]
                     [comp (fsm-complement accepts-only-a)])
                    ;; "a" was accepted, should now be rejected
                    (assert-false (fsm-accepts? comp "a"))
                    ;; "" was rejected (q0 not accepting), should now be accepted
                    (assert-true (fsm-accepts? comp ""))
                    ;; "b" had no transition from q0 (implicit reject), should now be accepted
                    (assert-true (fsm-accepts? comp "b"))
                    ;; "aa" - after "a" we're at q1, then "a" has no transition (implicit reject)
                    ;; After complement, this should be accepted
                    (assert-true (fsm-accepts? comp "aa"))
                    ;; "ab" - after "a" we're at q1, then "b" has no transition (implicit reject)
                    ;; After complement, this should be accepted
                    (assert-true (fsm-accepts? comp "ab"))
                    ;; "ba" - "b" from q0 has no transition (implicit reject)
                    ;; After complement, this should be accepted
                    (assert-true (fsm-accepts? comp "ba"))
                    ;; "bb" - "b" from q0 has no transition (implicit reject)
                    ;; After complement, this should be accepted
                    (assert-true (fsm-accepts? comp "bb")))))

;;; ============================================================
;;; Moore/Mealy Machine Tests
;;; ============================================================

(test-group moore-mealy
            (define-test moore-machine-test
              ;; Moore machine: output parity of a's seen
              (let* ([fsm (dfa '(even odd) (list char-a char-b)
                               (list (list 'even char-a 'odd) (list 'even char-b 'even)
                                     (list 'odd char-a 'even) (list 'odd char-b 'odd))
                               'even '(even odd))]
                     [outputs '((even . 0) (odd . 1))]
                     [moore (make-moore fsm outputs)])
                    (assert-true (moore? moore))
                    ;; Running on "aab" should produce: 0 (start) -> 1 (after a) -> 0 (after aa) -> 0 (after aab)
                    (let ([result (moore-run moore "aab")])
                         (assert-equal 4 (length result))
                         (assert-equal 0 (car result))   ; start: even
                         (assert-equal 1 (cadr result))  ; after 'a': odd
                         (assert-equal 0 (caddr result)) ; after 'aa': even
                         )))
            
            (define-test mealy-machine-test
              ;; Mealy machine: output 1 when transitioning to odd, 0 otherwise
              (let* ([fsm (dfa '(even odd) (list char-a)
                               (list (list 'even char-a 'odd) (list 'odd char-a 'even))
                               'even '(even odd))]
                     [trans-outputs (list (cons (cons 'even char-a) 1)
                                          (cons (cons 'odd char-a) 0))]
                     [mealy (make-mealy fsm trans-outputs)])
                    (assert-true (mealy? mealy))
                    (let ([result (mealy-run mealy "aaa")])
                         (assert-equal 3 (length result))
                         (assert-equal 1 (car result))    ; even -> odd
                         (assert-equal 0 (cadr result))   ; odd -> even
                         (assert-equal 1 (caddr result))) ; even -> odd
                    )))

;;; ============================================================
;;; Visualization Tests
;;; ============================================================

;;; Helper for string-contains
(define (string-contains str substr)
  (let ([slen (string-length str)]
        [sublen (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) slen) #f]
             [(string=? (substring str i (+ i sublen)) substr) #t]
             [else (loop (+ i 1))]))))

(test-group visualization
            (define-test fsm-to-dot-test
              (let* ([m (dfa '(q0 q1) (list char-a)
                             (list (list 'q0 char-a 'q1))
                             'q0 '(q1))]
                     [dot (fsm->dot m)])
                    (assert-true (string? dot))
                    (assert-true (> (string-length dot) 0))
                    ;; Should contain basic DOT elements
                    (assert-true (not (not (string-contains dot "digraph"))))
                    (assert-true (not (not (string-contains dot "doublecircle"))))))
            
            (define-test fsm-to-string-test
              (let* ([m (dfa '(q0 q1) (list char-a)
                             (list (list 'q0 char-a 'q1))
                             'q0 '(q1))]
                     [s (fsm->string m)])
                    (assert-true (string? s))
                    (assert-true (> (string-length s) 0))))
            
            ;; Test DOT injection prevention
            ;; State names with special characters should be escaped
            (define-test fsm-to-dot-injection-test
              ;; Test with malicious state names containing quotes, backslashes, newlines
              ;; These could allow DOT code injection if not properly escaped
              ;; The payload attempts to close the quoted string and inject DOT code
              (let* ([evil-state1 (string->symbol "bad\" -> evil; \"")]
                     [evil-state2 (string->symbol "q1")]
                     [m (make-fsm (list evil-state1 evil-state2)
                                  (list char-a)
                                  (list (cons (cons evil-state1 char-a) (list evil-state2)))
                                  evil-state1
                                  (list evil-state2))]
                     [dot (fsm->dot m)])
                    ;; Output should be valid - quotes are escaped
                    (assert-true (string? dot))
                    ;; Escaped double-quotes should be present (injection is neutralized)
                    (assert-true (string-contains dot "\\\""))
                    ;; The malicious state name should appear with escaped quotes
                    ;; NOT as: "bad" -> evil; "" (which would be injection)
                    ;; BUT as: "bad\" -> evil; \"" (properly escaped, single quoted string)
                    (assert-true (string-contains dot "\"bad\\\" -> evil; \\\"\""))))
            
            (define-test fsm-to-dot-escape-backslash-test
              ;; Test that backslashes are properly escaped
              (let* ([bs-state (string->symbol "q\\0")]
                     [m (make-fsm (list bs-state) (list char-a) '() bs-state (list bs-state))]
                     [dot (fsm->dot m)])
                    ;; Should contain escaped backslash
                    (assert-true (string-contains dot "\\\\"))))
            
            (define-test fsm-to-dot-custom-name-test
              ;; Test that custom graph names are also escaped
              (let* ([m (dfa '(q0) (list char-a) '() 'q0 '(q0))]
                     [dot (fsm->dot m "test\"graph")])
                    ;; Graph name quotes should be escaped
                    (assert-true (string-contains dot "test\\\"graph")))))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(test-group edge-cases
            (define-test empty-alphabet-test
              (let ([m (make-fsm '(q0) '() '() 'q0 '(q0))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "a"))))
            
            (define-test single-state-accepting-test
              (let ([m (make-fsm '(q0) (list char-a) '() 'q0 '(q0))])
                   (assert-true (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "a"))))
            
            (define-test single-state-not-accepting-test
              (let ([m (make-fsm '(q0) (list char-a) '() 'q0 '())])
                   (assert-false (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "a"))))
            
            (define-test fsm-literal-empty-test
              (let ([m (fsm-literal "")])
                   (assert-true (fsm-accepts? m ""))
                   (assert-false (fsm-accepts? m "a")))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All FSM tests passed.
")
    (display "
[FAILURE] Some FSM tests failed.
"))
