;;; core/types/test-tactics.ss --- Tests for Proof Tactics
;;;
;;; Tests for the proof tactic system.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/dep-infer.ss")
(load "core/types/tactics.ss")

;;; ====
;;; Test Framework
;;; ====

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "✓ ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "✗ ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

(define (test-success name result)
  (test name #t (tactic-success? result)))

(define (test-failure name result)
  (test name #t (tactic-failure? result)))

;;; ====
;;; Goal Tests
;;; ====

(define (run-goal-tests)
  (display "
--- Goal Tests ---
")
  
  ;; Goal construction
  (let ([g (make-goal '() '(= Nat x x))])
       (test-true "goal? on goal" (goal? g))
       (test "goal-context empty" '() (goal-context g))
       (test "goal-proposition" '(= Nat x x) (goal-proposition g)))
  
  ;; Goal with context
  (let ([g (make-goal '((x . Nat) (y . Nat)) '(= Nat x y))])
       (test "goal-context non-empty" '((x . Nat) (y . Nat)) (goal-context g))
       (test "goal-proposition with vars" '(= Nat x y) (goal-proposition g))))

;;; ====
;;; Reflexivity Tests
;;; ====

(define (run-reflexivity-tests)
  (display "
--- Reflexivity Tests ---
")
  
  ;; Simple reflexivity: x = x
  (let* ([goal (make-goal '() '(= Nat x x))]
         [result (tactic-reflexivity goal)])
        (test-success "reflexivity on x = x" result)
        (test "reflexivity produces no subgoals" '() (tactic-subgoals result)))
  
  ;; Reflexivity on numeric literals
  (let* ([goal (make-goal '() '(= Nat 5 5))]
         [result (tactic-reflexivity goal)])
        (test-success "reflexivity on 5 = 5" result))
  
  ;; Reflexivity fails on x = y
  (let* ([goal (make-goal '() '(= Nat x y))]
         [result (tactic-reflexivity goal)])
        (test-failure "reflexivity fails on x = y" result))
  
  ;; Reflexivity fails on non-equality goal
  (let* ([goal (make-goal '() 'Nat)]
         [result (tactic-reflexivity goal)])
        (test-failure "reflexivity fails on non-equality" result)))

;;; ====
;;; Symmetry Tests
;;; ====

(define (run-symmetry-tests)
  (display "
--- Symmetry Tests ---
")
  
  ;; Symmetry transforms y = x to x = y
  (let* ([goal (make-goal '() '(= Nat y x))]
         [result (tactic-symmetry goal)])
        (test-success "symmetry on y = x" result)
        (test "symmetry produces 1 subgoal" 1 (length (tactic-subgoals result)))
        (let ([subgoal (car (tactic-subgoals result))])
             (test "symmetry subgoal is x = y" '(= Nat x y) (goal-proposition subgoal))))
  
  ;; Symmetry fails on non-equality
  (let* ([goal (make-goal '() 'Bool)]
         [result (tactic-symmetry goal)])
        (test-failure "symmetry fails on non-equality" result)))

;;; ====
;;; Transitivity Tests
;;; ====

(define (run-transitivity-tests)
  (display "
--- Transitivity Tests ---
")
  
  ;; Transitivity with middle term
  (let* ([goal (make-goal '() '(= Nat x z))]
         [result ((tactic-transitivity 'y) goal)])
        (test-success "transitivity x = z via y" result)
        (test "transitivity produces 2 subgoals" 2 (length (tactic-subgoals result)))
        (let ([sg1 (car (tactic-subgoals result))]
              [sg2 (cadr (tactic-subgoals result))])
             (test "transitivity subgoal 1 is x = y" '(= Nat x y) (goal-proposition sg1))
             (test "transitivity subgoal 2 is y = z" '(= Nat y z) (goal-proposition sg2))))
  
  ;; Transitivity fails on non-equality
  (let* ([goal (make-goal '() 'Int)]
         [result ((tactic-transitivity 'y) goal)])
        (test-failure "transitivity fails on non-equality" result)))

;;; ====
;;; Assumption Tests
;;; ====

(define (run-assumption-tests)
  (display "
--- Assumption Tests ---
")
  
  ;; Find hypothesis in context
  (let* ([ctx '((p . (= Nat x y)))]
         [goal (make-goal ctx '(= Nat x y))]
         [result (tactic-assumption goal)])
        (test-success "assumption finds hypothesis" result)
        (test "assumption no subgoals" '() (tactic-subgoals result)))
  
  ;; No matching hypothesis
  (let* ([ctx '((q . Bool))]
         [goal (make-goal ctx '(= Nat x y))]
         [result (tactic-assumption goal)])
        (test-failure "assumption fails without match" result))
  
  ;; Empty context
  (let* ([goal (make-goal '() '(= Nat x y))]
         [result (tactic-assumption goal)])
        (test-failure "assumption fails on empty context" result)))

;;; ====
;;; Trivial Tests
;;; ====

(define (run-trivial-tests)
  (display "
--- Trivial Tests ---
")
  
  ;; Trivial on literal true
  (let* ([goal (make-goal '() #t)]
         [result (tactic-trivial goal)])
        (test-success "trivial on #t" result))
  
  ;; Trivial on Unit
  (let* ([goal (make-goal '() 'Unit)]
         [result (tactic-trivial goal)])
        (test-success "trivial on Unit" result))
  
  ;; Trivial tries reflexivity
  (let* ([goal (make-goal '() '(= Nat 1 1))]
         [result (tactic-trivial goal)])
        (test-success "trivial on 1 = 1" result))
  
  ;; Trivial fails on complex goals
  (let* ([goal (make-goal '() '(= Nat x y))]
         [result (tactic-trivial goal)])
        (test-failure "trivial fails on x = y" result)))

;;; ====
;;; Intro Tests
;;; ====

(define (run-intro-tests)
  (display "
--- Intro Tests ---
")
  
  ;; Intro on arrow type
  (let* ([goal (make-goal '() '(-> Nat Bool))]
         [result ((tactic-intro 'n) goal)])
        (test-success "intro on -> Nat Bool" result)
        (test "intro produces 1 subgoal" 1 (length (tactic-subgoals result)))
        (let ([sg (car (tactic-subgoals result))])
             (test "intro context has n : Nat" '((n . Nat)) (goal-context sg))
             (test "intro subgoal is Bool" 'Bool (goal-proposition sg))))
  
  ;; Intro fails on non-function type
  (let* ([goal (make-goal '() 'Nat)]
         [result ((tactic-intro 'x) goal)])
        (test-failure "intro fails on non-function" result)))

;;; ====
;;; Combinator Tests
;;; ====

(define (run-combinator-tests)
  (display "
--- Combinator Tests ---
")
  
  ;; orelse: first succeeds
  (let* ([t (tactic-orelse tactic-reflexivity tactic-trivial)]
         [goal (make-goal '() '(= Nat x x))]
         [result (t goal)])
        (test-success "orelse uses first on success" result))
  
  ;; orelse: first fails, second succeeds
  (let* ([t (tactic-orelse tactic-fail tactic-trivial)]
         [goal (make-goal '() #t)]
         [result (t goal)])
        (test-success "orelse uses second on first failure" result))
  
  ;; try: tactic succeeds
  (let* ([t (tactic-try tactic-reflexivity)]
         [goal (make-goal '() '(= Nat x x))]
         [result (t goal)])
        (test-success "try succeeds when tactic succeeds" result))
  
  ;; try: tactic fails but try succeeds
  (let* ([t (tactic-try tactic-fail)]
         [goal (make-goal '() 'Nat)]
         [result (t goal)])
        (test-success "try succeeds even when tactic fails" result))
  
  ;; id: leaves goal unchanged
  (let* ([goal (make-goal '() '(= Nat x y))]
         [result (tactic-id goal)])
        (test-success "id succeeds" result)
        (test "id produces 1 subgoal" 1 (length (tactic-subgoals result)))
        (test "id subgoal unchanged" goal (car (tactic-subgoals result)))))

;;; ====
;;; Proof State Tests
;;; ====

(define (run-proof-state-tests)
  (display "
--- Proof State Tests ---
")
  
  ;; Create proof state
  (let* ([g1 (make-goal '() '(= Nat x x))]
         [g2 (make-goal '() 'Bool)]
         [ps (make-proof-state (list g1 g2))])
        (test-true "proof-state?" (proof-state? ps))
        (test "proof-state-goals" 2 (length (proof-state-goals ps)))
        (test-false "proof-state-complete? with goals" (proof-state-complete? ps)))
  
  ;; Empty proof state is complete
  (let ([ps (make-proof-state '())])
       (test-true "proof-state-complete? empty" (proof-state-complete? ps)))
  
  ;; Apply tactic to proof state
  (let* ([goal (make-goal '() '(= Nat x x))]
         [ps (make-proof-state (list goal))]
         [result (apply-tactic tactic-reflexivity ps)])
        (test-true "apply-tactic success" (proof-state? result))
        (test-true "apply-tactic completes state" (proof-state-complete? result))))

;;; ====
;;; QED Helper Tests
;;; ====

(define (run-qed-tests)
  (display "
--- QED Helper Tests ---
")
  
  (test "qed-reflexivity" '(refl Nat 5) (qed-reflexivity 'Nat 5))
  (test "qed-symmetry" '(sym _ p) (qed-symmetry 'p))
  (test "qed-transitivity" '(trans _ p q) (qed-transitivity 'p 'q))
  (test "qed-congruence" '(cong f p) (qed-congruence 'f 'p)))

;;; ====
;;; Auto Tactic Tests
;;; ====

(define (run-auto-tests)
  (display "
--- Auto Tactic Tests ---
")
  
  ;; Auto solves reflexivity
  (let* ([goal (make-goal '() '(= Nat x x))]
         [result (tactic-auto goal)])
        (test-success "auto solves x = x" result))
  
  ;; Auto uses assumption
  (let* ([ctx '((p . (= Nat a b)))]
         [goal (make-goal ctx '(= Nat a b))]
         [result (tactic-auto goal)])
        (test-success "auto uses assumption" result)))

;;; ====
;;; Run All Tests
;;; ====

(define (run-tests)
  (display "
=== Proof Tactics Test Suite ===

")
  
  (run-goal-tests)
  (run-reflexivity-tests)
  (run-symmetry-tests)
  (run-transitivity-tests)
  (run-assumption-tests)
  (run-trivial-tests)
  (run-intro-tests)
  (run-combinator-tests)
  (run-proof-state-tests)
  (run-qed-tests)
  (run-auto-tests)
  
  (display "
=== Test Summary ===
")
  (display "Total: ") (display *test-count*)
  (display ", Passed: ") (display *pass-count*)
  (display ", Failed: ") (display *fail-count*)
  (newline)
  
  (if (= *fail-count* 0)
      (display "All tests passed!
")
      (display "Some tests failed.
"))
  
  (= *fail-count* 0))

;;; Run tests when loaded
(run-tests)
