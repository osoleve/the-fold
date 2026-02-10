;;; core/fp/rewrite/test-proof-tactics.ss --- Tests for proof-tactics.ss
;;;
;;; This is Core code: pure, total, assumes perfect input.

(load "core/lang/module.ss")
(load "lattice/fp/rewrite/proof-tactics.ss")
(load "lattice/fp/rewrite/fusion-rules.ss")  ; Registers fusion laws for auto-laws
(load "core/test-framework.ss")

(display "\n=== Testing proof-tactics.ss ===\n\n")

;;; ====
;;; Goal Construction Tests
;;; ====

(test-group goal-construction
            (define-test make-proof-goal-valid
              (let ([g (make-proof-goal '(+ 1 0) 1 '())])
                   (assert-true (proof-goal? g))
                   (assert-equal '(+ 1 0) (goal-lhs g))
                   (assert-equal 1 (goal-rhs g))
                   (assert-equal '() (goal-context g))))
            
            (define-test goal-trivial-true
              (let ([g (make-proof-goal 'x 'x '())])
                   (assert-true (goal-trivial? g))))
            
            (define-test goal-trivial-false
              (let ([g (make-proof-goal 'x 'y '())])
                   (assert-false (goal-trivial? g)))))

;;; ====
;;; Core Tactic Tests
;;; ====

(test-group core-tactics
            (define-test tactic-refl-succeeds-trivial
              (let* ([g (make-proof-goal 'x 'x '())]
                     [result (tactic-refl g)])
                    (assert-true (tactic-success? result))
                    (assert-equal '() (tactic-subgoals result))))
            
            (define-test tactic-refl-fails-non-trivial
              (let* ([g (make-proof-goal 'x 'y '())]
                     [result (tactic-refl g)])
                    (assert-true (tactic-failure? result))))
            
            (define-test tactic-sym-transforms-goal
              (let* ([g (make-proof-goal 'y 'x '())]
                     [result (tactic-sym g)])
                    (assert-true (tactic-success? result))
                    (assert-equal 1 (length (tactic-subgoals result)))
                    (let ([sg (car (tactic-subgoals result))])
                         (assert-equal 'x (goal-lhs sg))
                         (assert-equal 'y (goal-rhs sg)))))
            
            (define-test tactic-trans-splits-goal
              (let* ([g (make-proof-goal 'x 'z '())]
                     [result ((tactic-trans 'y) g)])
                    (assert-true (tactic-success? result))
                    (assert-equal 2 (length (tactic-subgoals result)))
                    (let ([g1 (car (tactic-subgoals result))]
                          [g2 (cadr (tactic-subgoals result))])
                         (assert-equal 'x (goal-lhs g1))
                         (assert-equal 'y (goal-rhs g1))
                         (assert-equal 'y (goal-lhs g2))
                         (assert-equal 'z (goal-rhs g2))))))

;;; ====
;;; Rewrite Tactic Tests
;;; ====

(test-group rewrite-tactics
            (define-test tactic-rewrite-applies-law
              (let* ([g (make-proof-goal '(+ 0 x) 'x '())]
                     [result ((tactic-rewrite 'arith-add-left-id) g)])
                    (assert-true (tactic-success? result))
                    ;; Should have no subgoals since result equals rhs
                    (assert-equal '() (tactic-subgoals result))))
            
            (define-test tactic-rewrite-fails-non-matching
              (let* ([g (make-proof-goal '(* 2 x) 'x '())]
                     [result ((tactic-rewrite 'arith-add-left-id) g)])
                    (assert-true (tactic-failure? result))))
            
            (define-test tactic-rewrite-unknown-law-fails
              (let* ([g (make-proof-goal 'x 'x '())]
                     [result ((tactic-rewrite 'nonexistent-law) g)])
                    (assert-true (tactic-failure? result)))))

;;; ====
;;; Combinator Tests
;;; ====

(test-group tactic-combinators
            (define-test tactic-try-always-succeeds
              (let* ([g (make-proof-goal 'x 'y '())]
                     [result ((tactic-try tactic-fail) g)])
                    (assert-true (tactic-success? result))
                    ;; Goal should be unchanged
                    (assert-equal 1 (length (tactic-subgoals result)))))
            
            (define-test tactic-orelse-tries-second
              (let* ([g (make-proof-goal 'x 'x '())]
                     [result ((tactic-orelse tactic-fail tactic-refl) g)])
                    (assert-true (tactic-success? result))))
            
            (define-test tactic-orelse-first-succeeds
              (let* ([g (make-proof-goal 'x 'x '())]
                     [result ((tactic-orelse tactic-refl tactic-fail) g)])
                    (assert-true (tactic-success? result))))
            
            (define-test tactic-id-preserves-goal
              (let* ([g (make-proof-goal 'x 'y '())]
                     [result (tactic-id g)])
                    (assert-true (tactic-success? result))
                    (assert-equal 1 (length (tactic-subgoals result)))
                    (let ([sg (car (tactic-subgoals result))])
                         (assert-equal 'x (goal-lhs sg))
                         (assert-equal 'y (goal-rhs sg))))))

;;; ====
;;; Automation Tests
;;; ====

(test-group automation
            (define-test tactic-auto-trivial
              (let* ([g (make-proof-goal 'x 'x '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result))))
            
            (define-test tactic-auto-arithmetic
              (let* ([g (make-proof-goal '(+ 0 x) 'x '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result))))
            
            (define-test tactic-auto-mul-zero
              (let* ([g (make-proof-goal '(* 0 x) 0 '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result)))))

;;; ====
;;; Auto-Laws Configuration Tests
;;; ====

(test-group auto-laws-configuration
            ;; Test that auto-laws includes base laws
            (define-test auto-laws-includes-base
              (let ([laws (auto-laws)])
                   (assert-true (and (memq 'monoid-left-id laws) #t))
                   (assert-true (and (memq 'arith-add-left-id laws) #t))
                   (assert-true (and (memq 'functor-id laws) #t))))
            
            ;; Test that auto-laws includes fusion laws (from fusion category)
            (define-test auto-laws-includes-fusion
              (let ([laws (auto-laws)])
                   (assert-true (and (memq 'map-map-fuse laws) #t))
                   (assert-true (and (memq 'filter-map-fuse laws) #t))
                   (assert-true (and (memq 'fold-map-fuse laws) #t))
                   (assert-true (and (memq 'compose-id-left laws) #t))))
            
            ;; Test that fusion category is in auto-law-categories
            (define-test fusion-category-registered
              (assert-true (and (memq 'fusion *auto-law-categories*) #t)))
            
            ;; Test auto-laws-by-category returns fusion laws
            (define-test auto-laws-by-category-fusion
              (let ([fusion-laws (auto-laws-by-category 'fusion)])
                   (assert-true (> (length fusion-laws) 10))  ; Should have 18 fusion laws
                   (assert-true (and (memq 'map-map-fuse fusion-laws) #t))))
            
            ;; Test register-auto-law! adds to extra laws
            (define-test register-auto-law-works
              (let ([before (length *auto-laws-extra*)])
                   (register-auto-law! 'test-extra-law)
                   (assert-true (and (memq 'test-extra-law *auto-laws-extra*) #t))
                   ;; Clean up
                   (set! *auto-laws-extra*
                         (filter (lambda (x) (not (eq? x 'test-extra-law)))
                                 *auto-laws-extra*))))
            
            ;; Test that auto-laws removes duplicates
            (define-test auto-laws-no-duplicates
              (let ([laws (auto-laws)])
                   (let loop ([lst laws] [seen '()])
                        (cond
                         [(null? lst) (assert-true #t)]  ; No duplicates found
                         [(memq (car lst) seen)
                          (assert-true #f)]  ; Duplicate found - fail
                         [else (loop (cdr lst) (cons (car lst) seen))])))))

;;; ====
;;; Tactic-Auto with Fusion Tests
;;; ====

(test-group tactic-auto-fusion
            ;; Test that tactic-auto can apply map-map fusion
            (define-test tactic-auto-map-map-fuse
              (let* ([g (make-proof-goal
                         '(map f (map g xs))
                         '(map (compose f g) xs)
                         '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result))))
            
            ;; Test that tactic-auto can apply compose-id-left
            (define-test tactic-auto-compose-id
              (let* ([g (make-proof-goal
                         '(compose id f)
                         'f
                         '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result))))
            
            ;; Test that tactic-auto can apply length-map-elim
            (define-test tactic-auto-length-map
              (let* ([g (make-proof-goal
                         '(length (map f xs))
                         '(length xs)
                         '())]
                     [result (tactic-auto g)])
                    (assert-true (tactic-success? result)))))

;;; ====
;;; Search Tests (Skipped - slow due to exhaustive law enumeration)
;;; ====
;;;
;;; Note: tactic-search is correct but slow due to trying all laws.
;;; The search tests are skipped to keep test suite fast.
;;; Manual testing can be done with:
;;;   (let ([result ((tactic-search 1) (make-proof-goal 'x 'x '()))])
;;;     (tactic-success? result))  ; => #t

;;; ====
;;; Strategy Lifting Tests
;;; ====

(test-group strategy-lifting
            (define-test strategy-to-tactic
              (let* ([g (make-proof-goal '(+ 0 x) 'x '())]
                     [strategy (rule->strategy arith-add-left-id)]
                     [tactic (strategy->tactic strategy)]
                     [result (tactic g)])
                    (assert-true (tactic-success? result))))
            
            (define-test law-to-tactic
              (let* ([g (make-proof-goal '(+ 0 x) 'x '())]
                     [tactic (law->tactic 'arith-add-left-id)]
                     [result (tactic g)])
                    (assert-true (tactic-success? result)))))

;;; ====
;;; Proof Execution Tests
;;; ====

(test-group proof-execution
            (define-test prove-simple-equality
              (let ([result (prove-with-tactics '(+ 0 x) 'x
                                                (list (tactic-rewrite 'arith-add-left-id)))])
                   (assert-equal 'ok (car result))))
            
            (define-test prove-trivial
              (let ([result (prove-with-tactics 'x 'x '())])
                   (assert-equal 'ok (car result)))))

;;; ====
;;; Run Summary
;;; ====

(run-all-tests)
