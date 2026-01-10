;;; core/fp/game/test-normal-form.ss — Tests for Normal Form Games
;;;
;;; NOTE: Run from project root: scheme --script core/fp/game/test-normal-form.ss

(load "core/test-framework.ss")
(load "lattice/fp/game/normal-form.ss")

(display "\n")
(display "==============================================================\n")
(display "         NORMAL FORM GAMES TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Game Construction Tests
;;; ============================================================

(test-group game-construction
            (define-test make-game-basic
              (let ([game (make-game '(a b) '(x y)
                                     '(((1 . 2) (3 . 4))
                                       ((5 . 6) (7 . 8))))])
                   (assert-equal 2 (game-num-strategies game 0))
                   (assert-equal 2 (game-num-strategies game 1))))
            
            (define-test game-payoff-access
              (let ([game (make-game '(a b) '(x y)
                                     '(((1 . 2) (3 . 4))
                                       ((5 . 6) (7 . 8))))])
                   (assert-equal '(1 . 2) (game-payoff game 0 0))
                   (assert-equal '(3 . 4) (game-payoff game 0 1))
                   (assert-equal '(5 . 6) (game-payoff game 1 0))
                   (assert-equal '(7 . 8) (game-payoff game 1 1))))
            
            (define-test game-payoff-individual
              (let ([game prisoners-dilemma])
                   (assert-equal 3 (game-payoff-p1 game 0 0))  ; (C,C)
                   (assert-equal 3 (game-payoff-p2 game 0 0))
                   (assert-equal 0 (game-payoff-p1 game 0 1))  ; (C,D)
                   (assert-equal 5 (game-payoff-p2 game 0 1))))
            
            (define-test classic-games-defined
              (assert-true (game%? prisoners-dilemma))
              (assert-true (game%? matching-pennies))
              (assert-true (game%? battle-of-sexes))
              (assert-true (game%? stag-hunt))
              (assert-true (game%? chicken))))

;;; ============================================================
;;; Best Response Tests
;;; ============================================================

(test-group best-response
            (define-test best-response-prisoners-dilemma
              ;; In PD, defect is always the best response
              (assert-equal '(1) (best-response-p1 prisoners-dilemma 0))  ; vs Cooperate
              (assert-equal '(1) (best-response-p1 prisoners-dilemma 1))  ; vs Defect
              (assert-equal '(1) (best-response-p2 prisoners-dilemma 0))  ; vs Cooperate
              (assert-equal '(1) (best-response-p2 prisoners-dilemma 1))) ; vs Defect
            
            (define-test best-response-matching-pennies
              ;; In MP, best response alternates
              (assert-equal '(0) (best-response-p1 matching-pennies 0))  ; vs Heads -> Heads
              (assert-equal '(1) (best-response-p1 matching-pennies 1))  ; vs Tails -> Tails
              (assert-equal '(1) (best-response-p2 matching-pennies 0))  ; vs Heads -> Tails
              (assert-equal '(0) (best-response-p2 matching-pennies 1))) ; vs Tails -> Heads
            
            (define-test best-response-battle-of-sexes
              ;; In BoS, coordinate on same event
              (assert-equal '(0) (best-response-p1 battle-of-sexes 0))  ; vs Opera
              (assert-equal '(1) (best-response-p1 battle-of-sexes 1))  ; vs Football
              (assert-equal '(0) (best-response-p2 battle-of-sexes 0))  ; vs Opera
              (assert-equal '(1) (best-response-p2 battle-of-sexes 1)))) ; vs Football

;;; ============================================================
;;; Dominated Strategy Tests
;;; ============================================================

(test-group dominated-strategies
            (define-test prisoners-dilemma-dominance
              ;; Cooperate is strictly dominated by Defect
              (assert-true (strictly-dominates-p1? prisoners-dilemma 1 0))
              (assert-true (strictly-dominates-p2? prisoners-dilemma 1 0))
              (assert-false (strictly-dominates-p1? prisoners-dilemma 0 1))
              (assert-false (strictly-dominates-p2? prisoners-dilemma 0 1)))
            
            (define-test find-dominated-pd
              (let ([dom (find-dominated-strategies prisoners-dilemma)])
                   (assert-equal '(0) (car dom))   ; P1: Cooperate dominated
                   (assert-equal '(0) (cdr dom)))) ; P2: Cooperate dominated
            
            (define-test matching-pennies-no-dominance
              ;; Matching Pennies has no dominated strategies
              (let ([dom (find-dominated-strategies matching-pennies)])
                   (assert-true (null? (car dom)))
                   (assert-true (null? (cdr dom)))))
            
            (define-test battle-of-sexes-no-dominance
              ;; Battle of Sexes has no dominated strategies
              (let ([dom (find-dominated-strategies battle-of-sexes)])
                   (assert-true (null? (car dom)))
                   (assert-true (null? (cdr dom))))))

;;; ============================================================
;;; IESDS Tests
;;; ============================================================

(test-group iesds
            (define-test iesds-prisoners-dilemma
              ;; IESDS should reduce PD to just (Defect, Defect)
              (let ([reduced (iesds prisoners-dilemma 10)])
                   (assert-equal 1 (game-num-strategies reduced 0))
                   (assert-equal 1 (game-num-strategies reduced 1))))
            
            (define-test iesds-matching-pennies
              ;; IESDS should not eliminate anything in MP
              (let ([reduced (iesds matching-pennies 10)])
                   (assert-equal 2 (game-num-strategies reduced 0))
                   (assert-equal 2 (game-num-strategies reduced 1)))))

;;; ============================================================
;;; Pure Nash Equilibrium Tests
;;; ============================================================

(test-group pure-nash
            (define-test is-pure-nash-prisoners-dilemma
              ;; (Defect, Defect) is Nash
              (assert-true (is-pure-nash? prisoners-dilemma 1 1))
              ;; Other profiles are not Nash
              (assert-false (is-pure-nash? prisoners-dilemma 0 0))
              (assert-false (is-pure-nash? prisoners-dilemma 0 1))
              (assert-false (is-pure-nash? prisoners-dilemma 1 0)))
            
            (define-test find-nash-prisoners-dilemma
              ;; PD has unique Nash: (Defect, Defect)
              (let ([nash (find-pure-nash prisoners-dilemma)])
                   (assert-equal 1 (length nash))
                   (assert-equal '(1 . 1) (car nash))))
            
            (define-test find-nash-battle-of-sexes
              ;; BoS has two pure Nash: (Opera, Opera) and (Football, Football)
              (let ([nash (find-pure-nash battle-of-sexes)])
                   (assert-equal 2 (length nash))
                   (assert-true (and (member '(0 . 0) nash) #t))
                   (assert-true (and (member '(1 . 1) nash) #t))))
            
            (define-test find-nash-matching-pennies
              ;; Matching Pennies has no pure Nash
              (let ([nash (find-pure-nash matching-pennies)])
                   (assert-equal 0 (length nash))))
            
            (define-test find-nash-stag-hunt
              ;; Stag Hunt has two pure Nash: (Stag, Stag) and (Hare, Hare)
              (let ([nash (find-pure-nash stag-hunt)])
                   (assert-equal 2 (length nash))
                   (assert-true (and (member '(0 . 0) nash) #t))
                   (assert-true (and (member '(1 . 1) nash) #t)))))

;;; ============================================================
;;; Mixed Strategy Tests
;;; ============================================================

(test-group mixed-strategies
            (define-test pure-strategy-creation
              (let ([s (pure-strategy 0 3)])
                   (assert-equal 1 (vector-ref s 0))
                   (assert-equal 0 (vector-ref s 1))
                   (assert-equal 0 (vector-ref s 2))))
            
            (define-test uniform-strategy-creation
              (let ([s (uniform-strategy 4)])
                   (assert-equal 1/4 (vector-ref s 0))
                   (assert-equal 1/4 (vector-ref s 1))
                   (assert-equal 1/4 (vector-ref s 2))
                   (assert-equal 1/4 (vector-ref s 3))))
            
            (define-test expected-payoff-pure
              ;; Expected payoff of pure strategies should equal cell payoff
              (let ([s1 (pure-strategy 0 2)]
                    [s2 (pure-strategy 1 2)])
                   (assert-equal 0 (expected-payoff-p1 prisoners-dilemma s1 s2))
                   (assert-equal 5 (expected-payoff-p2 prisoners-dilemma s1 s2))))
            
            (define-test expected-payoff-uniform
              ;; Uniform mix in PD: average of all cells
              (let ([s (uniform-strategy 2)])
                   ;; P1 payoffs: 3, 0, 5, 1 -> avg = 9/4
                   (assert-equal 9/4 (expected-payoff-p1 prisoners-dilemma s s)))))

;;; ============================================================
;;; 2x2 Nash Equilibrium Solver Tests
;;; ============================================================

(test-group nash-2x2
            (define-test solve-prisoners-dilemma
              ;; PD: unique pure Nash at (D,D)
              (let ([solutions (solve-2x2-nash prisoners-dilemma)])
                   (assert-equal 1 (length solutions))))
            
            (define-test solve-matching-pennies
              ;; MP: mixed Nash at (1/2, 1/2) for both
              (let ([solutions (solve-2x2-nash matching-pennies)])
                   (assert-equal 1 (length solutions))
                   (let* ([sol (car solutions)]
                          [sigma1 (car sol)]
                          [sigma2 (cdr sol)])
                         (assert-equal 1/2 (vector-ref sigma1 0))
                         (assert-equal 1/2 (vector-ref sigma2 0)))))
            
            (define-test solve-battle-of-sexes
              ;; BoS: 2 pure Nash + 1 mixed Nash = 3 total
              (let ([solutions (solve-2x2-nash battle-of-sexes)])
                   (assert-equal 3 (length solutions)))))

;;; ============================================================
;;; Display Tests
;;; ============================================================

(test-group display
            (define-test game->string-basic
              (let ([s (game->string prisoners-dilemma)])
                   (assert-true (string? s))
                   (assert-true (> (string-length s) 0)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All normal form games tests passed.\n")
    (display "\n[FAILURE] Some normal form games tests failed.\n"))
