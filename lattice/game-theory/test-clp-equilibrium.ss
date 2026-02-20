(load "core/testing/test-framework.ss")
(load "lattice/game-theory/clp-equilibrium.ss")

;;; Helper: check if a pair is in a list of pairs
(define (pair-in? p lst)
  (pair? (filter (lambda (x) (and (= (car x) (car p)) (= (cdr x) (cdr p)))) lst)))

;;; ====
;;; Pure Nash via CLP
;;; ====

(test-group "clp-pure-nash"

  (define-test "prisoner's dilemma has one Nash: (defect, defect)"
    (let ([nash (clp-find-pure-nash prisoners-dilemma)])
      (assert-equal 1 (length nash))
      (assert-true (pair-in? '(1 . 1) nash))))

  (define-test "battle of sexes has two Nash: (opera,opera) and (football,football)"
    (let ([nash (clp-find-pure-nash battle-of-sexes)])
      (assert-equal 2 (length nash))
      (assert-true (pair-in? '(0 . 0) nash))
      (assert-true (pair-in? '(1 . 1) nash))))

  (define-test "matching pennies has no pure Nash"
    (let ([nash (clp-find-pure-nash matching-pennies)])
      (assert-equal 0 (length nash))))

  (define-test "stag hunt has two Nash: (stag,stag) and (hare,hare)"
    (let ([nash (clp-find-pure-nash stag-hunt)])
      (assert-equal 2 (length nash))
      (assert-true (pair-in? '(0 . 0) nash))
      (assert-true (pair-in? '(1 . 1) nash))))

  (define-test "chicken has two Nash: (dare,swerve) and (swerve,dare)"
    (let ([nash (clp-find-pure-nash chicken)])
      (assert-equal 2 (length nash))
      (assert-true (pair-in? '(0 . 1) nash))
      (assert-true (pair-in? '(1 . 0) nash)))))

;;; ====
;;; CLP matches brute-force find-pure-nash
;;; ====

(test-group "clp-matches-brute-force"

  (define-test "PD: CLP matches find-pure-nash"
    (let ([clp-nash (clp-find-pure-nash prisoners-dilemma)]
          [bf-nash (find-pure-nash prisoners-dilemma)])
      (assert-equal (length bf-nash) (length clp-nash))))

  (define-test "BoS: CLP matches find-pure-nash"
    (let ([clp-nash (clp-find-pure-nash battle-of-sexes)]
          [bf-nash (find-pure-nash battle-of-sexes)])
      (assert-equal (length bf-nash) (length clp-nash))))

  (define-test "chicken: CLP matches find-pure-nash"
    (let ([clp-nash (clp-find-pure-nash chicken)]
          [bf-nash (find-pure-nash chicken)])
      (assert-equal (length bf-nash) (length clp-nash)))))

;;; ====
;;; Dominance Pruning
;;; ====

(test-group "clp-dominance"

  (define-test "PD: cooperate dominated for both players"
    (let ([p1 (make-lvar 'p1)]
          [p2 (make-lvar 'p2)])
      (let* ([cs (make-cstore)]
             [goal (clp-conj*
                    (list (goal-in-range p1 0 1)
                          (goal-in-range p2 0 1)
                          (clp-dominance-prune prisoners-dilemma p1 p2)
                          (goal-label (list p1 p2))))]
             [solutions (run-clp 10 (lambda () goal) (list p1 p2))])
        ;; After dominance pruning, only (1,1) remains
        (assert-equal 1 (length solutions))
        (assert-equal '(1 1) (car solutions)))))

  (define-test "BoS: no dominated strategies"
    (let ([dominated (find-dominated-strategies battle-of-sexes)])
      ;; Neither player has dominated strategies
      (assert-true (null? (car dominated)))
      (assert-true (null? (cdr dominated))))))

;;; ====
;;; Constrained Nash
;;; ====

(test-group "clp-constrained-nash"

  (define-test "BoS: force P1 to opera → only (opera, opera) Nash"
    (let ([nash (clp-constrained-nash battle-of-sexes
                 (lambda (p1 p2)
                   (goal-=fd p1 0)))])
      (assert-equal 1 (length nash))
      (assert-true (pair-in? '(0 . 0) nash))))

  (define-test "BoS: force P1 to football → only (football, football) Nash"
    (let ([nash (clp-constrained-nash battle-of-sexes
                 (lambda (p1 p2)
                   (goal-=fd p1 1)))])
      (assert-equal 1 (length nash))
      (assert-true (pair-in? '(1 . 1) nash))))

  (define-test "chicken: force P1 to dare → only (dare, swerve) Nash"
    (let ([nash (clp-constrained-nash chicken
                 (lambda (p1 p2)
                   (goal-=fd p1 0)))])
      (assert-equal 1 (length nash))
      (assert-true (pair-in? '(0 . 1) nash))))

  (define-test "matching pennies: constraint doesn't create false Nash"
    (let ([nash (clp-constrained-nash matching-pennies
                 (lambda (p1 p2)
                   (goal-=fd p1 0)))])
      (assert-equal 0 (length nash)))))

;;; ====
;;; Name Conversion
;;; ====

(test-group "nash-naming"

  (define-test "PD: (1,1) → (defect, defect)"
    (let ([names (nash-solution->names prisoners-dilemma '(1 . 1))])
      (assert-equal 'defect (car names))
      (assert-equal 'defect (cdr names))))

  (define-test "BoS: (0,0) → (opera, opera)"
    (let ([names (nash-solution->names battle-of-sexes '(0 . 0))])
      (assert-equal 'opera (car names))
      (assert-equal 'opera (cdr names))))

  (define-test "batch naming"
    (let ([names (nash-solutions->names chicken
                   '((0 . 1) (1 . 0)))])
      (assert-equal 2 (length names))
      (assert-equal 'dare (car (car names)))
      (assert-equal 'swerve (cdr (car names)))
      (assert-equal 'swerve (car (cadr names)))
      (assert-equal 'dare (cdr (cadr names))))))

;;; ====
;;; Game Setup
;;; ====

(test-group "game-clp-setup"

  (define-test "game->clp-nash returns goal and 2 vars"
    (call-with-values
     (lambda () (game->clp-nash prisoners-dilemma))
     (lambda (goal vars)
       (assert-equal 2 (length vars))
       (assert-true (procedure? goal)))))

  (define-test "custom 3x3 game with unique Nash"
    ;; P1 best response to j=0: i=1 (payoff 5)
    ;; P1 best response to j=1: i=0 (payoff 4)
    ;; P1 best response to j=2: i=1 (payoff 3)
    ;; P2 best response to i=0: j=0 (payoff 3)
    ;; P2 best response to i=1: j=0 (payoff 6)
    ;; P2 best response to i=2: j=1 (payoff 2)
    ;; Nash: (1,0) — only mutual best response
    (let ([g (make-game
              '(a b c)
              '(x y z)
              '(((2 . 3) (4 . 1) (1 . 0))
                ((5 . 6) (0 . 0) (3 . 1))
                ((1 . 1) (0 . 2) (2 . 0))))])
      (let ([nash (clp-find-pure-nash g)])
        (assert-equal 1 (length nash))
        (assert-true (pair-in? '(1 . 0) nash))))))

;;; ====
;;; Dominance + Nash Combined
;;; ====

(test-group "dominance-plus-nash"

  (define-test "dominance pruning + Nash finds same result"
    ;; PD with dominance pruning should still find (1,1)
    (let ([p1 (make-lvar 'p1)]
          [p2 (make-lvar 'p2)])
      (let* ([goal (clp-conj*
                    (list (goal-in-range p1 0 1)
                          (goal-in-range p2 0 1)
                          (clp-dominance-prune prisoners-dilemma p1 p2)
                          (clp-mutual-best-response prisoners-dilemma p1 p2)
                          (goal-label (list p1 p2))))]
             [solutions (run-clp 10 (lambda () goal) (list p1 p2))])
        (assert-equal 1 (length solutions))
        (assert-equal '(1 1) (car solutions))))))

(run-all-tests)
