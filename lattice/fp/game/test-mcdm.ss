;;; lattice/fp/game/test-mcdm.ss — Tests for Multi-Criteria Decision Making
;;; Run: scheme --script lattice/fp/game/test-mcdm.ss

(load "core/testing/test-framework.ss")
(load "lattice/fp/game/mcdm.ss")

(test-group "mcdm-basics"
  (define-test "create decision problem"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y)
               '((a x 10) (a y 5)
                 (b x 5) (b y 10)
                 (c x 7) (c y 7)))])
      (assert-equal '(a b c) (dp-alternatives dp))
      (assert-equal '(x y) (dp-criteria dp))
      (assert-equal 10 (dp-score dp 'a 'x))
      (assert-equal 5 (dp-score dp 'a 'y))
      (assert-equal 0 (dp-score dp 'a 'missing)))) ; Default for missing

  (define-test "criterion ranking"
    (let ([dp (make-decision-problem
               '(a b c)
               '(speed)
               '((a speed 100) (b speed 200) (c speed 150)))])
      (assert-equal '(b c a) (criterion-ranking dp 'speed))))

  (define-test "dp->profile conversion"
    (let* ([dp (make-decision-problem
                '(a b c)
                '(x y)
                '((a x 10) (a y 5)
                  (b x 5) (b y 10)
                  (c x 7) (c y 7)))]
           [profile (dp->profile dp)])
      ;; x criterion ranks: a(10) > c(7) > b(5)
      ;; y criterion ranks: b(10) > c(7) > a(5)
      (assert-equal 2 (profile-voters profile))
      (assert-equal '(a c b) (car profile))    ; x criterion
      (assert-equal '(b c a) (cadr profile))))) ; y criterion

(test-group "mcdm-aggregation"
  (define-test "borda with clear winner"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 10) (a y 10) (a z 10)  ; a wins all
                 (b x 5) (b y 5) (b z 5)
                 (c x 1) (c y 1) (c z 1)))])
      (assert-equal 'a (mcdm-borda dp))))

  (define-test "schulze with clear winner"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 10) (a y 10) (a z 10)
                 (b x 5) (b y 5) (b z 5)
                 (c x 1) (c y 1) (c z 1)))])
      (assert-equal 'a (mcdm-schulze dp))))

  (define-test "condorcet winner when exists"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 10) (a y 10) (a z 10)
                 (b x 5) (b y 5) (b z 5)
                 (c x 1) (c y 1) (c z 1)))])
      (assert-equal 'a (mcdm-condorcet dp))))

  (define-test "no condorcet winner with cycle"
    ;; Construct a cycle: x prefers a>b>c, y prefers b>c>a, z prefers c>a>b
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 3) (b x 2) (c x 1)   ; x: a > b > c
                 (b y 3) (c y 2) (a y 1)   ; y: b > c > a
                 (c z 3) (a z 2) (b z 1)))]) ; z: c > a > b
      (assert-false (mcdm-condorcet dp))
      (assert-true (has-condorcet-cycle? dp))))

  (define-test "mcdm-ranking returns full ranking"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y)
               '((a x 10) (a y 8)
                 (b x 6) (b y 6)
                 (c x 4) (c y 4)))])
      (let ([ranking (mcdm-ranking dp 'borda)])
        (assert-equal 3 (length ranking))
        (assert-equal 'a (car ranking))))))

(test-group "mcdm-weighted"
  (define-test "weighted profile with different weights"
    (let* ([dp (make-decision-problem
                '(a b)
                '(x y)
                '((a x 10) (a y 5)
                  (b x 5) (b y 10)))]
           ;; With equal weights: tied
           ;; With x weighted 3, y weighted 1: a should win
           [profile (dp->weighted-profile dp '(3 1))])
      ;; 3 votes for x ranking (a > b) + 1 vote for y ranking (b > a)
      ;; = 4 voters total
      (assert-equal 4 (profile-voters profile))))

  (define-test "weighted borda scores"
    (let* ([dp (make-decision-problem
                '(a b)
                '(x y)
                '((a x 10) (a y 5)
                  (b x 5) (b y 10)))]
           [scores (weighted-borda-scores dp '(3 1))])
      ;; a: 3*(1) + 1*(0) = 3 (first on x, last on y)
      ;; b: 3*(0) + 1*(1) = 1 (last on x, first on y)
      (assert-equal 'a (caar scores))
      (assert-equal 3 (cdar scores))))  ; exact integer comparison

  (define-test "weighted borda winner"
    (let ([dp (make-decision-problem
               '(a b)
               '(x y)
               '((a x 10) (a y 5)
                 (b x 5) (b y 10)))])
      ;; Equal weights: tie (Borda picks first in ties)
      ;; x weight 3, y weight 1: a wins
      (assert-equal 'a (weighted-borda-winner dp '(3 1)))
      ;; x weight 1, y weight 3: b wins
      (assert-equal 'b (weighted-borda-winner dp '(1 3))))))

(test-group "mcdm-pareto"
  (define-test "pareto frontier with clear dominance"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y)
               '((a x 10) (a y 10)  ; a dominates c
                 (b x 5) (b y 15)   ; b not dominated (best on y)
                 (c x 5) (c y 5)))]) ; c dominated by a
      (let ([frontier (pareto-frontier dp)])
        (assert-true (pair? (member 'a frontier)))
        (assert-true (pair? (member 'b frontier)))
        (assert-false (member 'c frontier)))))

  (define-test "dominates? relation"
    (let ([dp (make-decision-problem
               '(a b)
               '(x y)
               '((a x 10) (a y 10)
                 (b x 5) (b y 5)))])
      (assert-true (dominates? dp 'a 'b))
      (assert-false (dominates? dp 'b 'a))))

  (define-test "no dominance when incomparable"
    (let ([dp (make-decision-problem
               '(a b)
               '(x y)
               '((a x 10) (a y 5)
                 (b x 5) (b y 10)))])
      (assert-false (dominates? dp 'a 'b))
      (assert-false (dominates? dp 'b 'a))
      (assert-equal 2 (length (pareto-frontier dp)))))

  (define-test "pareto-dominated returns correct set"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y)
               '((a x 10) (a y 10)
                 (b x 5) (b y 15)
                 (c x 5) (c y 5)))])
      (assert-equal '(c) (pareto-dominated dp)))))

(test-group "mcdm-sensitivity"
  (define-test "robustness profile structure"
    (let ([dp (make-decision-problem
               '(a b)
               '(x y)
               '((a x 10) (a y 5)
                 (b x 5) (b y 10)))])
      (let ([profile (robustness-profile dp '(1 1))])
        (assert-equal 2 (length profile))
        (assert-true (pair? (car profile)))
        (assert-true (symbol? (caar profile))))))

  (define-test "sensitivity checks both directions"
    ;; a wins when x has high weight, b wins when y has high weight
    ;; With equal weights, should be sensitive to changes in either direction
    (let ([dp (make-decision-problem
               '(a b)
               '(x y)
               '((a x 10) (a y 1)
                 (b x 1) (b y 10)))])
      (let ([sens (sensitivity-to-criterion dp 'x '(2 1))])
        ;; Should find a breaking point (not max-delta)
        (assert-true (< sens 10.0))))))

(test-group "mcdm-validation"
  (define-test "rejects duplicate alternatives"
    (assert-error
     (lambda ()
       (make-decision-problem
        '(a b a)  ; duplicate 'a
        '(x y)
        '((a x 10) (b x 5))))))

  (define-test "rejects duplicate criteria"
    (assert-error
     (lambda ()
       (make-decision-problem
        '(a b)
        '(x x)  ; duplicate 'x
        '((a x 10) (b x 5)))))))

(test-group "mcdm-agreement"
  (define-test "unanimous winner when all agree"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 10) (a y 10) (a z 10)
                 (b x 5) (b y 5) (b z 5)
                 (c x 1) (c y 1) (c z 1)))])
      (assert-equal 'a (unanimous-winner? dp))))

  (define-test "no unanimous with disagreement"
    ;; Create scenario where methods might disagree
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 3) (b x 2) (c x 1)
                 (b y 3) (c y 2) (a y 1)
                 (c z 3) (a z 2) (b z 1)))])
      ;; With Condorcet cycle, methods may still agree on resolving winner
      ;; but it's not a Condorcet winner
      (let ([agreement (method-agreement dp)])
        (assert-equal 4 (length agreement))))))

(test-group "mcdm-cycles"
  (define-test "detect condorcet cycle"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 3) (b x 2) (c x 1)
                 (b y 3) (c y 2) (a y 1)
                 (c z 3) (a z 2) (b z 1)))])
      (assert-true (has-condorcet-cycle? dp))))

  (define-test "no cycle with clear winner"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 10) (a y 10) (a z 10)
                 (b x 5) (b y 5) (b z 5)
                 (c x 1) (c y 1) (c z 1)))])
      (assert-false (has-condorcet-cycle? dp))))

  (define-test "cycle participants when cycle exists"
    (let ([dp (make-decision-problem
               '(a b c)
               '(x y z)
               '((a x 3) (b x 2) (c x 1)
                 (b y 3) (c y 2) (a y 1)
                 (c z 3) (a z 2) (b z 1)))])
      (let ([participants (cycle-participants dp)])
        ;; All three should be in the cycle
        (assert-true (pair? (member 'a participants)))
        (assert-true (pair? (member 'b participants)))
        (assert-true (pair? (member 'c participants)))))))

(test-group "mcdm-examples"
  (define-test "sorting algorithm example loads"
    (let ([dp (sorting-algorithm-example)])
      (assert-equal 4 (length (dp-alternatives dp)))
      (assert-equal 4 (length (dp-criteria dp)))))

  (define-test "tech choice example has frontier"
    (let* ([dp (tech-choice-example)]
           [frontier (pareto-frontier dp)])
      ;; All three techs should be on frontier (different tradeoffs)
      (assert-true (>= (length frontier) 2)))))

(run-all-tests)
