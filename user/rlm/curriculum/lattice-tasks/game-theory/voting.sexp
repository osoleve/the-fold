;;; voting.sexp — Development tasks for lattice/game-theory/voting.ss
;;;
;;; Module: voting (tier 0, depends on prelude, sort)
;;; 638 lines, ~30 exported functions
;;; Social choice and voting rules: preference profiles, positional rules
;;; (Borda, plurality), Condorcet methods (pairwise margin, Copeland,
;;; minimax), Smith set, Schulze method, manipulation detection.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/game-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement borda-scores
  ;; ──────────────────────────────────────────────
  ;; Generate the Borda score vector for m candidates.

  (task
    (id "game-theory/voting/borda-scores-impl")
    (module voting)
    (tier 0)
    (type implement)
    (description "Implement borda-scores: generate the Borda count score vector for m candidates. In the Borda count, the first-place candidate gets m-1 points, second gets m-2, ..., last gets 0. Return a list of m scores in descending order. For m=3: (2 1 0). For m=0 or negative: return empty list. Use a named let loop building the list from index 0 to m-1, where score at position i is (m - 1 - i), accumulating in reverse and reversing at the end.")
    (context "Available: <=, >=, +, -, cons, reverse, if, let. Named let with index and accumulator.")
    (before "(define (borda-scores m)
  ;; TODO: (m-1, m-2, ..., 1, 0)
  '())")
    (test-file "lattice/game-theory/test-voting.ss")
    (test-forms (";; 3 candidates: (2 1 0)
(assert-equal '(2 1 0) (borda-scores 3))"
                 ";; 4 candidates: (3 2 1 0)
(assert-equal '(3 2 1 0) (borda-scores 4))"
                 ";; 1 candidate: (0)
(assert-equal '(0) (borda-scores 1))"
                 ";; 0 candidates: empty
(assert-equal '() (borda-scores 0))"))
    (available-modules (prelude sort))
    (hints ("Guard: (<= m 0) -> '()"
            "Named let loop: (let loop ([i 0] [acc '()]) ...)"
            "Base: (>= i m) -> (reverse acc)"
            "Score at position i: (- m 1 i)"
            "Step: (loop (+ i 1) (cons (- m 1 i) acc))"))
    (reference-solution "(define (borda-scores m)
  (if (<= m 0)
      '()
      (let loop ([i 0] [acc '()])
        (if (>= i m)
            (reverse acc)
            (loop (+ i 1) (cons (- m 1 i) acc))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement pairwise-margin
  ;; ──────────────────────────────────────────────
  ;; The foundation of Condorcet methods.

  (task
    (id "game-theory/voting/pairwise-margin-impl")
    (module voting)
    (tier 0)
    (type implement)
    (description "Implement pairwise-margin: compute the margin of victory of candidate a over candidate b. Count voters who prefer a over b (a appears before b in their ranking) and subtract voters who prefer b over a. A positive result means a beats b; negative means b beats a; zero is a tie. For each ranking in the profile, find the positions of a and b using position-of, then compare: if a's position < b's position, a is preferred. Use count-if to count each direction, then subtract.")
    (context "Available: count-if, position-of, lambda, let, and, <, -. Two count-if calls over the profile plus subtraction.")
    (before "(define (pairwise-margin a b profile)
  ;; TODO: voters(a>b) - voters(b>a)
  0)")
    (test-file "lattice/game-theory/test-voting.ss")
    (test-forms (";; 2 voters prefer a over b, 1 prefers b over a: margin = 1
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-equal 1 (pairwise-margin 'a 'b profile)))"
                 ";; Antisymmetric: margin(b,a) = -margin(a,b)
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-equal -1 (pairwise-margin 'b 'a profile)))"
                 ";; Condorcet cycle: each beats next by margin 1
(let ([profile (condorcet-cycle-example)])
  (assert-equal 1 (pairwise-margin 'a 'b profile))
  (assert-equal 1 (pairwise-margin 'b 'c profile))
  (assert-equal 1 (pairwise-margin 'c 'a profile)))"
                 ";; Unanimous: margin = number of voters
(let ([profile (make-preference-profile '((a b) (a b) (a b)))])
  (assert-equal 3 (pairwise-margin 'a 'b profile)))"))
    (available-modules (prelude sort))
    (hints ("a-over-b = (count-if (lambda (ranking) ...) profile)"
            "In each ranking: (let ([pa (position-of a ranking)] [pb (position-of b ranking)]) ...)"
            "Check: (and pa pb (< pa pb)) for a preferred over b"
            "Return: (- a-over-b b-over-a)"))
    (reference-solution "(define (pairwise-margin a b profile)
  (let ([a-over-b (count-if (lambda (ranking)
                              (let ([pa (position-of a ranking)]
                                    [pb (position-of b ranking)])
                                (and pa pb (< pa pb))))
                            profile)]
        [b-over-a (count-if (lambda (ranking)
                              (let ([pa (position-of a ranking)]
                                    [pb (position-of b ranking)])
                                (and pa pb (< pb pa))))
                            profile)])
    (- a-over-b b-over-a)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement condorcet-winner?
  ;; ──────────────────────────────────────────────
  ;; Does this candidate beat everyone pairwise?

  (task
    (id "game-theory/voting/condorcet-winner-impl")
    (module voting)
    (tier 0)
    (type implement)
    (description "Implement condorcet-winner?: check if a candidate is the Condorcet winner, meaning they beat every other candidate in pairwise comparison. Get the list of all candidates from the profile, remove the candidate being tested, then check that pairwise-beats? returns true against every remaining candidate. Use for-all? to check the universal condition. A Condorcet winner may not exist (e.g., the Condorcet cycle A>B>C>A), but if one exists it is unique.")
    (context "Available: profile-candidates, remove, pairwise-beats?, for-all?, lambda, let. One remove plus one for-all? call.")
    (before "(define (condorcet-winner? candidate profile)
  ;; TODO: does candidate beat all others pairwise?
  #f)")
    (test-file "lattice/game-theory/test-voting.ss")
    (test-forms (";; a is Condorcet winner when 2/3 prefer a first
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-true (condorcet-winner? 'a profile)))"
                 ";; b is NOT Condorcet winner
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-false (condorcet-winner? 'b profile)))"
                 ";; No Condorcet winner in a cycle
(let ([profile (condorcet-cycle-example)])
  (assert-false (condorcet-winner? 'a profile))
  (assert-false (condorcet-winner? 'b profile))
  (assert-false (condorcet-winner? 'c profile)))"
                 ";; Unanimous profile: first choice is Condorcet winner
(let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
  (assert-true (condorcet-winner? 'a profile)))"))
    (available-modules (prelude sort))
    (hints ("others = (remove candidate (profile-candidates profile))"
            "Check: (for-all? (lambda (other) (pairwise-beats? candidate other profile)) others)"))
    (reference-solution "(define (condorcet-winner? candidate profile)
  (let ([others (remove candidate (profile-candidates profile))])
    (for-all? (lambda (other) (pairwise-beats? candidate other profile))
              others)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement copeland-score
  ;; ──────────────────────────────────────────────
  ;; Wins minus losses in pairwise comparisons.

  (task
    (id "game-theory/voting/copeland-score-impl")
    (module voting)
    (tier 0)
    (type implement)
    (description "Implement copeland-score: compute the Copeland score for a candidate. The Copeland score counts pairwise wins minus pairwise losses. For each opponent: if the candidate beats them (positive margin), score +1; if the candidate loses (negative margin), score -1; if tied (zero margin), score 0. Sum these across all opponents. A Condorcet winner always has the maximum Copeland score (equal to n-1 where n is the number of candidates). Use remove to get opponents, map each to +1/-1/0 via pairwise-margin, and sum with apply +.")
    (context "Available: profile-candidates, remove, pairwise-margin, apply, +, map, lambda, cond, >, <, let. One remove, one map with cond, one apply +.")
    (before "(define (copeland-score candidate profile)
  ;; TODO: wins - losses in pairwise comparisons
  0)")
    (test-file "lattice/game-theory/test-voting.ss")
    (test-forms (";; Condorcet winner has max Copeland score
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-equal 2 (copeland-score 'a profile)))"
                 ";; Copeland scores in a cycle: all equal
(let ([profile (condorcet-cycle-example)])
  (assert-equal 0 (copeland-score 'a profile))
  (assert-equal 0 (copeland-score 'b profile))
  (assert-equal 0 (copeland-score 'c profile)))"
                 ";; Condorcet loser has minimum score
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))])
  (assert-equal -2 (copeland-score 'c profile)))"))
    (available-modules (prelude sort))
    (hints ("others = (remove candidate (profile-candidates profile))"
            "Map each opponent to score: (lambda (other) (let ([margin ...]) (cond [(> margin 0) 1] [(< margin 0) -1] [else 0])))"
            "Sum: (apply + scores)"))
    (reference-solution "(define (copeland-score candidate profile)
  (let ([others (remove candidate (profile-candidates profile))])
    (apply + (map (lambda (other)
                    (let ([margin (pairwise-margin candidate other profile)])
                      (cond [(> margin 0) 1]
                            [(< margin 0) -1]
                            [else 0])))
                  others))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement positional-score
  ;; ──────────────────────────────────────────────
  ;; Total score for a candidate under any positional rule.

  (task
    (id "game-theory/voting/positional-score-impl")
    (module voting)
    (tier 0)
    (type implement)
    (description "Implement positional-score: compute the total score for a candidate under a given positional scoring rule. A positional rule assigns points based on position in each ranking: scores[0] for first place, scores[1] for second, etc. For each ranking in the profile, find the candidate's position using position-of. If found, look up the score at that position via list-ref; if not found (shouldn't happen with valid profiles), contribute 0. Sum all contributions across all rankings using apply + and map.")
    (context "Available: apply, +, map, lambda, position-of, list-ref, if, let. One map over the profile with position lookup.")
    (before "(define (positional-score candidate profile scores)
  ;; TODO: sum of score-at-position across all rankings
  0)")
    (test-file "lattice/game-theory/test-voting.ss")
    (test-forms (";; Borda scores for 3-candidate profile
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))]
      [scores (borda-scores 3)])
  ;; a: 2+2+0=4, b: 1+1+2=4, c: 0+0+1=1
  (assert-equal 4 (positional-score 'a profile scores))
  (assert-equal 4 (positional-score 'b profile scores))
  (assert-equal 1 (positional-score 'c profile scores)))"
                 ";; Plurality: only first-place counts
(let ([profile (make-preference-profile '((a b c) (a b c) (b c a)))]
      [scores (plurality-scores 3)])
  ;; a: 1+1+0=2, b: 0+0+1=1, c: 0+0+0=0
  (assert-equal 2 (positional-score 'a profile scores))
  (assert-equal 1 (positional-score 'b profile scores))
  (assert-equal 0 (positional-score 'c profile scores)))"
                 ";; Unanimous: first-place candidate gets max score
(let ([profile (make-preference-profile '((a b) (a b) (a b)))]
      [scores (borda-scores 2)])
  (assert-equal 3 (positional-score 'a profile scores))
  (assert-equal 0 (positional-score 'b profile scores)))"))
    (available-modules (prelude sort))
    (hints ("Map over profile: each ranking contributes one score"
            "(lambda (ranking) (let ([pos (position-of candidate ranking)]) (if pos (list-ref scores pos) 0)))"
            "Sum: (apply + (map ... profile))"))
    (reference-solution "(define (positional-score candidate profile scores)
  (apply + (map (lambda (ranking)
                  (let ([pos (position-of candidate ranking)])
                    (if pos (list-ref scores pos) 0)))
                profile)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
