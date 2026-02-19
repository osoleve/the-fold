;;; strategic-voting.sexp — Development tasks for lattice/game-theory/strategic-voting.ss
;;;
;;; Module: strategic-voting (tier 1, depends on prelude, voting)
;;; 419 lines, ~15 exported functions
;;; Strategic voting equilibrium analysis: voter utility, social welfare,
;;; best response dynamics, Nash equilibrium detection, price of anarchy,
;;; strategy-proofness analysis, Gibbard-Satterthwaite demonstrations.
;;;
;;; Git history:
;;;   e68b9849 feat(module): migrate game-theory to require
;;;   c6dffccb refactor(lattice): reorganize skills, consolidate graph modules

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement voter-utility
  ;; ──────────────────────────────────────────────
  ;; How much a voter values a particular winner.

  (task
    (id "game-theory/strategic-voting/voter-utility-impl")
    (module strategic-voting)
    (tier 0)
    (type implement)
    (description "Implement voter-utility: compute how much a voter values a winner based on their true preference ranking. Uses positional scoring: if the winner is at position p in a ranking of n candidates, the utility is n-1-p. First choice gives maximum utility (n-1), last gives 0. If the winner isn't in the ranking at all, return 0. Use position-of (from the voting module) to find the candidate's position in the ranking.")
    (context "Available: position-of, length, -, if, let. One lookup + one subtraction.")
    (before "(define (voter-utility winner true-ranking)
  ;; TODO: positional utility for winner
  0)")
    (test-file "lattice/game-theory/test-strategic-voting.ss")
    (test-forms (";; First choice gives maximum utility
(assert-equal 2 (voter-utility 'a '(a b c)))"
                 ";; Second choice
(assert-equal 1 (voter-utility 'b '(a b c)))"
                 ";; Last choice gives 0
(assert-equal 0 (voter-utility 'c '(a b c)))"
                 ";; Not in ranking gives 0
(assert-equal 0 (voter-utility 'd '(a b c)))"))
    (available-modules (prelude voting))
    (hints ("Find position: (let ([pos (position-of winner true-ranking)]) ...)"
            "If pos is #f (not found), return 0"
            "Otherwise: (- (length true-ranking) 1 pos)"
            "position-of returns 0-indexed position or #f"))
    (reference-solution "(define (voter-utility winner true-ranking)
  (let ([pos (position-of winner true-ranking)])
    (if pos
        (- (length true-ranking) 1 pos)
        0)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement social-welfare
  ;; ──────────────────────────────────────────────
  ;; Sum of all voters' utilities for a winner.

  (task
    (id "game-theory/strategic-voting/social-welfare-impl")
    (module strategic-voting)
    (tier 0)
    (type implement)
    (description "Implement social-welfare: compute the utilitarian social welfare of a winner — the sum of all voters' utilities. A preference profile is a list of rankings (one per voter). For each voter's ranking, compute voter-utility of the winner, then sum across all voters using apply + and map. This measures the total satisfaction of society with the election outcome.")
    (context "Available: apply, +, map, lambda, voter-utility. Single expression with map + apply.")
    (before "(define (social-welfare winner true-profile)
  ;; TODO: sum of utilities across all voters
  0)")
    (test-file "lattice/game-theory/test-strategic-voting.ss")
    (test-forms (";; Unanimous first choice: each voter gets 2, total = 6
(let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
  (assert-equal 6 (social-welfare 'a profile)))"
                 ";; Unanimous last choice: total = 0
(let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
  (assert-equal 0 (social-welfare 'c profile)))"
                 ";; Mixed preferences
(let ([profile (make-preference-profile '((a b c) (b a c) (c a b)))])
  (assert-equal 4 (social-welfare 'a profile)))"))
    (available-modules (prelude voting))
    (hints ("Map voter-utility over each ranking in the profile"
            "(map (lambda (ranking) (voter-utility winner ranking)) true-profile)"
            "Sum: (apply + (map ...))"))
    (reference-solution "(define (social-welfare winner true-profile)
  (apply + (map (lambda (ranking) (voter-utility winner ranking))
                true-profile)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement cartesian-product-n
  ;; ──────────────────────────────────────────────
  ;; Generate all n-tuples from a list.

  (task
    (id "game-theory/strategic-voting/cartesian-product-n-impl")
    (module strategic-voting)
    (tier 0)
    (type implement)
    (description "Implement cartesian-product-n: generate all n-tuples drawn from a list of items. For n=0, return a list containing the empty list '(()). For n>0, recursively generate all (n-1)-tuples, then for each such tuple, prepend each possible item. Uses append-map to flatten the result: for each rest-tuple in the recursive result, map over items creating (cons item rest). This is exponential (|items|^n results) but essential for exhaustive equilibrium search.")
    (context "Available: =, -, cons, map, lambda, append-map, if. Recursive with append-map.")
    (before "(define (cartesian-product-n items n)
  ;; TODO: all n-tuples from items
  '(()))")
    (test-file "lattice/game-theory/test-strategic-voting.ss")
    (test-forms (";; 0-tuples: just the empty tuple
(assert-equal '(()) (cartesian-product-n '(a b) 0))"
                 ";; 1-tuples: each item wrapped
(assert-equal 2 (length (cartesian-product-n '(a b) 1)))"
                 ";; 2-tuples from 2 items: 4 results
(assert-equal 4 (length (cartesian-product-n '(x y) 2)))"
                 ";; 2-tuples from 3 items: 9 results
(assert-equal 9 (length (cartesian-product-n '(a b c) 2)))"))
    (available-modules (prelude voting))
    (hints ("Base case: n=0 -> '(())"
            "Recursive: get all (n-1)-tuples first"
            "For each rest-tuple, prepend each item: (map (lambda (item) (cons item rest)) items)"
            "Flatten with append-map: (append-map (lambda (rest) (map ...)) (cartesian-product-n items (- n 1)))"))
    (reference-solution "(define (cartesian-product-n items n)
  (if (= n 0)
      '(())
      (append-map (lambda (rest)
                    (map (lambda (item) (cons item rest))
                         items))
                  (cartesian-product-n items (- n 1)))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement is-strategic-equilibrium?
  ;; ──────────────────────────────────────────────
  ;; Check if a profile is a Nash equilibrium.

  (task
    (id "game-theory/strategic-voting/is-strategic-equilibrium-impl")
    (module strategic-voting)
    (tier 1)
    (type implement)
    (description "Implement is-strategic-equilibrium?: check whether a reported preference profile is a Nash equilibrium given voters' true preferences. A profile is a Nash equilibrium if no voter can benefit by unilaterally changing their ballot. For each voter i (0 to n-1), check if best-response-improves? returns #t — if it does, voter i can profitably deviate, so the profile is NOT an equilibrium. Use a named let loop over voter indices. Return #t only if all voters are checked without finding an improvement.")
    (context "Available: length, list-ref, >=, +, best-response-improves?, if, let. Named let loop with early exit.")
    (before "(define (is-strategic-equilibrium? reported-profile true-profile voting-rule)
  ;; TODO: no voter can benefit by deviation?
  #f)")
    (test-file "lattice/game-theory/test-strategic-voting.ss")
    (test-forms (";; Unanimous profile is equilibrium
(let ([profile (make-preference-profile '((a b c) (a b c) (a b c)))])
  (assert-true (is-strategic-equilibrium? profile profile plurality-winner)))"
                 ";; Manipulable profile is NOT equilibrium
(let ([profile (strategic-example-1)])
  (assert-false (is-strategic-equilibrium? profile profile plurality-winner)))"))
    (available-modules (prelude voting))
    (hints ("Get number of voters: (let ([n (length reported-profile)]) ...)"
            "Named let: (let loop ([i 0]) ...)"
            "If i >= n, all checked -> #t"
            "Check voter i: (best-response-improves? i reported-profile voting-rule (list-ref true-profile i))"
            "If improves -> #f (not equilibrium); else -> (loop (+ i 1))"))
    (reference-solution "(define (is-strategic-equilibrium? reported-profile true-profile voting-rule)
  (let ([n (length reported-profile)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (if (best-response-improves? i reported-profile voting-rule (list-ref true-profile i))
              #f
              (loop (+ i 1)))))))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement best-response-improves?
  ;; ──────────────────────────────────────────────
  ;; Can a voter improve by switching ballots?

  (task
    (id "game-theory/strategic-voting/best-response-improves-impl")
    (module strategic-voting)
    (tier 1)
    (type implement)
    (description "Implement best-response-improves?: check whether voter at index voter-idx can improve their utility by switching from their current ballot. Compute the current winner under voting-rule, then compute the voter's utility for that winner. Find the voter's best responses (using find-best-responses), pick the first one, compute the winner under that ballot, and check if the resulting utility exceeds the current utility. This detects whether a voter has an incentive to misreport their preferences.")
    (context "Available: voting-rule, voter-utility, find-best-responses, list-set, car, let*, >. Pipeline: current-winner -> current-utility -> best-responses -> best-utility -> compare.")
    (before "(define (best-response-improves? voter-idx current-profile voting-rule true-ranking)
  ;; TODO: can voter improve by switching?
  #f)")
    (test-file "lattice/game-theory/test-strategic-voting.ss")
    (test-forms (";; No improvement when first choice wins
(let* ([profile (make-preference-profile '((a b c) (a b c) (a b c)))]
       [true-ranking (car profile)])
  (assert-false (best-response-improves? 0 profile plurality-winner true-ranking)))"
                 ";; Can improve in strategic-example-1
(let* ([profile (strategic-example-1)]
       [true-ranking (list-ref profile 0)])
  (assert-true (best-response-improves? 0 profile plurality-winner true-ranking)))"))
    (available-modules (prelude voting))
    (hints ("Step 1: (let* ([current-winner (voting-rule current-profile)]) ...)"
            "Step 2: (let* ([current-utility (voter-utility current-winner true-ranking)]) ...)"
            "Step 3: (let* ([best-responses (find-best-responses voter-idx current-profile voting-rule true-ranking)]) ...)"
            "Step 4: Create new profile with best response: (list-set current-profile voter-idx (car best-responses))"
            "Step 5: Get best utility: (voter-utility (voting-rule new-profile) true-ranking)"
            "Return: (> best-utility current-utility)"))
    (reference-solution "(define (best-response-improves? voter-idx current-profile voting-rule true-ranking)
  (let* ([current-winner (voting-rule current-profile)]
         [current-utility (voter-utility current-winner true-ranking)]
         [best-responses (find-best-responses voter-idx current-profile voting-rule true-ranking)]
         [best-utility (voter-utility (voting-rule (list-set current-profile voter-idx (car best-responses)))
                                       true-ranking)])
    (> best-utility current-utility)))")
    (alternatives ())
    (source-commit "c6dffccb")
    (difficulty 2))

) ;; end tasks
