;;; mechanism.sexp — Development tasks for lattice/game-theory/mechanism.ss
;;;
;;; Module: mechanism (tier 0, depends on prelude, sort)
;;; 720 lines, ~25 exported functions
;;; Mechanism design: auctions (first/second/third-price, all-pay, Dutch),
;;; reserve prices, bidder utility, DSIC analysis, VCG combinatorial,
;;; pivot mechanism, Myerson optimal auction, double auctions.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/game-theory/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement second-price-auction
  ;; ──────────────────────────────────────────────
  ;; The Vickrey auction — DSIC by design.

  (task
    (id "game-theory/mechanism/second-price-auction-impl")
    (module mechanism)
    (tier 0)
    (type implement)
    (description "Implement second-price-auction: the Vickrey auction where the highest bidder wins but pays the second-highest bid. This is the canonical truthful (DSIC) auction. Steps: (1) Find all bidders tied for highest bid using bids-winners. (2) Select one winner using select-winner (handles ties). (3) The payment is the second-highest bid, obtained via bids-second-max. (4) Return an auction outcome using make-auction-outcome with the winner and payment.")
    (context "Available: bids-winners, select-winner, bids-second-max, make-auction-outcome, let*. Four function calls chained.")
    (before "(define (second-price-auction bids)
  ;; TODO: highest bidder wins, pays second-highest bid
  (make-auction-outcome #f 0))")
    (test-file "lattice/game-theory/test-mechanism.ss")
    (test-forms (";; Bidder 0 wins (bid 10), pays second-highest (7)
(let* ([bids (make-bids '(10 7 5))]
       [result (second-price-auction bids)])
  (assert-equal 0 (auction-outcome-winner result))
  (assert-equal 7 (auction-outcome-payment result)))"
                 ";; Revenue = payment = second-highest bid
(let* ([bids (make-bids '(10 7 5))]
       [result (second-price-auction bids)])
  (assert-equal 7 (auction-outcome-revenue result)))"
                 ";; Two bidders: winner pays the other's bid
(let* ([bids (make-bids '(8 5))]
       [result (second-price-auction bids)])
  (assert-equal 0 (auction-outcome-winner result))
  (assert-equal 5 (auction-outcome-payment result)))"
                 ";; Single bidder: pays 0 (second-max of 1 bidder)
(let* ([bids (make-bids '(10))]
       [result (second-price-auction bids)])
  (assert-equal 0 (auction-outcome-winner result))
  (assert-equal 0 (auction-outcome-payment result)))"))
    (available-modules (prelude sort))
    (hints ("winners = (bids-winners bids)"
            "winner = (select-winner winners)"
            "payment = (bids-second-max bids)"
            "Return: (make-auction-outcome winner payment)"))
    (reference-solution "(define (second-price-auction bids)
  (let* ([winners (bids-winners bids)]
         [winner (select-winner winners)]
         [payment (bids-second-max bids)])
    (make-auction-outcome winner payment)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement bids-second-max
  ;; ──────────────────────────────────────────────
  ;; Track the two largest values in a single pass.

  (task
    (id "game-theory/mechanism/bids-second-max-impl")
    (module mechanism)
    (tier 0)
    (type implement)
    (description "Implement bids-second-max: find the second-highest bid in a bid vector. If fewer than 2 bidders, return 0. Use a single-pass algorithm tracking the two largest values: 'first' (highest) and 'second' (second-highest), both initialized to -inf.0. For each bid b: if b > first, second becomes first and first becomes b. Else if b > second, second becomes b. After the loop, return second. Use a named let with index i, first, and second.")
    (context "Available: bids-length, bids-ref, <, >=, >, cond, let. Named let loop with three state variables (i, first, second).")
    (before "(define (bids-second-max bids)
  ;; TODO: second-highest bid, 0 if < 2 bidders
  0)")
    (test-file "lattice/game-theory/test-mechanism.ss")
    (test-forms (";; Standard case: second-highest of (10, 7, 5) = 7
(assert-equal 7 (bids-second-max (make-bids '(10 7 5))))"
                 ";; Reversed order: second-highest of (5, 7, 10) = 7
(assert-equal 7 (bids-second-max (make-bids '(5 7 10))))"
                 ";; Two equal highest: second-highest of (10, 10, 5) = 10
(assert-equal 10 (bids-second-max (make-bids '(10 10 5))))"
                 ";; Single bidder: return 0
(assert-equal 0 (bids-second-max (make-bids '(10))))"))
    (available-modules (prelude sort))
    (hints ("Guard: (< n 2) -> 0"
            "Loop: (let loop ([i 0] [first -inf.0] [second -inf.0]) ...)"
            "Base: (>= i n) -> second"
            "Update: (let ([b (bids-ref bids i)]) (cond [(> b first) (loop (+ i 1) b first)] [(> b second) (loop (+ i 1) first b)] [else (loop (+ i 1) first second)]))"
            "Note: -inf.0 is negative infinity in Scheme"))
    (reference-solution "(define (bids-second-max bids)
  (let ([n (bids-length bids)])
    (if (< n 2)
        0
        (let loop ([i 0] [first -inf.0] [second -inf.0])
          (if (>= i n)
              second
              (let ([b (bids-ref bids i)])
                (cond
                  [(> b first)
                   (loop (+ i 1) b first)]
                  [(> b second)
                   (loop (+ i 1) first b)]
                  [else
                   (loop (+ i 1) first second)])))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement bidder-utility
  ;; ──────────────────────────────────────────────
  ;; Quasi-linear utility: valuation × won? − payment.

  (task
    (id "game-theory/mechanism/bidder-utility-impl")
    (module mechanism)
    (tier 0)
    (type implement)
    (description "Implement bidder-utility: compute utility for bidder i with given valuation. Under quasi-linear preferences, utility = valuation (if won) minus payment. Handle three cases based on the payment type: (1) Vector payment (all-pay auction): everyone pays. Check if i is the winner (number? winner and = i winner, or list? winner and member). Utility = valuation if won else 0, minus payment from vector. (2) Single winner with scalar payment: if i = winner, utility = valuation - payment, else 0. (3) List of winners with scalar payment (tie): each winner gets (valuation - payment) / number-of-winners.")
    (context "Available: auction-outcome-winner, auction-outcome-payment, vector?, vector-ref, number?, list?, member, =, -, /, length, cond, and, if, let. Three-branch cond on payment type.")
    (before "(define (bidder-utility outcome i valuation)
  ;; TODO: valuation * won? - payment
  0)")
    (test-file "lattice/game-theory/test-mechanism.ss")
    (test-forms (";; Winner in second-price: utility = valuation - second-highest
(let* ([bids (make-bids '(10 7 5))]
       [result (second-price-auction bids)])
  (assert-equal 3 (bidder-utility result 0 10)))"
                 ";; Loser gets zero utility
(let* ([bids (make-bids '(10 7 5))]
       [result (second-price-auction bids)])
  (assert-equal 0 (bidder-utility result 1 7)))"
                 ";; Winner in first-price: utility = valuation - bid
(let* ([bids (make-bids '(10 7 5))]
       [result (first-price-auction bids)])
  (assert-equal 0 (bidder-utility result 0 10)))"))
    (available-modules (prelude sort))
    (hints ("Extract winner and payment from outcome"
            "Check (vector? payment) first for all-pay case"
            "For scalar payment: (if (= i winner) (- valuation payment) 0)"
            "For list winner: (/ (- valuation payment) (length winner)) if (member i winner)"
            "All-pay: (- (if won? valuation 0) (vector-ref payment i))"))
    (reference-solution "(define (bidder-utility outcome i valuation)
  (let ([winner (auction-outcome-winner outcome)]
        [payment (auction-outcome-payment outcome)])
    (cond
      [(vector? payment)
       (let ([my-payment (vector-ref payment i)]
             [won? (cond
                     [(number? winner) (= i winner)]
                     [(list? winner) (and (member i winner) #t)]
                     [else #f])])
         (- (if won? valuation 0) my-payment))]
      [(number? winner)
       (if (= i winner)
           (- valuation payment)
           0)]
      [(and (list? winner) (member i winner))
       (/ (- valuation payment) (length winner))]
      [else 0])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement first-price-auction
  ;; ──────────────────────────────────────────────
  ;; Highest bidder wins, pays their bid.

  (task
    (id "game-theory/mechanism/first-price-auction-impl")
    (module mechanism)
    (tier 0)
    (type implement)
    (description "Implement first-price-auction: the simplest sealed-bid auction. The highest bidder wins and pays their own bid. Unlike the second-price auction, this is NOT incentive compatible — bidders have incentive to shade their bids below their true valuation. Steps: (1) Find tied winners via bids-winners. (2) Select one winner via select-winner. (3) Payment is the winner's own bid: if winner exists, look up (bids-ref bids winner), otherwise 0. (4) Return make-auction-outcome.")
    (context "Available: bids-winners, select-winner, bids-ref, make-auction-outcome, if, let*. Similar structure to second-price but payment differs.")
    (before "(define (first-price-auction bids)
  ;; TODO: highest bidder wins, pays own bid
  (make-auction-outcome #f 0))")
    (test-file "lattice/game-theory/test-mechanism.ss")
    (test-forms (";; Bidder 0 wins (bid 10), pays 10
(let* ([bids (make-bids '(10 7 5))]
       [result (first-price-auction bids)])
  (assert-equal 0 (auction-outcome-winner result))
  (assert-equal 10 (auction-outcome-payment result)))"
                 ";; Revenue = winner's bid = highest bid
(let* ([bids (make-bids '(10 7 5))]
       [result (first-price-auction bids)])
  (assert-equal 10 (auction-outcome-revenue result)))"
                 ";; Two bidders
(let* ([bids (make-bids '(3 8))]
       [result (first-price-auction bids)])
  (assert-equal 1 (auction-outcome-winner result))
  (assert-equal 8 (auction-outcome-payment result)))"))
    (available-modules (prelude sort))
    (hints ("winners = (bids-winners bids)"
            "winner = (select-winner winners)"
            "payment = (if winner (bids-ref bids winner) 0)"
            "Return: (make-auction-outcome winner payment)"))
    (reference-solution "(define (first-price-auction bids)
  (let* ([winners (bids-winners bids)]
         [winner (select-winner winners)]
         [payment (if winner (bids-ref bids winner) 0)])
    (make-auction-outcome winner payment)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement vcg-total-welfare
  ;; ──────────────────────────────────────────────
  ;; Sum of valuations under a given allocation.

  (task
    (id "game-theory/mechanism/vcg-total-welfare-impl")
    (module mechanism)
    (tier 0)
    (type implement)
    (description "Implement vcg-total-welfare: compute the total social welfare for a given allocation in a combinatorial auction. The valuations argument is a vector of vectors: valuations[i][j] is agent i's value for bundle j. The allocation argument is a vector where allocation[i] is the bundle assigned to agent i. Total welfare = sum over all agents of valuations[i][allocation[i]]. Use a named let loop with index i and running total, accessing nested vectors via vector-ref.")
    (context "Available: vector-length, vector-ref, >=, +, let. Named let loop with index and accumulator. Nested vector access: (vector-ref (vector-ref valuations i) (vector-ref allocation i)).")
    (before "(define (vcg-total-welfare valuations allocation)
  ;; TODO: sum_i valuations[i][allocation[i]]
  0)")
    (test-file "lattice/game-theory/test-mechanism.ss")
    (test-forms (";; Two agents, bundles {0,1,2,3}: agent 0 gets bundle 1 (value 3), agent 1 gets bundle 2 (value 4)
(let ([vals (vector (vector 0 3 5 8) (vector 0 2 4 6))]
      [alloc (vector 1 2)])
  (assert-equal 7 (vcg-total-welfare vals alloc)))"
                 ";; All agents get empty bundle (index 0): welfare = 0
(let ([vals (vector (vector 0 3 5) (vector 0 2 4))]
      [alloc (vector 0 0)])
  (assert-equal 0 (vcg-total-welfare vals alloc)))"
                 ";; Single agent gets best bundle
(let ([vals (vector (vector 0 10 20))]
      [alloc (vector 2)])
  (assert-equal 20 (vcg-total-welfare vals alloc)))"))
    (available-modules (prelude sort))
    (hints ("n = (vector-length allocation)"
            "Named let loop: (let loop ([i 0] [total 0]) ...)"
            "Base: (>= i n) -> total"
            "Access: (vector-ref (vector-ref valuations i) (vector-ref allocation i))"
            "Step: (+ total value-for-agent-i)"))
    (reference-solution "(define (vcg-total-welfare valuations allocation)
  (let ([n (vector-length allocation)])
    (let loop ([i 0] [total 0])
      (if (>= i n)
          total
          (loop (+ i 1)
                (+ total (vector-ref (vector-ref valuations i)
                                     (vector-ref allocation i))))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

) ;; end tasks
