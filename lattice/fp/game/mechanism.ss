;;; lattice/fp/game/mechanism.ss — Mechanism Design
;;; @module mechanism
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'mechanism)
(doc 'description "Mechanism design: auctions, incentive compatibility, VCG mechanisms")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "A mechanism is a game where the designer controls the rules to achieve desired outcomes. Core concepts: DSIC (dominant strategy incentive compatible) means truthful reporting is a dominant strategy. IR (individual rationality) means participants gain non-negative utility. Efficiency means maximizing total welfare.")

;;; ============================================================================
;;; Section: Bid and Valuation Representation
;;; ============================================================================

(doc 'section 'bid-representation)
(doc 'note "A bid profile is a vector of bids, one per bidder. A valuation profile is the true valuations. In truthful mechanisms, bids = valuations at equilibrium.")

(define (make-bids bid-list)
  (doc 'export #t)
  (doc 'type '(-> (List Number) (Vector Number)))
  (doc 'description "Create a bid profile from a list of bids")
  (list->vector bid-list))

(define (bids-ref bids i)
  (doc 'type '(-> (Vector Number) Nat Number))
  (doc 'description "Get bid from bidder i")
  (vector-ref bids i))

(define (bids-length bids)
  (doc 'type '(-> (Vector Number) Nat))
  (doc 'description "Number of bidders")
  (vector-length bids))

(define (bids-max bids)
  (doc 'type '(-> (Vector Number) Number))
  (doc 'description "Maximum bid")
  (let ([n (bids-length bids)])
    (if (= n 0)
        0
        (let loop ([i 1] [max-bid (bids-ref bids 0)])
          (if (>= i n)
              max-bid
              (loop (+ i 1) (max max-bid (bids-ref bids i))))))))

(define (bids-second-max bids)
  (doc 'type '(-> (Vector Number) Number))
  (doc 'description "Second-highest bid (0 if < 2 bidders)")
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
                   (loop (+ i 1) first second)])))))))

(define (bids-winners bids)
  (doc 'type '(-> (Vector Number) (List Nat)))
  (doc 'description "List of bidders with the maximum bid (handles ties)")
  (let* ([n (bids-length bids)]
         [max-bid (bids-max bids)])
    (filter (lambda (i) (= (bids-ref bids i) max-bid))
            (iota n))))

;;; ============================================================================
;;; Section: Auction Outcome
;;; ============================================================================

(doc 'section 'auction-outcome)
(doc 'note "An auction outcome specifies the winner(s) and payments. For single-item auctions: one winner, one payment. For multi-item: allocation map and payment vector.")

(define-record-type auction-outcome%
  (fields winner    ; Nat | (List Nat) - winner index(es)
          payment   ; Number | (Vector Number) - payment(s)
          revenue)) ; Number - total seller revenue

(define (make-auction-outcome winner payment)
  (doc 'export #t)
  (doc 'type '(-> (U Nat (List Nat)) (U Number (Vector Number)) AuctionOutcome))
  (doc 'description "Create an auction outcome with winner(s) and payment(s)")
  (let ([revenue (if (vector? payment)
                     (let loop ([i 0] [sum 0])
                       (if (>= i (vector-length payment))
                           sum
                           (loop (+ i 1) (+ sum (vector-ref payment i)))))
                     payment)])
    (make-auction-outcome% winner payment revenue)))

(define (auction-outcome-winner outcome)
  (doc 'export #t)
  (doc 'type '(-> AuctionOutcome (U Nat (List Nat))))
  (auction-outcome%-winner outcome))

(define (auction-outcome-payment outcome)
  (doc 'export #t)
  (doc 'type '(-> AuctionOutcome (U Number (Vector Number))))
  (auction-outcome%-payment outcome))

(define (auction-outcome-revenue outcome)
  (doc 'export #t)
  (doc 'type '(-> AuctionOutcome Number))
  (auction-outcome%-revenue outcome))

;;; ============================================================================
;;; Section: Single-Item Auctions
;;; ============================================================================

(doc 'section 'single-item-auctions)
(doc 'note "Classic single-item sealed-bid auctions. All take a bid vector and return an outcome.")

;;; First-Price Sealed-Bid Auction
;;; Winner pays their bid. NOT truthful - optimal to shade bid below valuation.
(define (first-price-auction bids)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) AuctionOutcome))
  (doc 'description "First-price sealed-bid auction: highest bidder wins, pays their bid")
  (doc 'note "Not incentive compatible - bidders shade bids. Equilibrium bid = (n-1)/n × valuation for n risk-neutral bidders with uniform valuations.")
  (let* ([winners (bids-winners bids)]
         [winner (if (null? winners) #f (car winners))]
         [payment (if winner (bids-ref bids winner) 0)])
    (make-auction-outcome winner payment)))

;;; Second-Price Sealed-Bid Auction (Vickrey Auction)
;;; Winner pays second-highest bid. DSIC - truthful bidding is dominant.
(define (second-price-auction bids)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) AuctionOutcome))
  (doc 'description "Second-price sealed-bid (Vickrey) auction: highest bidder wins, pays second-highest bid")
  (doc 'note "DSIC: Truthful bidding is a dominant strategy. Winner's utility = valuation - payment = v - second-highest-bid.")
  (let* ([winners (bids-winners bids)]
         [winner (if (null? winners) #f (car winners))]
         [payment (bids-second-max bids)])
    (make-auction-outcome winner payment)))

;;; All-Pay Auction
;;; Everyone pays their bid, highest bidder wins.
;;; Used to model lobbying, R&D races, contests.
(define (all-pay-auction bids)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) AuctionOutcome))
  (doc 'description "All-pay auction: everyone pays their bid, highest bidder wins")
  (doc 'note "Models contests where effort is sunk (lobbying, R&D). Not truthful. Revenue can exceed valuation in equilibrium.")
  (let* ([n (bids-length bids)]
         [winners (bids-winners bids)]
         [winner (if (null? winners) #f (car winners))]
         [payments (let ([v (make-vector n 0)])
                     (do ([i 0 (+ i 1)])
                         [(= i n) v]
                       (vector-set! v i (bids-ref bids i))))]
         [total-revenue (let loop ([i 0] [sum 0])
                          (if (>= i n)
                              sum
                              (loop (+ i 1) (+ sum (bids-ref bids i)))))])
    (make-auction-outcome% winner payments total-revenue)))

;;; Third-Price Auction
;;; Winner pays third-highest bid. Interesting theoretical properties.
(define (third-price-auction bids)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) AuctionOutcome))
  (doc 'description "Third-price auction: highest bidder wins, pays third-highest bid")
  (doc 'note "Not DSIC. Incentivizes overbidding. Useful as a theoretical example of non-truthful mechanisms.")
  (let* ([n (bids-length bids)]
         [winners (bids-winners bids)]
         [winner (if (null? winners) #f (car winners))]
         [sorted (sort > (vector->list bids))]
         [payment (if (>= n 3) (caddr sorted) 0)])
    (make-auction-outcome winner payment)))

;;; ============================================================================
;;; Section: Reserve Prices
;;; ============================================================================

(doc 'section 'reserve-prices)
(doc 'note "Reserve price = minimum acceptable bid. Bids below reserve are rejected.")

(define (with-reserve auction reserve-price)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) Number (-> (Vector Number) AuctionOutcome)))
  (doc 'description "Wrap an auction with a reserve price")
  (lambda (bids)
    (let* ([n (bids-length bids)]
           [filtered-bids (make-vector n 0)])
      ;; Zero out bids below reserve
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (let ([b (bids-ref bids i)])
          (vector-set! filtered-bids i (if (>= b reserve-price) b 0))))
      ;; Check if anyone meets reserve
      (if (> (bids-max filtered-bids) 0)
          (let ([result (auction filtered-bids)])
            ;; Ensure payment is at least reserve
            (let ([winner (auction-outcome-winner result)]
                  [payment (auction-outcome-payment result)])
              (make-auction-outcome winner (max payment reserve-price))))
          ;; No sale
          (make-auction-outcome #f 0)))))

;;; ============================================================================
;;; Section: Bidder Utility
;;; ============================================================================

(doc 'section 'bidder-utility)
(doc 'note "Utility = valuation × (won?) - payment. Quasi-linear preferences assumed.")

(define (bidder-utility outcome i valuation)
  (doc 'export #t)
  (doc 'type '(-> AuctionOutcome Nat Number Number))
  (doc 'description "Compute utility for bidder i with given valuation")
  (let ([winner (auction-outcome-winner outcome)]
        [payment (auction-outcome-payment outcome)])
    (cond
      ;; All-pay: everyone pays (check first since payment is vector)
      [(vector? payment)
       (let ([my-payment (vector-ref payment i)]
             [won? (cond
                     [(number? winner) (= i winner)]
                     [(list? winner) (and (member i winner) #t)]
                     [else #f])])
         (- (if won? valuation 0) my-payment))]
      ;; Single winner with scalar payment
      [(number? winner)
       (if (= i winner)
           (- valuation payment)
           0)]
      ;; List of winners (tie) with scalar payment
      [(and (list? winner) (member i winner))
       (- valuation (/ payment (length winner)))]
      [else 0])))

;;; ============================================================================
;;; Section: Incentive Compatibility Analysis
;;; ============================================================================

(doc 'section 'incentive-compatibility)
(doc 'note "DSIC = dominant strategy incentive compatible. A mechanism is DSIC if truthful reporting is a dominant strategy for all agents, regardless of others' actions.")

(define (is-dsic-deviation-profitable? auction valuations i deviation)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) (Vector Number) Nat Number Boolean))
  (doc 'description "Would bidder i profit by bidding deviation instead of their true valuation?")
  (let* ([n (vector-length valuations)]
         [true-bids (vector-copy valuations)]
         [deviated-bids (let ([v (vector-copy valuations)])
                          (vector-set! v i deviation)
                          v)]
         [true-outcome (auction true-bids)]
         [deviated-outcome (auction deviated-bids)]
         [true-utility (bidder-utility true-outcome i (vector-ref valuations i))]
         [deviated-utility (bidder-utility deviated-outcome i (vector-ref valuations i))])
    (> deviated-utility true-utility)))

(define (check-dsic auction valuations deviations)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) (Vector Number) (List Number) (List (Nat Number Number Number))))
  (doc 'description "Check DSIC by testing deviations. Returns list of profitable deviations: (bidder deviation gain utility-diff)")
  (let* ([n (vector-length valuations)]
         [profitable '()])
    (do ([i 0 (+ i 1)])
        [(= i n) (reverse profitable)]
      (let ([vi (vector-ref valuations i)])
        (for-each
         (lambda (deviation)
           (when (is-dsic-deviation-profitable? auction valuations i deviation)
             (let* ([true-bids (vector-copy valuations)]
                    [deviated-bids (let ([v (vector-copy valuations)])
                                     (vector-set! v i deviation)
                                     v)]
                    [true-outcome (auction true-bids)]
                    [deviated-outcome (auction deviated-bids)]
                    [true-utility (bidder-utility true-outcome i vi)]
                    [deviated-utility (bidder-utility deviated-outcome i vi)])
               (set! profitable (cons (list i deviation (- deviated-utility true-utility) deviated-utility)
                                      profitable)))))
         deviations)))))

(define (verify-dsic auction test-valuations test-deviations)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) (List (Vector Number)) (List Number) Boolean))
  (doc 'description "Verify DSIC property across multiple valuation profiles. Returns #t if no profitable deviations found.")
  (let loop ([profiles test-valuations])
    (if (null? profiles)
        #t
        (let ([deviations (check-dsic auction (car profiles) test-deviations)])
          (if (null? deviations)
              (loop (cdr profiles))
              #f)))))

;;; ============================================================================
;;; Section: Revenue Equivalence
;;; ============================================================================

(doc 'section 'revenue-equivalence)
(doc 'note "Revenue Equivalence Theorem: Under certain conditions, all standard auctions yield the same expected revenue. This section provides tools to compare auction revenues.")

(define (expected-revenue-uniform auction n num-samples)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) Nat Nat Number))
  (doc 'description "Estimate expected revenue with n bidders, valuations uniform on [0,1], via Monte Carlo")
  (let loop ([samples 0] [total 0.0])
    (if (>= samples num-samples)
        (/ total num-samples)
        (let* ([valuations (make-vector n 0)]
               [_ (do ([i 0 (+ i 1)])
                      [(= i n)]
                    (vector-set! valuations i (random 1.0)))]
               [outcome (auction valuations)]
               [revenue (auction-outcome-revenue outcome)])
          (loop (+ samples 1) (+ total revenue))))))

;;; ============================================================================
;;; Section: VCG Mechanism (Multi-Item)
;;; ============================================================================

(doc 'section 'vcg-mechanism)
(doc 'note "VCG (Vickrey-Clarke-Groves) is the unique mechanism family that is DSIC + efficient for quasi-linear preferences. Each agent pays their externality on others.")

;;; VCG for combinatorial auctions
;;; valuations: n × 2^m matrix where valuations[i][S] = agent i's value for bundle S
;;; Returns: (allocation . payments) where allocation[i] = bundle for agent i

(define (vcg-combinatorial valuations m)
  (doc 'export #t)
  (doc 'type '(-> (Vector (Vector Number)) Nat (Pair (Vector Nat) (Vector Number))))
  (doc 'description "VCG mechanism for combinatorial auction with m items")
  (doc 'note "Items are allocated efficiently (maximize total value), payments = externality imposed on others. Complexity: O(n × 2^m) - exponential in items.")
  (let* ([n (vector-length valuations)]
         [num-bundles (expt 2 m)])
    ;; Find efficient allocation (max total welfare)
    ;; Allocation = vector of bundles, one per agent
    ;; Constraint: bundles must be disjoint
    (let-values ([(best-allocation best-welfare)
                  (vcg-find-efficient-allocation valuations n m)])
      ;; Compute VCG payments
      (let ([payments (make-vector n 0)])
        (do ([i 0 (+ i 1)])
            [(= i n)]
          ;; Payment_i = welfare of others without i - welfare of others with i
          (let-values ([(alloc-without-i welfare-without-i)
                        (vcg-find-efficient-allocation-without valuations n m i)])
            (let ([others-welfare-with-i
                   (- best-welfare (vector-ref (vector-ref valuations i)
                                               (vector-ref best-allocation i)))])
              (vector-set! payments i (- welfare-without-i others-welfare-with-i)))))
        (cons best-allocation payments)))))

(define (vcg-find-efficient-allocation valuations n m)
  (doc 'type '(-> (Vector (Vector Number)) Nat Nat (Values (Vector Nat) Number)))
  (doc 'description "Find welfare-maximizing allocation (brute force for small m)")
  ;; Brute force over all possible allocations
  ;; An allocation assigns a disjoint bundle to each agent
  (let ([best-alloc (make-vector n 0)]
        [best-welfare -inf.0])
    (vcg-enumerate-allocations
     n m
     (lambda (allocation)
       (let ([welfare (vcg-total-welfare valuations allocation)])
         (when (> welfare best-welfare)
           (set! best-welfare welfare)
           (do ([i 0 (+ i 1)])
               [(= i n)]
             (vector-set! best-alloc i (vector-ref allocation i)))))))
    (values best-alloc best-welfare)))

(define (vcg-find-efficient-allocation-without valuations n m excluded)
  (doc 'type '(-> (Vector (Vector Number)) Nat Nat Nat (Values (Vector Nat) Number)))
  (doc 'description "Find efficient allocation excluding agent 'excluded'")
  (let ([best-alloc (make-vector n 0)]
        [best-welfare -inf.0])
    (vcg-enumerate-allocations
     n m
     (lambda (allocation)
       ;; Skip allocations that give anything to excluded agent
       (when (= 0 (vector-ref allocation excluded))
         (let ([welfare (vcg-total-welfare-without valuations allocation excluded)])
           (when (> welfare best-welfare)
             (set! best-welfare welfare)
             (do ([i 0 (+ i 1)])
                 [(= i n)]
               (vector-set! best-alloc i (vector-ref allocation i))))))))
    (values best-alloc best-welfare)))

(define (vcg-total-welfare valuations allocation)
  (doc 'type '(-> (Vector (Vector Number)) (Vector Nat) Number))
  (let ([n (vector-length allocation)])
    (let loop ([i 0] [total 0])
      (if (>= i n)
          total
          (loop (+ i 1)
                (+ total (vector-ref (vector-ref valuations i)
                                     (vector-ref allocation i))))))))

(define (vcg-total-welfare-without valuations allocation excluded)
  (doc 'type '(-> (Vector (Vector Number)) (Vector Nat) Nat Number))
  (let ([n (vector-length allocation)])
    (let loop ([i 0] [total 0])
      (if (>= i n)
          total
          (if (= i excluded)
              (loop (+ i 1) total)
              (loop (+ i 1)
                    (+ total (vector-ref (vector-ref valuations i)
                                         (vector-ref allocation i)))))))))

(define (vcg-enumerate-allocations n m callback)
  (doc 'type '(-> Nat Nat (-> (Vector Nat) Void) Void))
  (doc 'description "Enumerate all valid allocations (disjoint bundles) and call callback")
  (let ([allocation (make-vector n 0)])
    (vcg-enumerate-helper allocation n m 0 0 callback)))

(define (vcg-enumerate-helper allocation n m agent used-items callback)
  (if (>= agent n)
      ;; Valid complete allocation
      (callback allocation)
      ;; Try all bundles disjoint from used-items
      (let ([num-bundles (expt 2 m)])
        (do ([bundle 0 (+ bundle 1)])
            [(= bundle num-bundles)]
          ;; Check if bundle is disjoint from used-items
          (when (= 0 (bitwise-and bundle used-items))
            (vector-set! allocation agent bundle)
            (vcg-enumerate-helper allocation n m (+ agent 1)
                                  (bitwise-ior used-items bundle)
                                  callback))))))

;;; ============================================================================
;;; Section: Public Goods and Pivot Mechanism
;;; ============================================================================

(doc 'section 'public-goods)
(doc 'note "For public goods (non-excludable, non-rival), the pivot mechanism determines provision and cost sharing. Also called the Clarke mechanism.")

(define (pivot-mechanism valuations cost)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Number (Pair Boolean (Vector Number))))
  (doc 'description "Pivot (Clarke) mechanism for binary public good provision")
  (doc 'note "Provide good if sum(valuations) >= cost. Each agent pays their 'pivot' amount if they change the outcome.")
  (let* ([n (vector-length valuations)]
         [total-value (let loop ([i 0] [sum 0])
                        (if (>= i n)
                            sum
                            (loop (+ i 1) (+ sum (vector-ref valuations i)))))]
         [provide? (>= total-value cost)]
         [payments (make-vector n 0)])
    ;; Compute pivot payments
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (let* ([value-without-i (- total-value (vector-ref valuations i))]
             [provide-without-i? (>= value-without-i cost)])
        ;; Pay only if pivotal (outcome changes)
        (when (not (eq? provide? provide-without-i?))
          (if provide?
              ;; Good provided because of i: pay what others lose
              (vector-set! payments i (- cost value-without-i))
              ;; Good not provided because of i: pay what others would gain
              (vector-set! payments i (- value-without-i cost))))))
    (cons provide? payments)))

;;; ============================================================================
;;; Section: Myerson Optimal Auction
;;; ============================================================================

(doc 'section 'myerson-optimal)
(doc 'note "Myerson's optimal auction maximizes expected revenue. For symmetric bidders with regular distributions, it's a second-price auction with an optimal reserve price.")

(define (myerson-virtual-value v f F)
  (doc 'export #t)
  (doc 'type '(-> Number (-> Number Number) (-> Number Number) Number))
  (doc 'description "Myerson virtual value: v - (1 - F(v))/f(v)")
  (doc 'note "f = PDF, F = CDF of value distribution. Allocate to highest non-negative virtual value.")
  (- v (/ (- 1 (F v)) (f v))))

(define (optimal-reserve-uniform n)
  (doc 'export #t)
  (doc 'type '(-> Nat Number))
  (doc 'description "Optimal reserve price for uniform[0,1] valuations: always 1/2")
  (doc 'note "This is a classic result: regardless of n, reserve = 1/2 maximizes revenue for uniform[0,1]")
  0.5)

;;; ============================================================================
;;; Section: Double Auctions
;;; ============================================================================

(doc 'section 'double-auction)
(doc 'note "Double auctions have buyers and sellers. Classic example: k-double auction where trade price = weighted average of k-th buyer and seller bids.")

(define-record-type double-auction-outcome%
  (fields trades      ; List of (buyer seller price quantity)
          buyer-surplus  ; Total buyer surplus
          seller-surplus ; Total seller surplus
          volume))    ; Total volume traded

(define (k-double-auction buyer-bids seller-asks k)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) (Vector Number) Number DoubleAuctionOutcome))
  (doc 'description "k-double auction: price = k × (marginal winning bid) + (1-k) × (marginal winning ask)")
  (doc 'note "k=0: seller's price (highest winning ask). k=1: buyer's price (lowest winning bid). k=0.5: split the difference. Only k=0.5 is budget-balanced in expectation.")
  (let* ([sorted-buyers (sort > (vector->list buyer-bids))]
         [sorted-sellers (sort < (vector->list seller-asks))]
         ;; Find market-clearing quantity
         [qty (let loop ([b sorted-buyers] [s sorted-sellers] [q 0])
                (if (or (null? b) (null? s) (< (car b) (car s)))
                    q
                    (loop (cdr b) (cdr s) (+ q 1))))])
    (if (= qty 0)
        (make-double-auction-outcome% '() 0 0 0)
        (let* (;; Price based on marginal (last) winning traders
               [clearing-buyer (list-ref sorted-buyers (- qty 1))]
               [clearing-seller (list-ref sorted-sellers (- qty 1))]
               ;; k-double price
               [price (+ (* k clearing-buyer) (* (- 1 k) clearing-seller))]
               ;; Compute surpluses
               [buyer-surplus (let loop ([bids sorted-buyers] [n qty] [sum 0])
                                (if (= n 0)
                                    sum
                                    (loop (cdr bids) (- n 1)
                                          (+ sum (- (car bids) price)))))]
               [seller-surplus (let loop ([asks sorted-sellers] [n qty] [sum 0])
                                 (if (= n 0)
                                     sum
                                     (loop (cdr asks) (- n 1)
                                           (+ sum (- price (car asks))))))])
          (make-double-auction-outcome%
           (list (list 'aggregate-trade price qty))
           buyer-surplus
           seller-surplus
           qty)))))

(define (double-auction-trades outcome)
  (doc 'export #t)
  (double-auction-outcome%-trades outcome))

(define (double-auction-volume outcome)
  (doc 'export #t)
  (double-auction-outcome%-volume outcome))

(define (double-auction-buyer-surplus outcome)
  (doc 'export #t)
  (double-auction-outcome%-buyer-surplus outcome))

(define (double-auction-seller-surplus outcome)
  (doc 'export #t)
  (double-auction-outcome%-seller-surplus outcome))

(define (double-auction-total-surplus outcome)
  (doc 'export #t)
  (doc 'type '(-> DoubleAuctionOutcome Number))
  (+ (double-auction-buyer-surplus outcome)
     (double-auction-seller-surplus outcome)))

;;; ============================================================================
;;; Section: Descending Clock Auction (Dutch)
;;; ============================================================================

(doc 'section 'descending-auction)
(doc 'note "Dutch (descending) auction: price starts high and drops until someone accepts. Strategically equivalent to first-price sealed-bid.")

(define (dutch-auction valuations start-price decrement)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Number Number AuctionOutcome))
  (doc 'description "Simulate Dutch auction with known valuations (jump bidding at v - ε)")
  (doc 'note "With rational bidders, winner is highest valuation, payment slightly below. Equivalent to first-price in expectation.")
  (let* ([n (vector-length valuations)]
         [max-val (let loop ([i 0] [best -inf.0])
                    (if (>= i n)
                        best
                        (loop (+ i 1) (max best (vector-ref valuations i)))))]
         [winner (let loop ([i 0])
                   (if (>= i n)
                       #f
                       (if (= (vector-ref valuations i) max-val)
                           i
                           (loop (+ i 1)))))]
         ;; Payment = highest price at or below valuation
         [payment (let loop ([price start-price])
                    (if (<= price max-val)
                        price
                        (loop (- price decrement))))])
    (make-auction-outcome winner payment)))

;;; ============================================================================
;;; Section: Revelation Principle
;;; ============================================================================

(doc 'section 'revelation-principle)
(doc 'note "Revelation principle: Any outcome achievable by any mechanism can be achieved by a direct, truthful mechanism. This section provides utilities for converting indirect mechanisms to direct ones.")

(define (make-direct-mechanism indirect-game equilibrium-strategy)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Any) Any) (-> Any Any) (-> (Vector Any) Any)))
  (doc 'description "Convert indirect mechanism to direct: agents report types, mechanism applies equilibrium strategy")
  (doc 'note "This constructs the 'revelation' version of any mechanism. If original mechanism has equilibrium where agents play σ(type), direct mechanism asks for type and plays σ internally.")
  (lambda (reported-types)
    (let* ([n (vector-length reported-types)]
           [actions (make-vector n #f)])
      ;; Apply equilibrium strategy to each reported type
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (vector-set! actions i (equilibrium-strategy (vector-ref reported-types i))))
      ;; Run indirect game with these actions
      (indirect-game actions))))

;;; ============================================================================
;;; Section: Individual Rationality
;;; ============================================================================

(doc 'section 'individual-rationality)
(doc 'note "IR = Individual Rationality. Ex-post IR: utility >= 0 always. Interim IR: expected utility >= 0. Ex-ante IR: expected utility before knowing type >= 0.")

(define (check-ex-post-ir auction valuations)
  (doc 'export #t)
  (doc 'type '(-> (-> (Vector Number) AuctionOutcome) (Vector Number) Boolean))
  (doc 'description "Check if all bidders have non-negative utility (ex-post IR)")
  (let* ([outcome (auction valuations)]
         [n (vector-length valuations)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (let ([utility (bidder-utility outcome i (vector-ref valuations i))])
            (if (>= utility 0)
                (loop (+ i 1))
                #f))))))

;;; ============================================================================
;;; Section: Budget Balance
;;; ============================================================================

(doc 'section 'budget-balance)
(doc 'note "A mechanism is budget-balanced if payments sum to zero (no money created/destroyed). Weak BB: payments sum to >= 0 (no deficit).")

(define (is-budget-balanced? payments tolerance)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Number Boolean))
  (doc 'description "Check if payments sum to zero (within tolerance)")
  (let ([total (let loop ([i 0] [sum 0])
                 (if (>= i (vector-length payments))
                     sum
                     (loop (+ i 1) (+ sum (vector-ref payments i)))))])
    (< (abs total) tolerance)))

(define (is-weakly-budget-balanced? payments)
  (doc 'export #t)
  (doc 'type '(-> (Vector Number) Boolean))
  (doc 'description "Check if payments sum to >= 0 (no deficit)")
  (let ([total (let loop ([i 0] [sum 0])
                 (if (>= i (vector-length payments))
                     sum
                     (loop (+ i 1) (+ sum (vector-ref payments i)))))])
    (>= total 0)))

;;; ============================================================================
;;; Section: Classic Impossibility Results
;;; ============================================================================

(doc 'section 'impossibility-results)
(doc 'note "Famous impossibility theorems constrain what mechanisms can achieve.")

(doc 'theorem "Myerson-Satterthwaite Impossibility"
     "For bilateral trade with private valuations, no mechanism is simultaneously: (1) DSIC, (2) ex-post individually rational, (3) ex-post budget balanced, (4) efficient (trade iff v_buyer > v_seller).")

(doc 'theorem "Gibbard-Satterthwaite"
     "For deterministic mechanisms with >= 3 outcomes and unrestricted preferences: if the mechanism is strategy-proof (DSIC), it must be dictatorial (one agent determines outcome).")

(doc 'theorem "Green-Laffont"
     "VCG is the ONLY mechanism family achieving DSIC + efficiency for quasi-linear preferences on arbitrary domains.")
