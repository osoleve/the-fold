;;; lattice/game-theory/test-mechanism.ss — Tests for Mechanism Design
;;; @requires mechanism test-framework

(load "core/testing/test-framework.ss")
(load "lattice/game-theory/mechanism.ss")

;;; ============================================================================
;;; Bid Utilities
;;; ============================================================================

(test-group "bid-utilities"
  (define-test "bids-max finds maximum"
    (let ([bids (make-bids '(10 25 15 20))])
      (assert-equal 25 (bids-max bids))))

  (define-test "bids-second-max finds second highest"
    (let ([bids (make-bids '(10 25 15 20))])
      (assert-equal 20 (bids-second-max bids))))

  (define-test "bids-winners handles ties"
    (let ([bids (make-bids '(10 25 25 20))])
      (assert-equal '(1 2) (bids-winners bids))))

  (define-test "bids-second-max with ties"
    (let ([bids (make-bids '(30 30 20 10))])
      (assert-equal 30 (bids-second-max bids)))))

;;; ============================================================================
;;; Tie-Breaking Strategies
;;; ============================================================================

(test-group "tie-breaking"
  (define-test "default strategy is 'first"
    (assert-equal 'first (*tie-break-strategy*)))

  (define-test "select-winner with 'first picks lowest index"
    (parameterize ([*tie-break-strategy* 'first])
      (let* ([bids (make-bids '(25 25 25))]  ; 3-way tie
             [outcome (first-price-auction bids)])
        (assert-equal 0 (auction-outcome-winner outcome)))))

  (define-test "select-winner with 'last picks highest index"
    (parameterize ([*tie-break-strategy* 'last])
      (let* ([bids (make-bids '(25 25 25))]  ; 3-way tie
             [outcome (first-price-auction bids)])
        (assert-equal 2 (auction-outcome-winner outcome)))))

  (define-test "with-tie-break convenience works"
    (let* ([bids (make-bids '(25 25 25))]
           [outcome-first (with-tie-break 'first
                            (lambda () (first-price-auction bids)))]
           [outcome-last (with-tie-break 'last
                           (lambda () (first-price-auction bids)))])
      (assert-equal 0 (auction-outcome-winner outcome-first))
      (assert-equal 2 (auction-outcome-winner outcome-last))))

  (define-test "custom tie-break function"
    ;; Always pick the middle winner
    (let* ([pick-middle (lambda (winners)
                          (list-ref winners (quotient (length winners) 2)))]
           [bids (make-bids '(25 25 25))]
           [outcome (with-tie-break pick-middle
                      (lambda () (first-price-auction bids)))])
      (assert-equal 1 (auction-outcome-winner outcome))))

  (define-test "tie-break affects second-price auction"
    (let* ([bids (make-bids '(30 30 20))]  ; bidders 0,1 tie for first
           [outcome-first (with-tie-break 'first
                            (lambda () (second-price-auction bids)))]
           [outcome-last (with-tie-break 'last
                           (lambda () (second-price-auction bids)))])
      (assert-equal 0 (auction-outcome-winner outcome-first))
      (assert-equal 1 (auction-outcome-winner outcome-last))
      ;; Payment is still second-highest (30 from the other winner)
      (assert-equal 30 (auction-outcome-payment outcome-first))
      (assert-equal 30 (auction-outcome-payment outcome-last))))

  (define-test "tie-break affects all-pay auction"
    (let* ([bids (make-bids '(25 25 10))]
           [outcome-last (with-tie-break 'last
                           (lambda () (all-pay-auction bids)))])
      (assert-equal 1 (auction-outcome-winner outcome-last))))

  (define-test "invalid strategy symbol raises error"
    (assert-error
     (lambda ()
       (with-tie-break 'invalid-strategy
         (lambda () (first-price-auction (make-bids '(25 25))))))))

  (define-test "custom strategy returning invalid winner raises error"
    (assert-error
     (lambda ()
       (with-tie-break (lambda (winners) 999)  ; 999 not in winners
         (lambda () (first-price-auction (make-bids '(25 25)))))))))

;;; ============================================================================
;;; First-Price Auction
;;; ============================================================================

(test-group "first-price-auction"
  (define-test "highest bidder wins"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (first-price-auction bids)])
      (assert-equal 1 (auction-outcome-winner outcome))))

  (define-test "winner pays their bid"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (first-price-auction bids)])
      (assert-equal 25 (auction-outcome-payment outcome))))

  (define-test "revenue equals winning bid"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (first-price-auction bids)])
      (assert-equal 25 (auction-outcome-revenue outcome)))))

;;; ============================================================================
;;; Second-Price (Vickrey) Auction
;;; ============================================================================

(test-group "second-price-auction"
  (define-test "highest bidder wins"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (second-price-auction bids)])
      (assert-equal 1 (auction-outcome-winner outcome))))

  (define-test "winner pays second-highest bid"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (second-price-auction bids)])
      (assert-equal 20 (auction-outcome-payment outcome))))

  (define-test "truthful bidding is optimal (DSIC)"
    ;; Bidder 1 has value 25, should bid 25
    ;; If they bid 30 (overbid), they still pay 20
    ;; If they bid 18 (underbid), they might lose
    (let* ([true-vals (make-bids '(10 25 15 20))]
           [true-outcome (second-price-auction true-vals)]
           [underbid (make-bids '(10 18 15 20))]  ; bidder 1 underbids
           [underbid-outcome (second-price-auction underbid)])
      ;; With true bid: wins, pays 20, utility = 25 - 20 = 5
      (assert-equal 1 (auction-outcome-winner true-outcome))
      (assert-equal 5 (bidder-utility true-outcome 1 25))
      ;; With underbid: loses to bidder 3, utility = 0
      (assert-equal 3 (auction-outcome-winner underbid-outcome))
      (assert-equal 0 (bidder-utility underbid-outcome 1 25))))

  (define-test "no profitable deviation for DSIC"
    (let* ([valuations (make-bids '(10 25 15 20))]
           [deviations '(5 10 15 18 22 25 30 35)])
      ;; For second-price, no bidder should profit from deviating
      (assert-true (null? (check-dsic second-price-auction valuations deviations))))))

;;; ============================================================================
;;; All-Pay Auction
;;; ============================================================================

(test-group "all-pay-auction"
  (define-test "highest bidder wins"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (all-pay-auction bids)])
      (assert-equal 1 (auction-outcome-winner outcome))))

  (define-test "everyone pays"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (all-pay-auction bids)]
           [payments (auction-outcome-payment outcome)])
      (assert-equal 10 (vector-ref payments 0))
      (assert-equal 25 (vector-ref payments 1))
      (assert-equal 15 (vector-ref payments 2))
      (assert-equal 20 (vector-ref payments 3))))

  (define-test "revenue is sum of all bids"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (all-pay-auction bids)])
      (assert-equal 70 (auction-outcome-revenue outcome)))))

;;; ============================================================================
;;; Reserve Prices
;;; ============================================================================

(test-group "reserve-prices"
  (define-test "bids below reserve rejected"
    (let* ([bids (make-bids '(10 25 15 20))]
           [auction-with-reserve (with-reserve second-price-auction 30)]
           [outcome (auction-with-reserve bids)])
      (assert-equal #f (auction-outcome-winner outcome))))

  (define-test "winner pays at least reserve"
    (let* ([bids (make-bids '(10 35 15 20))]
           [auction-with-reserve (with-reserve second-price-auction 25)]
           [outcome (auction-with-reserve bids)])
      (assert-equal 1 (auction-outcome-winner outcome))
      ;; Second-highest is 20, but reserve is 25
      (assert-equal 25 (auction-outcome-payment outcome))))

  (define-test "with-reserve works with all-pay (vector payments)"
    (let* ([bids (make-bids '(10 35 15 20))]
           [auction-with-reserve (with-reserve all-pay-auction 25)]
           [outcome (auction-with-reserve bids)])
      ;; Only bidder 1 (35) meets reserve of 25
      (assert-equal 1 (auction-outcome-winner outcome))
      ;; Payment should be a vector
      (assert-true (vector? (auction-outcome-payment outcome))))))

;;; ============================================================================
;;; Bidder Utility
;;; ============================================================================

(test-group "bidder-utility"
  (define-test "winner utility is value minus payment"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (second-price-auction bids)])
      ;; Winner (bidder 1) pays 20, has value 25
      (assert-equal 5 (bidder-utility outcome 1 25))))

  (define-test "loser utility is zero"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (second-price-auction bids)])
      (assert-equal 0 (bidder-utility outcome 0 10))
      (assert-equal 0 (bidder-utility outcome 2 15))
      (assert-equal 0 (bidder-utility outcome 3 20))))

  (define-test "all-pay loser has negative utility"
    (let* ([bids (make-bids '(10 25 15 20))]
           [outcome (all-pay-auction bids)])
      ;; Loser 0 paid 10, won nothing
      (assert-equal -10 (bidder-utility outcome 0 10)))))

;;; ============================================================================
;;; VCG Mechanism
;;; ============================================================================

(test-group "vcg-mechanism"
  (define-test "vcg single item matches Vickrey"
    ;; Single item auction: m=1, bundles are {}, {0}
    ;; Agent valuations: v_i(empty) = 0, v_i({item}) = bid
    (let* ([n 3]
           [m 1]
           ;; valuations[i] = vector of size 2: v(empty), v(item)
           [valuations (vector (vector 0 10)   ; agent 0 values item at 10
                               (vector 0 25)   ; agent 1 values item at 25
                               (vector 0 15))] ; agent 2 values item at 15
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Agent 1 should get the item (bundle 1 = {item0})
      (assert-equal 1 (vector-ref allocation 1))
      ;; Agents 0 and 2 get nothing (bundle 0 = empty)
      (assert-equal 0 (vector-ref allocation 0))
      (assert-equal 0 (vector-ref allocation 2))
      ;; VCG payment for agent 1:
      ;; Welfare without 1 = max(10, 15) = 15 (agent 2 wins)
      ;; Others' welfare with 1 = 0 (they get nothing)
      ;; Payment = 15 - 0 = 15
      (assert-equal 15 (vector-ref payments 1))
      ;; Losers pay 0
      (assert-equal 0 (vector-ref payments 0))
      (assert-equal 0 (vector-ref payments 2))))

  (define-test "vcg two items with complements"
    ;; 2 items, 2 bidders
    ;; Agent 0: values each item at 5, both at 20 (complementary)
    ;; Agent 1: values each item at 8, both at 16 (substitutes)
    (let* ([n 2]
           [m 2]
           ;; bundles: 0={}, 1={a}, 2={b}, 3={a,b}
           [valuations (vector (vector 0 5 5 20)    ; agent 0
                               (vector 0 8 8 16))]  ; agent 1
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Efficient: give both to agent 0 (value 20) vs split (8+8=16 or 5+8=13)
      (assert-equal 3 (vector-ref allocation 0))
      (assert-equal 0 (vector-ref allocation 1))
      ;; Payment for agent 0:
      ;; Welfare without 0 = 16 (agent 1 gets both)
      ;; Others' welfare with 0 = 0 (agent 1 gets nothing)
      ;; Payment = 16 - 0 = 16
      (assert-equal 16 (vector-ref payments 0))
      (assert-equal 0 (vector-ref payments 1))))

  (define-test "vcg three items tests recursion depth"
    ;; 3 items (8 bundles), 2 bidders
    ;; bundles: 0={}, 1={a}, 2={b}, 3={a,b}, 4={c}, 5={a,c}, 6={b,c}, 7={a,b,c}
    (let* ([n 2]
           [m 3]
           ;; Agent 0: specialist in items a,b together
           ;; Agent 1: wants item c strongly
           [valuations (vector (vector 0  2  2 15  1  3  3 16)   ; agent 0
                               (vector 0  3  3  6 10 13 13 16))] ; agent 1
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Efficient: agent 0 gets {a,b} (15), agent 1 gets {c} (10) = 25 total
      ;; vs agent 0 gets all (16), agent 1 gets nothing (0) = 16
      ;; vs agent 1 gets all (16), agent 0 gets nothing (0) = 16
      (assert-equal 3 (vector-ref allocation 0))  ; {a,b}
      (assert-equal 4 (vector-ref allocation 1)))) ; {c}

  (define-test "vcg with all zero valuations"
    ;; Edge case: nobody values anything
    (let* ([n 2]
           [m 1]
           [valuations (vector (vector 0 0)
                               (vector 0 0))]
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Any allocation is efficient (all have welfare 0)
      ;; Payments should be 0 for everyone
      (assert-equal 0 (vector-ref payments 0))
      (assert-equal 0 (vector-ref payments 1))))

  (define-test "vcg with single bidder"
    ;; Edge case: only one bidder
    (let* ([n 1]
           [m 2]
           [valuations (vector (vector 0 5 8 12))]
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Single bidder gets optimal bundle
      (assert-equal 3 (vector-ref allocation 0))  ; {a,b} = 12
      ;; No externality on others (no others!) -> payment = 0
      (assert-equal 0 (vector-ref payments 0))))

  (define-test "vcg efficient split beats giving all to one"
    ;; Ensure splitting items across bidders when efficient
    ;; 2 items, 2 bidders who each want different items
    (let* ([n 2]
           [m 2]
           ;; Agent 0 wants item a only
           ;; Agent 1 wants item b only
           [valuations (vector (vector 0 10 0 10)   ; only values {a}
                               (vector 0 0 10 10))] ; only values {b}
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Efficient: split - each gets their preferred item (total 20)
      ;; vs one gets both (10)
      (assert-equal 1 (vector-ref allocation 0))  ; {a}
      (assert-equal 2 (vector-ref allocation 1))  ; {b}
      ;; Payments:
      ;; Without agent 0: agent 1 still gets {b} for 10. Others' welfare = 10
      ;; With agent 0: agent 1 gets {b} for 10. Others' welfare = 10
      ;; Payment 0 = 10 - 10 = 0
      (assert-equal 0 (vector-ref payments 0))
      (assert-equal 0 (vector-ref payments 1))))

  (define-test "vcg payment reflects externality"
    ;; 2 items, 3 bidders - verify VCG payments are externalities
    (let* ([n 3]
           [m 2]
           ;; bundles: 0={}, 1={a}, 2={b}, 3={a,b}
           [valuations (vector (vector 0 20 0 20)    ; agent 0 wants {a}
                               (vector 0 0 15 15)    ; agent 1 wants {b}
                               (vector 0 10 10 18))] ; agent 2 wants both
           [result (vcg-combinatorial valuations m)]
           [allocation (car result)]
           [payments (cdr result)])
      ;; Efficient: agent 0 gets {a} (20), agent 1 gets {b} (15) = 35 total
      ;; vs agent 2 gets both (18) = 18
      (assert-equal 1 (vector-ref allocation 0))  ; {a}
      (assert-equal 2 (vector-ref allocation 1))  ; {b}
      (assert-equal 0 (vector-ref allocation 2))  ; nothing
      ;; Payment for agent 0:
      ;; Without 0: agent 1 gets {b} (15), agent 2 gets {a} (10). Others' = 25
      ;; With 0: agent 1 gets {b} (15), agent 2 gets nothing. Others' = 15
      ;; Payment = 25 - 15 = 10
      (assert-equal 10 (vector-ref payments 0))
      ;; Payment for agent 1:
      ;; Without 1: agent 0 gets {a} (20), agent 2 gets {b} (10). Others' = 30
      ;; With 1: agent 0 gets {a} (20), agent 2 nothing. Others' = 20
      ;; Payment = 30 - 20 = 10
      (assert-equal 10 (vector-ref payments 1))
      ;; Agent 2 gets nothing, pays nothing
      (assert-equal 0 (vector-ref payments 2)))))

;;; ============================================================================
;;; Public Goods / Pivot Mechanism
;;; ============================================================================

(test-group "pivot-mechanism"
  (define-test "provide when sum exceeds cost"
    (let* ([valuations (vector 30 25 20)]
           [cost 60]
           [result (pivot-mechanism valuations cost)]
           [provide? (car result)])
      ;; 30 + 25 + 20 = 75 >= 60
      (assert-true provide?)))

  (define-test "no provision when sum below cost"
    (let* ([valuations (vector 10 15 20)]
           [cost 60]
           [result (pivot-mechanism valuations cost)]
           [provide? (car result)])
      ;; 10 + 15 + 20 = 45 < 60
      (assert-false provide?)))

  (define-test "pivotal agent pays"
    ;; Agent 0: 30, Agent 1: 25, Agent 2: 20. Cost: 60. Total: 75
    ;; Without agent 0: 25+20=45 < 60 → no provision
    ;; Agent 0 is pivotal: pays cost - others = 60 - 45 = 15
    (let* ([valuations (vector 30 25 20)]
           [cost 60]
           [result (pivot-mechanism valuations cost)]
           [payments (cdr result)])
      (assert-equal 15 (vector-ref payments 0))))

  (define-test "non-pivotal agents pay nothing"
    ;; With valuations [30, 25, 20] and cost 60:
    ;; Without agent 1: 30+20=50 < 60 → no provision → agent 1 IS pivotal
    ;; Without agent 2: 30+25=55 < 60 → no provision → agent 2 IS pivotal
    ;; All agents are pivotal in this case!
    (let* ([valuations (vector 40 25 20)]  ; Now 40+25+20 = 85
           [cost 60]
           [result (pivot-mechanism valuations cost)]
           [payments (cdr result)])
      ;; Without agent 0: 25+20=45 < 60 → pivotal, pays 60-45=15
      (assert-equal 15 (vector-ref payments 0))
      ;; Without agent 1: 40+20=60 >= 60 → NOT pivotal, pays 0
      (assert-equal 0 (vector-ref payments 1))
      ;; Without agent 2: 40+25=65 >= 60 → NOT pivotal, pays 0
      (assert-equal 0 (vector-ref payments 2)))))

;;; ============================================================================
;;; Double Auction
;;; ============================================================================

(test-group "k-double-auction"
  (define-test "no trade when bids below asks"
    (let* ([buyers (vector 10 15)]
           [sellers (vector 20 25)]
           [outcome (k-double-auction buyers sellers 0.5)])
      (assert-equal 0 (double-auction-volume outcome))))

  (define-test "trade occurs when overlap"
    (let* ([buyers (vector 30 25 20)]    ; sorted: 30, 25, 20
           [sellers (vector 10 15 22)]   ; sorted: 10, 15, 22
           [outcome (k-double-auction buyers sellers 0.5)])
      ;; Trades: buyer@30 with seller@10, buyer@25 with seller@15
      ;; No trade: buyer@20 vs seller@22 (buyer < seller)
      (assert-equal 2 (double-auction-volume outcome))))

  (define-test "k=0.5 splits difference"
    (let* ([buyers (vector 30)]
           [sellers (vector 10)]
           [outcome (k-double-auction buyers sellers 0.5)]
           [trades (double-auction-trades outcome)])
      ;; Price should be (30 + 10) / 2 = 20
      (let ([trade (car trades)])
        (assert-equal 20.0 (cadr trade))))))

;;; ============================================================================
;;; Incentive Compatibility Verification
;;; ============================================================================

(test-group "dsic-verification"
  (define-test "second-price is DSIC for basic case"
    (let* ([profiles (list (make-bids '(10 25 15))
                           (make-bids '(5 5 5))
                           (make-bids '(100 50 75)))]
           [deviations '(0 5 10 15 20 25 30 50 75 100 150)])
      (assert-true (verify-dsic second-price-auction profiles deviations))))

  (define-test "first-price is NOT DSIC"
    ;; In first-price, you should shade your bid below valuation
    (let* ([valuations (make-bids '(10 25 15))]
           [deviations '(20 21 22 23 24)])  ; bids below 25
      ;; Bidder 1 (value 25) should find 24 more profitable than 25
      ;; At bid 25: wins, pays 25, utility = 0
      ;; At bid 24: still wins (beats 15), pays 24, utility = 1
      (assert-false (null? (check-dsic first-price-auction valuations deviations))))))

;;; ============================================================================
;;; Individual Rationality
;;; ============================================================================

(test-group "individual-rationality"
  (define-test "second-price is ex-post IR"
    (let ([valuations (make-bids '(10 25 15 20))])
      (assert-true (check-ex-post-ir second-price-auction valuations))))

  (define-test "reserve below value maintains IR"
    (let* ([valuations (make-bids '(10 35 15 20))]
           [auction-with-reserve (with-reserve second-price-auction 25)])
      (assert-true (check-ex-post-ir auction-with-reserve valuations)))))

;;; ============================================================================
;;; Budget Balance
;;; ============================================================================

(test-group "budget-balance"
  (define-test "pivot mechanism is weakly budget balanced"
    (let* ([valuations (vector 40 25 20)]
           [cost 60]
           [result (pivot-mechanism valuations cost)]
           [payments (cdr result)])
      (assert-true (is-weakly-budget-balanced? payments)))))

;;; ============================================================================
;;; Revenue
;;; ============================================================================

(test-group "revenue-analysis"
  (define-test "first-price revenue >= second-price for same bids"
    ;; Note: This only holds for the same bids, not in equilibrium!
    (let* ([bids (make-bids '(10 25 15 20))]
           [fp-outcome (first-price-auction bids)]
           [sp-outcome (second-price-auction bids)])
      (assert-true (>= (auction-outcome-revenue fp-outcome)
                       (auction-outcome-revenue sp-outcome)))))

  (define-test "optimal reserve for uniform is 0.5"
    (assert-equal 0.5 (optimal-reserve-uniform 5))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
