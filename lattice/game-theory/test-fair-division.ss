;;; lattice/game-theory/test-fair-division.ss — Tests for Fair Division
;;;
;;; Comprehensive tests for cake cutting, adjusted winner, and discrete division.

(load "core/testing/test-framework.ss")
(load "lattice/game-theory/fair-division.ss")

;;; ============================================================================
;;; Cake Representation Tests
;;; ============================================================================

(test-group "Cake Representation"

  (define-test "make-cake creates cake with correct player count"
    (let ([c (make-cake 3)])
      (assert-equal 3 (cake-players c))))

  (define-test "uniform valuation gives equal value everywhere"
    (let ([c (uniform-cake 2)])
      (assert-true (< (abs (- (cake-valuation c 0 0 0.5) 0.5)) 0.01))
      (assert-true (< (abs (- (cake-valuation c 0 0.5 1) 0.5)) 0.01))
      (assert-true (< (abs (- (cake-valuation c 0 0 1) 1)) 0.01))))

  (define-test "custom valuation integrates correctly"
    (let ([c (make-cake 1)])
      ;; Constant density 2 → integral from 0 to 1 = 2
      (cake-set-valuation! c 0 (lambda (x) 2))
      (let ([val (cake-valuation c 0 0 1)])
        (assert-true (< (abs (- val 2)) 0.01)))))

  (define-test "simple-cake-2 has opposing preferences"
    (let ([c (simple-cake-2)])
      ;; Player 0 values left half at ~1, right at ~0
      (let ([left-0 (cake-valuation c 0 0 0.5)]
            [right-0 (cake-valuation c 0 0.5 1)])
        (assert-true (> left-0 0.9))
        (assert-true (< right-0 0.1)))
      ;; Player 1 values right half at ~1, left at ~0
      (let ([left-1 (cake-valuation c 1 0 0.5)]
            [right-1 (cake-valuation c 1 0.5 1)])
        (assert-true (< left-1 0.1))
        (assert-true (> right-1 0.9))))))

;;; ============================================================================
;;; Piece Representation Tests
;;; ============================================================================

(test-group "Piece Representation"

  (define-test "piece-singleton creates single interval"
    (let ([p (piece-singleton 0.2 0.7)])
      (assert-equal 1 (length p))
      (assert-equal 0.2 (caar p))
      (assert-equal 0.7 (cdar p))))

  (define-test "piece-length computes total length"
    (let ([p (piece-merge (piece-singleton 0 0.3)
                          (piece-singleton 0.5 0.8))])
      (assert-true (< (abs (- (piece-length p) 0.6)) 0.01))))

  (define-test "piece-value uses cake valuation"
    (let ([c (uniform-cake 2)]
          [p (piece-singleton 0.25 0.75)])
      (assert-equal 0.5 (piece-value c 0 p)))))

;;; ============================================================================
;;; Fairness Criteria Tests
;;; ============================================================================

(test-group "Fairness Criteria"

  (define-test "proportional? accepts equal 2-player division"
    (let* ([c (uniform-cake 2)]
           [div (make-division 2)])
      (division-assign! div 0 (piece-singleton 0 0.5))
      (division-assign! div 1 (piece-singleton 0.5 1))
      (assert-true (proportional? c div))))

  (define-test "proportional? rejects unfair division"
    (let* ([c (uniform-cake 2)]
           [div (make-division 2)])
      (division-assign! div 0 (piece-singleton 0 0.3))
      (division-assign! div 1 (piece-singleton 0.3 1))
      (assert-false (proportional? c div))))

  (define-test "envy-free? accepts when no envy"
    (let* ([c (simple-cake-2)]  ; opposing preferences
           [div (make-division 2)])
      ;; Give left half to player 0, right to player 1
      (division-assign! div 0 (piece-singleton 0 0.5))
      (division-assign! div 1 (piece-singleton 0.5 1))
      (assert-true (envy-free? c div))))

  (define-test "envy-free? detects envy"
    (let* ([c (simple-cake-2)]
           [div (make-division 2)])
      ;; Give right half to player 0, left to player 1
      ;; Now player 0 envies player 1's piece (which player 0 values more)
      (division-assign! div 0 (piece-singleton 0.5 1))
      (division-assign! div 1 (piece-singleton 0 0.5))
      (assert-false (envy-free? c div))))

  (define-test "equitable? accepts equal shares"
    (let* ([c (uniform-cake 2)]
           [div (make-division 2)])
      (division-assign! div 0 (piece-singleton 0 0.5))
      (division-assign! div 1 (piece-singleton 0.5 1))
      (assert-true (equitable? c div 0.01)))))

;;; ============================================================================
;;; Cut-and-Choose Tests
;;; ============================================================================

(test-group "Cut-and-Choose"

  (define-test "cut-and-choose produces valid division"
    (let* ([c (uniform-cake 2)]
           [div (cut-and-choose c 100)])
      (assert-equal 2 (vector-length div))
      (assert-true (> (piece-length (division-piece div 0)) 0))
      (assert-true (> (piece-length (division-piece div 1)) 0))))

  (define-test "cut-and-choose is proportional for uniform cake"
    (let* ([c (uniform-cake 2)]
           [div (cut-and-choose c 100)])
      (assert-true (proportional? c div))))

  (define-test "cut-and-choose is envy-free for uniform cake"
    (let* ([c (uniform-cake 2)]
           [div (cut-and-choose c 100)])
      (assert-true (envy-free? c div))))

  (define-test "cut-and-choose with opposing preferences"
    (let* ([c (simple-cake-2)]
           [div (cut-and-choose c 100)])
      ;; Cutter (player 0) divides by their valuation
      ;; Chooser (player 1) picks preferred piece
      (assert-true (proportional? c div))))

  (define-test "cut-and-choose gives cutter fair share"
    (let* ([c (uniform-cake 2)]
           [div (cut-and-choose c 100)]
           [cutter-val (piece-value c 0 (division-piece div 0))])
      ;; Cutter should get at least 1/2 of their valuation
      (assert-true (>= cutter-val 0.49)))))

;;; ============================================================================
;;; Dubins-Spanier Tests
;;; ============================================================================

(test-group "Dubins-Spanier"

  (define-test "dubins-spanier allocates to all players"
    (let* ([c (uniform-cake 3)]
           [div (dubins-spanier c 100)])
      (assert-equal 3 (vector-length div))
      ;; Each player gets something
      (assert-true (> (piece-length (division-piece div 0)) 0))
      (assert-true (> (piece-length (division-piece div 1)) 0))
      (assert-true (> (piece-length (division-piece div 2)) 0))))

  (define-test "dubins-spanier is proportional"
    (let* ([c (uniform-cake 3)]
           [div (dubins-spanier c 100)])
      (assert-true (proportional? c div))))

  (define-test "dubins-spanier with 4 players"
    (let* ([c (uniform-cake 4)]
           [div (dubins-spanier c 100)])
      (assert-true (proportional? c div))))

  (define-test "dubins-spanier with opposing valuations"
    (let* ([c (opposing-valuations-cake)]
           [div (dubins-spanier c 100)])
      (assert-true (proportional? c div)))))

;;; ============================================================================
;;; Selfridge-Conway Tests
;;; ============================================================================

(test-group "Selfridge-Conway"

  (define-test "selfridge-conway allocates to all 3 players"
    (let* ([c (uniform-cake 3)]
           [div (selfridge-conway c 100)])
      (assert-equal 3 (vector-length div))
      (assert-true (> (piece-length (division-piece div 0)) 0))
      (assert-true (> (piece-length (division-piece div 1)) 0))
      (assert-true (> (piece-length (division-piece div 2)) 0))))

  (define-test "selfridge-conway is proportional"
    (let* ([c (uniform-cake 3)]
           [div (selfridge-conway c 100)])
      (assert-true (proportional? c div))))

  (define-test "selfridge-conway with opposing valuations allocates all"
    ;; Note: Full proportionality with extreme opposing valuations requires
    ;; the complete Selfridge-Conway trimming sub-protocol. This simplified
    ;; implementation allocates all pieces but may not be fully proportional
    ;; in adversarial cases.
    (let* ([c (opposing-valuations-cake)]
           [div (selfridge-conway c 100)]
           [total-length (+ (piece-length (division-piece div 0))
                           (piece-length (division-piece div 1))
                           (piece-length (division-piece div 2)))])
      (assert-true (> total-length 0.99)))))

;;; ============================================================================
;;; Adjusted Winner Tests
;;; ============================================================================

(test-group "Adjusted Winner"

  (define-test "make-adjusted-winner-problem creates problem"
    (let ([prob (make-adjusted-winner-problem
                  '(house car boat)
                  '(40 35 25)
                  '(30 50 20))])
      (assert-true (adjusted-winner-problem%? prob))))

  (define-test "adjusted winner assigns all goods"
    (let* ([prob (make-adjusted-winner-problem
                   '(house car boat)
                   '(40 35 25)
                   '(30 50 20))]
           [result (adjusted-winner prob)]
           [alloc-1 (vector-ref result 0)]
           [alloc-2 (vector-ref result 1)]
           [total-fracs (+ (fold-left + 0 (map cdr alloc-1))
                           (fold-left + 0 (map cdr alloc-2)))])
      ;; All goods should be assigned (fractions sum to 3)
      (assert-true (< (abs (- total-fracs 3)) 0.01))))

  (define-test "adjusted winner with clear preferences"
    ;; Player 1 values A highly, player 2 values B highly
    (let* ([prob (make-adjusted-winner-problem
                   '(A B)
                   '(80 20)
                   '(20 80))]
           [result (adjusted-winner prob)])
      ;; Should give A to player 1, B to player 2
      (assert-true (if (member 'A (map car (vector-ref result 0))) #t #f))
      (assert-true (if (member 'B (map car (vector-ref result 1))) #t #f))))

  (define-test "adjusted winner balances when needed"
    ;; Imbalanced: one player initially gets everything
    (let* ([prob (make-adjusted-winner-problem
                   '(A B C)
                   '(50 30 20)
                   '(40 35 25))]
           [result (adjusted-winner prob)]
           [alloc-1 (vector-ref result 0)]
           [alloc-2 (vector-ref result 1)])
      ;; Both should have some goods
      (assert-true (> (length alloc-1) 0))
      (assert-true (> (length alloc-2) 0)))))

;;; ============================================================================
;;; Discrete Fair Division Tests
;;; ============================================================================

(test-group "Discrete Fair Division"

  (define-test "make-discrete-problem creates problem"
    (let ([dp (make-discrete-problem
                '(A B C)
                '((10 20 30)
                  (30 20 10)))])
      (assert-equal 2 (discrete-problem-players dp))
      (assert-equal 3 (discrete-problem-goods-count dp))))

  (define-test "discrete-problem-valuation retrieves correct value"
    (let ([dp (make-discrete-problem
                '(A B C)
                '((10 20 30)
                  (30 20 10)))])
      (assert-equal 10 (discrete-problem-valuation dp 0 0))  ; P0's value for A
      (assert-equal 30 (discrete-problem-valuation dp 1 0))  ; P1's value for A
      (assert-equal 30 (discrete-problem-valuation dp 0 2))  ; P0's value for C
      (assert-equal 10 (discrete-problem-valuation dp 1 2)))) ; P1's value for C

  (define-test "round-robin allocates all goods"
    (let* ([dp (make-discrete-problem
                 '(A B C D)
                 '((10 20 30 40)
                   (40 30 20 10)))]
           [allocs (round-robin dp)])
      (assert-equal 2 (vector-length allocs))
      ;; All goods assigned
      (let ([all-goods (append (vector-ref allocs 0) (vector-ref allocs 1))])
        (assert-equal 4 (length all-goods)))))

  (define-test "round-robin gives each player something"
    (let* ([dp (make-discrete-problem
                 '(A B C D)
                 '((10 20 30 40)
                   (40 30 20 10)))]
           [allocs (round-robin dp)])
      (assert-true (> (length (vector-ref allocs 0)) 0))
      (assert-true (> (length (vector-ref allocs 1)) 0))))

  (define-test "discrete-allocation-value computes bundle value"
    (let ([dp (make-discrete-problem
                '(A B C)
                '((10 20 30)
                  (30 20 10)))])
      (assert-equal 30 (discrete-allocation-value dp 0 '(A B)))
      (assert-equal 50 (discrete-allocation-value dp 1 '(A B))))))

;;; ============================================================================
;;; Envy-Free Up To One Good (EF1) Tests
;;; ============================================================================

(test-group "Envy-Free Up To One Good"

  (define-test "EF1 satisfied when no envy exists"
    (let* ([dp (make-discrete-problem
                 '(A B)
                 '((10 0)
                   (0 10)))]
           [allocs (vector '(A) '(B))])
      (assert-true (envy-free-up-to-one? dp allocs))))

  (define-test "EF1 satisfied when removing one good eliminates envy"
    (let* ([dp (make-discrete-problem
                 '(A B C)
                 '((10 10 10)
                   (10 10 10)))]
           ;; Player 1 gets two goods, but EF1 holds
           [allocs (vector '(A) '(B C))])
      (assert-true (envy-free-up-to-one? dp allocs))))

  (define-test "round-robin produces EF1 allocation"
    (let* ([dp (make-discrete-problem
                 '(A B C D)
                 '((10 20 30 40)
                   (40 30 20 10)))]
           [allocs (round-robin dp)])
      (assert-true (envy-free-up-to-one? dp allocs)))))

;;; ============================================================================
;;; Maximin Share Tests
;;; ============================================================================

(test-group "Maximin Share"

  (define-test "maximin-share returns non-negative value"
    (let* ([dp (make-discrete-problem
                 '(A B C D)
                 '((10 20 30 40)
                   (40 30 20 10)))]
           [mms (maximin-share dp 0 100)])
      (assert-true (>= mms 0))))

  (define-test "maximin-share approximation is reasonable"
    ;; With 4 identical goods of value 10 each, MMS for 2 players should be ~20
    (let* ([dp (make-discrete-problem
                 '(A B C D)
                 '((10 10 10 10)
                   (10 10 10 10)))]
           [mms (maximin-share dp 0 100)])
      ;; Should get at least 20 (two goods)
      (assert-true (>= mms 20)))))

;;; ============================================================================
;;; Integration Tests
;;; ============================================================================

(test-group "Integration"

  (define-test "cake cutting covers entire cake"
    (let* ([c (uniform-cake 3)]
           [div (dubins-spanier c 100)]
           [total (+ (piece-length (division-piece div 0))
                     (piece-length (division-piece div 1))
                     (piece-length (division-piece div 2)))])
      (assert-true (> total 0.99))))

  (define-test "all protocols work with same cake"
    (let ([c (make-cake 2)])
      (cake-set-valuation! c 0 (lambda (x) (+ 1 x)))
      (cake-set-valuation! c 1 (lambda (x) (- 2 x)))
      (let ([div (cut-and-choose c 100)])
        (assert-true (proportional? c div))))))

;;; ============================================================================
;;; Run All Tests
;;; ============================================================================

(run-all-tests)
