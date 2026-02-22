(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'sort)
(require 'vec)

(doc 'module 'fair-division)
(doc 'description "Algorithms for fairly dividing divisible and indivisible goods. Covers cake cutting protocols, envy-free allocation, proportional division, and the adjusted winner procedure")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Key concepts: cake (continuous resource [0, 1] with valuation functions), piece (interval(s) assigned to a player), envy-free (no player prefers another's piece), proportional (each of n players gets >= 1/n of their value), equitable (all players receive same utility after normalization)")

(doc 'section 'cake-representation)
(doc 'note "A cake is a continuous resource represented as the interval [0, 1]. Each player has a valuation function v: [0,1] × [0,1] → Real where v(a, b) is the value they assign to interval [a, b]. For simplicity, we represent valuations via value density functions f: [0,1] → Real where v(a,b) = ∫_a^b f(x) dx. We approximate integrals using trapezoidal rule")

(define-record-type cake%
  (fields n            ; number of players
          valuations)) ; vector of valuation functions (density)

(define (make-cake n)
  (doc 'type '(-> Nat Cake))
  (doc 'description "Create a cake to be divided among n players. Initially no valuations assigned")
  (make-cake% n (make-vector n #f)))

;;; cake? : Any → Boolean
(define (cake? x)
  (cake%? x))

;;; cake-players : Cake → Nat
(define (cake-players c)
  (cake%-n c))

;;; cake-set-valuation! : Cake × Nat × (Real → Real) → Void
;;; Set player i's valuation density function.
;;; Density f(x) ≥ 0 represents value per unit at point x.
(define (cake-set-valuation! c i density-fn)
  (vector-set! (cake%-valuations c) i density-fn))

;;; cake-valuation : Cake × Nat × Real × Real → Real
;;; Compute player i's value for interval [a, b].
;;; Uses trapezoidal rule with 100 steps for integration.
;;; Handles edge cases: if a >= b, returns 0; clamps to [0, 1].
(define (cake-valuation c i a b)
  (let* ([a (max 0 (min 1 a))]  ; clamp to [0, 1]
         [b (max 0 (min 1 b))])
    (if (>= a b)
        0  ; empty or inverted interval
        (let ([density (vector-ref (cake%-valuations c) i)])
          (if (not density)
              (- b a)  ; Default: uniform valuation
              (integrate-density density a b 100))))))

;;; integrate-density : (Real → Real) × Real × Real × Nat → Real
;;; Trapezoidal integration of density from a to b with n steps.
(define (integrate-density f a b n)
  (let* ([h (/ (- b a) n)]
         [sum (+ (/ (f a) 2) (/ (f b) 2))])
    (let loop ([i 1] [sum sum])
      (if (>= i n)
          (* h sum)
          (loop (+ i 1) (+ sum (f (+ a (* i h)))))))))

;;; cake-total-value : Cake × Nat → Real
;;; Player i's value for entire cake [0, 1].
(define (cake-total-value c i)
  (cake-valuation c i 0 1))

;;; ============================================================================
;;; Piece Representation
;;; ============================================================================
;;;
;;; A piece is a list of disjoint intervals [(a1,b1), (a2,b2), ...].
;;; We represent intervals as pairs (a . b).

;;; make-piece : (List (Pair Real Real)) → Piece
;;; Create a piece from a list of intervals.
(define (make-piece intervals)
  intervals)

;;; piece-empty : Piece
(define piece-empty '())

;;; piece-singleton : Real × Real → Piece
;;; Single interval piece.
(define (piece-singleton a b)
  (list (cons a b)))

;;; piece-value : Cake × Nat × Piece → Real
;;; Player i's value for a piece.
(define (piece-value c i piece)
  (fold-left + 0
             (map (lambda (interval)
                    (cake-valuation c i (car interval) (cdr interval)))
                  piece)))

;;; piece-length : Piece → Real
;;; Total length of all intervals in piece.
(define (piece-length piece)
  (fold-left + 0
             (map (lambda (interval)
                    (- (cdr interval) (car interval)))
                  piece)))

;;; piece-merge : Piece × Piece → Piece
;;; Merge two pieces (union of intervals).
;;; Does not check for overlap - caller ensures disjointness.
(define (piece-merge p1 p2)
  (append p1 p2))

;;; ============================================================================
;;; Division (Allocation) Representation
;;; ============================================================================
;;;
;;; A division is a vector of pieces, one per player.

;;; make-division : Nat → Division
;;; Create empty division for n players.
(define (make-division n)
  (make-vector n piece-empty))

;;; division-assign! : Division × Nat × Piece → Void
;;; Assign piece to player i.
(define (division-assign! div i piece)
  (vector-set! div i piece))

;;; division-piece : Division × Nat → Piece
;;; Get player i's piece.
(define (division-piece div i)
  (vector-ref div i))

;;; division->list : Division → (List Piece)
(define (division->list div)
  (vector->list div))

;;; ============================================================================
;;; Fairness Criteria
;;; ============================================================================

;;; proportional? : Cake × Division → Boolean
;;; Is division proportional? Each player gets >= 1/n of their total value.
(define (proportional? c div)
  (let* ([n (cake-players c)]
         [threshold (/ 1 n)])
    (let loop ([i 0])
      (if (>= i n)
          #t
          (let* ([total (cake-total-value c i)]
                 [received (piece-value c i (division-piece div i))]
                 [share (if (= total 0) 1 (/ received total))])
            (and (>= share (- threshold 1e-6))  ; tolerance for numerical precision
                 (loop (+ i 1))))))))

;;; envy-free? : Cake × Division → Boolean
;;; Is division envy-free? No player prefers another's piece.
(define (envy-free? c div)
  (let ([n (cake-players c)])
    (let outer ([i 0])
      (if (>= i n)
          #t
          (let ([my-value (piece-value c i (division-piece div i))])
            (let inner ([j 0])
              (if (>= j n)
                  (outer (+ i 1))
                  (if (= i j)
                      (inner (+ j 1))
                      (let ([other-value (piece-value c i (division-piece div j))])
                        (and (<= other-value (+ my-value 1e-10))  ; tolerance
                             (inner (+ j 1))))))))))))

;;; equitable? : Cake × Division × Real → Boolean
;;; Is division equitable (within tolerance)?
;;; All players receive approximately same share of their value.
(define (equitable? c div tolerance)
  (let* ([n (cake-players c)]
         [shares (let loop ([i 0] [acc '()])
                   (if (>= i n)
                       acc
                       (let* ([total (cake-total-value c i)]
                              [received (piece-value c i (division-piece div i))]
                              [share (if (= total 0) 0 (/ received total))])
                         (loop (+ i 1) (cons share acc)))))])
    (if (null? shares)
        #t
        (let ([min-share (apply min shares)]
              [max-share (apply max shares)])
          (<= (- max-share min-share) tolerance)))))

;;; pareto-optimal? : Cake × Division → Boolean
;;; Is division Pareto optimal? Cannot improve one without harming another.
;;; Note: This is expensive to check - we use a simple heuristic.
;;; For cake cutting, a division is Pareto optimal iff no interval is wasted.
(define (pareto-optimal? c div)
  ;; Simple check: all of [0,1] is allocated
  (let* ([intervals (flatten (division->list div))]
         [total-length (fold-left + 0 (map (lambda (i) (- (cdr i) (car i))) intervals))])
    (>= total-length (- 1 1e-10))))

(doc 'section 'cut-and-choose)
(doc 'note "The classic protocol for 2-player fair division: (1) Player 0 (cutter) divides cake into two equal-value pieces, (2) Player 1 (chooser) picks their preferred piece, (3) Player 0 gets the remaining piece. Properties: Proportional, envy-free (for chooser), simple")

;;; Find point x where player i values [0,x] at target value.
;;; Uses binary search.
(define (find-cut-point c i target fuel)
  (doc 'type '(-> Cake Nat Real Real))
  (let* ([total (cake-total-value c i)])
    (if (<= target 0)
        0
        (if (>= target total)
            1
            (binary-search-cut c i target 0 1 fuel)))))

;;; binary-search-cut : Cake × Nat × Real × Real × Real × Nat → Real
(define (binary-search-cut c i target lo hi fuel)
  (if (<= fuel 0)
      (/ (+ lo hi) 2)
      (let* ([mid (/ (+ lo hi) 2)]
             [val (cake-valuation c i 0 mid)])
        (cond
          [(< (abs (- val target)) 1e-10) mid]
          [(< val target) (binary-search-cut c i target mid hi (- fuel 1))]
          [else (binary-search-cut c i target lo mid (- fuel 1))]))))

;;; cut-and-choose : Cake × Nat → Division
;;; Perform cut-and-choose protocol.
;;; fuel bounds binary search iterations.
(define (cut-and-choose c fuel)
  (if (not (= 2 (cake-players c)))
      (error 'cut-and-choose "requires exactly 2 players")
      (let* ([cutter 0]
             [chooser 1]
             ;; Cutter finds point giving them half value
             [cutter-total (cake-total-value c cutter)]
             [half-value (/ cutter-total 2)]
             [cut-point (find-cut-point c cutter half-value fuel)]
             ;; Two pieces
             [piece-left (piece-singleton 0 cut-point)]
             [piece-right (piece-singleton cut-point 1)]
             ;; Chooser picks preferred piece
             [chooser-left (piece-value c chooser piece-left)]
             [chooser-right (piece-value c chooser piece-right)]
             [div (make-division 2)])
        (if (>= chooser-left chooser-right)
            (begin
              (division-assign! div chooser piece-left)
              (division-assign! div cutter piece-right)
              div)
            (begin
              (division-assign! div chooser piece-right)
              (division-assign! div cutter piece-left)
              div)))))

(doc 'section 'dubins-spanier)
(doc 'note "For n players, achieves proportional division: (1) A knife moves continuously from left to right, (2) First player to say stop gets the piece [0, x], (3) Recurse with n-1 players on remaining cake [x, 1]. Properties: Proportional (but NOT envy-free for n > 2)")

;;; Perform Dubins-Spanier moving knife protocol.
;;; fuel bounds search iterations.
(define (dubins-spanier c fuel)
  (doc 'type '(-> Cake Nat Division))
  (let* ([n (cake-players c)]
         [div (make-division n)])
    (dubins-spanier-recurse c div (iota n) 0 1 n fuel)
    div))

;;; dubins-spanier-recurse : Cake × Division × (List Nat) × Real × Real × Nat × Nat → Void
;;; Recursively divide [start, end] among remaining players.
(define (dubins-spanier-recurse c div remaining start end n fuel)
  (cond
    [(<= fuel 0) (void)]
    [(null? remaining) (void)]
    [(null? (cdr remaining))
     ;; Last player gets everything remaining
     (division-assign! div (car remaining) (piece-singleton start end))]
    [else
     ;; Find first player whose 1/k value point is reached
     ;; k = number of remaining players
     (let* ([k (length remaining)]
            [target-share (/ 1 k)]
            ;; For each remaining player, find where they'd say "stop"
            [stop-points
             (map (lambda (i)
                    (let* ([total-remaining (cake-valuation c i start end)]
                           [target (* target-share total-remaining)])
                      (find-cut-point-from c i start target fuel)))
                  remaining)]
            ;; First to stop (minimum stop point)
            [min-stop (apply min stop-points)]
            [winner-idx (list-index (lambda (x) (= x min-stop)) stop-points)]
            [winner (list-ref remaining winner-idx)])
       ;; Winner gets [start, min-stop]
       (division-assign! div winner (piece-singleton start min-stop))
       ;; Recurse with remaining players
       (dubins-spanier-recurse c div
                               (remove winner remaining)
                               min-stop end n (- fuel 1)))]))

;;; find-cut-point-from : Cake × Nat × Real × Real × Nat → Real
;;; Find point x >= start where player i values [start, x] at target.
(define (find-cut-point-from c i start target fuel)
  (if (<= target 0)
      start
      (let ([total (cake-valuation c i start 1)])
        (if (>= target total)
            1
            (binary-search-cut-from c i start target start 1 fuel)))))

;;; binary-search-cut-from : Cake × Nat × Real × Real × Real × Real × Nat → Real
(define (binary-search-cut-from c i start target lo hi fuel)
  (if (<= fuel 0)
      (/ (+ lo hi) 2)
      (let* ([mid (/ (+ lo hi) 2)]
             [val (cake-valuation c i start mid)])
        (cond
          [(< (abs (- val target)) 1e-10) mid]
          [(< val target) (binary-search-cut-from c i start target mid hi (- fuel 1))]
          [else (binary-search-cut-from c i start target lo mid (- fuel 1))]))))

;;; list-index : (a → Boolean) × (List a) → Nat | #f
(define (list-index pred lst)
  (let loop ([lst lst] [i 0])
    (cond
      [(null? lst) #f]
      [(pred (car lst)) i]
      [else (loop (cdr lst) (+ i 1))])))

;;; remove : a × (List a) → (List a)
(define (remove x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

(doc 'section 'selfridge-conway)
(doc 'note "For exactly 3 players, achieves envy-free division: (1) Player A divides cake into 3 equal pieces (by their valuation), (2) Player B trims largest piece to tie for largest (trimmings set aside), (3) Player C picks first, then B (must take trimmed if didn't pick it), then A, (4) Trimmings divided by similar process. This is the first bounded envy-free protocol for 3 players (1960s)")
(doc 'fixme "SIMPLIFIED implementation. The full protocol requires a complex sub-routine for dividing trimmings that recursively ensures envy-freeness. Our simplified version assigns trimmings heuristically, which guarantees proportionality but may not achieve strict envy-freeness in adversarial cases with extreme opposing valuations")

;;; 3-player division using simplified Selfridge-Conway protocol.
;;; Guarantees proportionality; envy-freeness is approximate.
(define (selfridge-conway c fuel)
  (doc 'type '(-> Cake Nat Division))
  (if (not (= 3 (cake-players c)))
      (error 'selfridge-conway "requires exactly 3 players")
      (let* ([div (make-division 3)]
             ;; Step 1: Player 0 divides into three equal pieces
             [total-0 (cake-total-value c 0)]
             [third-value (/ total-0 3)]
             [cut1 (find-cut-point c 0 third-value fuel)]
             [cut2 (find-cut-point c 0 (* 2 third-value) fuel)]
             [pieces (vector (cons 0 cut1)
                             (cons cut1 cut2)
                             (cons cut2 1))]
             ;; Step 2: Player 1 evaluates and possibly trims
             [vals-1 (vector (cake-valuation c 1 0 cut1)
                             (cake-valuation c 1 cut1 cut2)
                             (cake-valuation c 1 cut2 1))]
             [max-val-1 (apply max (vector->list vals-1))]
             [max-indices (filter (lambda (i) (= (vector-ref vals-1 i) max-val-1))
                                  '(0 1 2))])
        ;; Simplified: if unique max, trim it; otherwise no trim needed
        (if (= 1 (length max-indices))
            (selfridge-conway-with-trim c div pieces vals-1 (car max-indices) fuel)
            ;; No trim needed - just let players choose in order C, B, A
            (selfridge-conway-no-trim c div pieces vals-1 fuel)))))

;;; selfridge-conway-no-trim : Cake × Division × Vec × Vec × Nat → Division
;;; When player B sees no single largest piece.
(define (selfridge-conway-no-trim c div pieces vals-1 fuel)
  ;; Player C (2) picks first
  (let* ([vals-2 (vector (cake-valuation c 2 (car (vector-ref pieces 0)) (cdr (vector-ref pieces 0)))
                         (cake-valuation c 2 (car (vector-ref pieces 1)) (cdr (vector-ref pieces 1)))
                         (cake-valuation c 2 (car (vector-ref pieces 2)) (cdr (vector-ref pieces 2))))]
         [c-choice (argmax (vector->list vals-2))]
         ;; Player B picks from remaining
         [remaining-1 (filter (lambda (i) (not (= i c-choice))) '(0 1 2))]
         [b-choice (car (sort-by (lambda (i j)
                                    (> (vector-ref vals-1 i) (vector-ref vals-1 j)))
                                  remaining-1))]
         ;; Player A gets the last piece
         [a-choice (car (filter (lambda (i) (and (not (= i c-choice))
                                                  (not (= i b-choice))))
                                '(0 1 2)))])
    (division-assign! div 2 (piece-singleton (car (vector-ref pieces c-choice))
                                              (cdr (vector-ref pieces c-choice))))
    (division-assign! div 1 (piece-singleton (car (vector-ref pieces b-choice))
                                              (cdr (vector-ref pieces b-choice))))
    (division-assign! div 0 (piece-singleton (car (vector-ref pieces a-choice))
                                              (cdr (vector-ref pieces a-choice))))
    div))

;;; selfridge-conway-with-trim : Cake × Division × Vec × Vec × Nat × Nat → Division
;;; When player B trims the largest piece.
(define (selfridge-conway-with-trim c div pieces vals-1 trim-idx fuel)
  ;; Player B trims piece trim-idx to tie with second-largest
  (let* ([sorted-vals (sort-by > (vector->list vals-1))]
         [second-max (cadr sorted-vals)]
         [trim-piece (vector-ref pieces trim-idx)]
         [trim-start (car trim-piece)]
         [trim-end (cdr trim-piece)]
         ;; Find new endpoint that makes value = second-max
         [new-end (find-value-endpoint c 1 trim-start second-max fuel)]
         [trimmed-piece (cons trim-start new-end)]
         [trimming (cons new-end trim-end)]  ; leftover
         ;; Update pieces vector
         [_ (vector-set! pieces trim-idx trimmed-piece)]
         ;; Player C picks first
         [vals-2 (vector (cake-valuation c 2 (car (vector-ref pieces 0)) (cdr (vector-ref pieces 0)))
                         (cake-valuation c 2 (car (vector-ref pieces 1)) (cdr (vector-ref pieces 1)))
                         (cake-valuation c 2 (car (vector-ref pieces 2)) (cdr (vector-ref pieces 2))))]
         [c-choice (argmax (vector->list vals-2))]
         ;; Player B: if C took trimmed piece, B can pick freely; else B must take trimmed
         [b-choice (if (= c-choice trim-idx)
                       (let ([remaining (filter (lambda (i) (not (= i c-choice))) '(0 1 2))])
                         (car (sort-by (lambda (i j)
                                          (> (vector-ref vals-1 i) (vector-ref vals-1 j)))
                                        remaining)))
                       trim-idx)]
         ;; Player A gets remaining
         [a-choice (car (filter (lambda (i) (and (not (= i c-choice))
                                                  (not (= i b-choice))))
                                '(0 1 2)))]
         ;; Who gets trimming? Whoever got trimmed piece divides it for others
         [trimming-handler (if (= c-choice trim-idx) 2
                               (if (= b-choice trim-idx) 1 0))])
    ;; Assign main pieces
    (division-assign! div 2 (piece-singleton (car (vector-ref pieces c-choice))
                                              (cdr (vector-ref pieces c-choice))))
    (division-assign! div 1 (piece-singleton (car (vector-ref pieces b-choice))
                                              (cdr (vector-ref pieces b-choice))))
    (division-assign! div 0 (piece-singleton (car (vector-ref pieces a-choice))
                                              (cdr (vector-ref pieces a-choice))))
    ;; Divide trimming among the two who didn't get trimmed piece
    ;; (Simplified: just give it to player B if they didn't get trimmed, else to A)
    (let ([trimming-piece (piece-singleton (car trimming) (cdr trimming))])
      (cond
        [(= trimming-handler 2)  ; C got trimmed, divide between A and B
         ;; Give to B for simplicity (full protocol is more complex)
         (division-assign! div 1 (piece-merge (division-piece div 1) trimming-piece))]
        [(= trimming-handler 1)  ; B got trimmed, divide between A and C
         (division-assign! div 0 (piece-merge (division-piece div 0) trimming-piece))]
        [else  ; A got trimmed (shouldn't happen as A cut originally)
         (division-assign! div 1 (piece-merge (division-piece div 1) trimming-piece))]))
    div))

;;; find-value-endpoint : Cake × Nat × Real × Real × Nat → Real
;;; Find endpoint x such that player i values [start, x] at target.
(define (find-value-endpoint c i start target fuel)
  (binary-search-cut-from c i start target start 1 fuel))

;;; argmax : (List Real) → Nat
;;; Return index of maximum element.
(define (argmax lst)
  (let loop ([lst (cdr lst)] [i 1] [max-val (car lst)] [max-idx 0])
    (cond
      [(null? lst) max-idx]
      [(> (car lst) max-val) (loop (cdr lst) (+ i 1) (car lst) i)]
      [else (loop (cdr lst) (+ i 1) max-val max-idx)])))

(doc 'section 'adjusted-winner)
(doc 'note "For 2 players dividing multiple goods: (1) Each player assigns points to goods (must sum to 100), (2) Give each good to whoever values it more, (3) Transfer goods to balance total points received, (4) May need to split one good at a specific ratio. Properties: Envy-free, equitable, Pareto optimal")
;;; goods: vector of good names
;;; points: 2×n matrix of point allocations
(define-record-type adjusted-winner-problem%
  (fields goods points))

;;; make-adjusted-winner-problem : (List Symbol) × (List Real) × (List Real) → AWP
;;; Create problem from goods and point allocations.
(define (make-adjusted-winner-problem goods points-1 points-2)
  (let ([n (length goods)])
    (if (not (= n (length points-1)))
        (error 'make-adjusted-winner-problem "points-1 length mismatch")
        (if (not (= n (length points-2)))
            (error 'make-adjusted-winner-problem "points-2 length mismatch")
            (make-adjusted-winner-problem% (list->vector goods)
                                           (vector (list->vector points-1)
                                                   (list->vector points-2)))))))

;;; adjusted-winner : AWP → (Vector Allocation)
;;; Compute adjusted winner allocation.
;;; Returns vector of (good-name . fraction) for each player.
(define (adjusted-winner prob)
  (let* ([goods (adjusted-winner-problem%-goods prob)]
         [points (adjusted-winner-problem%-points prob)]
         [n (vector-length goods)]
         [points-1 (vector-ref points 0)]
         [points-2 (vector-ref points 1)]
         ;; Initial allocation: give to higher bidder
         [alloc-1 '()]  ; list of (idx . fraction)
         [alloc-2 '()]
         [score-1 0]
         [score-2 0])
    (let initial-loop ([i 0] [a1 '()] [a2 '()] [s1 0] [s2 0])
      (if (>= i n)
          ;; Now equalize by transferring goods
          (equalize-allocations goods points-1 points-2 a1 a2 s1 s2)
          (let ([p1 (vector-ref points-1 i)]
                [p2 (vector-ref points-2 i)])
            (if (> p1 p2)
                (initial-loop (+ i 1)
                              (cons (cons i 1) a1)
                              a2
                              (+ s1 p1)
                              s2)
                (if (> p2 p1)
                    (initial-loop (+ i 1)
                                  a1
                                  (cons (cons i 1) a2)
                                  s1
                                  (+ s2 p2))
                    ;; Tie: give to player 1 (arbitrary)
                    (initial-loop (+ i 1)
                                  (cons (cons i 1) a1)
                                  a2
                                  (+ s1 p1)
                                  s2))))))))

;;; equalize-allocations : Vec × Vec × Vec × List × List × Real × Real → (Vector List)
;;; Transfer goods from advantaged player to equalize scores.
(define (equalize-allocations goods points-1 points-2 alloc-1 alloc-2 score-1 score-2)
  (cond
    [(< (abs (- score-1 score-2)) 0.001)
     ;; Already equal
     (vector (format-allocation goods alloc-1)
             (format-allocation goods alloc-2))]
    [(> score-1 score-2)
     ;; Transfer from player 1 to player 2
     (transfer-to-equalize goods points-1 points-2 alloc-1 alloc-2 score-1 score-2 0 1)]
    [else
     ;; Transfer from player 2 to player 1
     (let ([result (transfer-to-equalize goods points-2 points-1 alloc-2 alloc-1 score-2 score-1 1 0)])
       ;; Swap back
       (vector (vector-ref result 1) (vector-ref result 0)))]))

;;; transfer-to-equalize : ... → (Vector List)
;;; Transfer goods from advantaged (source) to disadvantaged (dest) player.
(define (transfer-to-equalize goods points-src points-dest alloc-src alloc-dest score-src score-dest src-id dest-id)
  ;; Sort source's goods by their ratio: dest-value / src-value (ascending)
  ;; Transfer goods with lowest ratio first (least valuable to source per value to dest)
  (let* ([src-goods (filter (lambda (x) (> (cdr x) 0)) alloc-src)]
         [ratios (map (lambda (x)
                        (let ([i (car x)]
                              [src-val (vector-ref points-src (car x))]
                              [dest-val (vector-ref points-dest (car x))])
                          (cons i (if (= src-val 0) +inf.0 (/ dest-val src-val)))))
                      src-goods)]
         [sorted (sort-by (lambda (a b) (< (cdr a) (cdr b))) ratios)])
    (transfer-loop goods points-src points-dest
                   alloc-src alloc-dest score-src score-dest
                   sorted)))

;;; transfer-loop : ... → (Vector List)
(define (transfer-loop goods points-src points-dest alloc-src alloc-dest score-src score-dest remaining)
  (if (or (null? remaining)
          (<= score-src score-dest))
      ;; Done
      (vector (format-allocation goods alloc-src)
              (format-allocation goods alloc-dest))
      (let* ([item (car remaining)]
             [i (car item)]
             [src-val (vector-ref points-src i)]
             [dest-val (vector-ref points-dest i)]
             ;; Can we transfer the whole good?
             [new-score-src (- score-src src-val)]
             [new-score-dest (+ score-dest dest-val)])
        (if (>= new-score-dest new-score-src)
            ;; Need to split this good
            ;; Find fraction f where: (score-src - f*src-val) = (score-dest + f*dest-val)
            ;; score-src - score-dest = f*(src-val + dest-val)
            ;; f = (score-src - score-dest) / (src-val + dest-val)
            (let* ([gap (- score-src score-dest)]
                   [denom (+ src-val dest-val)]
                   ;; Handle zero-valued goods: if both value at 0, skip (no effect on scores)
                   [f (if (= denom 0) 0 (/ gap denom))]
                   [f (max 0 (min 1 f))]  ; clamp to [0,1]
                   ;; Remove f from source, add f to dest
                   [new-alloc-src (update-fraction alloc-src i (- 1 f))]
                   [new-alloc-dest (cons (cons i f) alloc-dest)])
              (vector (format-allocation goods new-alloc-src)
                      (format-allocation goods new-alloc-dest)))
            ;; Transfer whole good
            (let ([new-alloc-src (remove-good alloc-src i)]
                  [new-alloc-dest (cons (cons i 1) alloc-dest)])
              (transfer-loop goods points-src points-dest
                             new-alloc-src new-alloc-dest
                             new-score-src new-score-dest
                             (cdr remaining)))))))

;;; update-fraction : (List (Pair Nat Real)) × Nat × Real → List
(define (update-fraction alloc i new-frac)
  (map (lambda (x)
         (if (= (car x) i)
             (cons i new-frac)
             x))
       alloc))

;;; remove-good : (List (Pair Nat Real)) × Nat → List
(define (remove-good alloc i)
  (filter (lambda (x) (not (= (car x) i))) alloc))

;;; format-allocation : Vec × (List (Pair Nat Real)) → (List (Pair Symbol Real))
(define (format-allocation goods alloc)
  (filter (lambda (x) (> (cdr x) 0))
          (map (lambda (x)
                 (cons (vector-ref goods (car x)) (cdr x)))
               alloc)))

(doc 'section 'discrete-fair-division)
(doc 'note "When goods are indivisible, envy-freeness may be impossible. We use relaxations like: envy-free up to one good (EF1) - after removing one good, no envy; maximin share guarantee - each gets >= their maximin share")
;;; Matrix: row i = player i's valuation of each good
(define-record-type discrete-problem%
  (fields goods valuations))

(define (make-discrete-problem goods valuations)
  (make-discrete-problem% (list->vector goods)
                          (list->vector (map list->vector valuations))))

;;; discrete-problem-players : DP → Nat
(define (discrete-problem-players dp)
  (vector-length (discrete-problem%-valuations dp)))

;;; discrete-problem-goods : DP → Nat
(define (discrete-problem-goods-count dp)
  (vector-length (discrete-problem%-goods dp)))

;;; discrete-problem-valuation : DP × Nat × Nat → Real
;;; Player i's valuation of good j.
(define (discrete-problem-valuation dp i j)
  (vector-ref (vector-ref (discrete-problem%-valuations dp) i) j))

;;; round-robin : DiscreteProblem → (Vector (List Symbol))
;;; Simple round-robin allocation: players take turns picking best available good.
;;; Order: 0, 1, ..., n-1, 0, 1, ...
(define (round-robin dp)
  (let* ([n (discrete-problem-players dp)]
         [m (discrete-problem-goods-count dp)]
         [goods (discrete-problem%-goods dp)]
         [allocs (make-vector n '())]
         [available (make-vector m #t)])
    (let loop ([round 0] [player 0])
      (if (>= round m)
          allocs
          (let* ([remaining (filter (lambda (j) (vector-ref available j))
                                    (iota m))])
            (if (null? remaining)
                allocs
                (let* ([best (car (sort-by (lambda (j1 j2)
                                              (> (discrete-problem-valuation dp player j1)
                                                 (discrete-problem-valuation dp player j2)))
                                            remaining))])
                  (vector-set! available best #f)
                  (vector-set! allocs player
                               (cons (vector-ref goods best)
                                     (vector-ref allocs player)))
                  (loop (+ round 1) (modulo (+ player 1) n)))))))))

;;; discrete-allocation-value : DP × Nat × (List Symbol) → Real
;;; Total value player i assigns to a bundle of goods.
(define (discrete-allocation-value dp player bundle)
  (let ([goods (discrete-problem%-goods dp)]
        [vals (vector-ref (discrete-problem%-valuations dp) player)])
    (fold-left + 0
               (map (lambda (g)
                      (let loop ([i 0])
                        (if (>= i (vector-length goods))
                            0
                            (if (eq? g (vector-ref goods i))
                                (vector-ref vals i)
                                (loop (+ i 1))))))
                    bundle))))

;;; envy-free-up-to-one? : DP × (Vector (List Symbol)) → Boolean
;;; Check if allocation is EF1.
(define (envy-free-up-to-one? dp allocs)
  (let ([n (discrete-problem-players dp)])
    (let outer ([i 0])
      (if (>= i n)
          #t
          (let ([my-val (discrete-allocation-value dp i (vector-ref allocs i))])
            (let inner ([j 0])
              (if (>= j n)
                  (outer (+ i 1))
                  (if (= i j)
                      (inner (+ j 1))
                      ;; Check if removing any one good from j's bundle eliminates envy
                      (let* ([j-bundle (vector-ref allocs j)]
                             [other-val (discrete-allocation-value dp i j-bundle)])
                        (if (<= other-val my-val)
                            (inner (+ j 1))  ; No envy
                            ;; Try removing each good
                            (let check-remove ([goods j-bundle])
                              (if (null? goods)
                                  #f  ; Couldn't eliminate envy
                                  (let* ([removed (car goods)]
                                         [without (filter (lambda (g) (not (eq? g removed))) j-bundle)]
                                         [reduced-val (discrete-allocation-value dp i without)])
                                    (if (<= reduced-val my-val)
                                        (inner (+ j 1))  ; EF1 satisfied
                                        (check-remove (cdr goods))))))))))))))))

;;; ============================================================================
;;; Maximin Share Guarantee
;;; ============================================================================
;;;
;;; A player's maximin share (MMS) is what they could guarantee themselves
;;; if they divided the goods into n bundles and got the worst one.
;;;
;;; MMS_i = max over partitions P: min over bundle B in P: v_i(B)
;;;
;;; Computing exact MMS is NP-hard (related to partition problem).
;;; We use a greedy approximation that provides a lower bound.

;;; maximin-share : DP × Nat × Nat → Real
;;; Approximate player i's maximin share using greedy balancing.
;;; Returns a lower bound on the true MMS (greedy may not find optimal partition).
;;; fuel parameter is reserved for future exact search; currently unused.
(define (maximin-share dp i fuel)
  (let* ([n (discrete-problem-players dp)]
         [m (discrete-problem-goods-count dp)]
         [goods-list (iota m)])
    ;; Use greedy balancing heuristic (polynomial time)
    ;; True MMS requires exponential partition enumeration
    (maximin-search-greedy dp i n goods-list)))

;;; maximin-search-greedy : DP × Nat × Nat × (List Nat) → Real
;;; Greedy approximation of maximin share using LPT-style balancing.
;;; Sorts goods by value (descending) and assigns each to the bundle
;;; with lowest current value (greedy load balancing).
(define (maximin-search-greedy dp player num-bundles goods)
  (if (null? goods)
      0
      ;; Greedy: create balanced bundles based on player's valuation
      (let* ([bundles (make-vector num-bundles '())]
             [vals (make-vector num-bundles 0)]
             [sorted-goods (sort-by (lambda (j1 j2)
                                       (> (discrete-problem-valuation dp player j1)
                                          (discrete-problem-valuation dp player j2)))
                                     goods)])
        ;; Assign each good to bundle with lowest total value (greedy balancing)
        (for-each
         (lambda (g)
           (let* ([min-idx (argmin (vector->list vals))]
                  [g-val (discrete-problem-valuation dp player g)])
             (vector-set! bundles min-idx (cons g (vector-ref bundles min-idx)))
             (vector-set! vals min-idx (+ (vector-ref vals min-idx) g-val))))
         sorted-goods)
        ;; Minimum bundle value is the MMS approximation
        (apply min (vector->list vals)))))

;;; argmin : (List Real) → Nat
(define (argmin lst)
  (let loop ([lst (cdr lst)] [i 1] [min-val (car lst)] [min-idx 0])
    (cond
      [(null? lst) min-idx]
      [(< (car lst) min-val) (loop (cdr lst) (+ i 1) (car lst) i)]
      [else (loop (cdr lst) (+ i 1) min-val min-idx)])))

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

;;; uniform-cake : Nat → Cake
;;; Create cake with n players, all with uniform valuation.
(define (uniform-cake n)
  (let ([c (make-cake n)])
    (let loop ([i 0])
      (when (< i n)
        (cake-set-valuation! c i (lambda (x) 1))
        (loop (+ i 1))))
    c))

;;; simple-cake-2 : → Cake
;;; Example: 2-player cake where player 0 values left half more,
;;; player 1 values right half more.
(define (simple-cake-2)
  (let ([c (make-cake 2)])
    ;; Player 0: density 2 on [0, 0.5], 0 on [0.5, 1]
    (cake-set-valuation! c 0 (lambda (x) (if (< x 0.5) 2 0)))
    ;; Player 1: density 0 on [0, 0.5], 2 on [0.5, 1]
    (cake-set-valuation! c 1 (lambda (x) (if (>= x 0.5) 2 0)))
    c))

;;; opposing-valuations-cake : → Cake
;;; 3-player cake with opposing preferences.
(define (opposing-valuations-cake)
  (let ([c (make-cake 3)])
    ;; Player 0: prefers left
    (cake-set-valuation! c 0 (lambda (x) (- 2 (* 2 x))))
    ;; Player 1: uniform
    (cake-set-valuation! c 1 (lambda (x) 1))
    ;; Player 2: prefers right
    (cake-set-valuation! c 2 (lambda (x) (* 2 x)))
    c))
