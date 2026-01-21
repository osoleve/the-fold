;;; lattice/fp/game/coop-games.ss — Cooperative Game Theory
;;; @module coop-games
;;; @requires prelude vec matrix matrix-solvers lp

(load "core/base/prelude.ss")
(load "lattice/linalg/vec.ss")
(load "lattice/linalg/matrix.ss")
(load "lattice/linalg/matrix-solvers.ss")
(load "lattice/optimization/lp.ss")

(doc 'module 'coop-games)
(doc 'description "Cooperative (Coalitional) Game Theory with transferable utility")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Constraint: n ≤ 60 players (fixnum bitmask limit on 64-bit)")

(doc 'note "A cooperative game consists of N players (indexed 0, 1, ... n-1) and a characteristic function v: 2^N → Real where v(S) is the value coalition S can achieve")
(doc 'note "Key concepts: Coalition (subset of players, bitmask representation), Allocation (division of v(N) among players), Imputation (efficient + individually rational allocation), Core (allocations no coalition would deviate from), Shapley value (unique fair allocation satisfying axioms)")

(doc 'section 'coalition-representation)
(doc 'note "Coalitions are represented as integers where bit i = 1 means player i is in the coalition. This gives O(1) membership, union, intersection")
(doc 'note "Examples: Coalition 0 = {} (empty), Coalition 1 = {0}, Coalition 5 = {0, 2} (binary 101), Coalition 7 = {0, 1, 2} (binary 111)")

(define coalition-empty 0)
(doc 'export #t)
(doc coalition-empty 'type 'Coalition)
(doc coalition-empty 'description "The empty coalition (no players)")

(define (coalition-singleton i)
  (doc 'export #t)
  (doc 'type '(-> Nat Coalition))
  (doc 'description "Coalition containing only player i")
  (bitwise-arithmetic-shift-left 1 i))

(define (coalition-member? i S)
  (doc 'export #t)
  (doc 'type '(-> Nat Coalition Boolean))
  (doc 'description "Is player i in coalition S?")
  (= 1 (bitwise-and 1 (bitwise-arithmetic-shift-right S i))))

(define (coalition-union S T)
  (doc 'export #t)
  (doc 'type '(-> Coalition Coalition Coalition))
  (doc 'description "Union of two coalitions")
  (bitwise-ior S T))

(define (coalition-intersection S T)
  (doc 'export #t)
  (doc 'type '(-> Coalition Coalition Coalition))
  (doc 'description "Intersection of two coalitions")
  (bitwise-and S T))

(define (coalition-difference S T)
  (doc 'type '(-> Coalition Coalition Coalition))
  (doc 'description "Set difference S \\ T (players in S but not T)")
  (bitwise-and S (bitwise-not T)))

(define (coalition-complement S n)
  (doc 'export #t)
  (doc 'type '(-> Coalition Nat Coalition))
  (doc 'description "Complement of S w.r.t. grand coalition of n players")
  (bitwise-xor S (- (bitwise-arithmetic-shift-left 1 n) 1)))

(define (coalition-size S)
  (doc 'export #t)
  (doc 'type '(-> Coalition Nat))
  (doc 'description "Number of players in coalition S")
  (bitwise-bit-count S))

(define (coalition->list S)
  (doc 'export #t)
  (doc 'type '(-> Coalition (List Nat)))
  (doc 'description "Convert coalition to list of player indices")
  (let loop ([S S] [i 0] [acc '()])
    (if (= S 0)
        (reverse acc)
        (loop (bitwise-arithmetic-shift-right S 1)
              (+ i 1)
              (if (= 1 (bitwise-and S 1))
                  (cons i acc)
                  acc)))))

(define (list->coalition players)
  (doc 'export #t)
  (doc 'type '(-> (List Nat) Coalition))
  (doc 'description "Convert list of player indices to coalition")
  (fold-left coalition-union coalition-empty
             (map coalition-singleton players)))

(define (all-coalitions n)
  (doc 'export #t)
  (doc 'type '(-> Nat (List Coalition)))
  (doc 'description "Generate all 2^n coalitions for n players")
  (iota (bitwise-arithmetic-shift-left 1 n)))

(define (coalition-subset? S T)
  (doc 'type '(-> Coalition Coalition Boolean))
  (doc 'description "Is S a subset of T?")
  (= S (coalition-intersection S T)))

(doc 'section 'cooperative-game-record-type)
(doc 'note "A cooperative game (TU game) is defined by n (number of players), v (characteristic function Coalition → Real where v({}) = 0), and game-type (:value or :cost)")
(doc 'note "Value games: v(S) = worth S can achieve. Core requires x(S) >= v(S)")
(doc 'note "Cost games: c(S) = cost S must pay. Core requires x(S) <= c(S)")

(define-record-type coop-game%
  (fields n v game-type))

(define (make-coop-game n v . args)
  (doc 'export #t)
  (doc 'type '(-> Nat (-> Coalition Real) [Symbol] CoopGame))
  (doc 'description "Create a cooperative game with n players and characteristic function v")
  (doc 'param 'args "Optional game-type: :value (default) or :cost")
  (let ([game-type (if (null? args) ':value (car args))])
    (unless (memq game-type '(:value :cost))
      (error 'make-coop-game "game-type must be :value or :cost" game-type))
    (make-coop-game% n v game-type)))

(define (coop-game? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Is x a cooperative game?")
  (coop-game%? x))

(define (coop-game-players g)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Nat))
  (doc 'description "Number of players in the game")
  (coop-game%-n g))

(define (coop-game-value g S)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Coalition Real))
  (doc 'description "Get characteristic function value for coalition S")
  ((coop-game%-v g) S))

(define (coop-game-grand-coalition g)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Coalition))
  (doc 'description "The grand coalition N = {0, 1, ..., n-1}")
  (- (bitwise-arithmetic-shift-left 1 (coop-game-players g)) 1))

(define (coop-game-type g)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Symbol))
  (doc 'description "Game type: :value or :cost")
  (coop-game%-game-type g))

(define (coop-game-cost? g)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Boolean))
  (doc 'description "Is this a cost game?")
  (eq? ':cost (coop-game-type g)))

(define (coop-game-value? g)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Boolean))
  (doc 'description "Is this a value game?")
  (eq? ':value (coop-game-type g)))

(doc 'section 'allocations)
(doc 'note "An allocation x is a vector of payoffs, one per player where x[i] = payoff to player i")

(define (allocation-total x)
  (doc 'export #t)
  (doc 'type '(-> (Vector Real) Real))
  (doc 'description "Sum of all payoffs in allocation")
  (let loop ([i 0] [sum 0])
    (if (>= i (vector-length x))
        sum
        (loop (+ i 1) (+ sum (vector-ref x i))))))

(define (allocation-coalition-total x S)
  (doc 'export #t)
  (doc 'type '(-> (Vector Real) Coalition Real))
  (doc 'description "Sum of payoffs for players in coalition S")
  (let loop ([S S] [i 0] [sum 0])
    (if (= S 0)
        sum
        (loop (bitwise-arithmetic-shift-right S 1)
              (+ i 1)
              (if (= 1 (bitwise-and S 1))
                  (+ sum (vector-ref x i))
                  sum)))))

(doc 'section 'imputations)
(doc 'note "An imputation satisfies: 1. Efficiency: sum(x) = v(N), 2. Individual rationality: x[i] >= v({i}) for all i")

(define (imputation? g x)
  (doc 'export #t)
  (doc 'type '(-> CoopGame (Vector Real) Boolean))
  (doc 'description "Is x an imputation for game g?")
  (doc 'note "Value games: x_i >= v({i}). Cost games: x_i <= c({i}).")
  (let ([n (coop-game-players g)]
        [grand (coop-game-grand-coalition g)]
        [is-cost (coop-game-cost? g)])
    (and
     ;; Efficiency: total allocation = v(N) or c(N)
     (= (allocation-total x) (coop-game-value g grand))
     ;; Individual rationality (direction depends on game type)
     (let loop ([i 0])
       (if (>= i n)
           #t
           (let ([x-i (vector-ref x i)]
                 [v-i (coop-game-value g (coalition-singleton i))])
             (and (if is-cost
                      ;; Cost game: player pays at most standalone cost
                      (<= x-i v-i)
                      ;; Value game: player gets at least standalone value
                      (>= x-i v-i))
                  (loop (+ i 1)))))))))

(doc 'section 'shapley-value)
(doc 'note "The Shapley value is the unique allocation satisfying: Efficiency (sum(phi_i) = v(N)), Symmetry (symmetric players get equal payoffs), Null player (player with zero marginal contribution gets zero), Additivity (phi(v + w) = phi(v) + phi(w))")
(doc 'note "Formula (subset-based, O(n * 2^n)): phi_i = SUM over S not containing i of [|S|!(n-|S|-1)!/n!] × [v(S ∪ {i}) - v(S)]")

(define (make-factorial-cache max-n)
  (doc 'type '(-> Nat (Vector Nat)))
  (doc 'description "Compute n! (memoized via vector cache)")
  (let ([cache (make-vector (+ max-n 1) 1)])
    (let loop ([i 1])
      (when (<= i max-n)
        (vector-set! cache i (* i (vector-ref cache (- i 1))))
        (loop (+ i 1))))
    cache))

(define (shapley-weight s n fact-cache)
  (doc 'type '(-> Nat Nat (Vector Nat) Real))
  (doc 'description "Weight for coalition of size s in game with n players: s!(n-s-1)!/n!")
  (/ (* (vector-ref fact-cache s)
        (vector-ref fact-cache (- n s 1)))
     (vector-ref fact-cache n)))

(define (shapley-value g fuel)
  (doc 'export #t)
  (doc 'type '(-> CoopGame Nat (Vector Real)))
  (doc 'description "Compute Shapley value for all players")
  (doc 'param 'fuel "Bounds computation (set to 2^n for exact result)")
  (let* ([n (coop-game-players g)]
         [fact-cache (make-factorial-cache n)]
         [result (make-vector n 0)])
    ;; For each player i
    (let player-loop ([i 0] [fuel fuel])
      (if (or (>= i n) (<= fuel 0))
          result
          (begin
            ;; Sum marginal contributions over all coalitions not containing i
            (let* ([player-bit (coalition-singleton i)]
                   [phi-i
                    (let coalition-loop ([S 0] [sum 0] [fuel fuel])
                      (if (or (>= S (bitwise-arithmetic-shift-left 1 n))
                              (<= fuel 0))
                          sum
                          (if (coalition-member? i S)
                              ;; Skip coalitions containing i
                              (coalition-loop (+ S 1) sum fuel)
                              ;; Compute marginal contribution
                              (let* ([s (coalition-size S)]
                                     [S-with-i (coalition-union S player-bit)]
                                     [marginal (- (coop-game-value g S-with-i)
                                                  (coop-game-value g S))]
                                     [weight (shapley-weight s n fact-cache)])
                                (coalition-loop (+ S 1)
                                                (+ sum (* weight marginal))
                                                (- fuel 1))))))])
              (vector-set! result i phi-i)
              (player-loop (+ i 1) fuel)))))))

;;; ============================================================================
;;; Core Stability
;;; ============================================================================
;;;
;;; The core is the set of imputations where no coalition can do better
;;; by deviating.
;;;
;;; x is in core iff:
;;;   - Efficiency: sum(x) = v(N)
;;;   - Coalition rationality: sum_{i in S}(x_i) >= v(S) for all S

;;; core-excess : CoopGame × (Vector Real) × Coalition → Real
;;; Excess of coalition S at allocation x.
;;; For value games: e(S, x) = v(S) - x(S). Positive = underpaid, would deviate.
;;; For cost games: e(S, x) = x(S) - c(S). Positive = overcharged, would leave.
;;; In both cases: excess <= 0 means coalition is satisfied (core condition).
(define (core-excess g x S)
  (doc 'export #t)
  (doc 'type '(-> CoopGame (Vector Real) Coalition Real))
  (doc 'description "Excess of coalition S at allocation x. Positive means S would deviate.")
  (let ([coalition-total (allocation-coalition-total x S)]
        [v-S (coop-game-value g S)])
    (if (coop-game-cost? g)
        ;; Cost game: overcharged if paying more than standalone cost
        (- coalition-total v-S)
        ;; Value game: underpaid if getting less than standalone value
        (- v-S coalition-total))))

;;; allocation-in-core? : CoopGame × (Vector Real) → Boolean
;;; Is allocation x in the core of game g?
(define (allocation-in-core? g x)
  (doc 'export #t)
  (let ([n (coop-game-players g)]
        [grand (coop-game-grand-coalition g)])
    (and
     ;; Efficiency: total = v(N)
     (= (allocation-total x) (coop-game-value g grand))
     ;; Coalition rationality: all excesses <= 0
     (let loop ([S 0])
       (if (> S grand)
           #t
           (and (<= (core-excess g x S) 0)
                (loop (+ S 1))))))))

;;; max-excess : CoopGame × (Vector Real) → Real
;;; Maximum excess across all coalitions (for core optimization).
(define (max-excess g x)
  (let ([n (coop-game-players g)]
        [grand (coop-game-grand-coalition g)])
    (let loop ([S 1] [max-e (core-excess g x 1)])
      (if (> S grand)
          max-e
          (loop (+ S 1) (max max-e (core-excess g x S)))))))

;;; ============================================================================
;;; Nucleolus (via Linear Programming)
;;; ============================================================================
;;;
;;; The nucleolus lexicographically minimizes the vector of excesses.
;;; At each iteration:
;;;   1. Minimize maximum excess ε over unfixed coalitions
;;;   2. Fix coalitions that achieved ε
;;;   3. Repeat until allocation is determined
;;;
;;; LP formulation at each stage:
;;;   minimize ε
;;;   subject to:
;;;     - sum(x) = v(N)                     (efficiency)
;;;     - x_i >= v({i})                     (individual rationality)
;;;     - v(S) - sum_{i∈S}(x_i) <= ε        (unfixed coalitions)
;;;     - v(S) - sum_{i∈S}(x_i) = ε_k       (fixed coalitions)

;;; nucleolus : CoopGame × Nat → (Vector Real) | Symbol
;;; Compute the nucleolus using iterated LP.
;;; fuel bounds the number of LP iterations.
(define (nucleolus g fuel)
  (doc 'export #t)
  (let* ([n (coop-game-players g)]
         [grand (coop-game-grand-coalition g)]
         [v-grand (coop-game-value g grand)]
         ;; All non-trivial coalitions (excluding empty and grand)
         [coalitions (filter (lambda (S) (and (> S 0) (< S grand)))
                             (all-coalitions n))]
         [num-coalitions (length coalitions)]
         [coalition-vec (list->vector coalitions)])
    (nucleolus-iterate g n v-grand coalition-vec
                       '()      ; fixed-constraints: list of (coalition . excess)
                       fuel)))

;;; nucleolus-iterate : CoopGame × Nat × Real × Vec × List × Nat → Vec | Symbol
;;; Single iteration of nucleolus algorithm.
(define (nucleolus-iterate g n v-grand coalition-vec fixed-constraints fuel)
  (if (<= fuel 0)
      '(out-of-fuel nucleolus)
      (let* ([num-coalitions (vector-length coalition-vec)]
             ;; Build LP: minimize ε subject to constraints
             ;; Variables: x_0, ..., x_{n-1}, ε
             ;; Index n is ε
             [lp-result (build-nucleolus-lp g n v-grand coalition-vec fixed-constraints)])
        (if (not lp-result)
            ;; No unfixed coalitions, extract allocation from fixed constraints
            (extract-nucleolus-allocation g n v-grand fixed-constraints)
            (let ([lp (car lp-result)]
                  [unfixed-coalitions (cdr lp-result)])
              (let ([result (lp-solve lp)])
                (cond
                  [(lp-infeasible? result)
                   '(error infeasible-nucleolus)]
                  [(lp-unbounded? result)
                   '(error unbounded-nucleolus)]
                  [else
                   (let* ([x-full (lp-result-x result)]
                          [epsilon (vector-ref x-full n)]
                          ;; Extract allocation (first n variables)
                          [alloc (let ([v (make-vector n 0)])
                                   (do ([i 0 (+ i 1)])
                                       [(= i n) v]
                                     (vector-set! v i (vector-ref x-full i))))]
                          ;; Find coalitions that achieved this epsilon (tight constraints)
                          [newly-fixed (find-tight-coalitions g alloc epsilon unfixed-coalitions)])
                     (if (null? newly-fixed)
                         ;; No new tight constraints, we're done
                         alloc
                         ;; Add to fixed constraints and iterate
                         (let ([new-fixed (append (map (lambda (S) (cons S epsilon)) newly-fixed)
                                                  fixed-constraints)]
                               [remaining (list->vector
                                           (filter (lambda (S) (not (member S newly-fixed)))
                                                   (vector->list unfixed-coalitions)))])
                           (if (= 0 (vector-length remaining))
                               alloc  ; All fixed
                               (nucleolus-iterate g n v-grand remaining new-fixed (- fuel 1))))))])))))))

;;; build-nucleolus-lp : CoopGame × Nat × Real × Vec × List → (LP . Vec) | #f
;;; Build LP for one nucleolus iteration.
;;; Returns (lp . unfixed-coalitions) or #f if no unfixed coalitions.
(define (build-nucleolus-lp g n v-grand coalition-vec fixed-constraints)
  (let ([num-unfixed (vector-length coalition-vec)])
    (if (= num-unfixed 0)
        #f  ; No unfixed coalitions
        (let* ([num-fixed (length fixed-constraints)]
               ;; Variables: x_0, ..., x_{n-1}, ε
               ;; Number of variables: n + 1
               [num-vars (+ n 1)]
               ;; Constraints:
               ;;   1 efficiency constraint (equality)
               ;;   n individual rationality (inequality, convert to equality with slack)
               ;;   num-unfixed excess <= ε (inequality)
               ;;   num-fixed excess = ε_k (equality)
               ;;
               ;; Convert to standard form Ax = b with slacks:
               ;;   - IR: x_i + slack_i = v({i}) for x_i >= v({i}) means x_i - slack = v({i}), slack >= 0
               ;;     Actually x_i >= v({i}) means we need -x_i <= -v({i})
               ;;   - Excess: v(S) - sum(x_i) <= ε means -sum(x_i) - ε <= -v(S), add slack
               ;;
               ;; Better: use variables x_0..x_{n-1}, ε, and slack variables
               ;; Total variables: n + 1 + n (IR slack) + num-unfixed (excess slack)
               [num-slacks (+ n num-unfixed)]
               [total-vars (+ num-vars num-slacks)]
               ;; Constraints:
               ;;   1 efficiency: sum(x) = v(N)
               ;;   n IR: x_i - slack_i = v({i}) where slack >= 0, so x_i >= v({i})... wait this is wrong
               ;;   Actually for x_i >= v({i}), we want x_i = v({i}) + surplus where surplus >= 0
               ;;   So x_i - surplus_i = v({i})
               ;;   And for v(S) - sum(x) <= ε:
               ;;     -sum_{i∈S}(x_i) - ε + slack_S = -v(S)
               ;;     i.e., v(S) - sum(x) + slack = ε
               [num-constraints (+ 1 n num-unfixed num-fixed)]
               ;; Build cost vector: minimize ε (index n), others 0
               [c (make-vector total-vars 0)])
          (vector-set! c n 1)  ; minimize ε
          ;; Build constraint matrix and RHS
          (let ([A (make-matrix num-constraints total-vars 0)]
                [b (make-vector num-constraints 0)]
                [row 0])
            ;; Constraint 1: Efficiency sum(x) = v(N)
            (do ([i 0 (+ i 1)])
                [(= i n)]
              (matrix-set! A row i 1))
            (vector-set! b row v-grand)
            (set! row (+ row 1))
            ;; Constraints 2 to n+1: Individual rationality x_i - surplus_i = v({i})
            ;; Surplus variable at index (n + 1 + i)
            (do ([i 0 (+ i 1)])
                [(= i n)]
              (matrix-set! A row i 1)  ; x_i coefficient
              (matrix-set! A row (+ n 1 i) -1)  ; surplus_i coefficient
              (vector-set! b row (coop-game-value g (coalition-singleton i)))
              (set! row (+ row 1)))
            ;; Constraints for unfixed coalitions: v(S) - sum(x) - ε + slack = 0
            ;; i.e., -sum_{i∈S}(x_i) - ε + slack_S = -v(S)
            (do ([k 0 (+ k 1)])
                [(= k num-unfixed)]
              (let ([S (vector-ref coalition-vec k)]
                    [slack-idx (+ n 1 n k)])  ; slack index
                ;; -sum_{i∈S}(x_i)
                (do ([i 0 (+ i 1)])
                    [(= i n)]
                  (when (coalition-member? i S)
                    (matrix-set! A row i -1)))
                ;; -ε
                (matrix-set! A row n -1)
                ;; +slack
                (matrix-set! A row slack-idx 1)
                ;; = -v(S)
                (vector-set! b row (- (coop-game-value g S)))
                (set! row (+ row 1))))
            ;; Constraints for fixed coalitions: v(S) - sum(x) = ε_k
            ;; i.e., -sum_{i∈S}(x_i) = ε_k - v(S)
            (for-each
             (lambda (fc)
               (let ([S (car fc)]
                     [eps-k (cdr fc)])
                 (do ([i 0 (+ i 1)])
                     [(= i n)]
                   (when (coalition-member? i S)
                     (matrix-set! A row i -1)))
                 (vector-set! b row (- eps-k (coop-game-value g S)))
                 (set! row (+ row 1))))
             fixed-constraints)
            ;; Return LP and unfixed coalitions
            (cons (make-lp c A b) coalition-vec))))))

;;; find-tight-coalitions : CoopGame × Vec × Real × Vec → List
;;; Find coalitions where excess equals epsilon (tight constraints).
(define (find-tight-coalitions g alloc epsilon coalition-vec)
  (let ([tolerance 1e-8])
    (filter (lambda (S)
              (let ([excess (core-excess g alloc S)])
                (< (abs (- excess epsilon)) tolerance)))
            (vector->list coalition-vec))))

;;; extract-nucleolus-allocation : CoopGame × Nat × Real × List → Vec
;;; When all coalitions are fixed, solve for the allocation.
;;; Uses least-squares on the system: efficiency + fixed coalition constraints.
(define (extract-nucleolus-allocation g n v-grand fixed-constraints)
  ;; Build system of equations from fixed constraints plus efficiency:
  ;;   - Efficiency: sum(x) = v(N)
  ;;   - For each (S, eps) in fixed-constraints: sum_{i in S}(x_i) = v(S) - eps
  ;;
  ;; This is typically overdetermined (more equations than n variables).
  ;; Use least-squares to find best-fit solution.
  (let* ([num-constraints (+ 1 (length fixed-constraints))]
         [A (make-matrix num-constraints n 0)]
         [b (make-vector num-constraints 0)])
    ;; Row 0: Efficiency constraint
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (matrix-set! A 0 i 1))
    (vector-set! b 0 v-grand)
    ;; Remaining rows: Fixed coalition constraints
    (let loop ([constraints fixed-constraints]
               [row 1])
      (if (null? constraints)
          ;; Solve the overdetermined system
          (let ([result (matrix-least-squares A b)])
            (if (pair? result)
                ;; Error case - fall back to Shapley
                (shapley-value g 10000)
                result))
          (let* ([constraint (car constraints)]
                 [S (car constraint)]
                 [eps (cdr constraint)]
                 [v-S (coop-game-value g S)])
            ;; sum_{i in S}(x_i) = v(S) - eps
            (do ([i 0 (+ i 1)])
                [(= i n)]
              (when (coalition-member? S i)
                (matrix-set! A row i 1)))
            (vector-set! b row (- v-S eps))
            (loop (cdr constraints) (+ row 1)))))))

;;; ============================================================================
;;; Bargaining Solutions (2-player)
;;; ============================================================================
;;;
;;; For 2-player cooperative games, bargaining theory provides solutions
;;; without requiring LP.

;;; bargaining-set : CoopGame → (Vector Real Real Real Real)
;;; Compute bargaining set bounds for 2-player game.
;;; Returns #(d0 d1 u0 u1) where:
;;;   d = disagreement point (individual values)
;;;   u = ideal point (best each can get cooperatively)
(define (bargaining-set g)
  (if (not (= 2 (coop-game-players g)))
      (error 'bargaining-set "requires 2-player game")
      (let* ([v0 (coop-game-value g 1)]   ; v({0})
             [v1 (coop-game-value g 2)]   ; v({1})
             [v01 (coop-game-value g 3)]  ; v({0,1})
             [u0 (- v01 v1)]  ; max player 0 can get
             [u1 (- v01 v0)]) ; max player 1 can get
        (vector v0 v1 u0 u1))))

;;; nash-bargaining : CoopGame → (Vector Real)
;;; Nash bargaining solution: maximize product of gains over disagreement.
;;; For symmetric surplus, each player gets: d_i + (v(N) - d_0 - d_1) / 2
(define (nash-bargaining g)
  (doc 'export #t)
  (if (not (= 2 (coop-game-players g)))
      (error 'nash-bargaining "requires 2-player game")
      (let* ([bs (bargaining-set g)]
             [d0 (vector-ref bs 0)]
             [d1 (vector-ref bs 1)]
             [v01 (coop-game-value g 3)]
             [surplus (- v01 d0 d1)])
        (vector (+ d0 (/ surplus 2))
                (+ d1 (/ surplus 2))))))

;;; kalai-smorodinsky : CoopGame → (Vector Real)
;;; Kalai-Smorodinsky solution: allocate proportionally to ideal gains.
(define (kalai-smorodinsky g)
  (doc 'export #t)
  (if (not (= 2 (coop-game-players g)))
      (error 'kalai-smorodinsky "requires 2-player game")
      (let* ([bs (bargaining-set g)]
             [d0 (vector-ref bs 0)]
             [d1 (vector-ref bs 1)]
             [u0 (vector-ref bs 2)]
             [u1 (vector-ref bs 3)]
             [v01 (coop-game-value g 3)]
             [surplus (- v01 d0 d1)]
             [gain0 (- u0 d0)]
             [gain1 (- u1 d1)]
             [total-gain (+ gain0 gain1)])
        (if (= total-gain 0)
            ;; Both players have same individual and cooperative value
            (vector d0 d1)
            (let ([ratio (/ gain0 total-gain)])
              (vector (+ d0 (* ratio surplus))
                      (+ d1 (* (- 1 ratio) surplus))))))))

;;; ============================================================================
;;; Classic Game Constructors
;;; ============================================================================

;;; make-additive-game : (List Real) → CoopGame
;;; Additive game: v(S) = sum of individual values.
;;; Core is singleton (Shapley = individual values).
(define (make-additive-game values)
  (doc 'export #t)
  (let* ([n (length values)]
         [v-vec (list->vector values)])
    (make-coop-game
     n
     (lambda (S)
       (allocation-coalition-total v-vec S)))))

;;; make-unanimity-game : Nat × Coalition → CoopGame
;;; Unanimity game u_T: v(S) = 1 if T ⊆ S, else 0.
;;; These form a basis for all cooperative games.
(define (make-unanimity-game n T)
  (doc 'export #t)
  (make-coop-game
   n
   (lambda (S)
     (if (coalition-subset? T S) 1 0))))

;;; make-weighted-voting-game : (List Real) × Real → CoopGame
;;; Weighted voting game: coalition wins if sum of weights >= quota.
;;; v(S) = 1 if winning, 0 otherwise.
(define (make-weighted-voting-game weights quota)
  (doc 'export #t)
  (let* ([n (length weights)]
         [w-vec (list->vector weights)])
    (make-coop-game
     n
     (lambda (S)
       (if (>= (allocation-coalition-total w-vec S) quota) 1 0)))))

;;; make-airport-game : (List Real) → CoopGame
;;; Airport game: players have runway cost requirements c_i.
;;; c(S) = max_{i in S}(c_i) (cost that coalition S must pay).
;;; This is a COST game: core requires x(S) <= c(S).
;;; Shapley gives fair cost allocation proportional to marginal contribution.
(define (make-airport-game costs)
  (doc 'export #t)
  (doc 'type '(-> (List Real) CoopGame))
  (doc 'description "Airport cost game: players share runway cost based on max need")
  (let* ([n (length costs)]
         [c-vec (list->vector costs)])
    (make-coop-game
     n
     (lambda (S)
       (if (= S 0)
           0
           (let ([members (coalition->list S)])
             (apply max (map (lambda (i) (vector-ref c-vec i)) members)))))
     ':cost)))

;;; make-bankruptcy-game : Real × (List Real) → CoopGame
;;; Bankruptcy game: estate E divided among creditors with claims c_i.
;;; v(S) = max(0, E - sum_{j not in S}(c_j))
(define (make-bankruptcy-game estate claims)
  (doc 'export #t)
  (let* ([n (length claims)]
         [c-vec (list->vector claims)]
         [total-claims (apply + claims)])
    (make-coop-game
     n
     (lambda (S)
       (let ([excluded-claims (- total-claims
                                 (allocation-coalition-total c-vec S))])
         (max 0 (- estate excluded-claims)))))))

;;; make-gloves-game : Nat × Nat → CoopGame
;;; Gloves game: L left-glove holders, R right-glove holders.
;;; v(S) = min(left gloves in S, right gloves in S)
;;; Players 0..L-1 have left gloves, L..L+R-1 have right gloves.
(define (make-gloves-game L R)
  (doc 'export #t)
  (let ([n (+ L R)])
    (make-coop-game
     n
     (lambda (S)
       (let ([left-count
              (let loop ([i 0] [count 0])
                (if (>= i L)
                    count
                    (loop (+ i 1)
                          (if (coalition-member? i S) (+ count 1) count))))]
             [right-count
              (let loop ([i L] [count 0])
                (if (>= i n)
                    count
                    (loop (+ i 1)
                          (if (coalition-member? i S) (+ count 1) count))))])
         (min left-count right-count))))))

;;; ============================================================================
;;; Game Properties
;;; ============================================================================

;;; coop-game-superadditive? : CoopGame × Nat → Boolean
;;; Check if v(S ∪ T) >= v(S) + v(T) for disjoint S, T.
(define (coop-game-superadditive? g fuel)
  (doc 'export #t)
  (let* ([n (coop-game-players g)]
         [grand (coop-game-grand-coalition g)])
    (let outer ([S 0] [fuel fuel])
      (if (or (> S grand) (<= fuel 0))
          #t
          (let inner ([T 0] [fuel fuel])
            (if (or (> T grand) (<= fuel 0))
                (outer (+ S 1) fuel)
                (if (= 0 (coalition-intersection S T))
                    ;; S and T are disjoint
                    (let ([union-val (coop-game-value g (coalition-union S T))]
                          [sum-val (+ (coop-game-value g S) (coop-game-value g T))])
                      (if (>= union-val sum-val)
                          (inner (+ T 1) (- fuel 1))
                          #f))
                    (inner (+ T 1) fuel))))))))

;;; coop-game-convex? : CoopGame × Nat → Boolean
;;; Check if v(S ∪ T) + v(S ∩ T) >= v(S) + v(T) for all S, T.
;;; Convex games always have non-empty core.
(define (coop-game-convex? g fuel)
  (doc 'export #t)
  (let* ([n (coop-game-players g)]
         [grand (coop-game-grand-coalition g)])
    (let outer ([S 0] [fuel fuel])
      (if (or (> S grand) (<= fuel 0))
          #t
          (let inner ([T 0] [fuel fuel])
            (if (or (> T grand) (<= fuel 0))
                (outer (+ S 1) fuel)
                (let ([lhs (+ (coop-game-value g (coalition-union S T))
                              (coop-game-value g (coalition-intersection S T)))]
                      [rhs (+ (coop-game-value g S) (coop-game-value g T))])
                  (if (>= lhs rhs)
                      (inner (+ T 1) (- fuel 1))
                      #f))))))))

;;; coop-game-monotonic? : CoopGame × Nat → Boolean
;;; Check if S ⊆ T implies v(S) <= v(T).
(define (coop-game-monotonic? g fuel)
  (doc 'export #t)
  (let* ([n (coop-game-players g)]
         [grand (coop-game-grand-coalition g)])
    (let outer ([S 0] [fuel fuel])
      (if (or (> S grand) (<= fuel 0))
          #t
          (let inner ([T 0] [fuel fuel])
            (if (or (> T grand) (<= fuel 0))
                (outer (+ S 1) fuel)
                (if (coalition-subset? S T)
                    (if (<= (coop-game-value g S) (coop-game-value g T))
                        (inner (+ T 1) (- fuel 1))
                        #f)
                    (inner (+ T 1) fuel))))))))

;;; coop-game-simple? : CoopGame → Boolean
;;; Check if game values are only 0 or 1 (voting game).
(define (coop-game-simple? g)
  (doc 'export #t)
  (let ([grand (coop-game-grand-coalition g)])
    (let loop ([S 0])
      (if (> S grand)
          #t
          (let ([val (coop-game-value g S)])
            (if (or (= val 0) (= val 1))
                (loop (+ S 1))
                #f))))))

;;; ============================================================================
;;; Power Indices (Voting Games)
;;; ============================================================================

;;; is-winning? : CoopGame × Coalition → Boolean
;;; Is coalition S winning? (for simple games)
(define (is-winning? g S)
  (doc 'export #t)
  (= 1 (coop-game-value g S)))

;;; is-blocking? : CoopGame × Coalition → Boolean
;;; Is coalition S blocking? (complement is not winning)
(define (is-blocking? g S)
  (doc 'export #t)
  (let ([n (coop-game-players g)])
    (not (is-winning? g (coalition-complement S n)))))

;;; is-pivotal? : CoopGame × Coalition × Nat → Boolean
;;; Is player i pivotal in coalition S?
;;; (S is winning but S \ {i} is not)
(define (is-pivotal? g S i)
  (doc 'export #t)
  (and (coalition-member? i S)
       (is-winning? g S)
       (not (is-winning? g (coalition-difference S (coalition-singleton i))))))

;;; banzhaf-index : CoopGame × Nat → (Vector Real)
;;; Banzhaf power index: probability of being pivotal.
;;; (normalized so index sums to 1)
(define (banzhaf-index g fuel)
  (doc 'export #t)
  (let* ([n (coop-game-players g)]
         [grand (coop-game-grand-coalition g)]
         [raw-scores (make-vector n 0)])
    ;; Count pivotal coalitions for each player
    (let outer ([i 0] [fuel fuel])
      (if (or (>= i n) (<= fuel 0))
          ;; Normalize
          (let ([total (let sum-loop ([j 0] [s 0])
                         (if (>= j n)
                             s
                             (sum-loop (+ j 1) (+ s (vector-ref raw-scores j)))))])
            (if (= total 0)
                raw-scores
                (let norm-loop ([j 0])
                  (if (>= j n)
                      raw-scores
                      (begin
                        (vector-set! raw-scores j (/ (vector-ref raw-scores j) total))
                        (norm-loop (+ j 1)))))))
          (begin
            ;; Count pivotal coalitions for player i
            (let inner ([S 0] [count 0] [fuel fuel])
              (if (or (> S grand) (<= fuel 0))
                  (vector-set! raw-scores i count)
                  (inner (+ S 1)
                         (if (is-pivotal? g S i) (+ count 1) count)
                         (- fuel 1))))
            (outer (+ i 1) fuel))))))

;;; ============================================================================
;;; Shapley Value Approximation (Sampling)
;;; ============================================================================
;;;
;;; For large N, exact Shapley computation is intractable (O(n*2^n)).
;;; This section provides sampling-based approximation (Castro et al., 2009).

;;; random-permutation : Nat → (Vector Nat)
;;; Generate a random permutation of 0..n-1 using Fisher-Yates shuffle.
(define (random-permutation n)
  (let ([v (make-vector n)])
    ;; Initialize with identity
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! v i i))
    ;; Fisher-Yates shuffle
    (do ([i (- n 1) (- i 1)])
        ((< i 1) v)
      (let* ([j (random (+ i 1))]
             [tmp (vector-ref v i)])
        (vector-set! v i (vector-ref v j))
        (vector-set! v j tmp)))))

;;; shapley-value-sample : CoopGame × Nat × Nat → (Vector Real)
;;; Approximate Shapley value using M random permutations.
;;; Returns normalized values that sum to 1 (for attribution).
;;;
;;; Arguments:
;;;   game        - Cooperative game
;;;   num-samples - Number of random permutations to sample
;;;   fuel        - Computation budget (currently unused, for API consistency)
;;;
;;; Algorithm (Castro et al., 2009):
;;;   For each sampled permutation π:
;;;     For each player i at position k in π:
;;;       Compute marginal contribution: v(S∪{i}) - v(S)
;;;       where S = players appearing before i in π
;;;   Average over all samples
;;;
;;; Complexity: O(num-samples × N²) where N = number of players
(define (shapley-value-sample game num-samples fuel)
  (let* ([n (coop-game-players game)]
         [sums (make-vector n 0.0)])
    (if (= n 0)
        sums
        (begin
          ;; Sample random permutations
          (do ([sample 0 (+ sample 1)])
              ((>= sample num-samples))
            (let ([perm (random-permutation n)])
              ;; Compute marginal contributions along this permutation
              (let loop ([i 0] [coalition 0])
                (when (< i n)
                  (let* ([agent-idx (vector-ref perm i)]
                         ;; v(S ∪ {i})
                         [with-agent (bitwise-ior coalition
                                                  (bitwise-arithmetic-shift-left 1 agent-idx))]
                         [v-with (coop-game-value game with-agent)]
                         ;; v(S)
                         [v-without (if (= coalition 0) 0 (coop-game-value game coalition))]
                         ;; Marginal contribution
                         [marginal (- v-with v-without)])
                    (vector-set! sums agent-idx
                                 (+ (vector-ref sums agent-idx) marginal))
                    (loop (+ i 1) with-agent))))))
          ;; Average and normalize to sum to 1
          (let* ([avg-values (let ([v (make-vector n 0.0)])
                               (do ([i 0 (+ i 1)])
                                   ((>= i n) v)
                                 (vector-set! v i (/ (vector-ref sums i) num-samples))))]
                 [total (let loop ([i 0] [sum 0])
                          (if (>= i n)
                              sum
                              (loop (+ i 1) (+ sum (vector-ref avg-values i)))))])
            (if (= total 0)
                ;; Equal split if no contributions
                (let ([equal-share (/ 1.0 n)])
                  (do ([i 0 (+ i 1)])
                      ((>= i n) avg-values)
                    (vector-set! avg-values i equal-share)))
                ;; Normalize to sum to 1
                (begin
                  (do ([i 0 (+ i 1)])
                      ((>= i n) avg-values)
                    (vector-set! avg-values i (/ (vector-ref avg-values i) total))))))))))

;;; shapley-value-adaptive : CoopGame × Nat → (Vector Real)
;;; Automatically choose exact vs sampling based on game size.
;;; Uses exact Shapley for N ≤ 15, sampling for larger games.
(define *shapley-exact-threshold* 15)
(define *shapley-default-samples* 1000)

(define (shapley-value-adaptive game fuel)
  (let ([n (coop-game-players game)])
    (if (<= n *shapley-exact-threshold*)
        (shapley-value game fuel)
        (shapley-value-sample game *shapley-default-samples* fuel))))
