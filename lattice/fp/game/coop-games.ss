;;; lattice/fp/game/coop-games.ss — Cooperative (Coalitional) Game Theory
;;;
;;; Implements cooperative games with transferable utility (TU games).
;;; Supports Shapley value, core stability, bargaining solutions,
;;; and classic game constructors.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; A cooperative game consists of:
;;;   - N players (indexed 0, 1, ... n-1)
;;;   - Characteristic function v: 2^N → Real
;;;     where v(S) is the value coalition S can achieve
;;;
;;; Key concepts:
;;;   - Coalition: subset of players (bitmask representation)
;;;   - Allocation: division of v(N) among players
;;;   - Imputation: efficient + individually rational allocation
;;;   - Core: allocations no coalition would deviate from
;;;   - Shapley value: unique fair allocation satisfying axioms
;;;
;;; Constraint: n ≤ 60 players (fixnum bitmask limit on 64-bit)
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================================
;;; Coalition Representation (Bitmask)
;;; ============================================================================
;;;
;;; Coalitions are represented as integers where bit i = 1 means player i
;;; is in the coalition. This gives O(1) membership, union, intersection.
;;;
;;; Examples:
;;;   Coalition 0 = {} (empty)
;;;   Coalition 1 = {0} (just player 0)
;;;   Coalition 5 = {0, 2} (binary 101)
;;;   Coalition 7 = {0, 1, 2} (binary 111)

;;; coalition-empty : Coalition
;;; The empty coalition (no players).
(define coalition-empty 0)

;;; coalition-singleton : Nat → Coalition
;;; Coalition containing only player i.
(define (coalition-singleton i)
  (bitwise-arithmetic-shift-left 1 i))

;;; coalition-member? : Nat × Coalition → Boolean
;;; Is player i in coalition S?
(define (coalition-member? i S)
  (= 1 (bitwise-and 1 (bitwise-arithmetic-shift-right S i))))

;;; coalition-union : Coalition × Coalition → Coalition
;;; Union of two coalitions.
(define (coalition-union S T)
  (bitwise-ior S T))

;;; coalition-intersection : Coalition × Coalition → Coalition
;;; Intersection of two coalitions.
(define (coalition-intersection S T)
  (bitwise-and S T))

;;; coalition-difference : Coalition × Coalition → Coalition
;;; Set difference S \ T (players in S but not T).
(define (coalition-difference S T)
  (bitwise-and S (bitwise-not T)))

;;; coalition-complement : Coalition × Nat → Coalition
;;; Complement of S w.r.t. grand coalition of n players.
(define (coalition-complement S n)
  (bitwise-xor S (- (bitwise-arithmetic-shift-left 1 n) 1)))

;;; coalition-size : Coalition → Nat
;;; Number of players in coalition S.
(define (coalition-size S)
  (bitwise-bit-count S))

;;; coalition->list : Coalition → (List Nat)
;;; Convert coalition to list of player indices.
(define (coalition->list S)
  (let loop ([S S] [i 0] [acc '()])
    (if (= S 0)
        (reverse acc)
        (loop (bitwise-arithmetic-shift-right S 1)
              (+ i 1)
              (if (= 1 (bitwise-and S 1))
                  (cons i acc)
                  acc)))))

;;; list->coalition : (List Nat) → Coalition
;;; Convert list of player indices to coalition.
(define (list->coalition players)
  (fold-left coalition-union coalition-empty
             (map coalition-singleton players)))

;;; all-coalitions : Nat → (List Coalition)
;;; Generate all 2^n coalitions for n players.
(define (all-coalitions n)
  (iota (bitwise-arithmetic-shift-left 1 n)))

;;; coalition-subset? : Coalition × Coalition → Boolean
;;; Is S a subset of T?
(define (coalition-subset? S T)
  (= S (coalition-intersection S T)))

;;; ============================================================================
;;; Cooperative Game Record Type
;;; ============================================================================

;;; A cooperative game (TU game) is defined by:
;;;   - n: number of players
;;;   - v: characteristic function (Coalition → Real)
;;;        Satisfies v({}) = 0

(define-record-type coop-game%
  (fields n v))

;;; make-coop-game : Nat × (Coalition → Real) → CoopGame
;;; Create a cooperative game with n players and characteristic function v.
(define (make-coop-game n v)
  (make-coop-game% n v))

;;; coop-game? : Any → Boolean
;;; Is x a cooperative game?
(define (coop-game? x)
  (coop-game%? x))

;;; coop-game-players : CoopGame → Nat
;;; Number of players in the game.
(define (coop-game-players g)
  (coop-game%-n g))

;;; coop-game-value : CoopGame × Coalition → Real
;;; Get characteristic function value for coalition S.
(define (coop-game-value g S)
  ((coop-game%-v g) S))

;;; coop-game-grand-coalition : CoopGame → Coalition
;;; The grand coalition N = {0, 1, ..., n-1}.
(define (coop-game-grand-coalition g)
  (- (bitwise-arithmetic-shift-left 1 (coop-game-players g)) 1))

;;; ============================================================================
;;; Allocations
;;; ============================================================================
;;;
;;; An allocation x is a vector of payoffs, one per player.
;;; x[i] = payoff to player i

;;; allocation-total : (Vector Real) → Real
;;; Sum of all payoffs in allocation.
(define (allocation-total x)
  (let loop ([i 0] [sum 0])
    (if (>= i (vector-length x))
        sum
        (loop (+ i 1) (+ sum (vector-ref x i))))))

;;; allocation-coalition-total : (Vector Real) × Coalition → Real
;;; Sum of payoffs for players in coalition S.
(define (allocation-coalition-total x S)
  (let loop ([S S] [i 0] [sum 0])
    (if (= S 0)
        sum
        (loop (bitwise-arithmetic-shift-right S 1)
              (+ i 1)
              (if (= 1 (bitwise-and S 1))
                  (+ sum (vector-ref x i))
                  sum)))))

;;; ============================================================================
;;; Imputations
;;; ============================================================================
;;;
;;; An imputation satisfies:
;;;   1. Efficiency: sum(x) = v(N)
;;;   2. Individual rationality: x[i] >= v({i}) for all i

;;; imputation? : CoopGame × (Vector Real) → Boolean
;;; Is x an imputation for game g?
(define (imputation? g x)
  (let ([n (coop-game-players g)]
        [grand (coop-game-grand-coalition g)])
    (and
     ;; Efficiency: total allocation = v(N)
     (= (allocation-total x) (coop-game-value g grand))
     ;; Individual rationality: each player gets at least v({i})
     (let loop ([i 0])
       (if (>= i n)
           #t
           (and (>= (vector-ref x i)
                    (coop-game-value g (coalition-singleton i)))
                (loop (+ i 1))))))))

;;; ============================================================================
;;; Shapley Value
;;; ============================================================================
;;;
;;; The Shapley value is the unique allocation satisfying:
;;;   - Efficiency: sum(phi_i) = v(N)
;;;   - Symmetry: symmetric players get equal payoffs
;;;   - Null player: player with zero marginal contribution gets zero
;;;   - Additivity: phi(v + w) = phi(v) + phi(w)
;;;
;;; Formula (subset-based, O(n * 2^n)):
;;;   phi_i = SUM over S not containing i of:
;;;           [|S|!(n-|S|-1)!/n!] × [v(S ∪ {i}) - v(S)]

;;; factorial : Nat → Nat
;;; Compute n! (memoized via vector cache).
(define (make-factorial-cache max-n)
  (let ([cache (make-vector (+ max-n 1) 1)])
    (let loop ([i 1])
      (when (<= i max-n)
        (vector-set! cache i (* i (vector-ref cache (- i 1))))
        (loop (+ i 1))))
    cache))

;;; shapley-weight : Nat × Nat × (Vector Nat) → Real
;;; Weight for coalition of size s in game with n players.
;;; = s!(n-s-1)!/n!
(define (shapley-weight s n fact-cache)
  (/ (* (vector-ref fact-cache s)
        (vector-ref fact-cache (- n s 1)))
     (vector-ref fact-cache n)))

;;; shapley-value : CoopGame × Nat → (Vector Real)
;;; Compute Shapley value for all players.
;;; fuel bounds computation (set to 2^n for exact result).
(define (shapley-value g fuel)
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
;;; e(S, x) = v(S) - sum_{i in S}(x_i)
;;; Positive excess = coalition S can do better by deviating.
(define (core-excess g x S)
  (- (coop-game-value g S) (allocation-coalition-total x S)))

;;; allocation-in-core? : CoopGame × (Vector Real) → Boolean
;;; Is allocation x in the core of game g?
(define (allocation-in-core? g x)
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
;;; Nucleolus (Placeholder - requires LP)
;;; ============================================================================
;;;
;;; The nucleolus lexicographically minimizes the maximum excess.
;;; It requires linear programming for exact computation.

;;; nucleolus : CoopGame × Nat → (Vector Real) | Symbol
;;; Compute the nucleolus.
;;; Currently returns placeholder pending LP implementation (fold-c3a).
(define (nucleolus g fuel)
  '(needs-lp nucleolus "Requires fold-c3a simplex implementation"))

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
  (make-coop-game
   n
   (lambda (S)
     (if (coalition-subset? T S) 1 0))))

;;; make-weighted-voting-game : (List Real) × Real → CoopGame
;;; Weighted voting game: coalition wins if sum of weights >= quota.
;;; v(S) = 1 if winning, 0 otherwise.
(define (make-weighted-voting-game weights quota)
  (let* ([n (length weights)]
         [w-vec (list->vector weights)])
    (make-coop-game
     n
     (lambda (S)
       (if (>= (allocation-coalition-total w-vec S) quota) 1 0)))))

;;; make-airport-game : (List Real) → CoopGame
;;; Airport game: players have runway cost requirements c_i.
;;; v(S) = max_{i in S}(c_i) (cost that coalition S must pay).
;;; Note: This is a cost game; Shapley gives fair cost allocation.
(define (make-airport-game costs)
  (let* ([n (length costs)]
         [c-vec (list->vector costs)])
    (make-coop-game
     n
     (lambda (S)
       (if (= S 0)
           0
           (let ([members (coalition->list S)])
             (apply max (map (lambda (i) (vector-ref c-vec i)) members))))))))

;;; make-bankruptcy-game : Real × (List Real) → CoopGame
;;; Bankruptcy game: estate E divided among creditors with claims c_i.
;;; v(S) = max(0, E - sum_{j not in S}(c_j))
(define (make-bankruptcy-game estate claims)
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
  (= 1 (coop-game-value g S)))

;;; is-blocking? : CoopGame × Coalition → Boolean
;;; Is coalition S blocking? (complement is not winning)
(define (is-blocking? g S)
  (let ([n (coop-game-players g)])
    (not (is-winning? g (coalition-complement S n)))))

;;; is-pivotal? : CoopGame × Coalition × Nat → Boolean
;;; Is player i pivotal in coalition S?
;;; (S is winning but S \ {i} is not)
(define (is-pivotal? g S i)
  (and (coalition-member? i S)
       (is-winning? g S)
       (not (is-winning? g (coalition-difference S (coalition-singleton i))))))

;;; banzhaf-index : CoopGame × Nat → (Vector Real)
;;; Banzhaf power index: probability of being pivotal.
;;; (normalized so index sums to 1)
(define (banzhaf-index g fuel)
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
