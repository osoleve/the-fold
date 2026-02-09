;;; lattice/game-theory/voting-games.ss — Voting Power Indices
;;; @module voting-games
;;; @requires voting coop-games

(load "lattice/game-theory/voting.ss")
(load "lattice/game-theory/coop-games.ss")

(doc 'module 'voting-games)
(doc 'description "Bridge between voting theory and cooperative game theory. The key insight: voting rules induce simple games where coalitions are 'winning' if they can determine the election outcome")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key concepts: simple game (v(S) ∈ {0, 1}, monotonic S ⊆ T ⟹ v(S) ≤ v(T)), winning coalition (v(S) = 1, can guarantee preferred candidate wins), power index (measures a voter's ability to influence outcomes), Shapley-Shubik (based on random orderings of voters), Banzhaf (based on counting swing votes)")
(doc 'note "The connection: Given a voting rule R and preference profile P, coalition S is winning if members of S voting together can guarantee any candidate they choose wins (regardless of non-members). For plurality with n voters: S is winning iff |S| > n/2. For other rules: winning conditions depend on the rule's structure")

(doc 'section 'profile-conversion)

(define (profile->majority-game profile)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile CoopGame))
  (doc 'description "Convert a preference profile to a simple majority game. Coalition S is winning iff |S| > n/2 (can control plurality outcome). This is the classical model: each voter has equal weight")
  (let* ([n (profile-voters profile)]
         [quota (+ (quotient n 2) 1)])  ; strict majority
    (make-coop-game
     n
     (lambda (S)
       (if (>= (coalition-size S) quota) 1 0)))))

;;; profile->weighted-voting-game : PreferenceProfile × (List Real) → CoopGame
;;; Convert profile to weighted voting game where each voter has custom weight.
;;; Uses strict majority quota: floor(total/2) + 1 to ensure proper simple game.
;;; Useful for modeling electoral systems with vote weights.
(define (profile->weighted-voting-game profile weights)
  (doc 'export #t)
  (let ([n (profile-voters profile)])
    (if (not (= n (length weights)))
        (error 'profile->weighted-voting-game
               "weights length must match number of voters")
        (let* ([total (apply + weights)]
               [quota (+ (floor (/ total 2)) 1)])  ; strict majority
          (make-weighted-voting-game weights quota)))))

;;; profile->rule-induced-game : PreferenceProfile × (Profile → Candidate) → CoopGame
;;; Most general conversion: coalition S is winning if members can coordinate
;;; to make any candidate win by strategic voting.
;;;
;;; A coalition S "wins" if for every candidate c, there exists a way for
;;; S members to vote such that c wins (S can elect anyone they want).
;;; Currently uses heuristic: coalition ranks target first (works for monotonic rules).
;;;
;;; Limitations:
;;;   - Assumes voting rule is monotonic (ranking candidate higher never hurts)
;;;   - May not capture all manipulation strategies for complex rules like STV
;;;   - Exponential in number of voters (2^n coalitions checked)
;;;
;;; Use profile->majority-game for most practical applications.
(define (profile->rule-induced-game profile voting-rule)
  (doc 'export #t)
  (let* ([n (profile-voters profile)]
         [candidates (profile-candidates profile)])
    (make-coop-game
     n
     (lambda (S)
       (if (= S 0)
           0
           ;; Can coalition S make any candidate win?
           (let ([s-members (coalition->list S)])
             (if (coalition-can-elect-any? profile voting-rule s-members candidates)
                 1
                 0)))))))

;;; coalition-can-elect-any? : Profile × Rule × (List Nat) × (List Cand) → Bool
;;; Can coalition members elect any candidate they choose?
(define (coalition-can-elect-any? profile voting-rule coalition-members candidates)
  ;; For the coalition to be "winning", they must be able to elect ANY candidate
  ;; by coordinating their votes (even if other voters vote against them)
  (for-all?
   (lambda (target-candidate)
     ;; Can coalition elect this target?
     (coalition-can-elect-candidate? profile voting-rule coalition-members target-candidate))
   candidates))

;;; coalition-can-elect-candidate? : Profile × Rule × (List Nat) × Cand → Bool
;;; Can coalition coordinate to elect target-candidate?
;;; Heuristic: all coalition members rank target first. Works for monotonic rules.
(define (coalition-can-elect-candidate? profile voting-rule coalition-members target-candidate)
  (let* ([candidates (profile-candidates profile)]
         [others (remove target-candidate candidates)]
         ;; Create a vote with target first
         [strategic-vote (cons target-candidate others)]
         ;; Modify profile: coalition members use strategic vote
         [modified-profile
          (let build ([i 0] [rankings profile] [result '()])
            (if (null? rankings)
                (reverse result)
                (let ([ranking (car rankings)])
                  (build (+ i 1)
                         (cdr rankings)
                         (cons (if (member i coalition-members)
                                   strategic-vote
                                   ranking)
                               result)))))])
    ;; Check if target wins under this configuration
    (eq? target-candidate (voting-rule modified-profile))))

(doc 'section 'power-indices)

(define (shapley-shubik-index profile)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile (Vector Real)))
  (doc 'description "Shapley-Shubik power index for voters based on induced majority game. This measures the probability that a voter is pivotal (turns a losing coalition into a winning one) under random orderings")
  (let* ([game (profile->majority-game profile)]
         [n (profile-voters profile)]
         [fuel (expt 2 n)])  ; Enough fuel for all coalitions
    (shapley-value game fuel)))

;;; shapley-shubik-weighted : PreferenceProfile × (List Real) → (Vector Real)
;;; Shapley-Shubik index with custom voter weights.
(define (shapley-shubik-weighted profile weights)
  (doc 'export #t)
  (let* ([game (profile->weighted-voting-game profile weights)]
         [n (profile-voters profile)]
         [fuel (expt 2 n)])
    (shapley-value game fuel)))

;;; banzhaf-voting-power : PreferenceProfile → (Vector Real)
;;; Normalized Banzhaf power index for voters based on induced majority game.
;;; Returns power distribution that sums to 1 (proportional to pivotal counts).
;;; Note: This is the normalized index, not the absolute probability of being pivotal.
(define (banzhaf-voting-power profile)
  (doc 'export #t)
  (let* ([game (profile->majority-game profile)]
         [n (profile-voters profile)]
         [fuel (* n (expt 2 n))])
    (banzhaf-index game fuel)))

;;; banzhaf-weighted : PreferenceProfile × (List Real) → (Vector Real)
;;; Banzhaf index with custom voter weights.
(define (banzhaf-weighted profile weights)
  (doc 'export #t)
  (let* ([game (profile->weighted-voting-game profile weights)]
         [n (profile-voters profile)]
         [fuel (* n (expt 2 n))])
    (banzhaf-index game fuel)))

(doc 'section 'power-analysis)

(define (voter-is-dictator? profile voter-idx)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile Nat Boolean))
  (doc 'description "Is voter i a dictator? (singleton coalition is winning)")
  (let ([game (profile->majority-game profile)])
    (is-winning? game (coalition-singleton voter-idx))))

;;; voter-is-dummy? : PreferenceProfile × Nat → Boolean
;;; Is voter i a dummy? (never pivotal in any coalition)
;;; Shapley value = 0 for dummy voters.
(define (voter-is-dummy? profile voter-idx)
  (doc 'export #t)
  (let* ([ss-index (shapley-shubik-index profile)])
    (= 0 (vector-ref ss-index voter-idx))))

;;; voter-has-veto? : PreferenceProfile × Nat → Boolean
;;; Does voter i have veto power? (complement of {i} is not winning)
(define (voter-has-veto? profile voter-idx)
  (doc 'export #t)
  (let* ([game (profile->majority-game profile)]
         [n (profile-voters profile)]
         [everyone-but-i (coalition-difference
                          (coop-game-grand-coalition game)
                          (coalition-singleton voter-idx))])
    (not (is-winning? game everyone-but-i))))

;;; minimal-winning-coalitions : PreferenceProfile → (List Coalition)
;;; Find all minimal winning coalitions (removing any member makes it losing).
(define (minimal-winning-coalitions profile)
  (doc 'export #t)
  (let* ([game (profile->majority-game profile)]
         [n (profile-voters profile)]
         [grand (coop-game-grand-coalition game)])
    (filter (lambda (S)
              (and (is-winning? game S)
                   (is-minimal-winning? game S)))
            (all-coalitions n))))

;;; is-minimal-winning? : CoopGame × Coalition → Boolean
;;; Is coalition S minimal winning? (winning but no proper subset is winning)
(define (is-minimal-winning? game S)
  (doc 'export #t)
  (and (is-winning? game S)
       (let ([members (coalition->list S)])
         (for-all? (lambda (i)
                     (not (is-winning? game
                                       (coalition-difference S (coalition-singleton i)))))
                   members))))

;;; ============================================================================
;;; Voting Rule Power Comparison
;;; ============================================================================

;;; voting-power-comparison : PreferenceProfile × (List (Pair Symbol Rule)) → (List Result)
;;; Compare voter power across different voting rules.
;;; Returns list of (rule-name . power-indices).
;;;
;;; Note: For general rules, this uses the rule-induced game which is expensive.
;;; For positional rules (plurality, Borda), the majority game is appropriate.
(define (voting-power-comparison profile rules)
  (map (lambda (rule-pair)
         (let ([name (car rule-pair)]
               [rule (cdr rule-pair)])
           (cons name (banzhaf-voting-power profile))))
       rules))

;;; power-concentration : (Vector Real) → Real
;;; Herfindahl-Hirschman Index of power concentration.
;;; HHI = sum(p_i^2), ranges from 1/n (equal) to 1 (monopoly).
(define (power-concentration power-vec)
  (doc 'export #t)
  (let loop ([i 0] [sum 0])
    (if (>= i (vector-length power-vec))
        sum
        (let ([p (vector-ref power-vec i)])
          (loop (+ i 1) (+ sum (* p p)))))))

;;; power-gini : (Vector Real) → Real
;;; Gini coefficient of power distribution.
;;; 0 = perfect equality, 1 = maximum inequality.
;;; Uses discrete formula: G = (Σ (2i - n - 1) * x_i) / (n * Σ x_i)
;;; where x_i are sorted values and i is 1-indexed.
(define (power-gini power-vec)
  (doc 'export #t)
  (let* ([n (vector-length power-vec)]
         [sorted (sort-by < (vector->list power-vec))]
         [total (apply + sorted)])
    (if (or (= n 0) (= total 0))
        0
        ;; Discrete Gini: sum of (2*i - n - 1) * x_i, where i is 1-indexed
        (let loop ([items sorted] [i 1] [numerator 0])
          (if (null? items)
              (/ numerator (* n total))
              (loop (cdr items)
                    (+ i 1)
                    (+ numerator (* (- (* 2 i) n 1) (car items)))))))))

(doc 'section 'strategic-implications)

(define (decisive-for-candidate profile voting-rule candidate)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile (Profile -> Candidate) Candidate (List Coalition)))
  (doc 'description "Find coalitions that can guarantee candidate c wins by voting strategically")
  (let* ([n (profile-voters profile)]
         [candidates (profile-candidates profile)]
         [others (remove candidate candidates)]
         [strategic-vote (cons candidate others)]
         [grand (- (expt 2 n) 1)])
    (filter
     (lambda (S)
       (let* ([members (coalition->list S)]
              [modified (map-indexed
                         (lambda (ranking i)
                           (if (member i members)
                               strategic-vote
                               ranking))
                         profile)]
              [winner (voting-rule modified)])
         (eq? winner candidate)))
     (map (lambda (i) i) (iota (+ grand 1))))))

;;; map-indexed : (a × Nat → b) × (List a) → (List b)
;;; Map with index.
(define (map-indexed f lst)
  (let loop ([lst lst] [i 0] [acc '()])
    (if (null? lst)
        (reverse acc)
        (loop (cdr lst) (+ i 1) (cons (f (car lst) i) acc)))))

;;; blocking-for-candidate : PreferenceProfile × (Profile → Candidate) × Candidate → (List Coalition)
;;; Find coalitions that can prevent candidate c from winning.
;;; A coalition blocks c if by ranking c last, c cannot win regardless of others.
(define (blocking-for-candidate profile voting-rule candidate)
  (doc 'export #t)
  (let* ([n (profile-voters profile)]
         [candidates (profile-candidates profile)]
         [others (remove candidate candidates)]
         [blocking-vote (append others (list candidate))]  ; c ranked last
         [grand (- (expt 2 n) 1)])
    (filter
     (lambda (S)
       (if (= S 0)
           #f  ; Empty coalition can't block
           (let* ([members (coalition->list S)]
                  ;; All non-members vote c first (worst case for blockers)
                  [non-members (filter (lambda (i) (not (member i members)))
                                       (iota n))]
                  [c-first-vote (cons candidate (remove candidate candidates))]
                  [modified (map-indexed
                             (lambda (ranking i)
                               (cond
                                 [(member i members) blocking-vote]
                                 [(member i non-members) c-first-vote]
                                 [else ranking]))
                             profile)]
                  [winner (voting-rule modified)])
             (not (eq? winner candidate)))))
     (map (lambda (i) i) (iota (+ grand 1))))))

(doc 'section 'electoral-college)
(doc 'note "Model multi-level voting systems like the US Electoral College.")
;;; Each state is a "block" of voters with a weight (electoral votes).

;;; make-electoral-college-game : (List (Pair Symbol Nat)) → CoopGame
;;; Create weighted voting game from state electoral vote allocations.
;;; Input: list of (state-name . electoral-votes)
(define (make-electoral-college-game state-votes)
  (doc 'export #t)
  (let* ([weights (map cdr state-votes)]
         [total (apply + weights)]
         [quota (+ (quotient total 2) 1)])
    (make-weighted-voting-game weights quota)))

;;; electoral-college-power : (List (Pair Symbol Nat)) → (List (Pair Symbol Real))
;;; Compute Banzhaf power for each state in electoral college.
(define (electoral-college-power state-votes)
  (doc 'export #t)
  (let* ([game (make-electoral-college-game state-votes)]
         [n (length state-votes)]
         [fuel (* n (expt 2 n))]
         [power (banzhaf-index game fuel)])
    (map (lambda (state-info i)
           (cons (car state-info) (vector-ref power i)))
         state-votes
         (iota n))))

;;; power-per-capita : (List (Pair Symbol Nat)) × (List (Pair Symbol Nat)) → (List (Pair Symbol Real))
;;; Compute voting power per capita for each state.
;;; First list: electoral votes, second list: populations.
(define (power-per-capita state-votes state-pops)
  (doc 'export #t)
  (let* ([power (electoral-college-power state-votes)]
         [pop-lookup (lambda (name)
                       (let ([found (assoc name state-pops)])
                         (if found (cdr found) 1)))])  ; default 1 to avoid div by 0
    (map (lambda (state-power)
           (let* ([name (car state-power)]
                  [pwr (cdr state-power)]
                  [pop (pop-lookup name)])
             (cons name (/ pwr pop))))
         power)))

;;; ============================================================================
;;; Classic Examples
;;; ============================================================================

;;; us-electoral-college-2020 : → (List (Pair Symbol Nat))
;;; Simplified US Electoral College allocation (selected large states).
(define (us-electoral-college-2020)
  (doc 'export #t)
  '((CA . 55) (TX . 38) (FL . 29) (NY . 29) (PA . 20)
    (IL . 20) (OH . 18) (GA . 16) (MI . 16) (NC . 15)))

;;; un-security-council : → CoopGame
;;; UN Security Council voting: 5 permanent members with veto, 10 temporary.
;;; A resolution passes if at least 9 vote yes AND all P5 vote yes.
(define (un-security-council)
  (doc 'export #t)
  (make-coop-game
   15  ; 5 permanent + 10 temporary
   (lambda (S)
     (let* ([p5-in? (lambda (i) (coalition-member? i S))]
            ;; P5 are players 0-4
            [all-p5-present? (and (p5-in? 0) (p5-in? 1) (p5-in? 2)
                                  (p5-in? 3) (p5-in? 4))]
            [size (coalition-size S)])
       (if (and all-p5-present? (>= size 9))
           1
           0)))))

;;; simple-3-voter-example : → PreferenceProfile
;;; Simple 3-voter, 3-candidate profile for demonstrations.
(define (simple-3-voter-example)
  (doc 'export #t)
  (make-preference-profile
   '((a b c)    ; Voter 0: a > b > c
     (b a c)    ; Voter 1: b > a > c
     (c b a)))) ; Voter 2: c > b > a

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Profile conversion:
;;;   profile->majority-game        ; Standard equal-weight game
;;;   profile->weighted-voting-game ; Custom voter weights
;;;   profile->rule-induced-game    ; General (expensive)
;;;
;;; Power indices:
;;;   shapley-shubik-index          ; Shapley value on majority game
;;;   shapley-shubik-weighted       ; With custom weights
;;;   banzhaf-voting-power          ; Banzhaf on majority game
;;;   banzhaf-weighted              ; With custom weights
;;;
;;; Analysis:
;;;   voter-is-dictator?            ; Singleton is winning
;;;   voter-is-dummy?               ; Zero power
;;;   voter-has-veto?               ; Can block alone
;;;   minimal-winning-coalitions    ; Smallest winning groups
;;;   power-concentration           ; HHI index
;;;   power-gini                    ; Inequality measure
;;;
;;; Strategic:
;;;   decisive-for-candidate        ; Coalitions that can elect c
;;;   blocking-for-candidate        ; Coalitions that can block c
;;;
;;; Electoral systems:
;;;   make-electoral-college-game   ; Weighted voting from state allocations
;;;   electoral-college-power       ; State power indices
;;;   power-per-capita              ; Per-voter power
;;;   us-electoral-college-2020     ; Example data
;;;   un-security-council           ; UNSC voting game
