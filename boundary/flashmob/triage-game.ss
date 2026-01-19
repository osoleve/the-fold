;;; boundary/flashmob/triage-game.ss — Game-Theoretic Triage Strategies
;;;
;;; Implements game-theoretic triage strategies using the full
;;; lattice/fp/game/ toolkit:
;;;
;;; Strategies:
;;;   - game      : PAV + Schulze. Balanced proportionality.
;;;   - game/sav  : SAV + Schulze. Filters noisy/over-approving agents.
;;;   - game/cc   : CC + Schulze. Maximizes coverage (diverse perspectives).
;;;
;;; Components:
;;;   - Selection: PAV, SAV, or CC (from multi-winner.ss)
;;;   - Ranking:   Schulze method (from voting.ss)
;;;   - Power:     Banzhaf index (expertise-weighted)
;;;   - Attribution: Shapley value (N≤15 exact, N>15 sampling)
;;;   - Consensus: Core stability measure
;;;   - Metrics:   Proportionality score, representation coverage
;;;
;;; Complexity:
;;;   - Shapley: O(2^N) exact, O(M*N²) sampling (M=1000 default)
;;;   - PAV/SAV: O(K*N*M) where K=committee size
;;;   - CC:      O(K*N*M)
;;;   - Schulze: O(N³) where N=candidates
;;;
;;; This module delegates to lattice/fp/game/ for core algorithms.
;;;
;;; This is Shell code: impure (agent lookups, lattice imports).

(load "boundary/flashmob/agents.ss")
(load "lattice/fp/game/voting.ss")
(load "lattice/fp/game/multi-winner.ss")
(load "lattice/fp/game/coop-games.ss")
(load "lattice/fp/game/voting-games.ss")

;;; ====
;;; Constants
;;; ====

;;; Maximum agents for full Shapley value computation.
;;; Beyond this, we fall back to proportional attribution.
(define *shapley-agent-limit* 15)

;;; ====
;;; Preference Building (for Schulze)
;;; ====

;;; game-build-preference-profile : (List Symbol) (List Alist) -> PreferenceProfile
;;; Build a preference profile for voting algorithms.
;;; Each agent's ranking becomes a ballot.
(define (game-build-preference-profile agent-ids findings)
  (let ([rankings (map (lambda (agent-id)
                         (let ([ranked (flashmob-agent-rank-findings agent-id findings)])
                           ;; Use finding identity (hash or title) as candidate
                           (map (lambda (pair)
                                  (cons 'finding (cdr (assq 'title (car pair)))))
                                ranked)))
                       agent-ids)])
    ;; make-preference-profile expects candidates as symbols/values
    ;; We'll use finding titles as identifiers
    (let ([candidate-rankings
           (map (lambda (agent-id)
                  (let ([ranked (flashmob-agent-rank-findings agent-id findings)])
                    (map (lambda (pair) (cdr (assq 'title (car pair))))
                         ranked)))
                agent-ids)])
      (if (null? candidate-rankings)
          '()
          candidate-rankings))))

;;; ====
;;; Approval Profile Building (for PAV)
;;; ====

;;; game-build-approval-profile : (List Symbol) (List Alist) -> ApprovalProfile
;;; Build an approval profile for PAV.
;;; Each agent approves findings above a threshold.
(define (game-build-approval-profile agent-ids findings)
  (let ([threshold 0.3])  ; Approve findings with score >= 0.3
    (map (lambda (agent-id)
           (let* ([ranked (flashmob-agent-rank-findings agent-id findings)]
                  [approved (filter (lambda (pair) (>= (cdr pair) threshold)) ranked)])
             (map (lambda (pair) (cdr (assq 'title (car pair))))
                  approved)))
         agent-ids)))

;;; ====
;;; PAV Selection (delegates to lattice/fp/game/multi-winner.ss)
;;; ====

;;; game-pav-select : (List Symbol) (List Alist) Int -> (List Alist)
;;; Select top K findings using PAV (Proportional Approval Voting).
;;; PAV ensures proportional representation across categories.
;;; Delegates to lattice pav-winners for the core algorithm.
(define (game-pav-select agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      '()
      (let* ([approval-profile (game-build-approval-profile agent-ids findings)]
             ;; Use lattice PAV implementation
             [winners (pav-winners approval-profile k)]
             ;; Map back to full finding data
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        (filter-map (lambda (title)
                      (let ([pair (assoc title title->finding)])
                        (if pair (cdr pair) #f)))
                    winners))))

;;; ====
;;; SAV Selection (delegates to lattice/fp/game/multi-winner.ss)
;;; ====

;;; game-sav-select : (List Symbol) (List Alist) Int -> (List Alist)
;;; Select top K findings using SAV (Satisfaction Approval Voting).
;;; SAV normalizes by approval set size, filtering noisy/spammy agents.
;;; Agents who approve everything have diluted influence.
;;; Delegates to lattice sav-winners for the core algorithm.
(define (game-sav-select agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      '()
      (let* ([approval-profile (game-build-approval-profile agent-ids findings)]
             ;; Use lattice SAV implementation
             [winners (sav-winners approval-profile k)]
             ;; Map back to full finding data
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        (filter-map (lambda (title)
                      (let ([pair (assoc title title->finding)])
                        (if pair (cdr pair) #f)))
                    winners))))

;;; ====
;;; CC Selection (delegates to lattice/fp/game/multi-winner.ss)
;;; ====

;;; game-cc-select : (List Symbol) (List Alist) Int -> (List Alist)
;;; Select top K findings using Chamberlin-Courant method.
;;; CC optimizes for coverage - ensures every agent has a champion finding.
;;; Best for QA where you want diverse perspectives represented.
;;; Delegates to lattice cc-greedy for the core algorithm.
(define (game-cc-select agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      '()
      (let* ([pref-profile (game-build-preference-profile agent-ids findings)]
             ;; Use lattice CC implementation
             [winners (cc-greedy pref-profile k)]
             ;; Map back to full finding data
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        (filter-map (lambda (title)
                      (let ([pair (assoc title title->finding)])
                        (if pair (cdr pair) #f)))
                    winners))))

;;; ====
;;; Schulze Ranking (delegates to lattice/fp/game/voting.ss)
;;; ====

;;; game-schulze-ranking : (List Symbol) (List Alist) -> (List Alist)
;;; Rank findings using Schulze method (beatpath).
;;; Falls back to Borda if insufficient pairwise data.
;;; Delegates to lattice schulze-ranking for the core algorithm.
(define (game-schulze-ranking agent-ids findings)
  (if (or (null? agent-ids) (< (length findings) 2))
      findings
      (let* ([pref-profile (game-build-preference-profile agent-ids findings)]
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        ;; Check if we have enough preferences to use Schulze
        (if (game-sparse-preferences? pref-profile)
            ;; Fall back to Borda for sparse data
            (game-borda-fallback agent-ids findings)
            ;; Use lattice Schulze implementation
            (let ([ranked-titles (schulze-ranking pref-profile)])
              (filter-map (lambda (title)
                           (let ([pair (assoc title title->finding)])
                             (if pair (cdr pair) #f)))
                         ranked-titles))))))

;;; game-sparse-preferences? : PreferenceProfile -> Boolean
;;; Check if preferences are too sparse for Schulze.
;;; Returns #t if < 50% of pairs have clear preferences.
(define (game-sparse-preferences? profile)
  (cond
    [(null? profile) #t]
    [(< (length (car profile)) 2) #t]
    [else
     (let* ([candidates (car profile)]
            [n (length candidates)]
            [total-pairs (* n (- n 1))])
       (if (= total-pairs 0)
           #t
           ;; Use lattice's margin matrix via its internal function
           ;; Just check that we have meaningful preferences
           (let ([clear-prefs (length (filter (lambda (r) (>= (length r) 2)) profile))])
             (< (/ clear-prefs (length profile)) 0.5))))]))

;;; game-borda-fallback : (List Symbol) (List Alist) -> (List Alist)
;;; Borda count fallback for sparse preferences.
;;; Uses lattice borda-scores-all for scoring.
(define (game-borda-fallback agent-ids findings)
  (let* ([pref-profile (game-build-preference-profile agent-ids findings)]
         [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
    (if (null? pref-profile)
        findings
        (let* ([borda-results (borda-scores-all pref-profile)]
               [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) borda-results)]
               [ranked-titles (map car sorted)])
          (filter-map (lambda (title)
                        (let ([pair (assoc title title->finding)])
                          (if pair (cdr pair) #f)))
                      ranked-titles)))))

;;; ====
;;; Banzhaf Power Indices
;;; ====

;;; game-banzhaf-power : (List Symbol) -> Alist
;;; Compute Banzhaf power indices weighted by expertise.
;;; Returns alist of (agent-id . power).
(define (game-banzhaf-power agent-ids)
  (if (null? agent-ids)
      '()
      (let* ([n (length agent-ids)]
             ;; Create expertise-weighted voting game
             [weights (map (lambda (id)
                            ;; Sum expertise across categories
                            (let ([vec (flashmob-agent-expertise-vector id)])
                              (+ (vector-ref vec 0)  ; security
                                 (vector-ref vec 1)  ; performance
                                 (vector-ref vec 2)  ; correctness
                                 (vector-ref vec 3)  ; style
                                 (vector-ref vec 4)))) ; documentation
                          agent-ids)]
             [total-weight (apply + weights)]
             [quota (/ total-weight 2)]  ; Simple majority
             [game (make-weighted-voting-game weights quota)]
             [fuel (* n (expt 2 n))]
             [power-vec (banzhaf-index game fuel)])
        (map (lambda (id i)
               (cons id (vector-ref power-vec i)))
             agent-ids
             (iota n)))))

;;; ====
;;; Shapley Attribution (delegates to lattice/fp/game/coop-games.ss)
;;; ====

;;; *shapley-sample-count* : Int
;;; Number of permutation samples for approximate Shapley (N > 15).
(define *shapley-sample-count* 1000)

;;; game-shapley-credits : (List Symbol) (List Alist) -> Alist
;;; Compute Shapley value attribution.
;;; Uses exact algorithm for N ≤ 15, sampling approximation for N > 15.
;;; Delegates to lattice shapley-value and shapley-value-sample.
(define (game-shapley-credits agent-ids findings)
  (if (null? agent-ids)
      '()
      (let* ([n (length agent-ids)]
             [game (game-create-qa-game agent-ids findings)]
             ;; Use lattice implementation: exact for small N, sample for large N
             [shapley-vec (if (> n *shapley-agent-limit*)
                             (shapley-value-sample game *shapley-sample-count* 0)
                             (let* ([fuel (expt 2 (+ n 1))]
                                    [raw (shapley-value game fuel)]
                                    ;; Normalize exact values to sum to 1
                                    [total (let loop ([i 0] [sum 0])
                                             (if (>= i n) sum
                                                 (loop (+ i 1) (+ sum (vector-ref raw i)))))])
                               (if (= total 0)
                                   (let ([v (make-vector n (/ 1.0 n))])
                                     v)
                                   (let ([v (make-vector n 0)])
                                     (do ([i 0 (+ i 1)])
                                         ((>= i n) v)
                                       (vector-set! v i (/ (vector-ref raw i) total)))))))])
        ;; Convert vector to alist
        (map (lambda (id i)
               (cons id (vector-ref shapley-vec i)))
             agent-ids
             (iota n)))))

;;; game-create-qa-game : (List Symbol) (List Alist) -> CoopGame
;;; Create a cooperative game for QA attribution.
;;; v(S) = total contribution of coalition S to finding quality.
(define (game-create-qa-game agent-ids findings)
  (let ([n (length agent-ids)])
    (make-coop-game
     n
     (lambda (coalition)
       ;; Value = sum of expertise-weighted scores for agents in coalition
       (let loop ([i 0] [total 0])
         (if (>= i n)
             total
             (loop (+ i 1)
                   (if (coalition-member? i coalition)
                       (let* ([agent-id (list-ref agent-ids i)]
                              [ranked (flashmob-agent-rank-findings agent-id findings)]
                              [agent-contrib (apply + (map cdr ranked))])
                         (+ total agent-contrib))
                       total))))))))

;;; ====
;;; Core Stability (Consensus)
;;; ====

;;; game-core-consensus : (List Symbol) (List Alist) -> Real
;;; Measure consensus using core stability concepts.
;;; Returns 0-1 where 1 = perfect stability.
(define (game-core-consensus agent-ids findings)
  (if (or (null? agent-ids) (< (length findings) 2))
      1.0
      ;; Measure stability of the ranking
      ;; Use normalized max excess as instability measure
      (let* ([game (game-create-qa-game agent-ids findings)]
             [n (length agent-ids)]
             ;; Get v(N) - the grand coalition value for scaling
             [grand-coalition (- (expt 2 n) 1)]
             [v-grand (coop-game-value game grand-coalition)]
             [credits (game-shapley-credits agent-ids findings)]
             ;; Scale credits back to match game values
             ;; (normalized credits sum to 1, but v(N) may be large)
             [alloc (let ([v (make-vector n 0)])
                     (for-each (lambda (c i)
                                ;; Multiply by v(N) to restore proper scale
                                (vector-set! v i (* (cdr c) v-grand)))
                              credits (iota n))
                     v)]
             ;; Compute max excess (now properly scaled)
             ;; Use <= to match threshold in game-shapley-credits
             [max-exc (if (<= n *shapley-agent-limit*)
                         (game-max-excess-approx game alloc n)
                         0.0)]
             ;; Normalize excess by v(N) to get 0-1 range
             [norm-exc (if (> v-grand 0) (/ max-exc v-grand) 0)])
        ;; Convert excess to consensus (lower excess = higher consensus)
        (max 0 (- 1.0 (abs norm-exc))))))

;;; game-max-excess-approx : CoopGame Vec Int -> Real
;;; Approximate maximum excess (faster than full core check).
(define (game-max-excess-approx game alloc n)
  (let ([grand (- (expt 2 n) 1)])
    ;; Sample some coalitions
    (let loop ([S 1] [count 0] [max-e 0])
      (if (or (> S grand) (> count 100))
          max-e
          (let ([excess (- (coop-game-value game S)
                          (game-coalition-total alloc S n))])
            (loop (+ S (max 1 (quotient grand 100)))
                  (+ count 1)
                  (max max-e excess)))))))

;;; game-coalition-total : Vec Coalition Int -> Real
;;; Sum of allocation for coalition members.
(define (game-coalition-total alloc S n)
  (let loop ([i 0] [sum 0])
    (if (>= i n)
        sum
        (loop (+ i 1)
              (if (coalition-member? i S)
                  (+ sum (vector-ref alloc i))
                  sum)))))

;;; ====
;;; Main Triage Function
;;; ====

;;; game-triage : (List Symbol) (List Alist) Int -> Alist
;;; Run the game-theoretic triage strategy.
;;;
;;; Arguments:
;;;   agent-ids - List of participating agent IDs
;;;   findings  - List of finding data alists
;;;   k         - Number of top findings to select (0 = all)
;;;
;;; Returns:
;;;   ((strategy . game)
;;;    (ranking . <schulze-sorted findings>)
;;;    (selected . <pav-selected findings>)
;;;    (consensus . <core stability score>)
;;;    (credits . <shapley attribution>)
;;;    (power . <banzhaf indices>)
;;;    (notes . <any warnings>))
(define (game-triage agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      `((strategy . game)
        (ranking . ())
        (selected . ())
        (consensus . 1.0)
        (credits . ())
        (power . ())
        (notes . ()))
      (let* ([n (length agent-ids)]
             [notes (if (> n *shapley-agent-limit*)
                       `((shapley-sampling . "Using sampled Shapley (>15 agents)"))
                       '())]
             [ranking (game-schulze-ranking agent-ids findings)]
             [selected (if (= k 0)
                          ranking
                          (game-pav-select agent-ids findings k))]
             [consensus (game-core-consensus agent-ids findings)]
             [credits (game-shapley-credits agent-ids findings)]
             [power (game-banzhaf-power agent-ids)])
        `((strategy . game)
          (ranking . ,ranking)
          (selected . ,selected)
          (consensus . ,consensus)
          (credits . ,credits)
          (power . ,power)
          (notes . ,notes)))))

;;; ====
;;; SAV Strategy (game/sav)
;;; ====

;;; game-triage-sav : (List Symbol) (List Alist) Int -> Alist
;;; Run game triage with SAV selection (filters noisy agents).
;;; SAV normalizes by approval set size - agents who approve everything
;;; have diluted influence. Best when some agents are over-approving.
;;;
;;; Uses: SAV selection + Schulze ranking + Shapley/Banzhaf metrics
(define (game-triage-sav agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      `((strategy . game/sav)
        (ranking . ())
        (selected . ())
        (consensus . 1.0)
        (credits . ())
        (power . ())
        (proportionality . 0)
        (coverage . 1.0)
        (notes . ()))
      (let* ([n (length agent-ids)]
             [notes (if (> n *shapley-agent-limit*)
                       `((shapley-sampling . "Using sampled Shapley (>15 agents)"))
                       '())]
             [ranking (game-schulze-ranking agent-ids findings)]
             [selected (if (= k 0)
                          ranking
                          (game-sav-select agent-ids findings k))]
             [consensus (game-core-consensus agent-ids findings)]
             [credits (game-shapley-credits agent-ids findings)]
             [power (game-banzhaf-power agent-ids)]
             ;; Additional metrics for enhanced strategies
             [pref-profile (game-build-preference-profile agent-ids findings)]
             [approval-profile (game-build-approval-profile agent-ids findings)]
             [selected-titles (map (lambda (f) (cdr (assq 'title f))) selected)]
             [prop-score (if (null? pref-profile) 0
                            (proportionality-score pref-profile selected-titles))]
             [cov-score (if (null? approval-profile) 1.0
                           (representation-coverage approval-profile selected-titles))])
        `((strategy . game/sav)
          (ranking . ,ranking)
          (selected . ,selected)
          (consensus . ,consensus)
          (credits . ,credits)
          (power . ,power)
          (proportionality . ,prop-score)
          (coverage . ,cov-score)
          (notes . ,notes)))))

;;; ====
;;; CC Strategy (game/cc)
;;; ====

;;; game-triage-cc : (List Symbol) (List Alist) Int -> Alist
;;; Run game triage with Chamberlin-Courant selection (optimizes coverage).
;;; CC ensures every agent has at least one representative finding.
;;; Best for QA where diverse perspectives matter.
;;;
;;; Uses: CC selection + Schulze ranking + Shapley/Banzhaf metrics
(define (game-triage-cc agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      `((strategy . game/cc)
        (ranking . ())
        (selected . ())
        (consensus . 1.0)
        (credits . ())
        (power . ())
        (proportionality . 0)
        (coverage . 1.0)
        (notes . ()))
      (let* ([n (length agent-ids)]
             [notes (if (> n *shapley-agent-limit*)
                       `((shapley-sampling . "Using sampled Shapley (>15 agents)"))
                       '())]
             [ranking (game-schulze-ranking agent-ids findings)]
             [selected (if (= k 0)
                          ranking
                          (game-cc-select agent-ids findings k))]
             [consensus (game-core-consensus agent-ids findings)]
             [credits (game-shapley-credits agent-ids findings)]
             [power (game-banzhaf-power agent-ids)]
             ;; Additional metrics for enhanced strategies
             [pref-profile (game-build-preference-profile agent-ids findings)]
             [approval-profile (game-build-approval-profile agent-ids findings)]
             [selected-titles (map (lambda (f) (cdr (assq 'title f))) selected)]
             [prop-score (if (null? pref-profile) 0
                            (proportionality-score pref-profile selected-titles))]
             [cov-score (if (null? approval-profile) 1.0
                           (representation-coverage approval-profile selected-titles))])
        `((strategy . game/cc)
          (ranking . ,ranking)
          (selected . ,selected)
          (consensus . ,consensus)
          (credits . ,credits)
          (power . ,power)
          (proportionality . ,prop-score)
          (coverage . ,cov-score)
          (notes . ,notes)))))

;;; ====
;;; Utility Functions
;;; ====

;;; filter-map : (a -> b | #f) (List a) -> (List b)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (loop (cdr lst)
                (if result (cons result acc) acc))))))

;;; iota : Int -> (List Int)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; equal-hash : Any -> Int
;;; Hash function for equal? hashtables.
(define (equal-hash x)
  (cond
    [(string? x) (string-hash x)]
    [(symbol? x) (symbol-hash x)]
    [(number? x) (abs (truncate x))]
    [else 0]))
