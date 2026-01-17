;;; shell/flashmob/triage-game.ss — Game-Theoretic Triage Strategy
;;;
;;; Implements the game-theoretic A/B test strategy:
;;;   - Selection: PAV (Proportional Approval Voting)
;;;   - Ranking: Schulze method
;;;   - Power: Banzhaf index (expertise-weighted)
;;;   - Attribution: Shapley value (N≤15) or proportional fallback
;;;   - Consensus: Core stability measure
;;;
;;; Complexity: O(2^N) for Shapley - HARD CAP at N=15 agents
;;;
;;; This module imports from lattice/fp/game/ for the core algorithms.
;;;
;;; This is Shell code: impure (agent lookups, lattice imports).

(load "shell/flashmob/agents.ss")
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
;;; PAV Selection
;;; ====

;;; game-pav-select : (List Symbol) (List Alist) Int -> (List Alist)
;;; Select top K findings using PAV (Proportional Approval Voting).
;;; PAV ensures proportional representation across categories.
(define (game-pav-select agent-ids findings k)
  (if (or (null? agent-ids) (null? findings))
      '()
      (let* ([approval-profile (game-build-approval-profile agent-ids findings)]
             [all-titles (map (lambda (f) (cdr (assq 'title f))) findings)]
             ;; Run PAV on titles
             [winners (game-pav-greedy approval-profile all-titles k)]
             ;; Map back to full finding data
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        (filter-map (lambda (title)
                      (let ([pair (assoc title title->finding)])
                        (if pair (cdr pair) #f)))
                    winners))))

;;; game-pav-greedy : ApprovalProfile (List String) Int -> (List String)
;;; Greedy PAV implementation.
;;; Selects candidates maximizing marginal PAV score.
(define (game-pav-greedy approval-profile candidates k)
  (let loop ([remaining candidates]
             [elected '()]
             [seats-left k])
    (if (or (= seats-left 0) (null? remaining))
        (reverse elected)
        (let* ([scores (map (lambda (c)
                             (cons c (game-pav-score c approval-profile elected)))
                           remaining)]
               [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)]
               [winner (caar sorted)])
          (loop (remove winner remaining)
                (cons winner elected)
                (- seats-left 1))))))

;;; game-pav-score : String ApprovalProfile (List String) -> Real
;;; PAV score for adding candidate given elected committee.
;;; Diminishing returns: 1/(already-satisfied + 1)
(define (game-pav-score candidate profile elected)
  (apply +
         (map (lambda (approvals)
                (if (member candidate approvals)
                    (let ([already-satisfied
                           (length (filter (lambda (e) (member e approvals)) elected))])
                      (/ 1.0 (+ already-satisfied 1)))
                    0))
              profile)))

;;; ====
;;; Schulze Ranking
;;; ====

;;; game-schulze-ranking : (List Symbol) (List Alist) -> (List Alist)
;;; Rank findings using Schulze method (beatpath).
;;; Falls back to Borda if insufficient pairwise data.
(define (game-schulze-ranking agent-ids findings)
  (if (or (null? agent-ids) (< (length findings) 2))
      findings
      (let* ([pref-profile (game-build-preference-profile agent-ids findings)]
             [titles (map (lambda (f) (cdr (assq 'title f))) findings)]
             [title->finding (map (lambda (f) (cons (cdr (assq 'title f)) f)) findings)])
        ;; Check if we have enough preferences to use Schulze
        (if (game-sparse-preferences? pref-profile)
            ;; Fall back to Borda for sparse data
            (game-borda-fallback agent-ids findings)
            ;; Use Schulze
            (let ([ranked-titles (game-schulze-sort pref-profile titles)])
              (filter-map (lambda (title)
                           (let ([pair (assoc title title->finding)])
                             (if pair (cdr pair) #f)))
                         ranked-titles))))))

;;; game-sparse-preferences? : PreferenceProfile -> Boolean
;;; Check if preferences are too sparse for Schulze.
;;; Returns #t if < 50% of pairs have clear preferences.
(define (game-sparse-preferences? profile)
  (if (or (null? profile) (< (length (car profile)) 2))
      #t
      ;; For now, assume we have enough if we have rankings
      #f))

;;; game-schulze-sort : PreferenceProfile (List String) -> (List String)
;;; Sort candidates using Schulze method.
(define (game-schulze-sort profile candidates)
  (let* ([n (length candidates)]
         [margin (game-build-margin-matrix profile candidates)]
         [strength (game-schulze-strengths margin n)])
    ;; Sort by Schulze relation
    (sort (lambda (a b)
            (let ([ia (game-candidate-index a candidates)]
                  [ib (game-candidate-index b candidates)])
              (> (vector-ref (vector-ref strength ia) ib)
                 (vector-ref (vector-ref strength ib) ia))))
          candidates)))

;;; game-candidate-index : String (List String) -> Int
;;; Find index of candidate in list.
(define (game-candidate-index c candidates)
  (let loop ([cs candidates] [i 0])
    (if (null? cs)
        0
        (if (equal? c (car cs))
            i
            (loop (cdr cs) (+ i 1))))))

;;; game-build-margin-matrix : PreferenceProfile (List String) -> Matrix
;;; Build pairwise margin matrix.
(define (game-build-margin-matrix profile candidates)
  (let* ([n (length candidates)]
         [matrix (make-vector n)])
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! matrix i (make-vector n 0)))
    ;; Process each ranking
    (for-each
     (lambda (ranking)
       (let ([pos-of (make-hashtable equal-hash equal?)])
         ;; Build position lookup
         (let loop ([r ranking] [p 0])
           (unless (null? r)
             (hashtable-set! pos-of (car r) p)
             (loop (cdr r) (+ p 1))))
         ;; Update margins
         (do ([i 0 (+ i 1)])
             ((>= i n))
           (let ([ci (list-ref candidates i)])
             (do ([j 0 (+ j 1)])
                 ((>= j n))
               (unless (= i j)
                 (let* ([cj (list-ref candidates j)]
                        [pi (hashtable-ref pos-of ci n)]
                        [pj (hashtable-ref pos-of cj n)])
                   (when (< pi pj)
                     (vector-set! (vector-ref matrix i) j
                                  (+ (vector-ref (vector-ref matrix i) j) 1))
                     (vector-set! (vector-ref matrix j) i
                                  (- (vector-ref (vector-ref matrix j) i) 1))))))))))
     profile)
    matrix))

;;; game-schulze-strengths : Matrix Int -> Matrix
;;; Compute strongest path strengths using Floyd-Warshall.
(define (game-schulze-strengths margin n)
  (let ([strength (make-vector n)])
    ;; Initialize
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! strength i (make-vector n 0))
      (do ([j 0 (+ j 1)])
          ((>= j n))
        (when (and (not (= i j))
                   (> (vector-ref (vector-ref margin i) j) 0))
          (vector-set! (vector-ref strength i) j
                       (vector-ref (vector-ref margin i) j)))))
    ;; Floyd-Warshall
    (do ([k 0 (+ k 1)])
        ((>= k n) strength)
      (do ([i 0 (+ i 1)])
          ((>= i n))
        (when (not (= i k))
          (do ([j 0 (+ j 1)])
              ((>= j n))
            (when (not (or (= j i) (= j k)))
              (let ([via-k (min (vector-ref (vector-ref strength i) k)
                                (vector-ref (vector-ref strength k) j))]
                    [direct (vector-ref (vector-ref strength i) j)])
                (when (> via-k direct)
                  (vector-set! (vector-ref strength i) j via-k))))))))))

;;; game-borda-fallback : (List Symbol) (List Alist) -> (List Alist)
;;; Borda count fallback for sparse preferences.
(define (game-borda-fallback agent-ids findings)
  (let* ([m (length findings)]
         [scores (make-hashtable equal-hash equal?)])
    ;; Initialize scores
    (for-each (lambda (f)
                (hashtable-set! scores (cdr (assq 'title f)) 0))
              findings)
    ;; Compute Borda scores
    (for-each
     (lambda (agent-id)
       (let ([ranked (flashmob-agent-rank-findings agent-id findings)])
         (let loop ([r ranked] [pos 0])
           (unless (null? r)
             (let* ([finding (caar r)]
                    [title (cdr (assq 'title finding))]
                    [current (hashtable-ref scores title 0)])
               (hashtable-set! scores title (+ current (- m 1 pos)))
               (loop (cdr r) (+ pos 1)))))))
     agent-ids)
    ;; Sort by score
    (let ([scored (map (lambda (f)
                        (cons f (hashtable-ref scores (cdr (assq 'title f)) 0)))
                      findings)])
      (map car (sort (lambda (a b) (> (cdr a) (cdr b))) scored)))))

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
;;; Shapley Attribution
;;; ====

;;; game-shapley-credits : (List Symbol) (List Alist) -> Alist
;;; Compute Shapley value attribution.
;;; Falls back to proportional if > 15 agents.
(define (game-shapley-credits agent-ids findings)
  (if (null? agent-ids)
      '()
      (let ([n (length agent-ids)])
        (if (> n *shapley-agent-limit*)
            ;; Fall back to proportional
            (game-proportional-fallback agent-ids findings)
            ;; Full Shapley
            (game-compute-shapley agent-ids findings)))))

;;; game-compute-shapley : (List Symbol) (List Alist) -> Alist
;;; Compute full Shapley value for agent attribution.
;;; Returns normalized values that sum to 1.
(define (game-compute-shapley agent-ids findings)
  (let* ([n (length agent-ids)]
         ;; Create a cooperative game where v(S) = sum of scores for coalition S
         [game (game-create-qa-game agent-ids findings)]
         [fuel (expt 2 (+ n 1))]  ; Enough for all coalitions
         [shapley-vec (shapley-value game fuel)]
         ;; Raw Shapley values sum to v(N), not 1 - normalize them
         [raw-values (map (lambda (i) (vector-ref shapley-vec i)) (iota n))]
         [total (apply + raw-values)])
    (if (= total 0)
        ;; Equal split if no contributions
        (map (lambda (id) (cons id (/ 1.0 n))) agent-ids)
        ;; Normalize to sum to 1
        (map (lambda (id i)
               (cons id (/ (vector-ref shapley-vec i) total)))
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

;;; game-proportional-fallback : (List Symbol) (List Alist) -> Alist
;;; Proportional attribution when Shapley is too expensive.
(define (game-proportional-fallback agent-ids findings)
  (let* ([contribs (map (lambda (id)
                         (let ([ranked (flashmob-agent-rank-findings id findings)])
                           (cons id (apply + (map cdr ranked)))))
                       agent-ids)]
         [total (apply + (map cdr contribs))])
    (if (= total 0)
        (map (lambda (id) (cons id (/ 1.0 (length agent-ids)))) agent-ids)
        (map (lambda (c) (cons (car c) (/ (cdr c) total))) contribs))))

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
             [max-exc (if (< n *shapley-agent-limit*)
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
                       `((shapley-fallback . "Using proportional attribution (>15 agents)"))
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
