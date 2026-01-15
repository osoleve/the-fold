;;; lattice/fp/game/matching.ss — Matching Theory
;;;
;;; Implements stable matching, assignment games, and optimal matching.
;;; Includes the Gale-Shapley deferred acceptance algorithm, bipartite
;;; assignment games with connections to cooperative game theory, and
;;; optimal assignment via linear programming.
;;;
;;; This is Lattice code: pure, total, assumes reasonable input.
;;;
;;; Key concepts:
;;;   - Two-sided matching: disjoint sets of agents with preferences
;;;   - Stability: no blocking pair (both prefer each other to current match)
;;;   - Assignment game: cooperative game from bipartite valuations
;;;   - Optimal assignment: maximum weight bipartite matching via LP
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/optimization/lp.ss
;;;   - lattice/fp/game/coop-games.ss (for assignment-game)

(load "core/base/prelude.ss")
(load "lattice/optimization/lp.ss")
(load "lattice/fp/game/coop-games.ss")

;;; ============================================================================
;;; Matching Market Data Structures
;;; ============================================================================
;;;
;;; A matching market consists of two disjoint sets of agents (proposers and
;;; receivers) where each agent has strict preferences over the other side.
;;;
;;; Preferences are represented as lists (most preferred first).
;;; Example: '(a b c) means a > b > c in preference ordering.

(define-record-type matching-market%
  (fields
   proposers    ; Vector of proposer IDs
   receivers    ; Vector of receiver IDs
   p-prefs      ; Hashtable: proposer-id -> preference list over receivers
   r-prefs))    ; Hashtable: receiver-id -> preference list over proposers

;;; make-matching-market : (List Id) × (List Id) × (List (Id . List)) × (List (Id . List)) → Market
;;; Create a matching market from proposers, receivers, and their preferences.
;;;
;;; Example:
;;;   (make-matching-market
;;;     '(m1 m2 m3)
;;;     '(w1 w2 w3)
;;;     '((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2))
;;;     '((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2)))
(define (make-matching-market proposers receivers p-pref-list r-pref-list)
  (let ((p-prefs (make-pref-table p-pref-list))
        (r-prefs (make-pref-table r-pref-list)))
    (make-matching-market%
     (list->vector proposers)
     (list->vector receivers)
     p-prefs
     r-prefs)))

;;; make-pref-table : (List (Id . List)) → Hashtable
;;; Build preference hashtable from alist.
;;; Input: ((id pref1 pref2 ...) ...)
(define (make-pref-table pref-list)
  (let ((table '()))
    (for-each
     (lambda (entry)
       (let ((id (car entry))
             (prefs (cdr entry)))
         (set! table (cons (cons id prefs) table))))
     pref-list)
    table))

;;; pref-lookup : Hashtable × Id → List
;;; Get preferences for an agent.
(define (pref-lookup table id)
  (let ((entry (assoc id table)))
    (if entry (cdr entry) '())))

;;; matching-market? : Any → Boolean
(define (matching-market? x)
  (matching-market%? x))

;;; market-proposers : Market → Vector
(define (market-proposers m)
  (matching-market%-proposers m))

;;; market-receivers : Market → Vector
(define (market-receivers m)
  (matching-market%-receivers m))

;;; market-proposer-prefs : Market × Id → List
(define (market-proposer-prefs m id)
  (pref-lookup (matching-market%-p-prefs m) id))

;;; market-receiver-prefs : Market × Id → List
(define (market-receiver-prefs m id)
  (pref-lookup (matching-market%-r-prefs m) id))

;;; market-num-proposers : Market → Nat
(define (market-num-proposers m)
  (vector-length (market-proposers m)))

;;; market-num-receivers : Market → Nat
(define (market-num-receivers m)
  (vector-length (market-receivers m)))

;;; ============================================================================
;;; Gale-Shapley Deferred Acceptance Algorithm
;;; ============================================================================
;;;
;;; The Gale-Shapley algorithm produces a stable matching that is optimal
;;; for the proposing side and pessimal for the receiving side.
;;;
;;; Algorithm:
;;;   1. Each unmatched proposer proposes to their most-preferred receiver
;;;      they haven't yet proposed to
;;;   2. Each receiver tentatively accepts their most-preferred proposal,
;;;      rejecting others
;;;   3. Repeat until all proposers are matched or exhausted preferences
;;;
;;; Properties:
;;;   - Always produces a stable matching
;;;   - Proposer-optimal: no proposer can do better in any stable matching
;;;   - Individual rationality: no agent matched to unacceptable partner
;;;   - Complexity: O(n²) proposals, O(n³) with current linear ID lookups
;;;     (future: O(n²) with pre-computed ID→index hashtables)

;;; stable-match : Market × Nat → (List (Id . Id))
;;; Compute proposer-optimal stable matching via deferred acceptance.
;;; Returns list of (proposer . receiver) pairs.
;;; fuel bounds iterations (set to n² for guaranteed completion).
(define (stable-match market fuel)
  (let* ((proposers (market-proposers market))
         (receivers (market-receivers market))
         (n-prop (vector-length proposers))
         (n-recv (vector-length receivers))
         (recv-match (make-vector n-recv #f))
         (prop-match (make-vector n-prop #f))
         (prop-next (make-vector n-prop 0))
         (free-props (iota n-prop))
         (recv-ranks (build-receiver-ranks market)))
    (gs-loop proposers receivers recv-match prop-match prop-next
             recv-ranks market free-props fuel)))

;;; gs-loop : Internal loop for Gale-Shapley algorithm
(define (gs-loop proposers receivers recv-match prop-match prop-next
                 recv-ranks market free fuel)
  (cond
    ((null? free) (extract-matching proposers prop-match))
    ((<= fuel 0) (extract-matching proposers prop-match))
    (else
     (let* ((p-idx (car free))
            (p-id (vector-ref proposers p-idx))
            (p-prefs (market-proposer-prefs market p-id))
            (next-idx (vector-ref prop-next p-idx)))
       (if (>= next-idx (length p-prefs))
           ;; Proposer exhausted preferences
           (gs-loop proposers receivers recv-match prop-match prop-next
                    recv-ranks market (cdr free) (- fuel 1))
           ;; Propose to next receiver
           (let* ((r-id (list-ref p-prefs next-idx))
                  (r-idx (find-receiver-idx receivers r-id)))
             (vector-set! prop-next p-idx (+ next-idx 1))
             (gs-process-proposal proposers receivers recv-match prop-match
                                  prop-next recv-ranks market free fuel
                                  p-idx p-id r-id r-idx)))))))

;;; gs-process-proposal : Handle a single proposal
;;; Enforces individual rationality: receivers only accept acceptable proposers
;;; (those in their preference list).
(define (gs-process-proposal proposers receivers recv-match prop-match
                             prop-next recv-ranks market free fuel
                             p-idx p-id r-id r-idx)
  (if (not r-idx)
      ;; Invalid receiver, skip
      (gs-loop proposers receivers recv-match prop-match prop-next
               recv-ranks market free (- fuel 1))
      (let* ((current-match (vector-ref recv-match r-idx))
             (ranks (vector-ref recv-ranks r-idx))
             (p-acceptable? (assoc p-id ranks)))  ; Check if proposer is acceptable
        (if (not p-acceptable?)
            ;; Proposer not in receiver's preferences (unacceptable) - reject
            (gs-loop proposers receivers recv-match prop-match prop-next
                     recv-ranks market free (- fuel 1))
            (if (not current-match)
                ;; Receiver is free and proposer is acceptable - accept
                (begin
                  (vector-set! recv-match r-idx p-id)
                  (vector-set! prop-match p-idx r-id)
                  (gs-loop proposers receivers recv-match prop-match prop-next
                           recv-ranks market (cdr free) (- fuel 1)))
                ;; Compare with current match
                (let ((current-idx (find-proposer-idx proposers current-match)))
                  (if (receiver-prefers? ranks p-id current-match)
                      ;; Receiver prefers new proposer
                      (begin
                        (vector-set! recv-match r-idx p-id)
                        (vector-set! prop-match p-idx r-id)
                        (vector-set! prop-match current-idx #f)
                        (gs-loop proposers receivers recv-match prop-match prop-next
                                 recv-ranks market (cons current-idx (cdr free))
                                 (- fuel 1)))
                      ;; Receiver prefers current
                      (gs-loop proposers receivers recv-match prop-match prop-next
                               recv-ranks market free (- fuel 1)))))))))

;;; build-receiver-ranks : Market → Vector of Hashtables
;;; Pre-compute ranking tables for O(1) preference comparison.
;;; recv-ranks[i] maps proposer-id -> rank (lower = more preferred)
(define (build-receiver-ranks market)
  (let* ((receivers (market-receivers market))
         (n (vector-length receivers))
         (ranks (make-vector n '())))
    (do ((i 0 (+ i 1)))
        ((= i n) ranks)
      (let* ((r-id (vector-ref receivers i))
             (prefs (market-receiver-prefs market r-id))
             (rank-table (build-rank-table prefs)))
        (vector-set! ranks i rank-table)))))

;;; build-rank-table : List → Hashtable
;;; Build rank lookup: id -> position (0 = most preferred)
(define (build-rank-table prefs)
  (let loop ((prefs prefs) (rank 0) (table '()))
    (if (null? prefs)
        table
        (loop (cdr prefs) (+ rank 1)
              (cons (cons (car prefs) rank) table)))))

;;; receiver-prefers? : Hashtable × Id × Id → Boolean
;;; Does receiver prefer p1 over p2?
(define (receiver-prefers? ranks p1-id p2-id)
  (let ((r1 (assoc p1-id ranks))
        (r2 (assoc p2-id ranks)))
    (cond
      ((not r1) #f)  ; p1 not in preferences
      ((not r2) #t)  ; p2 not in preferences, p1 wins
      (else (< (cdr r1) (cdr r2))))))

;;; find-receiver-idx : Vector × Id → Nat | #f
(define (find-receiver-idx receivers r-id)
  (let ((n (vector-length receivers)))
    (let loop ((i 0))
      (cond
        ((= i n) #f)
        ((equal? (vector-ref receivers i) r-id) i)
        (else (loop (+ i 1)))))))

;;; find-proposer-idx : Vector × Id → Nat | #f
(define (find-proposer-idx proposers p-id)
  (let ((n (vector-length proposers)))
    (let loop ((i 0))
      (cond
        ((= i n) #f)
        ((equal? (vector-ref proposers i) p-id) i)
        (else (loop (+ i 1)))))))

;;; extract-matching : Vector × Vector → (List (Id . Id))
;;; Extract matching as list of pairs from match vectors.
(define (extract-matching proposers prop-match)
  (let ((n (vector-length proposers)))
    (let loop ((i 0) (result '()))
      (if (= i n)
          (reverse result)
          (let ((p-id (vector-ref proposers i))
                (r-id (vector-ref prop-match i)))
            (loop (+ i 1)
                  (if r-id
                      (cons (cons p-id r-id) result)
                      result)))))))

;;; ============================================================================
;;; Stability Verification
;;; ============================================================================

;;; matching-stable? : Market × (List (Id . Id)) → Boolean
;;; Verify that a matching is stable (no blocking pairs).
;;; A blocking pair (p, r) exists if:
;;;   - p prefers r to current match (or p is unmatched)
;;;   - r prefers p to current match (or r is unmatched)
(define (matching-stable? market matching)
  (let* ((proposers (market-proposers market))
         (receivers (market-receivers market))
         (prop-to-recv (build-match-table-p matching))
         (recv-to-prop (build-match-table-r matching)))
    (not (find-blocking-pair market proposers receivers
                             prop-to-recv recv-to-prop))))

;;; build-match-table-p : (List (Id . Id)) → Hashtable
;;; Build proposer -> receiver lookup.
(define (build-match-table-p matching)
  (map (lambda (pair) (cons (car pair) (cdr pair))) matching))

;;; build-match-table-r : (List (Id . Id)) → Hashtable
;;; Build receiver -> proposer lookup.
(define (build-match-table-r matching)
  (map (lambda (pair) (cons (cdr pair) (car pair))) matching))

;;; find-blocking-pair : Market × ... → (Pair Id Id) | #f
;;; Find a blocking pair if one exists.
(define (find-blocking-pair market proposers receivers prop-to-recv recv-to-prop)
  (let ((n-prop (vector-length proposers)))
    (let p-loop ((i 0))
      (if (= i n-prop)
          #f
          (let* ((p-id (vector-ref proposers i))
                 (p-match (assoc-ref prop-to-recv p-id))
                 (p-prefs (market-proposer-prefs market p-id)))
            (or (let r-loop ((prefs p-prefs))
                  (cond
                    ((null? prefs) #f)
                    ((equal? (car prefs) p-match) #f)  ; reached current match
                    (else
                     (let* ((r-id (car prefs))
                            (r-match (assoc-ref recv-to-prop r-id))
                            (r-prefs (market-receiver-prefs market r-id)))
                       (if (prefers-to? r-prefs p-id r-match)
                           (cons p-id r-id)  ; blocking pair found
                           (r-loop (cdr prefs)))))))
                (p-loop (+ i 1))))))))

;;; assoc-ref : Hashtable × Key → Value | #f
(define (assoc-ref table key)
  (let ((entry (assoc key table)))
    (if entry (cdr entry) #f)))

;;; prefers-to? : List × Id × Id → Boolean
;;; Does agent prefer id1 to id2? (id2 may be #f for unmatched)
(define (prefers-to? prefs id1 id2)
  (if (not id2)
      (member id1 prefs)  ; anything beats unmatched
      (let loop ((prefs prefs))
        (cond
          ((null? prefs) #f)
          ((equal? (car prefs) id1) #t)    ; found id1 first
          ((equal? (car prefs) id2) #f)    ; found id2 first
          (else (loop (cdr prefs)))))))

;;; ============================================================================
;;; Assignment Games
;;; ============================================================================
;;;
;;; An assignment game is a cooperative game derived from a bipartite matching
;;; market with transferable utility. Each (proposer, receiver) pair has a
;;; value v(p, r) representing the worth of matching them.
;;;
;;; The characteristic function is:
;;;   v(S) = max weight matching in the induced bipartite graph on S
;;;
;;; Key results:
;;;   - The core is always non-empty
;;;   - Core = set of competitive equilibria
;;;   - Shapley value gives a fair division

;;; make-assignment-game : Nat × Nat × (Nat × Nat → Real) → CoopGame
;;; Create assignment game from bipartite valuations.
;;; n = number of proposers (players 0 to n-1)
;;; m = number of receivers (players n to n+m-1)
;;; valuation(i, j) = value of matching proposer i with receiver j-n
(define (make-assignment-game n m valuation)
  (let ((total-players (+ n m)))
    (make-coop-game
     total-players
     (lambda (S)
       (assignment-game-value n m valuation S)))))

;;; assignment-game-value : Nat × Nat × Fn × Coalition → Real
;;; Compute v(S) as the max weight matching in S.
(define (assignment-game-value n m valuation S)
  (let* ((props-in-S (filter (lambda (i) (coalition-member? i S)) (iota n)))
         (recvs-in-S (filter (lambda (j) (coalition-member? j S))
                              (map (lambda (k) (+ n k)) (iota m))))
         (num-props (length props-in-S))
         (num-recvs (length recvs-in-S)))
    (if (or (= num-props 0) (= num-recvs 0))
        0
        ;; Solve assignment problem for this subset
        (optimal-assignment-subset props-in-S recvs-in-S n valuation))))

;;; optimal-assignment-subset : List × List × Nat × Fn → Real
;;; Compute optimal assignment value for a subset of players.
;;; Uses LP: maximize sum v(i,j)*x(i,j) s.t. matching constraints.
(define (optimal-assignment-subset props recvs n valuation)
  (let* ((num-props (length props))
         (num-recvs (length recvs))
         (prop-vec (list->vector props))
         (recv-vec (list->vector recvs))
         (num-vars (* num-props num-recvs))
         (num-constraints (+ num-props num-recvs))
         (num-slacks num-constraints)
         (total-vars (+ num-vars num-slacks))
         (c (make-vector total-vars 0))
         (A (make-matrix num-constraints total-vars 0))
         (b (make-vector num-constraints 1)))
    ;; Fill cost vector (negated for maximization)
    (do ((i 0 (+ i 1)))
        ((= i num-props))
      (do ((j 0 (+ j 1)))
          ((= j num-recvs))
        (let ((var-idx (+ (* i num-recvs) j))
              (p-idx (vector-ref prop-vec i))
              (r-idx (- (vector-ref recv-vec j) n)))
          (vector-set! c var-idx (- (valuation p-idx r-idx))))))
    ;; Fill proposer constraints: sum_j x(i,j) + slack_i = 1
    (do ((i 0 (+ i 1)))
        ((= i num-props))
      (do ((j 0 (+ j 1)))
          ((= j num-recvs))
        (let ((var-idx (+ (* i num-recvs) j)))
          (matrix-set! A i var-idx 1)))
      (matrix-set! A i (+ num-vars i) 1))
    ;; Fill receiver constraints: sum_i x(i,j) + slack_{num-props+j} = 1
    (do ((j 0 (+ j 1)))
        ((= j num-recvs))
      (do ((i 0 (+ i 1)))
          ((= i num-props))
        (let ((var-idx (+ (* i num-recvs) j)))
          (matrix-set! A (+ num-props j) var-idx 1)))
      (matrix-set! A (+ num-props j) (+ num-vars num-props j) 1))
    ;; Solve LP
    (let ((lp (make-lp c A b)))
      (let ((result (lp-solve lp)))
        (if (lp-optimal? result)
            (- (lp-result-z result))  ; negate back to maximize
            0)))))  ; fallback for infeasible (shouldn't happen)

;;; ============================================================================
;;; Optimal Assignment (Maximum Weight Bipartite Matching)
;;; ============================================================================
;;;
;;; Finds the assignment that maximizes total value.
;;; Uses LP formulation which guarantees integral solution for
;;; bipartite matching (totally unimodular constraint matrix).

;;; optimal-assignment : Nat × Nat × (Nat × Nat → Real) → (List (Nat . Nat)) × Real
;;; Compute optimal assignment and its total value.
;;; Returns ((matches ...) . total-value) where matches are (prop . recv) pairs.
(define (optimal-assignment n m valuation)
  (let* ((num-vars (* n m))
         (num-constraints (+ n m))
         (num-slacks num-constraints)
         (total-vars (+ num-vars num-slacks))
         (c (make-vector total-vars 0))
         (A (make-matrix num-constraints total-vars 0))
         (b (make-vector num-constraints 1)))
    ;; Cost vector (negated for maximization)
    (do ((i 0 (+ i 1)))
        ((= i n))
      (do ((j 0 (+ j 1)))
          ((= j m))
        (let ((var-idx (+ (* i m) j)))
          (vector-set! c var-idx (- (valuation i j))))))
    ;; Proposer constraints
    (do ((i 0 (+ i 1)))
        ((= i n))
      (do ((j 0 (+ j 1)))
          ((= j m))
        (matrix-set! A i (+ (* i m) j) 1))
      (matrix-set! A i (+ num-vars i) 1))
    ;; Receiver constraints
    (do ((j 0 (+ j 1)))
        ((= j m))
      (do ((i 0 (+ i 1)))
          ((= i n))
        (matrix-set! A (+ n j) (+ (* i m) j) 1))
      (matrix-set! A (+ n j) (+ num-vars n j) 1))
    ;; Solve
    (let* ((lp (make-lp c A b))
           (result (lp-solve lp)))
      (if (lp-optimal? result)
          (let* ((x (lp-result-x result))
                 (total (- (lp-result-z result)))
                 (matches (extract-assignment-matches x n m)))
            (cons matches total))
          (cons '() 0)))))

;;; extract-assignment-matches : Vec × Nat × Nat → (List (Nat . Nat))
;;; Extract matching pairs from LP solution.
(define (extract-assignment-matches x n m)
  (let loop ((i 0) (j 0) (result '()))
    (cond
      ((= i n) (reverse result))
      ((= j m) (loop (+ i 1) 0 result))
      (else
       (let ((val (vector-ref x (+ (* i m) j))))
         (loop i (+ j 1)
               (if (> val 0.5)  ; threshold for integrality
                   (cons (cons i j) result)
                   result)))))))

;;; ============================================================================
;;; Classic Market Examples
;;; ============================================================================

;;; make-medical-residency-market : (List Id) × (List Id) → Market
;;; Create a simple medical residency matching market.
;;; Students = proposers, Hospitals = receivers (NRMP convention).
(define (make-medical-residency-market students hospitals student-prefs hospital-prefs)
  (make-matching-market students hospitals student-prefs hospital-prefs))

;;; make-school-choice-market : (List Id) × (List Id) → Market
;;; Create a school choice market.
;;; Students = proposers, Schools = receivers.
(define (make-school-choice-market students schools student-prefs school-prefs)
  (make-matching-market students schools student-prefs school-prefs))

;;; ============================================================================
;;; Helper: Rank-based preference generation
;;; ============================================================================

;;; make-random-preferences : (List Id) × (List Id) × (Nat → Nat) → (List (Id . List))
;;; Generate random preferences for one side.
;;; shuffle is a function that shuffles a list (can use Fisher-Yates).
(define (make-random-preferences agents targets shuffle)
  (map (lambda (agent)
         (cons agent (shuffle targets)))
       agents))

;;; preference-rank : List × Id → Nat | #f
;;; Get rank of an item in preference list (0 = most preferred).
(define (preference-rank prefs item)
  (let loop ((prefs prefs) (rank 0))
    (cond
      ((null? prefs) #f)
      ((equal? (car prefs) item) rank)
      (else (loop (cdr prefs) (+ rank 1))))))

;;; ============================================================================
;;; Receiver-Optimal Matching
;;; ============================================================================
;;;
;;; Running Gale-Shapley with receivers proposing gives receiver-optimal
;;; (and proposer-pessimal) stable matching.

;;; stable-match-receiver-optimal : Market × Nat → (List (Id . Id))
;;; Compute receiver-optimal stable matching.
;;; Swaps the roles of proposers and receivers.
(define (stable-match-receiver-optimal market fuel)
  ;; Create reversed market
  (let ((reversed-market (make-matching-market%
                          (market-receivers market)
                          (market-proposers market)
                          (matching-market%-r-prefs market)
                          (matching-market%-p-prefs market))))
    ;; Run GS with receivers as proposers
    (let ((matching (stable-match reversed-market fuel)))
      ;; Swap pairs back to (proposer . receiver) format
      (map (lambda (pair) (cons (cdr pair) (car pair))) matching))))

;;; ============================================================================
;;; Rural Hospital Theorem Verification
;;; ============================================================================
;;;
;;; The Rural Hospital Theorem states that in any two stable matchings:
;;; 1. The same agents are matched (not necessarily to the same partners)
;;; 2. The number of matches is the same
;;; This can be verified by comparing proposer-optimal and receiver-optimal.

;;; same-matched-agents? : (List (Id . Id)) × (List (Id . Id)) → Boolean
;;; Check if two matchings have the same set of matched agents.
(define (same-matched-agents? matching1 matching2)
  (let ((props1 (sort symbol<? (map car matching1)))
        (props2 (sort symbol<? (map car matching2)))
        (recvs1 (sort symbol<? (map cdr matching1)))
        (recvs2 (sort symbol<? (map cdr matching2))))
    (and (equal? props1 props2)
         (equal? recvs1 recvs2))))

;;; symbol<? : Symbol × Symbol → Boolean
;;; Lexicographic comparison for symbols.
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))
