;;; lattice/game-theory/matching.ss — Stable Matching
;;; @module matching
;;; @requires prelude lp ilp coop-games sort

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'lp)
(require 'ilp)
(require 'coop-games)
(require 'sort)

(doc 'module 'matching)
(doc 'description "Stable matching, assignment games, and optimal matching. Includes the Gale-Shapley deferred acceptance algorithm, bipartite assignment games with connections to cooperative game theory, and optimal assignment via linear programming")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key concepts: two-sided matching (disjoint sets of agents with preferences), stability (no blocking pair - both prefer each other to current match), assignment game (cooperative game from bipartite valuations), optimal assignment (maximum weight bipartite matching via LP)")

(doc 'section 'matching-market-data-structures)
(doc 'note "A matching market consists of two disjoint sets of agents (proposers and receivers) where each agent has strict preferences over the other side. Preferences are represented as lists (most preferred first). Example: (a b c) means a > b > c in preference ordering")

(define-record-type matching-market%
  (fields
   proposers    ; Vector of proposer IDs
   receivers    ; Vector of receiver IDs
   p-prefs      ; Hashtable: proposer-id -> preference list over receivers
   r-prefs))    ; Hashtable: receiver-id -> preference list over proposers

(define (make-matching-market proposers receivers p-pref-list r-pref-list)
  (doc 'type '(-> (List Id) (List Id) (List (Id . List)) (List (Id . List)) Market))
  (doc 'description "Create a matching market from proposers, receivers, and their preferences")
  (doc 'note "Example: (make-matching-market (m1 m2 m3) (w1 w2 w3) ((m1 w1 w2 w3) (m2 w2 w1 w3) (m3 w1 w3 w2)) ((w1 m2 m1 m3) (w2 m1 m2 m3) (w3 m1 m3 m2)))")
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

(doc 'section 'gale-shapley)
(doc 'note "The Gale-Shapley algorithm produces a stable matching that is optimal for the proposing side and pessimal for the receiving side")
(doc 'note "Algorithm: (1) Each unmatched proposer proposes to their most-preferred receiver they haven't yet proposed to, (2) Each receiver tentatively accepts their most-preferred proposal, rejecting others, (3) Repeat until all proposers are matched or exhausted preferences")
(doc 'note "Properties: always produces a stable matching, proposer-optimal (no proposer can do better in any stable matching), individual rationality (no agent matched to unacceptable partner), complexity O(n²) proposals, O(n³) with current linear ID lookups (future: O(n²) with pre-computed ID->index hashtables)")

(define (stable-match market fuel)
  (doc 'type '(-> Market Nat (List (Id . Id))))
  (doc 'description "Compute proposer-optimal stable matching via deferred acceptance. Returns list of (proposer . receiver) pairs. fuel bounds iterations (set to n² for guaranteed completion)")
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

(doc 'section 'stability-verification)

(define (matching-stable? market matching)
  (doc 'type '(-> Market (List (Id . Id)) Boolean))
  (doc 'description "Verify that a matching is stable (no blocking pairs). A blocking pair (p, r) exists if: p prefers r to current match (or p is unmatched), r prefers p to current match (or r is unmatched)")
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

;; assoc-ref is provided by prelude

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

(doc 'section 'assignment-games)
(doc 'note "An assignment game is a cooperative game derived from a bipartite matching market with transferable utility. Each (proposer, receiver) pair has a value v(p, r) representing the worth of matching them. The characteristic function is: v(S) = max weight matching in the induced bipartite graph on S. Key results: the core is always non-empty, Core = set of competitive equilibria, Shapley value gives a fair division")

(define (make-assignment-game n m valuation)
  (doc 'type '(-> Nat Nat (Nat Nat -> Real) CoopGame))
  (doc 'description "Create assignment game from bipartite valuations. n = number of proposers (players 0 to n-1), m = number of receivers (players n to n+m-1), valuation(i, j) = value of matching proposer i with receiver j-n")
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

(doc 'section 'optimal-assignment)
(doc 'note "Finds the assignment that maximizes total value. Uses LP formulation which guarantees integral solution for bipartite matching (totally unimodular constraint matrix)")

(define (optimal-assignment n m valuation)
  (doc 'type '(-> Nat Nat (Nat Nat -> Real) (List (Nat . Nat) . Real)))
  (doc 'description "Compute optimal assignment and its total value. Returns ((matches ...) . total-value) where matches are (prop . recv) pairs")
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
  (let ((props1 (sort-by symbol<? (map car matching1)))
        (props2 (sort-by symbol<? (map car matching2)))
        (recvs1 (sort-by symbol<? (map cdr matching1)))
        (recvs2 (sort-by symbol<? (map cdr matching2))))
    (and (equal? props1 props2)
         (equal? recvs1 recvs2))))

;;; symbol<? : Symbol × Symbol → Boolean
;;; Lexicographic comparison for symbols.
(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

(doc 'section 'ilp-matching)
(doc 'note "These algorithms use Integer Linear Programming for matching problems that require integral solutions (exact matchings)")

(doc 'section 'weighted-matching-ilp)
(doc 'note "Given a bipartite graph with weights W[i,j] for each edge (i,j), find a matching that maximizes the total weight. ILP formulation: maximize Σ W[i,j] · x[i,j] subject to Σ_j x[i,j] <= 1 for all i (each row matched at most once), Σ_i x[i,j] <= 1 for all j (each col matched at most once), x[i,j] ∈ {0,1}")

(doc 'type '(-> Nat Nat (Nat Nat -> Real) (List (Nat . Nat) . Real)))
;;; Compute maximum weighted bipartite matching using ILP.
;;; n = number of nodes on left side (rows)
;;; m = number of nodes on right side (cols)
;;; weight = weight function (i,j) → weight of edge (i,j)
;;; Returns ((matches ...) . total-weight) where matches are (left . right) pairs.
;;;
;;; Example:
;;;   (weighted-matching-ilp 2 2 (lambda (i j) (vector-ref '#(#(3 1) #(2 4)) i j)))
;;;   => (((0 . 0) (1 . 1)) . 7)
(define (weighted-matching-ilp n m weight)
  (let* (;; Number of decision variables: n*m binary variables x[i,j]
         [num-vars (* n m)]
         ;; Number of constraints: n row constraints + m col constraints
         [num-row-constraints n]
         [num-col-constraints m]
         [num-constraints (+ num-row-constraints num-col-constraints)]
         ;; Add slack variables for <= constraints
         [num-slacks num-constraints]
         [total-vars (+ num-vars num-slacks)]
         ;; Cost vector (negated for maximization via minimization)
         [c (make-vector total-vars 0)]
         ;; Constraint matrix A
         [A (make-matrix num-constraints total-vars 0)]
         ;; RHS vector b (all 1s for matching constraints)
         [b (make-vector num-constraints 1)])
    ;; Fill cost vector: c[i*m + j] = -weight(i,j) (negate to maximize)
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j 0 (+ j 1)])
          [(= j m)]
        (let ([var-idx (+ (* i m) j)])
          (vector-set! c var-idx (- (weight i j))))))
    ;; Fill row constraints: Σ_j x[i,j] + slack_i = 1
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j 0 (+ j 1)])
          [(= j m)]
        (let ([var-idx (+ (* i m) j)])
          (matrix-set! A i var-idx 1)))
      ;; Slack variable for row i
      (matrix-set! A i (+ num-vars i) 1))
    ;; Fill col constraints: Σ_i x[i,j] + slack_{n+j} = 1
    (do ([j 0 (+ j 1)])
        [(= j m)]
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (let ([var-idx (+ (* i m) j)])
          (matrix-set! A (+ n j) var-idx 1)))
      ;; Slack variable for col j
      (matrix-set! A (+ n j) (+ num-vars n j) 1))
    ;; Create and solve ILP (decision variables are binary)
    (let* ([ilp (make-ilp c A b (iota num-vars))]  ; all x[i,j] must be integer
           [result (ilp-solve ilp)])
      (if (ilp-optimal? result)
          (let* ([x (ilp-result-x result)]
                 ;; Extract matches from solution
                 [matches (extract-ilp-matches x n m)]
                 ;; Total weight is negation of objective (we minimized -weight)
                 [total-weight (- (ilp-result-z result))])
            (cons matches total-weight))
          ;; Fallback (shouldn't happen for bipartite matching)
          (cons '() 0)))))

;;; extract-ilp-matches : Vec × Nat × Nat → (List (Nat . Nat))
;;; Extract matching pairs from ILP solution vector.
(define (extract-ilp-matches x n m)
  (let loop ([i 0] [j 0] [result '()])
    (cond
      [(= i n) (reverse result)]
      [(= j m) (loop (+ i 1) 0 result)]
      [else
       (let* ([var-idx (+ (* i m) j)]
              [val (vector-ref x var-idx)])
         (loop i (+ j 1)
               (if (> val 0.5)  ; Threshold for binary variable
                   (cons (cons i j) result)
                   result)))])))

;;; ----------------------------------------------------------------------------
;;; Bottleneck Matching via ILP (Minimize Maximum Edge Weight)
;;; ----------------------------------------------------------------------------
;;;
;;; The bottleneck matching problem minimizes the maximum weight edge
;;; in a perfect or maximal matching. This is useful for load balancing
;;; and fairness applications.
;;;
;;; Solved via binary search on threshold T:
;;;   - For each T, solve feasibility ILP: can we match using only edges ≤ T?
;;;   - Binary search finds minimum T that admits a matching.
;;;
;;; Feasibility ILP at threshold T:
;;;   maximize   Σ x[i,j]  (or just check feasibility)
;;;   subject to x[i,j] = 0  if weight(i,j) > T
;;;              Σ_j x[i,j] <= 1  for all i
;;;              Σ_i x[i,j] <= 1  for all j
;;;              x[i,j] ∈ {0,1}

;;; bottleneck-matching-ilp : Nat × Nat × (Nat × Nat → Real) → (List (Nat . Nat)) × Real
;;; Compute bottleneck bipartite matching using binary search + ILP.
;;; Returns ((matches ...) . bottleneck-value) where bottleneck-value is
;;; the maximum edge weight in the matching.
;;;
;;; Example:
;;;   (bottleneck-matching-ilp 2 2 (lambda (i j) (vector-ref '#(#(5 3) #(4 2)) i j)))
;;;   => (((0 . 1) (1 . 1)) . 3) or similar optimal matching
(define (bottleneck-matching-ilp n m weight)
  (let* (;; Collect all distinct edge weights
         [all-weights (collect-edge-weights n m weight)]
         ;; Sort weights to enable binary search
         [sorted-weights (sort-numbers all-weights)]
         ;; Determine required matching size (min of n, m for maximal)
         [required-size (min n m)])
    (if (null? sorted-weights)
        ;; No edges - empty matching
        (cons '() 0)
        ;; Binary search for minimum bottleneck
        (let ([result (binary-search-bottleneck
                       n m weight sorted-weights required-size)])
          (if result
              result
              ;; Fallback if no matching found
              (cons '() +inf.0))))))

;;; collect-edge-weights : Nat × Nat × (Nat × Nat → Real) → List
;;; Collect all edge weights into a list.
(define (collect-edge-weights n m weight)
  (let loop ([i 0] [j 0] [result '()])
    (cond
      [(= i n) result]
      [(= j m) (loop (+ i 1) 0 result)]
      [else
       (let ([w (weight i j)])
         (loop i (+ j 1) (cons w result)))])))

;;; sort-numbers : List → List
;;; Sort a list of numbers in ascending order.
(define (sort-numbers lst)
  (sort-by < lst))

;;; remove-duplicates-numbers : List → List
;;; Remove duplicate numbers from sorted list.
(define (remove-duplicates-numbers sorted-lst)
  (if (or (null? sorted-lst) (null? (cdr sorted-lst)))
      sorted-lst
      (let loop ([lst (cdr sorted-lst)] [prev (car sorted-lst)] [result (list (car sorted-lst))])
        (if (null? lst)
            (reverse result)
            (let ([curr (car lst)])
              (if (= curr prev)
                  (loop (cdr lst) curr result)
                  (loop (cdr lst) curr (cons curr result))))))))

;;; binary-search-bottleneck : Nat × Nat × Fn × List × Nat → (List . Real) | #f
;;; Binary search for minimum threshold that admits required matching size.
(define (binary-search-bottleneck n m weight sorted-weights required-size)
  (let* ([unique-weights (remove-duplicates-numbers sorted-weights)]
         [num-thresholds (length unique-weights)]
         [weight-vec (list->vector unique-weights)])
    (if (= num-thresholds 0)
        #f
        (let loop ([lo 0] [hi (- num-thresholds 1)] [best-result #f])
          (if (> lo hi)
              best-result
              (let* ([mid (quotient (+ lo hi) 2)]
                     [threshold (vector-ref weight-vec mid)]
                     [feasible-result (check-bottleneck-feasibility
                                       n m weight threshold required-size)])
                (if feasible-result
                    ;; Found feasible matching at this threshold, try lower
                    (loop lo (- mid 1) feasible-result)
                    ;; Not feasible, need higher threshold
                    (loop (+ mid 1) hi best-result))))))))

;;; check-bottleneck-feasibility : Nat × Nat × Fn × Real × Nat → (List . Real) | #f
;;; Check if a matching of required size exists using only edges ≤ threshold.
;;; Returns (matches . threshold) if feasible, #f otherwise.
(define (check-bottleneck-feasibility n m weight threshold required-size)
  (let* (;; Create filtered weight function (edges above threshold become 0)
         ;; Actually for feasibility, we want to maximize # of matches
         ;; using only edges <= threshold
         [num-vars (* n m)]
         [num-constraints (+ n m)]
         [num-slacks num-constraints]
         [total-vars (+ num-vars num-slacks)]
         ;; Cost: maximize sum of x[i,j] => minimize -sum
         [c (make-vector total-vars 0)]
         [A (make-matrix num-constraints total-vars 0)]
         [b (make-vector num-constraints 1)])
    ;; Cost: -1 for each matching variable (to maximize count)
    ;; But only for edges <= threshold
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j 0 (+ j 1)])
          [(= j m)]
        (let ([var-idx (+ (* i m) j)]
              [w (weight i j)])
          (when (<= w threshold)
            (vector-set! c var-idx -1)))))
    ;; Row constraints
    (do ([i 0 (+ i 1)])
        [(= i n)]
      (do ([j 0 (+ j 1)])
          [(= j m)]
        (matrix-set! A i (+ (* i m) j) 1))
      (matrix-set! A i (+ num-vars i) 1))
    ;; Column constraints
    (do ([j 0 (+ j 1)])
        [(= j m)]
      (do ([i 0 (+ i 1)])
          [(= i n)]
        (matrix-set! A (+ n j) (+ (* i m) j) 1))
      (matrix-set! A (+ n j) (+ num-vars n j) 1))
    ;; Solve ILP
    (let* ([ilp (make-ilp c A b (iota num-vars))]
           [result (ilp-solve ilp)])
      (if (ilp-optimal? result)
          (let* ([x (ilp-result-x result)]
                 [matches (extract-ilp-matches x n m)]
                 [match-count (length matches)])
            (if (>= match-count required-size)
                (cons matches threshold)
                #f))
          #f))))
