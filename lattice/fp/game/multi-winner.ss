(load "lattice/fp/game/voting.ss")

(doc 'module 'multi-winner)
(doc 'description "Proportional representation methods for selecting committees or diverse option sets. Implements Single Transferable Vote (STV) with Droop/Hare quotas, approval voting, Proportional Approval Voting (PAV), Monroe method, and Chamberlin-Courant method")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key concepts: quota (minimum votes needed for election - Droop, Hare), transfer value (fractional weight when surplus votes transfer), approval profile (each voter's set of approved candidates), representation (which elected candidate represents each voter)")

(doc 'section 'quotas)

(define (droop-quota num-voters num-seats)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "Droop quota: floor(votes / (seats + 1)) + 1. Most common quota - ensures at most k candidates can meet quota. Example: 100 voters, 5 seats → floor(100/6) + 1 = 17")
  (+ (floor (/ num-voters (+ num-seats 1))) 1))

;;; hare-quota : Nat × Nat → Nat
;;; Hare quota: floor(votes / seats)
;;; More proportional but can leave seats unfilled in edge cases.
;;; Example: 100 voters, 5 seats → 20
(define (hare-quota num-voters num-seats)
  (floor (/ num-voters num-seats)))

(doc 'section 'stv)
(doc 'note "STV uses ranked preferences to elect k candidates proportionally.")
;;; Algorithm:
;;;   1. Count first preferences for each remaining candidate
;;;   2. If a candidate meets quota, elect them
;;;      - Transfer surplus votes at fractional value
;;;   3. If no candidate meets quota, eliminate lowest
;;;      - Transfer all their votes at full value
;;;   4. Repeat until k seats filled
;;;
;;; We represent ballots as (weight . ranking) pairs during counting.

;;; make-weighted-ballot : (List Candidate) → WeightedBallot
;;; Create ballot with weight 1 from a ranking.
(define (make-weighted-ballot ranking)
  (cons 1 ranking))

;;; ballot-weight : WeightedBallot → Real
(define (ballot-weight b) (car b))

;;; ballot-ranking : WeightedBallot → (List Candidate)
(define (ballot-ranking b) (cdr b))

;;; ballot-with-weight : WeightedBallot × Real → WeightedBallot
(define (ballot-with-weight b new-weight)
  (cons new-weight (ballot-ranking b)))

;;; ballot-top-choice : WeightedBallot × (Set Candidate) → Candidate | #f
;;; Get highest-ranked candidate still in the running.
(define (ballot-top-choice ballot remaining)
  (let loop ([ranking (ballot-ranking ballot)])
    (cond
      [(null? ranking) #f]
      [(member (car ranking) remaining) (car ranking)]
      [else (loop (cdr ranking))])))

;;; count-votes : (List WeightedBallot) × (List Candidate) → (List (Candidate . Real))
;;; Count weighted first-preference votes for each candidate.
(define (count-votes ballots remaining)
  (map (lambda (c)
         (cons c (apply + (map (lambda (b)
                                 (if (eq? c (ballot-top-choice b remaining))
                                     (ballot-weight b)
                                     0))
                               ballots))))
       remaining))

;;; stv : PreferenceProfile × Nat × (Nat × Nat → Nat) → (List Candidate)
;;; Run STV election to fill k seats using given quota function.
;;; Returns list of elected candidates in order of election.
(define (stv profile num-seats quota-fn)
  (if (profile-empty? profile)
      '()
      (let* ([candidates (profile-candidates profile)]
             [num-voters (profile-voters profile)]
             [quota (quota-fn num-voters num-seats)]
             [ballots (map make-weighted-ballot profile)])
        (stv-loop ballots candidates '() num-seats quota))))

;;; stv-loop : (List WeightedBallot) × (List Candidate) × (List Candidate) × Nat × Real → (List Candidate)
;;; Main STV counting loop.
(define (stv-loop ballots remaining elected seats-left quota)
  (cond
    ;; All seats filled
    [(= seats-left 0) (reverse elected)]
    ;; Not enough candidates left - elect them all
    [(<= (length remaining) seats-left)
     (reverse (append (reverse remaining) elected))]
    [else
     (let* ([vote-counts (count-votes ballots remaining)]
            [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) vote-counts)]
            [top-candidate (caar sorted)]
            [top-votes (cdar sorted)])
       (if (>= top-votes quota)
           ;; Candidate meets quota - elect and transfer surplus
           (let* ([surplus (- top-votes quota)]
                  [transfer-value (if (> top-votes 0) (/ surplus top-votes) 0)]
                  [new-ballots (transfer-surplus ballots top-candidate transfer-value remaining)]
                  [new-remaining (remove top-candidate remaining)])
             (stv-loop new-ballots
                       new-remaining
                       (cons top-candidate elected)
                       (- seats-left 1)
                       quota))
           ;; No one meets quota - eliminate lowest
           (let* ([bottom-candidate (caar (reverse sorted))]
                  [new-ballots (transfer-eliminated ballots bottom-candidate remaining)]
                  [new-remaining (remove bottom-candidate remaining)])
             (stv-loop new-ballots
                       new-remaining
                       elected
                       seats-left
                       quota))))]))

;;; transfer-surplus : (List WeightedBallot) × Candidate × Real × (List Candidate) → (List WeightedBallot)
;;; Transfer surplus votes from elected candidate at fractional value.
(define (transfer-surplus ballots elected transfer-value remaining)
  (map (lambda (b)
         (if (eq? (ballot-top-choice b remaining) elected)
             ;; This ballot's top choice was elected - reduce weight
             (ballot-with-weight b (* (ballot-weight b) transfer-value))
             b))
       ballots))

;;; transfer-eliminated : (List WeightedBallot) × Candidate × (List Candidate) → (List WeightedBallot)
;;; Transfer votes from eliminated candidate at full value (no change needed,
;;; ballot-top-choice will automatically skip to next preference).
(define (transfer-eliminated ballots eliminated remaining)
  ballots)  ; No weight change needed - top-choice logic handles skip

;;; stv-droop : PreferenceProfile × Nat → (List Candidate)
;;; STV with Droop quota (most common variant).
(define (stv-droop profile num-seats)
  (stv profile num-seats droop-quota))

;;; stv-hare : PreferenceProfile × Nat → (List Candidate)
;;; STV with Hare quota (more proportional).
(define (stv-hare profile num-seats)
  (stv profile num-seats hare-quota))

(doc 'section 'approval-voting)
(doc 'note "In approval voting, each voter submits a set of approved candidates.")
;;; For multi-winner: select k candidates with most approvals.
;;;
;;; Approval profile: list of (Set Candidate) where each set is one voter's approvals.

;;; make-approval-profile : (List (List Candidate)) → ApprovalProfile
;;; Create approval profile from list of approval sets.
(define (make-approval-profile approval-lists)
  (map list->set approval-lists))

;;; approval-profile-voters : ApprovalProfile → Nat
(define (approval-profile-voters profile)
  (length profile))

;;; approval-profile-candidates : ApprovalProfile → (List Candidate)
;;; Get all candidates mentioned in any ballot.
(define (approval-profile-candidates profile)
  (list->set (apply append profile)))

;;; approval-count : Candidate × ApprovalProfile → Nat
;;; Count how many voters approve of a candidate.
(define (approval-count candidate profile)
  (count-if (lambda (approvals) (member candidate approvals)) profile))

;;; approval-scores : ApprovalProfile → (List (Candidate . Nat))
;;; Get approval scores for all candidates.
(define (approval-scores profile)
  (let ([candidates (approval-profile-candidates profile)])
    (map (lambda (c) (cons c (approval-count c profile)))
         candidates)))

;;; approval-winners : ApprovalProfile × Nat → (List Candidate)
;;; Select k candidates with highest approval counts.
;;; Basic approval voting - not proportional (popular candidates sweep).
(define (approval-winners profile num-seats)
  (let* ([scores (approval-scores profile)]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)])
    (take-up-to num-seats (map car sorted))))

;;; take-up-to : Nat × (List a) → (List a)
;;; Take up to n elements from list.
(define (take-up-to n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-up-to (- n 1) (cdr lst)))))

(doc 'section 'pav)
(doc 'note "PAV uses a diminishing returns scoring function to achieve proportionality.")
;;; Each voter's contribution to a candidate's score decreases based on how
;;; many candidates they've already helped elect.
;;;
;;; Score contribution: 1/k where k = number of approved-and-elected candidates + 1
;;; This incentivizes electing candidates approved by underrepresented voters.

;;; pav-score : Candidate × ApprovalProfile × (List Candidate) → Real
;;; PAV score for adding candidate c given already-elected committee.
(define (pav-score candidate profile elected)
  (apply +
         (map (lambda (approvals)
                (if (member candidate approvals)
                    ;; Contribution diminishes based on how many elected candidates this voter approved
                    (let ([already-satisfied (count-if (lambda (e) (member e approvals)) elected)])
                      (/ 1 (+ already-satisfied 1)))
                    0))
              profile)))

;;; pav-winners : ApprovalProfile × Nat → (List Candidate)
;;; Sequential PAV: greedily add candidate maximizing marginal PAV score.
;;; Returns elected committee.
(define (pav-winners profile num-seats)
  (let ([candidates (approval-profile-candidates profile)])
    (pav-loop profile candidates '() num-seats)))

;;; pav-loop : ApprovalProfile × (List Candidate) × (List Candidate) × Nat → (List Candidate)
(define (pav-loop profile remaining elected seats-left)
  (if (or (= seats-left 0) (null? remaining))
      (reverse elected)
      (let* ([scores (map (lambda (c) (cons c (pav-score c profile elected))) remaining)]
             [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)]
             [winner (caar sorted)])
        (pav-loop profile
                  (remove winner remaining)
                  (cons winner elected)
                  (- seats-left 1)))))

;;; ============================================================================
;;; Satisfaction Approval Voting (SAV)
;;; ============================================================================

;;; SAV normalizes each voter's contribution by the size of their approval set.
;;; This prevents voters who approve many candidates from having outsized influence.

;;; sav-score : Candidate × ApprovalProfile → Real
;;; SAV score: sum over voters of (1/|approval set|) if they approve candidate.
(define (sav-score candidate profile)
  (apply +
         (map (lambda (approvals)
                (if (and (member candidate approvals) (> (length approvals) 0))
                    (/ 1 (length approvals))
                    0))
              profile)))

;;; sav-winners : ApprovalProfile × Nat → (List Candidate)
;;; Select k candidates with highest SAV scores.
(define (sav-winners profile num-seats)
  (let* ([candidates (approval-profile-candidates profile)]
         [scores (map (lambda (c) (cons c (sav-score c profile))) candidates)]
         [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)])
    (take-up-to num-seats (map car sorted))))

(doc 'section 'monroe)
(doc 'note "Monroe method assigns each voter to exactly one representative.")
;;; Goal: maximize total satisfaction where each committee member
;;; represents approximately n/k voters.
;;;
;;; This is NP-hard, so we use a greedy heuristic.
;;;
;;; Satisfaction function: position-based (higher rank = more satisfaction)

;;; voter-satisfaction : (List Candidate) × Candidate × Nat → Nat
;;; How satisfied is voter with candidate as their representative?
;;; Uses Borda-style scoring: m-1 for first choice, m-2 for second, etc.
(define (voter-satisfaction ranking candidate num-candidates)
  (let ([pos (position-of candidate ranking)])
    (if pos
        (- num-candidates 1 pos)
        0)))

;;; monroe-greedy : PreferenceProfile × Nat → (List Candidate)
;;; Greedy approximation to Monroe method.
;;; Strategy: iteratively add candidate who maximizes total satisfaction
;;; for voters not yet well-represented.
(define (monroe-greedy profile num-seats)
  (if (profile-empty? profile)
      '()
      (let* ([candidates (profile-candidates profile)]
             [num-voters (profile-voters profile)]
             [num-candidates (length candidates)]
             [voters-per-rep (floor (/ num-voters num-seats))])
        (monroe-loop profile candidates '() num-seats num-candidates))))

;;; monroe-loop : PreferenceProfile × (List Candidate) × (List Candidate) × Nat × Nat → (List Candidate)
(define (monroe-loop profile remaining elected seats-left num-candidates)
  (if (or (= seats-left 0) (null? remaining))
      (reverse elected)
      (let* ([scores (map (lambda (c)
                           (cons c (monroe-candidate-score c profile elected num-candidates)))
                         remaining)]
             [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)]
             [winner (caar sorted)])
        (monroe-loop profile
                     (remove winner remaining)
                     (cons winner elected)
                     (- seats-left 1)
                     num-candidates))))

;;; monroe-candidate-score : Candidate × PreferenceProfile × (List Candidate) × Nat → Nat
;;; Score candidate based on satisfaction of voters who'd prefer them over current committee.
(define (monroe-candidate-score candidate profile elected num-candidates)
  (apply + (map (lambda (ranking)
                  (let ([new-sat (voter-satisfaction ranking candidate num-candidates)]
                        [best-existing (best-satisfaction ranking elected num-candidates)])
                    ;; Only count improvement over current representation
                    (max 0 (- new-sat best-existing))))
                profile)))

;;; best-satisfaction : (List Candidate) × (List Candidate) × Nat → Nat
;;; Best satisfaction voter gets from any elected candidate.
(define (best-satisfaction ranking elected num-candidates)
  (if (null? elected)
      0
      (apply max (map (lambda (e) (voter-satisfaction ranking e num-candidates)) elected))))

(doc 'section 'chamberlin-courant)
(doc 'note "Chamberlin-Courant is like Monroe but doesn't require equal representation.")
;;; Each voter is represented by their most-preferred elected candidate.
;;; Goal: maximize sum of satisfactions.
;;;
;;; Also NP-hard, using greedy approximation.

;;; cc-greedy : PreferenceProfile × Nat → (List Candidate)
;;; Greedy Chamberlin-Courant: iteratively add candidate maximizing
;;; marginal satisfaction improvement.
(define (cc-greedy profile num-seats)
  (if (profile-empty? profile)
      '()
      (let* ([candidates (profile-candidates profile)]
             [num-candidates (length candidates)])
        (cc-loop profile candidates '() num-seats num-candidates))))

;;; cc-loop : PreferenceProfile × (List Candidate) × (List Candidate) × Nat × Nat → (List Candidate)
(define (cc-loop profile remaining elected seats-left num-candidates)
  (if (or (= seats-left 0) (null? remaining))
      (reverse elected)
      (let* ([scores (map (lambda (c)
                           (cons c (cc-marginal-score c profile elected num-candidates)))
                         remaining)]
             [sorted (sort (lambda (a b) (> (cdr a) (cdr b))) scores)]
             [winner (caar sorted)])
        (cc-loop profile
                 (remove winner remaining)
                 (cons winner elected)
                 (- seats-left 1)
                 num-candidates))))

;;; cc-marginal-score : Candidate × PreferenceProfile × (List Candidate) × Nat → Nat
;;; Marginal improvement in total satisfaction from adding candidate.
(define (cc-marginal-score candidate profile elected num-candidates)
  (apply + (map (lambda (ranking)
                  (let ([new-sat (voter-satisfaction ranking candidate num-candidates)]
                        [old-sat (best-satisfaction ranking elected num-candidates)])
                    (max 0 (- new-sat old-sat))))
                profile)))

;;; cc-total-satisfaction : PreferenceProfile × (List Candidate) → Nat
;;; Total CC satisfaction score for a committee.
(define (cc-total-satisfaction profile committee)
  (let ([num-candidates (if (null? profile) 0 (length (car profile)))])
    (apply + (map (lambda (ranking)
                    (best-satisfaction ranking committee num-candidates))
                  profile))))

;;; ============================================================================
;;; Analysis Functions
;;; ============================================================================

;;; proportionality-score : PreferenceProfile × (List Candidate) → Real
;;; Measure how proportionally the committee represents voter preferences.
;;; 1.0 = perfect proportionality, 0.0 = worst case.
;;; Uses variance of representation quality across voters.
(define (proportionality-score profile committee)
  (if (or (profile-empty? profile) (null? committee))
      0
      (let* ([num-candidates (length (car profile))]
             [sats (map (lambda (ranking)
                         (best-satisfaction ranking committee num-candidates))
                       profile)]
             [max-sat (- num-candidates 1)]
             [normalized (map (lambda (s) (if (> max-sat 0) (/ s max-sat) 1)) sats)]
             [mean (/ (apply + normalized) (length normalized))]
             [variance (/ (apply + (map (lambda (x) (expt (- x mean) 2)) normalized))
                         (length normalized))])
        ;; Convert variance to score (lower variance = better)
        (max 0 (- 1 (* 2 (sqrt variance)))))))

;;; representation-coverage : ApprovalProfile × (List Candidate) → Real
;;; Fraction of voters who approve at least one elected candidate.
(define (representation-coverage approval-profile committee)
  (if (null? approval-profile)
      1
      (let ([covered (count-if (lambda (approvals)
                                 (any? (lambda (c) (member c approvals)) committee))
                               approval-profile)])
        (/ covered (length approval-profile)))))

;;; any? : (a → Bool) × (List a) → Bool
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))

;;; diversity-score : (List Candidate) × (Candidate × Candidate → Real) → Real
;;; Measure committee diversity using pairwise distance function.
;;; Higher score = more diverse committee.
(define (diversity-score committee distance-fn)
  (if (< (length committee) 2)
      0
      (let* ([pairs (all-pairs committee)]
             [distances (map (lambda (p) (distance-fn (car p) (cdr p))) pairs)])
        (/ (apply + distances) (length pairs)))))

;;; all-pairs : (List a) → (List (a . a))
;;; Generate all unordered pairs.
(define (all-pairs lst)
  (if (null? lst)
      '()
      (append (map (lambda (x) (cons (car lst) x)) (cdr lst))
              (all-pairs (cdr lst)))))

;;; ============================================================================
;;; Conversion Functions
;;; ============================================================================

;;; profile->approval : PreferenceProfile × Nat → ApprovalProfile
;;; Convert ranked profile to approval profile by approving top k candidates.
(define (profile->approval profile k)
  (map (lambda (ranking) (take-up-to k ranking)) profile))

;;; approval->profile : ApprovalProfile → PreferenceProfile
;;; Convert approval profile to ranked profile (arbitrary order within approved).
;;; Non-approved candidates placed at end in arbitrary order.
(define (approval->profile approval-profile)
  (let ([all-candidates (approval-profile-candidates approval-profile)])
    (map (lambda (approvals)
           (append approvals
                   (filter (lambda (c) (not (member c approvals))) all-candidates)))
         approval-profile)))

;;; ============================================================================
;;; Classic Examples
;;; ============================================================================

;;; stv-example : → PreferenceProfile
;;; Classic STV example: 9 voters, 3 seats, parties roughly proportional.
(define (stv-example)
  (make-preference-profile
   '((a b c d)    ; 4 voters prefer party A
     (a b c d)
     (a b c d)
     (a b c d)
     (b a c d)    ; 2 voters prefer party B
     (b a c d)
     (c d a b)    ; 3 voters prefer party C
     (c d a b)
     (c d a b))))

;;; approval-example : → ApprovalProfile
;;; Example where basic approval and PAV differ.
(define (approval-example)
  (make-approval-profile
   '((a b)        ; 4 voters approve A and B
     (a b)
     (a b)
     (a b)
     (c d)        ; 3 voters approve C and D
     (c d)
     (c d)
     (a c)        ; 2 voters approve A and C
     (a c))))

;;; diverse-preferences-example : → PreferenceProfile
;;; Example with diverse preferences showing Monroe/CC in action.
(define (diverse-preferences-example)
  (make-preference-profile
   '((a b c d e)  ; Group 1: 3 voters
     (a b c d e)
     (a b c d e)
     (b c a d e)  ; Group 2: 3 voters
     (b c a d e)
     (b c a d e)
     (c d e a b)  ; Group 3: 2 voters
     (c d e a b)
     (d e c b a)  ; Group 4: 2 voters
     (d e c b a))))

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Quotas:
;;;   droop-quota, hare-quota
;;;
;;; STV (ranked input):
;;;   stv, stv-droop, stv-hare
;;;
;;; Approval-based (approval set input):
;;;   approval-winners          ; Basic (not proportional)
;;;   pav-winners              ; Proportional Approval Voting
;;;   sav-winners              ; Satisfaction Approval Voting
;;;
;;; Preference-based (ranked input):
;;;   monroe-greedy            ; Greedy Monroe method
;;;   cc-greedy                ; Greedy Chamberlin-Courant
;;;
;;; Analysis:
;;;   cc-total-satisfaction    ; Total CC satisfaction score
;;;   proportionality-score    ; Measure proportionality
;;;   representation-coverage  ; Fraction of voters covered
;;;   diversity-score          ; Committee diversity
;;;
;;; Conversion:
;;;   profile->approval        ; Ranked to approval
;;;   approval->profile        ; Approval to ranked
;;;
;;; Examples:
;;;   stv-example, approval-example, diverse-preferences-example
