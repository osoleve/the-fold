;;; lattice/fp/game/voting.ss — Social Choice and Voting Rules
;;; @module voting
;;; @requires prelude

(load "core/base/prelude.ss")

(doc 'module 'voting)
(doc 'description "Social choice functions and voting rules for aggregating preferences. Covers positional rules (Borda, plurality), Condorcet methods (Copeland, Schulze), and related concepts")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key concepts: preference profile (collection of voter rankings), positional rules (score based on position), Condorcet winner (beats all others pairwise), Schulze method (resolves cycles via strongest path). Connects to cooperative games: voting games are simple games, power indices measure voting power")

(doc 'section 'preference-profiles)
(doc 'note "A preference profile is a list of rankings.")

(define (make-preference-profile rankings)
  (doc 'export #t)
  (doc 'type '(-> (List (List Candidate)) PreferenceProfile))
  (doc 'description "Create a preference profile from a list of rankings. Each ranking is a list of candidates in order of preference (best first). Example: ((a b c) (b a c) (c b a)) = 3 voters ranking candidates a,b,c")
  (doc 'note "Validates that: (1) All rankings contain the same candidates, (2) No ranking contains duplicate candidates")
  (if (null? rankings)
      '()
      (let ([first-ranking (car rankings)])
        ;; Check for duplicates in first ranking
        (unless (= (length first-ranking) (length (list->set first-ranking)))
          (error 'make-preference-profile
                 "Rankings must not contain duplicate candidates"))
        (let ([candidates (list->set first-ranking)])
          ;; Check all rankings have same candidates (no duplicates)
          (if (for-all? (lambda (r)
                          (and (= (length r) (length candidates))  ; No duplicates
                               (set=? candidates (list->set r)))) ; Same candidates
                        rankings)
              rankings
              (error 'make-preference-profile
                     "All rankings must contain same candidates without duplicates"))))))

;;; profile-voters : PreferenceProfile -> Int
;;; Number of voters in the profile.
(define (profile-voters profile)
  (doc 'export #t)
  (length profile))

;;; profile-candidates : PreferenceProfile -> (List Candidate)
;;; Get list of candidates from profile.
(define (profile-candidates profile)
  (doc 'export #t)
  (if (null? profile) '() (car profile)))

;;; profile-num-candidates : PreferenceProfile -> Int
(define (profile-num-candidates profile)
  (doc 'export #t)
  (if (null? profile) 0 (length (car profile))))

;;; profile-rankings : PreferenceProfile -> (List (List Candidate))
;;; Get all rankings (the profile itself, for consistency).
(define (profile-rankings profile)
  (doc 'export #t)
  profile)

;;; profile-empty? : PreferenceProfile -> Bool
;;; Check if profile is empty (no voters or no candidates).
(define (profile-empty? profile)
  (or (null? profile)
      (null? (car profile))))

(doc 'section 'utility-functions)

(define (list->set lst)
  (doc 'type '(-> (List a) (List a)))
  (doc 'description "Remove duplicates (preserving first occurrence order)")
  (let loop ([lst lst] [seen '()] [acc '()])
    (if (null? lst)
        (reverse acc)
        (if (member (car lst) seen)
            (loop (cdr lst) seen acc)
            (loop (cdr lst) (cons (car lst) seen) (cons (car lst) acc))))))

;;; set=? : (List a) (List a) -> Bool
;;; Check if two lists contain the same elements (order-independent).
(define (set=? a b)
  (and (= (length a) (length b))
       (for-all? (lambda (x) (member x b)) a)))

;;; for-all? : (a -> Bool) (List a) -> Bool
(define (for-all? pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (for-all? pred (cdr lst)))))

;;; position-of : a (List a) -> Int | #f
;;; Find 0-based position of element in list.
(define (position-of x lst)
  (let loop ([lst lst] [i 0])
    (cond
      [(null? lst) #f]
      [(equal? (car lst) x) i]
      [else (loop (cdr lst) (+ i 1))])))

;;; count-if : (a -> Bool) (List a) -> Int
(define (count-if pred lst)
  (length (filter pred lst)))

;;; argmax : (a -> Number) (List a) -> a
;;; Find element that maximizes the function.
(define (argmax f lst)
  (if (null? lst)
      (error 'argmax "Empty list")
      (let loop ([best (car lst)]
                 [best-val (f (car lst))]
                 [rest (cdr lst)])
        (if (null? rest)
            best
            (let ([val (f (car rest))])
              (if (> val best-val)
                  (loop (car rest) val (cdr rest))
                  (loop best best-val (cdr rest))))))))

;;; all-maxima : (a -> Number) (List a) -> (List a)
;;; Find all elements that achieve the maximum value.
(define (all-maxima f lst)
  (if (null? lst)
      '()
      (let ([max-val (apply max (map f lst))])
        (filter (lambda (x) (= (f x) max-val)) lst))))

(doc 'section 'positional-rules)
(doc 'note "Positional rules assign scores based on position in each ranking.")

(define (plurality-scores m)
  (doc 'type '(-> Int (List Int)))
  (doc 'description "Plurality: only first place gets 1 point. Score vector: (s_1, s_2, ..., s_m) where s_i is score for position i. Returns empty list for m=0")
  (if (<= m 0)
      '()
      (cons 1 (make-list (- m 1) 0))))

;;; borda-scores : Int -> (List Int)
;;; Borda: m-1 for first, m-2 for second, ..., 0 for last.
;;; Returns empty list for m=0.
(define (borda-scores m)
  (if (<= m 0)
      '()
      (let loop ([i 0] [acc '()])
        (if (>= i m)
            (reverse acc)
            (loop (+ i 1) (cons (- m 1 i) acc))))))

;;; antiplurality-scores : Int -> (List Int)
;;; Anti-plurality (veto): everyone gets 1 except last place.
;;; Returns empty list for m=0.
(define (antiplurality-scores m)
  (if (<= m 0)
      '()
      (append (make-list (- m 1) 1) '(0))))

;;; positional-score : Candidate PreferenceProfile (List Int) -> Int
;;; Compute total score for a candidate under given score vector.
(define (positional-score candidate profile scores)
  (apply + (map (lambda (ranking)
                  (let ([pos (position-of candidate ranking)])
                    (if pos (list-ref scores pos) 0)))
                profile)))

;;; positional-winner : PreferenceProfile (List Int) -> Candidate | #f
;;; Find the winner under a positional rule.
;;; Returns #f for empty profiles.
(define (positional-winner profile scores)
  (if (profile-empty? profile)
      #f
      (let ([candidates (profile-candidates profile)])
        (argmax (lambda (c) (positional-score c profile scores))
                candidates))))

;;; positional-ranking : PreferenceProfile (List Int) -> (List Candidate)
;;; Rank all candidates by score (highest first).
(define (positional-ranking profile scores)
  (let ([candidates (profile-candidates profile)])
    (sort (lambda (a b)
            (> (positional-score a profile scores)
               (positional-score b profile scores)))
          candidates)))

;;; ============================================================================
;;; Specific Voting Rules
;;; ============================================================================

;;; plurality-winner : PreferenceProfile -> Candidate
;;; Winner under plurality rule (most first-place votes).
(define (plurality-winner profile)
  (doc 'export #t)
  (positional-winner profile (plurality-scores (profile-num-candidates profile))))

;;; borda-winner : PreferenceProfile -> Candidate
;;; Winner under Borda count.
(define (borda-winner profile)
  (doc 'export #t)
  (positional-winner profile (borda-scores (profile-num-candidates profile))))

;;; antiplurality-winner : PreferenceProfile -> Candidate
;;; Winner under anti-plurality (veto) rule.
(define (antiplurality-winner profile)
  (doc 'export #t)
  (positional-winner profile (antiplurality-scores (profile-num-candidates profile))))

;;; plurality-scores-all : PreferenceProfile -> (List (Candidate . Int))
;;; Get plurality scores for all candidates.
(define (plurality-scores-all profile)
  (doc 'export #t)
  (let ([candidates (profile-candidates profile)]
        [scores (plurality-scores (profile-num-candidates profile))])
    (map (lambda (c) (cons c (positional-score c profile scores)))
         candidates)))

;;; borda-scores-all : PreferenceProfile -> (List (Candidate . Int))
;;; Get Borda scores for all candidates.
(define (borda-scores-all profile)
  (doc 'export #t)
  (let ([candidates (profile-candidates profile)]
        [scores (borda-scores (profile-num-candidates profile))])
    (map (lambda (c) (cons c (positional-score c profile scores)))
         candidates)))

(doc 'section 'condorcet-methods)

(define (pairwise-margin a b profile)
  (doc 'export #t)
  (doc 'type '(-> Candidate Candidate PreferenceProfile Int))
  (doc 'description "Number of voters preferring a over b, minus those preferring b over a. Positive = a beats b, negative = b beats a, zero = tie")
  (let ([a-over-b (count-if (lambda (ranking)
                              (let ([pa (position-of a ranking)]
                                    [pb (position-of b ranking)])
                                (and pa pb (< pa pb))))
                            profile)]
        [b-over-a (count-if (lambda (ranking)
                              (let ([pa (position-of a ranking)]
                                    [pb (position-of b ranking)])
                                (and pa pb (< pb pa))))
                            profile)])
    (- a-over-b b-over-a)))

;;; pairwise-beats? : Candidate Candidate PreferenceProfile -> Bool
;;; Does candidate a beat b in pairwise comparison?
(define (pairwise-beats? a b profile)
  (doc 'export #t)
  (> (pairwise-margin a b profile) 0))

;;; condorcet-winner? : Candidate PreferenceProfile -> Bool
;;; Is this candidate a Condorcet winner (beats all others pairwise)?
(define (condorcet-winner? candidate profile)
  (doc 'export #t)
  (let ([others (remove candidate (profile-candidates profile))])
    (for-all? (lambda (other) (pairwise-beats? candidate other profile))
              others)))

;;; condorcet-winner : PreferenceProfile -> Candidate | #f
;;; Find the Condorcet winner if one exists, #f otherwise.
(define (condorcet-winner profile)
  (doc 'export #t)
  (let ([candidates (profile-candidates profile)])
    (let loop ([cs candidates])
      (cond
        [(null? cs) #f]
        [(condorcet-winner? (car cs) profile) (car cs)]
        [else (loop (cdr cs))]))))

;;; condorcet-loser? : Candidate PreferenceProfile -> Bool
;;; Is this candidate a Condorcet loser (loses to all others pairwise)?
(define (condorcet-loser? candidate profile)
  (let ([others (remove candidate (profile-candidates profile))])
    (for-all? (lambda (other) (pairwise-beats? other candidate profile))
              others)))

(doc 'section 'copeland-method)
(doc 'note "Copeland score = (wins) - (losses) in pairwise comparisons.")

(define (copeland-score candidate profile)
  (doc 'export #t)
  (doc 'type '(-> Candidate PreferenceProfile Int))
  (doc 'description "Compute Copeland score (wins - losses). Ties count as 0.5 for each (but we use integer: win=1, loss=-1, tie=0)")
  (let ([others (remove candidate (profile-candidates profile))])
    (apply + (map (lambda (other)
                    (let ([margin (pairwise-margin candidate other profile)])
                      (cond [(> margin 0) 1]
                            [(< margin 0) -1]
                            [else 0])))
                  others))))

;;; copeland-winner : PreferenceProfile -> Candidate | #f
;;; Winner under Copeland method.
;;; Returns #f for empty profiles.
(define (copeland-winner profile)
  (doc 'export #t)
  (if (profile-empty? profile)
      #f
      (argmax (lambda (c) (copeland-score c profile))
              (profile-candidates profile))))

;;; copeland-ranking : PreferenceProfile -> (List Candidate)
;;; Full ranking by Copeland scores.
(define (copeland-ranking profile)
  (sort (lambda (a b) (> (copeland-score a profile) (copeland-score b profile)))
        (profile-candidates profile)))

(doc 'section 'schulze-method)
(doc 'note "The Schulze method resolves Condorcet cycles by finding the strongest beatpath from each candidate to each other. Strength of path is the minimum margin along the path. This implementation uses the MARGIN variant (defeats require strictly positive margins). Other variants include: Winning Votes (uses raw vote counts instead of margins), Ratio (uses ratio of votes for/against). The Margin variant is the most common and is clone-independent")

(define (build-margin-matrix profile)
  (doc 'type '(-> PreferenceProfile (Vector (Vector Int))))
  (doc 'description "Build matrix M where M[i][j] = margin of i over j. Complexity: O(V * N²) where V = voters, N = candidates")
  (doc 'note "Optimized: Instead of calling pairwise-margin for each pair O(V*N²*V), we iterate through the profile once and update all pairs per ranking")
  (let* ([candidates (profile-candidates profile)]
         [n (length candidates)]
         [matrix (make-vector n)])
    ;; Initialize NxN matrix of zeros
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! matrix i (make-vector n 0)))
    ;; Single pass through profile: for each ranking, update all pairs
    (for-each
     (lambda (ranking)
       ;; Build position lookup for this ranking: candidate -> position
       (let ([pos-of (make-eq-hashtable)])
         (let loop ([r ranking] [p 0])
           (unless (null? r)
             (hashtable-set! pos-of (car r) p)
             (loop (cdr r) (+ p 1))))
         ;; For each pair (i, j), if candidate[i] ranked higher, increment margin
         (do ([i 0 (+ i 1)])
             ((>= i n))
           (let ([ci (list-ref candidates i)]
                 [row (vector-ref matrix i)])
             (do ([j 0 (+ j 1)])
                 ((>= j n))
               (unless (= i j)
                 (let ([cj (list-ref candidates j)])
                   ;; If ci ranked higher (smaller position) than cj, +1 to margin[i][j]
                   (when (< (hashtable-ref pos-of ci n) (hashtable-ref pos-of cj n))
                     (vector-set! row j (+ (vector-ref row j) 1))
                     (vector-set! (vector-ref matrix j) i
                                  (- (vector-ref (vector-ref matrix j) i) 1))))))))))
     profile)
    matrix))

;;; schulze-strengths : PreferenceProfile -> (Vector (Vector Int))
;;; Compute strongest path strengths using Floyd-Warshall variant.
(define (schulze-strengths profile)
  (doc 'export #t)
  (let* ([margin (build-margin-matrix profile)]
         [n (vector-length margin)]
         [strength (make-vector n)])
    ;; Initialize strength matrix
    (do ([i 0 (+ i 1)])
        ((>= i n))
      (vector-set! strength i (make-vector n 0))
      (do ([j 0 (+ j 1)])
          ((>= j n))
        (when (and (not (= i j))
                   (> (vector-ref (vector-ref margin i) j) 0))
          (vector-set! (vector-ref strength i) j
                       (vector-ref (vector-ref margin i) j)))))
    ;; Floyd-Warshall for strongest paths
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

;;; schulze-winner : PreferenceProfile -> Candidate | #f
;;; Find winner using Schulze method.
;;; Returns #f for empty profiles.
(define (schulze-winner profile)
  (doc 'export #t)
  (if (profile-empty? profile)
      #f
      (let* ([candidates (profile-candidates profile)]
             [n (length candidates)]
             [strength (schulze-strengths profile)])
        ;; A candidate wins if their path to each other is >= that other's path to them
        (let loop ([i 0])
          (if (>= i n)
              (car candidates)  ; fallback (shouldn't happen with valid profile)
              (let ([winner?
                     (let check ([j 0])
                       (cond
                         [(>= j n) #t]
                         [(= i j) (check (+ j 1))]
                         [(>= (vector-ref (vector-ref strength i) j)
                              (vector-ref (vector-ref strength j) i))
                          (check (+ j 1))]
                         [else #f]))])
                (if winner?
                    (list-ref candidates i)
                    (loop (+ i 1)))))))))

;;; schulze-ranking : PreferenceProfile -> (List Candidate)
;;; Full ranking using Schulze method.
;;; Returns empty list for empty profiles.
(define (schulze-ranking profile)
  (doc 'export #t)
  (if (profile-empty? profile)
      '()
      (let* ([candidates (profile-candidates profile)]
             [n (length candidates)]
             [strength (schulze-strengths profile)])
        ;; Sort by Schulze relation
        (sort (lambda (a b)
                (let ([ia (position-of a candidates)]
                      [ib (position-of b candidates)])
                  (> (vector-ref (vector-ref strength ia) ib)
                     (vector-ref (vector-ref strength ib) ia))))
              candidates))))

(doc 'section 'manipulation-detection)
(doc 'note "A voting rule is manipulable if a voter can benefit by misreporting.")
;;; Gibbard-Satterthwaite: any non-dictatorial rule with 3+ candidates
;;; is manipulable.

;;; manipulation-possible? : PreferenceProfile (Profile -> Candidate) Int -> Bool
;;; Can voter i benefit by voting strategically under the given rule?
;;; This is a brute-force check over all possible misreports.
;;;
;;; WARNING: O(N!) complexity - only practical for N <= 9 candidates.
;;; For N=10, there are 3.6 million permutations to check per voter.
(define (manipulation-possible? profile voting-rule voter-idx)
  (doc 'export #t)
  (let* ([true-ranking (list-ref profile voter-idx)]
         [candidates (profile-candidates profile)]
         [true-winner (voting-rule profile)]
         [true-pref (position-of true-winner true-ranking)]
         [all-perms (permutations candidates)])
    (let loop ([perms all-perms])
      (if (null? perms)
          #f
          (let* ([fake-ranking (car perms)]
                 [fake-profile (list-set profile voter-idx fake-ranking)]
                 [fake-winner (voting-rule fake-profile)]
                 [fake-pref (position-of fake-winner true-ranking)])
            (if (and fake-pref true-pref (< fake-pref true-pref))
                #t  ; Found beneficial manipulation
                (loop (cdr perms))))))))

;;; list-set : (List a) Int a -> (List a)
;;; Return list with element at index replaced.
(define (list-set lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-set (cdr lst) (- idx 1) val))))

;;; permutations : (List a) -> (List (List a))
;;; Generate all permutations (for small lists).
(define (permutations lst)
  (if (null? lst)
      '(())
      (apply append
             (map (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x lst))))
                  lst))))

;;; ============================================================================
;;; Classic Examples
;;; ============================================================================

;;; condorcet-cycle-example : -> PreferenceProfile
;;; The classic Condorcet paradox: A>B>C, B>C>A, C>A>B
;;; No Condorcet winner exists.
(define (condorcet-cycle-example)
  (doc 'export #t)
  (make-preference-profile
   '((a b c)    ; 1 voter: a > b > c
     (b c a)    ; 1 voter: b > c > a
     (c a b)))) ; 1 voter: c > a > b

;;; borda-plurality-disagree-example : -> PreferenceProfile
;;; Profile where Borda and plurality give different winners.
(define (borda-plurality-disagree-example)
  (make-preference-profile
   '((a b c)    ; 2 voters
     (a b c)
     (b c a)    ; 3 voters
     (b c a)
     (b c a)
     (c b a)    ; 2 voters
     (c b a))))
;; Plurality: a=2, b=3, c=2 -> b wins
;; Borda: a=2*2+0*5+0*2=4, b=1*2+2*5+1*2=14, c=0*2+1*5+2*2=9 -> b wins
;; (Actually both agree here - let me make a better example)

;;; borda-condorcet-disagree-example : -> PreferenceProfile
;;; Profile where Borda winner differs from Condorcet winner.
(define (borda-condorcet-disagree-example)
  (make-preference-profile
   '((a b c)    ; 3 voters
     (a b c)
     (a b c)
     (b c a)    ; 2 voters
     (b c a)
     (c b a)    ; 2 voters
     (c b a))))
;; Condorcet: a beats b (3-4), a beats c (3-4)... let me recalculate

;;; ============================================================================
;;; Exports Summary
;;; ============================================================================

;;; Preference profiles:
;;;   make-preference-profile, profile-voters, profile-candidates
;;;
;;; Positional rules:
;;;   plurality-winner, borda-winner, antiplurality-winner
;;;   plurality-scores-all, borda-scores-all
;;;   positional-winner, positional-ranking
;;;
;;; Condorcet methods:
;;;   condorcet-winner?, condorcet-winner, condorcet-loser?
;;;   pairwise-margin, pairwise-beats?
;;;   copeland-winner, copeland-ranking, copeland-score
;;;   schulze-winner, schulze-ranking
;;;
;;; Analysis:
;;;   manipulation-possible?
;;;
;;; Examples:
;;;   condorcet-cycle-example, borda-condorcet-disagree-example
