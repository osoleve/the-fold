;;; lattice/game-theory/strategic-voting.ss — Strategic Voting Equilibrium Analysis
;;; @module strategic-voting
;;; @requires prelude voting hamt

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'hamt)
(require 'voting)

(doc 'module 'strategic-voting)
(doc 'description "Strategic voting equilibrium analysis: Nash equilibria in voting games, iterative best response dynamics, price of anarchy, and Gibbard-Satterthwaite demonstrations")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Key concepts: (1) A voting game models each voter as a strategic player choosing a ballot; (2) Best response: the ballot that gives a voter their most-preferred winner given others' ballots; (3) Nash equilibrium: no voter can improve by unilateral deviation; (4) Price of anarchy: ratio of truthful welfare to worst equilibrium welfare")
(doc 'note "IMPORTANT: All voting-rule functions must be deterministic and return a SINGLE candidate (not a set of tied winners). Tie-breaking must be handled within the voting rule.")

;;; ============================================================================
;;; Utility Functions
;;; ============================================================================

(doc 'section 'utilities)

;;; voter-utility : Candidate Ranking -> Number
;;; How much a voter values this winner. Higher = better.
;;; Uses position in true ranking: first place = n-1, last place = 0.
(define (voter-utility winner true-ranking)
  (doc 'description "Compute voter's utility for winner based on their true preferences. First-choice winner gives highest utility")
  (let ([pos (position-of winner true-ranking)])
    (if pos
        (- (length true-ranking) 1 pos)
        0)))

;;; social-welfare : Candidate PreferenceProfile -> Number
;;; Sum of utilities across all voters.
(define (social-welfare winner true-profile)
  (doc 'export #t)
  (doc 'type '(-> Candidate PreferenceProfile Number))
  (doc 'description "Utilitarian social welfare: sum of all voters' utilities for winner")
  (apply + (map (lambda (ranking) (voter-utility winner ranking))
                true-profile)))

;;; ============================================================================
;;; Best Response Dynamics
;;; ============================================================================

(doc 'section 'best-response)

;;; find-best-responses : Int PreferenceProfile (Profile -> Candidate) Ranking -> (List Ranking)
;;; Find all best response ballots for voter at index.
;;; true-ranking: voter's actual preferences (for computing utility)
;;; Returns list of ballots that achieve maximum utility.
(define (find-best-responses voter-idx current-profile voting-rule true-ranking)
  (doc 'export #t)
  (doc 'type '(-> Nat PreferenceProfile VotingRule Ranking (List Ranking)))
  (doc 'description "Find all ballots that maximize voter's utility given current profile")
  (let* ([candidates (profile-candidates current-profile)]
         [all-ballots (permutations candidates)]
         ;; Compute utility for each possible ballot
         [ballot-utilities
          (map (lambda (ballot)
                 (let* ([new-profile (list-set current-profile voter-idx ballot)]
                        [winner (voting-rule new-profile)]
                        [utility (voter-utility winner true-ranking)])
                   (cons ballot utility)))
               all-ballots)]
         ;; Find maximum utility
         [max-utility (apply max (map cdr ballot-utilities))])
    ;; Return all ballots that achieve maximum
    (map car (filter (lambda (bu) (= (cdr bu) max-utility))
                     ballot-utilities))))

;;; best-response-improves? : Int PreferenceProfile (Profile -> Candidate) Ranking -> Bool
;;; Can voter improve by switching from current ballot?
(define (best-response-improves? voter-idx current-profile voting-rule true-ranking)
  (doc 'type '(-> Nat PreferenceProfile VotingRule Ranking Bool))
  (let* ([current-winner (voting-rule current-profile)]
         [current-utility (voter-utility current-winner true-ranking)]
         [best-responses (find-best-responses voter-idx current-profile voting-rule true-ranking)]
         [best-utility (voter-utility (voting-rule (list-set current-profile voter-idx (car best-responses)))
                                       true-ranking)])
    (> best-utility current-utility)))

;;; ============================================================================
;;; Nash Equilibrium
;;; ============================================================================

(doc 'section 'nash-equilibrium)

;;; is-strategic-equilibrium? : PreferenceProfile PreferenceProfile (Profile -> Candidate) -> Bool
;;; Is the reported profile a Nash equilibrium given true preferences?
(define (is-strategic-equilibrium? reported-profile true-profile voting-rule)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile PreferenceProfile VotingRule Bool))
  (doc 'description "Check if no voter can benefit by unilateral deviation. This is O(N! * V) where N = candidates, V = voters")
  (let ([n (length reported-profile)])
    (let loop ([i 0])
      (if (>= i n)
          #t  ; All voters checked, no beneficial deviation found
          (if (best-response-improves? i reported-profile voting-rule (list-ref true-profile i))
              #f  ; Voter i can improve
              (loop (+ i 1)))))))

;;; find-strategic-equilibrium : PreferenceProfile (Profile -> Candidate) Int -> (Result PreferenceProfile Symbol)
;;; Find a Nash equilibrium via iterated best response.
;;; Returns (ok equilibrium) or (err 'cycle) or (err 'fuel-exhausted).
(define (find-strategic-equilibrium true-profile voting-rule fuel)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile VotingRule Nat (Result PreferenceProfile Symbol)))
  (doc 'description "Find Nash equilibrium using iterated best response. Starts from truthful profile, lets each voter optimize in round-robin order until convergence or cycle detection")
  (doc 'note "WARNING: O(N! * V * fuel) complexity. Only practical for small instances (≤6 candidates)")
  (if (null? true-profile)
      (list 'ok '())
      (let* ([n (length true-profile)]
             [candidates (profile-candidates true-profile)]
             [all-ballots (permutations candidates)]  ; Compute once, reuse
             [visited hamt-empty])
        ;; Start from truthful profile
        (let loop ([current true-profile]
                   [remaining-fuel fuel]
                   [visited visited])
          (cond
            [(<= remaining-fuel 0)
             (list 'err 'fuel-exhausted)]
            [(hamt-lookup current visited)
             (list 'err 'cycle)]
            [else
             (let ([visited (hamt-assoc current #t visited)])
             ;; Combined equilibrium check and improvement finding (single pass)
             (let find-improving-voter ([i 0])
               (if (>= i n)
                   ;; No voter can improve -> this is an equilibrium
                   (list 'ok current)
                   ;; Check if voter i can improve
                   (let* ([true-ranking (list-ref true-profile i)]
                          [current-winner (voting-rule current)]
                          [current-utility (voter-utility current-winner true-ranking)]
                          ;; Find best response ballot and its utility
                          [ballot-utilities
                           (map (lambda (ballot)
                                  (let* ([new-profile (list-set current i ballot)]
                                         [winner (voting-rule new-profile)])
                                    (cons ballot (voter-utility winner true-ranking))))
                                all-ballots)]
                          [max-utility (apply max (map cdr ballot-utilities))])
                     (if (> max-utility current-utility)
                         ;; Found improvement - apply it and continue
                         (let* ([best-ballot (car (car (filter (lambda (bu) (= (cdr bu) max-utility))
                                                               ballot-utilities)))]
                                [new-profile (list-set current i best-ballot)])
                           (loop new-profile (- remaining-fuel 1) visited))
                         ;; No improvement for voter i, check next
                         (find-improving-voter (+ i 1)))))))])))))

;;; all-strategic-equilibria : PreferenceProfile (Profile -> Candidate) -> (List PreferenceProfile)
;;; Find all Nash equilibria by exhaustive search.
;;; WARNING: O((N!)^V) - only for tiny instances (≤4 candidates, ≤3 voters).
(define (all-strategic-equilibria true-profile voting-rule)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile VotingRule (List PreferenceProfile)))
  (doc 'description "Find ALL Nash equilibria by exhaustive search. Exponentially expensive!")
  (if (null? true-profile)
      (list '())
      (let* ([candidates (profile-candidates true-profile)]
             [n (length true-profile)]
             [all-ballots (permutations candidates)]
             ;; Generate all possible reported profiles
             [all-profiles (cartesian-product-n all-ballots n)])
        (filter (lambda (rp) (is-strategic-equilibrium? rp true-profile voting-rule))
                all-profiles))))

;;; cartesian-product-n : (List a) Int -> (List (List a))
;;; Generate all n-tuples from a list.
(define (cartesian-product-n items n)
  (if (= n 0)
      '(())
      (append-map (lambda (rest)
                    (map (lambda (item) (cons item rest))
                         items))
                  (cartesian-product-n items (- n 1)))))

;;; ============================================================================
;;; Price of Anarchy
;;; ============================================================================

(doc 'section 'price-of-anarchy)

;;; price-of-anarchy : PreferenceProfile (Profile -> Candidate) -> Number | 'no-equilibrium
;;; Ratio of truthful welfare to worst equilibrium welfare.
;;; POA ≥ 1; higher means strategic behavior is more costly.
(define (price-of-anarchy true-profile voting-rule)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile VotingRule (Union Number Symbol)))
  (doc 'description "Price of anarchy: truthful welfare / worst equilibrium welfare. Measures social cost of strategic behavior")
  (let* ([truthful-winner (voting-rule true-profile)]
         [truthful-welfare (social-welfare truthful-winner true-profile)]
         [equilibria (all-strategic-equilibria true-profile voting-rule)])
    (if (null? equilibria)
        'no-equilibrium
        (let* ([eq-welfares
                (map (lambda (eq)
                       (social-welfare (voting-rule eq) true-profile))
                     equilibria)]
               [worst-welfare (apply min eq-welfares)])
          (if (= worst-welfare 0)
              +inf.0
              (/ truthful-welfare worst-welfare))))))

;;; price-of-stability : PreferenceProfile (Profile -> Candidate) -> Number | 'no-equilibrium
;;; Ratio of truthful welfare to BEST equilibrium welfare.
;;; POS ≥ 1 but typically lower than POA.
(define (price-of-stability true-profile voting-rule)
  (doc 'export #t)
  (doc 'type '(-> PreferenceProfile VotingRule (Union Number Symbol)))
  (doc 'description "Price of stability: truthful welfare / best equilibrium welfare. Measures best-case cost of strategic behavior")
  (let* ([truthful-winner (voting-rule true-profile)]
         [truthful-welfare (social-welfare truthful-winner true-profile)]
         [equilibria (all-strategic-equilibria true-profile voting-rule)])
    (if (null? equilibria)
        'no-equilibrium
        (let* ([eq-welfares
                (map (lambda (eq)
                       (social-welfare (voting-rule eq) true-profile))
                     equilibria)]
               [best-welfare (apply max eq-welfares)])
          (if (= best-welfare 0)
              +inf.0
              (/ truthful-welfare best-welfare))))))

;;; ============================================================================
;;; Strategy-Proofness Analysis
;;; ============================================================================

(doc 'section 'strategy-proofness)

;;; count-manipulable-profiles : (Profile -> Candidate) Int Int -> (Pair Int Int)
;;; Count how many profiles are manipulable out of all possible.
;;; Returns (manipulable . total) for given number of candidates and voters.
;;; WARNING: O(N!^V * N! * V) - only for tiny instances.
(define (count-manipulable-profiles voting-rule num-candidates num-voters)
  (doc 'export #t)
  (doc 'type '(-> VotingRule Nat Nat (Pair Nat Nat)))
  (doc 'description "Count profiles where some voter can manipulate. Useful for comparing strategy-proofness of different rules")
  (let* ([candidates (map (lambda (i) (string->symbol (string (integer->char (+ i 97)))))
                          (iota num-candidates))]  ; 'a 'b 'c ...
         [all-rankings (permutations candidates)]
         [all-profiles (cartesian-product-n all-rankings num-voters)]
         [manipulable-count
          (count-if (lambda (profile)
                      ;; Check if any voter can manipulate
                      (let check-voter ([i 0])
                        (if (>= i num-voters)
                            #f
                            (if (manipulation-possible? profile voting-rule i)
                                #t
                                (check-voter (+ i 1))))))
                    all-profiles)])
    (cons manipulable-count (length all-profiles))))

;;; strategy-proofness-ratio : (Profile -> Candidate) Int Int -> Number
;;; Fraction of profiles that are NOT manipulable.
;;; 1.0 = fully strategy-proof (impossible for non-dictatorial with ≥3 candidates).
(define (strategy-proofness-ratio voting-rule num-candidates num-voters)
  (doc 'export #t)
  (doc 'type '(-> VotingRule Nat Nat Number))
  (doc 'description "Fraction of profiles where NO voter can manipulate. Higher = more strategy-proof")
  (let ([counts (count-manipulable-profiles voting-rule num-candidates num-voters)])
    (/ (- (cdr counts) (car counts)) (cdr counts))))

;;; compare-strategy-proofness : (List (Pair Symbol VotingRule)) Int Int -> (List (Pair Symbol Number))
;;; Compare strategy-proofness ratios across multiple voting rules.
(define (compare-strategy-proofness rules-alist num-candidates num-voters)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Symbol VotingRule)) Nat Nat (List (Pair Symbol Number))))
  (doc 'description "Compare multiple voting rules by strategy-proofness. Returns sorted list with most strategy-proof first")
  (let ([results
         (map (lambda (rule-pair)
                (cons (car rule-pair)
                      (strategy-proofness-ratio (cdr rule-pair) num-candidates num-voters)))
              rules-alist)])
    (sort-by (lambda (a b) (> (cdr a) (cdr b))) results)))

;;; ============================================================================
;;; Gibbard-Satterthwaite Demonstration
;;; ============================================================================

(doc 'section 'gibbard-satterthwaite)
(doc 'note "The Gibbard-Satterthwaite theorem states: Any voting rule for ≥3 candidates that is (1) surjective (every candidate can win) and (2) strategy-proof must be dictatorial. We demonstrate this by finding manipulations for common non-dictatorial rules")

;;; find-manipulation-example : (Profile -> Candidate) Int -> (List Symbol Any ...)
;;; Find a concrete example where a voter can manipulate.
;;; Returns (found voter-index true-profile fake-ballot true-winner fake-winner improvement)
;;; or (not-found).
(define (find-manipulation-example voting-rule num-candidates)
  (doc 'export #t)
  (doc 'type '(-> VotingRule Nat (Union (List ...) Symbol)))
  (doc 'description "Find a concrete manipulation example for a voting rule. Demonstrates Gibbard-Satterthwaite")
  (let* ([candidates (map (lambda (i) (string->symbol (string (integer->char (+ i 97)))))
                          (iota num-candidates))]
         [all-rankings (permutations candidates)]
         ;; Try profiles with 3 voters (minimal interesting case)
         [num-voters 3]
         [all-profiles (cartesian-product-n all-rankings num-voters)])
    (let search-profile ([profiles all-profiles])
      (if (null? profiles)
          'not-found
          (let* ([profile (car profiles)]
                 [result (find-manipulation-in-profile profile voting-rule)])
            (if (eq? result 'not-found)
                (search-profile (cdr profiles))
                result))))))

;;; find-manipulation-in-profile : PreferenceProfile (Profile -> Candidate) -> (List ...) | 'not-found
(define (find-manipulation-in-profile profile voting-rule)
  (let* ([n (length profile)]
         [true-winner (voting-rule profile)])
    (let search-voter ([i 0])
      (if (>= i n)
          'not-found
          (let* ([true-ranking (list-ref profile i)]
                 [true-utility (voter-utility true-winner true-ranking)])
            ;; Try all possible misreports
            (let search-ballot ([ballots (permutations (profile-candidates profile))])
              (if (null? ballots)
                  (search-voter (+ i 1))
                  (let* ([fake-ballot (car ballots)]
                         [fake-profile (list-set profile i fake-ballot)]
                         [fake-winner (voting-rule fake-profile)]
                         [fake-utility (voter-utility fake-winner true-ranking)])
                    (if (> fake-utility true-utility)
                        (list 'found i profile fake-ballot true-winner fake-winner
                              (- fake-utility true-utility))
                        (search-ballot (cdr ballots)))))))))))

;;; gibbard-satterthwaite-demo : -> Void
;;; Print demonstration of the theorem for common voting rules.
(define (gibbard-satterthwaite-demo)
  (doc 'export #t)
  (doc 'description "Demonstrate Gibbard-Satterthwaite by finding manipulations for Plurality, Borda, Copeland, and Schulze")
  (let ([rules `((plurality . ,plurality-winner)
                 (borda . ,borda-winner)
                 (copeland . ,copeland-winner)
                 (schulze . ,schulze-winner))])
    (for-each
     (lambda (rule-pair)
       (let* ([name (car rule-pair)]
              [rule (cdr rule-pair)]
              [example (find-manipulation-example rule 3)])
         (display (symbol->string name))
         (display ": ")
         (if (eq? example 'not-found)
             (display "No manipulation found (surprising!)\n")
             (begin
               (display "Manipulation found!\n")
               (display "  Voter ")
               (display (list-ref example 1))
               (display " in profile ")
               (display (list-ref example 2))
               (newline)
               (display "  True winner: ")
               (display (list-ref example 4))
               (display ", Fake ballot yields: ")
               (display (list-ref example 5))
               (newline)))))
     rules)))

;;; ============================================================================
;;; Example Profiles for Testing
;;; ============================================================================

(doc 'section 'examples)

;;; Example: Profile where strategic voting changes outcome
(define (strategic-example-1)
  (doc 'export #t)
  (doc 'description "Example where truthful voting gives different outcome than strategic equilibrium under plurality")
  ;; 2 voters prefer a > b > c
  ;; 2 voters prefer b > c > a
  ;; 3 voters prefer c > b > a
  ;; Truthful plurality: a=2, b=2, c=3 -> c wins
  ;; But a-voter can vote (b a c) to make b win (scores: a=1, b=3, c=3, tie-break to b)
  ;; a-voter prefers b (utility 1) to c (utility 0) - manipulation!
  (make-preference-profile
   '((a b c)
     (a b c)
     (b c a)
     (b c a)
     (c b a)
     (c b a)
     (c b a))))

;;; Example: Condorcet cycle with strategic implications
(define (strategic-example-cycle)
  (doc 'export #t)
  (doc 'description "Condorcet cycle where strategic behavior is complex")
  (condorcet-cycle-example))

;;; ============================================================================
;;; Exports Summary
;;; ============================================================================

;;; Utilities:
;;;   social-welfare, voter-utility
;;;
;;; Best Response:
;;;   find-best-responses, best-response-improves?
;;;
;;; Nash Equilibrium:
;;;   is-strategic-equilibrium?, find-strategic-equilibrium, all-strategic-equilibria
;;;
;;; Price of Anarchy/Stability:
;;;   price-of-anarchy, price-of-stability
;;;
;;; Strategy-Proofness:
;;;   count-manipulable-profiles, strategy-proofness-ratio, compare-strategy-proofness
;;;
;;; Gibbard-Satterthwaite:
;;;   find-manipulation-example, gibbard-satterthwaite-demo
;;;
;;; Examples:
;;;   strategic-example-1, strategic-example-cycle
