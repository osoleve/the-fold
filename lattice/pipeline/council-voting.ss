(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'council)
(require 'voting)

(doc 'module 'pipeline/council-voting)
(doc 'description "Voting Theory Integration for Councils. Extends council.ss with ranked-choice voting using voting.ss. Agents submit full preference rankings over proposals, and various aggregation rules (Schulze, Borda, Copeland, plurality) determine outcomes.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Effect interpretation happens in boundary/pipeline/interpreter.ss")
(doc 'features "Ranked preference submission; Multiple voting rules (configurable); Condorcet winner detection; Strategic voting analysis")
(doc 'dependencies '(lattice/pipeline/council.ss lattice/game-theory/voting.ss))

(doc 'section 'voting-rule-selection)

(doc 'type '(-> Any Boolean))
(define (voting-rule? x)
  (memq x '(plurality borda schulze copeland condorcet)))

;;; default-voting-rule : Symbol
(define default-voting-rule 'schulze)

(doc 'section 'ranked-voting-config)

(doc 'type '(-> (List Symbol) (List Any) Symbol Boolean Boolean Nat RankedVoteConfig))
(define (make-ranked-vote-config models
                                  proposals
                                  voting-rule
                                  require-condorcet
                                  allow-ties
                                  discussion-rounds)
  (list 'ranked-vote-config
        models proposals voting-rule
        require-condorcet allow-ties discussion-rounds))

;;; ranked-vote-config? : Any -> Boolean
(define (ranked-vote-config? x)
  (and (pair? x) (eq? (car x) 'ranked-vote-config)))

;;; Accessors
(define (rvc-models cfg) (list-ref cfg 1))
(define (rvc-proposals cfg) (list-ref cfg 2))
(define (rvc-voting-rule cfg) (list-ref cfg 3))
(define (rvc-require-condorcet cfg) (list-ref cfg 4))
(define (rvc-allow-ties cfg) (list-ref cfg 5))
(define (rvc-discussion-rounds cfg) (list-ref cfg 6))

;;; ============================================================================
;;; Ranked Voting Result
;;; ============================================================================

;;; make-ranked-vote-result : Fields -> RankedVoteResult
(define (make-ranked-vote-result winner
                                  ranking
                                  profile
                                  condorcet-winner
                                  vote-details
                                  rule-used)
  (list 'ranked-vote-result
        winner ranking profile
        condorcet-winner vote-details rule-used))

;;; ranked-vote-result? : Any -> Boolean
(define (ranked-vote-result? x)
  (and (pair? x) (eq? (car x) 'ranked-vote-result)))

;;; Result accessors
(define (rvr-winner r) (list-ref r 1))
(define (rvr-ranking r) (list-ref r 2))
(define (rvr-profile r) (list-ref r 3))
(define (rvr-condorcet r) (list-ref r 4))
(define (rvr-details r) (list-ref r 5))
(define (rvr-rule r) (list-ref r 6))

;;; ============================================================================
;;; Core Voting Functions
;;; ============================================================================

;;; apply-voting-rule : Symbol × PreferenceProfile → (Pair Symbol (List Symbol))
;;; Apply the specified voting rule to a profile.
;;; Returns (winner . full-ranking) where possible.
(define (apply-voting-rule rule profile)
  (case rule
    [(plurality)
     (let ([sorted (map car (sort-by (lambda (a b) (> (cdr a) (cdr b)))
                                     (plurality-scores-all profile)))])
       (cons (car sorted) sorted))]
    [(borda)
     (let ([sorted (map car (sort-by (lambda (a b) (> (cdr a) (cdr b)))
                                     (borda-scores-all profile)))])
       (cons (car sorted) sorted))]
    [(schulze)
     (let ([ranking (schulze-ranking profile)])
       (cons (car ranking) ranking))]
    [(copeland)
     (let ([sorted (sort-by (lambda (a b)
                              (> (copeland-score a profile)
                                 (copeland-score b profile)))
                            (profile-candidates profile))])
       (cons (car sorted) sorted))]
    [(condorcet)
     ;; Condorcet with Schulze fallback
     (let ([cw (condorcet-winner profile)])
       (if cw
           (cons cw (cons cw (remove cw (profile-candidates profile))))
           (let ([ranking (schulze-ranking profile)])
             (cons (car ranking) ranking))))]
    [else
     (error 'apply-voting-rule "Unknown voting rule" rule)]))

;;; remove : a × (List a) → (List a)
(define (remove x lst)
  (filter (lambda (y) (not (equal? x y))) lst))

;;; ============================================================================
;;; Council Voting Stages
;;; ============================================================================

;;; council-vote-ranked : (List Symbol) × (List Any) × Symbol → Stage
;;; Create a ranked voting council.
;;; models: list of agent/model names (voters)
;;; proposals: list of proposals to rank
;;; rule: voting rule to use ('plurality, 'borda, 'schulze, 'copeland, 'condorcet)
(define (council-vote-ranked models proposals rule)
  (council-vote-ranked-full models proposals rule #f #f 0))

;;; council-vote-ranked-full : (List Symbol) × (List Any) × Symbol × Bool × Bool × Nat → Stage
;;; Full configuration for ranked voting.
(define (council-vote-ranked-full models proposals rule require-condorcet allow-ties discussion-rounds)
  (let ([config (make-ranked-vote-config
                 models proposals rule
                 require-condorcet allow-ties discussion-rounds)])
    (make-stage 'council-vote-ranked
                (lambda (ctx input)
                  (list 'stage-effect 'ranked-vote
                        (list config input))))))

;;; council-schulze : (List Symbol) × (List Any) → Stage
;;; Convenience: Schulze method council.
(define (council-schulze models proposals)
  (council-vote-ranked models proposals 'schulze))

;;; council-borda : (List Symbol) × (List Any) → Stage
;;; Convenience: Borda count council.
(define (council-borda models proposals)
  (council-vote-ranked models proposals 'borda))

;;; council-condorcet : (List Symbol) × (List Any) → Stage
;;; Convenience: Condorcet method (Schulze fallback).
(define (council-condorcet models proposals)
  (council-vote-ranked models proposals 'condorcet))

(doc 'section 'deliberation-protocol)
(doc 'description "A deliberation combines discussion rounds with final voting: 1. Present proposals to all agents, 2. Optional discussion rounds (sequential council), 3. Each agent submits a ranking, 4. Aggregate via voting rule")

(doc 'type '(-> String (List Any) (List Symbol) Nat Symbol Symbol Deliberation))
(define (make-deliberation topic
                           proposals
                           voters
                           discussion-rounds
                           voting-rule
                           moderator)
  (list 'deliberation
        topic proposals voters
        discussion-rounds voting-rule moderator))

;;; deliberation? : Any -> Boolean
(define (deliberation? x)
  (and (pair? x) (eq? (car x) 'deliberation)))

;;; Accessors
(define (delib-topic d) (list-ref d 1))
(define (delib-proposals d) (list-ref d 2))
(define (delib-voters d) (list-ref d 3))
(define (delib-rounds d) (list-ref d 4))
(define (delib-rule d) (list-ref d 5))
(define (delib-moderator d) (list-ref d 6))

;;; deliberate : Deliberation → Stage
;;; Execute a full deliberation process.
(define (deliberate delib)
  (make-stage 'deliberate
              (lambda (ctx input)
                (list 'stage-effect 'deliberation
                      (list delib input)))))

(doc 'section 'deliberation-builders)

(doc 'type '(-> (List Symbol) (List String) Deliberation))
(doc 'description "Create a code review deliberation")
(doc 'param 'verdicts "list of possible verdicts (e.g., '(\"approve\" \"request-changes\" \"reject\"))")
(define (code-review-deliberation reviewers verdicts)
  (make-deliberation
   "Code Review"
   verdicts
   reviewers
   1  ; One round of discussion
   'schulze
   (car reviewers)))

;;; architecture-deliberation : (List Symbol) × (List String) → Deliberation
;;; Create an architecture decision deliberation.
(define (architecture-deliberation architects options)
  (make-deliberation
   "Architecture Decision"
   options
   architects
   2  ; Two rounds for complex decisions
   'schulze
   'opus))

;;; task-prioritization : (List Symbol) × (List String) → Deliberation
;;; Create a task prioritization deliberation.
(define (task-prioritization agents tasks)
  (make-deliberation
   "Task Prioritization"
   tasks
   agents
   0  ; No discussion, just vote
   'borda  ; Borda rewards consensus
   #f))

(doc 'section 'result-analysis)

(doc 'type '(-> RankedVoteResult Alist))
(doc 'description "Analyze voting result for insights")
(define (analyze-deliberation-result result)
  (let* ([profile (rvr-profile result)]
         [winner (rvr-winner result)]
         [condorcet (rvr-condorcet result)]
         [ranking (rvr-ranking result)])
    `((winner . ,winner)
      (ranking . ,ranking)
      (condorcet-winner . ,condorcet)
      (condorcet-exists . ,(if condorcet #t #f))
      (winner-is-condorcet . ,(equal? winner condorcet))
      (rule-used . ,(rvr-rule result))
      (num-voters . ,(profile-voters profile))
      (num-options . ,(profile-num-candidates profile)))))

;;; result-has-clear-winner? : RankedVoteResult → Boolean
;;; Check if result has a clear (Condorcet) winner.
(define (result-has-clear-winner? result)
  (and (rvr-condorcet result)
       (equal? (rvr-winner result) (rvr-condorcet result))))

;;; result-controversy-score : RankedVoteResult → Number
;;; Higher score means more controversial (different rules give different winners).
(define (result-controversy-score result)
  (let* ([profile (rvr-profile result)]
         [plurality-w (plurality-winner profile)]
         [borda-w (borda-winner profile)]
         [schulze-w (schulze-winner profile)]
         [condorcet-w (condorcet-winner profile)]
         [winners (list plurality-w borda-w schulze-w)]
         [unique-winners (length (remove-duplicates winners))])
    (cond
      [(= unique-winners 1) 0]   ; Full agreement
      [(= unique-winners 2) 1]   ; Some disagreement
      [else 2])))                ; High controversy

;; remove-duplicates is provided by prelude (via council.ss → context.ss)

;;; ============================================================================
;;; Common Patterns
;;; ============================================================================

;;; quick-vote : (List Symbol) × (List Any) → Stage
;;; Fast plurality vote for simple decisions.
(define (quick-vote voters options)
  (council-vote-ranked voters options 'plurality))

;;; consensus-vote : (List Symbol) × (List Any) → Stage
;;; Schulze vote seeking Condorcet winner.
(define (consensus-vote voters options)
  (council-vote-ranked-full voters options 'schulze #t #f 1))

;;; diverse-council-vote : (List Any) → Stage
;;; Vote with diverse model set using Schulze.
(define (diverse-council-vote proposals)
  (council-schulze '(opus sonnet haiku gemini-3) proposals))

;;; ============================================================================
;;; Effect Detection
;;; ============================================================================

;;; ranked-vote-effect? : Any → Bool
(define (ranked-vote-effect? x)
  (and (stage-effect? x)
       (eq? (stage-effect-type x) 'ranked-vote)))

;;; deliberation-effect? : Any → Bool
(define (deliberation-effect? x)
  (and (stage-effect? x)
       (eq? (stage-effect-type x) 'deliberation)))
