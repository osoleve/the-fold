;;; lattice/fp/game/mcdm.ss — Multi-Criteria Decision Making via Voting
;;; @module mcdm
;;; @requires voting

(load "lattice/fp/game/voting.ss")

(doc 'module 'mcdm)
(doc 'description "Multi-criteria decision making using social choice theory. Treats evaluation criteria as 'voters' that rank alternatives, then aggregates using voting rules. Connects optimization theory to social choice: instead of humans ranking candidates, we have metrics ranking implementations")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Key insight: multi-criteria optimization IS preference aggregation. Each criterion (performance, maintainability, security) ranks alternatives. Voting theory provides: (1) aggregation methods with known properties, (2) cycle detection (Condorcet paradox = no clear best), (3) robustness analysis via sensitivity. The Pareto frontier maps to undominated alternatives in social choice")

(doc 'section 'decision-problem)

;;; A decision problem consists of:
;;; - alternatives: list of options to choose from
;;; - criteria: list of criterion names
;;; - scores: alist mapping (alternative criterion) -> numeric score
;;;
;;; Example: choosing a sorting algorithm
;;;   alternatives: (quicksort mergesort heapsort)
;;;   criteria: (avg-time worst-time memory stability)
;;;   scores: ((quicksort avg-time 1.2) (quicksort worst-time 3.0) ...)

(define (make-decision-problem alternatives criteria scores)
  (doc 'export #t)
  (doc 'type '(-> (List Alternative) (List Criterion) (List (Triple Alternative Criterion Real)) DecisionProblem))
  (doc 'description "Create a multi-criteria decision problem. Alternatives are the options to rank. Criteria are the evaluation dimensions. Scores maps (alternative, criterion) pairs to numeric values. Higher scores are better by default")
  (doc 'note "Scores list format: ((alt1 crit1 val1) (alt1 crit2 val2) ...). Missing scores default to 0")
  (list 'decision-problem alternatives criteria scores))

(define (decision-problem? x)
  (and (pair? x) (eq? 'decision-problem (car x))))

(define (dp-alternatives dp)
  (doc 'export #t)
  (list-ref dp 1))

(define (dp-criteria dp)
  (doc 'export #t)
  (list-ref dp 2))

(define (dp-scores dp)
  (doc 'export #t)
  (list-ref dp 3))

;;; dp-score : DecisionProblem × Alternative × Criterion → Real
;;; Get score for a specific alternative on a specific criterion.
(define (dp-score dp alt crit)
  (doc 'export #t)
  (let ([found (find-score (dp-scores dp) alt crit)])
    (if found (caddr found) 0)))

(define (find-score scores alt crit)
  (cond
    [(null? scores) #f]
    [(and (eq? (caar scores) alt)
          (eq? (cadar scores) crit))
     (car scores)]
    [else (find-score (cdr scores) alt crit)]))

(define (cadar x) (car (cdar x)))

(doc 'section 'criterion-ranking)

;;; criterion-ranking : DecisionProblem × Criterion → (List Alternative)
;;; Rank alternatives by a single criterion (best first).
;;; Ties are broken by original order.
(define (criterion-ranking dp criterion)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Criterion (List Alternative)))
  (doc 'description "Rank alternatives by score on a single criterion (highest score first)")
  (let ([alts (dp-alternatives dp)])
    (sort (lambda (a b)
            (> (dp-score dp a criterion)
               (dp-score dp b criterion)))
          alts)))

;;; dp->profile : DecisionProblem → PreferenceProfile
;;; Convert decision problem to preference profile.
;;; Each criterion becomes a voter ranking alternatives by that criterion.
(define (dp->profile dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem PreferenceProfile))
  (doc 'description "Convert decision problem to voting profile. Each criterion becomes a 'voter' that ranks alternatives by its scores")
  (doc 'note "This is the core insight: treat criteria as voters. Aggregation rules that work for human preferences also work for metric-based rankings")
  (let ([criteria (dp-criteria dp)])
    (make-preference-profile
     (map (lambda (c) (criterion-ranking dp c))
          criteria))))

;;; dp->weighted-profile : DecisionProblem × (List Real) → PreferenceProfile
;;; Convert with criterion weights (implemented via vote repetition).
;;; Weight 2.0 means that criterion votes twice.
;;; Weights are discretized (rounded to nearest integer >= 1).
(define (dp->weighted-profile dp weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Real) PreferenceProfile))
  (doc 'description "Convert decision problem to weighted profile. Higher weights give criteria more influence. Weights are discretized to vote counts (minimum 1)")
  (doc 'note "Weight of 2.5 becomes 3 votes. For continuous weighting, use weighted-borda-scores instead")
  (let ([criteria (dp-criteria dp)])
    (if (not (= (length criteria) (length weights)))
        (error 'dp->weighted-profile "weights length must match criteria count")
        (make-preference-profile
         (apply append
                (map (lambda (c w)
                       (let ([ranking (criterion-ranking dp c)]
                             [votes (max 1 (round w))])
                         (make-list votes ranking)))
                     criteria weights))))))

(doc 'section 'aggregation-methods)

;;; mcdm-borda : DecisionProblem → Alternative
;;; Best alternative using Borda count aggregation.
(define (mcdm-borda dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Alternative))
  (doc 'description "Find best alternative using Borda count. Good for compromise solutions: rewards consistent good performance across criteria")
  (borda-winner (dp->profile dp)))

;;; mcdm-schulze : DecisionProblem → Alternative
;;; Best alternative using Schulze method.
(define (mcdm-schulze dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Alternative))
  (doc 'description "Find best alternative using Schulze method. Handles Condorcet cycles gracefully. Clone-independent: adding similar alternatives doesn't change ranking of others")
  (schulze-winner (dp->profile dp)))

;;; mcdm-copeland : DecisionProblem → Alternative
;;; Best alternative using Copeland method.
(define (mcdm-copeland dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Alternative))
  (doc 'description "Find best alternative using Copeland method (pairwise wins - losses). Good when pairwise comparisons matter")
  (copeland-winner (dp->profile dp)))

;;; mcdm-condorcet : DecisionProblem → Alternative | #f
;;; Find Condorcet winner if one exists.
(define (mcdm-condorcet dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (Maybe Alternative)))
  (doc 'description "Find Condorcet winner: alternative that beats all others pairwise. Returns #f if no such alternative exists (Condorcet paradox)")
  (condorcet-winner (dp->profile dp)))

;;; mcdm-ranking : DecisionProblem × Symbol → (List Alternative)
;;; Full ranking using specified method.
(define (mcdm-ranking dp method)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Symbol (List Alternative)))
  (doc 'description "Full ranking of alternatives. Methods: 'borda, 'schulze, 'copeland, 'minimax")
  (let ([profile (dp->profile dp)])
    (case method
      [(borda) (positional-ranking profile (borda-scores (profile-num-candidates profile)))]
      [(schulze) (schulze-ranking profile)]
      [(copeland) (copeland-ranking profile)]
      [(minimax) (minimax-ranking profile)]
      [else (error 'mcdm-ranking "Unknown method" method)])))

(doc 'section 'weighted-borda)
(doc 'note "Weighted Borda allows continuous criterion weights without discretization")

;;; weighted-borda-scores : DecisionProblem × (List Real) → (List (Pair Alternative Real))
;;; Compute weighted Borda scores for all alternatives.
;;; Each criterion contributes (weight × borda_score) to total.
(define (weighted-borda-scores dp weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Real) (List (Pair Alternative Real))))
  (doc 'description "Compute weighted Borda scores. Returns (alternative . score) pairs, highest first. Allows continuous weights unlike vote repetition")
  (let* ([alts (dp-alternatives dp)]
         [criteria (dp-criteria dp)]
         [n (length alts)]
         [borda (borda-scores n)])
    (if (not (= (length criteria) (length weights)))
        (error 'weighted-borda-scores "weights length must match criteria count")
        (let ([alt-scores
               (map (lambda (alt)
                      (cons alt
                            (apply +
                                   (map (lambda (c w)
                                          (let* ([ranking (criterion-ranking dp c)]
                                                 [pos (position-of alt ranking)])
                                            (if pos (* w (list-ref borda pos)) 0)))
                                        criteria weights))))
                    alts)])
          (sort (lambda (a b) (> (cdr a) (cdr b))) alt-scores)))))

;;; weighted-borda-winner : DecisionProblem × (List Real) → Alternative
;;; Best alternative using weighted Borda scores.
(define (weighted-borda-winner dp weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Real) Alternative))
  (doc 'description "Find best alternative using weighted Borda. Weights allow criteria to have different importance levels")
  (caar (weighted-borda-scores dp weights)))

(doc 'section 'pareto-analysis)
(doc 'note "The Pareto frontier contains alternatives not dominated by any other")

;;; dominates? : DecisionProblem × Alternative × Alternative → Boolean
;;; Does alternative a dominate alternative b?
;;; a dominates b if: a is >= b on all criteria AND a is > b on at least one.
(define (dominates? dp a b)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Alternative Alternative Boolean))
  (doc 'description "Does a Pareto-dominate b? (a >= b on all criteria, a > b on at least one)")
  (let ([criteria (dp-criteria dp)])
    (and (for-all? (lambda (c)
                     (>= (dp-score dp a c) (dp-score dp b c)))
                   criteria)
         (any? (lambda (c)
                 (> (dp-score dp a c) (dp-score dp b c)))
               criteria))))

;;; pareto-frontier : DecisionProblem → (List Alternative)
;;; Find the Pareto frontier (non-dominated alternatives).
(define (pareto-frontier dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Alternative)))
  (doc 'description "Find Pareto frontier: alternatives not dominated by any other. If choosing within frontier, need additional criteria or preferences")
  (doc 'note "Connection to voting: Smith set is the 'Condorcet-Pareto' frontier - minimal set where every member beats every non-member pairwise")
  (let ([alts (dp-alternatives dp)])
    (filter (lambda (a)
              (not (any? (lambda (b)
                           (and (not (eq? a b))
                                (dominates? dp b a)))
                         alts)))
            alts)))

;;; pareto-dominated : DecisionProblem → (List Alternative)
;;; Find dominated alternatives (NOT on the frontier).
(define (pareto-dominated dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Alternative)))
  (doc 'description "Find alternatives that are Pareto-dominated. Safe to eliminate from consideration")
  (let* ([alts (dp-alternatives dp)]
         [frontier (pareto-frontier dp)])
    (filter (lambda (a) (not (member a frontier))) alts)))

;;; is-pareto-optimal? : DecisionProblem × Alternative → Boolean
;;; Is this alternative on the Pareto frontier?
(define (is-pareto-optimal? dp alt)
  (doc 'export #t)
  (member alt (pareto-frontier dp)))

(doc 'section 'sensitivity-analysis)
(doc 'note "How stable is the choice under weight perturbations?")

;;; sensitivity-to-criterion : DecisionProblem × Criterion × (List Real) → Real
;;; How much can this criterion's weight change before the winner changes?
;;; Returns the margin as a fraction of current weight.
;;; Positive = weight can increase, negative = weight can decrease.
(define (sensitivity-to-criterion dp criterion base-weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Criterion (List Real) Real))
  (doc 'description "Compute sensitivity of winner to criterion weight. Returns fractional margin before winner changes. Larger absolute value = more robust")
  (let* ([criteria (dp-criteria dp)]
         [idx (criterion-index criterion criteria)]
         [current-weight (list-ref base-weights idx)]
         [current-winner (weighted-borda-winner dp base-weights)]
         [step 0.1]
         [max-delta 10.0])
    (if (not idx)
        (error 'sensitivity-to-criterion "Unknown criterion" criterion)
        ;; Binary search for critical weight change
        (let search ([delta step] [direction 1])
          (if (> (abs delta) max-delta)
              (* direction max-delta)  ; Very robust
              (let* ([new-weights (list-update base-weights idx
                                               (max 0.001 (+ current-weight (* direction delta))))]
                     [new-winner (weighted-borda-winner dp new-weights)])
                (if (not (eq? new-winner current-winner))
                    (* direction delta)  ; Found the breaking point
                    (search (* delta 2) direction))))))))

(define (criterion-index c criteria)
  (let loop ([cs criteria] [i 0])
    (cond
      [(null? cs) #f]
      [(eq? (car cs) c) i]
      [else (loop (cdr cs) (+ i 1))])))

(define (list-update lst idx val)
  (if (= idx 0)
      (cons val (cdr lst))
      (cons (car lst) (list-update (cdr lst) (- idx 1) val))))

;;; robustness-profile : DecisionProblem × (List Real) → (List (Pair Criterion Real))
;;; Sensitivity analysis for all criteria.
(define (robustness-profile dp base-weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Real) (List (Pair Criterion Real))))
  (doc 'description "Compute sensitivity to each criterion. Identifies which criteria most affect the outcome. Low values indicate decision hinges on that criterion")
  (map (lambda (c)
         (cons c (sensitivity-to-criterion dp c base-weights)))
       (dp-criteria dp)))

;;; min-robustness : DecisionProblem × (List Real) → Real
;;; Overall robustness: smallest sensitivity across all criteria.
(define (min-robustness dp base-weights)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Real) Real))
  (doc 'description "Overall decision robustness: smallest margin across all criteria. Higher = more confident in choice")
  (let ([profile (robustness-profile dp base-weights)])
    (apply min (map (lambda (p) (abs (cdr p))) profile))))

(doc 'section 'cycle-detection)

;;; has-condorcet-cycle? : DecisionProblem → Boolean
;;; Does this problem have a Condorcet cycle?
;;; If yes, no alternative beats all others pairwise.
(define (has-condorcet-cycle? dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Boolean))
  (doc 'description "Check for Condorcet paradox (cyclic preferences). If true, there's no clear 'best' and method choice matters more")
  (not (mcdm-condorcet dp)))

;;; cycle-participants : DecisionProblem → (List Alternative)
;;; Find alternatives involved in Condorcet cycles.
;;; These are in the Smith set but no Condorcet winner exists.
(define (cycle-participants dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List Alternative)))
  (doc 'description "Find alternatives in Condorcet cycles. If non-empty and no Condorcet winner, these alternatives are in a cycle")
  (if (not (has-condorcet-cycle? dp))
      '()
      (smith-set (dp->profile dp))))

(doc 'section 'comparison)

;;; method-agreement : DecisionProblem → (List Symbol)
;;; Which methods agree on the winner?
(define (method-agreement dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (List (Pair Symbol Alternative))))
  (doc 'description "Compare winners across methods. Agreement suggests robust choice; disagreement suggests sensitivity to aggregation method")
  (let ([borda (mcdm-borda dp)]
        [schulze (mcdm-schulze dp)]
        [copeland (mcdm-copeland dp)]
        [condorcet (mcdm-condorcet dp)])
    `((borda . ,borda)
      (schulze . ,schulze)
      (copeland . ,copeland)
      (condorcet . ,(or condorcet 'none)))))

;;; unanimous-winner? : DecisionProblem → Alternative | #f
;;; Is there a winner all methods agree on?
(define (unanimous-winner? dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem (Maybe Alternative)))
  (doc 'description "Find winner if all methods (Borda, Schulze, Copeland, Condorcet) agree. Strong endorsement if not #f")
  (let ([agreement (method-agreement dp)])
    (let ([borda (cdr (assq 'borda agreement))]
          [schulze (cdr (assq 'schulze agreement))]
          [copeland (cdr (assq 'copeland agreement))]
          [condorcet (cdr (assq 'condorcet agreement))])
      (if (and (eq? borda schulze)
               (eq? schulze copeland)
               (or (eq? condorcet 'none)
                   (eq? condorcet borda)))
          borda
          #f))))

(doc 'section 'convenience)

;;; mcdm-summary : DecisionProblem → Void (prints summary)
;;; Print a summary of the decision analysis.
(define (mcdm-summary dp)
  (doc 'export #t)
  (doc 'type '(-> DecisionProblem Void))
  (doc 'description "Print comprehensive analysis: Pareto frontier, method agreement, cycles, and rankings")
  (let ([frontier (pareto-frontier dp)]
        [agreement (method-agreement dp)]
        [cycle? (has-condorcet-cycle? dp)]
        [rankings (mcdm-ranking dp 'borda)])
    (display "=== Multi-Criteria Decision Analysis ===\n")
    (display "\nAlternatives: ") (display (dp-alternatives dp)) (newline)
    (display "Criteria: ") (display (dp-criteria dp)) (newline)
    (display "\nPareto frontier: ") (display frontier) (newline)
    (display "Dominated: ") (display (pareto-dominated dp)) (newline)
    (display "\nCondorcet cycle: ") (display (if cycle? "YES" "no")) (newline)
    (when cycle?
      (display "Cycle participants: ") (display (cycle-participants dp)) (newline))
    (display "\nMethod agreement:\n")
    (for-each (lambda (m)
                (display "  ") (display (car m)) (display ": ")
                (display (cdr m)) (newline))
              agreement)
    (display "\nBorda ranking: ") (display rankings) (newline)
    (let ([unanimous (unanimous-winner? dp)])
      (if unanimous
          (begin (display "\n★ UNANIMOUS WINNER: ") (display unanimous) (newline))
          (display "\n(No unanimous winner - methods disagree)\n")))))

(doc 'section 'examples)

;;; sorting-algorithm-example : → DecisionProblem
;;; Example: choosing a sorting algorithm by performance criteria.
(define (sorting-algorithm-example)
  (doc 'export #t)
  (make-decision-problem
   '(quicksort mergesort heapsort insertion-sort)
   '(avg-time worst-time memory stability)
   '((quicksort avg-time 4)      ; Best average (lower is worse for Borda)
     (quicksort worst-time 1)    ; Worst worst-case
     (quicksort memory 4)        ; Best memory (in-place)
     (quicksort stability 1)     ; Not stable

     (mergesort avg-time 3)
     (mergesort worst-time 4)    ; Best worst-case
     (mergesort memory 1)        ; Worst memory
     (mergesort stability 4)     ; Stable

     (heapsort avg-time 2)
     (heapsort worst-time 3)
     (heapsort memory 3)         ; In-place
     (heapsort stability 2)

     (insertion-sort avg-time 1) ; Worst average
     (insertion-sort worst-time 2)
     (insertion-sort memory 4)   ; In-place
     (insertion-sort stability 4)))) ; Stable

;;; tech-choice-example : → DecisionProblem
;;; Example: choosing a technology for a project.
(define (tech-choice-example)
  (doc 'export #t)
  (make-decision-problem
   '(react vue svelte)
   '(performance ecosystem learning-curve bundle-size)
   '((react performance 3)
     (react ecosystem 5)        ; Largest ecosystem
     (react learning-curve 2)
     (react bundle-size 2)

     (vue performance 4)
     (vue ecosystem 3)
     (vue learning-curve 4)     ; Easiest to learn
     (vue bundle-size 3)

     (svelte performance 5)     ; Best performance
     (svelte ecosystem 2)
     (svelte learning-curve 3)
     (svelte bundle-size 5))))  ; Smallest bundle

;;; ============================================================================
;;; Summary
;;; ============================================================================

;;; Decision problem:
;;;   make-decision-problem, dp-alternatives, dp-criteria, dp-score
;;;   criterion-ranking, dp->profile, dp->weighted-profile
;;;
;;; Aggregation:
;;;   mcdm-borda, mcdm-schulze, mcdm-copeland, mcdm-condorcet
;;;   mcdm-ranking, weighted-borda-scores, weighted-borda-winner
;;;
;;; Pareto analysis:
;;;   dominates?, pareto-frontier, pareto-dominated, is-pareto-optimal?
;;;
;;; Sensitivity:
;;;   sensitivity-to-criterion, robustness-profile, min-robustness
;;;
;;; Analysis:
;;;   has-condorcet-cycle?, cycle-participants
;;;   method-agreement, unanimous-winner?
;;;   mcdm-summary
;;;
;;; Examples:
;;;   sorting-algorithm-example, tech-choice-example
