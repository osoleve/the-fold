;;; boundary/flashmob/triage.ss — Flashmob Triage Strategy Dispatcher
;;;
;;; Provides unified interface for running triage with either strategy.
;;; Supports A/B testing by running both strategies and comparing results.
;;;
;;; Usage:
;;;   (flashmob-triage-run agents findings 'strategy 'simple)
;;;   (flashmob-triage-run agents findings 'strategy 'game)
;;;   (flashmob-triage-run agents findings)  ; Uses default
;;;   (flashmob-triage-compare agents findings)  ; Run both
;;;
;;; This is Shell code: coordinates triage execution.

(load "boundary/flashmob/triage-simple.ss")
(load "boundary/flashmob/triage-game.ss")

;;; ====
;;; Configuration
;;; ====

;;; Default strategy for triage.
(define *flashmob-default-strategy* 'simple)

;;; Default number of findings to select (0 = all).
(define *flashmob-default-k* 10)

;;; ====
;;; Strategy Dispatcher
;;; ====

;;; flashmob-triage-run : (List Symbol) (List Alist) . args -> Alist
;;; Run triage with specified or default strategy.
;;;
;;; Keyword arguments:
;;;   'strategy - 'simple | 'game | 'game/sav | 'game/cc
;;;               (default: *flashmob-default-strategy*)
;;;   'k - Number of findings to select (default: 10)
;;;
;;; Strategy guide:
;;;   - simple   : Fast, basic Borda + approval. Good baseline.
;;;   - game     : PAV + Schulze. Balanced proportionality.
;;;   - game/sav : SAV + Schulze. Filters noisy/over-approving agents.
;;;   - game/cc  : CC + Schulze. Maximizes coverage (diverse perspectives).
(define (flashmob-triage-run agent-ids findings . args)
  (let* ([strategy (triage-get-keyword-arg args 'strategy *flashmob-default-strategy*)]
         [k (triage-get-keyword-arg args 'k *flashmob-default-k*)])
    (case strategy
      [(simple)   (simple-triage agent-ids findings k)]
      [(game)     (game-triage agent-ids findings k)]
      [(game/sav) (game-triage-sav agent-ids findings k)]
      [(game/cc)  (game-triage-cc agent-ids findings k)]
      [else
       (error 'flashmob-triage-run
              "Unknown strategy: ~a (expected 'simple, 'game, 'game/sav, or 'game/cc)"
              strategy)])))

;;; triage-get-keyword-arg : (List Any) Symbol Any -> Any
;;; Extract keyword argument from argument list.
(define (triage-get-keyword-arg args key default)
  (let loop ([lst args])
    (cond
     [(null? lst) default]
     [(null? (cdr lst)) default]
     [(eq? (car lst) key) (cadr lst)]
     [else (loop (cdr lst))])))

;;; ====
;;; A/B Comparison
;;; ====

;;; flashmob-triage-compare : (List Symbol) (List Alist) . args -> Alist
;;; Run BOTH strategies and compare results.
;;;
;;; Keyword arguments:
;;;   'k - Number of findings to select (default: 10)
;;;
;;; Returns:
;;;   ((simple . <simple-result>)
;;;    (game . <game-result>)
;;;    (comparison . ((rank-correlation . <tau>)
;;;                   (top-k-overlap . <ratio>)
;;;                   (consensus-diff . <delta>)
;;;                   (runtime-ratio . <simple/game>))))
(define (flashmob-triage-compare agent-ids findings . args)
  (let* ([k (triage-get-keyword-arg args 'k *flashmob-default-k*)]
         ;; Time both strategies
         [start-simple (current-time-ns)]
         [simple-result (simple-triage agent-ids findings k)]
         [end-simple (current-time-ns)]
         [start-game (current-time-ns)]
         [game-result (game-triage agent-ids findings k)]
         [end-game (current-time-ns)]
         ;; Compute comparison metrics
         [simple-ranking (cdr (assq 'ranking simple-result))]
         [game-ranking (cdr (assq 'ranking game-result))]
         [simple-selected (cdr (assq 'selected simple-result))]
         [game-selected (cdr (assq 'selected game-result))]
         [rank-correlation (triage-kendall-tau simple-ranking game-ranking)]
         [top-k-overlap (triage-overlap-ratio simple-selected game-selected)]
         [consensus-diff (- (cdr (assq 'consensus game-result))
                           (cdr (assq 'consensus simple-result)))]
         [simple-time (- end-simple start-simple)]
         [game-time (- end-game start-game)]
         [runtime-ratio (if (> game-time 0) (/ simple-time game-time) 0)])
    `((simple . ,simple-result)
      (game . ,game-result)
      (comparison . ((rank-correlation . ,rank-correlation)
                     (top-k-overlap . ,top-k-overlap)
                     (consensus-diff . ,consensus-diff)
                     (runtime-ratio . ,runtime-ratio)
                     (simple-time-ns . ,simple-time)
                     (game-time-ns . ,game-time))))))

;;; ====
;;; Comparison Metrics
;;; ====

;;; triage-kendall-tau : (List a) (List a) -> Real
;;; Compute Kendall's tau rank correlation coefficient.
;;; Returns value in [-1, 1] where 1 = identical ranking.
(define (triage-kendall-tau ranking1 ranking2)
  (if (or (null? ranking1) (null? ranking2) (< (length ranking1) 2))
      1.0
      (let* ([items1 (triage-extract-titles ranking1)]
             [items2 (triage-extract-titles ranking2)]
             ;; Only compare items in both rankings
             [common (filter (lambda (x) (member x items2)) items1)]
             [n (length common)])
        (if (< n 2)
            1.0
            ;; Count concordant/discordant pairs
            (let* ([pairs (triage-all-pairs common)]
                   [concordant
                    (length (filter (lambda (pair)
                                     (let* ([a (car pair)]
                                            [b (cdr pair)]
                                            [pos1-a (triage-position-of a items1)]
                                            [pos1-b (triage-position-of b items1)]
                                            [pos2-a (triage-position-of a items2)]
                                            [pos2-b (triage-position-of b items2)])
                                       (or (and (< pos1-a pos1-b) (< pos2-a pos2-b))
                                           (and (> pos1-a pos1-b) (> pos2-a pos2-b)))))
                                   pairs))]
                   [total-pairs (length pairs)])
              (if (= total-pairs 0)
                  1.0
                  (/ (- (* 2 concordant) total-pairs) total-pairs)))))))

;;; triage-overlap-ratio : (List a) (List a) -> Real
;;; Compute overlap ratio between two selected sets.
;;; Returns value in [0, 1] where 1 = identical sets.
(define (triage-overlap-ratio selected1 selected2)
  (if (or (null? selected1) (null? selected2))
      (if (and (null? selected1) (null? selected2))
          1.0
          0.0)
      (let* ([titles1 (triage-extract-titles selected1)]
             [titles2 (triage-extract-titles selected2)]
             [intersection (filter (lambda (x) (member x titles2)) titles1)]
             [union-size (+ (length titles1)
                           (length (filter (lambda (x) (not (member x titles1))) titles2)))])
        (if (= union-size 0)
            1.0
            (/ (length intersection) union-size)))))

;;; triage-extract-titles : (List Alist) -> (List String)
;;; Extract titles from finding data list.
(define (triage-extract-titles findings)
  (map (lambda (f)
         (if (pair? f)
             (let ([title-pair (assq 'title f)])
               (if title-pair (cdr title-pair) ""))
             ""))
       findings))

;;; triage-position-of : a (List a) -> Int
;;; Find position in list (0-indexed).
(define (triage-position-of item lst)
  (let loop ([l lst] [i 0])
    (cond
      [(null? l) i]
      [(equal? (car l) item) i]
      [else (loop (cdr l) (+ i 1))])))

;;; triage-all-pairs : (List a) -> (List (Cons a a))
;;; Generate all unordered pairs.
(define (triage-all-pairs lst)
  (if (null? lst)
      '()
      (append (map (lambda (x) (cons (car lst) x)) (cdr lst))
              (triage-all-pairs (cdr lst)))))

;;; ====
;;; Strategy Recommendation
;;; ====

;;; flashmob-recommend-strategy : (List Symbol) (List Alist) -> Symbol
;;; Recommend which strategy to use based on session characteristics.
;;;
;;; Strategy selection logic:
;;;   - simple   : For >15 agents, <3 findings, or similar expertise
;;;   - game/cc  : Default for diverse teams (best coverage)
;;;   - game/sav : When some agents approve too many findings
;;;   - game     : Standard balanced approach
(define (flashmob-recommend-strategy agent-ids findings)
  (let ([n-agents (length agent-ids)]
        [n-findings (length findings)])
    (cond
      ;; Too many agents for exact Shapley - sampling still works, but simple is faster
      [(> n-agents 15) 'simple]
      ;; Very few findings - doesn't matter much
      [(< n-findings 3) 'simple]
      ;; Check expertise variance - if agents are similar, use simple
      [(< (triage-expertise-variance agent-ids) 0.1) 'simple]
      ;; Check for over-approving agents - use SAV to filter noise
      [(triage-noisy-approvers? agent-ids findings) 'game/sav]
      ;; Default to CC for best coverage
      [else 'game/cc])))

;;; triage-noisy-approvers? : (List Symbol) (List Alist) -> Boolean
;;; Check if any agent approves more than 80% of findings.
;;; Indicates potential for noisy/spammy voting behavior.
(define (triage-noisy-approvers? agent-ids findings)
  (if (or (null? agent-ids) (null? findings))
      #f
      (let ([threshold 0.8]
            [m (length findings)])
        (any? (lambda (agent-id)
                (let* ([ranked (flashmob-agent-rank-findings agent-id findings)]
                       [approved (filter (lambda (pair) (>= (cdr pair) 0.3)) ranked)])
                  (> (/ (length approved) m) threshold)))
              agent-ids))))

;;; any? : (a -> Bool) (List a) -> Bool
(define (any? pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (any? pred (cdr lst)))))

;;; triage-expertise-variance : (List Symbol) -> Real
;;; Compute variance of agent expertise profiles.
(define (triage-expertise-variance agent-ids)
  (if (null? agent-ids)
      0
      (let* ([vecs (map flashmob-agent-expertise-vector agent-ids)]
             ;; Compute total expertise per agent
             [totals (map (lambda (v)
                           (+ (vector-ref v 0)
                              (vector-ref v 1)
                              (vector-ref v 2)
                              (vector-ref v 3)
                              (vector-ref v 4)))
                         vecs)]
             [mean (/ (apply + totals) (length totals))]
             [variance (/ (apply + (map (lambda (t) (expt (- t mean) 2)) totals))
                         (length totals))])
        variance)))

;;; ====
;;; Timing Utility
;;; ====

;;; current-time-ns : -> Int
;;; Get current time in nanoseconds (approximation).
(define (current-time-ns)
  (let ([t (current-time)])
    (+ (* (time-second t) 1000000000)
       (time-nanosecond t))))

;;; ====
;;; Triage Result Accessors
;;; ====

;;; flashmob-triage-ranking : Alist -> (List Alist)
;;; Get the ranking from a triage result.
(define (flashmob-triage-ranking result)
  (cdr (assq 'ranking result)))

;;; flashmob-triage-selected : Alist -> (List Alist)
;;; Get the selected findings from a triage result.
(define (flashmob-triage-selected result)
  (cdr (assq 'selected result)))

;;; flashmob-triage-consensus : Alist -> Real
;;; Get the consensus score from a triage result.
(define (flashmob-triage-consensus result)
  (cdr (assq 'consensus result)))

;;; flashmob-triage-credits : Alist -> Alist
;;; Get the attribution credits from a triage result.
(define (flashmob-triage-credits result)
  (cdr (assq 'credits result)))

;;; flashmob-triage-power : Alist -> Alist
;;; Get the power indices from a triage result.
(define (flashmob-triage-power result)
  (cdr (assq 'power result)))

;;; flashmob-triage-strategy : Alist -> Symbol
;;; Get the strategy used from a triage result.
(define (flashmob-triage-strategy result)
  (cdr (assq 'strategy result)))

;;; flashmob-triage-proportionality : Alist -> Real
;;; Get the proportionality score from a triage result.
;;; Returns 0 if not computed (simple strategy).
(define (flashmob-triage-proportionality result)
  (let ([prop (assq 'proportionality result)])
    (if prop (cdr prop) 0)))

;;; flashmob-triage-coverage : Alist -> Real
;;; Get the coverage score from a triage result.
;;; Returns 1 if not computed (simple strategy).
(define (flashmob-triage-coverage result)
  (let ([cov (assq 'coverage result)])
    (if cov (cdr cov) 1.0)))

;;; flashmob-triage-notes : Alist -> Alist
;;; Get any notes/warnings from a triage result.
(define (flashmob-triage-notes result)
  (let ([notes (assq 'notes result)])
    (if notes (cdr notes) '())))
