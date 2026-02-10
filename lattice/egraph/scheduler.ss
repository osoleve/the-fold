;;; @module egraph/scheduler
;;; @requires prelude egraph/saturation sort
;;; lattice/egraph/scheduler.ss — Rule Scheduling for Equality Saturation
;;;
;;; Intelligent rule scheduling improves saturation performance by:
;;; - Prioritizing rules that frequently match
;;; - Backing off rules that rarely fire
;;; - Focusing on recently-changed e-classes
;;;
;;; Scheduling strategies:
;;; - Simple: Apply all rules every iteration (baseline)
;;; - Backoff: Exponentially decrease priority for unproductive rules
;;; - Worklist: Only process classes marked as dirty
;;;
;;; This is Lattice code: pure scheduling logic.

(require 'prelude)
(require 'egraph/saturation)
(require 'sort)

(doc 'module 'egraph/scheduler)
(doc 'description "Rule scheduling strategies for equality saturation")
(doc 'layer 'lattice)
(doc 'tier 1)
(doc 'purity 'partial)  ; Uses mutation for tracking state

;;; ============================================================
;;; Rule Statistics
;;; ============================================================

(doc 'section 'stats)

;;; Track per-rule statistics for adaptive scheduling.
(define rule-stats-tag 'rule-stats)

(define (make-rule-stats rule)
  (doc 'type (-> Rule RuleStats))
  (doc 'description "Create statistics tracker for a rule.")
  (doc 'export #t)
  (vector rule-stats-tag
          rule           ; The rule
          0              ; Total matches
          0              ; Matches in last iteration
          0              ; Consecutive zero-match iterations
          1.0))          ; Current priority (1.0 = normal)

(define (rule-stats? x)
  (and (vector? x)
       (>= (vector-length x) 6)
       (eq? (vector-ref x 0) rule-stats-tag)))

(define (stats-rule s) (vector-ref s 1))
(define (stats-total-matches s) (vector-ref s 2))
(define (stats-last-matches s) (vector-ref s 3))
(define (stats-zero-streaks s) (vector-ref s 4))
(define (stats-priority s) (vector-ref s 5))

(define (stats-set-total-matches! s v) (vector-set! s 2 v))
(define (stats-set-last-matches! s v) (vector-set! s 3 v))
(define (stats-set-zero-streaks! s v) (vector-set! s 4 v))
(define (stats-set-priority! s v) (vector-set! s 5 v))

;;; ============================================================
;;; Scheduler Protocol
;;; ============================================================

(doc 'section 'protocol)

;;; A scheduler controls which rules run on which classes.
(define scheduler-tag 'scheduler)

(define (make-scheduler name
                        init-fn        ; (rules) → state
                        select-fn      ; (state, eg) → (rules × classes)
                        update-fn)     ; (state, rule, matches) → state
  (doc 'type (-> Symbol Fn Fn Fn Scheduler))
  (doc 'description "Create a custom scheduler.")
  (doc 'export #t)
  (vector scheduler-tag name init-fn select-fn update-fn))

(define (scheduler? x)
  (and (vector? x)
       (>= (vector-length x) 5)
       (eq? (vector-ref x 0) scheduler-tag)))

(define (scheduler-name s) (vector-ref s 1))
(define (scheduler-init s) (vector-ref s 2))
(define (scheduler-select s) (vector-ref s 3))
(define (scheduler-update s) (vector-ref s 4))

;;; ============================================================
;;; Simple Scheduler (Baseline)
;;; ============================================================

(doc 'section 'simple)

;;; Simple scheduler: apply all rules to all classes every iteration.
;;; This is the baseline - no intelligence, but predictable behavior.
(define simple-scheduler
  (make-scheduler 'simple
    ;; init: just return rules as-is
    (lambda (rules) rules)
    ;; select: all rules, all roots
    (lambda (state eg)
      (cons state (uf-roots (egraph-uf eg))))
    ;; update: no-op
    (lambda (state rule matches) state)))

;;; ============================================================
;;; Backoff Scheduler
;;; ============================================================

(doc 'section 'backoff)

;;; Backoff scheduler: reduce priority of rules that don't match.
;;; After several consecutive zero-match iterations, the rule is
;;; tried less frequently.

(define *backoff-threshold* 3)    ; Zero-match iters before backoff
(define *backoff-factor* 0.5)     ; Priority multiplier on backoff
(define *min-priority* 0.1)       ; Minimum priority floor
(define *recovery-boost* 1.5)     ; Priority boost when rule fires

(define (make-backoff-scheduler)
  (doc 'type (-> Scheduler))
  (doc 'description "Create a backoff scheduler.")
  (doc 'export #t)
  (make-scheduler 'backoff
    ;; init: wrap each rule in stats
    (lambda (rules)
      (map make-rule-stats rules))
    ;; select: filter by priority, return all roots
    (lambda (state eg)
      (let* ([active-stats (filter (lambda (s)
                                     (should-run? s))
                                   state)]
             [rules (map stats-rule active-stats)])
        (cons state (uf-roots (egraph-uf eg)))))
    ;; update: adjust priority based on match count
    (lambda (state rule matches)
      (let ([stats (find-stats state rule)])
        (when stats
          (update-stats! stats matches)))
      state)))

(define (should-run? stats)
  (doc 'type (-> RuleStats Boolean))
  (doc 'description "Check if rule should run based on priority.")
  (let ([priority (stats-priority stats)])
    ;; Always run high-priority rules
    ;; For low priority, run probabilistically based on priority
    (or (>= priority 0.5)
        (< (random 1.0) priority))))

(define (find-stats state rule)
  (doc 'type (-> (List RuleStats) Rule (Or RuleStats #f)))
  (find (lambda (s) (equal? (stats-rule s) rule)) state))

(define (update-stats! stats matches)
  (doc 'type (-> RuleStats Nat Void))
  (doc 'description "Update rule statistics based on match count.")
  ;; Update totals
  (stats-set-total-matches! stats (+ (stats-total-matches stats) matches))
  (stats-set-last-matches! stats matches)

  (if (zero? matches)
      ;; No matches - increment streak, maybe backoff
      (begin
        (stats-set-zero-streaks! stats (+ (stats-zero-streaks stats) 1))
        (when (>= (stats-zero-streaks stats) *backoff-threshold*)
          (let ([new-priority (* (stats-priority stats) *backoff-factor*)])
            (stats-set-priority! stats (max *min-priority* new-priority)))))
      ;; Had matches - reset streak, boost priority
      (begin
        (stats-set-zero-streaks! stats 0)
        (let ([new-priority (* (stats-priority stats) *recovery-boost*)])
          (stats-set-priority! stats (min 1.0 new-priority))))))

;;; ============================================================
;;; Worklist Scheduler
;;; ============================================================

(doc 'section 'worklist)

;;; Worklist scheduler: only process classes that changed.
;;; Much more efficient when saturation converges incrementally.

(define (make-worklist-scheduler)
  (doc 'type (-> Scheduler))
  (doc 'description "Create a worklist-based scheduler.")
  (doc 'export #t)
  (make-scheduler 'worklist
    ;; init: return (rules . initial-worklist)
    (lambda (rules)
      (cons rules '()))  ; Worklist starts empty
    ;; select: return rules and current worklist (or all if empty)
    (lambda (state eg)
      (let ([rules (car state)]
            [worklist (cdr state)]
            [all-roots (uf-roots (egraph-uf eg))])
        ;; If worklist empty, use all roots (first iteration)
        (let ([classes (if (null? worklist) all-roots worklist)])
          ;; Clear worklist for next iteration
          (cons (cons rules '()) classes))))
    ;; update: add affected classes to worklist
    (lambda (state rule matches)
      ;; In a full implementation, we'd track which classes were
      ;; modified by the rule application and add them to worklist.
      ;; For now, this is a stub.
      state)))

;;; ============================================================
;;; Priority Queue Scheduler
;;; ============================================================

(doc 'section 'priority)

;;; Priority scheduler: sort rules by expected productivity.
;;; Rules that matched recently get higher priority.

(define (make-priority-scheduler)
  (doc 'type (-> Scheduler))
  (doc 'description "Create a priority-based scheduler.")
  (doc 'export #t)
  (make-scheduler 'priority
    ;; init: wrap rules in stats
    (lambda (rules)
      (map make-rule-stats rules))
    ;; select: sort by priority, return all roots
    (lambda (state eg)
      (let* ([sorted (sort-by (lambda (a b)
                                  (> (stats-priority a) (stats-priority b)))
                                state)]
             [rules (map stats-rule sorted)])
        (cons state (uf-roots (egraph-uf eg)))))
    ;; update: adjust priority based on matches
    (lambda (state rule matches)
      (let ([stats (find-stats state rule)])
        (when stats
          (update-stats! stats matches)))
      state)))

;;; ============================================================
;;; Scheduled Saturation
;;; ============================================================

(doc 'section 'saturate)

;;; saturate-scheduled : EGraph × Rules × Scheduler × Config → Result
;;; Run equality saturation with custom scheduling.
(define (saturate-scheduled eg rules scheduler config)
  (doc 'type (-> EGraph (List Rule) Scheduler SaturationConfig SaturationResult))
  (doc 'description "Run saturation with custom scheduling.")
  (doc 'export #t)
  (let* ([init-fn (scheduler-init scheduler)]
         [select-fn (scheduler-select scheduler)]
         [update-fn (scheduler-update scheduler)]
         [state (init-fn rules)]
         [fuel (config-fuel config)]
         [node-limit (config-node-limit config)]
         [iter-limit (config-iter-limit config)])

    (let loop ([iteration 0]
               [total-applied 0]
               [sched-state state])
      (cond
        ;; Fuel exhausted
        [(and (> fuel 0) (>= total-applied fuel))
         (make-saturation-result 'fuel-exhausted iteration total-applied
                                 (egraph-class-count eg)
                                 (egraph-node-count eg))]

        ;; Node limit reached
        [(and (> node-limit 0) (> (egraph-node-count eg) node-limit))
         (make-saturation-result 'node-limit iteration total-applied
                                 (egraph-class-count eg)
                                 (egraph-node-count eg))]

        ;; Run one iteration
        [else
         (let* ([selection (select-fn sched-state eg)]
                [new-state (car selection)]
                [classes (cdr selection)]
                ;; Extract rules from state based on scheduler type
                ;; State can be:
                ;;   - List of rules (simple scheduler)
                ;;   - List of rule-stats (backoff, priority schedulers)
                ;;   - Pair (rules . worklist) (worklist scheduler)
                [active-rules (cond
                                ;; List of rule-stats (backoff, priority schedulers)
                                [(and (pair? new-state)
                                      (rule-stats? (car new-state)))
                                 (map stats-rule new-state)]
                                ;; Pair of (rules . worklist) (worklist scheduler)
                                ;; Check: car is a list and cdr is a list (not a pair with cdr being a pattern)
                                [(and (pair? new-state)
                                      (list? (car new-state))
                                      (list? (cdr new-state)))
                                 (car new-state)]
                                ;; List of rules directly (simple scheduler)
                                ;; A rule is (lhs . rhs), check that each element is a proper rule pair
                                [(and (pair? new-state)
                                      (pair? (car new-state)))
                                 new-state]
                                ;; Fallback
                                [else rules])]
                [applied (scheduled-iteration eg active-rules classes iter-limit)])

           ;; Update scheduler state with match counts
           (let ([updated-state
                  (fold-left (lambda (s r)
                               (update-fn s r 0))  ; Simplified: assume 0 matches
                             new-state
                             active-rules)])

             ;; Rebuild after iteration
             (egraph-rebuild! eg)

             (cond
               ;; Saturated (no changes)
               [(zero? applied)
                (make-saturation-result 'saturated iteration total-applied
                                       (egraph-class-count eg)
                                       (egraph-node-count eg))]

               ;; Continue
               [else
                (loop (+ iteration 1)
                      (+ total-applied applied)
                      updated-state)])))]))))

;;; scheduled-iteration : EGraph × Rules × Classes × Limit → Nat
;;; Apply rules to specific classes. Returns match count.
(define (scheduled-iteration eg rules classes limit)
  (doc 'type (-> EGraph (List Rule) (List ClassId) Nat Nat))
  (doc 'description "Apply rules to specific classes.")
  (let ([count 0]
        [stop #f])
    (for-each
     (lambda (class-id)
       (unless stop
         (for-each
          (lambda (rule)
            (unless stop
              (let ([matches (ematch eg (rule-lhs rule) class-id)])
                (for-each
                 (lambda (subst)
                   (unless stop
                     (let* ([rhs-term (pattern-apply subst (rule-rhs rule))]
                            [rhs-id (add-instantiated-term eg rhs-term)]
                            [class-root (egraph-find eg class-id)]
                            [rhs-root (egraph-find eg rhs-id)])
                       (unless (= class-root rhs-root)
                         (egraph-merge! eg class-id rhs-id)
                         (set! count (+ count 1))
                         (when (and (> limit 0) (>= count limit))
                           (set! stop #t))))))
                 matches))))
          rules)))
     classes)
    count))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

(doc 'section 'convenience)

;;; optimize-scheduled : Term × Rules × CostModel × Scheduler → Term
;;; Optimize using custom scheduler.
(define (optimize-scheduled term rules cost-model scheduler)
  (doc 'type (-> Term (List Rule) CostModel Scheduler Term))
  (doc 'description "Optimize with custom scheduling.")
  (doc 'export #t)
  (let ([eg (make-egraph)])
    (let ([root (egraph-add-term! eg term)])
      (saturate-scheduled eg rules scheduler (default-saturation-config))
      (let ([state (make-extraction-state eg cost-model)])
        (extract state root)))))

;;; ============================================================
;;; Debugging
;;; ============================================================

(doc 'section 'debug)

(define (scheduler->string s)
  (doc 'type (-> Scheduler String))
  (doc 'description "Convert scheduler to string.")
  (doc 'export #t)
  (format "Scheduler(~a)" (scheduler-name s)))

(define (rule-stats->string s)
  (doc 'type (-> RuleStats String))
  (doc 'description "Convert rule stats to string.")
  (doc 'export #t)
  (format "Stats(total=~a, last=~a, streak=~a, priority=~a)"
          (stats-total-matches s)
          (stats-last-matches s)
          (stats-zero-streaks s)
          (stats-priority s)))

(define (scheduler-stats-report state)
  (doc 'type (-> (List RuleStats) Void))
  (doc 'description "Print scheduler statistics report.")
  (doc 'export #t)
  (printf "Scheduler Statistics:~n")
  (for-each
   (lambda (s)
     (printf "  ~a: ~a~n"
             (pattern->string (rule-lhs (stats-rule s)))
             (rule-stats->string s)))
   state))

