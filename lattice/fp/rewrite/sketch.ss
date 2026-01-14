;;; core/fp/rewrite/sketch.ss — Proof Sketch Data Structures
;;;
;;; A Sketch is a Trace with holes — an incomplete proof that can be
;;; progressively refined through tactic application. Following Gemini's
;;; design recommendation, we reuse trace infrastructure rather than
;;; duplicating it.
;;;
;;; Key insight: A hole is simply a trace step with rule-name 'unknown.
;;; This allows us to leverage verify.ss logic for checking plausibility.
;;;
;;; Sketch Structure:
;;;   ((trace . Trace)         - The underlying trace (may have holes)
;;;    (goals . (List Goal))   - Outstanding goals to discharge
;;;    (context . Alist)       - Ambient facts, hypotheses, lemmas
;;;    (history . (List Sketch)) - Previous states for undo
;;;    (focused . Nat | #f))   - Index of currently focused goal
;;;
;;; Goal Structure:
;;;   ((from . Expr)           - Expression to transform
;;;    (to . Expr)             - Target expression (or 'any for open)
;;;    (status . Symbol)       - 'open, 'discharged, 'blocked
;;;    (hints . (List Symbol)) - Suggested tactics/rules)
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/fp/rewrite/trace.ss
;;;   - core/fp/rewrite/rule.ss
;;;   - core/fp/rewrite/engine.ss
;;;   - core/fp/rewrite/verify.ss

(load "core/base/prelude.ss")
(load "lattice/fp/rewrite/trace.ss")
(load "lattice/fp/rewrite/rule.ss")
(load "lattice/fp/rewrite/engine.ss")
(load "lattice/fp/rewrite/verify.ss")

;;; ====
;;; Goal Construction
;;; ====

;;; make-goal : Expr × Expr → Goal
;;; Create a new goal from source to target expression.
;;; Use 'any as target for open-ended goals.
(define (make-goal from to)
  `((from . ,from)
    (to . ,to)
    (status . open)
    (hints . ())))

;;; make-open-goal : Expr → Goal
;;; Create an open-ended goal (simplify without specific target).
(define (make-open-goal from)
  (make-goal from 'any))

;;; ====
;;; Goal Predicates
;;; ====

;;; goal? : Any → Boolean
(define (goal? x)
  (and (pair? x)
       (assq 'from x)
       (assq 'to x)
       (assq 'status x)))

;;; goal-open? : Goal → Boolean
(define (goal-open? g)
  (eq? (goal-status g) 'open))

;;; goal-discharged? : Goal → Boolean
(define (goal-discharged? g)
  (eq? (goal-status g) 'discharged))

;;; goal-blocked? : Goal → Boolean
(define (goal-blocked? g)
  (eq? (goal-status g) 'blocked))

;;; goal-has-target? : Goal → Boolean
;;; True if goal has a specific target (not 'any).
(define (goal-has-target? g)
  (not (eq? (goal-to g) 'any)))

;;; ====
;;; Goal Accessors
;;; ====

;;; goal-from : Goal → Expr
(define (goal-from g)
  (cdr (assq 'from g)))

;;; goal-to : Goal → Expr
(define (goal-to g)
  (cdr (assq 'to g)))

;;; goal-status : Goal → Symbol
(define (goal-status g)
  (cdr (assq 'status g)))

;;; goal-hints : Goal → (List Symbol)
(define (goal-hints g)
  (let ([h (assq 'hints g)])
       (if h (cdr h) '())))

;;; ====
;;; Goal Modification
;;; ====

;;; goal-set-status : Goal × Symbol → Goal
(define (goal-set-status g status)
  `((from . ,(goal-from g))
    (to . ,(goal-to g))
    (status . ,status)
    (hints . ,(goal-hints g))))

;;; goal-set-hints : Goal × (List Symbol) → Goal
(define (goal-set-hints g hints)
  `((from . ,(goal-from g))
    (to . ,(goal-to g))
    (status . ,(goal-status g))
    (hints . ,hints)))

;;; goal-advance : Goal × Expr → Goal
;;; Move goal forward: new 'from' is the rewritten expression.
(define (goal-advance g new-from)
  `((from . ,new-from)
    (to . ,(goal-to g))
    (status . ,(goal-status g))
    (hints . ())))  ; Clear hints after advancing

;;; goal-discharge : Goal → Goal
;;; Mark goal as discharged.
(define (goal-discharge g)
  (goal-set-status g 'discharged))

;;; ====
;;; Hole Step Construction
;;; ====

;;; A hole is a trace step with 'unknown as the rule name.
;;; This marks a gap in the proof that needs to be filled.

;;; make-hole-step : Expr × Expr → Step
;;; Create a hole step representing an unproven transformation.
(define (make-hole-step from to)
  (make-trace-step from to 'unknown '() '((hole . #t))))

;;; hole-step? : Step → Boolean
;;; Check if a step is a hole (unfilled).
(define (hole-step? step)
  (eq? (step-rule step) 'unknown))

;;; fill-hole : Step × Rule × Bindings → Step
;;; Fill a hole with a concrete rule application.
(define (fill-hole hole-step rule bindings)
  (make-trace-step (step-from hole-step)
                   (step-to hole-step)
                   (rule-name rule)
                   (step-position hole-step)
                   bindings))

;;; ====
;;; Sketch Construction
;;; ====

;;; make-sketch : Expr × Alist → Sketch
;;; Create a new proof sketch for a property in a given context.
;;; The property is the expression to transform/prove.
(define (make-sketch property context)
  (let ([trace (make-trace property)]
        [goal (make-open-goal property)])
       `((trace . ,trace)
         (goals . (,goal))
         (context . ,context)
         (history . ())
         (focused . 0))))

;;; make-targeted-sketch : Expr × Expr × Alist → Sketch
;;; Create a sketch with a specific target expression.
(define (make-targeted-sketch from to context)
  (let ([trace (make-trace from)]
        [goal (make-goal from to)])
       `((trace . ,trace)
         (goals . (,goal))
         (context . ,context)
         (history . ())
         (focused . 0))))

;;; ====
;;; Sketch Predicates
;;; ====

;;; sketch? : Any → Boolean
(define (sketch? x)
  (and (pair? x)
       (assq 'trace x)
       (assq 'goals x)
       (assq 'context x)))

;;; sketch-complete? : Sketch → Boolean
;;; A sketch is complete when all goals are discharged
;;; and there are no holes remaining in the trace.
(define (sketch-complete? sk)
  (let ([goals (sketch-goals sk)]
        [trace (sketch-trace sk)])
       (and (for-all goal-discharged? goals)
            (not (sketch-has-holes? sk)))))

;;; sketch-has-holes? : Sketch → Boolean
;;; Check if the trace contains any hole steps.
(define (sketch-has-holes? sk)
  (let ([steps (trace-steps (sketch-trace sk))])
       (exists hole-step? steps)))

;;; sketch-valid? : Sketch → Boolean
;;; Check if sketch is well-formed (goals consistent with trace).
(define (sketch-valid? sk)
  (let ([trace (sketch-trace sk)]
        [goals (sketch-goals sk)])
       ;; Basic validity: trace and goals exist
       (and (trace? trace)
            (list? goals)
            (for-all goal? goals))))

;;; ====
;;; Sketch Accessors
;;; ====

;;; sketch-trace : Sketch → Trace
(define (sketch-trace sk)
  (cdr (assq 'trace sk)))

;;; sketch-goals : Sketch → (List Goal)
(define (sketch-goals sk)
  (cdr (assq 'goals sk)))

;;; sketch-context : Sketch → Alist
(define (sketch-context sk)
  (cdr (assq 'context sk)))

;;; sketch-history : Sketch → (List Sketch)
(define (sketch-history sk)
  (let ([h (assq 'history sk)])
       (if h (cdr h) '())))

;;; sketch-focused : Sketch → Nat | #f
(define (sketch-focused sk)
  (let ([f (assq 'focused sk)])
       (if f (cdr f) #f)))

;;; sketch-current-goal : Sketch → Goal | #f
;;; Get the currently focused goal.
(define (sketch-current-goal sk)
  (let ([idx (sketch-focused sk)]
        [goals (sketch-goals sk)])
       (if (and idx (< idx (length goals)))
           (list-ref goals idx)
           #f)))

;;; sketch-open-goals : Sketch → (List Goal)
;;; Get all goals that are still open.
(define (sketch-open-goals sk)
  (filter goal-open? (sketch-goals sk)))

;;; sketch-goal-count : Sketch → Nat
(define (sketch-goal-count sk)
  (length (sketch-goals sk)))

;;; sketch-open-goal-count : Sketch → Nat
(define (sketch-open-goal-count sk)
  (length (sketch-open-goals sk)))

;;; sketch-expression : Sketch → Expr
;;; Get the current expression (trace final).
(define (sketch-expression sk)
  (trace-final (sketch-trace sk)))

;;; sketch-initial : Sketch → Expr
;;; Get the initial expression.
(define (sketch-initial sk)
  (trace-initial (sketch-trace sk)))

;;; ====
;;; Sketch Modification (Pure)
;;; ====

;;; sketch-set-trace : Sketch × Trace → Sketch
(define (sketch-set-trace sk trace)
  `((trace . ,trace)
    (goals . ,(sketch-goals sk))
    (context . ,(sketch-context sk))
    (history . ,(sketch-history sk))
    (focused . ,(sketch-focused sk))))

;;; sketch-set-goals : Sketch × (List Goal) → Sketch
(define (sketch-set-goals sk goals)
  `((trace . ,(sketch-trace sk))
    (goals . ,goals)
    (context . ,(sketch-context sk))
    (history . ,(sketch-history sk))
    (focused . ,(sketch-focused sk))))

;;; sketch-set-context : Sketch × Alist → Sketch
(define (sketch-set-context sk context)
  `((trace . ,(sketch-trace sk))
    (goals . ,(sketch-goals sk))
    (context . ,context)
    (history . ,(sketch-history sk))
    (focused . ,(sketch-focused sk))))

;;; sketch-push-history : Sketch → Sketch
;;; Save current state to history (for undo).
(define (sketch-push-history sk)
  (let ([current-without-history
         `((trace . ,(sketch-trace sk))
           (goals . ,(sketch-goals sk))
           (context . ,(sketch-context sk))
           (history . ())
           (focused . ,(sketch-focused sk)))])
       `((trace . ,(sketch-trace sk))
         (goals . ,(sketch-goals sk))
         (context . ,(sketch-context sk))
         (history . ,(cons current-without-history (sketch-history sk)))
         (focused . ,(sketch-focused sk)))))

;;; sketch-set-focused : Sketch × Nat → Sketch
(define (sketch-set-focused sk idx)
  `((trace . ,(sketch-trace sk))
    (goals . ,(sketch-goals sk))
    (context . ,(sketch-context sk))
    (history . ,(sketch-history sk))
    (focused . ,idx)))

;;; ====
;;; Undo Support
;;; ====

;;; sketch-undo : Sketch → Sketch
;;; Restore previous sketch state from history.
;;; Returns unchanged sketch if history is empty.
(define (sketch-undo sk)
  (let ([history (sketch-history sk)])
       (if (null? history)
           sk
           (let ([prev (car history)]
                 [rest (cdr history)])
                ;; Restore previous state with remaining history
                `((trace . ,(cdr (assq 'trace prev)))
                  (goals . ,(cdr (assq 'goals prev)))
                  (context . ,(cdr (assq 'context prev)))
                  (history . ,rest)
                  (focused . ,(cdr (assq 'focused prev))))))))

;;; sketch-can-undo? : Sketch → Boolean
(define (sketch-can-undo? sk)
  (not (null? (sketch-history sk))))

;;; sketch-undo-depth : Sketch → Nat
(define (sketch-undo-depth sk)
  (length (sketch-history sk)))

;;; ====
;;; Tactic Application
;;; ====

;;; Tactics transform sketches. A tactic either succeeds (returning
;;; a new sketch) or fails (returning #f).

;;; Tactic = Sketch → Sketch | #f

;;; sketch-apply-tactic : Sketch × Tactic → Sketch | #f
;;; Apply a tactic to the current goal.
;;; Saves state to history before applying.
(define (sketch-apply-tactic sk tactic)
  (let ([sk-with-history (sketch-push-history sk)])
       (tactic sk-with-history)))

;;; ====
;;; Built-in Tactics
;;; ====

;;; tactic-apply-rule : Rule × Registry → Tactic
;;; Create a tactic that applies a specific rule.
(define (tactic-apply-rule rule registry)
  (lambda (sk)
          (let ([goal (sketch-current-goal sk)])
               (if (not goal)
                   #f  ; No current goal
                   (let* ([expr (goal-from goal)]
                          [result (apply-rule rule expr)])
                         (if (not result)
                             #f  ; Rule didn't match
                             (let* ([new-expr (car result)]
                                    [bindings (cdr result)]
                                    [step (make-trace-step expr new-expr
                                                           (rule-name rule) '() bindings)]
                                    [new-trace (trace-add-step (sketch-trace sk) step)]
                                    [new-goal (goal-advance goal new-expr)]
                                    ;; Check if goal is now discharged
                                    [final-goal
                                     (if (and (goal-has-target? new-goal)
                                              (full-equiv? new-expr (goal-to new-goal)))
                                         (goal-discharge new-goal)
                                         new-goal)]
                                    [new-goals (sketch-update-goal
                                                (sketch-goals sk)
                                                (sketch-focused sk)
                                                final-goal)])
                                   (sketch-set-goals
                                    (sketch-set-trace sk new-trace)
                                    new-goals))))))))

;;; tactic-reflexivity : Tactic
;;; Discharge goal if from = to (reflexive equality).
(define tactic-reflexivity
  (lambda (sk)
          (let ([goal (sketch-current-goal sk)])
               (if (not goal)
                   #f
                   (if (and (goal-has-target? goal)
                            (full-equiv? (goal-from goal) (goal-to goal)))
                       (let ([new-goals (sketch-update-goal
                                         (sketch-goals sk)
                                         (sketch-focused sk)
                                         (goal-discharge goal))])
                            (sketch-set-goals sk new-goals))
                       #f)))))

;;; tactic-assumption : Alist → Tactic
;;; Discharge goal if it matches an assumption in context.
(define (tactic-assumption assumptions)
  (lambda (sk)
          (let ([goal (sketch-current-goal sk)])
               (if (not goal)
                   #f
                   (let* ([from (goal-from goal)]
                          [to (goal-to goal)]
                          [found (assoc from assumptions)])
                         (if (and found
                                  (or (not (goal-has-target? goal))
                                      (full-equiv? (cdr found) to)))
                             (let ([new-goals (sketch-update-goal
                                               (sketch-goals sk)
                                               (sketch-focused sk)
                                               (goal-discharge goal))])
                                  (sketch-set-goals sk new-goals))
                             #f))))))

;;; tactic-split : Tactic
;;; Split current goal into subgoals for compound expressions.
;;; Handles:
;;;   - Pairs: (pair a b) = (pair c d) -> [a = c, b = d]
;;;   - And:   (and P Q) as target -> [P, Q]
;;;   - Cons:  (cons a b) = (cons c d) -> [a = c, b = d]
;;;   - List constructors with same shape
(define tactic-split
  (lambda (sk)
          (let ([goal (sketch-current-goal sk)])
               (if (not goal)
                   #f  ; No current goal
                   (let ([from (goal-from goal)]
                         [to (goal-to goal)])
                        (cond
                         ;; Case 1: Both are pair/cons expressions with same constructor
                         [(and (compound-expr? from)
                               (compound-expr? to)
                               (eq? (compound-tag from) (compound-tag to))
                               (= (compound-arity from) (compound-arity to)))
                          (split-compound-goal sk goal from to)]
                         
                         ;; Case 2: Goal target is (and P Q) - split into P and Q
                         [(and-expr? to)
                          (split-and-goal sk goal from to)]
                         
                         ;; Case 3: Goal from is (and P Q) - both must hold
                         [(and-expr? from)
                          (split-and-from-goal sk goal from to)]
                         
                         ;; No split applicable
                         [else #f]))))))

;;; ====
;;; Compound Expression Helpers
;;; ====

;;; compound-expr? : Expr -> Boolean
;;; Check if expression is a compound (pair, cons, tuple, etc.)
(define (compound-expr? expr)
  (and (pair? expr)
       (memq (car expr) '(pair cons tuple vec list))))

;;; compound-tag : Expr -> Symbol
;;; Get the constructor tag of a compound expression.
(define (compound-tag expr)
  (if (pair? expr) (car expr) #f))

;;; compound-arity : Expr -> Nat
;;; Get the number of components in a compound expression.
(define (compound-arity expr)
  (if (pair? expr) (length (cdr expr)) 0))

;;; compound-components : Expr -> (List Expr)
;;; Get the components of a compound expression.
(define (compound-components expr)
  (if (pair? expr) (cdr expr) '()))

;;; and-expr? : Expr -> Boolean
;;; Check if expression is an (and ...) conjunction.
(define (and-expr? expr)
  (and (pair? expr) (eq? (car expr) 'and)))

;;; and-conjuncts : Expr -> (List Expr)
;;; Get conjuncts from an (and ...) expression.
(define (and-conjuncts expr)
  (if (and-expr? expr) (cdr expr) '()))

;;; ====
;;; Split Implementation Helpers
;;; ====

;;; split-compound-goal : Sketch x Goal x Expr x Expr -> Sketch | #f
;;; Split a compound equality into component equalities.
;;; (pair a b) = (pair c d) becomes [a = c, b = d]
(define (split-compound-goal sk goal from to)
  (let* ([from-parts (compound-components from)]
         [to-parts (compound-components to)]
         [new-goals (map (lambda (f t) (make-goal f t))
                         from-parts to-parts)]
         [sk-with-history (sketch-push-history sk)]
         ;; Replace current goal with the new subgoals
         [all-goals (sketch-replace-goal-with-many
                     (sketch-goals sk-with-history)
                     (sketch-focused sk-with-history)
                     new-goals)])
        (sketch-set-goals sk-with-history all-goals)))

;;; split-and-goal : Sketch x Goal x Expr x Expr -> Sketch | #f
;;; When target is (and P Q), split into goals for P and Q.
(define (split-and-goal sk goal from to)
  (let* ([conjuncts (and-conjuncts to)]
         ;; Each conjunct becomes a separate goal from the same 'from'
         [new-goals (map (lambda (conj) (make-goal from conj))
                         conjuncts)]
         [sk-with-history (sketch-push-history sk)]
         [all-goals (sketch-replace-goal-with-many
                     (sketch-goals sk-with-history)
                     (sketch-focused sk-with-history)
                     new-goals)])
        (sketch-set-goals sk-with-history all-goals)))

;;; split-and-from-goal : Sketch x Goal x Expr x Expr -> Sketch | #f
;;; When from is (and P Q), both P and Q must lead to target.
(define (split-and-from-goal sk goal from to)
  (let* ([conjuncts (and-conjuncts from)]
         ;; Each conjunct is a separate path to the target
         [new-goals (map (lambda (conj) (make-goal conj to))
                         conjuncts)]
         [sk-with-history (sketch-push-history sk)]
         [all-goals (sketch-replace-goal-with-many
                     (sketch-goals sk-with-history)
                     (sketch-focused sk-with-history)
                     new-goals)])
        (sketch-set-goals sk-with-history all-goals)))

;;; sketch-replace-goal-with-many : (List Goal) x Nat x (List Goal) -> (List Goal)
;;; Replace goal at index with multiple new goals.
(define (sketch-replace-goal-with-many goals idx new-goals)
  (let loop ([gs goals] [i 0] [acc '()])
       (if (null? gs)
           (reverse acc)
           (if (= i idx)
               ;; Replace this goal with new-goals
               (loop (cdr gs) (+ i 1) (append (reverse new-goals) acc))
               (loop (cdr gs) (+ i 1) (cons (car gs) acc))))))

;;; ====
;;; Helper: Update Goal in List
;;; ====

;;; sketch-update-goal : (List Goal) × Nat × Goal → (List Goal)
(define (sketch-update-goal goals idx new-goal)
  (let loop ([gs goals] [i 0] [acc '()])
       (if (null? gs)
           (reverse acc)
           (if (= i idx)
               (loop (cdr gs) (+ i 1) (cons new-goal acc))
               (loop (cdr gs) (+ i 1) (cons (car gs) acc))))))

;;; ====
;;; Hint Generation
;;; ====

;;; sketch-hints : Sketch → (List (Symbol . String))
;;; Get suggested next steps for the current goal.
;;; Returns alist of (rule-name . description).
(define (sketch-hints sk)
  (let ([goal (sketch-current-goal sk)])
       (if (not goal)
           '()
           (let ([expr (goal-from goal)])
                (suggest-applicable-rules expr)))))

;;; suggest-applicable-rules : Expr → (List (Symbol . String))
;;; Find rules that could apply to the given expression.
;;; Requires *law-registry* to be defined (from laws.ss).
(define (suggest-applicable-rules expr)
  (if (not (bound? '*law-registry*))
      '()  ; No registry available
      (let ([all-rules (vector->list (hashtable-values *law-registry*))])
           (filter-map
            (lambda (rule)
                    (let ([result (apply-rule rule expr)])
                         (if result
                             (cons (rule-name rule)
                                   (format "~a -> ~a" expr (car result)))
                             #f)))
            all-rules))))

;;; bound? : Symbol → Boolean
;;; Check if a symbol is bound (has a value).
(define (bound? sym)
  (guard (e [else #f])
         (eval sym)
         #t))

;;; ====
;;; Goal Navigation
;;; ====

;;; sketch-next-goal : Sketch → Sketch
;;; Move focus to next open goal.
(define (sketch-next-goal sk)
  (let* ([goals (sketch-goals sk)]
         [current (sketch-focused sk)]
         [n (length goals)])
        (if (= n 0)
            sk
            (let loop ([i (+ current 1)])
                 (cond
                  [(>= i n)
                   ;; Wrap around
                   (loop 0)]
                  [(= i current)
                   ;; Back to start, no other open goals
                   sk]
                  [(goal-open? (list-ref goals i))
                   (sketch-set-focused sk i)]
                  [else
                   (loop (+ i 1))])))))

;;; sketch-prev-goal : Sketch → Sketch
;;; Move focus to previous open goal.
(define (sketch-prev-goal sk)
  (let* ([goals (sketch-goals sk)]
         [current (sketch-focused sk)]
         [n (length goals)])
        (if (= n 0)
            sk
            (let loop ([i (- current 1)])
                 (cond
                  [(< i 0)
                   (loop (- n 1))]
                  [(= i current)
                   sk]
                  [(goal-open? (list-ref goals i))
                   (sketch-set-focused sk i)]
                  [else
                   (loop (- i 1))])))))

;;; sketch-focus-goal : Sketch × Nat → Sketch
;;; Focus on a specific goal by index.
(define (sketch-focus-goal sk idx)
  (let ([goals (sketch-goals sk)])
       (if (< idx (length goals))
           (sketch-set-focused sk idx)
           sk)))

;;; ====
;;; Sketch Display
;;; ====

;;; sketch->string : Sketch → String
;;; Format a sketch for display.
(define (sketch->string sk)
  (let* ([goals (sketch-goals sk)]
         [open (sketch-open-goals sk)]
         [trace (sketch-trace sk)]
         [current (sketch-current-goal sk)])
        (format "Sketch: ~a → ~a\n  Goals: ~a open of ~a\n  Steps: ~a\n  Current: ~a"
                (sketch-initial sk)
                (if current (goal-to current) "?")
                (length open)
                (length goals)
                (trace-step-count trace)
                (if current
                    (format "~a → ~a [~a]"
                            (goal-from current)
                            (goal-to current)
                            (goal-status current))
                    "none"))))

;;; sketch-progress : Sketch → (Nat . Nat)
;;; Return (discharged . total) goal counts.
(define (sketch-progress sk)
  (let* ([goals (sketch-goals sk)]
         [discharged (filter goal-discharged? goals)])
        (cons (length discharged) (length goals))))

;;; ====
;;; Sketch Verification
;;; ====

;;; sketch-verify : Sketch → Boolean
;;; Verify that all non-hole steps are valid equivalences.
(define (sketch-verify sk)
  (let ([trace (sketch-trace sk)])
       (let ([steps (trace-steps trace)])
            (for-all
             (lambda (step)
                     (or (hole-step? step)
                         (let ([result (verify-equivalence (step-from step) (step-to step))])
                              (and (equiv-ok? result) (equiv-result result)))))
             steps))))

;;; sketch-holes : Sketch → (List Step)
;;; Get all hole steps from the sketch.
(define (sketch-holes sk)
  (filter hole-step? (trace-steps (sketch-trace sk))))

;;; sketch-hole-count : Sketch → Nat
(define (sketch-hole-count sk)
  (length (sketch-holes sk)))

;;; ====
;;; Context Operations
;;; ====

;;; sketch-add-hypothesis : Sketch × Symbol × Expr → Sketch
;;; Add a hypothesis to the context.
(define (sketch-add-hypothesis sk name expr)
  (let ([ctx (sketch-context sk)])
       (sketch-set-context sk (cons (cons name expr) ctx))))

;;; sketch-lookup-hypothesis : Sketch × Symbol → Expr | #f
(define (sketch-lookup-hypothesis sk name)
  (let ([entry (assq name (sketch-context sk))])
       (if entry (cdr entry) #f)))

;;; sketch-clear-context : Sketch → Sketch
(define (sketch-clear-context sk)
  (sketch-set-context sk '()))

;;; ====
;;; Compound Operations
;;; ====

;;; sketch-rewrite : Sketch × Symbol × Registry → Sketch | #f
;;; Apply a named rule to the current goal.
(define (sketch-rewrite sk rule-name registry)
  (let ([rule (hashtable-ref registry rule-name #f)])
       (if rule
           (sketch-apply-tactic sk (tactic-apply-rule rule registry))
           #f)))

;;; sketch-simplify : Sketch × Strategy × Nat → Sketch
;;; Apply a strategy to simplify the current goal.
(define (sketch-simplify sk strategy fuel)
  (let ([goal (sketch-current-goal sk)])
       (if (not goal)
           sk
           (let* ([expr (goal-from goal)]
                  [simplified ((repeat strategy fuel) expr)])
                 (if (equal? expr simplified)
                     sk  ; No change
                     (let* ([step (make-trace-step expr simplified 'simplify '() '())]
                            [new-trace (trace-add-step (sketch-trace sk) step)]
                            [new-goal (goal-advance goal simplified)]
                            [final-goal
                             (if (and (goal-has-target? new-goal)
                                      (full-equiv? simplified (goal-to new-goal)))
                                 (goal-discharge new-goal)
                                 new-goal)]
                            [new-goals (sketch-update-goal
                                        (sketch-goals sk)
                                        (sketch-focused sk)
                                        final-goal)]
                            [sk-with-history (sketch-push-history sk)])
                           (sketch-set-goals
                            (sketch-set-trace sk-with-history new-trace)
                            new-goals)))))))

;;; sketch-auto : Sketch × (List Rule) × Nat → Sketch
;;; Automatically apply rules until goal is discharged or fuel exhausted.
(define (sketch-auto sk rules fuel)
  (if (or (<= fuel 0)
          (sketch-complete? sk)
          (not (sketch-current-goal sk)))
      sk
      (let loop ([remaining-rules rules])
           (if (null? remaining-rules)
               sk  ; No rule applied
               (let* ([rule (car remaining-rules)]
                      [tactic (tactic-apply-rule rule '())]  ; Empty registry OK
                      [result (tactic sk)])
                     (if result
                         (sketch-auto result rules (- fuel 1))
                         (loop (cdr remaining-rules))))))))
