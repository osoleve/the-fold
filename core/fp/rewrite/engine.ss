;;; core/fp/rewrite/engine.ss — Pattern Matching and Rewriting Engine
;;;
;;; Core engine for pattern matching, rule application, and rewrite strategies.
;;;
;;; Pattern Matching:
;;;   match-pattern : Pattern × Expr × Env → Env | #f
;;;   - Matches patterns with metavariables against expressions
;;;   - Returns binding environment on success, #f on failure
;;;
;;; Rule Application:
;;;   apply-rule : Rule × Expr → (Expr . Bindings) | #f
;;;   apply-rule-anywhere : Rule × Expr → (List (Expr × Position))
;;;   rewrite-at : Rule × Expr × Position → Expr
;;;
;;; Strategy Combinators:
;;;   Strategy = Expr → Expr | #f
;;;   seq, choice, try, repeat, topdown, bottomup
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/fp/rewrite/rule.ss
;;;   - core/fp/rewrite/trace.ss

(load "core/base/prelude.ss")
(load "core/fp/rewrite/rule.ss")
(load "core/fp/rewrite/trace.ss")

;;; ============================================================
;;; Constraint Checking
;;; ============================================================

;;; check-constraint : Symbol × Expr → Boolean
;;; Check if expression satisfies a type constraint.
;;; Errors on unknown constraints to catch typos.
(define (check-constraint constraint expr)
  (case constraint
        [(number) (number? expr)]
        [(symbol) (symbol? expr)]
        [(list) (list? expr)]
        [(pair) (pair? expr)]
        [(atom) (not (pair? expr))]
        [(any) #t]
        [else (error 'check-constraint
                     (format "Unknown constraint: ~a" constraint))]))

;;; ============================================================
;;; Pattern Matching
;;; ============================================================

;;; match-pattern : Pattern × Expr × Env → Env | #f
;;; Match a pattern against an expression, extending the environment.
;;; Returns updated environment on success, #f on failure.
(define (match-pattern pattern expr env)
  (cond
   ;; Metavariable: (?x) or (?x constraint)
   [(metavar? pattern)
    (match-metavar pattern expr env)]
   
   ;; Both are pairs: match structurally
   [(and (pair? pattern) (pair? expr))
    (let ([car-env (match-pattern (car pattern) (car expr) env)])
         (and car-env
              (match-pattern (cdr pattern) (cdr expr) car-env)))]
   
   ;; Both are null: success
   [(and (null? pattern) (null? expr)) env]
   
   ;; Literal match
   [(equal? pattern expr) env]
   
   ;; Mismatch
   [else #f]))

;;; match-metavar : Pattern × Expr × Env → Env | #f
;;; Handle metavariable matching with constraint checking.
(define (match-metavar pattern expr env)
  (let* ([var (metavar-name pattern)]
         [constraint (metavar-constraint pattern)]
         [bound (assq var env)])
        (cond
         ;; Already bound: check consistency
         [bound
          (if (equal? (cdr bound) expr)
              env
              #f)]
         
         ;; Check constraint if present
         [(and constraint (not (check-constraint constraint expr)))
          #f]
         
         ;; Bind variable
         [else (cons (cons var expr) env)])))

;;; ============================================================
;;; Template Substitution
;;; ============================================================

;;; substitute-template : Template × Env → Expr
;;; Replace metavariables in template with their bound values.
(define (substitute-template template env)
  (cond
   ;; Metavariable: look up in environment
   [(metavar? template)
    (let* ([var (metavar-name template)]
           [bound (assq var env)])
          (if bound
              (cdr bound)
              (error 'substitute-template
                     (format "Unbound variable: ~a" var))))]
   
   ;; Pair: recurse on both parts
   [(pair? template)
    (cons (substitute-template (car template) env)
          (substitute-template (cdr template) env))]
   
   ;; Atom: return as-is
   [else template]))

;;; ============================================================
;;; Rule Application
;;; ============================================================

;;; apply-rule : Rule × Expr → (Expr . Bindings) | #f
;;; Try to apply a rule at the root of an expression.
;;; Returns (result . bindings) on success, #f on failure.
(define (apply-rule rule expr)
  (let ([bindings (match-pattern (rule-lhs rule) expr '())])
       (if bindings
           (let ([result (substitute-template (rule-rhs rule) bindings)])
                (cons result bindings))
           #f)))

;;; apply-rule-name : Symbol × Expr × Registry → (Expr . Bindings) | #f
;;; Apply a named rule from a registry.
(define (apply-rule-name name expr registry)
  (let ([rule (hashtable-ref registry name #f)])
       (if rule
           (apply-rule rule expr)
           #f)))

;;; ============================================================
;;; Position-Based Rewriting
;;; ============================================================

;;; Positions are paths into an expression tree.
;;; '() means root
;;; '(0) means first element of list
;;; '(1 2) means second element's third element

;;; expr-at : Expr × Position → Expr
;;; Get the subexpression at a position.
(define (expr-at expr pos)
  (if (null? pos)
      expr
      (if (pair? expr)
          (expr-at (list-ref expr (car pos)) (cdr pos))
          (error 'expr-at "Invalid position"))))

;;; expr-set-at : Expr × Position × Expr → Expr
;;; Replace the subexpression at a position.
(define (expr-set-at expr pos new-subexpr)
  (if (null? pos)
      new-subexpr
      (if (pair? expr)
          (let ([idx (car pos)])
               (let loop ([lst expr] [i 0])
                    (cond
                     [(null? lst) '()]
                     [(= i idx)
                      (cons (expr-set-at (car lst) (cdr pos) new-subexpr)
                            (cdr lst))]
                     [else
                      (cons (car lst) (loop (cdr lst) (+ i 1)))])))
          (error 'expr-set-at "Invalid position"))))

;;; apply-rule-at : Rule × Expr × Position → Expr | #f
;;; Apply a rule at a specific position.
(define (apply-rule-at rule expr position)
  (let* ([subexpr (expr-at expr position)]
         [result (apply-rule rule subexpr)])
        (if result
            (expr-set-at expr position (car result))
            #f)))

;;; ============================================================
;;; Finding All Matches
;;; ============================================================

;;; find-all-positions : Expr → (List Position)
;;; Get all positions in an expression tree.
(define (find-all-positions expr)
  (define (helper expr path)
    (cons path
          (if (pair? expr)
              (let loop ([lst expr] [i 0])
                   (if (null? lst)
                       '()
                       (append (helper (car lst) (append path (list i)))
                               (loop (cdr lst) (+ i 1)))))
              '())))
  (helper expr '()))

;;; apply-rule-anywhere : Rule × Expr → (List (Expr × Position × Bindings))
;;; Find all positions where a rule can apply.
(define (apply-rule-anywhere rule expr)
  (let ([positions (find-all-positions expr)])
       (filter-map
        (lambda (pos)
                (let* ([subexpr (expr-at expr pos)]
                       [result (apply-rule rule subexpr)])
                      (if result
                          (list (expr-set-at expr pos (car result))
                                pos
                                (cdr result))
                          #f)))
        positions)))

;;; filter-map helper
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; ============================================================
;;; Strategy Combinators
;;; ============================================================

;;; A strategy is: Expr → Expr | #f
;;; Returns the rewritten expression on success, #f on failure.

;;; id-strategy : Expr → Expr
;;; Identity strategy (always succeeds, returns input unchanged).
(define (id-strategy expr) expr)

;;; fail-strategy : Expr → #f
;;; Failure strategy (always fails).
(define (fail-strategy expr) #f)

;;; seq : Strategy × Strategy → Strategy
;;; Sequential composition: apply s1, then s2 to result.
(define (seq s1 s2)
  (lambda (expr)
          (let ([r1 (s1 expr)])
               (and r1 (s2 r1)))))

;;; choice : Strategy × Strategy → Strategy
;;; Choice: try s1, if fails try s2.
(define (choice s1 s2)
  (lambda (expr)
          (or (s1 expr) (s2 expr))))

;;; try : Strategy → Strategy
;;; Try: apply strategy, but succeed with identity if it fails.
(define (try s)
  (lambda (expr)
          (or (s expr) expr)))

;;; *max-rewrite-fuel* : Nat
;;; Default fuel limit for repeat to prevent infinite loops.
(define *max-rewrite-fuel* 1000)

;;; repeat : Strategy [× Nat] → Strategy
;;; Repeat: apply strategy until it fails, produces no change, or runs out of fuel.
;;; Uses *max-rewrite-fuel* as default limit to prevent infinite loops.
(define repeat
  (case-lambda
   [(s) (repeat s *max-rewrite-fuel*)]
   [(s fuel)
    (lambda (expr)
            (let loop ([current expr] [remaining fuel])
                 (if (<= remaining 0)
                     current  ; Out of fuel, return current result
                     (let ([result (s current)])
                          (if (and result (not (equal? result current)))
                              (loop result (- remaining 1))
                              current)))))]))

;;; repeat-n : Strategy × Nat → Strategy
;;; Repeat at most n times.
(define (repeat-n s n)
  (lambda (expr)
          (let loop ([current expr] [count 0])
               (if (>= count n)
                   current
                   (let ([result (s current)])
                        (if result
                            (loop result (+ count 1))
                            current))))))

;;; ============================================================
;;; Traversal Strategies
;;; ============================================================

;;; map-children : Strategy × Expr → Expr | #f
;;; Apply strategy to all immediate children.
(define (map-children s expr)
  (if (pair? expr)
      (let loop ([lst expr] [acc '()] [changed? #f])
           (if (null? lst)
               (if changed? (reverse acc) expr)
               (let ([result (s (car lst))])
                    (if result
                        (loop (cdr lst) (cons result acc) #t)
                        (loop (cdr lst) (cons (car lst) acc) changed?)))))
      expr))

;;; all-children : Strategy × Expr → Expr | #f
;;; Apply strategy to all children; fail if any child fails.
(define (all-children s expr)
  (if (pair? expr)
      (let loop ([lst expr] [acc '()])
           (if (null? lst)
               (reverse acc)
               (let ([result (s (car lst))])
                    (if result
                        (loop (cdr lst) (cons result acc))
                        #f))))
      expr))

;;; topdown : Strategy → Strategy
;;; Apply strategy at root, then recurse on children.
(define (topdown s)
  (lambda (expr)
          (let ([result (s expr)])
               (let ([current (or result expr)])
                    (map-children (topdown s) current)))))

;;; bottomup : Strategy → Strategy
;;; Recurse on children first, then apply strategy at root.
(define (bottomup s)
  (lambda (expr)
          (let ([mapped (map-children (bottomup s) expr)])
               (or (s mapped) mapped))))

;;; innermost : Strategy → Strategy
;;; Apply strategy innermost-first (exhaustive).
(define (innermost s)
  (repeat (bottomup (try s))))

;;; outermost : Strategy → Strategy
;;; Apply strategy outermost-first (exhaustive).
(define (outermost s)
  (repeat (topdown (try s))))

;;; ============================================================
;;; Rule to Strategy
;;; ============================================================

;;; rule->strategy : Rule → Strategy
;;; Convert a rule into a strategy that applies it at root.
(define (rule->strategy rule)
  (lambda (expr)
          (let ([result (apply-rule rule expr)])
               (and result (car result)))))

;;; rules->strategy : (List Rule) → Strategy
;;; Try each rule in order until one succeeds.
(define (rules->strategy rules)
  (if (null? rules)
      fail-strategy
      (choice (rule->strategy (car rules))
              (rules->strategy (cdr rules)))))

;;; ============================================================
;;; Traced Rewriting
;;; ============================================================

;;; rewrite-with-trace : Rule × Expr → Trace
;;; Apply a rule and return a trace.
(define (rewrite-with-trace rule expr)
  (let ([trace (make-trace expr)]
        [result (apply-rule rule expr)])
       (if result
           (let ([step (make-trace-step expr (car result) (rule-name rule) '() (cdr result))])
                (trace-add-step trace step))
           trace)))

;;; rewrite-steps : (List Symbol) × Expr × Registry → Trace
;;; Apply a sequence of named rules, building a trace.
(define (rewrite-steps rule-names expr registry)
  (let loop ([trace (make-trace expr)]
             [current expr]
             [names rule-names])
       (if (null? names)
           trace
           (let* ([name (car names)]
                  [rule (hashtable-ref registry name #f)])
                 (if (not rule)
                     (begin
                      (error 'rewrite-steps (format "Unknown rule: ~a" name))
                      trace)
                     (let ([result (apply-rule rule current)])
                          (if result
                              (let ([step (make-trace-step current (car result) name '() (cdr result))])
                                   (loop (trace-add-step trace step) (car result) (cdr names)))
                              (begin
                               ;; Rule didn't match; stop here
                               trace))))))))

;;; simplify-with-trace : Strategy × Expr × Nat → Trace
;;; Apply a strategy exhaustively with fuel limit, building trace.
;;; Note: This is a simplified version that doesn't track individual steps.
(define (simplify-with-trace strategy expr fuel)
  (let ([trace (make-trace expr)])
       (let loop ([current expr] [remaining fuel])
            (if (= remaining 0)
                (trace-set-verified trace #f)
                (let ([result (strategy current)])
                     (if (and result (not (equal? result current)))
                         (loop result (- remaining 1))
                         (trace-set-verified
                          `((initial . ,(trace-initial trace))
                            (steps . ,(trace-steps trace))
                            (final . ,current)
                            (verified? . #f))
                          #f)))))))
