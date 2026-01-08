;;; shell/tools/rewrite-repl.ss — Equational Reasoning REPL Commands
;;;
;;; Interactive commands for the equational reasoning and rewrite assistant.
;;; Provides REPL interface for:
;;;   - Applying rewrite rules
;;;   - Viewing transformation traces
;;;   - Verifying equivalence
;;;   - Discovering and managing laws
;;;
;;; This is Shell code: handles IO and user interaction.
;;;
;;; Dependencies:
;;;   - core/fp/rewrite/rule.ss
;;;   - core/fp/rewrite/trace.ss
;;;   - core/fp/rewrite/engine.ss
;;;   - core/fp/rewrite/laws.ss
;;;   - core/fp/rewrite/verify.ss
;;;   - shell/commands.ss (for registration)

(load "core/fp/rewrite/rule.ss")
(load "core/fp/rewrite/trace.ss")
(load "core/fp/rewrite/engine.ss")
(load "core/fp/rewrite/laws.ss")
(load "core/fp/rewrite/verify.ss")

;;; ============================================================
;;; Display Helpers
;;; ============================================================

;;; display-divider : → void
(define (display-divider)
  (display "────────────────────────────────────────────────────────────\n"))

;;; display-header : String → void
(define (display-header title)
  (newline)
  (display-divider)
  (display "  ")
  (display title)
  (newline)
  (display-divider)
  (newline))

;;; ============================================================
;;; Rewrite Commands
;;; ============================================================

;;; rewrite : Expr × Symbol → Trace
;;; Apply a named rule to an expression.
(define (rewrite expr rule-name)
  (let ([rule (get-law rule-name)])
       (if (not rule)
           (begin
            (display "  Error: Unknown rule '")
            (display rule-name)
            (display "'\n")
            (display "  Use (laws) to see available rules.\n")
            #f)
           (let ([trace (rewrite-with-trace rule expr)])
                (if (> (trace-step-count trace) 0)
                    (begin
                     (display "  ")
                     (display (trace-initial trace))
                     (display "\n  → ")
                     (display (trace-final trace))
                     (display "\n  [")
                     (display rule-name)
                     (display "]\n")
                     trace)
                    (begin
                     (display "  Rule did not match.\n")
                     trace))))))

;;; rewrite* : Expr × (List Symbol) → Trace
;;; Apply multiple rules in sequence.
(define (rewrite* expr rule-names)
  (let loop ([trace (make-trace expr)]
             [current expr]
             [names rule-names]
             [step-num 1])
       (if (null? names)
           (begin
            (display-header "Rewrite Complete")
            (display "  Initial: ")
            (display (trace-initial trace))
            (newline)
            (display "  Final:   ")
            (display current)
            (newline)
            (display "  Steps:   ")
            (display (trace-step-count trace))
            (newline)
            trace)
           (let* ([name (car names)]
                  [rule (get-law name)])
                 (if (not rule)
                     (begin
                      (display "  Error: Unknown rule '")
                      (display name)
                      (display "' at step ")
                      (display step-num)
                      (newline)
                      trace)
                     (let ([result (apply-rule rule current)])
                          (if result
                              (let* ([new-expr (car result)]
                                     [bindings (cdr result)]
                                     [step (make-trace-step current new-expr name '() bindings)]
                                     [new-trace (trace-add-step trace step)])
                                    (display "  Step ")
                                    (display step-num)
                                    (display ": ")
                                    (display name)
                                    (newline)
                                    (display "    ")
                                    (display current)
                                    (display " → ")
                                    (display new-expr)
                                    (newline)
                                    (loop new-trace new-expr (cdr names) (+ step-num 1)))
                              (begin
                               (display "  Step ")
                               (display step-num)
                               (display ": ")
                               (display name)
                               (display " - did not match\n")
                               trace))))))))

;;; simplify : Expr → Expr
;;; Apply all simplification rules exhaustively.
(define (simplify expr)
  (display "  Simplifying: ")
  (display expr)
  (newline)
  (let ([result (all-simplify expr)])
       (display "  Result: ")
       (display result)
       (newline)
       result))

;;; simplify-with : Expr × Symbol → Expr
;;; Apply rules from a specific category exhaustively.
(define (simplify-with expr category)
  (let ([strat (case category
                     [(monad) monad-simplify]
                     [(functor) functor-simplify]
                     [(monoid) monoid-simplify]
                     [(arithmetic) arith-simplify]
                     [else (begin
                            (display "  Unknown category: ")
                            (display category)
                            (newline)
                            id-strategy)])])
       (display "  Simplifying with ")
       (display category)
       (display " laws: ")
       (display expr)
       (newline)
       (let ([result (strat expr)])
            (display "  Result: ")
            (display result)
            (newline)
            result)))

;;; ============================================================
;;; Trace Display
;;; ============================================================

;;; show-trace : Trace → void
;;; Display a rewrite trace with formatting.
(define (show-trace trace)
  (display-header "Rewrite Trace")
  (display "  Initial: ")
  (display (trace-initial trace))
  (newline)
  (newline)
  
  (let ([steps (trace-steps trace)])
       (if (null? steps)
           (display "  (no rewrite steps)\n")
           (let loop ([steps steps] [i 1])
                (when (not (null? steps))
                      (let ([step (car steps)])
                           (display "  Step ")
                           (display i)
                           (display ": ")
                           (display (step-rule step))
                           (newline)
                           (display "    ")
                           (display (step-from step))
                           (newline)
                           (display "  → ")
                           (display (step-to step))
                           (newline)
                           (newline)
                           (loop (cdr steps) (+ i 1)))))))
  
  (display "  Final: ")
  (display (trace-final trace))
  (newline)
  
  (when (trace-verified? trace)
        (display "\n  [Verified: expressions are equivalent]\n")))

;;; ============================================================
;;; Verification Commands
;;; ============================================================

;;; verify : Expr × Expr → Boolean
;;; Check if two expressions are equivalent.
(define (verify e1 e2)
  (let ([result (verify-equivalence e1 e2)])
       (cond
        [(and (equiv-ok? result) (equiv-result result))
         (display "  Equivalent (")
         (display (equiv-reason result))
         (display ")\n")
         #t]
        [(equiv-ok? result)
         (display "  NOT equivalent\n")
         (let ([diff (find-difference e1 e2)])
              (when diff
                    (display "  Difference: ")
                    (display (diff-description diff))
                    (newline)))
         #f]
        [else
         (display "  Could not verify: ")
         (display result)
         (newline)
         #f])))

;;; equiv? : Expr × Expr → Boolean
;;; Quick equivalence check.
(define (equiv? e1 e2)
  (full-equiv? e1 e2))

;;; ============================================================
;;; Law Discovery Commands
;;; ============================================================

;;; laws : [Symbol] → void
;;; List available laws, optionally filtered by category.
(define laws
  (case-lambda
   [()
    (display-header "Available Rewrite Laws")
    (let ([cats (law-categories)])
         (for-each
          (lambda (cat)
                  (display "  ")
                  (display cat)
                  (display ":\n")
                  (for-each
                   (lambda (law)
                           (display "    ")
                           (display (rule-name law))
                           (newline))
                   (laws-by-category cat))
                  (newline))
          cats))]
   [(category)
    (display-header (format "~a Laws" category))
    (let ([cat-laws (laws-by-category category)])
         (if (null? cat-laws)
             (begin
              (display "  No laws in category '")
              (display category)
              (display "'\n"))
             (for-each
              (lambda (law)
                      (display "  ")
                      (display (rule-name law))
                      (display ": ")
                      (display (rule-lhs law))
                      (display " → ")
                      (display (rule-rhs law))
                      (newline))
              cat-laws)))]))

;;; describe-law : Symbol → void
;;; Show details of a specific law.
(define (describe-law name)
  (let ([law (get-law name)])
       (if (not law)
           (begin
            (display "  Unknown law: ")
            (display name)
            (newline))
           (begin
            (display-header (symbol->string name))
            (display "  Category:  ")
            (display (rule-category law))
            (newline)
            (display "  Direction: ")
            (display (rule-direction law))
            (newline)
            (display "  Pattern:   ")
            (display (rule-lhs law))
            (newline)
            (display "  Template:  ")
            (display (rule-rhs law))
            (newline)
            (let ([conds (rule-conditions law)])
                 (when (not (null? conds))
                       (display "  Conditions: ")
                       (display conds)
                       (newline)))))))

;;; suggest-rules : Expr → void
;;; Suggest rules that could apply to an expression.
(define (suggest-rules expr)
  (display-header "Applicable Rules")
  (let ([applicable '()])
       (for-each
        (lambda (law)
                (let ([result (apply-rule law expr)])
                     (when result
                           (set! applicable (cons (cons law result) applicable)))))
        (all-laws))
       (if (null? applicable)
           (display "  No rules match this expression.\n")
           (for-each
            (lambda (law-result)
                    (let* ([law (car law-result)]
                           [result (cdr law-result)])
                          (display "  ")
                          (display (rule-name law))
                          (display " → ")
                          (display (car result))
                          (newline)))
            (reverse applicable)))))

;;; ============================================================
;;; Custom Rule Definition
;;; ============================================================

;;; define-rule : Symbol × Pattern × Template × Opts... → void
;;; Define a custom rewrite rule.
(define (define-rewrite-rule name lhs rhs . opts)
  (let ([rule (apply make-rule name lhs rhs opts)])
       (if (valid-rule? rule)
           (begin
            (register-law! rule)
            (display "  Rule '")
            (display name)
            (display "' registered.\n"))
           (begin
            (display "  Error: Invalid rule - RHS variables must appear in LHS.\n")))))

;;; ============================================================
;;; Help
;;; ============================================================

;;; rewrite-help : → void
(define (rewrite-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │              EQUATIONAL REASONING ASSISTANT                        │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  Basic Commands:\n")
  (display "    (rewrite expr 'rule-name)       Apply a rule once\n")
  (display "    (rewrite* expr '(r1 r2 ...))    Apply rules in sequence\n")
  (display "    (simplify expr)                 Auto-simplify with all laws\n")
  (display "    (simplify-with expr 'category) Simplify with specific category\n")
  (display "    (show-trace trace)              Display transformation steps\n")
  (display "\n")
  (display "  Verification:\n")
  (display "    (verify e1 e2)                  Check if expressions are equal\n")
  (display "    (equiv? e1 e2)                  Quick equivalence check\n")
  (display "\n")
  (display "  Discovery:\n")
  (display "    (laws)                          List all available laws\n")
  (display "    (laws 'monad)                   List laws by category\n")
  (display "    (describe-law 'name)            Show law details\n")
  (display "    (suggest-rules expr)            Suggest applicable rules\n")
  (display "\n")
  (display "  Custom Rules:\n")
  (display "    (define-rewrite-rule 'name 'lhs 'rhs)  Define a custom rule\n")
  (display "\n")
  (display "  Categories: monad, functor, applicative, monoid, semigroup,\n")
  (display "              lambda, arithmetic, list\n")
  (display "\n")
  (display "  Example:\n")
  (display "    (rewrite '(bind (pure x) f) 'monad-left-id)\n")
  (display "    → (f x)\n")
  (display "\n"))

;;; ============================================================
;;; Command Registration (if commands.ss available)
;;; ============================================================

;;; Try to register with shell command system
(define (register-rewrite-commands!)
  (when (top-level-bound? 'register-command!)
        (register-command!
         'rewrite
         "Apply rewrite rule"
         "Apply a named rewrite rule to an expression.\nUsage: (rewrite expr 'rule-name)"
         rewrite)
        
        (register-command!
         'simplify
         "Auto-simplify expression"
         "Apply simplification rules exhaustively.\nUsage: (simplify expr)"
         simplify)
        
        (register-command!
         'laws
         "List rewrite laws"
         "Display available rewrite laws.\nUsage: (laws) or (laws 'category)"
         laws)
        
        (register-command!
         'verify
         "Check equivalence"
         "Verify two expressions are equivalent.\nUsage: (verify e1 e2)"
         verify)
        
        (register-command!
         'suggest-rules
         "Suggest applicable rules"
         "Show rules that can apply to an expression.\nUsage: (suggest-rules expr)"
         suggest-rules)
        
        (register-command!
         'describe-law
         "Describe a law"
         "Show details of a rewrite law.\nUsage: (describe-law 'name)"
         describe-law)
        
        (register-command!
         'rewrite-help
         "Rewrite system help"
         "Show help for equational reasoning commands."
         rewrite-help)))

;;; Auto-register if possible
(guard (ex [else #f])
       (register-rewrite-commands!))
