;;; boundary/tools/proof-repl.ss --- Interactive Proof Sketcher REPL Commands
;;;
;;; Provides REPL commands for interactive equational reasoning and proof
;;; construction. Users can incrementally build proofs by applying laws,
;;; receiving hints, and navigating between goals.
;;;
;;; Commands:
;;;   (sketch '(= lhs rhs))      - Start a new proof sketch
;;;   (goals)                    - List remaining goals
;;;   (focus n)                  - Focus on goal n
;;;   (apply 'law-name)          - Apply a law to current goal
;;;   (hint)                     - Get suggestions for next step
;;;   (undo)                     - Undo last step
;;;   (qed)                      - Finalize proof
;;;   (show)                     - Display current proof state
;;;   (trace)                    - Show steps taken so far
;;;   (proof-help)               - Show documentation
;;;
;;; This is Shell code: handles IO, maintains session state.
;;;
;;; Dependencies:
;;;   - core/fp/rewrite/goals.ss
;;;   - core/fp/rewrite/sketch.ss
;;;   - core/fp/rewrite/proof-tactics.ss
;;;   - core/fp/rewrite/laws.ss
;;;   - boundary/commands.ss (for registration)

(load "lattice/fp/rewrite/goals.ss")
(load "lattice/fp/rewrite/sketch.ss")
(load "lattice/fp/rewrite/proof-tactics.ss")
(load "lattice/fp/rewrite/laws.ss")

;;; ====
;;; Session State
;;; ====

;;; Global state for the current proof sketch
(define *current-sketch* #f)

;;; History of completed proofs (for reference)
(define *proof-history* '())

;;; Maximum history entries to retain
(define *max-proof-history* 10)

;;; ====
;;; Display Helpers
;;; ====

;;; display-divider : -> void
(define (display-divider)
  (display "----\n"))

;;; display-header : String -> void
(define (display-header title)
  (newline)
  (display-divider)
  (display "  ")
  (display title)
  (newline)
  (display-divider)
  (newline))

;;; display-box : String -> void
(define (display-box title)
  (newline)
  (display "  +----+\n")
  (display "  |  ")
  (display title)
  (let* ([title-len (string-length title)]
         [padding (max 0 (- 54 title-len))])
        (display (make-string padding #\space)))
  (display "  |\n")
  (display "  +----+\n")
  (newline))

;;; truncate-expr : Expr x Nat -> String
(define (truncate-expr expr max-len)
  (let* ([str (format "~a" expr)]
         [len (string-length str)])
        (cond
         ;; BUGFIX: Handle max-len too small to truncate meaningfully
         [(<= max-len 3) (if (> len max-len) "..." (substring str 0 (min len max-len)))]
         [(> len max-len) (string-append (substring str 0 (- max-len 3)) "...")]
         [else str])))

;;; goal-status-symbol : Symbol -> String
(define (goal-status-symbol status)
  (case status
        [(open) "[open]"]
        [(discharged) "[done]"]
        [(blocked) "[blocked]"]
        [else "[??]"]))

;;; ====
;;; Sketch Command
;;; ====

;;; parse-equality : Expr -> (lhs . rhs) | #f
;;; Parse an equality expression like (= lhs rhs)
(define (parse-equality expr)
  (cond
   ;; (= lhs rhs) form
   [(and (pair? expr)
         (eq? (car expr) '=)
         (= (length expr) 3))
    (cons (cadr expr) (caddr expr))]
   ;; (== lhs rhs) form
   [(and (pair? expr)
         (eq? (car expr) '==)
         (= (length expr) 3))
    (cons (cadr expr) (caddr expr))]
   ;; (lhs = rhs) infix form
   [(and (pair? expr)
         (= (length expr) 3)
         (eq? (cadr expr) '=))
    (cons (car expr) (caddr expr))]
   [else #f]))

;;; sketch : Expr -> void
;;; Start a new proof sketch for an equality.
(define (sketch property)
  (let ([eq-parts (parse-equality property)])
       (if (not eq-parts)
           (begin
            (display "\n  Error: Expected equality expression.\n")
            (display "  Examples:\n")
            (display "    (sketch '(= (bind (pure x) f) (f x)))\n")
            (display "    (sketch '(== (+ 0 x) x))\n")
            (newline))
           (let* ([lhs (car eq-parts)]
                  [rhs (cdr eq-parts)]
                  [sk (make-targeted-sketch lhs rhs '())])
                 (set! *current-sketch* sk)
                 (display-box "NEW PROOF SKETCH")
                 (display "  Goal: ")
                 (display (truncate-expr lhs 45))
                 (newline)
                 (display "      = ")
                 (display (truncate-expr rhs 45))
                 (newline)
                 (newline)
                 (display "  Use (apply 'law-name) to apply a law.\n")
                 (display "  Use (hint) for suggestions.\n")
                 (display "  Use (goals) to see remaining goals.\n")
                 (newline)
                 (void)))))

;;; ====
;;; Goals Command
;;; ====

;;; goals : -> void
;;; Display all remaining goals.
(define (goals)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch. Use (sketch '(= lhs rhs)) to start.\n\n")
      (let* ([sk *current-sketch*]
             [all-goals (sketch-goals sk)]
             [focused (sketch-focused sk)]
             [open-count (sketch-open-goal-count sk)]
             [total-count (sketch-goal-count sk)])
            (display-header (format "GOALS (~a/~a remaining)" open-count total-count))
            (if (null? all-goals)
                (display "  (no goals)\n")
                (let loop ([gs all-goals] [i 0])
                     (when (not (null? gs))
                           (let* ([g (car gs)]
                                  [status (goal-status g)]
                                  [from (goal-from g)]
                                  [to (goal-to g)]
                                  [is-focused (= i focused)])
                                 (display (if is-focused " >> " "    "))
                                 (display (format "~a. " i))
                                 (display (goal-status-symbol status))
                                 (display " ")
                                 (display (truncate-expr from 35))
                                 (display "\n       = ")
                                 (display (truncate-expr to 35))
                                 (newline)
                                 (when (goal-open? g)
                                       (let ([hints (goal-hints g)])
                                            (unless (null? hints)
                                                    (display "       hints: ")
                                                    (display hints)
                                                    (newline))))
                                 (newline)
                                 (loop (cdr gs) (+ i 1))))))
            (when (zero? open-count)
                  (display "  All goals discharged! Use (qed) to finalize.\n"))
            (newline))))

;;; ====
;;; Focus Command
;;; ====

;;; focus : Nat -> void
;;; Focus on a specific goal by index.
(define (focus n)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let ([total (sketch-goal-count *current-sketch*)])
           (if (or (< n 0) (>= n total))
               (begin
                (display "\n  Invalid goal index. ")
                (display (format "Valid range: 0-~a\n\n" (- total 1))))
               (begin
                (set! *current-sketch* (sketch-focus-goal *current-sketch* n))
                (let* ([g (sketch-current-goal *current-sketch*)]
                       [status (goal-status g)])
                      (display "\n  Focused on goal ")
                      (display n)
                      (display ": ")
                      (display (goal-status-symbol status))
                      (newline)
                      (display "    ")
                      (display (truncate-expr (goal-from g) 50))
                      (newline)
                      (display "  = ")
                      (display (truncate-expr (goal-to g) 50))
                      (newline)
                      (newline)))))))

;;; ====
;;; Apply Command
;;; ====

;;; proof-apply : Symbol -> void
;;; Apply a named law to the current goal.
(define (proof-apply law-name)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let ([current-goal (sketch-current-goal *current-sketch*)])
           (if (not current-goal)
               (display "\n  No current goal. Use (focus n) to select one.\n\n")
               (if (goal-discharged? current-goal)
                   (display "\n  Current goal is already discharged.\n\n")
                   (let ([law (get-law law-name)])
                        (if (not law)
                            (begin
                             (display "\n  Unknown law: ")
                             (display law-name)
                             (display "\n  Use (laws) to see available laws.\n\n"))
                            (let ([result (sketch-rewrite *current-sketch* law-name *law-registry*)])
                                 (if (not result)
                                     (begin
                                      (display "\n  Law '")
                                      (display law-name)
                                      (display "' did not apply to:\n    ")
                                      (display (truncate-expr (goal-from current-goal) 50))
                                      (display "\n\n  Hint: Use (hint) to see applicable laws.\n\n"))
                                     (begin
                                      (set! *current-sketch* result)
                                      (let* ([new-goal (sketch-current-goal result)]
                                             [new-from (if new-goal (goal-from new-goal) #f)])
                                            (display "\n  Applied: ")
                                            (display law-name)
                                            (newline)
                                            (if (and new-goal (goal-discharged? new-goal))
                                                (begin
                                                 (display "  GOAL DISCHARGED!\n")
                                                 (let ([open (sketch-open-goal-count result)])
                                                      (if (zero? open)
                                                          (display "  All goals complete! Use (qed) to finalize.\n")
                                                          (begin
                                                           (display (format "  ~a goal(s) remaining.\n" open))))))
                                                (when new-from
                                                      (display "  Now proving:\n    ")
                                                      (display (truncate-expr new-from 50))
                                                      (newline)))
                                            (newline))))))))))))

;;; ====
;;; Hint Command
;;; ====

;;; hint : -> void
;;; Suggest applicable laws for the current goal.
(define (hint)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let ([current-goal (sketch-current-goal *current-sketch*)])
           (if (not current-goal)
               (display "\n  No current goal.\n\n")
               (if (goal-discharged? current-goal)
                   (display "\n  Current goal is already discharged. Use (focus n) for another goal.\n\n")
                   (let* ([expr (goal-from current-goal)]
                          [applicable (find-applicable-laws expr)])
                         (display-header "HINTS")
                         (if (null? applicable)
                             (begin
                              (display "  No laws directly apply to the current expression.\n")
                              (display "  Try:\n")
                              (display "    - (laws) to see all available laws\n")
                              (display "    - (describe-law 'name) for law details\n"))
                             (begin
                              (display "  Applicable laws:\n\n")
                              (for-each
                               (lambda (entry)
                                       (let ([name (car entry)]
                                             [result (cdr entry)])
                                            (display "    (apply '")
                                            (display name)
                                            (display ")\n")
                                            (display "      => ")
                                            (display (truncate-expr result 45))
                                            (newline)
                                            (newline)))
                               applicable)))
                         (newline)))))))

;;; find-applicable-laws : Expr -> (List (name . result))
;;; Find laws that can apply to the expression.
(define (find-applicable-laws expr)
  (let ([laws-list (all-laws)])
       (filter-map
        (lambda (law)
                (let ([result (apply-rule law expr)])
                     (if result
                         (cons (rule-name law) (car result))
                         #f)))
        laws-list)))

;;; filter-map provided by prelude

;;; ====
;;; Undo Command
;;; ====

;;; undo : -> void
;;; Undo the last step in the proof.
(define (undo)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (if (not (sketch-can-undo? *current-sketch*))
          (display "\n  Nothing to undo.\n\n")
          (begin
           (set! *current-sketch* (sketch-undo *current-sketch*))
           (display "\n  Undid last step.\n")
           (let ([g (sketch-current-goal *current-sketch*)])
                (when g
                      (display "  Current goal:\n    ")
                      (display (truncate-expr (goal-from g) 50))
                      (display "\n  = ")
                      (display (truncate-expr (goal-to g) 50))
                      (newline)))
           (display (format "  (~a more undo(s) available)\n\n"
                            (sketch-undo-depth *current-sketch*)))))))

;;; ====
;;; QED Command
;;; ====

;;; qed : -> void
;;; Finalize the proof if all goals are discharged.
(define (qed)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let ([open-count (sketch-open-goal-count *current-sketch*)])
           (if (> open-count 0)
               (begin
                (display "\n  Cannot finalize: ")
                (display open-count)
                (display " goal(s) remain.\n")
                (display "  Use (goals) to see remaining goals.\n\n"))
               (begin
                (display-box "QED - PROOF COMPLETE")
                (display "  Initial:  ")
                (display (truncate-expr (sketch-initial *current-sketch*) 45))
                (newline)
                (display "  Final:    ")
                (display (truncate-expr (sketch-expression *current-sketch*) 45))
                (newline)
                (display "  Steps:    ")
                (display (trace-step-count (sketch-trace *current-sketch*)))
                (newline)
                (newline)
                ;; Save to history
                (set! *proof-history*
                      (take *max-proof-history*
                                  (cons *current-sketch* *proof-history*)))
                (set! *current-sketch* #f)
                (display "  Proof saved to history.\n")
                (display "  Start a new proof with (sketch '(= lhs rhs)).\n")
                (newline))))))

;;; ====
;;; Show Command
;;; ====

;;; show : -> void
;;; Display the current proof state.
(define (show)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch. Use (sketch '(= lhs rhs)) to start.\n\n")
      (let* ([sk *current-sketch*]
             [trace (sketch-trace sk)]
             [current-goal (sketch-current-goal sk)]
             [progress (sketch-progress sk)])
            (display-box "PROOF STATE")
            (display "  Initial expression:\n    ")
            (display (truncate-expr (sketch-initial sk) 55))
            (newline)
            (newline)
            (display "  Current expression:\n    ")
            (display (truncate-expr (sketch-expression sk) 55))
            (newline)
            (newline)
            (display (format "  Progress: ~a/~a goals discharged\n"
                             (car progress) (cdr progress)))
            (display (format "  Steps taken: ~a\n" (trace-step-count trace)))
            (display (format "  Undo depth: ~a\n" (sketch-undo-depth sk)))
            (newline)
            (when current-goal
                  (display "  Current goal ")
                  (display (goal-status-symbol (goal-status current-goal)))
                  (display ":\n    ")
                  (display (truncate-expr (goal-from current-goal) 50))
                  (display "\n  = ")
                  (display (truncate-expr (goal-to current-goal) 50))
                  (newline))
            (newline))))

;;; ====
;;; Trace Command
;;; ====

;;; proof-trace : -> void
;;; Show the steps taken so far.
(define (proof-trace)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let* ([sk *current-sketch*]
             [trace (sketch-trace sk)]
             [steps (trace-steps trace)])
            (display-header "PROOF TRACE")
            (display "  Initial: ")
            (display (truncate-expr (trace-initial trace) 45))
            (newline)
            (newline)
            (if (null? steps)
                (display "  (no steps taken yet)\n")
                (let loop ([ss steps] [i 1])
                     (when (not (null? ss))
                           (let* ([step (car ss)]
                                  [rule-name (step-rule step)]
                                  [from (step-from step)]
                                  [to (step-to step)])
                                 (display (format "  ~a. [~a]\n" i rule-name))
                                 (display "     ")
                                 (display (truncate-expr from 50))
                                 (display "\n  => ")
                                 (display (truncate-expr to 50))
                                 (newline)
                                 (newline)
                                 (loop (cdr ss) (+ i 1))))))
            (display "  Current: ")
            (display (truncate-expr (trace-final trace) 45))
            (newline)
            (newline))))

;;; ====
;;; Help Command
;;; ====

;;; proof-help : -> void
;;; Show help for proof sketcher commands.
(define (proof-help)
  (display "\n")
  (display "  +----+\n")
  (display "  |              PROOF SKETCHER - INTERACTIVE HELP           |\n")
  (display "  +----+\n")
  (display "\n")
  (display "  Starting a Proof:\n")
  (display "    (sketch '(= lhs rhs))      Start proving lhs = rhs\n")
  (display "    (sketch '(== lhs rhs))     Same, alternative syntax\n")
  (display "\n")
  (display "  Working with Goals:\n")
  (display "    (goals)                    List all remaining goals\n")
  (display "    (focus n)                  Focus on goal number n\n")
  (display "    (show)                     Display current proof state\n")
  (display "\n")
  (display "  Applying Laws:\n")
  (display "    (apply 'law-name)          Apply named law to current goal\n")
  (display "    (hint)                     Get suggestions for applicable laws\n")
  (display "    (laws)                     List all available laws\n")
  (display "    (describe-law 'name)       Show details of a specific law\n")
  (display "\n")
  (display "  Navigation:\n")
  (display "    (undo)                     Undo last step\n")
  (display "    (trace)                    Show proof steps taken so far\n")
  (display "\n")
  (display "  Completing a Proof:\n")
  (display "    (qed)                      Finalize when all goals discharged\n")
  (display "\n")
  (display "  Example Session:\n")
  (display "    > (sketch '(= (bind (pure x) f) (f x)))\n")
  (display "    > (hint)\n")
  (display "    > (apply 'monad-left-id)\n")
  (display "    > (qed)\n")
  (display "\n")
  (display "  Available Law Categories:\n")
  (display "    monad, functor, applicative, monoid, semigroup,\n")
  (display "    arithmetic, lambda, list\n")
  (display "\n"))

;;; ====
;;; Additional Convenience Commands
;;; ====

;;; simplify-goal : -> void
;;; Apply automatic simplification to the current goal.
(define (simplify-goal)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (let ([current-goal (sketch-current-goal *current-sketch*)])
           (if (not current-goal)
               (display "\n  No current goal.\n\n")
               (if (goal-discharged? current-goal)
                   (display "\n  Current goal is already discharged.\n\n")
                   (let* ([result (sketch-simplify *current-sketch* all-simplify 100)])
                         (if (equal? result *current-sketch*)
                             (display "\n  No simplifications found.\n\n")
                             (begin
                              (set! *current-sketch* result)
                              (let ([new-goal (sketch-current-goal result)])
                                   (display "\n  Simplified.\n")
                                   (if (and new-goal (goal-discharged? new-goal))
                                       (display "  GOAL DISCHARGED!\n")
                                       (when new-goal
                                             (display "  Now proving:\n    ")
                                             (display (truncate-expr (goal-from new-goal) 50))
                                             (newline)))
                                   (newline))))))))))

;;; abort-proof : -> void
;;; Abandon the current proof sketch.
(define (abort-proof)
  (if (not *current-sketch*)
      (display "\n  No active proof sketch.\n\n")
      (begin
       (set! *current-sketch* #f)
       (display "\n  Proof sketch abandoned.\n\n"))))

;;; proof-history-cmd : -> void
;;; Show completed proof history.
(define (proof-history-cmd)
  (display-header "PROOF HISTORY")
  (if (null? *proof-history*)
      (display "  (no completed proofs)\n")
      (let loop ([ps *proof-history*] [i 1])
           (when (not (null? ps))
                 (let* ([sk (car ps)]
                        [init (sketch-initial sk)]
                        [steps (trace-step-count (sketch-trace sk))])
                       (display (format "  ~a. " i))
                       (display (truncate-expr init 45))
                       (display (format " (~a steps)\n" steps))
                       (loop (cdr ps) (+ i 1))))))
  (newline))

;;; ====
;;; Aliases
;;; ====

;;; Make 'apply work but avoid shadowing the built-in
(define proof-apply-law proof-apply)

;;; Alias 'trace for proof context (avoid conflict with debug-repl)
(define proof-steps proof-trace)

;;; ====
;;; Command Registration
;;; ====

;;; register-proof-commands! : -> void
;;; Register proof sketcher commands with the boundary command system.
(define (register-proof-commands!)
  (when (top-level-bound? 'register-command!)
        (register-command!
         'sketch
         "Start proof sketch"
         "Begin a new proof sketch for an equality.\nUsage: (sketch '(= lhs rhs))"
         sketch)
        
        (register-command!
         'goals
         "List proof goals"
         "Display all remaining goals in the current proof.\nUsage: (goals)"
         goals)
        
        (register-command!
         'focus
         "Focus on goal"
         "Focus on a specific goal by index.\nUsage: (focus 2)"
         focus)
        
        (register-command!
         'proof-apply
         "Apply law to goal"
         "Apply a named law to the current goal.\nUsage: (proof-apply 'monad-left-id)"
         proof-apply)
        
        (register-command!
         'hint
         "Get proof hints"
         "Show applicable laws for the current goal.\nUsage: (hint)"
         hint)
        
        (register-command!
         'undo
         "Undo proof step"
         "Undo the last proof step.\nUsage: (undo)"
         undo)
        
        (register-command!
         'qed
         "Finalize proof"
         "Complete the proof when all goals are discharged.\nUsage: (qed)"
         qed)
        
        (register-command!
         'show
         "Show proof state"
         "Display the current proof sketch state.\nUsage: (show)"
         show)
        
        (register-command!
         'proof-trace
         "Show proof trace"
         "Display steps taken in the current proof.\nUsage: (proof-trace)"
         proof-trace)
        
        (register-command!
         'proof-help
         "Proof sketcher help"
         "Show help for proof sketcher commands.\nUsage: (proof-help)"
         proof-help)
        
        (register-command!
         'simplify-goal
         "Auto-simplify goal"
         "Apply automatic simplification to current goal.\nUsage: (simplify-goal)"
         simplify-goal)
        
        (register-command!
         'abort-proof
         "Abandon proof"
         "Abandon the current proof sketch.\nUsage: (abort-proof)"
         abort-proof)
        
        (register-command!
         'proof-history
         "Show proof history"
         "Display completed proofs.\nUsage: (proof-history)"
         proof-history-cmd)))

;;; Auto-register if possible
(guard (ex [else #f])
       (register-proof-commands!))
