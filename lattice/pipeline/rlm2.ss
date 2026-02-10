(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'pipeline/stage)
(require 'pipeline/effects)

(doc 'module 'pipeline/rlm2)
(doc 'description "RLM v2 pure types: HUD state, action language, config, step records, and completion/loop detection. All types are tagged-list S-expressions suitable for CAS storage.")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'dependencies '(pipeline/stage.ss pipeline/effects.ss))

;;; ====
;;; RLM v2 State (The HUD)
;;; ====
;;;
;;; The complete context for one agent step. No conversation history —
;;; the state IS the context. The model reads this, makes a move.
;;;
;;; Fields:
;;;   task     : String — the objective
;;;   plan     : Alist of (item . status) — agent-owned todo list
;;;   env      : Alist of (key type size hash) — inventory metadata
;;;   loaded   : List of Symbol — available lattice modules
;;;   notes    : List of String — semantic memory (reflection distillates)
;;;   episodic : Alist of (step-num . cas-hash) — pointers to full records
;;;   journal  : List of (tag . text) — tagged within-run scratch
;;;   last-result : Any — most recent observation (cleared each step)
;;;   fuel     : Nat — remaining fuel budget
;;;   step     : Nat — current step number

(doc 'section 'rlm2-state)

(doc 'type '(-> String Alist Alist (List Symbol) (List String) Alist (List (Symbol . String)) Any Nat Nat Rlm2State))
(doc 'description "Create an RLM v2 state — the complete HUD for one agent step")
(define (make-rlm2-state task plan env loaded notes episodic journal last-result fuel step)
  (list 'rlm2-state task plan env loaded notes episodic journal last-result fuel step))

(define (rlm2-state? x)
  (and (pair? x) (eq? (car x) 'rlm2-state)))

(define (rlm2-state-task s)        (list-ref s 1))
(define (rlm2-state-plan s)        (list-ref s 2))
(define (rlm2-state-env s)         (list-ref s 3))
(define (rlm2-state-loaded s)      (list-ref s 4))
(define (rlm2-state-notes s)       (list-ref s 5))
(define (rlm2-state-episodic s)    (list-ref s 6))
(define (rlm2-state-journal s)     (list-ref s 7))
(define (rlm2-state-last-result s) (list-ref s 8))
(define (rlm2-state-fuel s)        (list-ref s 9))
(define (rlm2-state-step s)        (list-ref s 10))

(doc 'type '(-> String Any Nat Rlm2State))
(doc 'description "Create an initial state with just task, input as last-result, and fuel budget")
(define (make-initial-rlm2-state task initial-result fuel)
  (make-rlm2-state task '() '() '() '() '() '() initial-result fuel 0))

;;; ====
;;; State Update (Functional — returns new state)
;;; ====

(doc 'section 'rlm2-state-update)

(doc 'type '(-> Rlm2State Alist Rlm2State))
(doc 'description "Return a new state with the plan replaced")
(define (rlm2-state-with-plan s plan)
  (make-rlm2-state (rlm2-state-task s) plan (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State Alist Rlm2State))
(doc 'description "Return a new state with the env replaced")
(define (rlm2-state-with-env s env)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) env
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State (List Symbol) Rlm2State))
(doc 'description "Return a new state with loaded modules replaced")
(define (rlm2-state-with-loaded s loaded)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   loaded (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State String Rlm2State))
(doc 'description "Return a new state with a note appended")
(define (rlm2-state-add-note s note)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s)
                   (append (rlm2-state-notes s) (list note))
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State (List String) Rlm2State))
(doc 'description "Return a new state with notes replaced (for compaction)")
(define (rlm2-state-with-notes s notes)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) notes
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State Nat String Rlm2State))
(doc 'description "Return a new state with an episodic entry added")
(define (rlm2-state-add-episodic s step-num cas-hash)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (append (rlm2-state-episodic s) (list (cons step-num cas-hash)))
                   (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State Any Rlm2State))
(doc 'description "Return a new state with last-result replaced")
(define (rlm2-state-with-last-result s result)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   result
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State Nat Rlm2State))
(doc 'description "Return a new state with fuel reduced")
(define (rlm2-state-use-fuel s amount)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (max 0 (- (rlm2-state-fuel s) amount))
                   (rlm2-state-step s)))

(doc 'type '(-> Rlm2State Rlm2State))
(doc 'description "Return a new state with step counter incremented")
(define (rlm2-state-advance-step s)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) (rlm2-state-journal s)
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s)
                   (+ 1 (rlm2-state-step s))))

(doc 'type '(-> Rlm2State Symbol String Rlm2State))
(doc 'description "Return a new state with a journal entry appended")
(define (rlm2-state-add-journal s tag text)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s)
                   (append (rlm2-state-journal s) (list (cons tag text)))
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

(doc 'type '(-> Rlm2State (List (Symbol . String)) Rlm2State))
(doc 'description "Return a new state with journal replaced")
(define (rlm2-state-with-journal s journal)
  (make-rlm2-state (rlm2-state-task s) (rlm2-state-plan s) (rlm2-state-env s)
                   (rlm2-state-loaded s) (rlm2-state-notes s)
                   (rlm2-state-episodic s) journal
                   (rlm2-state-last-result s)
                   (rlm2-state-fuel s) (rlm2-state-step s)))

;;; ====
;;; Completion & Exhaustion
;;; ====

(doc 'section 'rlm2-completion)

(doc 'type '(-> Rlm2State Boolean))
(doc 'description "True when the state carries a successful result (submit was accepted)")
(define (rlm2-state-complete? s)
  (let ([r (rlm2-state-last-result s)])
    (and (pair? r) (eq? (car r) 'rlm2-result))))

(doc 'type '(-> Rlm2State Any))
(doc 'description "Extract the final result from a completed state")
(define (rlm2-state-result s)
  (let ([r (rlm2-state-last-result s)])
    (and (pair? r) (eq? (car r) 'rlm2-result) (cadr r))))

(doc 'type '(-> Any Rlm2Result))
(doc 'description "Wrap a value as a completion result")
(define (make-rlm2-result value)
  (list 'rlm2-result value))

(define (rlm2-result? x)
  (and (pair? x) (eq? (car x) 'rlm2-result)))

;;; ====
;;; RLM v2 Configuration
;;; ====

(doc 'section 'rlm2-config)

(doc 'type '(-> RlmProvider String Nat Nat Nat Nat Nat Nat (Maybe Proc) Rlm2Config))
(doc 'description "Create an RLM v2 configuration")
(define (make-rlm2-config provider system-prompt max-steps max-fuel
                          chunk-size max-depth loop-window context-budget
                          verifier)
  (list 'rlm2-config provider system-prompt max-steps max-fuel
        chunk-size max-depth loop-window context-budget verifier))

(define (rlm2-config? x)
  (and (pair? x) (eq? (car x) 'rlm2-config)))

(define (rlm2-config-provider cfg)       (list-ref cfg 1))
(define (rlm2-config-system-prompt cfg)  (list-ref cfg 2))
(define (rlm2-config-max-steps cfg)      (list-ref cfg 3))
(define (rlm2-config-max-fuel cfg)       (list-ref cfg 4))
(define (rlm2-config-chunk-size cfg)     (list-ref cfg 5))
(define (rlm2-config-max-depth cfg)      (list-ref cfg 6))
(define (rlm2-config-loop-window cfg)    (list-ref cfg 7))
(define (rlm2-config-context-budget cfg) (list-ref cfg 8))
(define (rlm2-config-verifier cfg)       (list-ref cfg 9))

(doc 'type '(-> Rlm2Config (List Msg)))
(doc 'description "Few-shot examples as user/assistant message pairs. Empty list if not set.")
(define (rlm2-config-few-shot cfg)
  (if (> (length cfg) 10) (list-ref cfg 10) '()))

(doc 'type '(-> RlmProvider String Rlm2Config))
(doc 'description "Sensible defaults: 20 steps, 50k fuel, 2k chunks, depth 2, window 3, 8k budget, no verifier")
(define (make-rlm2-config-default provider system-prompt)
  (make-rlm2-config provider system-prompt
                    20     ; max-steps
                    50000  ; max-fuel
                    2000   ; chunk-size (chars)
                    2      ; max-depth
                    3      ; loop-window
                    8000   ; context-budget (chars)
                    #f))   ; no verifier

(doc 'type '(-> Rlm2State Rlm2Config Boolean))
(doc 'description "True when fuel or steps are exhausted")
(define (rlm2-state-exhausted? s cfg)
  (or (<= (rlm2-state-fuel s) 0)
      (>= (rlm2-state-step s) (rlm2-config-max-steps cfg))))

;;; ====
;;; Action Language
;;; ====
;;;
;;; 18 forms + begin. Each is a tagged list.
;;;
;;; (search query)           (inspect skill)        (exports skill)
;;; (load module)            (eval expr)            (store key expr)
;;; (retrieve key)           (peek key n)           (grep key pattern k)
;;; (slice key start end)    (recall-step n)        (submit expr)
;;; (think text)             (plan! items)          (map-chunks key expr)
;;; (journal tag text)       (recall tag)           (memorize key text)
;;; (remember query)
;;; (begin action ...)

(doc 'section 'rlm2-actions)

;;; Known action type symbols
(define *rlm2-action-types*
  '(search inspect exports load eval store retrieve peek
    grep slice recall-step submit think plan! map-chunks
    journal recall memorize remember begin))

(doc 'type '(-> Any Boolean))
(doc 'description "True if x is a well-formed action (tagged list with known type)")
(define (rlm2-action? x)
  (and (pair? x)
       (symbol? (car x))
       (memq (car x) *rlm2-action-types*)
       #t))

(doc 'type '(-> Action Symbol))
(doc 'description "Extract the action type tag")
(define (rlm2-action-type a) (car a))

;;; Per-type predicates
(define (rlm2-search? a)      (and (pair? a) (eq? (car a) 'search)))
(define (rlm2-inspect? a)     (and (pair? a) (eq? (car a) 'inspect)))
(define (rlm2-exports? a)     (and (pair? a) (eq? (car a) 'exports)))
(define (rlm2-load? a)        (and (pair? a) (eq? (car a) 'load)))
(define (rlm2-eval? a)        (and (pair? a) (eq? (car a) 'eval)))
(define (rlm2-store? a)       (and (pair? a) (eq? (car a) 'store)))
(define (rlm2-retrieve? a)    (and (pair? a) (eq? (car a) 'retrieve)))
(define (rlm2-peek? a)        (and (pair? a) (eq? (car a) 'peek)))
(define (rlm2-grep? a)        (and (pair? a) (eq? (car a) 'grep)))
(define (rlm2-slice? a)       (and (pair? a) (eq? (car a) 'slice)))
(define (rlm2-recall-step? a) (and (pair? a) (eq? (car a) 'recall-step)))
(define (rlm2-submit? a)      (and (pair? a) (eq? (car a) 'submit)))
(define (rlm2-think? a)       (and (pair? a) (eq? (car a) 'think)))
(define (rlm2-plan!? a)       (and (pair? a) (eq? (car a) 'plan!)))
(define (rlm2-map-chunks? a)  (and (pair? a) (eq? (car a) 'map-chunks)))
(define (rlm2-journal? a)     (and (pair? a) (eq? (car a) 'journal)))
(define (rlm2-recall? a)      (and (pair? a) (eq? (car a) 'recall)))
(define (rlm2-memorize? a)    (and (pair? a) (eq? (car a) 'memorize)))
(define (rlm2-remember? a)    (and (pair? a) (eq? (car a) 'remember)))
(define (rlm2-begin? a)       (and (pair? a) (eq? (car a) 'begin)))

;;; Unquote helper: (quote x) -> x, else identity.
;;; After `read`, model output like (retrieve 'x) has (quote x) as the arg.
(define (rlm2-unquote arg)
  (if (and (pair? arg) (eq? (car arg) 'quote) (pair? (cdr arg)))
      (cadr arg)
      arg))

;;; Per-type accessors
;;; Symbol/key args are run through rlm2-unquote to normalize quoted forms.
;;; Expression args (eval, store, submit) are NOT unquoted — they may
;;; legitimately contain quote forms.
(define (rlm2-search-query a)       (cadr a))                    ; (search query) — string, no unquote
(define (rlm2-inspect-skill a)      (rlm2-unquote (cadr a)))     ; (inspect 'skill)
(define (rlm2-exports-skill a)      (rlm2-unquote (cadr a)))     ; (exports 'skill)
(define (rlm2-load-module a)        (rlm2-unquote (cadr a)))     ; (load 'module)
(define (rlm2-eval-expr a)          (cadr a))                    ; (eval expr) — expr, no unquote
(define (rlm2-store-key a)          (rlm2-unquote (cadr a)))     ; (store 'key expr)
(define (rlm2-store-expr a)         (caddr a))                   ; (store key expr) — expr, no unquote
(define (rlm2-retrieve-key a)       (rlm2-unquote (cadr a)))     ; (retrieve 'key)
(define (rlm2-peek-key a)           (rlm2-unquote (cadr a)))     ; (peek 'key n)
(define (rlm2-peek-n a)             (caddr a))                   ; (peek key n)
(define (rlm2-grep-key a)           (rlm2-unquote (cadr a)))     ; (grep 'key pattern k)
(define (rlm2-grep-pattern a)       (caddr a))                   ; (grep key pattern k)
(define (rlm2-grep-k a)             (cadddr a))                  ; (grep key pattern k)
(define (rlm2-slice-key a)          (rlm2-unquote (cadr a)))     ; (slice 'key start end)
(define (rlm2-slice-start a)        (caddr a))                   ; (slice key start end)
(define (rlm2-slice-end a)          (cadddr a))                  ; (slice key start end)
(define (rlm2-recall-step-n a)      (cadr a))                    ; (recall-step n)
(define (rlm2-submit-expr a)        (cadr a))                    ; (submit expr) — expr, no unquote
(define (rlm2-think-text a)         (cadr a))                    ; (think text)
(define (rlm2-plan!-items a)        (cadr a))                    ; (plan! items)
(define (rlm2-map-chunks-key a)     (rlm2-unquote (cadr a)))     ; (map-chunks 'key expr)
(define (rlm2-map-chunks-expr a)    (caddr a))                   ; (map-chunks key expr) — string
(define (rlm2-journal-tag a)        (rlm2-unquote (cadr a)))     ; (journal 'tag text)
(define (rlm2-journal-text a)       (caddr a))                   ; (journal tag text) — string
(define (rlm2-recall-tag a)         (rlm2-unquote (cadr a)))     ; (recall 'tag)
(define (rlm2-memorize-key a)       (rlm2-unquote (cadr a)))     ; (memorize 'key text)
(define (rlm2-memorize-text a)      (caddr a))                   ; (memorize key text) — string
(define (rlm2-remember-query a)     (cadr a))                    ; (remember query) — string
(define (rlm2-begin-actions a)      (cdr a))                     ; (begin action ...)

;;; ====
;;; Action Arity Table
;;; ====
;;;
;;; Used by the parser to validate well-formedness.
;;; Maps action-type -> expected arg count (excluding tag).
;;; #f means variadic (begin).

(doc 'section 'rlm2-action-arity)

(define *rlm2-action-arity*
  '((search      . 1)
    (inspect     . 1)
    (exports     . 1)
    (load        . 1)
    (eval        . 1)
    (store       . 2)
    (retrieve    . 1)
    (peek        . 2)
    (grep        . 3)
    (slice       . 3)
    (recall-step . 1)
    (submit      . 1)
    (think       . 1)
    (plan!       . 1)
    (map-chunks  . 2)
    (journal     . 2)
    (recall      . 1)
    (memorize    . 2)
    (remember    . 1)
    (begin       . #f)))

(doc 'type '(-> Symbol (Maybe Nat)))
(doc 'description "Look up expected arity for an action type. #f for variadic (begin).")
(define (rlm2-action-expected-arity type)
  (let ([entry (assq type *rlm2-action-arity*)])
    (and entry (cdr entry))))

;;; ====
;;; Action Validation
;;; ====

(doc 'section 'rlm2-action-validation)

(doc 'type '(-> Any (Result Action String)))
(doc 'description "Validate an S-expression as a well-formed action. Returns (ok action) or (err reason).")
(define (rlm2-validate-action expr)
  (cond
    [(not (pair? expr))
     (list 'err "Action must be a list")]
    [(not (symbol? (car expr)))
     (list 'err (format "Action type must be a symbol, got ~a" (car expr)))]
    [(not (memq (car expr) *rlm2-action-types*))
     (list 'err (format "Unknown action type: ~a" (car expr)))]
    [else
     (let* ([type (car expr)]
            [args (cdr expr)]
            [expected (rlm2-action-expected-arity type)])
       (cond
         ;; Variadic (begin) — validate children recursively
         [(not expected)
          (if (null? args)
              (list 'err "begin requires at least one action")
              (let loop ([children args])
                (if (null? children)
                    (list 'ok expr)
                    (let ([child-result (rlm2-validate-action (car children))])
                      (if (eq? (car child-result) 'ok)
                          (loop (cdr children))
                          child-result)))))]
         ;; Fixed arity
         [(not (= (length args) expected))
          (list 'err (format "~a expects ~a arg~a, got ~a"
                             type expected
                             (if (= expected 1) "" "s")
                             (length args)))]
         [else (list 'ok expr)]))]))

(define (rlm2-validation-ok? r) (eq? (car r) 'ok))
(define (rlm2-validation-value r) (cadr r))
(define (rlm2-validation-error r) (cadr r))

;;; ====
;;; Act Result (agent output before execution)
;;; ====

(doc 'section 'rlm2-act-result)

(doc 'type '(-> (Maybe String) Action ActResult))
(doc 'description "Package agent output: optional raw thought + parsed action")
(define (make-rlm2-act-result thought action)
  (list 'rlm2-act-result thought action))

(define (rlm2-act-result? x)
  (and (pair? x) (eq? (car x) 'rlm2-act-result)))

(define (rlm2-act-result-thought r) (list-ref r 1))
(define (rlm2-act-result-action r)  (list-ref r 2))

;;; ====
;;; Observation (action execution result)
;;; ====

(doc 'section 'rlm2-observation)

(doc 'type '(-> Symbol Any Any Boolean Rlm2Observation))
(doc 'description "Record an action's execution result")
(define (make-rlm2-observation action-type target value ok?)
  (list 'rlm2-observation action-type target value ok?))

(define (rlm2-observation? x)
  (and (pair? x) (eq? (car x) 'rlm2-observation)))

(define (rlm2-observation-action-type o) (list-ref o 1))
(define (rlm2-observation-target o)      (list-ref o 2))
(define (rlm2-observation-value o)       (list-ref o 3))
(define (rlm2-observation-ok? o)         (list-ref o 4))

;;; ====
;;; Step Record (for CAS storage)
;;; ====

(doc 'section 'rlm2-step-record)

(doc 'type '(-> Nat Action Rlm2Observation String String Rlm2StepRecord))
(doc 'description "Immutable record of a single v2 step — stored in CAS")
(define (make-rlm2-step-record step-num action observation note state-hash)
  (list 'rlm2-step step-num action observation note state-hash))

(define (rlm2-step-record? x)
  (and (pair? x) (eq? (car x) 'rlm2-step)))

(define (rlm2-step-record-num r)         (list-ref r 1))
(define (rlm2-step-record-action r)      (list-ref r 2))
(define (rlm2-step-record-observation r) (list-ref r 3))
(define (rlm2-step-record-note r)        (list-ref r 4))
(define (rlm2-step-record-state-hash r)  (list-ref r 5))

;;; ====
;;; Trajectory (complete run summary for CAS)
;;; ====

(doc 'section 'rlm2-trajectory)

(doc 'type '(-> String String String Nat Nat Symbol String String Nat Rlm2Trajectory))
(doc 'description "Summary record for a complete v2 run")
(define (make-rlm2-trajectory config-hash input-hash output-hash
                              steps total-fuel status
                              started finished depth)
  (list 'rlm2-trajectory config-hash input-hash output-hash
        steps total-fuel status started finished depth))

(define (rlm2-trajectory? x)
  (and (pair? x) (eq? (car x) 'rlm2-trajectory)))

(define (rlm2-trajectory-config-hash t) (list-ref t 1))
(define (rlm2-trajectory-input-hash t)  (list-ref t 2))
(define (rlm2-trajectory-output-hash t) (list-ref t 3))
(define (rlm2-trajectory-steps t)       (list-ref t 4))
(define (rlm2-trajectory-total-fuel t)  (list-ref t 5))
(define (rlm2-trajectory-status t)      (list-ref t 6))
(define (rlm2-trajectory-started t)     (list-ref t 7))
(define (rlm2-trajectory-finished t)    (list-ref t 8))
(define (rlm2-trajectory-depth t)       (list-ref t 9))

;;; ====
;;; Semantic Loop Detection
;;; ====
;;;
;;; v1 fingerprinted env-summary + code. v2 fingerprints
;;; (plan-status, last-action-type) — detects logical stuckness
;;; rather than textual repetition.

(doc 'section 'rlm2-loop-detection)

(doc 'type '(-> Rlm2State Symbol String))
(doc 'description "Create a semantic fingerprint from plan status + action type")
(define (rlm2-semantic-fingerprint state action-type)
  (let* ([plan (rlm2-state-plan state)]
         ;; Guard: only extract status from well-formed alist entries
         [plan-status (map (lambda (item)
                            (if (pair? item)
                                (cons (car item) (cdr item))
                                (cons item 'unknown)))
                          plan)]
         [fp-data (list plan-status action-type)])
    (format "~a" fp-data)))

(doc 'type '(-> (List String) String Nat Boolean))
(doc 'description "Check if fingerprint appears in recent history window")
(define (rlm2-loop-detected? history current-fingerprint window-size)
  (let loop ([h history] [k window-size])
    (cond
      [(or (= k 0) (null? h)) #f]
      [(string=? (car h) current-fingerprint) #t]
      [else (loop (cdr h) (- k 1))])))

;;; ====
;;; Telemetry Record
;;; ====

(doc 'section 'rlm2-telemetry)

(doc 'type '(-> String Symbol Nat Nat Boolean String Rlm2Telemetry))
(doc 'description "Lightweight telemetry for action logging")
(define (make-rlm2-telemetry model-id action-type step-num duration-ms ok? detail)
  (list 'rlm2-telemetry model-id action-type step-num duration-ms ok? detail))

(define (rlm2-telemetry? x)
  (and (pair? x) (eq? (car x) 'rlm2-telemetry)))

(define (rlm2-telemetry-model-id t)    (list-ref t 1))
(define (rlm2-telemetry-action-type t) (list-ref t 2))
(define (rlm2-telemetry-step-num t)    (list-ref t 3))
(define (rlm2-telemetry-duration t)    (list-ref t 4))
(define (rlm2-telemetry-ok? t)         (list-ref t 5))
(define (rlm2-telemetry-detail t)      (list-ref t 6))

;;; ====
;;; Pipeline Effect Constructor
;;; ====

(doc 'section 'rlm2-effect)

(doc 'type '(-> Rlm2Config String Nat (Stage ctx i o)))
(doc 'description "Create an RLM v2 effect stage for pipeline composition")
(define (rlm2-effect config system-prompt max-steps)
  (effect 'rlm2 (list 'run config system-prompt max-steps)))

(define (rlm2-effect? e)
  (and (stage-effect? e)
       (eq? (stage-effect-type e) 'rlm2)))
