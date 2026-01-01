;;; playpen/loom/spell/spell-action.ss — Custom Action Definition Macros
;;;
;;; The def-action macro creates custom game actions with validation,
;;; cost specifications, execution logic, and event generation.
;;;
;;; This is part of Spell: the DSL for weaving game logic.
;;;
;;; Named spell-action.ss to avoid conflict with loom/action.ss
;;;
;;; Exports:
;;;   def-action      Define a custom action type
;;;   try-action      Attempt to execute an action with validation
;;;   action-registry Access to registered actions

;;; ============================================================
;;; Action Registry
;;; ============================================================

;;; *actions* : Hashtable Symbol -> ActionDefinition
;;; Global registry of defined actions.
(define *actions* (make-hashtable symbol-hash eq?))

;;; ActionDefinition : Record
;;;   name       — Symbol, action identifier
;;;   validator  — (World × Actor × Target × Data -> ValidationResult)
;;;   cost-fn    — (Actor × Target × Data -> Nat)
;;;   executor   — (World × Actor × Target × Data -> ActionResult)
;;;   event-gen  — (Actor × Target × Data -> List Event) | #f

(define (make-action-definition name validator cost-fn executor event-gen)
  (list 'action-def name validator cost-fn executor event-gen))

(define (action-def-name def) (cadr def))
(define (action-def-validator def) (caddr def))
(define (action-def-cost-fn def) (cadddr def))
(define (action-def-executor def) (car (cddddr def)))
(define (action-def-event-gen def) (cadr (cddddr def)))

;;; action-define! : Symbol × ActionDefinition -> Void
(define (action-define! name def)
  (hashtable-set! *actions* name def))

;;; action-get-def : Symbol -> ActionDefinition | #f
(define (action-get-def name)
  (hashtable-ref *actions* name #f))

;;; ============================================================
;;; Action Definition Macro
;;; ============================================================

;;; def-action : Name × Clauses... -> Definitions
;;;
;;; Define a custom action with validation, cost, execution, and events.
;;;
;;; Syntax:
;;;   (def-action action-name
;;;     :params (param ...)              ; Additional parameters from action data
;;;     :validate condition              ; Must be true to execute
;;;     :cost cost-expr                  ; Action point cost (default 100)
;;;     :execute body ...                ; Code to execute the action
;;;     :events (event-expr ...))        ; Events to emit on success
;;;
;;; The following variables are bound in all clauses:
;;;   world  — Current game world
;;;   actor  — Entity performing the action
;;;   target — Action target (entity, point, or #f)
;;;   data   — Action data alist
;;;
;;; Example:
;;;   (def-action cast-fireball
;;;     :params (damage radius)
;;;     :validate (and (>= (entity-mana actor) 20)
;;;                    (entity-alive? target))
;;;     :cost 150
;;;     :execute
;;;       (let ([updated-actor (entity-drain-mana actor 20)]
;;;             [updated-target (entity-damage target damage)])
;;;         (list updated-actor updated-target))
;;;     :events
;;;       ((make-event 'spell-cast (entity-id actor) (entity-id target)
;;;                    `((spell . fireball) (damage . ,damage)))))

(define-syntax def-action
  (lambda (stx)
          (syntax-case stx (:params :validate :cost :execute :events)
                       ;; Full form with all clauses
                       [(_ action-name
                           :params (param ...)
                           :validate validate-expr
                           :cost cost-expr
                           :execute execute-body ...
                           :events event-exprs)
                        #'(begin
                           (action-define! 'action-name
                                           (make-action-definition
                                            'action-name
                                            ;; Validator
                                            (lambda (world actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         (if validate-expr
                                                             validation-success
                                                             (validation-failure "Action validation failed"))))
                                            ;; Cost function
                                            (lambda (actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         cost-expr))
                                            ;; Executor
                                            (lambda (world actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         (let ([result (begin execute-body ...)])
                                                              (action-success '() `((result . ,result))))))
                                            ;; Event generator
                                            (lambda (actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         event-exprs)))))]
                       
                       ;; Without events
                       [(_ action-name
                           :params (param ...)
                           :validate validate-expr
                           :cost cost-expr
                           :execute execute-body ...)
                        #'(def-action action-name
                           :params (param ...)
                           :validate validate-expr
                           :cost cost-expr
                           :execute execute-body ...
                           :events ())]
                       
                       ;; Without cost (default 100)
                       [(_ action-name
                           :params (param ...)
                           :validate validate-expr
                           :execute execute-body ...
                           :events event-exprs)
                        #'(begin
                           (action-define! 'action-name
                                           (make-action-definition
                                            'action-name
                                            (lambda (world actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         (if validate-expr
                                                             validation-success
                                                             (validation-failure "Action validation failed"))))
                                            (lambda (actor target data) 100)
                                            (lambda (world actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         (let ([result (begin execute-body ...)])
                                                              (action-success '() `((result . ,result))))))
                                            (lambda (actor target data)
                                                    (let ([param (alist-ref data 'param)] ...)
                                                         event-exprs)))))]
                       
                       ;; Without params
                       [(_ action-name
                           :validate validate-expr
                           :cost cost-expr
                           :execute execute-body ...
                           :events event-exprs)
                        #'(begin
                           (action-define! 'action-name
                                           (make-action-definition
                                            'action-name
                                            (lambda (world actor target data)
                                                    (if validate-expr
                                                        validation-success
                                                        (validation-failure "Action validation failed")))
                                            (lambda (actor target data) cost-expr)
                                            (lambda (world actor target data)
                                                    (let ([result (begin execute-body ...)])
                                                         (action-success '() `((result . ,result)))))
                                            (lambda (actor target data)
                                                    event-exprs))))]
                       
                       ;; Minimal form: just validate and execute
                       [(_ action-name
                           :validate validate-expr
                           :execute execute-body ...)
                        #'(def-action action-name
                           :validate validate-expr
                           :cost 100
                           :execute execute-body ...
                           :events ())]
                       
                       ;; Execute only (always valid)
                       [(_ action-name
                           :execute execute-body ...)
                        #'(def-action action-name
                           :validate #t
                           :cost 100
                           :execute execute-body ...
                           :events ())])))

;;; ============================================================
;;; Action Execution
;;; ============================================================

;;; try-action : Symbol × World × Entity × Any × Alist -> ActionResult
;;;
;;; Attempt to execute a custom action:
;;; 1. Look up action definition
;;; 2. Validate preconditions
;;; 3. Check cost against actor's energy/AP
;;; 4. Execute if valid
;;; 5. Generate events
;;;
(define (try-action action-name world actor target data)
  (let ([def (action-get-def action-name)])
       (if def
           (let* ([validator (action-def-validator def)]
                  [cost-fn (action-def-cost-fn def)]
                  [executor (action-def-executor def)]
                  [event-gen (action-def-event-gen def)]
                  ;; Validate
                  [validation (validator world actor target data)])
                 (if (validation-result-valid? validation)
                     ;; Execute and generate events
                     (let* ([result (executor world actor target data)]
                            [events (if event-gen (event-gen actor target data) '())])
                           ;; Add events to result
                           (fold-left action-result-add-event result events))
                     ;; Validation failed
                     (action-failure '() '()
                                     (validation-result-reason validation))))
           ;; Unknown action
           (action-failure '() '()
                           (string-append "Unknown action: " (symbol->string action-name))))))

;;; ============================================================
;;; Action Creation Helper
;;; ============================================================

;;; make-spell-action : Symbol × Nat × Any × Alist -> Action
;;; Create an action for a custom-defined action type.
(define (make-spell-action action-name actor-id target data)
  (let ([def (action-get-def action-name)])
       (if def
           (let ([cost ((action-def-cost-fn def)
                        #f  ; actor not available here
                        target
                        data)])
                (make-action 'custom actor-id target cost
                             (alist-set data 'action-name action-name)))
           (make-action 'custom actor-id target 100
                        (alist-set data 'action-name action-name)))))

;;; ============================================================
;;; Common Action Helpers
;;; ============================================================

;;; entity-has-energy? : Entity × Nat -> Bool
;;; Check if entity has enough energy/AP for an action.
(define (entity-has-energy? entity cost)
  (let ([actor-comp (entity-get-component entity 'actor)])
       (if actor-comp
           (>= (alist-ref actor-comp 'energy 0) cost)
           #t)))  ; No actor component = always has energy

;;; entity-drain-energy : Entity × Nat -> Entity
;;; Subtract energy from entity's actor component.
(define (entity-drain-energy entity cost)
  (entity-modify-component entity 'actor
                           (lambda (comp)
                                   (alist-set comp 'energy
                                              (- (alist-ref comp 'energy 0) cost)))))

;;; ============================================================
;;; Example Usage (commented out)
;;; ============================================================

#|
;;; Define a healing spell action
(def-action heal-self
  :validate (and (entity-alive? actor)
                 (< (entity-hp-percent actor) 100))
  :cost 50
  :execute
  (entity-heal actor 20)
  :events
  ((make-event 'spell-cast (entity-id actor) (entity-id actor)
               '((spell . heal) (amount . 20)))))


;;; Define a fireball spell with parameters
(def-action fireball
  :params (damage mana-cost)
  :validate (and (>= (entity-mana actor) mana-cost)
                 (entity-alive? target)
                 (< (entity-distance actor target) 10))
  :cost 150
  :execute
  (let* ([drained-actor (entity-drain-mana actor mana-cost)]
         [damaged-target (entity-damage target damage)])
        (list drained-actor damaged-target))
  :events
  ((make-event 'spell-cast (entity-id actor) (entity-id target)
               `((spell . fireball) (damage . ,damage)))))


;;; Define a simple melee attack
(def-action power-attack
  :validate (and (entity-alive? actor)
                 (entity-alive? target)
                 (entity-adjacent? actor target))
  :cost 200
  :execute
  (let* ([attack (entity-attack actor)]
         [damage (* attack 2)])  ; Double damage
        (entity-damage target damage))
  :events
  ((make-event 'attack (entity-id actor) (entity-id target)
               '((type . power-attack)))))


;;; Usage in game loop:
;;; (let ([result (try-action 'fireball world player enemy
;;;                           '((damage . 25) (mana-cost . 15)))])
;;;   (if (action-result-success? result)
;;;       (apply-result result world)
;;;       (display (action-result-message result))))
|#
