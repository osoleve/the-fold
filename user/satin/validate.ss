;;; playpen/satin/validate.ss — Satin validation
;;;
;;; Validates Satin specifications before compilation.
;;; Collects all issues for batch reporting.

;;; ============================================================
;;; Issue Structure
;;; ============================================================

;;; Issues are: (satin-issue level code message span details)
;;;   level: 'error | 'warning | 'info
;;;   code: symbol identifying the issue type
;;;   message: human-readable description
;;;   span: source location
;;;   details: alist of additional context

(define (satin-issue level code message span details)
  (list 'satin-issue level code message span details))

(define (satin-issue? x)
  (and (pair? x)
       (eq? (car x) 'satin-issue)))

(define (satin-issue-level i) (list-ref i 1))
(define (satin-issue-code i) (list-ref i 2))
(define (satin-issue-message i) (list-ref i 3))
(define (satin-issue-span i) (list-ref i 4))
(define (satin-issue-details i) (list-ref i 5))

(define (satin-error? i)
  (and (satin-issue? i)
       (eq? (satin-issue-level i) 'error)))

(define (satin-warning? i)
  (and (satin-issue? i)
       (eq? (satin-issue-level i) 'warning)))

;;; ============================================================
;;; Issue Construction Helpers
;;; ============================================================

(define (satin-error code message span details)
  (satin-issue 'error code message span details))

(define (satin-warning code message span details)
  (satin-issue 'warning code message span details))

(define (satin-info code message span details)
  (satin-issue 'info code message span details))

;;; ============================================================
;;; Issue Formatting
;;; ============================================================

(define (satin-format-issue issue)
  (let ([level (satin-issue-level issue)]
        [code (satin-issue-code issue)]
        [message (satin-issue-message issue)]
        [span (satin-issue-span issue)])
       (string-append
        (case level
              [(error) "ERROR"]
              [(warning) "WARNING"]
              [(info) "INFO"]
              [else "ISSUE"])
        " [" (symbol->string code) "]: "
        message
        (if (and (span? span) (not (no-span? span)))
            (string-append "
  at " (span->string span))
            ""))))

;;; ============================================================
;;; Validation Functions
;;; ============================================================

;;; satin-validate : SatinSpec → (List Issue)
;;; Validate a Satin specification and return all issues.
(define (satin-validate spec)
  (let ([issues '()])
       (define (err code msg span details)
         (set! issues (cons (satin-error code msg span details) issues)))
       (define (warn code msg span details)
         (set! issues (cons (satin-warning code msg span details) issues)))
       
       ;; Check top-level structure
       (unless (satin-story? spec)
               (err 'invalid-structure
                    "Expected (satin-story ...)"
                    (get-span spec)
                    '())
               (set! issues (reverse issues))
               issues)
       
       ;; Check required fields
       (unless (satin-has-field? (satin-story-fields spec) 'id)
               (err 'missing-field
                    "Story missing required (id ...) field"
                    (get-span spec)
                    '()))
       
       (unless (satin-has-field? (satin-story-fields spec) 'start)
               (err 'missing-field
                    "Story missing required (start ...) field"
                    (get-span spec)
                    '()))
       
       ;; Collect all defined IDs
       (let* ([nodes (satin-story-nodes spec)]
              [node-ids (map satin-node-id nodes)]
              [dialogues (satin-story-dialogues spec)]
              [dialogue-ids (map satin-dialogue-id dialogues)]
              [quests (satin-story-quests spec)]
              [quest-ids (map satin-quest-id quests)]
              [exercises (satin-story-exercises spec)]
              [exercise-ids (map satin-exercise-id exercises)]
              [start-node (satin-story-start spec)])
             
             ;; Check for duplicate node IDs
             (let ([seen '()])
                  (for-each
                   (lambda (node)
                           (let ([id (satin-node-id node)])
                                (when (memq id seen)
                                      (err 'duplicate-id
                                           (format "Duplicate node id: ~a" id)
                                           (get-span node)
                                           `((id . ,id))))
                                (set! seen (cons id seen))))
                   nodes))
             
             ;; Check start node exists
             (unless (memq start-node node-ids)
                     (err 'undefined-node
                          (format "Start node ~a not defined" start-node)
                          (get-span spec)
                          `((start . ,start-node)
                            (defined-nodes . ,node-ids))))
             
             ;; Validate each node
             (for-each
              (lambda (node)
                      (satin-validate-node node node-ids dialogue-ids issues err warn))
              nodes)
             
             ;; Validate each dialogue
             (for-each
              (lambda (dialogue)
                      (satin-validate-dialogue dialogue issues err warn))
              dialogues)
             
             ;; Validate each quest
             (for-each
              (lambda (quest)
                      (satin-validate-quest quest issues err warn))
              quests)
             
             ;; Validate each exercise
             (for-each
              (lambda (exercise)
                      (satin-validate-exercise exercise node-ids issues err warn))
              exercises))
       
       (reverse issues)))

;;; ============================================================
;;; Node Validation
;;; ============================================================

(define (satin-validate-node node node-ids dialogue-ids issues err warn)
  (let ([node-id (satin-node-id node)]
        [choices (satin-node-choices node)]
        [on-enter (satin-node-on-enter node)])
       
       ;; Validate each choice
       (for-each
        (lambda (choice)
                (let ([label (satin-choice-label choice)]
                      [target (satin-choice-target choice)]
                      [guard (satin-choice-guard choice)]
                      [effects (satin-choice-effects choice)])
                     
                     ;; Check label
                     (unless label
                             (err 'invalid-choice
                                  (format "Choice in node ~a missing label" node-id)
                                  (get-span choice)
                                  `((node . ,node-id))))
                     
                     ;; Check target
                     (unless target
                             (err 'invalid-choice
                                  (format "Choice in node ~a missing target" node-id)
                                  (get-span choice)
                                  `((node . ,node-id))))
                     
                     ;; Check target exists
                     (when (and target (not (memq target node-ids)))
                           (let ([suggestions (satin-suggest-id target node-ids)])
                                (err 'undefined-node
                                     (format "Choice target ~a not defined" target)
                                     (get-span choice)
                                     `((target . ,target)
                                       (node . ,node-id)
                                       (suggestions . ,suggestions)))))
                     
                     ;; Validate guard if present
                     (when (and guard (not (satin-guard-form? guard)))
                           (warn 'invalid-guard
                                 (format "Unrecognized guard form in node ~a" node-id)
                                 (get-span choice)
                                 `((guard . ,guard))))
                     
                     ;; Validate effects
                     (for-each
                      (lambda (effect)
                              (unless (satin-effect-valid? effect)
                                      (err 'invalid-effect
                                           (format "Invalid effect form: ~a" effect)
                                           (get-span effect)
                                           `((node . ,node-id)))))
                      effects)))
        choices)
       
       ;; Validate on-enter effects
       (for-each
        (lambda (effect)
                (unless (satin-effect-valid? effect)
                        (err 'invalid-effect
                             (format "Invalid on-enter effect: ~a" effect)
                             (get-span effect)
                             `((node . ,node-id)))))
        on-enter)))

;;; ============================================================
;;; Dialogue Validation
;;; ============================================================

(define (satin-validate-dialogue dialogue issues err warn)
  (let* ([dialogue-id (satin-dialogue-id dialogue)]
         [nodes (satin-dialogue-nodes dialogue)]
         [node-ids (map satin-node-id nodes)]
         [start (satin-dialogue-start dialogue)])
        
        ;; Check start node exists
        (unless (memq start node-ids)
                (err 'undefined-node
                     (format "Dialogue ~a start node ~a not defined" dialogue-id start)
                     (get-span dialogue)
                     `((dialogue . ,dialogue-id)
                       (start . ,start))))
        
        ;; Validate dialogue nodes
        (for-each
         (lambda (node)
                 (let ([choices (satin-node-choices node)])
                      (for-each
                       (lambda (choice)
                               (let ([target (satin-choice-target choice)])
                                    (when (and target
                                               (not (eq? target 'end))
                                               (not (memq target node-ids)))
                                          (err 'undefined-node
                                               (format "Dialogue choice target ~a not defined" target)
                                               (get-span choice)
                                               `((dialogue . ,dialogue-id)
                                                 (target . ,target))))))
                       choices)))
         nodes)))

;;; ============================================================
;;; Quest Validation
;;; ============================================================

(define (satin-validate-quest quest issues err warn)
  (let ([quest-id (satin-quest-id quest)]
        [steps (satin-quest-steps quest)])
       
       ;; Check for empty steps
       (when (null? steps)
             (warn 'empty-quest
                   (format "Quest ~a has no steps" quest-id)
                   (get-span quest)
                   `((quest . ,quest-id))))
       
       ;; Validate each step
       (for-each
        (lambda (step)
                (when (pair? step)
                      (let ([tag (car (strip-span step))])
                           (unless (eq? tag 'step)
                                   (err 'invalid-quest-step
                                        (format "Invalid quest step form in ~a" quest-id)
                                        (get-span step)
                                        `((quest . ,quest-id)))))))
        steps)))

;;; ============================================================
;;; Exercise Validation
;;; ============================================================

(define (satin-validate-exercise exercise node-ids issues err warn)
  (let ([ex-id (satin-exercise-id exercise)]
        [rubric (satin-exercise-rubric exercise)]
        [on-pass (satin-exercise-on-pass exercise)])
       
       ;; Check for empty rubric
       (when (null? rubric)
             (warn 'empty-rubric
                   (format "Exercise ~a has no rubric checks" ex-id)
                   (get-span exercise)
                   `((exercise . ,ex-id))))
       
       ;; Validate rubric checks
       (for-each
        (lambda (check)
                (when (pair? check)
                      (let ([tag (car (strip-span check))])
                           (unless (eq? tag 'check)
                                   (err 'invalid-rubric
                                        (format "Invalid rubric check in ~a" ex-id)
                                        (get-span check)
                                        `((exercise . ,ex-id)))))))
        rubric)
       
       ;; Validate on-pass effects
       (for-each
        (lambda (effect)
                (unless (satin-effect-valid? effect)
                        (err 'invalid-effect
                             (format "Invalid on-pass effect in exercise ~a" ex-id)
                             (get-span effect)
                             `((exercise . ,ex-id)))))
        on-pass)))

;;; ============================================================
;;; Suggestion Generation
;;; ============================================================

;;; Generate suggestions for a misspelled ID
(define (satin-find-suggestions target candidates)
  (let ([target-str (symbol->string target)])
       (filter
        (lambda (c)
                (< (edit-distance target-str (symbol->string c)) 3))
        candidates)))
