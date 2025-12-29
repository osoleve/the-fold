;;; playpen/satin/docs.ss — Documentation-as-data for Satin forms
;;;
;;; Extracts documentation from Satin specifications into
;;; in-REPL inspectable structures. No markdown files!

;;; ============================================================
;;; Documentation Structure
;;; ============================================================

;;; A doc entry is an alist with:
;;; - id: symbol
;;; - type: symbol (story, node, choice, dialogue, quest, exercise, etc.)
;;; - name: string (human-readable name)
;;; - description: string
;;; - fields: alist of field docs
;;; - examples: list of example forms
;;; - related: list of related ids

(define (make-doc id type name description fields examples related)
  `((id . ,id)
    (type . ,type)
    (name . ,name)
    (description . ,description)
    (fields . ,fields)
    (examples . ,examples)
    (related . ,related)))

(define (doc-id d) (cdr (assq 'id d)))
(define (doc-type d) (cdr (assq 'type d)))
(define (doc-name d) (cdr (assq 'name d)))
(define (doc-description d) (cdr (assq 'description d)))
(define (doc-fields d) (cdr (assq 'fields d)))
(define (doc-examples d) (cdr (assq 'examples d)))
(define (doc-related d) (cdr (assq 'related d)))

;;; ============================================================
;;; Built-in Documentation Database
;;; ============================================================

(define *satin-docs*
  (list
   ;; Core story form
   (make-doc 'satin-story 'form
             "satin-story"
             "The root form for a Satin story specification. Contains all nodes, dialogues, quests, exercises, and metadata."
             '((id . "Unique identifier for the story (symbol)")
               (title . "Human-readable title (string)")
               (author . "Author name (string)")
               (version . "Version string")
               (start . "ID of the starting node (symbol)")
               (meta . "Additional metadata (alist)"))
             '((satin-story
                (id my-story)
                (title "My Story")
                (author "Me")
                (start intro)
                (node intro (body "Hello!"))))
             '(node dialogue quest exercise))
   
   ;; Node form
   (make-doc 'node 'form
             "node"
             "A story node representing a scene or location. Contains narrative text, choices, and effects."
             '((id . "Unique node identifier (symbol)")
               (title . "Optional display title (string)")
               (body . "Narrative text shown to the player (string)")
               (on-enter . "Effects to run when entering the node")
               (on-exit . "Effects to run when leaving the node")
               (choice . "One or more choice forms")
               (exercise . "Optional exercise ID to present")
               (end . "Mark this node as an ending"))
             '((node kitchen
                     (title "The Kitchen")
                     (body "You stand in a cozy kitchen.")
                     (on-enter (set-flag! visited-kitchen?))
                     (choice "Look around" kitchen)
                     (choice "Go outside" garden)))
             '(choice satin-story))
   
   ;; Choice form
   (make-doc 'choice 'form
             "choice"
             "An option the player can select in a node. Can have guards and effects."
             '((label . "Text displayed to the player (string)")
               (target . "Node to transition to (symbol)")
               (:when . "Guard expression that must be true to show choice")
               (:effects . "Effects to run when choice is selected"))
             '((choice "Open door" hallway)
               (choice "Use key" hallway :when (has-item? key) :effects (take-item! key)))
             '(node guard effect))
   
   ;; Dialogue form
   (make-doc 'dialogue 'form
             "dialogue"
             "A conversation structure with multiple participants and branching paths."
             '((id . "Unique dialogue identifier (symbol)")
               (participants . "List of participant symbols")
               (start . "Starting node within dialogue")
               (node . "Dialogue nodes with speaker and text"))
             '((dialogue merchant-chat
                         (participants merchant player)
                         (start greeting)
                         (node greeting
                               (speaker merchant)
                               (text "Welcome to my shop!")
                               (choice "Show me your wares" shop)
                               (choice "Goodbye" end))))
             '(node satin-story))
   
   ;; Quest form
   (make-doc 'quest 'form
             "quest"
             "A multi-step objective with progress tracking."
             '((id . "Unique quest identifier (symbol)")
               (title . "Quest title shown to player")
               (description . "Quest description")
               (steps . "List of (step id description guard) forms")
               (on-activate . "Effects when quest starts")
               (on-complete . "Effects when quest finishes"))
             '((quest find-key
                      (title "Find the Key")
                      (description "Search the house for the lost key")
                      (steps
                       (step search "Search the rooms" #t)
                       (step find "Find the key" (has-item? key)))
                      (on-complete (set-flag! quest-done?))))
             '(node effect))
   
   ;; Exercise form
   (make-doc 'exercise 'form
             "exercise"
             "An interactive exercise with rubric checks."
             '((id . "Unique exercise identifier (symbol)")
               (title . "Exercise title")
               (prompt . "Question or task description")
               (hints . "List of hint strings")
               (rubric . "List of (check id description predicate) forms")
               (on-pass . "Effects when exercise is passed")
               (on-fail . "Effects when exercise is failed"))
             '((exercise count-items
                         (title "Counting")
                         (prompt "How many items are on the table?")
                         (hints "Count carefully")
                         (rubric
                          (check correct "Correct count" (answer-eq? 5)))
                         (on-pass (set-flag! exercise-passed?))))
             '(mcq short-answer code-task))
   
   ;; MCQ form
   (make-doc 'mcq 'form
             "mcq"
             "A multiple choice question that compiles to an exercise."
             '((id . "Unique MCQ identifier (symbol)")
               (question . "The question text (string)")
               (options . "List of option strings")
               (correct . "1-based index of correct answer (number)")
               (explanation . "Explanation shown after answering")
               (hints . "List of hint strings"))
             '((mcq what-is-scheme
                    (question "What type of language is Scheme?")
                    (options "Object-oriented" "Functional" "Procedural" "Markup")
                    (correct 2)
                    (explanation "Scheme is a functional programming language")))
             '(exercise short-answer))
   
   ;; Short answer form
   (make-doc 'short-answer 'form
             "short-answer"
             "A free-text answer question with expected response matching."
             '((id . "Unique ID (symbol)")
               (question . "The question text (string)")
               (expected . "Expected answer for auto-checking (string)")
               (rubric . "Custom rubric checks if expected matching isn't enough")
               (hints . "List of hint strings"))
             '((short-answer capital-france
                             (question "What is the capital of France?")
                             (expected "Paris")
                             (hints "It's the City of Light")))
             '(exercise mcq))
   
   ;; Code task form
   (make-doc 'code-task 'form
             "code-task"
             "A programming exercise with starter code and tests."
             '((id . "Unique ID (symbol)")
               (title . "Task title")
               (description . "What to implement")
               (starter . "Starting code provided to learner")
               (solution . "Reference solution (hidden)")
               (tests . "List of (test description predicate) forms")
               (hints . "List of hint strings"))
             '((code-task add-function
                          (title "Write Add")
                          (description "Write a function that adds two numbers")
                          (starter "(define (add a b)
  )")
                          (solution "(define (add a b) (+ a b))")
                          (tests
                           (test "adds 1+2=3" (answer-eq? 3)))))
             '(exercise))
   
   ;; Lesson form
   (make-doc 'lesson 'form
             "lesson"
             "A structured learning unit with objectives and a sequence of content."
             '((id . "Unique lesson identifier (symbol)")
               (title . "Lesson title")
               (objectives . "List of learning objectives (strings)")
               (prerequisites . "List of required prior lesson IDs")
               (sequence . "Ordered list of node/exercise IDs")
               (on-complete . "Effects when lesson finishes"))
             '((lesson intro-to-scheme
                       (title "Introduction to Scheme")
                       (objectives "Understand S-expressions" "Write simple code")
                       (sequence welcome s-expr-node exercise-1 summary)
                       (on-complete (set-flag! intro-done?))))
             '(mcq exercise mastery))
   
   ;; Mastery form
   (make-doc 'mastery 'form
             "mastery"
             "Tracks skill progression across multiple exercises."
             '((id . "Unique mastery ID (symbol)")
               (skill . "Name of the skill being tracked")
               (levels . "List of level names (beginner, intermediate, etc.)")
               (threshold . "Number of exercises needed per level")
               (exercises . "List of exercise IDs that contribute to this skill"))
             '((mastery recursion-skill
                        (skill "Recursive Thinking")
                        (levels beginner intermediate advanced)
                        (threshold 3)
                        (exercises rec-1 rec-2 rec-3 rec-4 rec-5)))
             '(lesson exercise))
   
   ;; Rule form
   (make-doc 'rule 'form
             "rule"
             "A reactive rule that triggers when conditions are met."
             '((id . "Unique rule identifier (symbol)")
               (when . "Guard expression that triggers the rule")
               (once? . "If true, rule only fires once")
               (effects . "Effects to run when rule fires"))
             '((rule low-health-warning
                     (when (var<= health 10))
                     (once? #f)
                     (effects (print! "Warning: Low health!"))))
             '(node timeline))
   
   ;; Timeline form
   (make-doc 'timeline 'form
             "timeline"
             "Scheduled events that fire at specific times."
             '((event . "Event definitions with (at time) and (effects ...)"))
             '((timeline
                (event dawn (at 0) (effects (print! "The sun rises.")))
                (event noon (at 100) (once? #t) (effects (print! "Noon!")))))
             '(rule node))))

;;; ============================================================
;;; Documentation Lookup
;;; ============================================================

;;; satin-doc : Symbol → Doc | #f
;;; Look up documentation for a form type.
(define (satin-doc id)
  (let loop ([docs *satin-docs*])
       (cond
        [(null? docs) #f]
        [(eq? (doc-id (car docs)) id) (car docs)]
        [else (loop (cdr docs))])))

;;; satin-doc-all : → (List Doc)
;;; Return all documentation entries.
(define (satin-doc-all)
  *satin-docs*)

;;; satin-doc-types : → (List Symbol)
;;; Return all documented form types.
(define (satin-doc-types)
  (map doc-id *satin-docs*))

;;; ============================================================
;;; Story Introspection
;;; ============================================================

;;; satin-describe : SatinSpec → Doc
;;; Generate documentation for a specific story.
(define (satin-describe spec)
  (if (not (satin-story? spec))
      #f
      (let* ([id (satin-story-id spec)]
             [title (satin-story-title spec)]
             [nodes (satin-story-nodes spec)]
             [dialogues (satin-story-dialogues spec)]
             [quests (satin-story-quests spec)]
             [exercises (satin-story-exercises spec)]
             [lessons (satin-story-lessons spec)]
             [mcqs (satin-story-mcqs spec)])
            `((id . ,id)
              (title . ,title)
              (type . story)
              (stats . ((nodes . ,(length nodes))
                        (dialogues . ,(length dialogues))
                        (quests . ,(length quests))
                        (exercises . ,(+ (length exercises)
                                         (length mcqs)
                                         (length (satin-story-short-answers spec))
                                         (length (satin-story-code-tasks spec))))
                        (lessons . ,(length lessons))))
              (node-ids . ,(map satin-node-id nodes))
              (dialogue-ids . ,(map satin-dialogue-id dialogues))
              (quest-ids . ,(map satin-quest-id quests))
              (exercise-ids . ,(append
                                (map satin-exercise-id exercises)
                                (map satin-mcq-id mcqs)
                                (map satin-short-answer-id (satin-story-short-answers spec))
                                (map satin-code-task-id (satin-story-code-tasks spec))))))))

;;; satin-describe-node : Node → Doc
;;; Generate documentation for a specific node.
(define (satin-describe-node node)
  (let ([id (satin-node-id node)]
        [title (satin-node-title node)]
        [body (satin-node-body node)]
        [choices (satin-node-choices node)])
       `((id . ,id)
         (type . node)
         (title . ,title)
         (body-preview . ,(if (and (string? body) (> (string-length body) 100))
                              (string-append (substring body 0 100) "...")
                              body))
         (choice-count . ,(length choices))
         (choice-targets . ,(map satin-choice-target choices))
         (is-end . ,(satin-node-end? node)))))

;;; ============================================================
;;; Help Display
;;; ============================================================

;;; satin-help : [Symbol] → void
;;; Display help for a form type, or general help if no argument.
(define (satin-help . args)
  (if (null? args)
      (satin-help-overview)
      (let ([id (car args)])
           (let ([doc (satin-doc id)])
                (if doc
                    (satin-display-doc doc)
                    (begin
                     (display (format "No documentation for '~a'
" id))
                     (display "Available forms:
")
                     (for-each
                      (lambda (d) (display (format "  ~a
" (doc-id d))))
                      *satin-docs*)))))))

;;; Display overview help.
(define (satin-help-overview)
  (display "Satin DSL Help
")
  (display "==============

")
  (display "Satin is a declarative DSL for authoring Quill stories.

")
  (display "Core forms:
")
  (for-each
   (lambda (d)
           (display (format "  (~a ...) - ~a
" (doc-id d) (doc-name d))))
   *satin-docs*)
  (display "
Use (satin-help '<form>) for detailed help.
"))

;;; Display documentation for a form.
(define (satin-display-doc doc)
  (display (format "~a
" (doc-name doc)))
  (display (make-string (string-length (doc-name doc)) #\=))
  (newline)
  (newline)
  (display (doc-description doc))
  (newline)
  (newline)
  (display "Fields:
")
  (for-each
   (lambda (f)
           (display (format "  ~a: ~a
" (car f) (cdr f))))
   (doc-fields doc))
  (newline)
  (display "Example:
")
  (for-each
   (lambda (ex)
           (display "  ")
           (write ex)
           (newline))
   (doc-examples doc))
  (newline)
  (let ([related (doc-related doc)])
       (unless (null? related)
               (display "Related: ")
               (display (string-join (map symbol->string related) ", "))
               (newline))))

;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Guard and Effect Documentation
;;; ============================================================

(define *satin-guard-docs*
  '((flag? . "Check if a flag is set: (flag? name)")
    (has-item? . "Check if player has item: (has-item? item)")
    (visited? . "Check if node was visited: (visited? node-id)")
    (var= . "Check variable equals: (var= name value)")
    (var>= . "Check variable >= : (var>= name value)")
    (var<= . "Check variable <= : (var<= name value)")
    (var> . "Check variable > : (var> name value)")
    (var< . "Check variable < : (var< name value)")
    (not . "Negate a guard: (not guard)")
    (and . "All guards must be true: (and guard ...)")
    (or . "Any guard must be true: (or guard ...)")))

(define *satin-effect-docs*
  '((set-flag! . "Set a flag: (set-flag! name)")
    (clear-flag! . "Clear a flag: (clear-flag! name)")
    (toggle-flag! . "Toggle a flag: (toggle-flag! name)")
    (give-item! . "Add item to inventory: (give-item! item)")
    (take-item! . "Remove item from inventory: (take-item! item)")
    (set-var! . "Set variable: (set-var! name value)")
    (inc-var! . "Increment variable: (inc-var! name [amount])")
    (dec-var! . "Decrement variable: (dec-var! name [amount])")
    (print! . "Display message: (print! message)")
    (goto! . "Jump to node: (goto! node-id)")
    (start-dialogue! . "Start dialogue: (start-dialogue! id)")
    (end-dialogue! . "End current dialogue: (end-dialogue!)")
    (activate-quest! . "Start quest: (activate-quest! id)")
    (complete-quest! . "Finish quest: (complete-quest! id)")
    (complete-step! . "Complete quest step: (complete-step! quest-id step-id)")
    (advance-clock! . "Advance time: (advance-clock! [amount])")
    (end . "End the story: (end)")))

;;; satin-guards : → void
;;; Display all guard predicates.
(define (satin-guards)
  (display "Satin Guard Predicates
")
  (display "======================

")
  (for-each
   (lambda (g)
           (display (format "  ~a
    ~a

" (car g) (cdr g))))
   *satin-guard-docs*))

;;; satin-effects : → void
;;; Display all effect forms.
(define (satin-effects)
  (display "Satin Effects
")
  (display "=============

")
  (for-each
   (lambda (e)
           (display (format "  ~a
    ~a

" (car e) (cdr e))))
   *satin-effect-docs*))
