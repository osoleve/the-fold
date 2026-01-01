;;; playpen/chronicle/tagless-chronicle.ss — Tagless Final Chronicle DSL
;;;
;;; Demonstrates the tagless final pattern applied to the Chronicle
;;; narrative engine. This enables multiple interpreters:
;;;   - Runtime execution (play the story)
;;;   - Validation (check for dead ends, unreachable scenes)
;;;   - Graph export (generate DOT/Mermaid diagrams)
;;;   - Analysis (count branches, measure complexity)
;;;
;;; The core insight: Chronicle definitions are functions that take
;;; an algebra dictionary and produce a result. Different dictionaries
;;; yield different interpretations of the same narrative structure.

(load "core/dsl/tagless.ss")

;;; ============================================================
;;; Chronicle Algebra
;;; ============================================================
;;;
;;; Core operations for defining narratives:
;;;   scene   : Id -> Text -> [Choice] -> repr Scene
;;;   choice  : Label -> Target -> Guard -> Effect -> repr Choice
;;;   guard   : Predicate -> repr Guard
;;;   effect  : Action -> repr Effect
;;;   text    : Template -> repr Text

;;; make-chronicle-algebra : ops... -> ChronicleDict
(define (make-chronicle-algebra
         scene-op choice-op guard-op effect-op text-op
         guard-always-op guard-flag-op guard-item-op guard-var-op
         guard-and-op guard-or-op guard-not-op
         effect-noop-op effect-flag-op effect-item-op effect-var-op
         effect-seq-op)
  (make-dict 'chronicle
             `((scene . ,scene-op)
               (choice . ,choice-op)
               (guard . ,guard-op)
               (effect . ,effect-op)
               (text . ,text-op)
               ;; Guard constructors
               (guard-always . ,guard-always-op)
               (guard-flag . ,guard-flag-op)
               (guard-item . ,guard-item-op)
               (guard-var . ,guard-var-op)
               (guard-and . ,guard-and-op)
               (guard-or . ,guard-or-op)
               (guard-not . ,guard-not-op)
               ;; Effect constructors
               (effect-noop . ,effect-noop-op)
               (effect-flag . ,effect-flag-op)
               (effect-item . ,effect-item-op)
               (effect-var . ,effect-var-op)
               (effect-seq . ,effect-seq-op))))

;;; ============================================================
;;; Algebra Accessors
;;; ============================================================

(define (tl-scene d id text . choices)
  (apply (dict-ref d 'scene) id text choices))

(define (tl-choice d label target guard effect)
  ((dict-ref d 'choice) label target guard effect))

(define (tl-text d template)
  ((dict-ref d 'text) template))

;; Guards
(define (tl-guard-always d)
  ((dict-ref d 'guard-always)))

(define (tl-guard-flag d name)
  ((dict-ref d 'guard-flag) name))

(define (tl-guard-item d name)
  ((dict-ref d 'guard-item) name))

(define (tl-guard-var d name op value)
  ((dict-ref d 'guard-var) name op value))

(define (tl-guard-and d . guards)
  (apply (dict-ref d 'guard-and) guards))

(define (tl-guard-or d . guards)
  (apply (dict-ref d 'guard-or) guards))

(define (tl-guard-not d guard)
  ((dict-ref d 'guard-not) guard))

;; Effects
(define (tl-effect-noop d)
  ((dict-ref d 'effect-noop)))

(define (tl-effect-flag d name value)
  ((dict-ref d 'effect-flag) name value))

(define (tl-effect-item d action name)
  ((dict-ref d 'effect-item) action name))

(define (tl-effect-var d name op value)
  ((dict-ref d 'effect-var) name op value))

(define (tl-effect-seq d . effects)
  (apply (dict-ref d 'effect-seq) effects))

;;; ============================================================
;;; Runtime Interpreter
;;; ============================================================
;;;
;;; Produces executable Chronicle structures.

(define runtime-chronicle-dict
  (make-chronicle-algebra
   ;; scene: (id text choices...) -> Scene structure
   (lambda (id text . choices)
           `(scene ,id ,text ,@choices))
   
   ;; choice: (label target guard effect) -> Choice structure
   (lambda (label target guard effect)
           `(choice ,label ,target ,guard ,effect))
   
   ;; guard: predicate -> Guard procedure
   (lambda (pred) pred)
   
   ;; effect: action -> Effect procedure
   (lambda (action) action)
   
   ;; text: template -> Text (identity for now)
   (lambda (template) template)
   
   ;; Guard constructors
   (lambda () (lambda (state) #t))  ; guard-always
   
   (lambda (name)  ; guard-flag
           (lambda (state)
                   (let ([flags (assq 'flags state)])
                        (and flags (assq name (cdr flags)) #t))))
   
   (lambda (name)  ; guard-item
           (lambda (state)
                   (let ([inv (assq 'inventory state)])
                        (and inv (memq name (cdr inv)) #t))))
   
   (lambda (name op value)  ; guard-var
           (lambda (state)
                   (let ([vars (assq 'variables state)])
                        (if vars
                            (let ([var (assq name (cdr vars))])
                                 (if var
                                     (op (cdr var) value)
                                     #f))
                            #f))))
   
   (lambda guards  ; guard-and
           (lambda (state)
                   (and-map (lambda (g) (g state)) guards)))
   
   (lambda guards  ; guard-or
           (lambda (state)
                   (or-map (lambda (g) (g state)) guards)))
   
   (lambda (guard)  ; guard-not
           (lambda (state)
                   (not (guard state))))
   
   ;; Effect constructors
   (lambda () (lambda (state) state))  ; effect-noop
   
   (lambda (name value)  ; effect-flag
           (lambda (state)
                   (let* ([flags (or (assq 'flags state) (cons 'flags '()))]
                          [new-flags (cons (cons name value)
                                           (filter (lambda (p) (not (eq? (car p) name)))
                                                   (cdr flags)))])
                         (cons (cons 'flags new-flags)
                               (filter (lambda (p) (not (eq? (car p) 'flags))) state)))))
   
   (lambda (action name)  ; effect-item
           (lambda (state)
                   (let* ([inv (or (assq 'inventory state) (cons 'inventory '()))]
                          [items (cdr inv)]
                          [new-items (case action
                                           [(add) (if (memq name items) items (cons name items))]
                                           [(remove) (filter (lambda (i) (not (eq? i name))) items)]
                                           [else items])])
                         (cons (cons 'inventory new-items)
                               (filter (lambda (p) (not (eq? (car p) 'inventory))) state)))))
   
   (lambda (name op value)  ; effect-var
           (lambda (state)
                   (let* ([vars (or (assq 'variables state) (cons 'variables '()))]
                          [current (let ([v (assq name (cdr vars))]) (if v (cdr v) 0))]
                          [new-val (case op
                                         [(set) value]
                                         [(add) (+ current value)]
                                         [(sub) (- current value)]
                                         [(mul) (* current value)]
                                         [else current])]
                          [new-vars (cons (cons name new-val)
                                          (filter (lambda (p) (not (eq? (car p) name)))
                                                  (cdr vars)))])
                         (cons (cons 'variables new-vars)
                               (filter (lambda (p) (not (eq? (car p) 'variables))) state)))))
   
   (lambda effects  ; effect-seq
           (lambda (state)
                   (fold-left (lambda (s e) (e s)) state effects)))))

;;; Helper for and-map / or-map
(define (and-map pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (and-map pred (cdr lst)))))

(define (or-map pred lst)
  (and (not (null? lst))
       (or (pred (car lst))
           (or-map pred (cdr lst)))))

;;; ============================================================
;;; Validation Interpreter
;;; ============================================================
;;;
;;; Collects structural information for validation.

(define validation-chronicle-dict
  (make-chronicle-algebra
   ;; scene: collect (id . targets)
   (lambda (id text . choices)
           (cons id (map (lambda (c) (list-ref c 2)) choices)))
   
   ;; choice: collect target
   (lambda (label target guard effect)
           (list 'choice label target))
   
   ;; guard/effect/text: identity
   (lambda (pred) 'guard)
   (lambda (action) 'effect)
   (lambda (template) template)
   
   ;; Guard constructors - just tags
   (lambda () 'always)
   (lambda (name) `(flag ,name))
   (lambda (name) `(item ,name))
   (lambda (name op value) `(var ,name ,op ,value))
   (lambda guards `(and ,@guards))
   (lambda guards `(or ,@guards))
   (lambda (guard) `(not ,guard))
   
   ;; Effect constructors - just tags
   (lambda () 'noop)
   (lambda (name value) `(flag ,name ,value))
   (lambda (action name) `(item ,action ,name))
   (lambda (name op value) `(var ,name ,op ,value))
   (lambda effects `(seq ,@effects))))

;;; validate-chronicle : ChronicleProgram -> ValidationResult
(define (validate-chronicle program start-scene)
  (let* ([scenes (program validation-chronicle-dict)]
         [scene-ids (map car scenes)]
         [all-targets (apply append (map cdr scenes))])
        ;; Check for issues
        (let ([missing-targets (filter (lambda (t) (not (memq t scene-ids)))
                                       all-targets)]
              [unreachable (filter (lambda (s)
                                           (and (not (eq? s start-scene))
                                                (not (memq s all-targets))))
                                   scene-ids)]
              [dead-ends (filter (lambda (s) (null? (cdr (assq s scenes))))
                                 scene-ids)])
             `((scenes . ,scene-ids)
               (missing-targets . ,missing-targets)
               (unreachable . ,unreachable)
               (dead-ends . ,dead-ends)
               (valid . ,(and (null? missing-targets)
                              (null? unreachable)))))))

;;; ============================================================
;;; Graph Export Interpreter
;;; ============================================================
;;;
;;; Generates graph representations for visualization.

(define graph-chronicle-dict
  (make-chronicle-algebra
   ;; scene: (id text choices...) -> (id . edges)
   (lambda (id text . choices)
           (cons id (map (lambda (c)
                                 (list (list-ref c 1)   ; label
                                       (list-ref c 2))) ; target
                         choices)))
   
   ;; choice: (label target guard effect) -> (label target)
   (lambda (label target guard effect)
           (list label target guard))
   
   ;; Others: identity
   (lambda (pred) pred)
   (lambda (action) action)
   (lambda (template) template)
   
   ;; Guards - descriptive strings
   (lambda () "always")
   (lambda (name) (format "[flag: ~a]" name))
   (lambda (name) (format "[has: ~a]" name))
   (lambda (name op value) (format "[~a ~a ~a]" name op value))
   (lambda guards (format "(~a)" (string-join (map ->string guards) " AND ")))
   (lambda guards (format "(~a)" (string-join (map ->string guards) " OR ")))
   (lambda (guard) (format "NOT ~a" guard))
   
   ;; Effects - descriptive strings
   (lambda () "")
   (lambda (name value) (format "flag:~a=~a" name value))
   (lambda (action name) (format "~a:~a" action name))
   (lambda (name op value) (format "~a ~a ~a" name op value))
   (lambda effects (string-join effects "; "))))

;;; Helper for string-join (if not available)
(define (string-join strings sep)
  (if (null? strings)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strings)
                 (cdr strings))))

(define (->string x)
  (if (string? x) x (format "~a" x)))

;;; chronicle->dot : ChronicleProgram -> String
;;; Generate GraphViz DOT format.
(define (chronicle->dot program title)
  (let ([scenes (program graph-chronicle-dict)])
       (string-append
        "digraph " (symbol->string title) " {\n"
        "  rankdir=TB;\n"
        "  node [shape=box];\n"
        (apply string-append
               (map (lambda (scene)
                            (let ([id (car scene)]
                                  [edges (cdr scene)])
                                 (apply string-append
                                        (map (lambda (edge)
                                                     (format "  ~a -> ~a [label=\"~a\"];\n"
                                                             id
                                                             (cadr edge)
                                                             (car edge)))
                                             edges))))
                    scenes))
        "}\n")))

;;; chronicle->mermaid : ChronicleProgram -> String
;;; Generate Mermaid flowchart format.
(define (chronicle->mermaid program)
  (let ([scenes (program graph-chronicle-dict)])
       (string-append
        "flowchart TD\n"
        (apply string-append
               (map (lambda (scene)
                            (let ([id (car scene)]
                                  [edges (cdr scene)])
                                 (apply string-append
                                        (map (lambda (edge)
                                                     (format "    ~a -->|~a| ~a\n"
                                                             id
                                                             (car edge)
                                                             (cadr edge)))
                                             edges))))
                    scenes)))))

;;; ============================================================
;;; Analysis Interpreter
;;; ============================================================
;;;
;;; Computes metrics about the narrative.

(define analysis-chronicle-dict
  (make-chronicle-algebra
   ;; scene: count choices, text may be string or number
   (lambda (id text . choices)
           `(scene ,id ,(length choices) ,(if (string? text) (string-length text) text)))
   
   ;; choice: count conditions
   (lambda (label target guard effect)
           `(choice ,(if (eq? guard 'always) 0 1)
             ,(if (eq? effect 'noop) 0 1)))
   
   ;; Others
   (lambda (pred) pred)
   (lambda (action) action)
   (lambda (template) (string-length template))
   
   ;; Guards - complexity score
   (lambda () 'always)
   (lambda (name) 1)
   (lambda (name) 1)
   (lambda (name op value) 2)
   (lambda guards (+ 1 (apply + (map (lambda (g) (if (number? g) g 0)) guards))))
   (lambda guards (+ 1 (apply + (map (lambda (g) (if (number? g) g 0)) guards))))
   (lambda (guard) (+ 1 (if (number? guard) guard 0)))
   
   ;; Effects - action count
   (lambda () 'noop)
   (lambda (name value) 1)
   (lambda (action name) 1)
   (lambda (name op value) 1)
   (lambda effects (apply + (filter number? effects)))))

;;; analyze-chronicle : ChronicleProgram -> Analysis
(define (analyze-chronicle program)
  (let ([scenes (program analysis-chronicle-dict)])
       (let ([scene-count (length scenes)]
             ;; scenes are (scene id choice-count text-len)
             [total-choices (apply + (map (lambda (s) (caddr s)) scenes))]
             [total-text-len (apply + (map (lambda (s) (cadddr s)) scenes))])
            `((scene-count . ,scene-count)
              (total-choices . ,total-choices)
              (avg-choices-per-scene . ,(if (> scene-count 0)
                                            (/ total-choices scene-count)
                                            0))
              (total-text-length . ,total-text-len)
              (avg-text-per-scene . ,(if (> scene-count 0)
                                         (/ total-text-len scene-count)
                                         0))))))

;;; ============================================================
;;; Example: The Mysterious Door
;;; ============================================================
;;;
;;; A sample chronicle program in tagless final style.

(define (mysterious-door-chronicle d)
  (list
   ;; Entrance scene
   (tl-scene d 'entrance
             "You stand before a mysterious door. It's locked."
             (tl-choice d "Try the door" 'entrance
                        (tl-guard-not d (tl-guard-item d 'key))
                        (tl-effect-noop d))
             (tl-choice d "Use the key" 'hallway
                        (tl-guard-item d 'key)
                        (tl-effect-noop d))
             (tl-choice d "Search the area" 'entrance
                        (tl-guard-not d (tl-guard-item d 'key))
                        (tl-effect-item d 'add 'key))
             (tl-choice d "Leave" 'outside
                        (tl-guard-always d)
                        (tl-effect-noop d)))
   
   ;; Hallway scene
   (tl-scene d 'hallway
             "The door creaks open. A long hallway stretches before you."
             (tl-choice d "Go forward" 'treasure
                        (tl-guard-always d)
                        (tl-effect-flag d 'entered-hall #t))
             (tl-choice d "Go back" 'entrance
                        (tl-guard-always d)
                        (tl-effect-noop d)))
   
   ;; Treasure scene
   (tl-scene d 'treasure
             "You found the treasure! Gold coins glitter in the torchlight."
             (tl-choice d "Take the gold" 'ending
                        (tl-guard-always d)
                        (tl-effect-var d 'gold 'add 1000)))
   
   ;; Outside scene
   (tl-scene d 'outside
             "You leave the mysterious building behind."
             )
   
   ;; Ending scene
   (tl-scene d 'ending
             "Congratulations! You've completed the adventure."
             )))

;;; ============================================================
;;; Running the Example
;;; ============================================================

;;; Get runtime structure
;;; (mysterious-door-chronicle runtime-chronicle-dict)

;;; Validate the chronicle
;;; (validate-chronicle mysterious-door-chronicle 'entrance)

;;; Export to DOT
;;; (chronicle->dot mysterious-door-chronicle 'mysterious_door)

;;; Analyze complexity
;;; (analyze-chronicle mysterious-door-chronicle)

;;; ============================================================
;;; Integration with Original Chronicle
;;; ============================================================
;;;
;;; Convert tagless programs to original Chronicle structures.

;;; Note: This requires loading the original chronicle module.
;;; For now, we provide a simple execution model.

;;; make-chronicle-runtime : ChronicleProgram -> Runtime
(define (make-chronicle-runtime program start-scene)
  (let ([scenes (program runtime-chronicle-dict)]
        [state '()])
       (list 'chronicle-runtime scenes start-scene state)))

;;; chronicle-runtime-scene : Runtime -> Scene
(define (chronicle-runtime-scene runtime)
  (let ([scenes (list-ref runtime 1)]
        [current (list-ref runtime 2)])
       ;; Scenes are ((scene id text choices...) ...), find by id
       (let loop ([ss scenes])
            (cond
             [(null? ss) #f]
             [(eq? (cadr (car ss)) current) (car ss)]
             [else (loop (cdr ss))]))))

;;; chronicle-runtime-choices : Runtime -> [Choice]
;;; Get visible choices (guards pass).
(define (chronicle-runtime-choices runtime)
  (let* ([scene (chronicle-runtime-scene runtime)]
         [state (list-ref runtime 3)]
         [choices (cdddr scene)])  ; skip 'scene, id, text
        (filter (lambda (c)
                        (let ([guard (list-ref c 3)])
                             (guard state)))
                choices)))

;;; chronicle-runtime-choose : Runtime -> Int -> Runtime
;;; Make a choice and transition to next scene.
(define (chronicle-runtime-choose runtime idx)
  (let* ([choices (chronicle-runtime-choices runtime)]
         [choice (list-ref choices idx)]
         [target (list-ref choice 2)]
         [effect (list-ref choice 4)]
         [old-state (list-ref runtime 3)]
         [new-state (effect old-state)]
         [scenes (list-ref runtime 1)])
        (list 'chronicle-runtime scenes target new-state)))

;;; chronicle-runtime-text : Runtime -> String
;;; Get current scene text.
(define (chronicle-runtime-text runtime)
  (let ([scene (chronicle-runtime-scene runtime)])
       (list-ref scene 2)))

;;; ============================================================
;;; Summary
;;; ============================================================
;;;
;;; The tagless final Chronicle demonstrates:
;;;
;;; 1. Same program definition, multiple interpretations:
;;;    - runtime-chronicle-dict: Executable story
;;;    - validation-chronicle-dict: Structural validation
;;;    - graph-chronicle-dict: Visualization export
;;;    - analysis-chronicle-dict: Complexity metrics
;;;
;;; 2. Extensibility:
;;;    - Add new guard types without changing core
;;;    - Add new effect types via dictionary extension
;;;    - Add new interpreters without modifying programs
;;;
;;; 3. Composability:
;;;    - run-both for simultaneous interpretation
;;;    - Dictionary transformers for cross-cutting concerns
;;;
;;; 4. Type safety (conceptual):
;;;    - Guards and effects are properly typed by construction
;;;    - Invalid combinations are impossible to express
