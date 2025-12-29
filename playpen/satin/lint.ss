;;; playpen/satin/lint.ss — Satin linter for style and correctness
;;;
;;; Provides style suggestions and warnings beyond basic validation.
;;; Catches common authoring mistakes and suggests improvements.

;;; ============================================================
;;; Lint Issue Structure
;;; ============================================================

;;; Lint issues are similar to validation issues but include:
;;; - 'style' level for purely stylistic suggestions
;;; - 'hint' level for helpful tips

(define (satin-lint-issue level code message span suggestion)
  (list 'satin-lint-issue level code message span suggestion))

(define (satin-lint-issue? x)
  (and (pair? x)
       (eq? (car x) 'satin-lint-issue)))

(define (satin-lint-level i) (list-ref i 1))
(define (satin-lint-code i) (list-ref i 2))
(define (satin-lint-message i) (list-ref i 3))
(define (satin-lint-span i) (list-ref i 4))
(define (satin-lint-suggestion i) (list-ref i 5))

;;; Issue levels
(define (satin-lint-error? i)
  (and (satin-lint-issue? i)
       (eq? (satin-lint-level i) 'error)))

(define (satin-lint-warning? i)
  (and (satin-lint-issue? i)
       (eq? (satin-lint-level i) 'warning)))

(define (satin-lint-style? i)
  (and (satin-lint-issue? i)
       (eq? (satin-lint-level i) 'style)))

(define (satin-lint-hint? i)
  (and (satin-lint-issue? i)
       (eq? (satin-lint-level i) 'hint)))

;;; ============================================================
;;; Main Lint Function
;;; ============================================================

;;; satin-lint : SatinSpec → (List LintIssue)
;;; Lint a Satin specification for style and correctness issues.
(define (satin-lint spec)
  (if (not (satin-story? spec))
      (list (satin-lint-issue 'error 'invalid-structure
                              "Expected a satin-story form"
                              (get-span spec)
                              "Start with (satin-story ...)"))
      (let ([issues '()])
           (define (add! issue)
             (set! issues (cons issue issues)))
           
           ;; Story-level lints
           (satin-lint-story spec add!)
           
           ;; Node-level lints
           (for-each (lambda (n) (satin-lint-node n add!))
                     (satin-story-nodes spec))
           
           ;; Dialogue lints
           (for-each (lambda (d) (satin-lint-dialogue d add!))
                     (satin-story-dialogues spec))
           
           ;; Quest lints
           (for-each (lambda (q) (satin-lint-quest q add!))
                     (satin-story-quests spec))
           
           ;; Exercise lints
           (for-each (lambda (e) (satin-lint-exercise e add!))
                     (satin-story-exercises spec))
           
           ;; Education form lints
           (for-each (lambda (l) (satin-lint-lesson l add!))
                     (satin-story-lessons spec))
           
           (for-each (lambda (m) (satin-lint-mcq m add!))
                     (satin-story-mcqs spec))
           
           (reverse issues))))

;;; ============================================================
;;; Story-Level Lints
;;; ============================================================

(define (satin-lint-story spec add!)
  ;; Check for missing title
  (unless (satin-get-field (satin-story-fields spec) 'title #f)
          (add! (satin-lint-issue 'style 'missing-title
                                  "Story has no title"
                                  (get-span spec)
                                  "Add (title \"Your Story Title\")")))
  
  ;; Check for missing author
  (unless (satin-get-field (satin-story-fields spec) 'author #f)
          (add! (satin-lint-issue 'style 'missing-author
                                  "Story has no author"
                                  (get-span spec)
                                  "Add (author \"Author Name\")")))
  
  ;; Check for very few nodes
  (let ([node-count (length (satin-story-nodes spec))])
       (when (and (> node-count 0) (< node-count 3))
             (add! (satin-lint-issue 'hint 'few-nodes
                                     (format "Story has only ~a node(s)" node-count)
                                     (get-span spec)
                                     "Consider adding more content"))))
  
  ;; Check for orphan nodes (not reachable from start)
  (let* ([nodes (satin-story-nodes spec)]
         [start (satin-story-start spec)]
         [reachable (satin-find-reachable nodes start)]
         [all-ids (map satin-node-id nodes)]
         [orphans (filter (lambda (id) (not (memq id reachable))) all-ids)])
        (for-each
         (lambda (id)
                 (add! (satin-lint-issue 'warning 'orphan-node
                                         (format "Node ~a is not reachable from start" id)
                                         (get-span spec)
                                         "Check choice targets or remove unused node")))
         orphans))
  
  ;; Check for naming conventions
  (let ([id (satin-story-id spec)])
       (when (and (symbol? id)
                  (let ([s (symbol->string id)])
                       (or (char-upper-case? (string-ref s 0))
                           (string-contains? s " "))))
             (add! (satin-lint-issue 'style 'id-naming
                                     (format "Story id '~a' should use kebab-case" id)
                                     (get-span spec)
                                     "Use lowercase-with-hyphens for IDs")))))

;;; ============================================================
;;; Node-Level Lints
;;; ============================================================

(define (satin-lint-node node add!)
  (let ([id (satin-node-id node)]
        [body (satin-node-body node)]
        [choices (satin-node-choices node)]
        [is-end? (satin-node-end? node)])
       
       ;; Check for empty body
       (when (or (not body) (and (string? body) (string=? body "")))
             (add! (satin-lint-issue 'warning 'empty-body
                                     (format "Node ~a has no body text" id)
                                     (get-span node)
                                     "Add (body \"...\") to describe the scene")))
       
       ;; Check for very long body
       (when (and (string? body) (> (string-length body) 1500))
             (add! (satin-lint-issue 'style 'long-body
                                     (format "Node ~a body is very long (~a chars)" id (string-length body))
                                     (get-span node)
                                     "Consider breaking into multiple nodes")))
       
       ;; Check for dead-end nodes (no choices, not marked end)
       (when (and (null? choices) (not is-end?))
             (add! (satin-lint-issue 'warning 'dead-end
                                     (format "Node ~a has no choices and is not marked (end)" id)
                                     (get-span node)
                                     "Add choices or mark as (end)")))
       
       ;; Check for too many choices
       (when (> (length choices) 6)
             (add! (satin-lint-issue 'style 'too-many-choices
                                     (format "Node ~a has ~a choices" id (length choices))
                                     (get-span node)
                                     "Consider reducing choices to improve clarity")))
       
       ;; Check for self-referential choices (same target as node id)
       (for-each
        (lambda (c)
                (let ([target (satin-choice-target c)])
                     (when (eq? target id)
                           (add! (satin-lint-issue 'hint 'self-reference
                                                   (format "Choice in ~a loops back to itself" id)
                                                   (get-span c)
                                                   "This creates a loop; make sure it's intentional")))))
        choices)
       
       ;; Check for duplicate choice labels
       (let* ([labels (filter string? (map satin-choice-label choices))]
              [seen '()]
              [dups '()])
             (for-each
              (lambda (l)
                      (if (member l seen)
                          (set! dups (cons l dups))
                          (set! seen (cons l seen))))
              labels)
             (for-each
              (lambda (l)
                      (add! (satin-lint-issue 'warning 'duplicate-choice-label
                                              (format "Duplicate choice label: ~s" l)
                                              (get-span node)
                                              "Use unique labels for clarity")))
              dups))))

;;; ============================================================
;;; Dialogue-Level Lints
;;; ============================================================

(define (satin-lint-dialogue dialogue add!)
  (let ([id (satin-dialogue-id dialogue)]
        [nodes (satin-dialogue-nodes dialogue)])
       
       ;; Check for empty dialogue
       (when (null? nodes)
             (add! (satin-lint-issue 'warning 'empty-dialogue
                                     (format "Dialogue ~a has no nodes" id)
                                     (get-span dialogue)
                                     "Add at least one dialogue node")))))

;;; ============================================================
;;; Quest-Level Lints
;;; ============================================================

(define (satin-lint-quest quest add!)
  (let ([id (satin-quest-id quest)]
        [steps (satin-quest-steps quest)])
       
       ;; Check for very short quest
       (when (and (not (null? steps)) (< (length steps) 2))
             (add! (satin-lint-issue 'hint 'short-quest
                                     (format "Quest ~a has only ~a step(s)" id (length steps))
                                     (get-span quest)
                                     "Consider adding more steps or using a flag instead")))))

;;; ============================================================
;;; Exercise-Level Lints
;;; ============================================================

(define (satin-lint-exercise exercise add!)
  (let ([id (satin-exercise-id exercise)]
        [hints (satin-exercise-hints exercise)]
        [rubric (satin-exercise-rubric exercise)])
       
       ;; Check for no hints
       (when (null? hints)
             (add! (satin-lint-issue 'hint 'no-hints
                                     (format "Exercise ~a has no hints" id)
                                     (get-span exercise)
                                     "Consider adding (hints ...) for struggling learners")))))

;;; ============================================================
;;; Lesson-Level Lints
;;; ============================================================

(define (satin-lint-lesson lesson add!)
  (let ([id (satin-lesson-id lesson)]
        [objectives (satin-lesson-objectives lesson)]
        [sequence (satin-lesson-sequence lesson)])
       
       ;; Check for missing objectives
       (when (null? objectives)
             (add! (satin-lint-issue 'style 'no-objectives
                                     (format "Lesson ~a has no learning objectives" id)
                                     (get-span lesson)
                                     "Add (objectives ...) to clarify what students will learn")))
       
       ;; Check for empty sequence
       (when (null? sequence)
             (add! (satin-lint-issue 'warning 'empty-sequence
                                     (format "Lesson ~a has no content in sequence" id)
                                     (get-span lesson)
                                     "Add node/exercise IDs to the sequence")))))

;;; ============================================================
;;; MCQ-Level Lints
;;; ============================================================

(define (satin-lint-mcq mcq add!)
  (let ([id (satin-mcq-id mcq)]
        [options (satin-mcq-options mcq)]
        [correct (satin-mcq-correct mcq)]
        [explanation (satin-mcq-explanation mcq)])
       
       ;; Check option count
       (when (< (length options) 2)
             (add! (satin-lint-issue 'warning 'few-options
                                     (format "MCQ ~a has fewer than 2 options" id)
                                     (get-span mcq)
                                     "Add more answer options")))
       
       (when (> (length options) 6)
             (add! (satin-lint-issue 'style 'many-options
                                     (format "MCQ ~a has ~a options" id (length options))
                                     (get-span mcq)
                                     "Consider reducing to 4-5 options")))
       
       ;; Check correct index is valid
       (when (or (< correct 1) (> correct (length options)))
             (add! (satin-lint-issue 'error 'invalid-correct
                                     (format "MCQ ~a correct index ~a is out of range" id correct)
                                     (get-span mcq)
                                     "Use an index from 1 to the number of options")))
       
       ;; Check for missing explanation
       (when (or (not explanation) (string=? explanation ""))
             (add! (satin-lint-issue 'hint 'no-explanation
                                     (format "MCQ ~a has no explanation" id)
                                     (get-span mcq)
                                     "Add (explanation ...) to help learners understand")))))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; Find all nodes reachable from start via choices.
(define (satin-find-reachable nodes start)
  (let ([id->choices (make-hashtable symbol-hash eq?)])
       ;; Build map of node id -> choice targets
       (for-each
        (lambda (n)
                (let ([id (satin-node-id n)]
                      [choices (satin-node-choices n)])
                     (hashtable-set! id->choices id
                                     (filter symbol? (map satin-choice-target choices)))))
        nodes)
       ;; BFS from start
       (let loop ([queue (list start)]
                  [visited '()])
            (if (null? queue)
                visited
                (let ([current (car queue)]
                      [rest (cdr queue)])
                     (if (memq current visited)
                         (loop rest visited)
                         (let ([targets (hashtable-ref id->choices current '())])
                              (loop (append rest (filter (lambda (t) (not (memq t visited))) targets))
                                    (cons current visited)))))))))

;;; Check if string contains substring.
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (if (> sub-len str-len)
           #f
           (let loop ([i 0])
                (cond
                 [(> (+ i sub-len) str-len) #f]
                 [(string-match-at? str substr i) #t]
                 [else (loop (+ i 1))])))))

(define (string-match-at? str substr pos)
  (let ([sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(= i sub-len) #t]
             [(not (char=? (string-ref str (+ pos i))
                           (string-ref substr i))) #f]
             [else (loop (+ i 1))]))))

;;; ============================================================
;;; Issue Formatting
;;; ============================================================

;;; satin-format-lint : LintIssue → String
(define (satin-format-lint issue)
  (let ([level (satin-lint-level issue)]
        [code (satin-lint-code issue)]
        [message (satin-lint-message issue)]
        [span (satin-lint-span issue)]
        [suggestion (satin-lint-suggestion issue)])
       (string-append
        (case level
              [(error) "ERROR"]
              [(warning) "WARNING"]
              [(style) "STYLE"]
              [(hint) "HINT"]
              [else "ISSUE"])
        " [" (symbol->string code) "]: "
        message
        (if (and (span? span) (not (no-span? span)))
            (string-append "
  at " (span->string span))
            "")
        (if suggestion
            (string-append "
  → " suggestion)
            ""))))

;;; satin-print-lints : (List LintIssue) → void
(define (satin-print-lints issues)
  (if (null? issues)
      (display "No lint issues found.
")
      (for-each
       (lambda (i)
               (display (satin-format-lint i))
               (newline))
       issues)))
