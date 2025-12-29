;;; playpen/satin/pretty.ss — Pretty-printer for Satin forms
;;;
;;; Formats Satin specifications for display and debugging.
;;; Produces readable, consistently-indented output.

;;; ============================================================
;;; Pretty-Printer Configuration
;;; ============================================================

(define *satin-indent-width* 2)
(define *satin-max-line-width* 80)

;;; ============================================================
;;; Core Pretty-Printing
;;; ============================================================

;;; satin-pretty : SatinSpec → String
;;; Pretty-print an entire Satin specification.
(define (satin-pretty spec)
  (if (satin-story? spec)
      (satin-pretty-story spec)
      (satin-pretty-form spec 0)))

;;; satin-pretty-story : SatinStory → String
;;; Pretty-print a story with proper structure.
(define (satin-pretty-story spec)
  (let ([fields (satin-story-fields spec)])
       (string-append
        "(satin-story
"
        (satin-pretty-story-fields fields 1)
        ")
")))

;;; Pretty-print story fields grouped by type.
(define (satin-pretty-story-fields fields indent)
  (let* ([meta-fields (filter satin-meta-field? fields)]
         [nodes (filter satin-node? fields)]
         [dialogues (filter satin-dialogue? fields)]
         [quests (filter satin-quest? fields)]
         [exercises (filter satin-exercise? fields)]
         [lessons (filter satin-lesson? fields)]
         [mcqs (filter satin-mcq? fields)]
         [short-answers (filter satin-short-answer? fields)]
         [code-tasks (filter satin-code-task? fields)]
         [masteries (filter satin-mastery? fields)]
         [rules (filter satin-rule? fields)]
         [timelines (filter satin-timeline? fields)])
        (string-append
         ;; Metadata first
         (satin-pretty-fields meta-fields indent)
         ;; Then content
         (if (null? nodes) ""
             (string-append
              "
"
              (satin-indent indent) ";; Nodes
"
              (satin-pretty-fields nodes indent)))
         (if (null? dialogues) ""
             (string-append
              "
"
              (satin-indent indent) ";; Dialogues
"
              (satin-pretty-fields dialogues indent)))
         (if (null? quests) ""
             (string-append
              "
"
              (satin-indent indent) ";; Quests
"
              (satin-pretty-fields quests indent)))
         (if (null? exercises) ""
             (string-append
              "
"
              (satin-indent indent) ";; Exercises
"
              (satin-pretty-fields exercises indent)))
         (if (null? lessons) ""
             (string-append
              "
"
              (satin-indent indent) ";; Lessons
"
              (satin-pretty-fields lessons indent)))
         (if (null? mcqs) ""
             (string-append
              "
"
              (satin-indent indent) ";; MCQs
"
              (satin-pretty-fields mcqs indent)))
         (if (null? short-answers) ""
             (string-append
              "
"
              (satin-indent indent) ";; Short Answers
"
              (satin-pretty-fields short-answers indent)))
         (if (null? code-tasks) ""
             (string-append
              "
"
              (satin-indent indent) ";; Code Tasks
"
              (satin-pretty-fields code-tasks indent)))
         (if (null? masteries) ""
             (string-append
              "
"
              (satin-indent indent) ";; Masteries
"
              (satin-pretty-fields masteries indent)))
         (if (null? rules) ""
             (string-append
              "
"
              (satin-indent indent) ";; Rules
"
              (satin-pretty-fields rules indent)))
         (if (null? timelines) ""
             (string-append
              "
"
              (satin-indent indent) ";; Timeline
"
              (satin-pretty-fields timelines indent))))))

;;; Check if a field is a metadata field (id, title, author, etc.)
(define (satin-meta-field? x)
  (and (pair? x)
       (memq (car (strip-span x)) '(id title author version start meta))))

;;; ============================================================
;;; Form-Specific Pretty-Printers
;;; ============================================================

;;; Pretty-print a node form.
(define (satin-pretty-node node indent)
  (let ([id (satin-node-id node)]
        [fields (satin-node-fields node)])
       (string-append
        (satin-indent indent)
        "(node " (symbol->string id) "
"
        (satin-pretty-node-fields fields (+ indent 1))
        (satin-indent indent) ")
")))

;;; Pretty-print node fields.
(define (satin-pretty-node-fields fields indent)
  (let ([title (satin-get-field fields 'title #f)]
        [body (satin-get-field fields 'body #f)]
        [choices (filter satin-choice? fields)]
        [on-enter (satin-get-all-fields fields 'on-enter)]
        [exercise-ref (satin-get-field fields 'exercise #f)]
        [end? (satin-has-field? fields 'end)])
       (string-append
        (if title
            (string-append (satin-indent indent) "(title " (satin-pretty-string title) ")
")
            "")
        (if body
            (string-append (satin-indent indent) "(body " (satin-pretty-string body) ")
")
            "")
        (if (null? on-enter) ""
            (string-append
             (satin-indent indent) "(on-enter
"
             (satin-pretty-fields (cdr (strip-span (car on-enter))) (+ indent 1))
             (satin-indent indent) ")
"))
        (if exercise-ref
            (string-append (satin-indent indent) "(exercise " (symbol->string exercise-ref) ")
")
            "")
        (if end?
            (string-append (satin-indent indent) "(end)
")
            "")
        (satin-pretty-choices choices indent))))

;;; Pretty-print choices.
(define (satin-pretty-choices choices indent)
  (apply string-append
         (map (lambda (c) (satin-pretty-choice c indent)) choices)))

;;; Pretty-print a single choice.
(define (satin-pretty-choice choice indent)
  (let ([label (satin-choice-label choice)]
        [target (satin-choice-target choice)]
        [guard (satin-choice-guard choice)]
        [effects (satin-choice-effects choice)])
       (string-append
        (satin-indent indent)
        "(choice " (satin-pretty-string label) " " (symbol->string target)
        (if guard
            (string-append " :when " (satin-pretty-form guard 0))
            "")
        (if (null? effects)
            ""
            (string-append " :effects " (satin-pretty-inline-list effects)))
        ")
")))

;;; ============================================================
;;; Generic Form Pretty-Printing
;;; ============================================================

;;; satin-pretty-form : Form × Indent → String
;;; Pretty-print any s-expression form.
(define (satin-pretty-form form indent)
  (let ([raw (strip-span form)])
       (cond
        [(null? raw) "()"]
        [(symbol? raw) (symbol->string raw)]
        [(string? raw) (satin-pretty-string raw)]
        [(number? raw) (number->string raw)]
        [(boolean? raw) (if raw "#t" "#f")]
        [(and (pair? raw) (eq? (car raw) 'quote) (pair? (cdr raw)))
         (string-append "'" (satin-pretty-form (cadr raw) indent))]
        [(pair? raw)
         (let ([tag (car raw)])
              (cond
               [(satin-node? form)
                (satin-pretty-node form indent)]
               [(satin-choice? form)
                (satin-pretty-choice form indent)]
               [else
                (satin-pretty-list form indent)]))]
        [else (format "~s" raw)])))

;;; Pretty-print a general list.
(define (satin-pretty-list form indent)
  (let* ([raw (strip-span form)]
         [items (map (lambda (x) (satin-pretty-form x 0)) raw)]
         [inline (satin-inline-list items)])
        (if (< (string-length inline) (- *satin-max-line-width* (* indent *satin-indent-width*)))
            (string-append (satin-indent indent) inline "
")
            (string-append
             (satin-indent indent) "(" (satin-pretty-form (car raw) 0) "
"
             (apply string-append
                    (map (lambda (x)
                                 (string-append (satin-indent (+ indent 1))
                                                (satin-pretty-form x (+ indent 1))
                                                "
"))
                         (cdr raw)))
             (satin-indent indent) ")
"))))

;;; Pretty-print a list of fields.
(define (satin-pretty-fields fields indent)
  (apply string-append
         (map (lambda (f) (satin-pretty-form f indent)) fields)))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; Create indentation string.
(define (satin-indent n)
  (make-string (* n *satin-indent-width*) #\space))

;;; Pretty-print a string with proper escaping.
(define (satin-pretty-string s)
  (string-append "\"" (satin-escape-string s) "\""))

;;; Escape special characters in a string.
(define (satin-escape-string s)
  (let ([chars (string->list s)])
       (list->string
        (apply append
               (map (lambda (c)
                            (case c
                                  [(#
ewline) (list #\ #
)]
                                  [(#	ab) (list #\ #	)]
                                  [(#\") (list #\ #\")]
                                  [(#\) (list #\ #\)]
                                  [else (list c)]))
                    chars)))))

;;; Format a list inline.
(define (satin-inline-list items)
  (string-append "(" (string-join items " ") ")"))

;;; Format a list inline (for effects etc).
(define (satin-pretty-inline-list forms)
  (let ([items (map (lambda (x) (satin-pretty-form x 0)) forms)])
       (if (= (length items) 1)
           (car items)
           (satin-inline-list items))))

;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; satin-print : SatinSpec → void
;;; Print a pretty-formatted specification.
(define (satin-print spec)
  (display (satin-pretty spec)))

;;; satin-pretty-issues : (List Issue) → String
;;; Format a list of issues for display.
(define (satin-pretty-issues issues)
  (if (null? issues)
      "No issues found.
"
      (apply string-append
             (map (lambda (i)
                          (string-append (satin-format-issue i) "
"))
                  issues))))
