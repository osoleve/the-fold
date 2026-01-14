;;; playpen/satin/syntax.ss — Satin syntax definitions and parsing
;;;
;;; Defines the core syntax forms of Satin and their structure.
;;; This module recognizes Satin forms and extracts their components.

;;; ====
;;; Form Recognition
;;; ====

;;; Is this a satin-story form?
(define (satin-story? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'satin-story)))

;;; Is this a node form?
(define (satin-node? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'node)))

;;; Is this a choice form?
(define (satin-choice? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'choice)))

;;; Is this a dialogue form?
(define (satin-dialogue? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'dialogue)))

;;; Is this a quest form?
(define (satin-quest? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'quest)))

;;; Is this an exercise form?
(define (satin-exercise? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'exercise)))

;;; Is this a rule form?
(define (satin-rule? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'rule)))

;;; Is this a timeline form?
(define (satin-timeline? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'timeline)))

;;; ====
;;; Field Extraction Utilities
;;; ====

;;; Get a tagged field from a list of forms.
;;; (get-field forms 'title "default") → value or default
(define (satin-get-field forms tag default)
  (let loop ([fs forms])
       (cond
        [(null? fs) default]
        [(not (pair? (car fs))) (loop (cdr fs))]
        [(eq? (car (strip-span (car fs))) tag)
         (if (pair? (cdr (strip-span (car fs))))
             (cadr (strip-span (car fs)))
             default)]
        [else (loop (cdr fs))])))

;;; Get all forms with a specific tag.
(define (satin-get-all-fields forms tag)
  (filter (lambda (f)
                  (and (pair? f)
                       (eq? (car (strip-span f)) tag)))
          forms))

;;; Check if a field exists.
(define (satin-has-field? forms tag)
  (ormap (lambda (f)
                 (and (pair? f)
                      (eq? (car (strip-span f)) tag)))
         forms))

;;; ====
;;; Story Extraction
;;; ====

(define (satin-story-fields spec)
  (cdr (strip-span spec)))

(define (satin-story-id spec)
  (satin-get-field (satin-story-fields spec) 'id 'story))

(define (satin-story-title spec)
  (satin-get-field (satin-story-fields spec) 'title "Untitled"))

(define (satin-story-author spec)
  (satin-get-field (satin-story-fields spec) 'author "Anonymous"))

(define (satin-story-version spec)
  (satin-get-field (satin-story-fields spec) 'version "1.0"))

(define (satin-story-start spec)
  (satin-get-field (satin-story-fields spec) 'start 'start))

(define (satin-story-meta spec)
  (let ([meta-field (satin-get-all-fields (satin-story-fields spec) 'meta)])
       (if (null? meta-field)
           '()
           (cdr (strip-span (car meta-field))))))

(define (satin-story-nodes spec)
  (filter satin-node? (satin-story-fields spec)))

(define (satin-story-dialogues spec)
  (filter satin-dialogue? (satin-story-fields spec)))

(define (satin-story-quests spec)
  (filter satin-quest? (satin-story-fields spec)))

(define (satin-story-exercises spec)
  (filter satin-exercise? (satin-story-fields spec)))

(define (satin-story-rules spec)
  (filter satin-rule? (satin-story-fields spec)))

(define (satin-story-timelines spec)
  (filter satin-timeline? (satin-story-fields spec)))

(define (satin-story-lessons spec)
  (filter satin-lesson? (satin-story-fields spec)))

(define (satin-story-mcqs spec)
  (filter satin-mcq? (satin-story-fields spec)))

(define (satin-story-short-answers spec)
  (filter satin-short-answer? (satin-story-fields spec)))

(define (satin-story-code-tasks spec)
  (filter satin-code-task? (satin-story-fields spec)))

(define (satin-story-masteries spec)
  (filter satin-mastery? (satin-story-fields spec)))

;;; ====
;;; Node Extraction
;;; ====

(define (satin-node-fields node)
  (cddr (strip-span node)))

(define (satin-node-id node)
  (cadr (strip-span node)))

(define (satin-node-title node)
  (satin-get-field (satin-node-fields node) 'title ""))

(define (satin-node-body node)
  (satin-get-field (satin-node-fields node) 'body ""))

(define (satin-node-on-enter node)
  (let ([field (satin-get-all-fields (satin-node-fields node) 'on-enter)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-node-on-exit node)
  (let ([field (satin-get-all-fields (satin-node-fields node) 'on-exit)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-node-choices node)
  (filter satin-choice? (satin-node-fields node)))

(define (satin-node-end? node)
  (satin-has-field? (satin-node-fields node) 'end))

(define (satin-node-exercise node)
  (satin-get-field (satin-node-fields node) 'exercise #f))

;;; ====
;;; Choice Extraction
;;; ====

;;; Choices have syntax:
;;; (choice "label" target [:when guard] [:effects effect ...])

(define (satin-choice-label choice)
  (let ([raw (strip-span choice)])
       (and (pair? (cdr raw))
            (string? (cadr raw))
            (cadr raw))))

(define (satin-choice-target choice)
  (let ([raw (strip-span choice)])
       (and (pair? (cdr raw))
            (pair? (cddr raw))
            (symbol? (caddr raw))
            (caddr raw))))

;;; Extract keyword arguments from choice tail
(define (satin-choice-tail choice)
  (let ([raw (strip-span choice)])
       (if (and (pair? (cdr raw))
                (pair? (cddr raw))
                (pair? (cdddr raw)))
           (cdddr raw)
           '())))

;;; Get keyword value from tail: (:key val ...)
(define (satin-get-keyword tail key)
  (let loop ([xs tail])
       (cond
        [(null? xs) #f]
        [(and (symbol? (car xs))
              (string=? (symbol->string (car xs))
                        (string-append ":" (symbol->string key))))
         (if (pair? (cdr xs))
             (cadr xs)
             #f)]
        [else (loop (cdr xs))])))

;;; Collect all values after a keyword until next keyword
(define (satin-get-keyword-values tail key)
  (let loop ([xs tail] [found? #f] [acc '()])
       (cond
        [(null? xs)
         (reverse acc)]
        [(and (symbol? (car xs))
              (let ([s (symbol->string (car xs))])
                   (and (> (string-length s) 0)
                        (char=? (string-ref s 0) #\:))))
         (if found?
             (reverse acc)
             (if (string=? (symbol->string (car xs))
                           (string-append ":" (symbol->string key)))
                 (loop (cdr xs) #t '())
                 (loop (cdr xs) #f '())))]
        [else
         (if found?
             (loop (cdr xs) #t (cons (car xs) acc))
             (loop (cdr xs) #f acc))])))

(define (satin-choice-guard choice)
  (satin-get-keyword (satin-choice-tail choice) 'when))

(define (satin-choice-effects choice)
  (satin-get-keyword-values (satin-choice-tail choice) 'effects))

;;; ====
;;; Dialogue Extraction
;;; ====

(define (satin-dialogue-fields dialogue)
  (cddr (strip-span dialogue)))

(define (satin-dialogue-id dialogue)
  (cadr (strip-span dialogue)))

(define (satin-dialogue-start dialogue)
  (satin-get-field (satin-dialogue-fields dialogue) 'start 'start))

(define (satin-dialogue-participants dialogue)
  (let ([field (satin-get-all-fields (satin-dialogue-fields dialogue) 'participants)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-dialogue-nodes dialogue)
  (filter satin-node? (satin-dialogue-fields dialogue)))

;;; ====
;;; Quest Extraction
;;; ====

(define (satin-quest-fields quest)
  (cddr (strip-span quest)))

(define (satin-quest-id quest)
  (cadr (strip-span quest)))

(define (satin-quest-title quest)
  (satin-get-field (satin-quest-fields quest) 'title ""))

(define (satin-quest-description quest)
  (satin-get-field (satin-quest-fields quest) 'description ""))

(define (satin-quest-steps quest)
  (let ([field (satin-get-all-fields (satin-quest-fields quest) 'steps)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-quest-on-activate quest)
  (let ([field (satin-get-all-fields (satin-quest-fields quest) 'on-activate)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-quest-on-complete quest)
  (let ([field (satin-get-all-fields (satin-quest-fields quest) 'on-complete)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Exercise Extraction
;;; ====

(define (satin-exercise-fields exercise)
  (cddr (strip-span exercise)))

(define (satin-exercise-id exercise)
  (cadr (strip-span exercise)))

(define (satin-exercise-title exercise)
  (satin-get-field (satin-exercise-fields exercise) 'title ""))

(define (satin-exercise-prompt exercise)
  (satin-get-field (satin-exercise-fields exercise) 'prompt ""))

(define (satin-exercise-hints exercise)
  (let ([field (satin-get-all-fields (satin-exercise-fields exercise) 'hints)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-exercise-rubric exercise)
  (let ([field (satin-get-all-fields (satin-exercise-fields exercise) 'rubric)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-exercise-on-pass exercise)
  (let ([field (satin-get-all-fields (satin-exercise-fields exercise) 'on-pass)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-exercise-on-fail exercise)
  (let ([field (satin-get-all-fields (satin-exercise-fields exercise) 'on-fail)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Lesson Extraction
;;; ====

;;; Is this a lesson form?
(define (satin-lesson? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'lesson)))

(define (satin-lesson-fields lesson)
  (cddr (strip-span lesson)))

(define (satin-lesson-id lesson)
  (cadr (strip-span lesson)))

(define (satin-lesson-title lesson)
  (satin-get-field (satin-lesson-fields lesson) 'title ""))

(define (satin-lesson-objectives lesson)
  (let ([field (satin-get-all-fields (satin-lesson-fields lesson) 'objectives)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-lesson-prerequisites lesson)
  (let ([field (satin-get-all-fields (satin-lesson-fields lesson) 'prerequisites)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-lesson-sequence lesson)
  (let ([field (satin-get-all-fields (satin-lesson-fields lesson) 'sequence)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-lesson-on-complete lesson)
  (let ([field (satin-get-all-fields (satin-lesson-fields lesson) 'on-complete)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; MCQ (Multiple Choice Question) Extraction
;;; ====

;;; Is this an mcq form?
(define (satin-mcq? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'mcq)))

(define (satin-mcq-fields mcq)
  (cddr (strip-span mcq)))

(define (satin-mcq-id mcq)
  (cadr (strip-span mcq)))

(define (satin-mcq-question mcq)
  (satin-get-field (satin-mcq-fields mcq) 'question ""))

(define (satin-mcq-options mcq)
  (let ([field (satin-get-all-fields (satin-mcq-fields mcq) 'options)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-mcq-correct mcq)
  (satin-get-field (satin-mcq-fields mcq) 'correct 0))

(define (satin-mcq-explanation mcq)
  (satin-get-field (satin-mcq-fields mcq) 'explanation ""))

(define (satin-mcq-hints mcq)
  (let ([field (satin-get-all-fields (satin-mcq-fields mcq) 'hints)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Short Answer Extraction
;;; ====

;;; Is this a short-answer form?
(define (satin-short-answer? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'short-answer)))

(define (satin-short-answer-fields sa)
  (cddr (strip-span sa)))

(define (satin-short-answer-id sa)
  (cadr (strip-span sa)))

(define (satin-short-answer-question sa)
  (satin-get-field (satin-short-answer-fields sa) 'question ""))

(define (satin-short-answer-expected sa)
  (satin-get-field (satin-short-answer-fields sa) 'expected ""))

(define (satin-short-answer-rubric sa)
  (let ([field (satin-get-all-fields (satin-short-answer-fields sa) 'rubric)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-short-answer-hints sa)
  (let ([field (satin-get-all-fields (satin-short-answer-fields sa) 'hints)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Code Task Extraction
;;; ====

;;; Is this a code-task form?
(define (satin-code-task? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'code-task)))

(define (satin-code-task-fields ct)
  (cddr (strip-span ct)))

(define (satin-code-task-id ct)
  (cadr (strip-span ct)))

(define (satin-code-task-title ct)
  (satin-get-field (satin-code-task-fields ct) 'title ""))

(define (satin-code-task-description ct)
  (satin-get-field (satin-code-task-fields ct) 'description ""))

(define (satin-code-task-starter ct)
  (satin-get-field (satin-code-task-fields ct) 'starter ""))

(define (satin-code-task-solution ct)
  (satin-get-field (satin-code-task-fields ct) 'solution ""))

(define (satin-code-task-tests ct)
  (let ([field (satin-get-all-fields (satin-code-task-fields ct) 'tests)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-code-task-hints ct)
  (let ([field (satin-get-all-fields (satin-code-task-fields ct) 'hints)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Mastery Extraction
;;; ====

;;; Is this a mastery form?
(define (satin-mastery? x)
  (and (pair? x)
       (eq? (car (strip-span x)) 'mastery)))

(define (satin-mastery-fields mastery)
  (cddr (strip-span mastery)))

(define (satin-mastery-id mastery)
  (cadr (strip-span mastery)))

(define (satin-mastery-skill mastery)
  (satin-get-field (satin-mastery-fields mastery) 'skill ""))

(define (satin-mastery-levels mastery)
  (let ([field (satin-get-all-fields (satin-mastery-fields mastery) 'levels)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

(define (satin-mastery-threshold mastery)
  (satin-get-field (satin-mastery-fields mastery) 'threshold 3))

(define (satin-mastery-exercises mastery)
  (let ([field (satin-get-all-fields (satin-mastery-fields mastery) 'exercises)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))

;;; ====
;;; Rule Extraction
;;; ====

(define (satin-rule-fields rule)
  (cddr (strip-span rule)))

(define (satin-rule-id rule)
  (cadr (strip-span rule)))

(define (satin-rule-when rule)
  (satin-get-field (satin-rule-fields rule) 'when #t))

(define (satin-rule-once? rule)
  (satin-get-field (satin-rule-fields rule) 'once? #f))

(define (satin-rule-effects rule)
  (let ([field (satin-get-all-fields (satin-rule-fields rule) 'effects)])
       (if (null? field)
           '()
           (cdr (strip-span (car field))))))
