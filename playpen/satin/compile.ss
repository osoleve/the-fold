;;; playpen/satin/compile.ss — Satin to Quill compiler
;;;
;;; Compiles Satin specifications to Quill runtime structures.
;;; This is the main compilation pipeline.

;;; ============================================================
;;; Main Compilation Entry Point
;;; ============================================================

;;; satin-compile-impl : SatinSpec → (Values QuillStory Issues)
;;; Main compilation function. Returns story and issues.
(define (satin-compile-impl spec)
  (let ([issues (satin-validate spec)])
       ;; If there are errors, don't attempt compilation
       (if (ormap satin-error? issues)
           (values #f issues)
           (let* ([story (satin-compile-story spec)]
                  [more-issues (quill-validate-story story)])
                 (values story (append issues more-issues))))))

;;; ============================================================
;;; Story Compilation
;;; ============================================================

(define (satin-compile-story spec)
  (let* ([id (satin-story-id spec)]
         [title (satin-story-title spec)]
         [start (satin-story-start spec)]
         [nodes (map satin-compile-node (satin-story-nodes spec))]
         [dialogues (map satin-compile-dialogue-to-meta (satin-story-dialogues spec))]
         [quests (map satin-compile-quest-to-meta (satin-story-quests spec))]
         [rules (map satin-compile-rule-to-meta (satin-story-rules spec))]
         [exercises (map satin-compile-exercise-to-meta (satin-story-exercises spec))]
         [timelines (satin-compile-timelines (satin-story-timelines spec))]
         [author (satin-story-author spec)]
         [version (satin-story-version spec)]
         [base-meta `((author . ,author)
                      (version . ,version)
                      (satin-version . ,*satin-version*))]
         [meta (append
                (if (null? dialogues) '() `((dialogues . ,dialogues)))
                (if (null? quests) '() `((quests . ,quests)))
                (if (null? rules) '() `((rules . ,rules)))
                (if (null? exercises) '() `((exercises . ,exercises)))
                (if (null? timelines) '() `((timeline . ,timelines)))
                base-meta)])
        (make-quill-story id title start nodes meta)))

;;; ============================================================
;;; Node Compilation
;;; ============================================================

(define (satin-compile-node node)
  (let* ([id (satin-node-id node)]
         [title (satin-node-title node)]
         [body (satin-compile-body (satin-node-body node))]
         [choices (map satin-compile-choice (satin-node-choices node))]
         [on-enter-base (satin-compile-effects (satin-node-on-enter node))]
         ;; Automatically mark node as visited
         [on-enter (cons `(set-flag ,(satin-visited-flag id)) on-enter-base)]
         [is-end? (satin-node-end? node)]
         [on-enter-final (if is-end? (append on-enter '(end)) on-enter)]
         [exercise-ref (satin-node-exercise node)]
         [meta (if exercise-ref
                   `((exercise . ,exercise-ref))
                   '())])
        (cons id (make-quill-node id title body choices on-enter-final meta))))

;;; ============================================================
;;; Body Compilation
;;; ============================================================

;;; Compile body text, potentially with template interpolation.
;;; Templates use {var:name}, {flag:name?}, {item:name} syntax.
(define (satin-compile-body body)
  (cond
   [(string? body)
    (if (satin-has-template? body)
        (satin-compile-template body)
        body)]
   [(procedure? body) body]
   [else (format "~a" body)]))

;;; Check if string has template markers
(define (satin-has-template? s)
  (let ([len (string-length s)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref s i) #\{) #t]
             [else (loop (+ i 1))]))))

;;; Compile template string to procedure
(define (satin-compile-template s)
  (lambda (state)
          (satin-expand-template s state)))

;;; Expand template at runtime
(define (satin-expand-template s state)
  (let ([len (string-length s)])
       (let loop ([i 0] [result '()])
            (cond
             [(>= i len)
              (apply string-append (reverse result))]
             [(char=? (string-ref s i) #\{)
              (let ([end (satin-find-close-brace s i)])
                   (if end
                       (let* ([expr (substring s (+ i 1) end)]
                              [value (satin-eval-template-expr expr state)])
                             (loop (+ end 1) (cons value result)))
                       (loop (+ i 1) (cons "{" result))))]
             [else
              (let ([next-brace (satin-find-char s i #\{)])
                   (if next-brace
                       (loop next-brace (cons (substring s i next-brace) result))
                       (loop len (cons (substring s i len) result))))]))))

(define (satin-find-close-brace s start)
  (let ([len (string-length s)])
       (let loop ([i (+ start 1)])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref s i) #\}) i]
             [else (loop (+ i 1))]))))

(define (satin-find-char s start ch)
  (let ([len (string-length s)])
       (let loop ([i start])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref s i) ch) i]
             [else (loop (+ i 1))]))))

;;; Evaluate template expression
(define (satin-eval-template-expr expr state)
  (let ([colon (satin-find-char expr 0 #\:)])
       (if colon
           (let ([type (substring expr 0 colon)]
                 [name (substring expr (+ colon 1) (string-length expr))])
                (cond
                 [(string=? type "var")
                  (let ([val (quill-state-var state (string->symbol name))])
                       (if val (format "~a" val) ""))]
                 [(string=? type "flag")
                  (if (quill-state-flag? state (string->symbol name)) "yes" "no")]
                 [(string=? type "item")
                  (if (quill-state-has-item? state (string->symbol name))
                      name "")]
                 [else (string-append "{" expr "}")]))
           (string-append "{" expr "}"))))

;;; ============================================================
;;; Choice Compilation
;;; ============================================================

(define (satin-compile-choice choice)
  (let* ([label (or (satin-choice-label choice) "Continue")]
         [target (or (satin-choice-target choice) 'missing)]
         [guard-expr (satin-choice-guard choice)]
         [guard (if guard-expr
                    (satin-compile-guard guard-expr)
                    (lambda (_s) #t))]
         [effects (satin-compile-effects (satin-choice-effects choice))]
         [meta `((source . satin))])
        (make-quill-choice label target guard effects meta)))

;;; ============================================================
;;; Dialogue Compilation
;;; ============================================================

(define (satin-compile-dialogue-to-meta dialogue)
  (let* ([id (satin-dialogue-id dialogue)]
         [start (satin-dialogue-start dialogue)]
         [participants (satin-dialogue-participants dialogue)]
         [nodes (map satin-compile-dialogue-node (satin-dialogue-nodes dialogue))]
         [meta `((start . ,start)
                 (participants . ,participants))])
        (make-quill-dialogue id nodes meta)))

(define (satin-compile-dialogue-node node)
  (let* ([id (satin-node-id node)]
         [speaker (satin-get-field (satin-node-fields node) 'speaker 'narrator)]
         [text (satin-get-field (satin-node-fields node) 'text "")]
         [choices (map satin-compile-dialogue-choice (satin-node-choices node))]
         [on-enter (satin-compile-effects (satin-node-on-enter node))]
         [meta `((speaker . ,speaker))])
        (cons id (make-quill-dialogue-node id speaker text choices on-enter meta))))

(define (satin-compile-dialogue-choice choice)
  (let* ([label (or (satin-choice-label choice) "Continue")]
         [target (or (satin-choice-target choice) 'end)]
         [guard-expr (satin-choice-guard choice)]
         [guard (if guard-expr (satin-compile-guard guard-expr) #f)]
         [effects (satin-compile-effects (satin-choice-effects choice))]
         [meta '()])
        (make-quill-dialogue-choice label target guard effects meta)))

;;; ============================================================
;;; Quest Compilation
;;; ============================================================

(define (satin-compile-quest-to-meta quest)
  (let* ([id (satin-quest-id quest)]
         [title (satin-quest-title quest)]
         [steps (map satin-compile-quest-step (satin-quest-steps quest))]
         [on-complete (satin-compile-effects (satin-quest-on-complete quest))]
         [meta '()])
        (make-quill-quest id title steps on-complete meta)))

(define (satin-compile-quest-step step)
  (let ([raw (strip-span step)])
       (if (and (pair? raw) (eq? (car raw) 'step))
           (let* ([id (cadr raw)]
                  [desc (if (pair? (cddr raw)) (caddr raw) "")]
                  [guard-expr (if (pair? (cdddr raw)) (cadddr raw) #t)]
                  [guard (satin-compile-guard guard-expr)]
                  [meta '()])
                 (make-quill-quest-step id desc guard meta))
           (make-quill-quest-step 'unknown "" (lambda (_) #f) '()))))

;;; ============================================================
;;; Rule Compilation
;;; ============================================================

(define (satin-compile-rule-to-meta rule)
  (let* ([id (satin-rule-id rule)]
         [guard (satin-compile-guard (satin-rule-when rule))]
         [effects (satin-compile-effects (satin-rule-effects rule))]
         [once? (satin-rule-once? rule)]
         [meta '()])
        (make-quill-rule id guard effects once? meta)))

;;; ============================================================
;;; Exercise Compilation
;;; ============================================================

(define (satin-compile-exercise-to-meta exercise)
  (let* ([id (satin-exercise-id exercise)]
         [title (satin-exercise-title exercise)]
         [prompt (satin-exercise-prompt exercise)]
         [checks (map satin-compile-check (satin-exercise-rubric exercise))]
         [on-pass (satin-compile-effects (satin-exercise-on-pass exercise))]
         [hints (satin-exercise-hints exercise)]
         [meta `((hints . ,hints))])
        (make-quill-exercise id title prompt checks on-pass meta)))

(define (satin-compile-check check)
  (let ([raw (strip-span check)])
       (if (and (pair? raw) (eq? (car raw) 'check))
           (let* ([id (cadr raw)]
                  [desc (caddr raw)]
                  [pred-expr (cadddr raw)]
                  [pred (satin-compile-check-predicate pred-expr)]
                  [meta '()])
                 (make-quill-check id desc pred meta))
           (make-quill-check 'unknown "" (lambda (_s _r _a) #f) '()))))

;;; Compile check predicate
(define (satin-compile-check-predicate expr)
  (cond
   [(procedure? expr) expr]
   [(not (pair? expr))
    (lambda (_s _r _a) (if expr #t #f))]
   [else
    (let ([tag (car (strip-span expr))]
          [args (cdr (strip-span expr))])
         (case tag
               [(answer-eq?)
                (let ([expected (car args)])
                     (lambda (_s _r a) (equal? a expected)))]
               
               [(answer-contains?)
                (let ([substr (car args)])
                     (lambda (_s _r a)
                             (and (string? a)
                                  (string-contains? a substr))))]
               
               [(answer-matches?)
                ;; For now, just do substring match (no regex)
                (let ([pattern (car args)])
                     (lambda (_s _r a)
                             (and (string? a)
                                  (string-contains? a pattern))))]
               
               [(answer-length>=)
                (let ([n (car args)])
                     (lambda (_s _r a)
                             (and (string? a)
                                  (>= (string-length a) n))))]
               
               [(and)
                (let ([preds (map satin-compile-check-predicate args)])
                     (lambda (s r a)
                             (andmap (lambda (p) (p s r a)) preds)))]
               
               [(or)
                (let ([preds (map satin-compile-check-predicate args)])
                     (lambda (s r a)
                             (ormap (lambda (p) (p s r a)) preds)))]
               
               [(not)
                (let ([pred (satin-compile-check-predicate (car args))])
                     (lambda (s r a)
                             (not (pred s r a))))]
               
               [(custom)
                (car args)]
               
               [else
                (lambda (_s _r _a) #t)]))]))

;;; Helper: check if string contains substring
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
;;; Timeline Compilation
;;; ============================================================

(define (satin-compile-timelines timelines)
  (apply append (map satin-compile-timeline-events timelines)))

(define (satin-compile-timeline-events timeline)
  (let ([raw (strip-span timeline)])
       (if (and (pair? raw) (eq? (car raw) 'timeline))
           (filter-map satin-compile-timeline-event (cdr raw))
           '())))

(define (satin-compile-timeline-event event)
  (let ([raw (strip-span event)])
       (if (and (pair? raw) (eq? (car raw) 'event))
           (let* ([id (cadr raw)]
                  [time (satin-get-field (cddr raw) 'at 0)]
                  [effects (satin-compile-effects
                            (let ([field (satin-get-all-fields (cddr raw) 'effects)])
                                 (if (null? field) '() (cdr (strip-span (car field))))))]
                  [once? (satin-get-field (cddr raw) 'once? #t)]
                  [meta '()])
                 (make-quill-timeline-event id time effects once? meta))
           #f)))

;;; Helper for filter-map
(define (filter-map f xs)
  (let loop ([xs xs] [acc '()])
       (cond
        [(null? xs) (reverse acc)]
        [else
         (let ([result (f (car xs))])
              (if result
                  (loop (cdr xs) (cons result acc))
                  (loop (cdr xs) acc)))])))
