;;; playpen/quill/dsl.ss — Quill content DSL (S-expressions)
;;;
;;; This module provides a small authoring DSL that compiles into Quill runtime
;;; structures. It is intentionally conservative: data-first, deterministic,
;;; and easily validated.
;;;
;;; Entry points:
;;;   (quill-compile spec)                 -> (values story issues)
;;;   (quill-compile! spec)                -> story | error
;;;   (quill-dsl-example)                  -> example spec
;;;
;;; DSL (minimal):
;;;   (story
;;;     (id <symbol>)
;;;     (title <string>)
;;;     (start <symbol>)
;;;     (meta (<k> <v>) ...)
;;;     (node <symbol>
;;;       (title <string>)
;;;       (body <string>|<procedure>)
;;;       (on-enter <effect> ...)
;;;       (choice <label-string> <target-symbol> [<guard-expr>] [<effects...>|(do <effect> ...)]))
;;;     ...)
;;;
;;; Guard expressions compile to (lambda (state) -> bool):
;;;   #t | #f
;;;   (flag? <sym>)
;;;   (has-item? <sym>)
;;;   (var <sym>)                ; numeric (or #f)
;;;   (var= <sym> <value>)
;;;   (var>= <sym> <number>)     ; numeric compare (missing => 0)
;;;   (not <expr>)
;;;   (and <expr> ...)
;;;   (or  <expr> ...)
;;;
;;; Effects are the runtime effect forms (see playpen/quill/runtime.ss).

;;; ====
;;; Small utilities
;;; ====

(define (quill-alist-get alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))

(define (quill-symbol? x) (symbol? x))
(define (quill-string? x) (string? x))

(define (quill-list-of? pred xs)
  (and (list? xs) (andmap pred xs)))

(define (quill-pair-list? xs)
  (and (list? xs)
       (andmap (lambda (p) (and (pair? p) (pair? (cdr p)) (null? (cddr p)))) xs)))

;;; ====
;;; Guard compilation
;;; ====

(define (quill-guard-form? expr)
  (cond
   ((boolean? expr) #t)
   ((procedure? expr) #t)
   ((and (pair? expr) (symbol? (car expr)))
    (memq (car expr) '(flag? has-item? var var= var>= not and or)))
   (else #f)))

(define (quill-guard->proc expr)
  (cond
   ((eq? expr #t) (lambda (_s) #t))
   ((eq? expr #f) (lambda (_s) #f))
   ((procedure? expr) expr)
   ((and (pair? expr) (symbol? (car expr)))
    (case (car expr)
          ((flag?)
           (let ([flag (cadr expr)])
                (lambda (s) (and (quill-state-flag? s flag) #t))))
          ((has-item?)
           (let ([item (cadr expr)])
                (lambda (s) (and (quill-state-has-item? s item) #t))))
          ((var)
           (let ([k (cadr expr)])
                (lambda (s) (quill-state-var s k))))
          ((var=)
           (let ([k (cadr expr)] [v (caddr expr)])
                (lambda (s) (equal? (quill-state-var s k) v))))
          ((var>=)
           (let ([k (cadr expr)] [n (caddr expr)])
                (lambda (s) (>= (or (quill-state-var s k) 0) n))))
          ((not)
           (let ([p (quill-guard->proc (cadr expr))])
                (lambda (s) (not (p s)))))
          ((and)
           (let ([ps (map quill-guard->proc (cdr expr))])
                (lambda (s) (andmap (lambda (p) (p s)) ps))))
          ((or)
           (let ([ps (map quill-guard->proc (cdr expr))])
                (lambda (s) (ormap (lambda (p) (p s)) ps))))
          (else
           (lambda (_s) #t))))
   (else (lambda (_s) #t))))

;;; ====
;;; Effect parsing helpers
;;; ====

(define (quill-normalize-effects xs)
  (cond
   ((null? xs) '())
   ((and (pair? xs) (pair? (car xs)) (eq? (caar xs) 'do))
    (cdar xs))
   (else xs)))

;;; ====
;;; Node parsing
;;; ====

(define (quill-compile-choice node-id form)
  ;; (choice label target [guard] [effects...])
  (let* ([label (if (and (pair? (cdr form)) (string? (cadr form))) (cadr form) #f)]
         [target (if (and (pair? (cddr form)) (symbol? (caddr form))) (caddr form) #f)]
         [rest (cdddr form)]
         [guard-expr (if (and (pair? rest) (quill-guard-form? (car rest)))
                         (car rest)
                         #t)]
         [effects-raw (if (eq? guard-expr #t) rest (cdr rest))]
         [effects (quill-normalize-effects effects-raw)]
         [guard (quill-guard->proc guard-expr)]
         [meta `((node . ,node-id))])
        (make-quill-choice (or label (format "~a" form))
                           (or target 'missing-target)
                           guard
                           effects
                           meta)))

(define (quill-compile-node node-id forms)
  ;; forms are the tail of (node node-id ...)
  (let loop ([fs forms]
             [title ""]
             [body ""]
             [on-enter '()]
             [choices '()]
             [meta '()])
       (cond
        ((null? fs)
         (make-quill-node node-id title body (reverse choices) (reverse on-enter) (reverse meta)))
        ((not (pair? (car fs)))
         (loop (cdr fs) title body on-enter choices (cons (cons 'unknown (car fs)) meta)))
        (else
         (let* ([f (car fs)]
                [tag (car f)])
               (case tag
                     ((title)
                      (loop (cdr fs)
                            (if (and (pair? (cdr f)) (string? (cadr f))) (cadr f) title)
                            body
                            on-enter
                            choices
                            meta))
                     ((body)
                      (loop (cdr fs)
                            title
                            (if (pair? (cdr f)) (cadr f) body)
                            on-enter
                            choices
                            meta))
                     ((meta)
                      (loop (cdr fs)
                            title
                            body
                            on-enter
                            choices
                            (append (if (quill-pair-list? (cdr f))
                                        (cdr f)
                                        (list (cons 'meta f)))
                                    meta)))
                     ((on-enter)
                      (loop (cdr fs)
                            title
                            body
                            (append (quill-normalize-effects (cdr f)) on-enter)
                            choices
                            meta))
                     ((choice)
                      (loop (cdr fs)
                            title
                            body
                            on-enter
                            (cons (quill-compile-choice node-id f) choices)
                            meta))
                     ((end)
                      (loop (cdr fs) title body (cons 'end on-enter) choices meta))
                     (else
                      (loop (cdr fs)
                            title
                            body
                            on-enter
                            choices
                            (cons (cons 'unknown f) meta)))))))))

;;; ====
;;; Story parsing
;;; ====

(define (quill-compile-story spec)
  (let loop ([fs (cdr spec)]
             [id 'story]
             [title ""]
             [start 'start]
             [nodes '()]
             [meta '()])
       (cond
        ((null? fs)
         (make-quill-story id title start (reverse nodes) (reverse meta)))
        ((not (pair? (car fs)))
         (loop (cdr fs) id title start nodes (cons (cons 'unknown (car fs)) meta)))
        (else
         (let* ([f (car fs)]
                [tag (car f)])
               (case tag
                     ((id) (loop (cdr fs) (if (symbol? (cadr f)) (cadr f) id) title start nodes meta))
                     ((title) (loop (cdr fs) id (if (string? (cadr f)) (cadr f) title) start nodes meta))
                     ((start) (loop (cdr fs) id title (if (symbol? (cadr f)) (cadr f) start) nodes meta))
                     ((meta)
                      (loop (cdr fs) id title start nodes
                            (append (if (quill-pair-list? (cdr f)) (cdr f) (list (cons 'meta f))) meta)))
                     ((node)
                      (let* ([node-id (cadr f)]
                             [node-forms (cddr f)]
                             [node (quill-compile-node node-id node-forms)])
                            (loop (cdr fs) id title start (cons (cons node-id node) nodes) meta)))
                     (else
                      (loop (cdr fs) id title start nodes (cons (cons 'unknown f) meta)))))))))

;;; ====
;;; Public API
;;; ====

(define (quill-dsl-validate spec)
  (let ((issues '()))
       (define (err code msg data)
         (set! issues (cons (quill-issue 'error code msg data) issues)))
       (define (warn code msg data)
         (set! issues (cons (quill-issue 'warn code msg data) issues)))
       
       (define (validate-node nid node-forms)
         (let node-loop ((nfs node-forms))
              (when (pair? nfs)
                    (let ((nf (car nfs)))
                         (cond
                          ((and (pair? nf) (eq? (car nf) 'choice))
                           (let ((ok (and (pair? (cdr nf)) (string? (cadr nf))
                                          (pair? (cddr nf)) (symbol? (caddr nf)))))
                                (unless ok
                                        (err 'invalid-choice
                                             (format "Choice must be (choice <string> <symbol> ...): ~a" nf)
                                             `((node . ,nid) (form . ,nf))))))
                          ((and (pair? nf) (eq? (car nf) 'on-enter))
                           (let ((effects (quill-normalize-effects (cdr nf))))
                                (for-each
                                 (lambda (e)
                                         (unless (quill-effect-form-ok? e)
                                                 (err 'invalid-effect
                                                      (format "Invalid effect form: ~a" e)
                                                      `((node . ,nid) (form . ,e)))))
                                 effects)))
                          (else (void)))
                         (node-loop (cdr nfs))))))
       
       (define (duplicates xs)
         (let loop ((ys xs) (seen '()) (acc '()))
              (cond
               ((null? ys) (reverse acc))
               ((memq (car ys) seen) (loop (cdr ys) seen (cons (car ys) acc)))
               (else (loop (cdr ys) (cons (car ys) seen) acc)))))
       
       (if (not (and (pair? spec) (eq? (car spec) 'story)))
           (begin
            (err 'invalid-dsl "Expected (story ...)" `((spec . ,spec)))
            (reverse issues))
           (let ((fields (cdr spec)))
                (let loop ((fs fields) (seen-id? #f) (seen-start? #f) (node-ids '()))
                     (cond
                      ((null? fs)
                       (unless seen-id? (err 'missing-id "Story missing (id ...)" '()))
                       (unless seen-start? (err 'missing-start "Story missing (start ...)" '()))
                       (let ((dups (duplicates node-ids)))
                            (when (not (null? dups))
                                  (err 'duplicate-nodes (format "Duplicate node ids: ~a" dups) `((nodes . ,dups)))))
                       (reverse issues))
                      (else
                       (let ((f (car fs)))
                            (cond
                             ((not (pair? f))
                              (warn 'unknown-story-form
                                    (format "Ignoring non-list story form: ~a" f)
                                    `((form . ,f)))
                              (loop (cdr fs) seen-id? seen-start? node-ids))
                             ((eq? (car f) 'id)
                              (unless (and (pair? (cdr f)) (symbol? (cadr f)))
                                      (err 'invalid-id (format "Invalid (id ...): ~a" f) `((form . ,f))))
                              (when seen-id? (warn 'duplicate-id "Duplicate (id ...) field" `((form . ,f))))
                              (loop (cdr fs) #t seen-start? node-ids))
                             ((eq? (car f) 'start)
                              (unless (and (pair? (cdr f)) (symbol? (cadr f)))
                                      (err 'invalid-start (format "Invalid (start ...): ~a" f) `((form . ,f))))
                              (when seen-start? (warn 'duplicate-start "Duplicate (start ...) field" `((form . ,f))))
                              (loop (cdr fs) seen-id? #t node-ids))
                             ((or (eq? (car f) 'title) (eq? (car f) 'meta))
                              (loop (cdr fs) seen-id? seen-start? node-ids))
                             ((eq? (car f) 'node)
                              (let ((nid (and (pair? (cdr f)) (cadr f))))
                                   (unless (symbol? nid)
                                           (err 'invalid-node-id (format "Node id must be a symbol: ~a" nid) `((form . ,f))))
                                   (when (symbol? nid)
                                         (validate-node nid (cddr f)))
                                   (loop (cdr fs) seen-id? seen-start?
                                         (if (symbol? nid) (cons nid node-ids) node-ids))))
                             (else
                              (warn 'unknown-story-form
                                    (format "Unknown story field: ~a" (car f))
                                    `((form . ,f)))
                              (loop (cdr fs) seen-id? seen-start? node-ids))))))))))
  )

(define (quill-compile spec)
  (let* ([dsl-issues (quill-dsl-validate spec)]
         [story (if (ormap quill-error? dsl-issues) #f (quill-compile-story spec))]
         [more (if story (quill-validate-story story) '())]
         [all (append dsl-issues more)])
        (values story all)))

(define (quill-compile! spec)
  (let-values ([(story issues) (quill-compile spec)])
              (if (and story (not (ormap quill-error? issues)))
                  story
                  (error 'quill-compile!
                         (string-append
                          "DSL compilation failed:\n"
                          (apply string-append
                                 (map (lambda (i) (string-append "  - " (quill-issue-message i) "\n"))
                                      (filter quill-error? issues))))))))

(define (quill-dsl-example)
  '(story
    (id demo)
    (title "Quill DSL Example")
    (start start)
    (node start
          (title "Start")
          (body "You are in a white room.")
          (choice "Leave" end))
    (node end
          (title "Outside")
          (body "You step outside.")
          (end))))
