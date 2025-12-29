;;; playpen/quill/validate.ss — Quill story validation
;;;
;;; Quill stories are data. Validation is how we keep authoring ergonomic:
;;; catch missing nodes, broken links, and malformed effects early with
;;; source-mappable issue objects.
;;;
;;; API:
;;;   (quill-validate-story story) -> (List Issue)
;;;   (quill-valid? story)         -> Bool
;;;   (quill-assert-valid! story)  -> void | error
;;;
;;; Issue = ((severity . error|warn) (code . Symbol) (message . String) (data . Alist))

(define (quill-issue severity code message data)
  `((severity . ,severity)
    (code . ,code)
    (message . ,message)
    (data . ,data)))

(define (quill-issue-severity issue) (cdr (assq 'severity issue)))
(define (quill-issue-message issue) (cdr (assq 'message issue)))

(define (quill-error? issue) (eq? (quill-issue-severity issue) 'error))

(define (quill-effect-form-ok? eff)
  (cond
    [(eq? eff 'end) #t]
    [(not (pair? eff)) #f]
    [else
     (case (car eff)
       [(end) (null? (cdr eff))]
       [(set inc) (and (pair? (cdr eff)) (pair? (cddr eff)) (null? (cdddr eff)))]
       [(flag unflag add-item remove-item goto message) (and (pair? (cdr eff)) (null? (cddr eff)))]
       [else #f])]))

(define (quill-validate-effects where effects)
  (let loop ([es effects] [acc '()])
    (cond
      [(null? es) (reverse acc)]
      [else
       (let ([e (car es)])
         (loop (cdr es)
               (if (quill-effect-form-ok? e)
                   acc
                   (cons (quill-issue
                           'error
                           'invalid-effect
                           (format "Invalid effect form: ~a" e)
                           `((where . ,where) (effect . ,e)))
                         acc))))])))

(define (quill-validate-choice story node-id choice idx)
  (let* ([label (quill-choice-label choice)]
         [target (quill-choice-target choice)]
         [guard (quill-choice-guard choice)]
         [effects (quill-choice-effects choice)]
         [where `((node . ,node-id) (choice-index . ,idx) (label . ,label))])
    (append
      (if (and target (quill-story-has-node? story target))
          '()
          (list (quill-issue
                  'error
                  'missing-target
                  (format "Choice target node missing: ~a" target)
                  (append where `((target . ,target))))))
      (cond
        [(or (not guard) (eq? guard #t) (eq? guard #f) (procedure? guard)) '()]
        [else
         (list (quill-issue
                 'warn
                 'nonstandard-guard
                 (format "Choice guard is not a boolean or procedure: ~a" guard)
                 (append where `((guard . ,guard)))) )])
      (quill-validate-effects (append where '((where . choice-effects))) effects))))

(define (quill-validate-node story node-id node)
  (let* ([choices (or (quill-node-choices node) '())]
         [on-enter (or (quill-node-on-enter node) '())]
         [choice-issues
          (let loop ([cs choices] [i 1] [acc '()])
            (if (null? cs)
                (reverse acc)
                (loop (cdr cs)
                      (+ i 1)
                      (append (quill-validate-choice story node-id (car cs) i) acc))))])
    (append
      (quill-validate-effects `((node . ,node-id) (where . on-enter)) on-enter)
      choice-issues)))

(define (quill-reachable-nodes story)
  (let ([start (quill-story-start-node story)]
        [nodes (quill-story-nodes story)])
    (let loop ([queue (list start)] [seen '()])
      (cond
        [(null? queue) seen]
        [else
         (let* ([n (car queue)]
                [rest (cdr queue)])
           (if (memq n seen)
               (loop rest seen)
               (let ([node (quill-story-node story n)])
                 (if (not node)
                     (loop rest (cons n seen))
                     (let* ([choices (or (quill-node-choices node) '())]
                            [targets (map quill-choice-target choices)]
                            [next (append targets rest)])
                       (loop next (cons n seen)))))))]))))

(define (quill-validate-story story)
  (let* ([issues '()]
         [sid (quill-story-id story)]
         [start (quill-story-start-node story)]
         [nodes (quill-story-nodes story)])
    (when (not (symbol? sid))
      (set! issues
            (cons (quill-issue 'error 'invalid-story-id
                               (format "Story id must be a symbol: ~a" sid)
                               `((id . ,sid)))
                  issues)))
    (when (not (quill-story-has-node? story start))
      (set! issues
            (cons (quill-issue 'error 'missing-start-node
                               (format "Start node missing: ~a" start)
                               `((start . ,start)))
                  issues)))
    ;; Validate nodes + edges
    (for-each
      (lambda (pair)
        (let ([node-id (car pair)]
              [node (cdr pair)])
          (set! issues (append (quill-validate-node story node-id node) issues))))
      nodes)
    ;; Unreachable nodes are warnings (useful in education authoring)
    (let* ([reachable (quill-reachable-nodes story)]
           [unreachable
            (filter (lambda (p) (not (memq (car p) reachable))) nodes)])
      (when (not (null? unreachable))
        (set! issues
              (cons (quill-issue 'warn 'unreachable-nodes
                                 (format "Unreachable nodes: ~a" (map car unreachable))
                                 `((nodes . ,(map car unreachable))))
                    issues))))
    (reverse issues)))

(define (quill-valid? story)
  (let ([issues (quill-validate-story story)])
    (not (ormap quill-error? issues))))

(define (quill-assert-valid! story)
  (let ([issues (quill-validate-story story)])
    (let ([errs (filter quill-error? issues)])
      (when (not (null? errs))
        (error 'quill-assert-valid!
               (string-append
                 "Story validation failed:\n"
                 (apply string-append
                        (map (lambda (i) (string-append "  - " (quill-issue-message i) "\n"))
                             errs))))))))

