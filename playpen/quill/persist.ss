;;; playpen/quill/persist.ss — Quill persistence helpers
;;;
;;; Save/load snapshots are pure data. Checkpoints and undo history
;;; are stored in state meta so callers can opt in.

(define *quill-snapshot-version* 1)

;;; ------------------------------------------------------------
;;; Small utilities
;;; ------------------------------------------------------------

(define (quill-alist-ref alist key default)
  (let ([p (assq key alist)])
       (if p (cdr p) default)))

(define (quill-alist-remove alist key)
  (let ([p (assq key alist)])
       (if p (remq p alist) alist)))

(define (quill-alist-remove-keys alist keys)
  (if (null? keys)
      alist
      (quill-alist-remove-keys (quill-alist-remove alist (car keys)) (cdr keys))))

(define (quill-state-strip-meta-keys state keys)
  (let* ([meta (quill-state-meta state)]
         [meta2 (quill-alist-remove-keys meta keys)])
        (quill-state-set-section state 'meta meta2)))

;;; ------------------------------------------------------------
;;; Snapshots
;;; ------------------------------------------------------------

(define (quill-make-snapshot story-id node-id state done? message transcript)
  `((version . ,*quill-snapshot-version*)
    (story-id . ,story-id)
    (node-id . ,node-id)
    (state . ,state)
    (done? . ,done?)
    (message . ,message)
    (transcript . ,transcript)))

(define (quill-run->snapshot run . options-opt)
  (let* ([options (if (null? options-opt) '() (car options-opt))]
         [include-transcript? (quill-alist-ref options 'include-transcript? #t)]
         [include-message? (quill-alist-ref options 'include-message? #t)]
         [strip-meta (quill-alist-ref options 'strip-meta '(history checkpoints))]
         [state0 (quill-run-state run)]
         [state (quill-state-strip-meta-keys state0 strip-meta)]
         [message (if include-message? (quill-run-message run) #f)]
         [transcript (if include-transcript? (quill-run-transcript run) '())])
        (quill-make-snapshot
         (quill-run-story-id run)
         (quill-run-node-id run)
         state
         (quill-run-done? run)
         message
         transcript)))

(define (quill-snapshot->run snapshot)
  (let* ([story-id (quill-alist-ref snapshot 'story-id #f)]
         [node-id (quill-alist-ref snapshot 'node-id #f)]
         [state (quill-alist-ref snapshot 'state (make-quill-state))]
         [done? (quill-alist-ref snapshot 'done? #f)]
         [message (quill-alist-ref snapshot 'message #f)]
         [transcript (quill-alist-ref snapshot 'transcript '())])
        (if (and story-id node-id)
            (make-quill-run story-id node-id state transcript done? message)
            #f)))

(define (quill-snapshot-valid? story snapshot)
  (let ([story-id (quill-alist-ref snapshot 'story-id #f)])
       (and story-id (eq? story-id (quill-story-id story)))))

(define (quill-load-run story snapshot)
  (let ([run (quill-snapshot->run snapshot)])
       (if (and run (quill-snapshot-valid? story snapshot))
           run
           #f)))

;;; ------------------------------------------------------------
;;; Checkpoints and undo history
;;; ------------------------------------------------------------

(define (quill-run-history run)
  (quill-state-meta-get (quill-run-state run) 'history '()))

(define (quill-run-record-history run . options-opt)
  (let* ([options (if (null? options-opt) '() (car options-opt))]
         [snapshot (quill-run->snapshot run options)]
         [history (quill-run-history run)]
         [state1 (quill-state-meta-set (quill-run-state run) 'history (cons snapshot history))])
        (quill-run-with-state run state1)))

(define (quill-run-undo run)
  (let ([history (quill-run-history run)])
       (if (null? history)
           (values run #f)
           (let* ([snapshot (car history)]
                  [rest (cdr history)]
                  [run2 (quill-snapshot->run snapshot)])
                 (if (not run2)
                     (values run #f)
                     (let* ([state2 (quill-state-meta-set (quill-run-state run2) 'history rest)]
                            [state3 (quill-state-meta-set state2 'checkpoints (quill-run-checkpoints run))])
                           (values (quill-run-with-state run2 state3) #t)))))))

(define (quill-run-checkpoints run)
  (quill-state-meta-get (quill-run-state run) 'checkpoints '()))

(define (quill-default-checkpoint-label run)
  (string->symbol (string-append "turn-" (number->string (length (quill-run-transcript run))))))

(define (quill-run-checkpoint run . args)
  (let* ([label (if (null? args) (quill-default-checkpoint-label run) (car args))]
         [options (if (and (pair? args) (pair? (cdr args))) (cadr args) '())]
         [snapshot (quill-run->snapshot run options)]
         [entry (cons label snapshot)]
         [checkpoints (quill-run-checkpoints run)]
         [state1 (quill-state-meta-set (quill-run-state run) 'checkpoints (cons entry checkpoints))])
        (quill-run-with-state run state1)))

(define (quill-find-checkpoint checkpoints label)
  (cond
   [(null? checkpoints) #f]
   [(equal? (caar checkpoints) label) (car checkpoints)]
   [else (quill-find-checkpoint (cdr checkpoints) label)]))

(define (quill-run-restore-checkpoint run label)
  (let* ([checkpoints (quill-run-checkpoints run)]
         [entry (quill-find-checkpoint checkpoints label)])
        (if (not entry)
            #f
            (let* ([snapshot (cdr entry)]
                   [run2 (quill-snapshot->run snapshot)])
                  (if (not run2)
                      #f
                      (let* ([state1 (quill-state-meta-set (quill-run-state run2) 'checkpoints checkpoints)]
                             [state2 (quill-state-meta-set state1 'history (quill-run-history run))])
                            (quill-run-with-state run2 state2)))))))

;;; ------------------------------------------------------------
;;; Convenience wrappers
;;; ------------------------------------------------------------

(define (quill-step-with-history story run input . options-opt)
  (let* ([options (if (null? options-opt) '() (car options-opt))]
         [run1 (quill-run-record-history run options)])
        (quill-step story run1 input)))
