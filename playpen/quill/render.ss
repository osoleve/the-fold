;;; thimble/quill/render.ss — Quill rendering
;;;
;;; Rendering is pure: given a Story + Run, produce a display string.
;;; Advanced formatting (wrapping, styles, transcript views) comes later.

(define (quill-node-body->string body state)
  (cond
    ((string? body) body)
    ((procedure? body) (body state))
    (else (format "~a" body))))

(define (quill-choice-visible? choice state)
  (let ((g (quill-choice-guard choice)))
    (cond
      ((not g) #t)
      ((eq? g #t) #t)
      ((procedure? g) (and (g state) #t))
      (else #t))))

(define (quill-visible-choices node state)
  (filter (lambda (c) (quill-choice-visible? c state))
          (quill-node-choices node)))

(define (quill-choices->string choices)
  (let loop ((cs choices) (i 1) (acc ""))
    (if (null? cs)
        acc
        (loop (cdr cs)
              (+ i 1)
              (string-append acc
                             "  "
                             (number->string i)
                             ". "
                             (quill-choice-label (car cs))
                             "\n")))))

(define (quill-render story run)
  (let* ((node (quill-story-node story (quill-run-node-id run)))
         (state (quill-run-state run))
         (msg (quill-run-message run)))
    (cond
      ((not node)
       (string-append "Quill error: missing node "
                      (format "~a" (quill-run-node-id run))
                      "\n"))
      (else
       (let* ((title (quill-node-title node))
              (body (quill-node-body->string (quill-node-body node) state))
              (choices (quill-visible-choices node state))
              (choices-text (if (null? choices) "" (quill-choices->string choices))))
         (string-append
           (if (and msg (not (string-blank? msg)))
               (string-append msg "\n\n")
               "")
           (if (and title (not (string-blank? title)))
               (string-append title "\n" (make-string (string-length title) #\-) "\n\n")
               "")
           body
           "\n\n"
           choices-text
	           (cond
	             ((quill-run-done? run) "\n[The End]\n")
	             ((null? choices) "\n(Type 'quit' to exit)\n")
	             (else "\n(Choose a number, or 'help')\n"))))))))
