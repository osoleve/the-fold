;;; playpen/quill/repl.ss — Quill REPL helpers
;;;
;;; Minimal interactive runner. Advanced tooling (inspectors, debugging)
;;; lands in a dedicated tooling file later.

(define (quill-repl-message run msg)
  (quill-run-with-message run msg))

(define (quill-repl-parse-command line)
  (let* ([trimmed (string-trim line)]
         [body (if (and (> (string-length trimmed) 0)
                        (char=? (string-ref trimmed 0) #\:))
                   (string-trim (substring trimmed 1 (string-length trimmed)))
                   trimmed)]
         [parts (if (string-blank? body) '() (string-split body " "))]
         [cmd (if (null? parts) "" (car parts))]
         [args (if (null? parts) '() (cdr parts))]
         [rest (if (null? args) "" (string-trim (string-join args " ")))])
        (list cmd args rest)))

(define (quill-repl-help)
  (string-append
   "REPL commands:\n"
   "  :help                      show this help\n"
   "  :undo                      undo last step\n"
   "  :checkpoint [label]        create checkpoint\n"
   "  :restore <label>           restore checkpoint\n"
   "  :dialogue <id>             start dialogue\n"
   "  :transcript                show transcript\n"
   "  :snapshot                  print snapshot data\n"
   "  :exercise <id>             select exercise\n"
   "  :answer <text>             grade current exercise\n"))

(define (quill-repl-current-exercise run)
  (quill-state-meta-get (quill-run-state run) 'current-exercise #f))

(define (quill-repl-set-current-exercise run exercise-id)
  (let ([state1 (quill-state-meta-set (quill-run-state run) 'current-exercise exercise-id)])
       (quill-run-with-state run state1)))

(define (quill-repl-handle-command story run line)
  (let* ([parsed (quill-repl-parse-command line)]
         [cmd (car parsed)]
         [args (cadr parsed)]
         [rest (caddr parsed)])
        (cond
         [(or (string=? cmd "") (string=? cmd "help"))
          (values (quill-repl-message run (quill-repl-help)) #t)]
         [(string=? cmd "undo")
          (let-values ([(r2 ok?) (quill-run-undo run)])
                      (values (quill-repl-message r2 (if ok? "Undone." "Nothing to undo.")) #t))]
         [(string=? cmd "checkpoint")
          (let* ([label (and (pair? args) (string->symbol (car args)))]
                 [r2 (if label (quill-run-checkpoint run label) (quill-run-checkpoint run))]
                 [name (if label (symbol->string label) "checkpoint")])
                (values (quill-repl-message r2 (string-append "Saved " name ".")) #t))]
         [(string=? cmd "restore")
          (if (null? args)
              (values (quill-repl-message run "Usage: :restore <label>") #t)
              (let* ([label (string->symbol (car args))]
                     [r2 (quill-run-restore-checkpoint run label)])
                    (if r2
                        (values (quill-repl-message r2 (string-append "Restored " (symbol->string label) ".")) #t)
                        (values (quill-repl-message run "No such checkpoint.") #t))))]
         [(string=? cmd "dialogue")
          (if (null? args)
              (values (quill-repl-message run "Usage: :dialogue <id>") #t)
              (values (quill-dialogue-start story run (string->symbol (car args)) quill-apply-effects) #t))]
         [(string=? cmd "transcript")
          (values (quill-repl-message run (quill-render-transcript run)) #t)]
         [(string=? cmd "snapshot")
          (values (quill-repl-message run (format "~s" (quill-run->snapshot run))) #t)]
         [(string=? cmd "exercise")
          (if (null? args)
              (values (quill-repl-message run "Usage: :exercise <id>") #t)
              (let* ([exercise-id (string->symbol (car args))]
                     [exercise (quill-story-exercise story exercise-id)])
                    (if exercise
                        (let ([r2 (quill-repl-set-current-exercise run exercise-id)])
                             (values (quill-repl-message r2 (quill-exercise-prompt->string story r2 exercise)) #t))
                        (values (quill-repl-message run "No such exercise.") #t))))]
         [(string=? cmd "answer")
          (let* ([exercise-id (quill-repl-current-exercise run)]
                 [exercise (and exercise-id (quill-story-exercise story exercise-id))])
            (if (not exercise)
                (values (quill-repl-message run "No active exercise.") #t)
                (let-values ([(r2 result) (quill-apply-exercise story run exercise rest quill-apply-effects)])
                  (let* ([pass? (and (assq 'pass? result) (cdr (assq 'pass? result)))]
                         [failed (assq 'failed result)]
                         [msg (if pass?
                                  "Correct."
                                  (string-append "Not yet. Failed: "
                                                 (format "~a" (if failed (cdr failed) ""))))])
                    (values (quill-repl-message r2 msg) #t)))))]
        [else
         (values (quill-repl-message run "Unknown REPL command. Try :help.") #t)])))

(define (quill-repl-step story run line)
  (if (and (string? line) (string-starts-with? (string-trim line) ":"))
      (let-values ([(r2 _handled) (quill-repl-handle-command story run line)])
                  (values r2 #f))
      (quill-step story run line)))

(define (quill-run story . state-opt)
  (quill-assert-valid! story)
  (let ([run (apply quill-start story state-opt)])
       ;; Apply on-enter for the start node once.
       (let loop ([r (quill-enter-node story run)])
            (display (quill-render story r))
            (newline)
            (if (quill-run-done? r)
                r
                (begin
                 (display "> ")
                 (flush-output-port (current-output-port))
                 (let ([line (get-line (current-input-port))])
                      (if (eof-object? line)
                          (quill-run-with-done r #t)
                          (let-values ([(r2 _out) (quill-repl-step story r line)])
                                      (loop r2)))))))))
