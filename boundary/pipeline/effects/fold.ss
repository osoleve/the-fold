;;; boundary/pipeline/effects/fold.ss — Fold Effect Handler
;;;
;;; Handles interaction with the Fold REPL daemon via IPC.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

;;; ====
;;; Current Session Parameter
;;; ====

;;; Current session for Fold IPC
(define *pipeline-session* (make-parameter "pipeline"))

;;; ====
;;; Security Utilities
;;; ====

;;; safe-symbol-name? : String → Boolean
;;; SECURITY: Check if a string is a valid Scheme symbol identifier.
;;; Prevents injection via channel names in forum-post.
(define (safe-symbol-name? name)
  (and (string? name)
       (> (string-length name) 0)
       (<= (string-length name) 64)
       ;; Must start with a letter
       (char-alphabetic? (string-ref name 0))
       ;; Can only contain letters, digits, and hyphens
       (let loop ([i 0])
            (if (>= i (string-length name))
                #t
                (let ([c (string-ref name i)])
                     (if (or (char-alphabetic? c)
                             (char-numeric? c)
                             (char=? c #\-)
                             (char=? c #\_))
                         (loop (+ i 1))
                         #f))))))

;;; ====
;;; Fold Effect Interpretation
;;; ====

;;; interpret-fold-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-fold-effect payload ctx state input)
  (let ([op (car payload)])
       (case op
             [(eval)
              (let* ([expr-template (cadr payload)]
                     [expr (expand-template-with-ctx expr-template ctx input)]
                     [result (fold-ipc-eval expr)])
                    (if (fold-result-ok? result)
                        (cons (stage-ok (fold-result-value result)) state)
                        (cons (stage-err 'fold-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(call)
              (let* ([fn-name (cadr payload)]
                     [args (caddr payload)]
                     [expr (format "(~a ~a)" fn-name
                                   (apply string-append
                                          (map (lambda (a) (format " ~s" a)) args)))]
                     [result (fold-ipc-eval expr)])
                    (if (fold-result-ok? result)
                        (cons (stage-ok (fold-result-value result)) state)
                        (cons (stage-err 'fold-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(load)
              (let* ([path (cadr payload)]
                     [result (fold-ipc-eval (format "(load ~s)" path))])
                    (if (fold-result-ok? result)
                        (cons (stage-ok '()) state)
                        (cons (stage-err 'fold-load-error
                                         (fold-result-error result)
                                         result)
                              state)))]
             [(forum-post)
              ;; SECURITY: Validate channel name to prevent Scheme injection
              (let* ([channel-raw (cadr payload)]
                     [channel (if (symbol? channel-raw)
                                  (symbol->string channel-raw)
                                  channel-raw)]
                     [title (caddr payload)]
                     [body (cadddr payload)])
                    (if (not (safe-symbol-name? channel))
                        (cons (stage-err 'forum-error
                                         (format "Invalid channel name: ~a" channel)
                                         payload)
                              state)
                        ;; Channel is validated, safe to use in expression
                        (let* ([expr (format "(msg '~a ~s ~s)" channel title body)]
                               [result (fold-ipc-eval expr)])
                              (if (fold-result-ok? result)
                                  (cons (stage-ok (fold-result-value result)) state)
                                  (cons (stage-err 'forum-error
                                                   (fold-result-error result)
                                                   result)
                                        state)))))]
             [else
              (cons (stage-err 'unknown-fold-op
                               (format "Unknown Fold operation: ~a" op)
                               payload)
                    state)])))

;;; ====
;;; Helper Functions
;;; ====

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

;;; ====
;;; Fold IPC Implementation
;;; ====

;;; fold-ipc-eval : String -> FoldResult
;;; Evaluate an expression via the Fold REPL daemon IPC.
;;; Uses file-based IPC: write to requests/, poll responses/
(define (fold-ipc-eval expr)
  (let ([session-id (*pipeline-session*)]
        [req-dir ".fold-repl/requests"]
        [resp-dir ".fold-repl/responses"])
       (guard (ex [else
                   (list 'fold-result #f #f
                         (format "fold-ipc-eval error: ~a"
                                 (if (message-condition? ex)
                                     (condition-message ex)
                                     "unknown error")))])
              (let ([req-path (string-append req-dir "/" session-id ".ss")]
                    [resp-path (string-append resp-dir "/" session-id ".txt")])
                   ;; Clear any stale response
                   (when (file-exists? resp-path)
                         (delete-file resp-path))
                   ;; Write request
                   (call-with-output-file req-path
                                          (lambda (p)
                                                  (display (format "~s"
                                                                   `((session-id . ,session-id)
                                                                     (expression . ,expr)
                                                                     (timestamp . ,(time-second (current-time)))))
                                                           p)))
                   ;; Poll for response (max 30 seconds)
                   (let loop ([attempts 300])
                        (cond
                         [(file-exists? resp-path)
                          (let ([response (call-with-input-file resp-path get-string-all)])
                               (delete-file resp-path)
                               ;; Check if response indicates error
                               (if (and (>= (string-length response) 6)
                                        (string=? (substring response 0 6) "ERROR:"))
                                   (list 'fold-result #f #f response)
                                   (list 'fold-result #t response #f)))]
                         [(= attempts 0)
                          (list 'fold-result #f #f "Timeout waiting for daemon response")]
                         [else
                          (sleep (make-time 'time-duration 100000000 0))  ; 100ms
                          (loop (- attempts 1))]))))))

(define (fold-result-ok? r) (list-ref r 1))
(define (fold-result-value r) (list-ref r 2))
(define (fold-result-error r) (list-ref r 3))
