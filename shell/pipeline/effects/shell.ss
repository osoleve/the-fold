;;; shell/pipeline/effects/shell.ss — Shell Effect Handler
;;;
;;; Handles shell command execution.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "core/pipeline/stage.ss")
(load "core/pipeline/effects.ss")
(load "core/pipeline/context.ss")

;;; ============================================================
;;; Shell Effect Interpretation
;;; ============================================================

;;; interpret-shell-effect : Payload -> Context -> State -> Input -> (Result . State)
(define (interpret-shell-effect payload ctx state input)
  (let ([op (car payload)]
        [cmd-template (cadr payload)])
       (case op
             [(run)
              (let* ([cmd (expand-template-with-ctx cmd-template ctx input)]
                     [result (shell-exec cmd)])
                    (if (shell-result-ok? result)
                        (let ([new-state (state-add-log state
                                                        (make-log-entry 'debug
                                                                        (format "Shell: ~a" cmd)
                                                                        '()))])
                             (cons (stage-ok (shell-result-stdout result)) new-state))
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(run-stdin)
              (let* ([cmd (expand-template-with-ctx cmd-template ctx input)]
                     [result (shell-exec-with-stdin cmd input)])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [(run-env)
              (let* ([env-vars (cadr payload)]
                     [cmd (expand-template-with-ctx (caddr payload) ctx input)]
                     [result (shell-exec-with-env env-vars cmd)])
                    (if (shell-result-ok? result)
                        (cons (stage-ok (shell-result-stdout result)) state)
                        (cons (stage-err 'shell-error
                                         (shell-result-stderr result)
                                         result)
                              state)))]
             [else
              (cons (stage-err 'unknown-shell-op
                               (format "Unknown shell operation: ~a" op)
                               payload)
                    state)])))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; expand-template-with-ctx : String -> Context -> Input -> String
(define (expand-template-with-ctx template ctx input)
  (let ([bindings (append (list (cons "input" input))
                          (map (lambda (p) (cons (symbol->string (car p)) (cdr p)))
                               (ctx-env ctx)))])
       (expand-template template bindings)))

;;; ============================================================
;;; Shell Execution Implementation
;;; ============================================================

;;; shell-exec : String -> ShellResult
;;; Execute a shell command and return result with stdout/stderr.
;;; Uses Chez Scheme's open-process-ports for subprocess handling.
(define (shell-exec cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         (let-values ([(to-stdin from-stdout from-stderr process-id)
                       (open-process-ports cmd
                                           (buffer-mode block)
                                           (native-transcoder))])
                     (close-port to-stdin)
                     (let ([stdout-all (get-string-all from-stdout)]
                           [stderr-all (get-string-all from-stderr)])
                          (close-port from-stdout)
                          (close-port from-stderr)
                          ;; Convert eof objects to empty strings
                          (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                               ;; Consider it successful if stderr is empty
                               (list 'shell-result
                                     (string=? stderr-str "")
                                     stdout-str
                                     stderr-str))))))

;;; shell-exec-with-stdin : String -> String -> ShellResult
;;; Execute a shell command with stdin input.
(define (shell-exec-with-stdin cmd stdin-content)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-stdin error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         (let-values ([(to-stdin from-stdout from-stderr process-id)
                       (open-process-ports cmd
                                           (buffer-mode block)
                                           (native-transcoder))])
                     ;; Write stdin content
                     (put-string to-stdin stdin-content)
                     (close-port to-stdin)
                     (let ([stdout-all (get-string-all from-stdout)]
                           [stderr-all (get-string-all from-stderr)])
                          (close-port from-stdout)
                          (close-port from-stderr)
                          (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                               (list 'shell-result
                                     (string=? stderr-str "")
                                     stdout-str
                                     stderr-str))))))

;;; shell-exec-with-env : Alist -> String -> ShellResult
;;; Execute a shell command with environment variables.
;;; env is an alist of (name . value) pairs.
(define (shell-exec-with-env env cmd)
  (let ([env-prefix (apply string-append
                           (map (lambda (pair)
                                        (format "~a=~a " (car pair) (cdr pair)))
                                env))])
       (shell-exec (string-append "env " env-prefix cmd))))

(define (shell-result-ok? r) (list-ref r 1))
(define (shell-result-stdout r) (list-ref r 2))
(define (shell-result-stderr r) (list-ref r 3))
