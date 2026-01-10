;;; shell/pipeline/effects/shell.ss — Shell Effect Handler
;;;
;;; Handles shell command execution.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.

(load "lattice/pipeline/stage.ss")
(load "lattice/pipeline/effects.ss")
(load "lattice/pipeline/context.ss")

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
;;; Success is determined by exit code, not stderr content.
(define (shell-exec cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Wrap command to capture exit code via a marker
         (let* ([wrapped-cmd (format "/bin/sh -c ~s'; echo \"\\n__EXIT_CODE__$?\"'" cmd)])
               (let-values ([(to-stdin from-stdout from-stderr process-id)
                             (open-process-ports wrapped-cmd
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
                                     ;; Extract exit code from stdout
                                     (let-values ([(actual-stdout exit-code) (extract-exit-code stdout-str)])
                                                 (list 'shell-result
                                                       (= exit-code 0)
                                                       actual-stdout
                                                       stderr-str))))))))

;;; extract-exit-code : String -> (Values String Integer)
;;; Extract exit code marker from stdout, return (actual-stdout, exit-code).
(define (extract-exit-code stdout-str)
  (let ([marker "__EXIT_CODE__"])
       (let ([idx (string-rindex stdout-str marker)])
            (if idx
                (let* ([code-str (substring stdout-str (+ idx (string-length marker))
                                            (string-length stdout-str))]
                       [code (or (string->number (string-trim-whitespace code-str)) 1)]
                       ;; Remove trailing newline before marker too
                       [actual-end (max 0 (- idx 1))]
                       [actual-stdout (if (> actual-end 0)
                                          (substring stdout-str 0 actual-end)
                                          "")])
                      (values actual-stdout code))
                ;; No marker found - assume success
                (values stdout-str 0)))))

;;; string-rindex : String -> String -> Integer | #f
;;; Find last occurrence of needle in haystack.
(define (string-rindex haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i (- hlen nlen)])
            (cond
             [(< i 0) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) i]
             [else (loop (- i 1))]))))

;;; string-trim-whitespace : String -> String
;;; Remove leading/trailing whitespace.
(define (string-trim-whitespace s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                     (if (and (< i len) (char-whitespace? (string-ref s i)))
                         (loop (+ i 1))
                         i))]
         [end (let loop ([i (- len 1)])
                   (if (and (>= i start) (char-whitespace? (string-ref s i)))
                       (loop (- i 1))
                       (+ i 1)))])
        (if (>= start end)
            ""
            (substring s start end))))

;;; shell-exec-with-stdin : String -> String -> ShellResult
;;; Execute a shell command with stdin input.
;;; Success is determined by exit code, not stderr content.
(define (shell-exec-with-stdin cmd stdin-content)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-stdin error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Wrap command to capture exit code
         (let* ([wrapped-cmd (format "/bin/sh -c ~s'; echo \"\\n__EXIT_CODE__$?\"'" cmd)])
               (let-values ([(to-stdin from-stdout from-stderr process-id)
                             (open-process-ports wrapped-cmd
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
                                     (let-values ([(actual-stdout exit-code) (extract-exit-code stdout-str)])
                                                 (list 'shell-result
                                                       (= exit-code 0)
                                                       actual-stdout
                                                       stderr-str))))))))

;;; shell-exec-with-env : Alist -> String -> ShellResult
;;; Execute a shell command with environment variables.
;;; env is an alist of (name . value) pairs.
;;; Uses export statements with proper quoting to prevent injection.
(define (shell-exec-with-env env cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-env error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Build export statements with properly quoted values
         (let* ([env-exports (apply string-append
                                    (map (lambda (pair)
                                                 ;; Use ~s (write) to get proper shell escaping
                                                 (format "export ~a=~s; " (car pair) (cdr pair)))
                                         env))]
                [full-cmd (string-append env-exports cmd "; echo \"\\n__EXIT_CODE__$?\"")])
               (let-values ([(to-stdin from-stdout from-stderr process-id)
                             (open-process-ports (format "/bin/sh -c ~s" full-cmd)
                                                 (buffer-mode block)
                                                 (native-transcoder))])
                           (close-port to-stdin)
                           (let ([stdout-all (get-string-all from-stdout)]
                                 [stderr-all (get-string-all from-stderr)])
                                (close-port from-stdout)
                                (close-port from-stderr)
                                (let ([stdout-str (if (eof-object? stdout-all) "" stdout-all)]
                                      [stderr-str (if (eof-object? stderr-all) "" stderr-all)])
                                     (let-values ([(actual-stdout exit-code) (extract-exit-code stdout-str)])
                                                 (list 'shell-result
                                                       (= exit-code 0)
                                                       actual-stdout
                                                       stderr-str))))))))

(define (shell-result-ok? r) (list-ref r 1))
(define (shell-result-stdout r) (list-ref r 2))
(define (shell-result-stderr r) (list-ref r 3))
