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
;;; Shell Escaping (Security Critical)
;;; ============================================================

;;; shell-escape : String -> String
;;; Escape a string for safe use in shell single quotes.
;;; Single quotes prevent all shell interpretation. To include a single quote
;;; inside single quotes, we end the quote, add an escaped quote, and restart.
;;; Example: "don't" becomes 'don'\''t'
(define (shell-escape str)
  (let ([len (string-length str)])
       (let loop ([i 0]
                  [chars '()])
            (if (>= i len)
                (list->string (reverse chars))
                (let ([c (string-ref str i)])
                     (if (char=? c #\')
                         ;; Replace ' with '\'' (end quote, escaped quote, start quote)
                         (loop (+ i 1)
                               (append (reverse (string->list "'\\''")) chars))
                         (loop (+ i 1) (cons c chars))))))))

;;; ============================================================
;;; Shell Execution Implementation
;;; ============================================================

;;; shell-exec : String -> ShellResult
;;; Execute a shell command and return result with stdout/stderr.
;;; Uses Chez Scheme's open-process-ports for subprocess handling.
;;; Success is determined by exit code, not stderr content.
;;; SECURITY: Command is wrapped in single quotes to prevent injection.
(define (shell-exec cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Wrap command in single quotes to prevent shell metacharacter interpretation
         ;; The exit code marker is appended outside the quotes
         (let* ([escaped-cmd (shell-escape cmd)]
                [wrapped-cmd (format "/bin/sh -c '~a; echo \"\\n__EXIT_CODE__$?\"'" escaped-cmd)])
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
;;; SECURITY: Command is wrapped in single quotes to prevent injection.
(define (shell-exec-with-stdin cmd stdin-content)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-stdin error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Wrap command in single quotes to prevent shell metacharacter interpretation
         (let* ([escaped-cmd (shell-escape cmd)]
                [wrapped-cmd (format "/bin/sh -c '~a; echo \"\\n__EXIT_CODE__$?\"'" escaped-cmd)])
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

;;; valid-env-name? : String -> Boolean
;;; Check if a string is a valid shell environment variable name.
;;; Only alphanumeric and underscore, must start with letter or underscore.
(define (valid-env-name? name)
  (and (string? name)
       (> (string-length name) 0)
       (let ([c (string-ref name 0)])
            (or (char-alphabetic? c) (char=? c #\_)))
       (let loop ([i 0])
            (if (>= i (string-length name))
                #t
                (let ([c (string-ref name i)])
                     (if (or (char-alphabetic? c)
                             (char-numeric? c)
                             (char=? c #\_))
                         (loop (+ i 1))
                         #f))))))

;;; shell-exec-with-env : Alist -> String -> ShellResult
;;; Execute a shell command with environment variables.
;;; env is an alist of (name . value) pairs.
;;; SECURITY: Environment variable names are validated, values use single-quote escaping.
(define (shell-exec-with-env env cmd)
  (guard (ex [else
              (list 'shell-result #f ""
                    (format "shell-exec-with-env error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error")))])
         ;; Validate all env var names first
         (let ([invalid-names (filter (lambda (pair) (not (valid-env-name? (car pair)))) env)])
              (if (not (null? invalid-names))
                  (list 'shell-result #f ""
                        (format "Invalid environment variable names: ~a"
                                (map car invalid-names)))
                  ;; Build export statements with single-quote escaped values
                  (let* ([env-exports (apply string-append
                                             (map (lambda (pair)
                                                          ;; Use single quotes with proper escaping
                                                          (format "export ~a='~a'; "
                                                                  (car pair)
                                                                  (shell-escape (cdr pair))))
                                                  env))]
                         [escaped-cmd (shell-escape cmd)]
                         [full-cmd (string-append env-exports escaped-cmd "; echo \"\\n__EXIT_CODE__$?\"")])
                        (let-values ([(to-stdin from-stdout from-stderr process-id)
                                      (open-process-ports (format "/bin/sh -c '~a'" full-cmd)
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
                                                       stderr-str)))))))))

(define (shell-result-ok? r) (list-ref r 1))
(define (shell-result-stdout r) (list-ref r 2))
(define (shell-result-stderr r) (list-ref r 3))
