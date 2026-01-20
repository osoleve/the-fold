(load "core/base/prelude.ss")

(doc 'module 'process)
(doc 'description "Process Execution Utilities — Simple utilities for running shell commands and capturing output. Provides a cleaner interface over Chez Scheme's open-process-ports.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(prelude))
(doc 'note "Security: Use shell-escape for untrusted input in commands.")

(doc 'section 'shell-escaping)
(doc 'note "Security critical section.")

(define (shell-escape str)
  (doc 'type (-> String String))
  (doc 'description "Escape a string for safe use in shell single quotes. Single quotes prevent all shell interpretation. To include a single quote inside single quotes, we end the quote, add an escaped quote, and restart. Example: \"don't\" becomes \"don'\\''t\".")
  (doc 'export #t)
  (let ([len (string-length str)])
    (let loop ([i 0] [chars '()])
      (if (>= i len)
          (list->string (reverse chars))
          (let ([c (string-ref str i)])
            (if (char=? c #\')
                ;; Replace ' with '\'' (end quote, escaped quote, start quote)
                (loop (+ i 1) (append (reverse (string->list "'\\''")) chars))
                (loop (+ i 1) (cons c chars))))))))

(doc 'section 'core-process-execution)

(define (shell-capture-result cmd)
  (doc 'type (-> String (List Bool String String Integer)))
  (doc 'description "Execute a shell command and return structured result. Returns: (ok? stdout stderr exit-code) where ok? is #t if exit code is 0.")
  (doc 'export #t)
  (guard (ex [else
              (list #f ""
                    (format "Process error: ~a"
                            (if (message-condition? ex)
                                (condition-message ex)
                                "unknown error"))
                    1)])
    ;; Use exit code marker technique to capture exit status
    ;; Wrap command in subshell so 'exit' doesn't prevent marker output
    (let ([wrapped-cmd (format "/bin/sh -c '(~a); echo \"__EXIT__$?\"'"
                               (shell-escape cmd))])
      (let-values ([(to-stdin from-stdout from-stderr pid)
                    (open-process-ports wrapped-cmd
                                        (buffer-mode block)
                                        (native-transcoder))])
        (close-port to-stdin)
        (let ([stdout-raw (get-string-all from-stdout)]
              [stderr-raw (get-string-all from-stderr)])
          (close-port from-stdout)
          (close-port from-stderr)
          (let ([stdout-str (if (eof-object? stdout-raw) "" stdout-raw)]
                [stderr-str (if (eof-object? stderr-raw) "" stderr-raw)])
            (let-values ([(stdout exit-code) (extract-exit-code stdout-str)])
              (list (= exit-code 0) stdout stderr-str exit-code))))))))

(define (extract-exit-code stdout-str)
  (doc 'type (-> String (Values String Integer)))
  (doc 'description "Extract exit code marker from stdout, return (actual-stdout, exit-code).")
  (let ([marker "__EXIT__"])
    (let ([idx (string-last-index-of stdout-str marker)])
      (if idx
          (let* ([code-start (+ idx (string-length marker))]
                 [code-str (string-trim (substring stdout-str code-start
                                                   (string-length stdout-str)))]
                 [code (or (string->number code-str) 1)]
                 ;; Only remove trailing newline if present (fixes off-by-one bug)
                 [has-newline (and (> idx 0)
                                   (char=? (string-ref stdout-str (- idx 1)) #\newline))]
                 [actual-end (if has-newline (- idx 1) idx)]
                 [actual-stdout (if (> actual-end 0)
                                    (substring stdout-str 0 actual-end)
                                    "")])
            (values actual-stdout code))
          ;; No marker found - assume success
          (values stdout-str 0)))))

(define (string-last-index-of haystack needle)
  (doc 'type (-> String String (Maybe Integer)))
  (doc 'description "Find last occurrence of needle in haystack.")
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i (- hlen nlen)])
      (cond
        [(< i 0) #f]
        [(string=? (substring haystack i (+ i nlen)) needle) i]
        [else (loop (- i 1))]))))

(doc 'section 'convenience-functions)

(define (shell-capture cmd)
  (doc 'type (-> String String))
  (doc 'description "Execute command and return stdout as string. Throws on non-zero exit code.")
  (doc 'export #t)
  (let ([result (shell-capture-result cmd)])
    (if (car result)
        (cadr result)  ; stdout
        (error 'shell-capture
               (format "Command failed (exit ~a): ~a"
                       (list-ref result 3)
                       (caddr result))))))  ; stderr

(define (shell-capture-stdout cmd)
  (doc 'type (-> String String))
  (doc 'description "Execute command and return stdout, ignoring exit code. Use when you want output regardless of success.")
  (doc 'export #t)
  (cadr (shell-capture-result cmd)))

(define (shell-run cmd)
  (doc 'type (-> String Bool))
  (doc 'description "Execute command and return success status. Use for commands where you only care about success/failure.")
  (doc 'export #t)
  (car (shell-capture-result cmd)))

(define (shell-run-quiet cmd)
  (doc 'type (-> String Bool))
  (doc 'description "Execute command silently, return success status. Stderr is discarded.")
  (doc 'export #t)
  (car (shell-capture-result (string-append cmd " 2>/dev/null"))))

(doc 'section 'result-accessors)
(doc 'note "For use with shell-capture-result returns.")

(doc process-ok? 'type (-> (List Bool String String Integer) Bool))
(doc process-ok? 'export #t)
(define (process-ok? r) (car r))

(doc process-stdout 'type (-> (List Bool String String Integer) String))
(doc process-stdout 'export #t)
(define (process-stdout r) (cadr r))

(doc process-stderr 'type (-> (List Bool String String Integer) String))
(doc process-stderr 'export #t)
(define (process-stderr r) (caddr r))

(doc process-exit-code 'type (-> (List Bool String String Integer) Integer))
(doc process-exit-code 'export #t)
(define (process-exit-code r) (list-ref r 3))

(doc 'section 'help)

(printf "process.ss loaded.\n")
(printf "  (shell-capture cmd)        - Run command, return stdout\n")
(printf "  (shell-capture-result cmd) - Full result (ok? stdout stderr code)\n")
(printf "  (shell-run cmd)            - Run command, return success?\n")
(printf "  (shell-escape str)         - Escape for shell safety\n")
