;;; shell/io/process.ss — Process Execution Utilities
;;;
;;; Simple utilities for running shell commands and capturing output.
;;; Provides a cleaner interface over Chez Scheme's open-process-ports.
;;;
;;; This is Shell code: handles IO, may fail, contains defensive logic.
;;;
;;; Usage:
;;;   (shell-capture "ls -la")           ; Returns stdout as string
;;;   (shell-capture-result "ls -la")    ; Returns (ok? stdout stderr exit-code)
;;;   (shell-run "echo hello")           ; Returns #t/#f for success/failure
;;;
;;; Security:
;;;   Use shell-escape for untrusted input in commands.

(load "core/base/prelude.ss")

;;; ====
;;; Shell Escaping (Security Critical)
;;; ====

;;; shell-escape : String -> String
;;; Escape a string for safe use in shell single quotes.
;;; Single quotes prevent all shell interpretation. To include a single quote
;;; inside single quotes, we end the quote, add an escaped quote, and restart.
;;; Example: "don't" becomes "don'\''t"
(define (shell-escape str)
  (let ([len (string-length str)])
    (let loop ([i 0] [chars '()])
      (if (>= i len)
          (list->string (reverse chars))
          (let ([c (string-ref str i)])
            (if (char=? c #\')
                ;; Replace ' with '\'' (end quote, escaped quote, start quote)
                (loop (+ i 1) (append (reverse (string->list "'\\''")) chars))
                (loop (+ i 1) (cons c chars))))))))

;;; ====
;;; Core Process Execution
;;; ====

;;; shell-capture-result : String -> (List Boolean String String Integer)
;;; Execute a shell command and return structured result.
;;; Returns: (ok? stdout stderr exit-code)
;;;   ok?       - #t if exit code is 0
;;;   stdout    - captured stdout as string
;;;   stderr    - captured stderr as string
;;;   exit-code - numeric exit code
(define (shell-capture-result cmd)
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

;;; extract-exit-code : String -> (Values String Integer)
;;; Extract exit code marker from stdout, return (actual-stdout, exit-code).
(define (extract-exit-code stdout-str)
  (let ([marker "__EXIT__"])
    (let ([idx (string-last-index-of stdout-str marker)])
      (if idx
          (let* ([code-start (+ idx (string-length marker))]
                 [code-str (string-trim (substring stdout-str code-start
                                                   (string-length stdout-str)))]
                 [code (or (string->number code-str) 1)]
                 ;; Remove trailing newline before marker
                 [actual-end (max 0 (- idx 1))]
                 [actual-stdout (if (> actual-end 0)
                                    (substring stdout-str 0 actual-end)
                                    "")])
            (values actual-stdout code))
          ;; No marker found - assume success
          (values stdout-str 0)))))

;;; string-last-index-of : String String -> Integer | #f
;;; Find last occurrence of needle in haystack.
(define (string-last-index-of haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i (- hlen nlen)])
      (cond
        [(< i 0) #f]
        [(string=? (substring haystack i (+ i nlen)) needle) i]
        [else (loop (- i 1))]))))

;;; ====
;;; Convenience Functions
;;; ====

;;; shell-capture : String -> String
;;; Execute command and return stdout as string.
;;; Throws on non-zero exit code.
(define (shell-capture cmd)
  (let ([result (shell-capture-result cmd)])
    (if (car result)
        (cadr result)  ; stdout
        (error 'shell-capture
               (format "Command failed (exit ~a): ~a"
                       (list-ref result 3)
                       (caddr result))))))  ; stderr

;;; shell-capture-stdout : String -> String
;;; Execute command and return stdout, ignoring exit code.
;;; Use when you want output regardless of success.
(define (shell-capture-stdout cmd)
  (cadr (shell-capture-result cmd)))

;;; shell-run : String -> Boolean
;;; Execute command and return success status.
;;; Use for commands where you only care about success/failure.
(define (shell-run cmd)
  (car (shell-capture-result cmd)))

;;; shell-run-quiet : String -> Boolean
;;; Execute command silently, return success status.
;;; Stderr is discarded.
(define (shell-run-quiet cmd)
  (car (shell-capture-result (string-append cmd " 2>/dev/null"))))

;;; ====
;;; Result Accessors
;;; ====

;;; For use with shell-capture-result returns
(define (process-ok? r) (car r))
(define (process-stdout r) (cadr r))
(define (process-stderr r) (caddr r))
(define (process-exit-code r) (list-ref r 3))

;;; ====
;;; REPL Interface
;;; ====

(printf "process.ss loaded.\n")
(printf "  (shell-capture cmd)        - Run command, return stdout\n")
(printf "  (shell-capture-result cmd) - Full result (ok? stdout stderr code)\n")
(printf "  (shell-run cmd)            - Run command, return success?\n")
(printf "  (shell-escape str)         - Escape for shell safety\n")
