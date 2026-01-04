#!/usr/bin/env scheme --script
(define *quiet* #t)
;; Redirect loading output to stderr to keep stdout clean for JSON
(parameterize ((current-output-port (current-error-port)))
              (load "shell/repl.ss"))

;; Parse a JSON string value, handling escape sequences
(define (parse-json-string str start)
  ;; start should point to the opening quote
  (let loop ((i (+ start 1)) (chars '()))
       (if (>= i (string-length str))
           (values #f i)  ; unterminated string
           (let ((c (string-ref str i)))
                (cond
                 ((char=? c #\")
                  ;; End of string - unescape and return
                  (values (unescape-json-string (list->string (reverse chars))) (+ i 1)))
                 ((char=? c #\\)
                  ;; Escape sequence
                  (if (>= (+ i 1) (string-length str))
                      (values #f i)
                      (let ((next (string-ref str (+ i 1))))
                           (loop (+ i 2) (cons next chars)))))
                 (else
                  (loop (+ i 1) (cons c chars))))))))

(define (unescape-json-string s)
  ;; Handle common JSON escape sequences
  (let ((s (string-replace s "\\n" "\n")))
       (let ((s (string-replace s "\\t" "\t")))
            (let ((s (string-replace s "\\r" "\r")))
                 s))))

(define (json-extract-field line field)
  (let* ((search (string-append "\"" field "\":"))
         (pos (string-index-of line search)))
        (if pos
            (let* ((start (+ pos (string-length search)))
                   (sub (substring line start (string-length line)))
                   (trimmed (string-trim sub)))
                  (if (and (> (string-length trimmed) 0)
                           (char=? (string-ref trimmed 0) #\"))
                      (let-values (((val end) (parse-json-string trimmed 0)))
                                  val)
                      #f))
            #f)))

(define (write-json-line id type key val)
  (display "{")
  (display (format "\"id\": \"~a\", " id))
  (display (format "\"type\": \"~a\", " type))
  (display (format "\"~a\": \"~a\"" key (escape-json-string (format "~a" val))))
  (display "}")
  (newline)
  (flush-output-port (current-output-port)))

(define (escape-json-string s)
  (let* ((s (string-replace s "\\" "\\\\"))
         (s (string-replace s "\"" "\\\""))
         (s (string-replace s "\n" "\\n"))
         (s (string-replace s "\r" "\\r"))
         (s (string-replace s "\t" "\\t")))
        s))

(define (execute-code id code)
  (let ((out-p (open-output-string))
        (err-p (open-output-string))
        (actual-stdout (current-output-port)))
       (let ((result
              (guard (e (else (parameterize ((current-output-port actual-stdout))
                                            (write-json-line id "error" "message" (format "~a" e)))
                              (void)))
                     (parameterize ((current-output-port out-p)
                                    (current-error-port err-p)
                                    (*current-session-id* "worker"))
                                   (eval (read (open-input-string code)))))))
            (let ((out (get-output-string out-p))
                  (err (get-output-string err-p)))
                 (parameterize ((current-output-port actual-stdout))
                               (when (> (string-length out) 0) (write-json-line id "stdout" "chunk" out))
                               (when (> (string-length err) 0) (write-json-line id "stderr" "chunk" err))
                               ;; Always send result - use empty string for void to signal completion
                               (write-json-line id "result" "value" (if (eq? result (void)) "" result)))))))

(define (main)
  (let ((line (get-line (current-input-port))))
       (if (and line (not (eof-object? line)))
           (let ((id (json-extract-field line "id"))
                 (code (json-extract-field line "code")))
                (if (and id code)
                    (execute-code id code)
                    (begin
                     (display (format "Worker: Invalid request: ~a\n" line) (console-error-port))))
                (main))
           (exit 0))))

(main)
