;;; thimble/repl-worker.ss — Per-session REPL worker process
;;;
;;; Each worker handles a single session-id and processes requests from:
;;;   .fold-repl/requests/<session-id>.ss
;;; It writes responses to:
;;;   .fold-repl/responses/<session-id>.txt
;;;   .fold-repl/responses/<session-id>.error.txt
;;;
;;; The broker daemon spawns one worker per session-id.
;;;
;;; When a definition is evaluated, returns the content-address (SHA-256)
;;; of the defined value instead of void.

;;; Load hashing dependencies early for content-addressing
(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")

;;; Load normalization for α-equivalence (same variable names = same hash)
;;; This ensures (define (foo x) x) and (define (foo y) y) have the same address
(load "core/blocks/normalize.ss")

;;; Load condition formatter for better error messages
(load "shell/condition-formatter.ss")

(define *poll-interval-ns* 100000000)  ; 100ms
(define *heartbeat-interval* 5)        ; seconds

;; Suppress REPL startup chatter in worker logs.
(define *quiet* #t)

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")
(define *workers-dir* ".fold-repl/workers")
(define *ready-file* ".fold-repl/ready")

;;; ============================================================
;;; Utilities
;;; ============================================================

(define (ensure-dirs!)
  (unless (file-exists? *repl-dir*)
          (mkdir *repl-dir*))
  (unless (file-exists? *requests-dir*)
          (mkdir *requests-dir*))
  (unless (file-exists? *responses-dir*)
          (mkdir *responses-dir*))
  (unless (file-exists? *workers-dir*)
          (mkdir *workers-dir*)))

(define (daemon-running?)
  (file-exists? *ready-file*))

;; format-condition now provided by shell/condition-formatter.ss

(define (escape-json-string str)
  (let ([out (open-output-string)])
       (string-for-each
        (lambda (c)
                (cond
                 [(char=? c #\") (display "\\\"" out)]
                 [(char=? c #\) (display "\\\\") out)]
                 [(char=? c #\b) (display "\\b" out)]
                 [(char=? c #\f) (display "\\f" out)]
                 [(char=? c #\n) (display "\\n" out)]
                 [(char=? c #\r) (display "\\r" out)]
                 [(char=? c #\t) (display "\\t" out)]
                 [(< (char->integer c) 32)
                  (fprintf out "\\u~4,'0x" (char->integer c))]
                 [else (write-char c out)]))
        str)
       (get-output-string out)))

;;; ============================================================
;;; Paths
;;; ============================================================

(define (response-path session-id)
  (string-append *responses-dir* "/" session-id ".txt"))

(define (error-path session-id)
  (string-append *responses-dir* "/" session-id ".error.txt"))

(define (pid-path session-id)
  (string-append *workers-dir* "/" session-id ".pid"))

(define (ready-path session-id)
  (string-append *workers-dir* "/" session-id ".ready"))

(define (heartbeat-path session-id)
  (string-append *workers-dir* "/" session-id ".heartbeat"))

(define (starting-path session-id)
  (string-append *workers-dir* "/" session-id ".starting"))

;;; ============================================================
;;; JSON Parsing (Robust, Dependency-Free)
;;; ============================================================

(define (find-unescaped-quote s start)
  (let loop ([i start])
       (cond
        [(>= i (string-length s)) #f]
        [(char=? (string-ref s i) #\")
         (if (and (> i 0) (char=? (string-ref s (- i 1)) #\\))
             (loop (+ i 1))
             i)]
        [else (loop (+ i 1))])))

(define (unescape-json-string s)
  (let ([out (open-output-string)])
       (let loop ([i 0])
            (cond
             [(>= i (string-length s)) (get-output-string out)]
             [(char=? (string-ref s i) #\\)
              (if (< (+ i 1) (string-length s))
                  (let ([next (string-ref s (+ i 1))])
                       (cond
                        [(char=? next #\n) (display #\newline out)]
                        [(char=? next #\r) (display #\return out)]
                        [(char=? next #\t) (display #\tab out)]
                        [(char=? next #\") (display #\" out)]
                        [(char=? next #\\) (display #\\ out)]
                        [else (display next out)])
                       (loop (+ i 2)))
                  (loop (+ i 1)))]
             [else
              (display (string-ref s i) out)
              (loop (+ i 1))]))))

(define (extract-json-field s field)
  (let* ([quoted-field (string-append "\"" field "\"")]
         [field-pos (string-index-of s quoted-field)])
        (if field-pos
            (let* ([after-field (+ field-pos (string-length quoted-field))]
                   [colon-pos (find-char s #\: after-field)])
                  (if colon-pos
                      (let ([val-start (find-char s #\" (+ colon-pos 1))])
                           (if val-start
                               (let ([val-end (find-unescaped-quote s (+ val-start 1))])
                                    (if val-end
                                        (unescape-json-string (substring s (+ val-start 1) val-end))
                                        #f))
                               #f))
                      #f))
            #f)))

(define (find-char s target start)
  (let loop ([i start])
       (cond
        [(>= i (string-length s)) #f]
        [(char=? (string-ref s i) target) i]
        [else (loop (+ i 1))])))

(define (fold-worker-json-parse s)
  (guard (e [else (list (cons 'error "JSON Parse Error"))])
         (let ([code (extract-json-field s "code")]
               [expr (extract-json-field s "expression")]
               [fmt (extract-json-field s "format")])
              (if (or code expr)
                  (list (cons "code" (or code expr))
                        (cons "format" (or fmt "json")))
                  (list (cons 'error "Missing 'code' or 'expression' field"))))))

;;; ============================================================
;;; Request Parsing
;;; ============================================================

(define (parse-session-request content)
  (guard (e [else #f])
         (let ([data (read (open-input-string content))])
              (if (and (list? data) (assq 'session-id data) (assq 'expression data))
                  data
                  #f))))

(define (extract-expression request)
  (cdr (assq 'expression request)))

;;; ============================================================
;;; Content Addressing
;;; ============================================================

;;; extract-definition-body : S-expr → S-expr
(define (extract-definition-body expr)
  (cond
   [(and (pair? expr) (eq? (car expr) 'define))
    (let ([form (cadr expr)])
         (cond
          [(pair? form)
           (cons 'fn (cons (cdr form) (cddr expr)))]
          [else (caddr expr)]))]
   [else expr]))

;;; content-address : Any → String
(define (content-address value)
  (let* ([body-to-hash (if (pair? value)
                           (extract-definition-body value)
                           value)]
         [normalized (if (pair? body-to-hash)
                         (normalize body-to-hash)
                         body-to-hash)]
         [serialized (string->utf8 (format "~s" normalized))]
         [hash (sha256 serialized)])
        (hash->hex hash)))

;;; definition? : S-expr → Boolean
(define (definition? expr)
  (and (pair? expr)
       (memq (car expr) '(define define-syntax))))

;;; definition-name : S-expr → Symbol
(define (definition-name expr)
  (let ([form (cadr expr)])
       (if (pair? form)
           (car form)
           form)))

;;; ============================================================
;;; Evaluation
;;; ============================================================

(define (scheme-eval-string str)
  (let ([port (open-input-string str)])
       (let loop ([last-result (void)]
                  [last-def-name #f]
                  [last-def-expr #f])
            (let ([expr (read port)])
                 (if (eof-object? expr)
                     (values last-result last-def-name last-def-expr)
                     (let ([is-def (definition? expr)]
                           [result (eval expr)])
                          (loop result
                                (if is-def
                                    (definition-name expr)
                                    last-def-name)
                                (if is-def
                                    expr
                                    last-def-expr))))))))

(define (scheme-eval-and-capture session-id str)
  (let ([output-port (open-output-string)])
       (let-values ([(result def-name def-expr)
                     (parameterize ([current-output-port output-port]
                                    [*current-session-id* session-id])
                                   (scheme-eval-string str))])
                   (let ([output (get-output-string output-port)])
                        (values output
                                (cond
                                 [def-expr (content-address def-expr)]
                                 [(eq? result (void)) (void)]
                                 [else (format "~a" result)])
                                (if def-expr #t #f))))))

;;; ============================================================
;;; Response Helpers
;;; ============================================================

(define (write-response path result)
  (when (file-exists? path)
        (delete-file path))
  (call-with-output-file path
                         (lambda (p)
                                 (display result p))))

(define (write-error path msg)
  (when (file-exists? path)
        (delete-file path))
  (call-with-output-file path
                         (lambda (p)
                                 (display msg p))))

;;; ============================================================
;;; Worker Loop
;;; ============================================================

(define (write-pid! session-id)
  (call-with-output-file (pid-path session-id)
                         (lambda (p)
                                 (display (get-process-id) p))
                         'replace))

(define (write-ready! session-id)
  (call-with-output-file (ready-path session-id)
                         (lambda (p)
                                 (display (format "~a" (current-time)) p))
                         'replace))

(define (write-heartbeat! session-id)
  (call-with-output-file (heartbeat-path session-id)
                         (lambda (p)
                                 (display (time-second (current-time)) p))
                         'replace))

(define (clear-starting! session-id)
  (let ([path (starting-path session-id)])
       (when (file-exists? path)
             (delete-file path))))

(define (cleanup-worker! session-id)
  (let ([paths (list (pid-path session-id)
                     (ready-path session-id)
                     (heartbeat-path session-id))])
       (for-each
        (lambda (path)
                (when (file-exists? path)
                      (delete-file path)))
        paths)))

(define (request-path-ss session-id)
  (string-append *requests-dir* "/" session-id ".ss"))

(define (request-path-json session-id)
  (string-append *requests-dir* "/" session-id ".json"))

(define (process-request! session-id)
  (let ([path-ss (request-path-ss session-id)]
        [path-json (request-path-json session-id)])
       (cond
        [(file-exists? path-json)
         (process-json-request! session-id path-json)]
        [(file-exists? path-ss)
         (process-ss-request! session-id path-ss)])))

(define (process-json-request! session-id path)
  (let* ([content (call-with-input-file path get-string-all)]
         [data (fold-worker-json-parse content)]
         [resp-path (response-path session-id)]
         [err-path (error-path session-id)])
        (when (file-exists? err-path)
              (delete-file err-path))
        (let ([err (assoc 'error data)]
              [expr (cdr (or (assoc "code" data) (assoc "expression" data) '(#f . #f)))]
              [fmt (cdr (or (assoc "format" data) '(#f . "json")))])
             (if err
                 (write-response resp-path (format "{\"status\": \"error\", \"error\": \"~a\"}" (escape-json-string (cdr err))))
                 (if expr
                     (execute-and-respond! session-id expr (string->symbol fmt) resp-path err-path)
                     (write-response resp-path "{\"status\": \"error\", \"error\": \"No code in JSON\"}")))
             (delete-file path))))

(define (process-ss-request! session-id path)
  (let* ([content (call-with-input-file path get-string-all)]
         [request (parse-session-request content)]
         [fmt (if (and request (assq 'format request))
                  (cdr (assq 'format request))
                  'text)]
         [expr (if request
                   (extract-expression request)
                   content)]
         [resp-path (response-path session-id)]
         [err-path (error-path session-id)])
        (execute-and-respond! session-id expr fmt resp-path err-path)
        (delete-file path)))

(define (execute-and-respond! session-id expr fmt resp-path err-path)
  (let ([expr-str (if (string? expr)
                      expr
                      (format "~s" expr))])
       (when (file-exists? err-path)
             (delete-file err-path))
       (guard (e [else
                  (if (eq? fmt 'json)
                      (write-response resp-path
                                      (format "{\"status\": \"error\", \"error\": \"~a\", \"output\": \"\", \"result\": \"\"}"
                                              (escape-json-string (format-condition e))))
                      (write-error err-path (format-condition e)))])
              (let-values ([(output val is-def?) (scheme-eval-and-capture session-id expr-str)])
                          (if (eq? fmt 'json)
                              (let ([val-str (if (eq? val (void)) "" val)])
                                   (write-response resp-path
                                                   (format "{\"output\": \"~a\", \"result\": \"~a\", \"status\": \"success\"}"
                                                           (escape-json-string output)
                                                           (escape-json-string val-str))))
                              (let ([legacy-resp
                                     (cond
                                      [is-def?
                                       (if (> (string-length output) 0)
                                           (string-append output "\n" val)
                                           val)]
                                      [(and (eq? val (void)) (> (string-length output) 0))
                                       output]
                                      [(> (string-length output) 0)
                                       (string-append output
                                                      (if (eq? val (void))
                                                          ""
                                                          (string-append "\n=> " val)))]
                                      [(not (eq? val (void)))
                                       val]
                                      [else ""])])
                                   (write-response resp-path legacy-resp)))))))

(define (worker-loop session-id)
  (let loop ([last-heartbeat 0])
       (if (daemon-running?)
           (begin
            (process-request! session-id)
            (let ([now (time-second (current-time))])
                 (when (>= (- now last-heartbeat) *heartbeat-interval*)
                       (write-heartbeat! session-id)
                       (set! last-heartbeat now)))
            (sleep (make-time 'time-duration *poll-interval-ns* 0))
            (loop last-heartbeat))
           (cleanup-worker! session-id))))

;;; ============================================================
;;; Startup
;;; ============================================================

(define (require-session-id args)
  (if (and (pair? args) (pair? (cdr args)))
      (cadr args)
      (begin
       (display "Usage: scheme --script thimble/repl-worker.ss <session-id>\n")
       (exit 1))))

(define (start-worker!)
  (let ([session-id (require-session-id (command-line))])
       (ensure-dirs!)
       (write-pid! session-id)
       (load "shell/repl.ss")
       (write-ready! session-id)
       (write-heartbeat! session-id)
       (clear-starting! session-id)
       (worker-loop session-id)))

(start-worker!)
