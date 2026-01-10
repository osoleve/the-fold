;;; shell/repl/repl-worker.ss — Per-session REPL worker process
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

(define (condition->string c)
  "Build a human-readable string from condition components."
  (let ([who (and (who-condition? c) (condition-who c))]
        [msg (and (message-condition? c) (condition-message c))]
        [irritants (and (irritants-condition? c) (condition-irritants c))])
       (cond
        [(and msg (null? (or irritants '())))
         (if who (format "~a: ~a" who msg) msg)]
        [(and msg irritants)
         (if who
             (format "~a: ~a ~s" who msg irritants)
             (format "~a ~s" msg irritants))]
        [(and who irritants)
         (format "~a: ~s" who irritants)]
        [who (format "error in ~a" who)]
        [msg msg]
        [irritants (format "error: ~s" irritants)]
        [else "unknown error"])))

(define (format-condition e)
  "Format a condition with its irritants properly filled in."
  (if (condition? e)
      (guard (e2 [else (condition->string e)])  ; fallback to component extraction
             (if (message-condition? e)
                 (let ([template (condition-message e)]
                       [irritants (if (irritants-condition? e)
                                      (condition-irritants e)
                                      '())])
                      (if (null? irritants)
                          template
                          ;; Try to apply format; if it fails, just append irritants
                          (guard (e3 [else
                                      (let ([who (and (who-condition? e) (condition-who e))])
                                           (if who
                                               (format "~a: ~a ~s" who template irritants)
                                               (format "~a ~s" template irritants)))])
                                 (apply format template irritants))))
                 ;; No message - build description from available info
                 (condition->string e)))
      (format "~a" e)))

;;; ============================================================
;;; Paths
;;; ============================================================

(define (request-path session-id)
  (string-append *requests-dir* "/" session-id ".ss"))

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
;;; Extract the actual code being defined (without the 'define' keyword).
;;; For (define (foo x) x), returns (fn (x) x) for normalization.
;;; For (define foo 42), returns 42.
(define (extract-definition-body expr)
  (cond
   [(and (pair? expr) (eq? (car expr) 'define))
    (let ([form (cadr expr)])
         (cond
          [(pair? form)
           ;; (define (name args...) body...)
           ;; Convert to (fn (args...) body...) for normalization
           (cons 'fn (cons (cdr form) (cddr expr)))]
          [else
           ;; (define name value)
           ;; Just return the value
           (caddr expr)]))]
   [else expr]))

;;; content-address : Any → String
;;; Compute the content-address (SHA-256 hex) of any Scheme value.
;;; For definitions, hashes the normalized body (not the outer define form).
;;; Normalization converts to de Bruijn indices, ensuring α-equivalent
;;; expressions (same structure, different variable names) hash identically.
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
;;; Check if an expression is a definition form.
(define (definition? expr)
  (and (pair? expr)
       (memq (car expr) '(define define-syntax))))

;;; definition-name : S-expr → Symbol
;;; Extract the name being defined from a definition form.
(define (definition-name expr)
  (let ([form (cadr expr)])
       (if (pair? form)
           (car form)   ; (define (foo x) ...) -> foo
           form)))      ; (define foo ...) -> foo

;;; ============================================================
;;; Evaluation
;;; ============================================================

(define (scheme-eval-string str)
  "Evaluate a string containing Scheme expressions.
   Returns (values result last-defined-name last-def-expr) where:
   - last-defined-name is the symbol of the last definition, or #f
   - last-def-expr is the full definition expression, or #f"
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
  "Evaluate expressions and capture both stdout and return value.
   For definitions, returns the content-address of the definition expression."
  (let ([output-port (open-output-string)])
       (let-values ([(result def-name def-expr)
                     (parameterize ([current-output-port output-port]
                                    [*current-session-id* session-id])
                                   (scheme-eval-string str))])
                   (let ([output (get-output-string output-port)])
                        (cond
                         ;; Definition: return content-address of the definition expression
                         [def-expr
                           (let ([addr (content-address def-expr)])
                                (if (> (string-length output) 0)
                                    (string-append output "\n" addr)
                                    addr))]
                         ;; Only output, no meaningful return value
                         [(and (eq? result (void)) (> (string-length output) 0))
                          output]
                         ;; Both output and result
                         [(> (string-length output) 0)
                          (string-append output
                                         (if (eq? result (void))
                                             ""
                                             (string-append "\n=> " (format "~a" result))))]
                         ;; Only result, no output
                         [(not (eq? result (void)))
                          (format "~a" result)]
                         ;; Nothing
                         [else ""])))))

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

(define (process-request! session-id)
  (let ([path (request-path session-id)])
       (when (file-exists? path)
             (let* ([content (call-with-input-file path get-string-all)]
                    [request (parse-session-request content)]
                    [expr (if request
                              (extract-expression request)
                              content)]
                    [expr-str (if (string? expr)
                                  expr
                                  (format "~s" expr))]
                    [resp-path (response-path session-id)]
                    [err-path (error-path session-id)])
                   (when (file-exists? err-path)
                         (delete-file err-path))
                   (guard (e [else
                              (write-error err-path (format-condition e))])
                          (let ([result (scheme-eval-and-capture session-id expr-str)])
                               (write-response resp-path result)))
                   (delete-file path)))))

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
       (display "Usage: scheme --script shell/repl-worker.ss <session-id>\n")
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
