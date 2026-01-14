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

;;; Expiration with decay parameters
(define *base-extension* 600)     ; Base extension: 10 minutes
(define *max-horizon* 3600)       ; Maximum horizon: 1 hour
(define *min-decay-factor* 0.1)   ; Minimum extension factor (10%)

;; Suppress REPL startup chatter in worker logs.
(define *quiet* #t)

(define *repl-dir* ".fold-repl")
(define *requests-dir* ".fold-repl/requests")
(define *responses-dir* ".fold-repl/responses")
(define *workers-dir* ".fold-repl/workers")
(define *ready-file* ".fold-repl/ready")

;;; ====
;;; Utilities
;;; ====

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

;;; ====
;;; Paths
;;; ====

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

(define (lastreq-path session-id)
  (string-append *workers-dir* "/" session-id ".lastreq"))

(define (expires-path session-id)
  (string-append *workers-dir* "/" session-id ".expires"))

;;; ====
;;; Request Parsing
;;; ====

(define (parse-session-request content)
  (guard (e [else #f])
         (let ([data (read (open-input-string content))])
              (if (and (list? data) (assq 'session-id data) (assq 'expression data))
                  data
                  #f))))

(define (extract-expression request)
  (cdr (assq 'expression request)))

;;; ====
;;; Content Addressing
;;; ====

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

;;; ====
;;; Evaluation
;;; ====

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

;;; ====
;;; Response Helpers
;;; ====

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

;;; ====
;;; Worker Loop
;;; ====

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

(define (write-lastreq! session-id)
  "Write the last-request timestamp. Used by daemon to detect idle workers."
  (call-with-output-file (lastreq-path session-id)
                         (lambda (p)
                                 (display (time-second (current-time)) p))
                         'replace))

;;; read-expires : String -> Int
;;; Read current expiration timestamp, or 0 if not set.
(define (read-expires session-id)
  (guard (e [else 0])
         (let ([path (expires-path session-id)])
              (if (file-exists? path)
                  (call-with-input-file path
                                        (lambda (p)
                                                (let ([line (get-line p)])
                                                     (or (string->number line) 0))))
                  0))))

;;; compute-new-expiration : Int -> Int
;;; Compute new expiration with decay based on remaining time.
;;; Sessions with lots of time left get smaller extensions (diminishing returns).
(define (compute-new-expiration current-expires)
  (let* ([now (time-second (current-time))]
         [time-remaining (max 0 (- current-expires now))]
         ;; Decay factor: 1.0 when about to expire, min-decay-factor when at max horizon
         [decay-factor (max *min-decay-factor*
                            (- 1.0 (/ (exact->inexact time-remaining)
                                      (exact->inexact *max-horizon*))))]
         [extension (inexact->exact (floor (* *base-extension* decay-factor)))]
         ;; Ensure at least base extension from now
         [new-expires (+ now (max extension *base-extension*))])
    new-expires))

;;; write-expires! : String -> Void
;;; Update expiration with decay-based extension.
(define (write-expires! session-id)
  (let* ([current (read-expires session-id)]
         [new-exp (compute-new-expiration current)])
    (call-with-output-file (expires-path session-id)
                           (lambda (p)
                                   (display new-exp p))
                           'replace)))

(define (clear-starting! session-id)
  (let ([path (starting-path session-id)])
       (when (file-exists? path)
             (delete-file path))))

(define (cleanup-worker! session-id)
  (let ([paths (list (pid-path session-id)
                     (ready-path session-id)
                     (heartbeat-path session-id)
                     (lastreq-path session-id)
                     (expires-path session-id))])
       (for-each
        (lambda (path)
                (when (file-exists? path)
                      (delete-file path)))
        paths)))

(define (process-request! session-id)
  (let ([path (request-path session-id)])
       (when (file-exists? path)
             (write-lastreq! session-id)  ; Track last request time (legacy)
             (write-expires! session-id)  ; Extend expiration with decay
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

;;; ====
;;; Startup
;;; ====

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
       (write-lastreq! session-id)  ; Initialize last-request timestamp (legacy)
       (write-expires! session-id)  ; Initialize expiration with base extension
       (clear-starting! session-id)
       (worker-loop session-id)))

(start-worker!)
