;;; boundary/repl/repl-worker-socket.ss — Frame-Based REPL Worker
;;;
;;; Per-session worker that reads IPC frames from stdin and writes
;;; response frames to stdout. Spawned by repl-daemon-socket.ss.
;;;
;;; Protocol: Length-prefixed s-expression frames on stdin/stdout.
;;; Same eval logic as repl-worker.ss, different transport.

(load "core/base/sha256.ss")
(load "core/blocks/cas.ss")
(load "core/blocks/normalize.ss")
(load "lattice/ipc/protocol.ss")

(define *quiet* #t)
(load "boundary/repl/error-context.ss")

;;; ====
;;; Content Addressing (same as repl-worker.ss)
;;; ====

(define (extract-definition-body expr)
  (cond
   [(and (pair? expr) (eq? (car expr) 'define))
    (let ([form (cadr expr)])
      (cond
       [(pair? form)
        (cons 'fn (cons (cdr form) (cddr expr)))]
       [else
        (caddr expr)]))]
   [else expr]))

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

(define (definition? expr)
  (and (pair? expr)
       (memq (car expr) '(define define-syntax))))

(define (definition-name expr)
  (let ([form (cadr expr)])
    (if (pair? form) (car form) form)))

;;; ====
;;; Evaluation (same core logic as repl-worker.ss)
;;; ====

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
                    (if is-def (definition-name expr) last-def-name)
                    (if is-def expr last-def-expr))))))))

(define (condition->string c)
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
     [(and who irritants) (format "~a: ~s" who irritants)]
     [who (format "error in ~a" who)]
     [msg msg]
     [irritants (format "error: ~s" irritants)]
     [else "unknown error"])))

(define (suggest-for-unbound sym)
  (guard (ex [else ""])
    (if (and (top-level-bound? 'lattice-export-source)
             (top-level-bound? 'kg-initialized?)
             (kg-initialized?))
        (let ([skill (lattice-export-source sym)])
          (if skill
              (format "\n  Hint: exported by '~a' skill. Try: (li '~a) to find the module"
                      skill skill)
              ""))
        "")))

(define (format-condition e)
  (let ([base-msg
         (if (condition? e)
             (guard (e2 [else (condition->string e)])
               (if (message-condition? e)
                   (let ([template (condition-message e)]
                         [irritants (if (irritants-condition? e)
                                        (condition-irritants e)
                                        '())])
                     (if (null? irritants)
                         template
                         (guard (e3 [else
                                     (let ([who (and (who-condition? e) (condition-who e))])
                                       (if who
                                           (format "~a: ~a ~s" who template irritants)
                                           (format "~a ~s" template irritants)))])
                           (apply format template irritants))))
                   (condition->string e)))
             (format "~a" e))])
    ;; Enrich "not bound" errors with lattice suggestions
    (if (and (condition? e)
             (message-condition? e)
             (string=? (condition-message e) "variable ~:s is not bound")
             (irritants-condition? e)
             (pair? (condition-irritants e)))
        (let* ([raw-sym (car (condition-irritants e))]
               [sym (string->symbol (symbol->string raw-sym))])
          (string-append base-msg (suggest-for-unbound sym)))
        base-msg)))

(define (scheme-eval-and-capture session-id str)
  (let ([output-port (open-output-string)]
        [error-port (open-output-string)])
    (let-values ([(result def-name def-expr)
                  (parameterize ([current-output-port output-port]
                                 [current-error-port error-port]
                                 [*current-session-id* session-id])
                    (scheme-eval-string str))])
      (let* ([stdout-str (get-output-string output-port)]
             [stderr-str (get-output-string error-port)]
             [output (cond
                      [(and (> (string-length stdout-str) 0)
                            (> (string-length stderr-str) 0))
                       (string-append stdout-str "\nstderr: " stderr-str)]
                      [(> (string-length stderr-str) 0)
                       (string-append "stderr: " stderr-str)]
                      [else stdout-str])])
        (cond
         [def-expr
           ;; Compute content-address for CAS but don't return it to user
           (content-address def-expr)
           output]
         [(and (eq? result (void)) (> (string-length output) 0))
          output]
         [(> (string-length output) 0)
          (string-append output
                         (if (eq? result (void))
                             ""
                             (string-append "\n=> " (format "~s" result))))]
         [(not (eq? result (void)))
          (format "~s" result)]
         [else ""])))))

;;; ====
;;; Frame I/O on stdin/stdout
;;; ====

;;; Cache binary ports once — (standard-input-port) and (standard-output-port)
;;; create FRESH ports on each call per R6RS. Multiple ports on the same fd
;;; have independent buffers, so buffered data from port #1 is invisible to
;;; port #2. This causes payload reads to block forever after header reads.
(define *stdin-binary* (standard-input-port))
(define *stdout-binary* (standard-output-port))

;;; read-frame-stdin : → Bytevector | #f
;;; Read a complete length-prefixed frame from stdin (binary mode).
(define (read-frame-stdin)
  (let ([header (get-bytevector-n *stdin-binary* 4)])
    (if (or (eof-object? header) (< (bytevector-length header) 4))
        #f
        (let ([payload-len (bytevector-u32-ref header 0 (endianness big))])
          (if (> payload-len (* 16 1024 1024))
              #f  ; Oversized
              (let ([payload (get-bytevector-n *stdin-binary* payload-len)])
                (if (or (eof-object? payload)
                        (< (bytevector-length payload) payload-len))
                    #f
                    ;; Reconstruct full frame
                    (let ([frame (make-bytevector (+ 4 payload-len))])
                      (bytevector-copy! header 0 frame 0 4)
                      (bytevector-copy! payload 0 frame 4 payload-len)
                      frame))))))))

;;; write-frame-stdout : Bytevector → Void
;;; Write a complete frame to stdout (binary mode).
(define (write-frame-stdout frame)
  (put-bytevector *stdout-binary* frame)
  (flush-output-port *stdout-binary*))

;;; ====
;;; Request Processing
;;; ====

(define (process-request! session-id msg)
  (let ([req-id (ipc-message-id msg)]
        [expr (ipc-message-get msg 'expr)])
    (guard (ex [else
                ;; Eval error → capture for (last-error), then send error frame
                (capture-error! ex)
                (let* ([err-str (format-condition ex)]
                       [err-msg (ipc-make-error req-id 'eval-error err-str)]
                       [frame (ipc-encode-frame err-msg)])
                  (write-frame-stdout frame))])
      (let ([result (scheme-eval-and-capture session-id expr)])
        (let* ([resp (ipc-make-result req-id result)]
               [frame (ipc-encode-frame resp)])
          (write-frame-stdout frame))))))

;;; ====
;;; Worker Loop
;;; ====

(define (worker-loop session-id)
  (let ([frame (read-frame-stdin)])
    (when frame
      (let ([continue?
             (guard (ex [else
                         ;; Decode error — skip this frame
                         (display (format "Worker ~a: decode error\n" session-id)
                                  (current-error-port))
                         #t])
               (let ([msg (ipc-decode-frame frame)])
                 (case (ipc-message-type msg)
                   [(request)
                    (process-request! session-id msg)
                    #t]
                   [(ping)
                    (write-frame-stdout (ipc-encode-frame (ipc-make-pong)))
                    #t]
                   [(shutdown)
                    (write-frame-stdout (ipc-encode-frame (ipc-make-shutdown-ack)))
                    #f]  ; stop looping
                   [else #t])))])
        (when continue?
          (worker-loop session-id))))))

;;; ====
;;; Startup
;;; ====

(define (require-session-id args)
  (if (and (pair? args) (pair? (cdr args)))
      (cadr args)
      (begin
       (display "Usage: scheme --script boundary/repl/repl-worker-socket.ss <session-id>\n"
                (current-error-port))
       (exit 1))))

(define (start-socket-worker!)
  (let ([session-id (require-session-id (command-line))])
    ;; Load the REPL environment so workers have access to Fold commands.
    ;; Redirect stdout to stderr during init to keep the binary frame
    ;; protocol clean — many modules print load banners on stdout.
    (parameterize ([current-output-port (current-error-port)])
      (load "boundary/repl/repl.ss"))
    ;; Run the frame-based worker loop with structured cleanup.
    ;; dynamic-wind ensures flush on any exit path: normal return,
    ;; shutdown frame, EOF, or uncaught exception.
    (dynamic-wind
      (lambda () #f)
      (lambda () (worker-loop session-id))
      (lambda ()
        (guard (ex [else #f])
          (flush-output-port *stdout-binary*))
        (display (format "Worker ~a exiting.\n" session-id)
                 (current-error-port))))))

(start-socket-worker!)
