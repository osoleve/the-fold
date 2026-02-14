;;; boundary/repl/repl-daemon-socket.ss — Socket-Based Session Broker Daemon
;;;
;;; Replaces file-polling IPC with Unix domain sockets.
;;; Rust IO reactor handles socket management, frame buffering, connection lifecycle.
;;; Scheme owns routing, worker management, and session policy.
;;;
;;; Architecture:
;;;   Clients ──sock──→ Rust IO Reactor (epoll, frame buffer)
;;;                          ↕ mailbox (ipc-poll-next / ipc-send!)
;;;                     Scheme Daemon (this file)
;;;                          ↕ stdin/stdout pipes
;;;                     Workers (repl-worker-socket.ss)

(load "core/base/prelude.ss")
(load "boundary/ffi/socket-ffi.ss")
(load "lattice/ipc/protocol.ss")

;;; ====
;;; Configuration
;;; ====

(define *repl-dir* ".fold-repl")
(define *workers-dir* ".fold-repl/workers")
(define *ready-file* ".fold-repl/ready")
(define *socket-path* ".fold-repl/fold.sock")
(define *worker-script* "boundary/repl/repl-worker-socket.ss")

(define *poll-interval-ns* 2000000)   ; 2ms — not 100ms like file polling
(define *worker-timeout* 600)         ; 10 min without heartbeat
(define *cleanup-interval* 60)        ; 1 minute between cleanups
(define *idle-timeout* 600)           ; 10 min without request
(define *max-workers* 50)
(define *heartbeat-interval* 30)      ; 30s between pings
(define *heartbeat-timeout* 90)       ; 90s without pong = dead worker

(define *last-cleanup* (time-second (current-time)))

;;; ====
;;; Session State
;;; ====

;;; Session entry: (session-id . session-record)
;;; session-record: (input-port output-port stderr-port pid client-ids last-request expires last-pong)
;;; NOTE: stderr-port MUST be stored to prevent GC from closing the pipe fd.
;;; If GC collects an unreferenced port, Chez closes the fd, the worker gets
;;; SIGPIPE when writing to stderr, and dies silently during init.
(define *sessions* '())

(define (make-session-record inp outp errp pid)
  (let ([now (time-second (current-time))])
    (list inp outp errp pid '() now (+ now 600) now)))

(define (session-input-port rec) (list-ref rec 0))
(define (session-output-port rec) (list-ref rec 1))
(define (session-stderr-port rec) (list-ref rec 2))
(define (session-pid rec) (list-ref rec 3))
(define (session-client-ids rec) (list-ref rec 4))
(define (session-last-request rec) (list-ref rec 5))
(define (session-expires rec) (list-ref rec 6))
(define (session-last-pong rec) (list-ref rec 7))

(define (session-update-pong! rec)
  (set-car! (list-tail rec 7) (time-second (current-time))))

(define (session-set-client-ids! rec ids)
  (set-car! (list-tail rec 4) ids))

(define (session-touch! rec)
  (let ([now (time-second (current-time))])
    (set-car! (list-tail rec 5) now)
    ;; Decay-based expiration extension
    (let* ([current-expires (session-expires rec)]
           [remaining (max 0 (- current-expires now))]
           [decay (max 0.1 (- 1.0 (/ (exact->inexact remaining) 3600.0)))]
           [extension (inexact->exact (floor (* 600 decay)))]
           [new-expires (+ now (max extension 600))])
      (set-car! (list-tail rec 6) new-expires))))

;;; ====
;;; Input Validation
;;; ====

(define (valid-session-id? s)
  (and (string? s)
       (> (string-length s) 0)
       (<= (string-length s) 128)
       (let loop ([i 0])
         (if (>= i (string-length s))
             #t
             (let ([c (string-ref s i)])
               (if (or (char-alphabetic? c)
                       (char-numeric? c)
                       (char=? c #\-)
                       (char=? c #\_))
                   (loop (+ i 1))
                   #f))))))

(define (valid-scheme-command? s)
  (and (string? s)
       (> (string-length s) 0)
       (<= (string-length s) 256)
       (let loop ([i 0])
         (if (>= i (string-length s))
             #t
             (let ([c (string-ref s i)])
               (if (or (char-alphabetic? c)
                       (char-numeric? c)
                       (char=? c #\-)
                       (char=? c #\_)
                       (char=? c #\.)
                       (char=? c #\/))
                   (loop (+ i 1))
                   #f))))))

;;; ====
;;; Utilities
;;; ====

(define (ensure-dirs!)
  (unless (file-exists? *repl-dir*) (mkdir *repl-dir*))
  (unless (file-exists? *workers-dir*) (mkdir *workers-dir*)))

(define (write-ready!)
  (when (file-exists? *ready-file*)
    (delete-file *ready-file*))
  (call-with-output-file *ready-file*
    (lambda (p)
      (display (format "socket:~a" *socket-path*) p))))

(define (clear-ready!)
  (when (file-exists? *ready-file*)
    (delete-file *ready-file*)))

(define (scheme-command)
  (let ([env-cmd (getenv "FOLD_SCHEME_CMD")])
    (cond
     [(not env-cmd) "scheme"]
     [(valid-scheme-command? env-cmd) env-cmd]
     [else
      (display (format "WARNING: Invalid FOLD_SCHEME_CMD rejected: ~s (using 'scheme')\n" env-cmd))
      "scheme"])))

;;; ====
;;; Worker Management
;;; ====

;;; spawn-worker! : String → session-record | #f
;;; Spawn a worker process for a session, connected via stdin/stdout pipes.
(define (spawn-worker! session-id)
  (unless (valid-session-id? session-id)
    (display (format "WARNING: Invalid session-id rejected: ~s\n" session-id))
    (error 'spawn-worker! "Invalid session-id" session-id))

  ;; Count active sessions
  (when (>= (length *sessions*) *max-workers*)
    (display (format "WARNING: Max workers (~a) reached, rejecting session ~a\n"
                     *max-workers* session-id))
    #f)

  (let* ([scheme (scheme-command)]
         [cmd (format "~a --script ~a ~a" scheme *worker-script* session-id)])
    (guard (ex [else
                (display (format "ERROR: Failed to spawn worker for ~a: ~a\n"
                                 session-id
                                 (if (message-condition? ex)
                                     (condition-message ex)
                                     "unknown error")))
                #f])
      (let-values ([(to-worker from-worker from-stderr worker-pid) (open-process-ports cmd)])
        (let ([rec (make-session-record to-worker from-worker from-stderr worker-pid)])
          (set! *sessions* (cons (cons session-id rec) *sessions*))
          (display (format "  [spawn] Worker for session '~a' (pid ~a)\n" session-id worker-pid))
          rec)))))

;;; get-or-spawn-session! : String → session-record | #f
(define (get-or-spawn-session! session-id)
  (let ([entry (assoc session-id *sessions*)])
    (if entry
        (cdr entry)
        (spawn-worker! session-id))))

;;; ====
;;; Message Routing
;;; ====

;;; route-message! : u64 × Sexp → Void
;;; Route an inbound message from a client to the appropriate worker.
(define (route-message! client-id msg)
  (let ([msg-type (ipc-message-type msg)])
    (case msg-type
      [(request)
       (let ([session-id (ipc-message-get msg 'session)]
             [req-id (ipc-message-id msg)]
             [expr (ipc-message-get msg 'expr)])
         (if (and session-id (valid-session-id? session-id))
             (let ([rec (get-or-spawn-session! session-id)])
               (if rec
                   (begin
                     ;; Register this client with the session
                     (let ([current-clients (session-client-ids rec)])
                       (unless (memv client-id current-clients)
                         (session-set-client-ids! rec (cons client-id current-clients))))
                     (session-touch! rec)
                     ;; Forward the request to the worker via stdin pipe
                     (guard (ex [else
                                 ;; Worker died — send error back to client
                                 (let* ([err-msg (ipc-make-error req-id 'worker-error
                                                                  (format "Worker for ~a died" session-id))]
                                        [frame (ipc-encode-frame err-msg)])
                                   (ipc-send! client-id frame))])
                       (let ([frame (ipc-encode-frame msg)])
                         (put-bytevector (session-input-port rec) frame)
                         (flush-output-port (session-input-port rec)))))
                   ;; Could not spawn worker
                   (let* ([err-msg (ipc-make-error req-id 'spawn-error
                                                    (format "Failed to spawn worker for ~a" session-id))]
                          [frame (ipc-encode-frame err-msg)])
                     (ipc-send! client-id frame))))
             ;; Invalid session-id
             (when req-id
               (let* ([err-msg (ipc-make-error req-id 'invalid-session
                                                "Invalid session ID")]
                      [frame (ipc-encode-frame err-msg)])
                 (ipc-send! client-id frame)))))]
      [(ping)
       ;; Respond with pong directly
       (let ([frame (ipc-encode-frame (ipc-make-pong))])
         (ipc-send! client-id frame))]
      [else
       ;; Unknown message type — ignore
       (display (format "  [warn] Unknown message type from client ~a: ~a\n"
                        client-id msg-type))])))

;;; ====
;;; Worker Output Polling
;;; ====

;;; read-available-bytes : InputPort → Bytevector | #f
;;; Read whatever bytes are currently available without blocking.
;;; Returns #f if nothing is available or port is at EOF.
;;; NOTE: Do NOT use port-eof? here — on pipe ports it blocks waiting for
;;; data or close. input-port-ready? is the correct non-blocking check.
(define (read-available-bytes port)
  (guard (ex [else #f])
    (if (input-port-ready? port)
        (let ([data (get-bytevector-some port)])
          (if (eof-object? data) #f data))
        #f)))

;;; Per-session read buffers for incremental frame assembly
(define *session-read-buffers* '())

(define (get-read-buffer session-id)
  (let ([entry (assoc session-id *session-read-buffers*)])
    (if entry (cdr entry) (make-bytevector 0))))

(define (set-read-buffer! session-id bv)
  (let ([entry (assoc session-id *session-read-buffers*)])
    (if entry
        (set-cdr! entry bv)
        (set! *session-read-buffers*
              (cons (cons session-id bv) *session-read-buffers*)))))

(define (clear-read-buffer! session-id)
  (set! *session-read-buffers*
        (filter (lambda (e) (not (equal? (car e) session-id)))
                *session-read-buffers*)))

;;; bytevector-append : Bytevector × Bytevector → Bytevector
(define (bytevector-append a b)
  (let* ([alen (bytevector-length a)]
         [blen (bytevector-length b)]
         [result (make-bytevector (+ alen blen))])
    (bytevector-copy! a 0 result 0 alen)
    (bytevector-copy! b 0 result alen blen)
    result))

;;; try-extract-frame : Bytevector → (Bytevector . Bytevector) | #f
;;; If the buffer contains a complete frame, returns (frame . remaining).
;;; Otherwise returns #f.
(define (try-extract-frame buf)
  (let ([buflen (bytevector-length buf)])
    (if (< buflen 4)
        #f
        (let ([payload-len (bytevector-u32-ref buf 0 (endianness big))])
          (cond
           [(> payload-len (* 16 1024 1024))
            ;; Oversized — discard entire buffer (corrupted stream)
            (cons #f (make-bytevector 0))]
           [(< buflen (+ 4 payload-len))
            #f]  ; Not enough data yet
           [else
            (let ([frame (make-bytevector (+ 4 payload-len))]
                  [remaining-len (- buflen (+ 4 payload-len))])
              (bytevector-copy! buf 0 frame 0 (+ 4 payload-len))
              (let ([remaining (make-bytevector remaining-len)])
                (when (> remaining-len 0)
                  (bytevector-copy! buf (+ 4 payload-len) remaining 0 remaining-len))
                (cons frame remaining)))])))))

;;; read-frame-from-port : InputPort × String → Bytevector | #f
;;; Non-blocking frame read with incremental accumulation.
;;; Never blocks — reads whatever is available and assembles frames
;;; from per-session buffers.
(define (read-frame-from-port port session-id)
  (guard (ex [else #f])
    ;; Read any available bytes into session buffer
    (let ([new-bytes (read-available-bytes port)])
      (when new-bytes
        (set-read-buffer! session-id
                          (bytevector-append (get-read-buffer session-id) new-bytes))))
    ;; Try to extract a complete frame
    (let ([buf (get-read-buffer session-id)])
      (let ([result (try-extract-frame buf)])
        (cond
         [(not result) #f]  ; Not enough data
         [(not (car result))
          ;; Oversized/corrupt — clear buffer
          (clear-read-buffer! session-id)
          #f]
         [else
          (set-read-buffer! session-id (cdr result))
          (car result)])))))

;;; check-worker-outputs! : → Void
;;; Non-blocking poll of all worker output ports.
;;; Reads complete frames, intercepts daemon-internal messages (pong, shutdown-ack),
;;; and routes application responses to subscribed clients.
(define (check-worker-outputs!)
  (for-each
   (lambda (entry)
     (let* ([session-id (car entry)]
            [rec (cdr entry)]
            [outp (session-output-port rec)])
       (guard (ex [else
                   ;; Worker output error — session may be dead
                   #f])
         (let loop ()
           (let ([frame (read-frame-from-port outp session-id)])
             (when frame
               (let ([msg (guard (ex [else #f])
                            (ipc-decode-frame frame))])
                 (when msg
                   (case (ipc-message-type msg)
                     [(pong)
                      ;; Heartbeat response — update timestamp, don't forward
                      (session-update-pong! rec)]
                     [(shutdown-ack)
                      ;; Worker acknowledged shutdown — don't forward
                      #f]
                     [else
                      ;; Application message — forward to subscribed clients
                      (let ([clients (session-client-ids rec)])
                        (for-each
                         (lambda (cid)
                           (guard (ex [else #f])
                             (ipc-send! cid frame)))
                         clients))])))
               (loop)))))))
   *sessions*))

;;; ====
;;; Graceful Worker Shutdown
;;; ====

;;; shutdown-session! : String × SessionRecord → Void
;;; Send shutdown frame, wait briefly for ack, then close ports.
(define (shutdown-session! session-id rec)
  ;; Send shutdown frame
  (guard (ex [else #f])
    (let ([frame (ipc-encode-frame (ipc-make-shutdown))])
      (put-bytevector (session-input-port rec) frame)
      (flush-output-port (session-input-port rec))))
  ;; Poll up to 500ms for shutdown-ack
  (let loop ([attempts 0])
    (when (< attempts 5)
      (let ([frame (read-frame-from-port (session-output-port rec) session-id)])
        (if frame
            (let ([msg (guard (ex [else #f]) (ipc-decode-frame frame))])
              (unless (and msg (eq? (ipc-message-type msg) 'shutdown-ack))
                ;; Not the ack — keep waiting
                (sleep (make-time 'time-duration 100000000 0))
                (loop (+ attempts 1))))
            (begin
              (sleep (make-time 'time-duration 100000000 0))
              (loop (+ attempts 1)))))))
  ;; Close ports regardless
  (guard (ex [else #f]) (close-port (session-input-port rec)))
  (guard (ex [else #f]) (close-port (session-output-port rec)))
  (guard (ex [else #f]) (close-port (session-stderr-port rec)))
  (clear-read-buffer! session-id))

;;; ====
;;; Heartbeat
;;; ====

(define *last-heartbeat* 0)

;;; send-heartbeats! : → Void
;;; Ping all workers. Track unresponsive ones for cleanup.
(define (send-heartbeats!)
  (let ([now (time-second (current-time))])
    (when (> (- now *last-heartbeat*) *heartbeat-interval*)
      (set! *last-heartbeat* now)
      (let ([ping-frame (ipc-encode-frame (ipc-make-ping))])
        (for-each
         (lambda (entry)
           (guard (ex [else #f])
             (put-bytevector (session-input-port (cdr entry)) ping-frame)
             (flush-output-port (session-input-port (cdr entry)))))
         *sessions*)))))

;;; ====
;;; Session Cleanup
;;; ====

(define (cleanup-expired-sessions!)
  (let ([now (time-second (current-time))])
    (let loop ([remaining *sessions*]
               [kept '()])
      (if (null? remaining)
          (set! *sessions* (reverse kept))
          (let* ([entry (car remaining)]
                 [session-id (car entry)]
                 [rec (cdr entry)]
                 [expired? (>= now (session-expires rec))]
                 [unresponsive? (> (- now (session-last-pong rec))
                                   *heartbeat-timeout*)])
            (cond
             [unresponsive?
              ;; Worker already confirmed dead — close ports immediately,
              ;; don't waste time waiting for a shutdown-ack it can't send
              (display (format "  [dead] Session '~a'\n" session-id))
              (guard (ex [else #f]) (close-port (session-input-port rec)))
              (guard (ex [else #f]) (close-port (session-output-port rec)))
              (guard (ex [else #f]) (close-port (session-stderr-port rec)))
              (clear-read-buffer! session-id)
              (loop (cdr remaining) kept)]
             [expired?
              ;; Idle but healthy — attempt graceful shutdown handshake
              (display (format "  [expire] Session '~a'\n" session-id))
              (shutdown-session! session-id rec)
              (loop (cdr remaining) kept)]
             [else
              (loop (cdr remaining) (cons entry kept))]))))))

(define *last-ready-check* 0)

(define (periodic-cleanup!)
  (let ([now (time-second (current-time))])
    (when (> (- now *last-cleanup*) *cleanup-interval*)
      (cleanup-expired-sessions!)
      (set! *last-cleanup* now))
    ;; Check ready file every 2s (fallback if signal handler missed)
    (when (> (- now *last-ready-check*) 2)
      (set! *last-ready-check* now)
      (unless (file-exists? *ready-file*)
        (display "  [shutdown] Ready file removed, initiating shutdown...\n")
        (set! *daemon-running* #f)))))

;;; ====
;;; Main Daemon Loop
;;; ====

(define *daemon-running* #t)

(define (daemon-stop!)
  (set! *daemon-running* #f)
  ;; Stop the reactor first — no new client messages
  (guard (ex [else #f])
    (ipc-reactor-stop!))
  ;; Send shutdown to all workers, then wait briefly for acks
  (let ([shutdown-frame (ipc-encode-frame (ipc-make-shutdown))])
    (for-each
     (lambda (entry)
       (guard (ex [else #f])
         (put-bytevector (session-input-port (cdr entry)) shutdown-frame)
         (flush-output-port (session-input-port (cdr entry)))))
     *sessions*))
  ;; 500ms grace period for workers to ack and exit
  (sleep (make-time 'time-duration 500000000 0))
  ;; Close all worker connections
  (for-each
   (lambda (entry)
     (guard (ex [else #f])
       (close-port (session-input-port (cdr entry))))
     (guard (ex [else #f])
       (close-port (session-output-port (cdr entry))))
     (guard (ex [else #f])
       (close-port (session-stderr-port (cdr entry))))
     (clear-read-buffer! (car entry)))
   *sessions*)
  (set! *sessions* '())
  (clear-ready!)
  ;; Clean up socket file
  (when (file-exists? *socket-path*)
    (delete-file *socket-path*)))

(define (daemon-loop)
  (when *daemon-running*
    ;; 1. Poll for inbound messages from clients (non-blocking)
    (let ([msg-pair (guard (ex [else #f]) (ipc-poll-next))])
      (when msg-pair
        (let ([client-id (car msg-pair)]
              [payload-bv (cdr msg-pair)])
          (guard (ex [else
                      (display (format "  [error] Failed to decode message from client ~a: ~a\n"
                                       client-id
                                       (if (message-condition? ex)
                                           (condition-message ex)
                                           "unknown")))])
            (let ([msg (ipc-decode-payload payload-bv)])
              (route-message! client-id msg))))))

    ;; 2. Check worker outputs (non-blocking)
    (check-worker-outputs!)

    ;; 3. Heartbeat pings
    (send-heartbeats!)

    ;; 4. Periodic maintenance
    (periodic-cleanup!)

    ;; 5. Small sleep to avoid busy-wait (2ms)
    (sleep (make-time 'time-duration *poll-interval-ns* 0))

    (daemon-loop)))

;;; ====
;;; Startup
;;; ====

(define (start-socket-daemon!)
  (ensure-dirs!)

  ;; Load FFI
  (unless (ipc-load!)
    (display "FATAL: Could not load IPC FFI library.\n")
    (display "Build with: cd boundary/ffi/rust-accel && cargo build --release\n")
    (exit 1))

  ;; Start reactor
  (ipc-reactor-start! *socket-path*)

  ;; Write ready sentinel with socket path
  (write-ready!)

  (display "==============================================================\n")
  (display "  THE FOLD -- SOCKET DAEMON STARTED\n")
  (display "==============================================================\n")
  (display (format "Socket:   ~a\n" *socket-path*))
  (display (format "Workers:  ~a\n" *workers-dir*))
  (display (format "Max:      ~a concurrent workers\n" *max-workers*))
  (display "Waiting for connections...\n\n")

  ;; Signal handlers for clean shutdown
  (register-signal-handler 2   ; SIGINT
    (lambda (sig) (set! *daemon-running* #f)))
  (register-signal-handler 15  ; SIGTERM
    (lambda (sig) (set! *daemon-running* #f)))

  (daemon-loop)

  (daemon-stop!)
  (display "Socket daemon stopped.\n"))
