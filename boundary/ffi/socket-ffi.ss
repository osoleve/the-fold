;;; boundary/ffi/socket-ffi.ss — IPC Reactor FFI Bindings
;;;
;;; Scheme-side bindings for the Rust IPC reactor.
;;; Follows posix-ffi.ss patterns: load-once, out-pointer, memory-safe cleanup.
;;;
;;; Self-contained: loads the shared library directly.

;;; ====
;;; Library Loading (self-contained, same paths as ffi-core.ss)
;;; ====

(define *ipc-lib-loaded* #f)

(define *ipc-lib-paths*
  (list
   "boundary/ffi/rust-accel/target/release/libfold_accel.so"
   "boundary/ffi/rust-accel/target/debug/libfold_accel.so"
   "/usr/local/lib/libfold_accel.so"))

(define (ipc-find-library)
  (let loop ([paths *ipc-lib-paths*])
    (if (null? paths)
        #f
        (if (file-exists? (car paths))
            (car paths)
            (loop (cdr paths))))))

(define (ipc-ensure-library!)
  (if *ipc-lib-loaded*
      #t
      (let ([path (ipc-find-library)])
        (if path
            (guard (ex [else #f])
              (load-shared-object path)
              (set! *ipc-lib-loaded* #t)
              #t)
            #f))))

;;; ====
;;; FFI Type Definitions
;;; ====

;;; Matches Rust's StatusResult (from posix.rs)
(define-ftype ipc-status-result-t
  (struct
   [status unsigned-8]
   [error-code integer-32]))

;;; Matches Rust's IntResult (from posix.rs)
(define-ftype ipc-int-result-t
  (struct
   [status unsigned-8]
   [value  integer-32]
   [error-code integer-32]))

;;; ====
;;; Foreign Procedure Bindings
;;; ====

(define *ipc-bound* #f)

;; Server-side (reactor)
(define rust-ipc-start #f)
(define rust-ipc-stop #f)
(define rust-ipc-poll #f)
(define rust-ipc-send #f)
(define rust-ipc-client-count #f)

;; Client-side
(define rust-ipc-connect #f)
(define rust-ipc-client-send #f)
(define rust-ipc-client-recv #f)
(define rust-ipc-client-close #f)

;;; Poll buffer: reusable 16MB buffer for poll results
(define *ipc-poll-buffer* #f)
(define *ipc-poll-buffer-size* (* 16 1024 1024))

;;; Recv buffer: reusable buffer for client recv
(define *ipc-recv-buffer* #f)
(define *ipc-recv-buffer-size* (* 16 1024 1024))

(define (bind-ipc-procedures!)
  (when (not *ipc-bound*)
    ;; Server: start reactor
    (set! rust-ipc-start
          (foreign-procedure "fold_ipc_start"
                             (u8* size_t (* ipc-status-result-t))
                             void))
    ;; Server: stop reactor
    (set! rust-ipc-stop
          (foreign-procedure "fold_ipc_stop"
                             ((* ipc-status-result-t))
                             void))
    ;; Server: poll inbound message
    ;; out_client_id and out_len are passed as bytevectors (u8*), read via bytevector-u64-native-ref
    (set! rust-ipc-poll
          (foreign-procedure "fold_ipc_poll"
                             (u8* u8* size_t u8* (* ipc-status-result-t))
                             void))
    ;; Server: send to client
    (set! rust-ipc-send
          (foreign-procedure "fold_ipc_send"
                             (unsigned-64 u8* size_t (* ipc-status-result-t))
                             void))
    ;; Server: client count
    (set! rust-ipc-client-count
          (foreign-procedure "fold_ipc_client_count" () unsigned-64))

    ;; Client: connect
    (set! rust-ipc-connect
          (foreign-procedure "fold_ipc_connect"
                             (u8* size_t (* ipc-int-result-t))
                             void))
    ;; Client: send
    (set! rust-ipc-client-send
          (foreign-procedure "fold_ipc_client_send"
                             (integer-32 u8* size_t (* ipc-status-result-t))
                             void))
    ;; Client: recv
    ;; out_len is passed as bytevector (u8*), read via bytevector-u64-native-ref
    (set! rust-ipc-client-recv
          (foreign-procedure "fold_ipc_client_recv"
                             (integer-32 u8* size_t u8* (* ipc-status-result-t))
                             void))
    ;; Client: close
    (set! rust-ipc-client-close
          (foreign-procedure "fold_ipc_client_close"
                             (integer-32 (* ipc-status-result-t))
                             void))

    ;; Allocate buffers
    (set! *ipc-poll-buffer* (make-bytevector *ipc-poll-buffer-size*))
    (set! *ipc-recv-buffer* (make-bytevector *ipc-recv-buffer-size*))

    (set! *ipc-bound* #t)))

;;; ====
;;; Public Loading API
;;; ====

;;; ipc-load! : → Boolean
;;; Load acceleration library and bind IPC procedures.
(define (ipc-load!)
  (if *ipc-bound*
      #t
      (if (ipc-ensure-library!)
          (begin
            (bind-ipc-procedures!)
            #t)
          #f)))

;;; ipc-available? : → Boolean
(define (ipc-available?)
  (and *ipc-lib-loaded* *ipc-bound*))

;;; ====
;;; Memory-Safe FFI Helpers
;;; ====

(define (ipc-call-with-foreign-alloc size thunk)
  (let ([addr (foreign-alloc size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk addr))
      (lambda () (foreign-free addr)))))

(define (ipc-call-with-status-result ffi-proc result-proc)
  (ipc-call-with-foreign-alloc
   (ftype-sizeof ipc-status-result-t)
   (lambda (addr)
     (let ([ptr (make-ftype-pointer ipc-status-result-t addr)])
       (ffi-proc ptr)
       (result-proc
        (ftype-ref ipc-status-result-t (status) ptr)
        (ftype-ref ipc-status-result-t (error-code) ptr))))))

(define (ipc-call-with-int-result ffi-proc result-proc)
  (ipc-call-with-foreign-alloc
   (ftype-sizeof ipc-int-result-t)
   (lambda (addr)
     (let ([ptr (make-ftype-pointer ipc-int-result-t addr)])
       (ffi-proc ptr)
       (result-proc
        (ftype-ref ipc-int-result-t (status) ptr)
        (ftype-ref ipc-int-result-t (value) ptr)
        (ftype-ref ipc-int-result-t (error-code) ptr))))))

;;; ====
;;; Server-Side API
;;; ====

;;; ipc-reactor-start! : String → Boolean
;;; Start the Rust IPC reactor on the given Unix domain socket path.
(define (ipc-reactor-start! path)
  (unless (ipc-available?)
    (error 'ipc-reactor-start! "IPC FFI not loaded. Call (ipc-load!) first."))
  (let ([path-bv (string->utf8 path)])
    (ipc-call-with-status-result
     (lambda (ptr)
       (rust-ipc-start path-bv (bytevector-length path-bv) ptr))
     (lambda (status errno)
       (if (= status 0)
           #t
           (error 'ipc-reactor-start!
                  (format "Failed to start reactor on ~a (errno ~a)" path errno)))))))

;;; ipc-reactor-stop! : → Boolean
;;; Stop the reactor and clean up.
(define (ipc-reactor-stop!)
  (unless (ipc-available?)
    (error 'ipc-reactor-stop! "IPC FFI not loaded"))
  (ipc-call-with-status-result
   (lambda (ptr) (rust-ipc-stop ptr))
   (lambda (status errno) (= status 0))))

;;; ipc-poll-next : → (client-id . Bytevector) | #f
;;; Non-blocking poll for the next complete inbound message.
;;; Returns (client-id . payload-bytevector) or #f if queue is empty.
(define (ipc-poll-next)
  (unless (ipc-available?)
    (error 'ipc-poll-next "IPC FFI not loaded"))
  (let ([cid-bv (make-bytevector 8 0)]
        [len-bv (make-bytevector 8 0)])
    (ipc-call-with-status-result
     (lambda (ptr)
       (rust-ipc-poll cid-bv *ipc-poll-buffer* *ipc-poll-buffer-size* len-bv ptr))
     (lambda (status errno)
       (cond
        [(= status 0)
         (let ([client-id (bytevector-u64-native-ref cid-bv 0)]
               [payload-len (bytevector-u64-native-ref len-bv 0)])
           (let ([payload (make-bytevector payload-len)])
             (bytevector-copy! *ipc-poll-buffer* 0 payload 0 payload-len)
             (cons client-id payload)))]
        [(= status 1) #f]
        [else
         (error 'ipc-poll-next
                (format "Poll error (status ~a, errno ~a)" status errno))])))))

;;; ipc-send! : u64 × Bytevector → Boolean
;;; Send a complete frame (with length header) to a specific client.
(define (ipc-send! client-id frame-bv)
  (unless (ipc-available?)
    (error 'ipc-send! "IPC FFI not loaded"))
  (ipc-call-with-status-result
   (lambda (ptr)
     (rust-ipc-send client-id frame-bv (bytevector-length frame-bv) ptr))
   (lambda (status errno) (= status 0))))

;;; ipc-client-count : → Fixnum
(define (ipc-client-count)
  (unless (ipc-available?)
    (error 'ipc-client-count "IPC FFI not loaded"))
  (rust-ipc-client-count))

;;; ====
;;; Client-Side API
;;; ====

;;; ipc-connect : String → Fixnum (fd)
;;; Connect to the daemon's Unix domain socket. Returns the fd.
(define (ipc-connect path)
  (unless (ipc-available?)
    (error 'ipc-connect "IPC FFI not loaded"))
  (let ([path-bv (string->utf8 path)])
    (ipc-call-with-int-result
     (lambda (ptr)
       (rust-ipc-connect path-bv (bytevector-length path-bv) ptr))
     (lambda (status value errno)
       (if (= status 0)
           value
           (error 'ipc-connect
                  (format "Failed to connect to ~a (errno ~a)" path errno)))))))

;;; ipc-client-send : Fixnum × Bytevector → Boolean
(define (ipc-client-send fd frame-bv)
  (unless (ipc-available?)
    (error 'ipc-client-send "IPC FFI not loaded"))
  (ipc-call-with-status-result
   (lambda (ptr)
     (rust-ipc-client-send fd frame-bv (bytevector-length frame-bv) ptr))
   (lambda (status errno) (= status 0))))

;;; ipc-client-recv : Fixnum → Bytevector | #f
;;; Receive a complete frame payload from the daemon (blocking with timeout).
(define (ipc-client-recv fd)
  (unless (ipc-available?)
    (error 'ipc-client-recv "IPC FFI not loaded"))
  (let ([len-bv (make-bytevector 8 0)])
    (ipc-call-with-status-result
     (lambda (ptr)
       (rust-ipc-client-recv fd *ipc-recv-buffer* *ipc-recv-buffer-size* len-bv ptr))
     (lambda (status errno)
       (if (= status 0)
           (let ([payload-len (bytevector-u64-native-ref len-bv 0)])
             (let ([payload (make-bytevector payload-len)])
               (bytevector-copy! *ipc-recv-buffer* 0 payload 0 payload-len)
               payload))
           #f)))))

;;; ipc-client-close : Fixnum → Boolean
(define (ipc-client-close fd)
  (unless (ipc-available?)
    (error 'ipc-client-close "IPC FFI not loaded"))
  (ipc-call-with-status-result
   (lambda (ptr) (rust-ipc-client-close fd ptr))
   (lambda (status errno) (= status 0))))

;;; ====
;;; Initialization
;;; ====

(unless (or (top-level-bound? '*socket-ffi-loaded*)
            (and (top-level-bound? '*quiet*) (top-level-value '*quiet*)))
  (display "socket-ffi.ss loaded.\n")
  (display "  (ipc-load!)                      - Load FFI library\n")
  (display "  (ipc-reactor-start! path)        - Start reactor\n")
  (display "  (ipc-reactor-stop!)              - Stop reactor\n")
  (display "  (ipc-poll-next)                  - Poll for inbound message\n")
  (display "  (ipc-send! client-id frame-bv)   - Send frame to client\n"))

(define *socket-ffi-loaded* #t)
