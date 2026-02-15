;;; @module ipc-protocol
;;; lattice/ipc/protocol.ss — IPC Wire Protocol
;;;
;;; Pure functions for length-prefixed s-expression framing.
;;; The language IS the protocol: every message is an alist with a 'type field.
;;;
;;; Frame format:
;;;   ┌──────────────┬───────────────────────────┐
;;;   │ length (u32) │ s-expression (UTF-8 bytes) │
;;;   │ big-endian   │ exactly `length` bytes     │
;;;   └──────────────┴───────────────────────────┘

(doc 'module 'ipc-protocol)
(doc 'description "IPC wire protocol — length-prefixed s-expression framing and message types")
(doc 'layer 'lattice)
(doc 'purity 'pure)

;;; ====
;;; Constants
;;; ====

;;; Maximum frame payload size: 16MB
(define *ipc-max-frame-size* (* 16 1024 1024))

;;; Frame header size: 4 bytes (u32 big-endian length)
(define *ipc-header-size* 4)

;;; ====
;;; Request ID Generation
;;; ====

;;; Simple counter-based request ID.
;;; Not globally unique, but unique per-session which is sufficient
;;; since request IDs are scoped to a single connection.
(define *ipc-request-counter* 0)

(define (ipc-next-id!)
  (set! *ipc-request-counter* (+ *ipc-request-counter* 1))
  (string-append "req-" (number->string *ipc-request-counter*)))

;;; ====
;;; Frame Encoding
;;; ====

;;; ipc-encode-frame : S-expr → Bytevector
;;; Encode an s-expression into a length-prefixed frame.
;;; Returns a bytevector: 4-byte big-endian length + UTF-8 payload.
(define (ipc-encode-frame sexp)
  (let* ([payload-str (format "~s" sexp)]
         [payload-bv (string->utf8 payload-str)]
         [payload-len (bytevector-length payload-bv)])
    (when (> payload-len *ipc-max-frame-size*)
      (error 'ipc-encode-frame
             (format "Payload exceeds maximum frame size (~a > ~a)"
                     payload-len *ipc-max-frame-size*)))
    (let ([frame (make-bytevector (+ *ipc-header-size* payload-len))])
      ;; Write 4-byte big-endian length
      (bytevector-u32-set! frame 0 payload-len (endianness big))
      ;; Copy payload
      (bytevector-copy! payload-bv 0 frame *ipc-header-size* payload-len)
      frame)))

;;; ====
;;; Frame Decoding
;;; ====

;;; ipc-frame-length : Bytevector → Fixnum
;;; Extract the payload length from a frame header (first 4 bytes).
;;; The bytevector must be at least 4 bytes long.
(define (ipc-frame-length bv)
  (when (< (bytevector-length bv) *ipc-header-size*)
    (error 'ipc-frame-length "Bytevector too short for frame header"))
  (bytevector-u32-ref bv 0 (endianness big)))

;;; ipc-decode-frame : Bytevector → S-expr
;;; Decode a complete frame (header + payload) into an s-expression.
;;; The bytevector must contain the full frame (header + payload).
(define (ipc-decode-frame bv)
  (let ([bv-len (bytevector-length bv)])
    (when (< bv-len *ipc-header-size*)
      (error 'ipc-decode-frame "Frame too short for header"))
    (let ([payload-len (ipc-frame-length bv)])
      (when (> payload-len *ipc-max-frame-size*)
        (error 'ipc-decode-frame
               (format "Frame payload exceeds maximum size (~a > ~a)"
                       payload-len *ipc-max-frame-size*)))
      (when (< bv-len (+ *ipc-header-size* payload-len))
        (error 'ipc-decode-frame
               (format "Frame truncated: expected ~a payload bytes, have ~a"
                       payload-len (- bv-len *ipc-header-size*))))
      ;; Extract payload bytes
      (let* ([payload-bv (make-bytevector payload-len)])
        (bytevector-copy! bv *ipc-header-size* payload-bv 0 payload-len)
        (let ([payload-str (utf8->string payload-bv)])
          (read (open-input-string payload-str)))))))

;;; ipc-decode-payload : Bytevector → S-expr
;;; Decode a payload bytevector (without header) into an s-expression.
;;; Used when the reactor has already stripped the length prefix.
(define (ipc-decode-payload bv)
  (let ([payload-str (utf8->string bv)])
    (read (open-input-string payload-str))))

;;; ====
;;; Message Constructors
;;; ====

;;; ipc-make-request : String × String → Alist
;;; Construct a request message.
(define (ipc-make-request session expr)
  (let ([id (ipc-next-id!)])
    `((type . request)
      (id . ,id)
      (session . ,session)
      (expr . ,expr))))

;;; ipc-make-request/id : String × String × String → Alist
;;; Construct a request message with explicit ID.
(define (ipc-make-request/id id session expr)
  `((type . request)
    (id . ,id)
    (session . ,session)
    (expr . ,expr)))

;;; ipc-make-result : String × String → Alist
;;; Construct a result message.
(define (ipc-make-result id value)
  `((type . result)
    (id . ,id)
    (value . ,value)))

;;; ipc-make-error : String × Symbol × String → Alist
;;; Construct an error message.
(define (ipc-make-error id code message)
  `((type . error)
    (id . ,id)
    (code . ,code)
    (message . ,message)))

;;; ipc-make-stream : String × String × Boolean → Alist
;;; Construct a stream chunk message.
(define (ipc-make-stream id chunk final?)
  `((type . stream)
    (id . ,id)
    (chunk . ,chunk)
    (final . ,final?)))

;;; ====
;;; v2 Message Constructors
;;; ====

;;; ipc-make-command : String × Symbol × Alist → Alist
;;; Construct a v2 command request with typed args.
(define (ipc-make-command session cmd args)
  (let ([id (ipc-next-id!)])
    `((type . request)
      (v . 2)
      (id . ,id)
      (session . ,session)
      (cmd . ,cmd)
      (args . ,args))))

;;; ipc-make-data-result : String × String × Any → Alist
;;; Construct a v2 result with both display value and structured data.
(define (ipc-make-data-result id value data)
  `((type . result)
    (v . 2)
    (id . ,id)
    (value . ,value)
    (data . ,data)))

;;; ====
;;; v2 Message Accessors
;;; ====

;;; ipc-message-version : Alist → Nat
;;; Extract protocol version. Returns 1 for v1 messages (no v field).
(define (ipc-message-version msg)
  (let ([pair (assq 'v msg)])
    (if pair (cdr pair) 1)))

;;; ipc-message-cmd : Alist → Symbol
;;; Extract command type. Returns 'eval for v1 compat (expr-only requests).
(define (ipc-message-cmd msg)
  (let ([pair (assq 'cmd msg)])
    (if pair (cdr pair) 'eval)))

;;; ipc-message-args : Alist → Alist | #f
;;; Extract command args, or #f for v1 messages.
(define (ipc-message-args msg)
  (let ([pair (assq 'args msg)])
    (if pair (cdr pair) #f)))

;;; ipc-message-data : Alist → Any | #f
;;; Extract structured data from v2 result, or #f for v1 results.
(define (ipc-message-data msg)
  (let ([pair (assq 'data msg)])
    (if pair (cdr pair) #f)))

;;; ipc-make-ping : → Alist
(define (ipc-make-ping) '((type . ping)))

;;; ipc-make-pong : → Alist
(define (ipc-make-pong) '((type . pong)))

;;; ipc-make-ready : → Alist
(define (ipc-make-ready) '((type . ready)))

;;; ipc-make-shutdown : → Alist
;;; Request worker to shut down cleanly.
(define (ipc-make-shutdown) '((type . shutdown)))

;;; ipc-make-shutdown-ack : → Alist
;;; Worker acknowledges shutdown and is exiting.
(define (ipc-make-shutdown-ack) '((type . shutdown-ack)))

;;; ====
;;; Message Accessors
;;; ====

;;; ipc-message-type : Alist → Symbol
;;; Extract the message type.
(define (ipc-message-type msg)
  (let ([pair (assq 'type msg)])
    (if pair (cdr pair) #f)))

;;; ipc-message-id : Alist → String | #f
;;; Extract the request/response ID.
(define (ipc-message-id msg)
  (let ([pair (assq 'id msg)])
    (if pair (cdr pair) #f)))

;;; ipc-message-get : Alist × Symbol → Value | #f
;;; Generic accessor for any message field.
(define (ipc-message-get msg key)
  (let ([pair (assq key msg)])
    (if pair (cdr pair) #f)))

;;; ====
;;; Validation
;;; ====

;;; ipc-valid-message? : Any → Boolean
;;; Check if a value is a structurally valid IPC message.
(define (ipc-valid-message? msg)
  (and (list? msg)
       (not (null? msg))
       (pair? (car msg))  ; alist check
       (assq 'type msg)
       (symbol? (cdr (assq 'type msg)))))

;;; ipc-request? : Alist → Boolean
;;; Accepts both v1 (expr field) and v2 (cmd+args fields) requests.
(define (ipc-request? msg)
  (and (ipc-valid-message? msg)
       (eq? (ipc-message-type msg) 'request)
       (ipc-message-id msg)
       (ipc-message-get msg 'session)
       (or (ipc-message-get msg 'expr)    ; v1
           (ipc-message-get msg 'cmd))    ; v2
       #t))

;;; ipc-result? : Alist → Boolean
(define (ipc-result? msg)
  (and (ipc-valid-message? msg)
       (eq? (ipc-message-type msg) 'result)
       (ipc-message-id msg)
       #t))

;;; ipc-error? : Alist → Boolean
(define (ipc-error? msg)
  (and (ipc-valid-message? msg)
       (eq? (ipc-message-type msg) 'error)
       (ipc-message-id msg)
       #t))
