;;; shell/lsp/lsp-transport.ss — LSP Transport Layer
;;; @module lsp-transport
;;; @requires prelude json
;;;
;;; Handles LSP message framing over stdio:
;;;   - Content-Length header parsing
;;;   - Buffered reading from stdin
;;;   - Writing with proper framing to stdout
;;;
;;; LSP uses HTTP-like headers:
;;;   Content-Length: <length>\r\n
;;;   \r\n
;;;   <json-body>
;;;
;;; This is Shell code: performs I/O.

(load "core/base/prelude.ss")
(load "shell/tools/string-utils.ss")
(load "shell/lsp/json.ss")

;;; ====
;;; Transport State
;;; ====

;;; The LSP server state
;;; CRITICAL: Capture binary ports at load time!
;;; standard-input-port and standard-output-port can only be called once
;;; reliably - subsequent calls may return EOF ports.
(define *lsp-stdin* (standard-input-port))
(define *lsp-stdout* (standard-output-port))
(define *lsp-stderr* (current-error-port))
(define *lsp-running* #f)

;;; ====
;;; Header Parsing
;;; ====

;;; read-headers : InputPort → (Alist String String)
;;; Read HTTP-like headers until empty line.
(define (read-headers port)
  (let loop ([headers '()])
       (let ([line (read-line-crlf port)])
            (cond
             [(eof-object? line) headers]
             [(string=? line "") (reverse headers)]
             [else
              (let ([colon-pos (string-index line #\:)])
                   (if colon-pos
                       (let* ([key (string-trim (substring line 0 colon-pos))]
                              [val (string-trim (substring line (+ colon-pos 1)
                                                           (string-length line)))])
                             (loop (cons (cons key val) headers)))
                       (loop headers)))]))))

;;; read-line-crlf : InputPort → String | eof
;;; Read a line terminated by \r\n (or just \n for compatibility).
;;; Works with binary ports - headers are ASCII.
(define (read-line-crlf port)
  (let loop ([bytes '()])
       (let ([b (get-u8 port)])
            (cond
             [(eof-object? b)
              (if (null? bytes)
                  b
                  (utf8->string (u8-list->bytevector (reverse bytes))))]
             [(= b 10)  ; \n
              (utf8->string (u8-list->bytevector (reverse bytes)))]
             [(= b 13)  ; \r
              ;; Skip \r, expect \n next
              (let ([next (lookahead-u8 port)])
                   (when (and (not (eof-object? next)) (= next 10))
                         (get-u8 port))
                   (utf8->string (u8-list->bytevector (reverse bytes))))]
             [else
              (loop (cons b bytes))]))))

;;; NOTE: string-index provided by shell/tools/string-utils.ss
;;; NOTE: string-trim provided by core/base/prelude.ss

;;; ====
;;; Message Reading
;;; ====

;;; Maximum message size (10 MB) to prevent DoS attacks
(define *max-message-size* (* 10 1024 1024))

;;; read-lsp-message : InputPort → JsonValue | eof | (error String)
;;; Read a complete LSP message with Content-Length framing.
;;; CRITICAL: Content-Length is in BYTES, not characters.
;;; We must read bytes and decode as UTF-8.
(define (read-lsp-message port)
  (let ([headers (read-headers port)])
       (if (or (eof-object? headers) (null? headers))
           (eof-object)
           (let ([content-length (assoc "Content-Length" headers)])
                (if (not content-length)
                    '(error "Missing Content-Length header")
                    (let ([len (string->number (cdr content-length))])
                         (cond
                          [(not len)
                           '(error "Invalid Content-Length value")]
                          [(> len *max-message-size*)
                           `(error ,(string-append "Message too large: "
                                                   (number->string len)
                                                   " bytes"))]
                          [else
                           (let ([body (read-n-bytes-as-string port len)])
                                (if (not body)
                                    '(error "Incomplete message body")
                                    (json-read body)))])))))))

;;; read-n-bytes-as-string : InputPort × Nat → String | #f
;;; Read exactly n BYTES from port and decode as UTF-8.
;;; Returns #f if fewer bytes available.
(define (read-n-bytes-as-string port n)
  (let ([bv (make-bytevector n)])
       (let loop ([i 0])
            (if (>= i n)
                (utf8->string bv)
                (let ([b (get-u8 port)])
                     (if (eof-object? b)
                         #f  ; Incomplete
                         (begin
                          (bytevector-u8-set! bv i b)
                          (loop (+ i 1)))))))))

;;; ====
;;; Message Writing
;;; ====

;;; write-lsp-message : OutputPort × JsonValue → Void
;;; Write a complete LSP message with Content-Length framing.
;;; CRITICAL: Content-Length must be in BYTES (UTF-8 encoded length).
;;; Uses binary output for proper byte handling.
(define (write-lsp-message port msg)
  (let* ([body (json-write msg)]
         [body-bytes (string->utf8 body)]
         [len (bytevector-length body-bytes)]
         [header (string-append "Content-Length: "
                                (number->string len)
                                "\r\n\r\n")]
         [header-bytes (string->utf8 header)])
        ;; Write header and body as bytes
        (put-bytevector port header-bytes)
        (put-bytevector port body-bytes)
        (flush-output-port port)))

;;; write-lsp-response : Id × JsonValue → Void
;;; Write a JSON-RPC response.
(define (write-lsp-response id result)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "id" id
                               "result" result)))

;;; write-lsp-error : Id × Int × String → Void
;;; Write a JSON-RPC error response.
(define (write-lsp-error id code message)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "id" id
                               "error" (json-obj "code" code
                                                 "message" message))))

;;; write-lsp-notification : String × JsonValue → Void
;;; Write a JSON-RPC notification (no id).
(define (write-lsp-notification method params)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "method" method
                               "params" params)))

;;; ====
;;; Logging
;;; ====

;;; lsp-log : String × ... → Void
;;; Write to stderr for debugging (won't interfere with LSP protocol).
(define (lsp-log fmt . args)
  (apply fprintf *lsp-stderr* (string-append "[fold-lsp] " fmt "\n") args)
  (flush-output-port *lsp-stderr*))

;;; ====
;;; Transport Utilities
;;; ====

;;; init-transport! : → Void
;;; Initialize the transport layer.
;;; Binary ports are already captured at load time.
(define (init-transport!)
  ;; Ports already set at load time - just set running flag
  (set! *lsp-running* #t)
  (lsp-log "Transport initialized (binary mode)"))

;;; shutdown-transport! : → Void
;;; Clean up the transport layer.
(define (shutdown-transport!)
  (set! *lsp-running* #f)
  (lsp-log "Transport shutdown"))

;;; transport-running? : → Boolean
(define (transport-running?)
  *lsp-running*)

;;; ====
;;; Progress Reporting
;;; ====

;;; Progress tokens are simple incrementing integers
(define *progress-token-counter* 0)

;;; next-progress-token : → Int
;;; Generate a unique progress token.
(define (next-progress-token)
  (set! *progress-token-counter* (+ *progress-token-counter* 1))
  *progress-token-counter*)

;;; progress-begin : String × String [× Int] → Int
;;; Start a progress operation. Returns the token for later updates.
;;; title: The title of the operation
;;; message: Optional initial message
;;; percentage: Optional initial percentage (0-100)
(define (progress-begin title . opts)
  (let* ([token (next-progress-token)]
         [message (if (pair? opts) (car opts) #f)]
         [percentage (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #f)]
         [params (json-obj "kind" "begin"
                           "title" title
                           "cancellable" #f)])
        ;; Add optional fields
        (when message
              (set! params (json-obj-set params "message" message)))
        (when percentage
              (set! params (json-obj-set params "percentage" percentage)))
        ;; Send the notification
        (write-lsp-notification "$/progress"
                                (json-obj "token" token
                                          "value" params))
        token))

;;; progress-report : Int × String [× Int] → Void
;;; Report progress on an operation.
;;; token: The token from progress-begin
;;; message: Status message
;;; percentage: Optional percentage (0-100)
(define (progress-report token message . opts)
  (let* ([percentage (if (pair? opts) (car opts) #f)]
         [params (json-obj "kind" "report"
                           "message" message)])
        (when percentage
              (set! params (json-obj-set params "percentage" percentage)))
        (write-lsp-notification "$/progress"
                                (json-obj "token" token
                                          "value" params))))

;;; progress-end : Int [× String] → Void
;;; End a progress operation.
;;; token: The token from progress-begin
;;; message: Optional final message
(define (progress-end token . opts)
  (let* ([message (if (pair? opts) (car opts) #f)]
         [params (json-obj "kind" "end")])
        (when message
              (set! params (json-obj-set params "message" message)))
        (write-lsp-notification "$/progress"
                                (json-obj "token" token
                                          "value" params))))

;;; json-obj-set : JsonObject × String × Any → JsonObject
;;; Add or update a key in a JSON object.
(define (json-obj-set obj key value)
  (let ([pairs (cdr obj)])
       (cons 'json-object
             (cons (cons key value)
                   (filter (lambda (p) (not (string=? (car p) key))) pairs)))))

;;; with-progress : String × (→ α) → α
;;; Execute a thunk with progress reporting.
;;; Shows "Working..." while running, then completes.
(define (with-progress title thunk)
  (let ([token (progress-begin title "Working...")])
       (guard (e [else
                  (progress-end token "Failed")
                  (raise e)])
              (let ([result (thunk)])
                   (progress-end token "Done")
                   result))))

;;; ====
;;; Main Read Loop Helper
;;; ====

;;; with-lsp-message : (JsonValue → Void) → Void
;;; Read one message and call handler, returns #f on EOF or error.
(define (with-lsp-message handler)
  (let ([msg (read-lsp-message *lsp-stdin*)])
       (cond
        [(eof-object? msg)
         (lsp-log "EOF received, shutting down")
         #f]
        [(and (pair? msg) (eq? (car msg) 'error))
         (lsp-log "Read error: ~a" (cadr msg))
         #f]
        [(and (pair? msg) (eq? (car msg) 'ok))
         (handler (cadr msg))
         #t]
        [else
         (lsp-log "Unexpected read result: ~a" msg)
         #f])))
