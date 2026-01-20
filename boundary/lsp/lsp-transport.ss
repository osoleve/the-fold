(load "core/base/prelude.ss")
(load "boundary/tools/string-utils.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/state.ss")

(doc 'module 'lsp/lsp-transport)
(doc 'description "Handles LSP message framing over stdio: Content-Length header parsing, buffered reading from stdin, and writing with proper framing to stdout")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude string-utils json))
(doc 'note "LSP uses HTTP-like headers: Content-Length: <length>\\r\\n\\r\\n<json-body>")

(doc 'section 'transport-state)

(doc 'note "CRITICAL: Capture binary ports at load time! standard-input-port and standard-output-port can only be called once reliably - subsequent calls may return EOF ports.")
(define *lsp-stdin* (standard-input-port))
(define *lsp-stdout* (standard-output-port))
(define *lsp-stderr* (current-error-port))
(define *lsp-running* #f)

;;; Note: *progress-token-counter* is defined later in this file
;;; Registration happens after it's defined

(doc 'section 'header-parsing)

(doc read-headers 'type '(-> InputPort (Alist String String)))
(doc read-headers 'description "Read HTTP-like headers until empty line")
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

(doc read-line-crlf 'type '(-> InputPort (U String eof)))
(doc read-line-crlf 'description "Read a line terminated by \\r\\n (or just \\n for compatibility)")
(doc read-line-crlf 'note "Works with binary ports - headers are ASCII")
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

(doc 'note "string-index provided by boundary/tools/string-utils.ss")
(doc 'note "string-trim provided by core/base/prelude.ss")

(doc 'section 'message-reading)

(doc 'note "Maximum message size (10 MB) to prevent DoS attacks")
(define *max-message-size* (* 10 1024 1024))

(doc read-lsp-message 'type '(-> InputPort (U JsonValue eof (error String))))
(doc read-lsp-message 'description "Read a complete LSP message with Content-Length framing")
(doc read-lsp-message 'note "CRITICAL: Content-Length is in BYTES, not characters. We must read bytes and decode as UTF-8.")
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

(doc read-n-bytes-as-string 'type '(-> InputPort Nat (U String #f)))
(doc read-n-bytes-as-string 'description "Read exactly n BYTES from port and decode as UTF-8")
(doc read-n-bytes-as-string 'returns "Returns #f if fewer bytes available")
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

(doc 'section 'message-writing)

(doc write-lsp-message 'type '(-> OutputPort JsonValue Void))
(doc write-lsp-message 'description "Write a complete LSP message with Content-Length framing")
(doc write-lsp-message 'note "CRITICAL: Content-Length must be in BYTES (UTF-8 encoded length). Uses binary output for proper byte handling.")
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

(doc write-lsp-response 'type '(-> Id JsonValue Void))
(doc write-lsp-response 'description "Write a JSON-RPC response")
(define (write-lsp-response id result)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "id" id
                               "result" result)))

(doc write-lsp-error 'type '(-> Id Int String Void))
(doc write-lsp-error 'description "Write a JSON-RPC error response")
(define (write-lsp-error id code message)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "id" id
                               "error" (json-obj "code" code
                                                 "message" message))))

(doc write-lsp-notification 'type '(-> String JsonValue Void))
(doc write-lsp-notification 'description "Write a JSON-RPC notification (no id)")
(define (write-lsp-notification method params)
  (write-lsp-message *lsp-stdout*
                     (json-obj "jsonrpc" "2.0"
                               "method" method
                               "params" params)))

(doc 'section 'logging)

(doc lsp-log 'type '(-> String (* Any) Void))
(doc lsp-log 'description "Write to stderr for debugging (won't interfere with LSP protocol)")
(define (lsp-log fmt . args)
  (apply fprintf *lsp-stderr* (string-append "[fold-lsp] " fmt "\n") args)
  (flush-output-port *lsp-stderr*))

(doc 'section 'transport-utilities)

(doc init-transport! 'type '(-> Void))
(doc init-transport! 'description "Initialize the transport layer. Binary ports are already captured at load time.")
(define (init-transport!)
  ;; Ports already set at load time - just set running flag
  (set! *lsp-running* #t)
  (lsp-log "Transport initialized (binary mode)"))

(doc shutdown-transport! 'type '(-> Void))
(doc shutdown-transport! 'description "Clean up the transport layer")
(define (shutdown-transport!)
  (set! *lsp-running* #f)
  (lsp-log "Transport shutdown"))

(doc transport-running? 'type '(-> Boolean))
(define (transport-running?)
  *lsp-running*)

(doc 'section 'progress-reporting)

(doc 'note "Progress tokens are simple incrementing integers")
(define *progress-token-counter* 0)

;;; Register transport state with the LSP state registry
;;; Note: We don't reset ports - only runtime state
(lsp-register-state! 'transport
                     '(*lsp-running* *progress-token-counter*)
                     (lambda ()
                       (set! *lsp-running* #f)
                       (set! *progress-token-counter* 0)))

(doc next-progress-token 'type '(-> Int))
(doc next-progress-token 'description "Generate a unique progress token")
(define (next-progress-token)
  (set! *progress-token-counter* (+ *progress-token-counter* 1))
  *progress-token-counter*)

(doc progress-begin 'type '(-> String (* String) Int))
(doc progress-begin 'description "Start a progress operation. Returns the token for later updates")
(doc progress-begin 'param 'title "The title of the operation")
(doc progress-begin 'param 'message "Optional initial message")
(doc progress-begin 'param 'percentage "Optional initial percentage (0-100)")
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

(doc progress-report 'type '(-> Int String (* Int) Void))
(doc progress-report 'description "Report progress on an operation")
(doc progress-report 'param 'token "The token from progress-begin")
(doc progress-report 'param 'message "Status message")
(doc progress-report 'param 'percentage "Optional percentage (0-100)")
(define (progress-report token message . opts)
  (let* ([percentage (if (pair? opts) (car opts) #f)]
         [params (json-obj "kind" "report"
                           "message" message)])
        (when percentage
              (set! params (json-obj-set params "percentage" percentage)))
        (write-lsp-notification "$/progress"
                                (json-obj "token" token
                                          "value" params))))

(doc progress-end 'type '(-> Int (* String) Void))
(doc progress-end 'description "End a progress operation")
(doc progress-end 'param 'token "The token from progress-begin")
(doc progress-end 'param 'message "Optional final message")
(define (progress-end token . opts)
  (let* ([message (if (pair? opts) (car opts) #f)]
         [params (json-obj "kind" "end")])
        (when message
              (set! params (json-obj-set params "message" message)))
        (write-lsp-notification "$/progress"
                                (json-obj "token" token
                                          "value" params))))

(doc json-obj-set 'type '(-> JsonObject String Any JsonObject))
(doc json-obj-set 'description "Add or update a key in a JSON object")
(define (json-obj-set obj key value)
  (let ([pairs (cdr obj)])
       (cons 'json-object
             (cons (cons key value)
                   (filter (lambda (p) (not (string=? (car p) key))) pairs)))))

(doc with-progress 'type '(-> String (-> α) α))
(doc with-progress 'description "Execute a thunk with progress reporting")
(doc with-progress 'note "Shows 'Working...' while running, then completes")
(define (with-progress title thunk)
  (let ([token (progress-begin title "Working...")])
       (guard (e [else
                  (progress-end token "Failed")
                  (raise e)])
              (let ([result (thunk)])
                   (progress-end token "Done")
                   result))))

(doc 'section 'main-read-loop-helper)

(doc with-lsp-message 'type '(-> (-> JsonValue Void) Boolean))
(doc with-lsp-message 'description "Read one message and call handler. Returns #f only on EOF (shutdown)")
(doc with-lsp-message 'note "Parse errors send a JSON-RPC ParseError (-32700) and continue")
(define (with-lsp-message handler)
  (let ([msg (read-lsp-message *lsp-stdin*)])
       (cond
        [(eof-object? msg)
         (lsp-log "EOF received, shutting down")
         #f]
        [(and (pair? msg) (eq? (car msg) 'error))
         ;; Parse or transport error - send ParseError response and continue
         ;; JSON-RPC spec: id is null when we couldn't parse the request
         (lsp-log "Read error (continuing): ~a" (cadr msg))
         (write-lsp-error 'null -32700 (cadr msg))  ; -32700 = ParseError
         #t]  ; Continue the server loop
        [(and (pair? msg) (eq? (car msg) 'ok))
         (handler (cadr msg))
         #t]
        [else
         (lsp-log "Unexpected read result (continuing): ~a" msg)
         (write-lsp-error 'null -32700 "Malformed message")
         #t])))
