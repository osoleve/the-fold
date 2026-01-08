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
(load "core/lsp/json.ss")

;;; ============================================================
;;; Transport State
;;; ============================================================

;;; The LSP server state
(define *lsp-stdin* (current-input-port))
(define *lsp-stdout* (current-output-port))
(define *lsp-stderr* (current-error-port))
(define *lsp-running* #f)

;;; ============================================================
;;; Header Parsing
;;; ============================================================

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
(define (read-line-crlf port)
  (let loop ([chars '()])
       (let ([c (read-char port)])
            (cond
             [(eof-object? c)
              (if (null? chars) c (list->string (reverse chars)))]
             [(char=? c #\newline)
              (list->string (reverse chars))]
             [(char=? c #\return)
              ;; Skip \r, expect \n next
              (let ([next (peek-char port)])
                   (when (and (not (eof-object? next)) (char=? next #\newline))
                         (read-char port))
                   (list->string (reverse chars)))]
             [else
              (loop (cons c chars))]))))

;;; string-index : String × Char → Nat | #f
;;; Find first occurrence of character in string.
(define (string-index str ch)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; string-trim : String → String
;;; Trim leading and trailing whitespace.
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                     (if (or (>= i len)
                             (not (char-whitespace? (string-ref str i))))
                         i
                         (loop (+ i 1))))]
         [end (let loop ([i len])
                   (if (or (<= i start)
                           (not (char-whitespace? (string-ref str (- i 1)))))
                       i
                       (loop (- i 1))))])
        (if (>= start end)
            ""
            (substring str start end))))

;;; ============================================================
;;; Message Reading
;;; ============================================================

;;; read-lsp-message : InputPort → JsonValue | eof | (error String)
;;; Read a complete LSP message with Content-Length framing.
(define (read-lsp-message port)
  (let ([headers (read-headers port)])
       (if (or (eof-object? headers) (null? headers))
           (eof-object)
           (let ([content-length (assoc "Content-Length" headers)])
                (if (not content-length)
                    '(error "Missing Content-Length header")
                    (let ([len (string->number (cdr content-length))])
                         (if (not len)
                             '(error "Invalid Content-Length value")
                             (let ([body (read-n-chars port len)])
                                  (if (< (string-length body) len)
                                      '(error "Incomplete message body")
                                      (json-read body))))))))))

;;; read-n-chars : InputPort × Nat → String
;;; Read exactly n characters from port.
(define (read-n-chars port n)
  (let ([buf (make-string n)])
       (let loop ([i 0])
            (if (>= i n)
                buf
                (let ([c (read-char port)])
                     (if (eof-object? c)
                         (substring buf 0 i)
                         (begin
                          (string-set! buf i c)
                          (loop (+ i 1)))))))))

;;; ============================================================
;;; Message Writing
;;; ============================================================

;;; write-lsp-message : OutputPort × JsonValue → Void
;;; Write a complete LSP message with Content-Length framing.
(define (write-lsp-message port msg)
  (let* ([body (json-write msg)]
         [len (string-length body)])
        (display "Content-Length: " port)
        (display len port)
        (display "\r\n\r\n" port)
        (display body port)
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

;;; ============================================================
;;; Logging
;;; ============================================================

;;; lsp-log : String × ... → Void
;;; Write to stderr for debugging (won't interfere with LSP protocol).
(define (lsp-log fmt . args)
  (apply fprintf *lsp-stderr* (string-append "[fold-lsp] " fmt "\n") args)
  (flush-output-port *lsp-stderr*))

;;; ============================================================
;;; Transport Utilities
;;; ============================================================

;;; init-transport! : → Void
;;; Initialize the transport layer.
(define (init-transport!)
  (set! *lsp-stdin* (current-input-port))
  (set! *lsp-stdout* (current-output-port))
  (set! *lsp-stderr* (current-error-port))
  (set! *lsp-running* #t)
  ;; Ensure binary mode for proper Content-Length handling
  ;; (Chez Scheme ports are already binary-safe)
  (lsp-log "Transport initialized"))

;;; shutdown-transport! : → Void
;;; Clean up the transport layer.
(define (shutdown-transport!)
  (set! *lsp-running* #f)
  (lsp-log "Transport shutdown"))

;;; transport-running? : → Boolean
(define (transport-running?)
  *lsp-running*)

;;; ============================================================
;;; Main Read Loop Helper
;;; ============================================================

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
