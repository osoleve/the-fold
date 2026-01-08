;;; shell/lsp/lsp-server.ss — LSP Server Main Loop
;;; @module lsp-server
;;; @requires transport protocol documents handlers
;;;
;;; The main Language Server Protocol server for The Fold.
;;; Handles the message loop and coordinates all components.
;;;
;;; This is Shell code: performs I/O, manages lifecycle.

(load "core/base/prelude.ss")
(load "core/lsp/json.ss")
(load "core/lsp/protocol.ss")
(load "core/lsp/documents.ss")
(load "core/lsp/diagnostics.ss")
(load "core/lsp/capabilities.ss")
(load "shell/lsp/lsp-transport.ss")

;;; ============================================================
;;; Server State
;;; ============================================================

(define *server-initialized* #f)
(define *server-shutdown-requested* #f)
(define *client-capabilities* #f)
(define *root-uri* #f)

;;; ============================================================
;;; Request Handlers
;;; ============================================================

;;; handle-initialize : JsonObject → JsonObject
(define (handle-initialize params)
  (set! *client-capabilities* (json-get params "capabilities"))
  (set! *root-uri* (json-get params "rootUri"))
  (lsp-log "Initializing with root: ~a" *root-uri*)
  
  ;; Return InitializeResult
  (json-obj "capabilities" (fold-server-capabilities)
            "serverInfo" (fold-server-info)))

;;; handle-initialized : JsonObject → Void
(define (handle-initialized params)
  (set! *server-initialized* #t)
  (lsp-log "Server initialized"))

;;; handle-shutdown : → JsonObject
(define (handle-shutdown)
  (set! *server-shutdown-requested* #t)
  (lsp-log "Shutdown requested")
  'null)

;;; ============================================================
;;; Document Sync Handlers
;;; ============================================================

;;; handle-did-open : JsonObject → Void
(define (handle-did-open params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [version (json-get text-doc "version")]
         [text (json-get text-doc "text")])
        (lsp-log "Document opened: ~a" uri)
        (doc-open! uri version text)
        ;; Trigger diagnostics
        (publish-diagnostics uri)))

;;; handle-did-change : JsonObject → Void
(define (handle-did-change params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [version (json-get text-doc "version")]
         [changes (json-get params "contentChanges")])
        ;; Full sync: take the complete new text from first change
        (when (and (json-array? changes) (pair? (cdr changes)))
              (let ([change (cadr changes)])  ; First element after json-array tag
                   (let ([new-text (json-get change "text")])
                        (when new-text
                              (lsp-log "Document changed: ~a (v~a)" uri version)
                              (doc-update! uri version new-text)
                              ;; Trigger diagnostics
                              (publish-diagnostics uri)))))))

;;; handle-did-close : JsonObject → Void
(define (handle-did-close params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")])
        (lsp-log "Document closed: ~a" uri)
        (doc-close! uri)
        ;; Clear diagnostics
        (write-lsp-notification *method-publish-diagnostics*
                                (json-obj "uri" uri
                                          "diagnostics" (json-arr)))))

;;; handle-did-save : JsonObject → Void
(define (handle-did-save params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [text (json-get params "text")])
        (lsp-log "Document saved: ~a" uri)
        (when text
              (let ([doc (doc-get uri)])
                   (when doc
                         (doc-update! uri (+ 1 (document-version doc)) text)
                         (publish-diagnostics uri))))))

;;; ============================================================
;;; Language Feature Handlers (Stubs)
;;; ============================================================

;;; handle-hover : JsonObject → JsonObject | null
(define (handle-hover params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-hover doc position)
            'null)))

;;; handle-definition : JsonObject → JsonObject | null
(define (handle-definition params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-definition doc position)
            'null)))

;;; handle-completion : JsonObject → JsonObject
(define (handle-completion params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-completions doc position)
            (json-obj "isIncomplete" #f
                      "items" (json-arr)))))

;;; handle-signature-help : JsonObject → JsonObject | null
(define (handle-signature-help params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-signature-help doc position)
            'null)))

;;; handle-document-symbol : JsonObject → JsonArray
(define (handle-document-symbol params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [doc (doc-get uri)])
        (if doc
            (compute-document-symbols doc)
            (json-arr))))

;;; ============================================================
;;; Diagnostics
;;; ============================================================

;;; publish-diagnostics : String → Void
;;; Analyze document and publish diagnostics.
(define (publish-diagnostics uri)
  (let ([doc (doc-get uri)])
       (when doc
             (let ([diagnostics (analyze-document-for-diagnostics doc)])
                  (write-lsp-notification *method-publish-diagnostics*
                                          (json-obj "uri" uri
                                                    "diagnostics" (apply json-arr diagnostics)))))))


;;; ============================================================
;;; Message Dispatch
;;; ============================================================

;;; dispatch-request : String × JsonObject × Id → Void
;;; Dispatch a request to the appropriate handler.
(define (dispatch-request method params id)
  (let ([result
         (cond
          [(string=? method *method-initialize*)
           (handle-initialize params)]
          [(string=? method *method-shutdown*)
           (handle-shutdown)]
          [(string=? method *method-hover*)
           (handle-hover params)]
          [(string=? method *method-definition*)
           (handle-definition params)]
          [(string=? method *method-completion*)
           (handle-completion params)]
          [(string=? method *method-signature-help*)
           (handle-signature-help params)]
          [(string=? method *method-document-symbol*)
           (handle-document-symbol params)]
          [else
           (lsp-log "Unknown method: ~a" method)
           (make-error-response id *error-method-not-found*
                                (format "Unknown method: ~a" method))])])
       ;; Send response
       (if (and (json-object? result)
                (json-get result "error"))
           (write-lsp-message *lsp-stdout* result)
           (write-lsp-response id result))))

;;; dispatch-notification : String × JsonObject → Void
;;; Dispatch a notification to the appropriate handler.
(define (dispatch-notification method params)
  (cond
   [(string=? method *method-initialized*)
    (handle-initialized params)]
   [(string=? method *method-exit*)
    (lsp-log "Exit notification received")
    (shutdown-transport!)
    (exit (if *server-shutdown-requested* 0 1))]
   [(string=? method *method-did-open*)
    (handle-did-open params)]
   [(string=? method *method-did-change*)
    (handle-did-change params)]
   [(string=? method *method-did-close*)
    (handle-did-close params)]
   [(string=? method *method-did-save*)
    (handle-did-save params)]
   [else
    (lsp-log "Ignoring notification: ~a" method)]))

;;; handle-message : JsonObject → Void
;;; Handle an incoming LSP message.
(define (handle-message msg)
  (cond
   [(lsp-request? msg)
    (let ([id (lsp-message-id msg)]
          [method (lsp-message-method msg)]
          [params (lsp-message-params msg)])
         (lsp-log "Request: ~a (id=~a)" method id)
         (if (and (not *server-initialized*)
                  (not (string=? method *method-initialize*)))
             (write-lsp-error id *error-server-not-initialized*
                              "Server not initialized")
             (guard (e [else
                        (lsp-log "Error handling request: ~a" (format-condition e))
                        (write-lsp-error id *error-internal-error*
                                         (format "Internal error: ~a" (format-condition e)))])
                    (dispatch-request method params id))))]
   [(lsp-notification? msg)
    (let ([method (lsp-message-method msg)]
          [params (lsp-message-params msg)])
         (lsp-log "Notification: ~a" method)
         (guard (e [else
                    (lsp-log "Error handling notification: ~a" (format-condition e))])
                (dispatch-notification method params)))]
   [else
    (lsp-log "Unknown message type")]))

;;; format-condition : Condition → String
(define (format-condition e)
  (if (condition? e)
      (call-with-string-output-port
       (lambda (p) (display-condition e p)))
      (format "~a" e)))

;;; ============================================================
;;; Main Loop
;;; ============================================================

;;; run-server! : → Void
;;; Main server loop.
(define (run-server!)
  (init-transport!)
  (lsp-log "fold-lsp server starting")
  
  (let loop ()
       (when (transport-running?)
             (when (with-lsp-message handle-message)
                   (loop))))
  
  (lsp-log "fold-lsp server stopped"))

;;; ============================================================
;;; Entry Point
;;; ============================================================

;;; Note: Server is started explicitly from start-lsp.ss
;;; Do not auto-start here to avoid double-initialization.
