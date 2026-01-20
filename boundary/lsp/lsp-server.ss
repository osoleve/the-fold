(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/documents.ss")
(load "boundary/lsp/diagnostics.ss")
(load "boundary/lsp/capabilities.ss")
(load "boundary/lsp/lsp-transport.ss")

(doc 'module 'lsp/lsp-server)
(doc 'description "The main Language Server Protocol server for The Fold. Handles the message loop and coordinates all components.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude json protocol documents diagnostics capabilities lsp-transport))

(doc 'section 'server-state)

(doc 'note "Server initialization state")
(define *server-initialized* #f)
(define *server-shutdown-requested* #f)
(define *client-capabilities* #f)
(define *root-uri* #f)

;;; Register state with the LSP state registry
(lsp-register-state! 'server
                     '(*server-initialized* *server-shutdown-requested*
                       *client-capabilities* *root-uri*)
                     (lambda ()
                       (set! *server-initialized* #f)
                       (set! *server-shutdown-requested* #f)
                       (set! *client-capabilities* #f)
                       (set! *root-uri* #f)))

(doc 'section 'request-handlers)

(doc handle-initialize 'type '(-> JsonObject JsonObject))
(doc handle-initialize 'description "Handle LSP initialize request")
(define (handle-initialize params)
  (set! *client-capabilities* (json-get params "capabilities"))
  (set! *root-uri* (json-get params "rootUri"))
  (lsp-log "Initializing with root: ~a" *root-uri*)
  
  ;; Return InitializeResult
  (json-obj "capabilities" (fold-server-capabilities)
            "serverInfo" (fold-server-info)))

(doc handle-initialized 'type '(-> JsonObject Void))
(define (handle-initialized params)
  (set! *server-initialized* #t)
  (lsp-log "Server initialized"))

(doc handle-shutdown 'type '(-> JsonObject))
(define (handle-shutdown)
  (set! *server-shutdown-requested* #t)
  (lsp-log "Shutdown requested")
  'null)

(doc 'section 'document-sync-handlers)

(doc handle-did-open 'type '(-> JsonObject Void))
(doc handle-did-open 'description "Handle textDocument/didOpen notification")
(define (handle-did-open params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [version (json-get text-doc "version")]
         [text (json-get text-doc "text")])
        (lsp-log "Document opened: ~a" uri)
        (doc-open! uri version text)
        ;; Trigger diagnostics
        (publish-diagnostics uri)))

(doc handle-did-change 'type '(-> JsonObject Void))
(define (handle-did-change params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [version (json-get text-doc "version")]
         [changes (json-get params "contentChanges")])
        ;; Handle both incremental and full sync
        (when (and (json-array? changes) (pair? (cdr changes)))
              (let ([change-list (cdr changes)])  ; Get list of changes
                   (lsp-log "Document changed: ~a (v~a, ~a changes)" uri version (length change-list))
                   ;; Apply changes incrementally
                   (doc-apply-changes! uri version change-list)
                   ;; Trigger diagnostics
                   (publish-diagnostics uri)))))

(doc handle-did-close 'type '(-> JsonObject Void))
(define (handle-did-close params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")])
        (lsp-log "Document closed: ~a" uri)
        (doc-close! uri)
        ;; Clear diagnostics
        (write-lsp-notification *method-publish-diagnostics*
                                (json-obj "uri" uri
                                          "diagnostics" (json-arr)))))

(doc handle-did-save 'type '(-> JsonObject Void))
(define (handle-did-save params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [text (json-get params "text")]
         [path (uri->path uri)])
        (lsp-log "Document saved: ~a" uri)
        ;; Refresh symbol index for this file
        (when (and (string? path)
                   (string-ends-with? path ".ss")
                   (top-level-bound? 'index-refresh-file!))
          (guard (e [else (lsp-log "Index refresh failed: ~a" e)])
                 (index-refresh-file! path)
                 (lsp-log "Index refreshed for ~a" path)))
        (when text
              (let ([doc (doc-get uri)])
                   (when doc
                         (doc-update! uri (+ 1 (document-version doc)) text)
                         (publish-diagnostics uri))))))

(doc 'section 'language-feature-handlers)

(doc handle-hover 'type '(-> JsonObject (U JsonObject null)))
(doc handle-hover 'description "Handle textDocument/hover request")
(define (handle-hover params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-hover doc position)
            'null)))

(doc handle-definition 'type '(-> JsonObject (U JsonObject null)))
(define (handle-definition params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-definition doc position)
            'null)))

(doc handle-completion 'type '(-> JsonObject JsonObject))
(define (handle-completion params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-completions doc position)
            (json-obj "isIncomplete" #f
                      "items" (json-arr)))))

(doc handle-signature-help 'type '(-> JsonObject (U JsonObject null)))
(define (handle-signature-help params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [doc (doc-get uri)])
        (if doc
            (compute-signature-help doc position)
            'null)))

(doc handle-document-symbol 'type '(-> JsonObject JsonArray))
(define (handle-document-symbol params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [doc (doc-get uri)])
        (if doc
            (compute-document-symbols doc)
            (json-arr))))

(doc handle-references 'type '(-> JsonObject JsonArray))
(doc handle-references 'description "Find all references to a symbol")
(define (handle-references params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [context (json-get params "context")]
         [include-decl (if context
                           (json-get context "includeDeclaration")
                           #t)]
         [doc (doc-get uri)])
        (if doc
            (compute-references doc position include-decl)
            (json-arr))))

(doc handle-workspace-symbol 'type '(-> JsonObject JsonArray))
(doc handle-workspace-symbol 'description "Search for symbols across the workspace")
(define (handle-workspace-symbol params)
  (let ([query (or (json-get params "query") "")])
       (compute-workspace-symbols query)))

(doc handle-formatting 'type '(-> JsonObject JsonArray))
(doc handle-formatting 'description "Format a document")
(define (handle-formatting params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [options (json-get params "options")]
         [doc (doc-get uri)])
        (if doc
            (compute-formatting doc options)
            (json-arr))))

(doc handle-rename 'type '(-> JsonObject (U JsonObject null)))
(doc handle-rename 'description "Rename a symbol across all open documents")
(define (handle-rename params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [position (json-get params "position")]
         [new-name (json-get params "newName")]
         [doc (doc-get uri)])
        (if (and doc new-name)
            (compute-rename doc position new-name)
            'null)))

(doc handle-code-action 'type '(-> JsonObject JsonArray))
(doc handle-code-action 'description "Get code actions for a range in a document")
(define (handle-code-action params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [range (json-get params "range")]
         [context (json-get params "context")]
         [doc (doc-get uri)])
        (if doc
            (compute-code-actions doc uri range context)
            (json-arr))))

(doc handle-semantic-tokens 'type '(-> JsonObject JsonObject))
(doc handle-semantic-tokens 'description "Get semantic tokens for a document")
(define (handle-semantic-tokens params)
  (let* ([text-doc (json-get params "textDocument")]
         [uri (json-get text-doc "uri")]
         [doc (doc-get uri)])
        (if doc
            (compute-semantic-tokens doc)
            (json-obj "data" (json-arr)))))

(doc 'section 'diagnostics)

(doc publish-diagnostics 'type '(-> String Void))
(doc publish-diagnostics 'description "Analyze document and publish diagnostics")
(define (publish-diagnostics uri)
  (let ([doc (doc-get uri)])
       (when doc
             (let ([diagnostics (analyze-document-for-diagnostics doc)])
                  (write-lsp-notification *method-publish-diagnostics*
                                          (json-obj "uri" uri
                                                    "diagnostics" (apply json-arr diagnostics)))))))


(doc 'section 'message-dispatch)

(doc dispatch-request 'type '(-> String JsonObject Id Void))
(doc dispatch-request 'description "Dispatch a request to the appropriate handler")
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
          [(string=? method *method-references*)
           (handle-references params)]
          [(string=? method *method-workspace-symbol*)
           (handle-workspace-symbol params)]
          [(string=? method *method-formatting*)
           (handle-formatting params)]
          [(string=? method *method-rename*)
           (handle-rename params)]
          [(string=? method *method-code-action*)
           (handle-code-action params)]
          [(string=? method *method-semantic-tokens*)
           (handle-semantic-tokens params)]
          [else
           (lsp-log "Unknown method: ~a" method)
           (make-error-response id *error-method-not-found*
                                (format "Unknown method: ~a" method))])])
       ;; Send response
       (if (and (json-object? result)
                (json-get result "error"))
           (write-lsp-message *lsp-stdout* result)
           (write-lsp-response id result))))

(doc dispatch-notification 'type '(-> String JsonObject Void))
(doc dispatch-notification 'description "Dispatch a notification to the appropriate handler")
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

(doc handle-message 'type '(-> JsonObject Void))
(doc handle-message 'description "Handle an incoming LSP message")
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

(doc format-condition 'type '(-> Condition String))
(define (format-condition e)
  (if (condition? e)
      (call-with-string-output-port
       (lambda (p) (display-condition e p)))
      (format "~a" e)))

(doc 'section 'main-loop)

(doc run-server! 'type '(-> Void))
(doc run-server! 'description "Main server loop")
(define (run-server!)
  (init-transport!)
  (lsp-log "fold-lsp server starting")
  
  (let loop ()
       (when (transport-running?)
             (when (with-lsp-message handle-message)
                   (loop))))
  
  (lsp-log "fold-lsp server stopped"))

(doc 'section 'entry-point)

(doc 'note "Server is started explicitly from start-lsp.ss")
(doc 'note "Do not auto-start here to avoid double-initialization")
