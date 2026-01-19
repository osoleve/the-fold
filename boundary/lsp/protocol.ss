;;; core/lsp/protocol.ss — LSP Protocol Types and Constants
;;; @module protocol
;;; @requires prelude json
;;;
;;; Defines LSP protocol structures:
;;;   - JSON-RPC message types (request, response, notification)
;;;   - LSP method names and constants
;;;   - Error codes
;;;   - Server capabilities
;;;
;;; This is Core code: pure definitions.

(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")

;;; ====
;;; JSON-RPC Message Types
;;; ====

;;; Message classification
;;; A request has: id, method, params
;;; A response has: id, result OR error
;;; A notification has: method, params (no id)

;;; lsp-request? : JsonObject → Boolean
;;; Note: Use assoc to check key presence, not json-get.
;;; json-get returns #f for both "key missing" and "value is #f",
;;; which would incorrectly reject id:0 or id:false (though id:false
;;; is invalid JSON-RPC, robustness is preferred).
(define (lsp-request? msg)
  (and (json-object? msg)
       (assoc "id" (cdr msg))       ; Key exists (any value including 0, null)
       (json-get msg "method")))

;;; lsp-notification? : JsonObject → Boolean
;;; Note: Use assoc to check key absence.
(define (lsp-notification? msg)
  (and (json-object? msg)
       (not (assoc "id" (cdr msg))) ; Key must NOT exist
       (json-get msg "method")))

;;; lsp-response? : JsonObject → Boolean
;;; Note: Use assoc for id check (consistent with lsp-request?).
(define (lsp-response? msg)
  (and (json-object? msg)
       (assoc "id" (cdr msg))          ; Key exists (handles id:0)
       (or (assoc "result" (cdr msg))  ; Use assoc to detect explicit null
           (json-get msg "error"))))

;;; ====
;;; Message Accessors
;;; ====

;;; lsp-message-id : JsonObject → Any
(define (lsp-message-id msg)
  (json-get msg "id"))

;;; lsp-message-method : JsonObject → String
(define (lsp-message-method msg)
  (json-get msg "method"))

;;; lsp-message-params : JsonObject → JsonValue
(define (lsp-message-params msg)
  (or (json-get msg "params") (json-obj)))

;;; ====
;;; Response Construction
;;; ====

;;; make-response : Id × JsonValue → JsonObject
(define (make-response id result)
  (json-obj "jsonrpc" "2.0"
            "id" id
            "result" result))

;;; make-error-response : Id × Int × String [× JsonValue] → JsonObject
(define (make-error-response id code message . data)
  (let ([error-obj (if (null? data)
                       (json-obj "code" code "message" message)
                       (json-obj "code" code "message" message "data" (car data)))])
       (json-obj "jsonrpc" "2.0"
                 "id" id
                 "error" error-obj)))

;;; make-notification : String × JsonValue → JsonObject
(define (make-notification method params)
  (json-obj "jsonrpc" "2.0"
            "method" method
            "params" params))

;;; ====
;;; LSP Error Codes
;;; ====

;;; JSON-RPC reserved errors
(define *error-parse-error* -32700)
(define *error-invalid-request* -32600)
(define *error-method-not-found* -32601)
(define *error-invalid-params* -32602)
(define *error-internal-error* -32603)

;;; LSP reserved errors
(define *error-server-not-initialized* -32002)
(define *error-unknown-error-code* -32001)
(define *error-request-failed* -32803)
(define *error-server-cancelled* -32802)
(define *error-content-modified* -32801)
(define *error-request-cancelled* -32800)

;;; ====
;;; LSP Method Names
;;; ====

;;; Lifecycle
(define *method-initialize* "initialize")
(define *method-initialized* "initialized")
(define *method-shutdown* "shutdown")
(define *method-exit* "exit")

;;; Document Synchronization
(define *method-did-open* "textDocument/didOpen")
(define *method-did-change* "textDocument/didChange")
(define *method-did-close* "textDocument/didClose")
(define *method-did-save* "textDocument/didSave")

;;; Language Features
(define *method-hover* "textDocument/hover")
(define *method-completion* "textDocument/completion")
(define *method-signature-help* "textDocument/signatureHelp")
(define *method-definition* "textDocument/definition")
(define *method-references* "textDocument/references")
(define *method-document-symbol* "textDocument/documentSymbol")
(define *method-workspace-symbol* "workspace/symbol")
(define *method-rename* "textDocument/rename")
(define *method-formatting* "textDocument/formatting")
(define *method-code-action* "textDocument/codeAction")
(define *method-semantic-tokens* "textDocument/semanticTokens/full")
(define *method-diagnostic* "textDocument/diagnostic")

;;; Server → Client notifications
(define *method-publish-diagnostics* "textDocument/publishDiagnostics")
(define *method-log-message* "window/logMessage")
(define *method-show-message* "window/showMessage")

;;; ====
;;; Server Capabilities
;;; ====

;;; TextDocumentSyncKind
(define *sync-none* 0)
(define *sync-full* 1)
(define *sync-incremental* 2)

;;; CompletionTriggerKind
(define *trigger-invoked* 1)
(define *trigger-character* 2)
(define *trigger-incomplete* 3)

;;; CompletionItemKind
(define *completion-text* 1)
(define *completion-method* 2)
(define *completion-function* 3)
(define *completion-constructor* 4)
(define *completion-field* 5)
(define *completion-variable* 6)
(define *completion-class* 7)
(define *completion-interface* 8)
(define *completion-module* 9)
(define *completion-property* 10)
(define *completion-keyword* 14)
(define *completion-snippet* 15)
(define *completion-operator* 24)

;;; DiagnosticSeverity
(define *severity-error* 1)
(define *severity-warning* 2)
(define *severity-information* 3)
(define *severity-hint* 4)

;;; MessageType (for window/showMessage)
(define *message-error* 1)
(define *message-warning* 2)
(define *message-info* 3)
(define *message-log* 4)

;;; fold-server-capabilities : → JsonObject
;;; Returns the server capabilities for the initialize response.
(define (fold-server-capabilities)
  (json-obj
   ;; Document sync: incremental sync for better performance
   "textDocumentSync" (json-obj
                       "openClose" #t
                       "change" *sync-incremental*
                       "save" (json-obj "includeText" #t))
   ;; Hover support
   "hoverProvider" #t
   ;; Completion support
   "completionProvider" (json-obj
                         "triggerCharacters" (json-arr "(" "'" ":")
                         "resolveProvider" #f)
   ;; Signature help (function parameter hints)
   "signatureHelpProvider" (json-obj
                            "triggerCharacters" (json-arr "(" " ")
                            "retriggerCharacters" (json-arr " "))
   ;; Go to definition
   "definitionProvider" #t
   ;; Find all references
   "referencesProvider" #t
   ;; Document symbols (outline)
   "documentSymbolProvider" #t
   ;; Workspace symbol search
   "workspaceSymbolProvider" #t
   ;; Document formatting
   "documentFormattingProvider" #t
   ;; Rename support
   "renameProvider" #t
   ;; Code actions (quick fixes)
   "codeActionProvider" (json-obj
                         "codeActionKinds" (json-arr "quickfix" "refactor"))
   ;; Semantic tokens for rich syntax highlighting
   "semanticTokensProvider" (json-obj
                             "legend" (json-obj
                                       "tokenTypes" (json-arr
                                                     "keyword"       ; 0
                                                     "function"      ; 1
                                                     "variable"      ; 2
                                                     "string"        ; 3
                                                     "number"        ; 4
                                                     "comment"       ; 5
                                                     "operator"      ; 6
                                                     "macro"         ; 7
                                                     "parameter"     ; 8
                                                     "type")         ; 9
                                       "tokenModifiers" (json-arr
                                                         "definition"
                                                         "declaration"
                                                         "readonly"))
                             "full" #t)))

;;; fold-server-info : → JsonObject
;;; Returns server info for the initialize response.
(define (fold-server-info)
  (json-obj "name" "fold-lsp"
            "version" "0.1.0"))

;;; ====
;;; Position and Range Types
;;; ====

;;; make-position : Nat × Nat → JsonObject
;;; Create an LSP Position (0-indexed line and character).
(define (make-position line character)
  (json-obj "line" line "character" character))

;;; make-range : Position × Position → JsonObject
;;; Create an LSP Range.
(define (make-range start end)
  (json-obj "start" start "end" end))

;;; make-location : String × Range → JsonObject
;;; Create an LSP Location.
(define (make-location uri range)
  (json-obj "uri" uri "range" range))

;;; ====
;;; Diagnostic Construction
;;; ====

;;; make-diagnostic : Range × String × Int [× String] → JsonObject
;;; Create an LSP Diagnostic.
(define (make-diagnostic range message severity . opts)
  (let* ([base (json-obj "range" range
                         "severity" severity
                         "source" "fold"
                         "message" message)]
         [with-code (if (and (pair? opts) (car opts))
                        (cons (cons "code" (car opts)) (cdr base))
                        (cdr base))])
        (cons 'json-object with-code)))

;;; ====
;;; Hover Content
;;; ====

;;; make-hover : String [× Range] → JsonObject
;;; Create an LSP Hover response.
(define (make-hover contents . range)
  (let ([base (json-obj "contents" (json-obj "kind" "markdown"
                                             "value" contents))])
       (if (and (pair? range) (car range))
           (cons 'json-object (cons (cons "range" (car range)) (cdr base)))
           base)))

;;; ====
;;; Signature Help
;;; ====

;;; make-signature-help : (List SignatureInfo) × Int × Int → JsonObject
;;; Create an LSP SignatureHelp response.
;;; signatures: list of signature infos
;;; activeSignature: index of active signature (0-indexed)
;;; activeParameter: index of active parameter (0-indexed)
(define (make-signature-help signatures active-sig active-param)
  (json-obj "signatures" (apply json-arr signatures)
            "activeSignature" active-sig
            "activeParameter" active-param))

;;; make-signature-info : String × String × (List ParameterInfo) → JsonObject
;;; Create a SignatureInformation object.
;;; label: full signature text (e.g., "(map f lst)")
;;; doc: markdown documentation
;;; params: list of parameter infos
(define (make-signature-info label doc params)
  (json-obj "label" label
            "documentation" (json-obj "kind" "markdown" "value" doc)
            "parameters" (apply json-arr params)))

;;; make-parameter-info : String × String → JsonObject
;;; Create a ParameterInformation object.
;;; label: parameter label (substring of signature or [start, end] offsets)
;;; doc: parameter documentation
(define (make-parameter-info label doc)
  (json-obj "label" label
            "documentation" doc))

;;; ====
;;; Completion Items
;;; ====

;;; make-completion-item : String × Int [× String × String] → JsonObject
;;; Create an LSP CompletionItem.
(define (make-completion-item label kind . opts)
  (let* ([detail (if (pair? opts) (car opts) #f)]
         [doc (if (and (pair? opts) (pair? (cdr opts))) (cadr opts) #f)]
         [base (json-obj "label" label "kind" kind)]
         [with-detail (if detail
                          (cons 'json-object (cons (cons "detail" detail) (cdr base)))
                          base)]
         [with-doc (if doc
                       (cons 'json-object
                             (cons (cons "documentation"
                                         (json-obj "kind" "markdown" "value" doc))
                                   (cdr with-detail)))
                       with-detail)])
        with-doc))

;;; ====
;;; URI Utilities
;;; ====

;;; path->uri : String → String
;;; Convert a file path to a file:// URI.
(define (path->uri path)
  (if (string-prefix? path "file://")
      path
      (string-append "file://"
                     (if (string-prefix? path "/")
                         path
                         (string-append "/" path)))))

;;; uri->path : String → String
;;; Convert a file:// URI to a path.
(define (uri->path uri)
  (if (string-prefix? uri "file://")
      (substring uri 7 (string-length uri))
      uri))
