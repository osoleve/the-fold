(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")

(doc 'module 'lsp/protocol)
(doc 'description "Defines LSP protocol structures: JSON-RPC message types (request, response, notification), LSP method names and constants, error codes, and server capabilities")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'requires '(prelude json))

(doc 'section 'json-rpc-message-types)

(doc 'note "Message classification: A request has (id, method, params); A response has (id, result OR error); A notification has (method, params) but no id")

(doc lsp-request? 'type '(-> JsonObject Boolean))
(doc lsp-request? 'description "Check if message is a JSON-RPC request")
(doc lsp-request? 'note "Use assoc to check key presence, not json-get. json-get returns #f for both 'key missing' and 'value is #f', which would incorrectly reject id:0 or id:false")
(define (lsp-request? msg)
  (and (json-object? msg)
       (assoc "id" (cdr msg))       ; Key exists (any value including 0, null)
       (json-get msg "method")))

(doc lsp-notification? 'type '(-> JsonObject Boolean))
(doc lsp-notification? 'description "Check if message is a JSON-RPC notification")
(doc lsp-notification? 'note "Use assoc to check key absence")
(define (lsp-notification? msg)
  (and (json-object? msg)
       (not (assoc "id" (cdr msg)))
       (json-get msg "method")))

(doc lsp-response? 'type '(-> JsonObject Boolean))
(doc lsp-response? 'description "Check if message is a JSON-RPC response")
(doc lsp-response? 'note "Use assoc for id check (consistent with lsp-request?)")
(define (lsp-response? msg)
  (and (json-object? msg)
       (assoc "id" (cdr msg))
       (or (assoc "result" (cdr msg))
           (json-get msg "error"))))

(doc 'section 'message-accessors)

;;; lsp-message-id : JsonObject → Any
(define (lsp-message-id msg)
  (json-get msg "id"))

;;; lsp-message-method : JsonObject → String
(define (lsp-message-method msg)
  (json-get msg "method"))

;;; lsp-message-params : JsonObject → JsonValue
(define (lsp-message-params msg)
  (or (json-get msg "params") (json-obj)))

(doc 'section 'response-construction)

(doc make-response 'type '(-> Id JsonValue JsonObject))
(doc make-response 'description "Construct a JSON-RPC success response")
(define (make-response id result)
  (json-obj "jsonrpc" "2.0"
            "id" id
            "result" result))

(doc make-error-response 'type '(-> Id Int String (* JsonValue) JsonObject))
(doc make-error-response 'description "Construct a JSON-RPC error response")
(define (make-error-response id code message . data)
  (let ([error-obj (if (null? data)
                       (json-obj "code" code "message" message)
                       (json-obj "code" code "message" message "data" (car data)))])
       (json-obj "jsonrpc" "2.0"
                 "id" id
                 "error" error-obj)))

(doc make-notification 'type '(-> String JsonValue JsonObject))
(doc make-notification 'description "Construct a JSON-RPC notification")
(define (make-notification method params)
  (json-obj "jsonrpc" "2.0"
            "method" method
            "params" params))

(doc 'section 'lsp-error-codes)

(doc 'note "JSON-RPC reserved errors")
(define *error-parse-error* -32700)
(define *error-invalid-request* -32600)
(define *error-method-not-found* -32601)
(define *error-invalid-params* -32602)
(define *error-internal-error* -32603)

(doc 'note "LSP reserved errors")
(define *error-server-not-initialized* -32002)
(define *error-unknown-error-code* -32001)
(define *error-request-failed* -32803)
(define *error-server-cancelled* -32802)
(define *error-content-modified* -32801)
(define *error-request-cancelled* -32800)

(doc 'section 'lsp-method-names)

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

(doc 'section 'server-capabilities)

(doc 'note "TextDocumentSyncKind constants")
(define *sync-none* 0)
(define *sync-full* 1)
(define *sync-incremental* 2)

(doc 'note "CompletionTriggerKind constants")
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

(doc fold-server-capabilities 'type '(-> JsonObject))
(doc fold-server-capabilities 'description "Returns the server capabilities for the initialize response")
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

(doc fold-server-info 'type '(-> JsonObject))
(doc fold-server-info 'description "Returns server info for the initialize response")
(define (fold-server-info)
  (json-obj "name" "fold-lsp"
            "version" "0.1.0"))

(doc 'section 'position-and-range-types)

(doc make-position 'type '(-> Nat Nat JsonObject))
(doc make-position 'description "Create an LSP Position (0-indexed line and character)")
(define (make-position line character)
  (json-obj "line" line "character" character))

(doc make-range 'type '(-> Position Position JsonObject))
(doc make-range 'description "Create an LSP Range")
(define (make-range start end)
  (json-obj "start" start "end" end))

(doc make-location 'type '(-> String Range JsonObject))
(doc make-location 'description "Create an LSP Location")
(define (make-location uri range)
  (json-obj "uri" uri "range" range))

(doc 'section 'diagnostic-construction)

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

(doc 'section 'hover-content)

(doc make-hover 'type '(-> String (* Range) JsonObject))
(doc make-hover 'description "Create an LSP Hover response")
(define (make-hover contents . range)
  (let ([base (json-obj "contents" (json-obj "kind" "markdown"
                                             "value" contents))])
       (if (and (pair? range) (car range))
           (cons 'json-object (cons (cons "range" (car range)) (cdr base)))
           base)))

(doc 'section 'signature-help)

(doc make-signature-help 'type '(-> (List SignatureInfo) Int Int JsonObject))
(doc make-signature-help 'description "Create an LSP SignatureHelp response")
(doc make-signature-help 'param 'signatures "list of signature infos")
(doc make-signature-help 'param 'activeSignature "index of active signature (0-indexed)")
(doc make-signature-help 'param 'activeParameter "index of active parameter (0-indexed)")
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

(doc 'section 'completion-items)

(doc make-completion-item 'type '(-> String Int (* String) JsonObject))
(doc make-completion-item 'description "Create an LSP CompletionItem")
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

(doc 'section 'uri-utilities)

(doc path->uri 'type '(-> String String))
(doc path->uri 'description "Convert a file path to a file:// URI")
(define (path->uri path)
  (if (string-prefix? path "file://")
      path
      (string-append "file://"
                     (if (string-prefix? path "/")
                         path
                         (string-append "/" path)))))

(doc uri->path 'type '(-> String String))
(doc uri->path 'description "Convert a file:// URI to a path. Handles URL-encoded characters (%20 for space, etc.)")
(define (uri->path uri)
  (if (string-prefix? uri "file://")
      (url-decode (substring uri 7 (string-length uri)))
      uri))

(doc url-decode 'type '(-> String String))
(doc url-decode 'description "Decode percent-encoded characters in a URL path (%20 → space, %23 → #, etc.)")
(define (url-decode str)
  (let ([len (string-length str)])
       (let loop ([i 0] [chars '()])
            (cond
             [(>= i len)
              (list->string (reverse chars))]
             [(and (char=? (string-ref str i) #\%)
                   (< (+ i 2) len))
              ;; Found %, try to decode next two hex digits
              (let ([hex (substring str (+ i 1) (+ i 3))])
                   (let ([code (string->number hex 16)])
                        (if code
                            (loop (+ i 3) (cons (integer->char code) chars))
                            ;; Invalid hex, keep literal %
                            (loop (+ i 1) (cons #\% chars)))))]
             [else
              (loop (+ i 1) (cons (string-ref str i) chars))]))))
