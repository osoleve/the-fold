(load "core/base/prelude.ss")

(doc 'module 'lsp/state)
(doc 'description "Documents and manages all global mutable state in the LSP system. Provides centralized documentation of all global state, reset functions for testing, and state accessors.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude))

(doc 'note "Global State Registry (by module):")
(doc 'note "documents.ss: *documents* (Hashtable String → Document) - in-memory document store")
(doc 'note "capabilities.ss: *pretty-available*, *index-available*, *infer-available* (Boolean) - feature flags")
(doc 'note "lsp-transport.ss: *lsp-stdin*, *lsp-stdout*, *lsp-stderr* (ports), *lsp-running* (Boolean), *progress-token-counter* (Int)")
(doc 'note "lsp-server.ss: *server-initialized*, *server-shutdown-requested* (Boolean), *client-capabilities* (JsonObject), *root-uri* (String)")

(doc 'section 'state-reset)

(doc lsp-reset-documents! 'type '(-> Void))
(doc lsp-reset-documents! 'description "Clear the document store. Requires documents.ss to be loaded.")
(define (lsp-reset-documents!)
  (when (top-level-bound? '*documents*)
        (hashtable-clear! *documents*)))

(doc lsp-reset-server-state! 'type '(-> Void))
(doc lsp-reset-server-state! 'description "Reset server state to initial values. Requires lsp-server.ss to be loaded.")
(define (lsp-reset-server-state!)
  (when (top-level-bound? '*server-initialized*)
        (set! *server-initialized* #f))
  (when (top-level-bound? '*server-shutdown-requested*)
        (set! *server-shutdown-requested* #f))
  (when (top-level-bound? '*client-capabilities*)
        (set! *client-capabilities* #f))
  (when (top-level-bound? '*root-uri*)
        (set! *root-uri* #f)))

(doc lsp-reset-transport-state! 'type '(-> Void))
(doc lsp-reset-transport-state! 'description "Reset transport state (except ports). Requires lsp-transport.ss to be loaded.")
(define (lsp-reset-transport-state!)
  (when (top-level-bound? '*lsp-running*)
        (set! *lsp-running* #f))
  (when (top-level-bound? '*progress-token-counter*)
        (set! *progress-token-counter* 0)))

(doc lsp-reset-all-state! 'type '(-> Void))
(doc lsp-reset-all-state! 'description "Reset all mutable state for testing")
(define (lsp-reset-all-state!)
  (lsp-reset-documents!)
  (lsp-reset-server-state!)
  (lsp-reset-transport-state!))

(doc 'section 'state-queries)

(doc lsp-document-count 'type '(-> Int))
(doc lsp-document-count 'description "Return number of open documents")
(define (lsp-document-count)
  (if (top-level-bound? '*documents*)
      (hashtable-size *documents*)
      0))

(doc lsp-server-ready? 'type '(-> Boolean))
(doc lsp-server-ready? 'description "Check if server is initialized and not shutting down")
(define (lsp-server-ready?)
  (and (top-level-bound? '*server-initialized*)
       *server-initialized*
       (or (not (top-level-bound? '*server-shutdown-requested*))
           (not *server-shutdown-requested*))))

(doc lsp-feature-flags 'type '(-> (Alist Symbol Boolean)))
(doc lsp-feature-flags 'description "Return status of optional feature flags")
(define (lsp-feature-flags)
  (list (cons 'pretty (and (top-level-bound? '*pretty-available*) *pretty-available*))
        (cons 'index (and (top-level-bound? '*index-available*) *index-available*))
        (cons 'infer (and (top-level-bound? '*infer-available*) *infer-available*))))

(doc 'section 'state-summary)

(doc lsp-state-summary 'type '(-> String))
(doc lsp-state-summary 'description "Return a human-readable summary of LSP state")
(define (lsp-state-summary)
  (string-append
   "LSP State Summary:\n"
   "  Documents: " (number->string (lsp-document-count)) "\n"
   "  Server ready: " (if (lsp-server-ready?) "yes" "no") "\n"
   "  Features: "
   (let ([flags (lsp-feature-flags)])
        (string-append
         (if (cdr (assq 'pretty flags)) "pretty " "")
         (if (cdr (assq 'index flags)) "index " "")
         (if (cdr (assq 'infer flags)) "infer " "")))
   "\n"))
