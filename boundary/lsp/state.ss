(load "core/base/prelude.ss")

(doc 'module 'lsp/state)
(doc 'description "Documents and manages all global mutable state in the LSP system. Uses explicit registration instead of top-level-bound? checks for reliable, order-independent behavior.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude))

(doc 'note "Global State Registry (by module):")
(doc 'note "documents.ss: *documents* (Hashtable String → Document) - in-memory document store")
(doc 'note "capabilities.ss: *pretty-available*, *index-available*, *infer-available* (Boolean) - feature flags")
(doc 'note "lsp-transport.ss: *lsp-stdin*, *lsp-stdout*, *lsp-stderr* (ports), *lsp-running* (Boolean), *progress-token-counter* (Int)")
(doc 'note "lsp-server.ss: *server-initialized*, *server-shutdown-requested* (Boolean), *client-capabilities* (JsonObject), *root-uri* (String)")

;;; ============================================================
;;; State Registry
;;; ============================================================
;;; Modules register their state explicitly when they load.
;;; This avoids fragile top-level-bound? checks.
;;; Guard against double-loading resetting the registry.

(doc 'section 'registry)

(doc *lsp-state-registry* 'type '(Hashtable Symbol (List Symbol (-> Void))))
(doc *lsp-state-registry* 'description "Registry mapping module names to (state-vars reset-thunk) pairs")
(define *lsp-state-registry*
  (if (top-level-bound? '*lsp-state-registry*)
      *lsp-state-registry*  ; Keep existing registry
      (make-eq-hashtable))) ; Create new one

(doc lsp-register-state! 'type '(-> Symbol (List Symbol) (-> Void) Void))
(doc lsp-register-state! 'description "Register a module's state variables and reset function")
(define (lsp-register-state! module-name state-vars reset-thunk)
  (hashtable-set! *lsp-state-registry* module-name (list state-vars reset-thunk)))

(doc lsp-registered-modules 'type '(-> (List Symbol)))
(doc lsp-registered-modules 'description "List all modules that have registered state")
(define (lsp-registered-modules)
  (vector->list (hashtable-keys *lsp-state-registry*)))

(doc lsp-module-registered? 'type '(-> Symbol Boolean))
(doc lsp-module-registered? 'description "Check if a module has registered its state")
(define (lsp-module-registered? module-name)
  (hashtable-contains? *lsp-state-registry* module-name))

;;; ============================================================
;;; State Reset
;;; ============================================================

(doc 'section 'state-reset)

(doc lsp-reset-module-state! 'type '(-> Symbol Void))
(doc lsp-reset-module-state! 'description "Reset state for a specific registered module")
(define (lsp-reset-module-state! module-name)
  (let ([entry (hashtable-ref *lsp-state-registry* module-name #f)])
       (when entry
             (let ([reset-thunk (cadr entry)])
                  (reset-thunk)))))

(doc lsp-reset-all-state! 'type '(-> Void))
(doc lsp-reset-all-state! 'description "Reset all registered mutable state for testing")
(define (lsp-reset-all-state!)
  (let ([modules (vector->list (hashtable-keys *lsp-state-registry*))])
       (for-each lsp-reset-module-state! modules)))

;;; Legacy compatibility - these call into the registry
(doc lsp-reset-documents! 'type '(-> Void))
(doc lsp-reset-documents! 'description "Clear the document store (legacy, use lsp-reset-module-state! 'documents)")
(define (lsp-reset-documents!)
  (lsp-reset-module-state! 'documents))

(doc lsp-reset-server-state! 'type '(-> Void))
(doc lsp-reset-server-state! 'description "Reset server state (legacy, use lsp-reset-module-state! 'server)")
(define (lsp-reset-server-state!)
  (lsp-reset-module-state! 'server))

(doc lsp-reset-transport-state! 'type '(-> Void))
(doc lsp-reset-transport-state! 'description "Reset transport state (legacy, use lsp-reset-module-state! 'transport)")
(define (lsp-reset-transport-state!)
  (lsp-reset-module-state! 'transport))

;;; ============================================================
;;; State Queries
;;; ============================================================

(doc 'section 'state-queries)

(doc lsp-document-count 'type '(-> Int))
(doc lsp-document-count 'description "Return number of open documents")
(define (lsp-document-count)
  (if (lsp-module-registered? 'documents)
      (hashtable-size *documents*)
      0))

(doc lsp-server-ready? 'type '(-> Boolean))
(doc lsp-server-ready? 'description "Check if server is initialized and not shutting down")
(define (lsp-server-ready?)
  (and (lsp-module-registered? 'server)
       *server-initialized*
       (not *server-shutdown-requested*)))

(doc lsp-feature-flags 'type '(-> (Alist Symbol Boolean)))
(doc lsp-feature-flags 'description "Return status of optional feature flags")
(define (lsp-feature-flags)
  (if (lsp-module-registered? 'capabilities)
      (list (cons 'pretty *pretty-available*)
            (cons 'index *index-available*)
            (cons 'infer *infer-available*))
      (list (cons 'pretty #f)
            (cons 'index #f)
            (cons 'infer #f))))

;;; ============================================================
;;; State Summary
;;; ============================================================

(doc 'section 'state-summary)

(doc lsp-state-summary 'type '(-> String))
(doc lsp-state-summary 'description "Return a human-readable summary of LSP state")
(define (lsp-state-summary)
  (string-append
   "LSP State Summary:\n"
   "  Registered modules: " (format "~a" (lsp-registered-modules)) "\n"
   "  Documents: " (number->string (lsp-document-count)) "\n"
   "  Server ready: " (if (lsp-server-ready?) "yes" "no") "\n"
   "  Features: "
   (let ([flags (lsp-feature-flags)])
        (string-append
         (if (cdr (assq 'pretty flags)) "pretty " "")
         (if (cdr (assq 'index flags)) "index " "")
         (if (cdr (assq 'infer flags)) "infer " "")))
   "\n"))
