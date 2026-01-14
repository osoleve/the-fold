;;; core/lsp/state.ss — LSP Global State Management
;;; @module state
;;; @requires prelude
;;;
;;; Documents and manages all global mutable state in the LSP system.
;;; This module provides:
;;;   - Centralized documentation of all global state
;;;   - Reset functions for testing
;;;   - State accessors where appropriate
;;;
;;; Global State Registry (by module):
;;;
;;; core/lsp/documents.ss:
;;;   *documents*    : Hashtable String → Document
;;;                    The in-memory document store
;;;
;;; core/lsp/capabilities.ss:
;;;   *pretty-available*  : Boolean - pretty.ss loaded?
;;;   *index-available*   : Boolean - index.ss loaded?
;;;   *infer-available*   : Boolean - infer.ss loaded?
;;;
;;; shell/lsp/lsp-transport.ss:
;;;   *lsp-stdin*   : BinaryInputPort  - captured at load time
;;;   *lsp-stdout*  : BinaryOutputPort - captured at load time
;;;   *lsp-stderr*  : TextOutputPort   - error logging
;;;   *lsp-running* : Boolean          - transport active?
;;;   *progress-token-counter* : Int   - progress token generator
;;;
;;; shell/lsp/lsp-server.ss:
;;;   *server-initialized*        : Boolean   - past initialize?
;;;   *server-shutdown-requested* : Boolean   - shutdown pending?
;;;   *client-capabilities*       : JsonObject - client caps
;;;   *root-uri*                  : String    - workspace root
;;;
;;; This is Core code: pure functions for state management.

(load "core/base/prelude.ss")

;;; ====
;;; State Reset (for testing)
;;; ====

;;; lsp-reset-documents! : → Void
;;; Clear the document store.
;;; Requires documents.ss to be loaded.
(define (lsp-reset-documents!)
  (when (top-level-bound? '*documents*)
        (hashtable-clear! *documents*)))

;;; lsp-reset-server-state! : → Void
;;; Reset server state to initial values.
;;; Requires lsp-server.ss to be loaded.
(define (lsp-reset-server-state!)
  (when (top-level-bound? '*server-initialized*)
        (set! *server-initialized* #f))
  (when (top-level-bound? '*server-shutdown-requested*)
        (set! *server-shutdown-requested* #f))
  (when (top-level-bound? '*client-capabilities*)
        (set! *client-capabilities* #f))
  (when (top-level-bound? '*root-uri*)
        (set! *root-uri* #f)))

;;; lsp-reset-transport-state! : → Void
;;; Reset transport state (except ports).
;;; Requires lsp-transport.ss to be loaded.
(define (lsp-reset-transport-state!)
  (when (top-level-bound? '*lsp-running*)
        (set! *lsp-running* #f))
  (when (top-level-bound? '*progress-token-counter*)
        (set! *progress-token-counter* 0)))

;;; lsp-reset-all-state! : → Void
;;; Reset all mutable state for testing.
(define (lsp-reset-all-state!)
  (lsp-reset-documents!)
  (lsp-reset-server-state!)
  (lsp-reset-transport-state!))

;;; ====
;;; State Queries
;;; ====

;;; lsp-document-count : → Int
;;; Return number of open documents.
(define (lsp-document-count)
  (if (top-level-bound? '*documents*)
      (hashtable-size *documents*)
      0))

;;; lsp-server-ready? : → Boolean
;;; Check if server is initialized and not shutting down.
(define (lsp-server-ready?)
  (and (top-level-bound? '*server-initialized*)
       *server-initialized*
       (or (not (top-level-bound? '*server-shutdown-requested*))
           (not *server-shutdown-requested*))))

;;; lsp-feature-flags : → (Alist Symbol Boolean)
;;; Return status of optional feature flags.
(define (lsp-feature-flags)
  (list (cons 'pretty (and (top-level-bound? '*pretty-available*) *pretty-available*))
        (cons 'index (and (top-level-bound? '*index-available*) *index-available*))
        (cons 'infer (and (top-level-bound? '*infer-available*) *infer-available*))))

;;; ====
;;; State Summary (for debugging)
;;; ====

;;; lsp-state-summary : → String
;;; Return a human-readable summary of LSP state.
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
