;;; boundary/meta/xref-io.ss — I/O orchestrator for cross-reference cache
;;; @module xref-io
;;; @requires file-io xref
;;;
;;; Reads source files from disk and populates the lattice xref cache.
;;; Pure analysis stays in lattice/meta/xref.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/xref.ss")

;;; ====
;;; I/O Orchestration
;;; ====

;;; extract-xrefs-from-file : String -> (List (Symbol . (List Symbol)))
;;; Parse a file and extract (definer . callees) pairs.
(define (extract-xrefs-from-file path)
  (guard (e [else '()])
    (let ([sexps (read-all-sexps path)])
      (if (or (not sexps) (null? sexps))
          '()
          (let loop ([sexps sexps] [results '()])
            (if (null? sexps)
                results
                (let ([xref (extract-xref-from-toplevel (car sexps))])
                  (loop (cdr sexps)
                        (if xref (cons xref results) results)))))))))

;;; build-xref-cache! : -> Void
;;; Build cross-reference indices from source files.
(define (build-xref-cache!)
  (printf "Building cross-reference cache...\n")
  (let ([all-xrefs '()])
    ;; Collect from lattice/
    (for-each
     (lambda (path)
       (set! all-xrefs (append (extract-xrefs-from-file path) all-xrefs)))
     (find-scheme-files "lattice"))
    ;; Collect from core/
    (for-each
     (lambda (path)
       (set! all-xrefs (append (extract-xrefs-from-file path) all-xrefs)))
     (find-scheme-files "core"))
    ;; Populate the pure cache
    (populate-xref-cache! all-xrefs)
    (printf "  Indexed ~a definitions, ~a call edges\n"
            (hashtable-size *xref-known-defs*)
            (hashtable-size *xref-callers*))))
