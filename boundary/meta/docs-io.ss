;;; boundary/meta/docs-io.ss — I/O orchestrator for doc form index
;;; @module docs-io
;;; @requires file-io docs
;;;
;;; Reads source files from disk and populates the lattice doc index.
;;; Pure parsing stays in lattice/meta/docs.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/docs.ss")

;;; ====
;;; I/O Orchestration
;;; ====

;;; extract-docs-from-file : String -> (List (file line tag content target?))
;;; Extract all doc forms from a source file.
(define (extract-docs-from-file path)
  (let ([sexps (read-all-sexps path)])
    (if (or (not sexps) (null? sexps))
        '()
        (let loop ([sexps sexps] [line 1] [acc '()])
          (if (null? sexps)
              acc
              (let* ([sexp (car sexps)]
                     [docs (extract-docs-from-sexp sexp line)]
                     [tagged (map (lambda (d) (cons path d)) docs)])
                (loop (cdr sexps)
                      (+ line 1)  ; Approximate - real line tracking would need reader
                      (append acc tagged))))))))

;;; build-doc-index! : -> Void
;;; Build the doc index from the codebase.
(define (build-doc-index!)
  (let ([roots '("core" "lattice" "boundary")]
        [acc '()])
    (for-each
     (lambda (root)
       (let ([files (find-scheme-files root)])
         (for-each
          (lambda (file)
            (let ([docs (extract-docs-from-file file)])
              (set! acc (append docs acc))))
          files)))
     roots)
    (populate-doc-index! (reverse acc)))
  (unless *docs-quiet*
    (printf "Doc index built: ~a entries~n" (length *doc-index*))))

;;; ensure-doc-index! : -> Void
;;; Build index if not yet built (handles empty index correctly).
(define (ensure-doc-index!)
  (unless *doc-index-built?*
    (build-doc-index!)))

;;; doc-reindex! : -> Void
;;; Force full rebuild of the doc index.
(define (doc-reindex!)
  (set! *doc-index-built?* #f)
  (build-doc-index!))
