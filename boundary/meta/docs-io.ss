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

;;; extract-header-description : String -> (Option String)
;;; Extract @description from a file's header annotations.
;;; Returns the description string or #f if not found.
(define (extract-header-description path)
  (let ([meta (guard (exn [else #f]) (parse-module-metadata path))])
    (and meta (cdr (assq 'description meta)))))

;;; extract-docs-from-file : String -> (List (file line tag content target?))
;;; Extract all doc forms from a source file.
;;; When an @description header annotation exists, it replaces any
;;; module-level (doc 'description ...) form to prevent drift.
(define (extract-docs-from-file path)
  (let ([sexps (read-all-sexps path)]
        [header-desc (extract-header-description path)])
    (if (or (not sexps) (null? sexps))
        ;; Even with no sexps, inject @description if present
        (if header-desc
            (list (list path 0 'description (list header-desc) #f))
            '())
        (let loop ([sexps sexps] [line 1] [acc '()])
          (if (null? sexps)
              (if header-desc
                  ;; Prepend @description, filtering out the module-level (doc 'description ...)
                  ;; Only remove untargeted descriptions in the first ~15 top-level forms
                  ;; (where module-level docs live) to preserve function-level descriptions
                  (cons (list path 0 'description (list header-desc) #f)
                        (filter (lambda (entry)
                                  (not (and (eq? (caddr entry) 'description)
                                            (not (list-ref entry 4))
                                            (< (cadr entry) 15))))
                                acc))
                  acc)
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
