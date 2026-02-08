;;; boundary/meta/docstrings-io.ss — I/O orchestrator for docstring cache
;;; @module docstrings-io
;;; @requires file-io docstrings
;;;
;;; Reads source files from disk and populates the lattice docstring cache.
;;; Pure parsing stays in lattice/meta/docstrings.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/docstrings.ss")

;;; ====
;;; I/O Orchestration
;;; ====

;;; extract-docstrings-from-file : String -> (List (Pair Symbol String))
;;; Read a file and extract docstrings for defined functions.
(define (extract-docstrings-from-file path)
  (let ([lines (read-file-lines path)])
       (if (or (not lines) (null? lines))
           '()
           (parse-docstrings lines))))

;;; build-docstring-cache! : -> Void
;;; Build the global docstring cache from all lattice source files.
(define (build-docstring-cache!)
  (printf "Building docstring cache...\n")
  (set! *docstrings* (make-hashtable symbol-hash eq?))
  (let* ([files (find-scheme-files "lattice")]
         [count 0])
        (for-each
         (lambda (path)
                 (let ([docstrings (extract-docstrings-from-file path)])
                      (populate-docstrings! docstrings)
                      (set! count (+ count (length docstrings)))))
         files)
        (printf "  Scanned ~a files, extracted ~a docstrings\n"
                (length files) count)))
