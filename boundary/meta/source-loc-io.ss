;;; boundary/meta/source-loc-io.ss — I/O orchestrator for source location cache
;;; @module source-loc-io
;;; @requires file-io source-loc
;;;
;;; Reads source files from disk and populates the lattice source-loc cache.
;;; Pure parsing stays in lattice/meta/source-loc.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/source-loc.ss")

;;; ====
;;; I/O Orchestration
;;; ====

;;; extract-definitions-from-file : String -> (List (Symbol File Line))
;;; Read a file and extract all definitions with line numbers.
(define (extract-definitions-from-file path)
  (let ([lines (read-file-lines path)])
       (if (or (not lines) (null? lines))
           '()
           (parse-definitions-with-lines path lines))))

;;; build-source-location-cache! : -> Void
;;; Build the global source location cache from all lattice and core files.
;;; Skips if cache is already populated.
(define (build-source-location-cache!)
  (when (zero? (source-location-count))
        (build-source-location-cache-fresh!)))

;;; build-source-location-cache-fresh! : -> Void
;;; Force rebuild of source location cache.
(define (build-source-location-cache-fresh!)
  (printf "Building source location cache...\n")
  ;; Reset the cache
  (set! *source-locations* (make-hashtable symbol-hash eq?))
  (let ([count 0])
       ;; Scan lattice/
       (for-each
        (lambda (path)
                (let ([defs (extract-definitions-from-file path)])
                     (populate-source-locations! defs)
                     (set! count (+ count (length defs)))))
        (find-scheme-files "lattice"))
       ;; Also scan core/
       (for-each
        (lambda (path)
                (let ([defs (extract-definitions-from-file path)])
                     (populate-source-locations! defs)
                     (set! count (+ count (length defs)))))
        (find-scheme-files "core"))
       (printf "  Indexed ~a definitions\n" count)))
