;;; boundary/repl/loader.ss — Tracked loading with smart reload
;;;
;;; Provides load! for interactive development:
;;;   (load! "path")  - Load file and track it
;;;   (load!)         - Reload all tracked files in order
;;;
;;; Does NOT shadow Chez's load - use load! explicitly for tracking.

;;; ====
;;; State
;;; ====

;;; *loaded-files* : (List String)
;;; Files loaded via load!, in load order. No duplicates.
(define *loaded-files* '())

;;; ====
;;; Core Implementation
;;; ====

;;; normalize-path : String -> String
;;; Normalize a file path for consistent tracking.
(define (normalize-path path)
  ;; For now, just use the path as-is
  ;; Could expand to absolute path if needed
  path)

;;; track-file! : String -> Void
;;; Add file to tracking (at end if not present, or move to end if present).
(define (track-file! path)
  (let ([norm (normalize-path path)])
    ;; Remove if already present (we'll re-add at end)
    (set! *loaded-files*
          (filter (lambda (f) (not (string=? f norm)))
                  *loaded-files*))
    ;; Add at end
    (set! *loaded-files*
          (append *loaded-files* (list norm)))))

;;; load-with-error-handling : String -> Any
;;; Load a file with graceful error handling.
;;; Returns the result or prints error and returns #f.
(define (load-with-error-handling path)
  (guard (e [else
             (display (format "Error loading ~a: ~a\n" path
                              (if (condition? e)
                                  (condition-message e)
                                  e)))
             #f])
    (load path)))

;;; ====
;;; Public API
;;; ====

;;; load! : [String] -> Any
;;; Load and track a file, or reload all tracked files.
;;;
;;; (load! "path")  - Load file and track it for later reload
;;; (load!)         - Reload all tracked files in order
(define load!
  (case-lambda
    [()
     ;; Reload all tracked files
     (if (null? *loaded-files*)
         (display "No files tracked. Use (load! \"path\") first.\n")
         (begin
           (display (format "Reloading ~a files...\n" (length *loaded-files*)))
           (for-each
            (lambda (path)
              (display (format "  ~a\n" path))
              (load-with-error-handling path))
            *loaded-files*)
           (display "Done.\n")))]
    [(path)
     ;; Load and track
     (let ([result (load-with-error-handling path)])
       (when result
         (track-file! path))
       result)]))

;;; loaded : -> Void
;;; Show currently tracked files.
(define (loaded)
  (if (null? *loaded-files*)
      (display "No files tracked.\n")
      (begin
        (display "Tracked files (in load order):\n")
        (for-each
         (lambda (path)
           (display (format "  ~a\n" path)))
         *loaded-files*))))

;;; unload! : String -> Void
;;; Remove a file from tracking (does not undefine its bindings).
(define (unload! path)
  (let ([norm (normalize-path path)])
    (set! *loaded-files*
          (filter (lambda (f) (not (string=? f norm)))
                  *loaded-files*))
    (display (format "Untracked: ~a\n" norm))))

;;; clear-loaded! : -> Void
;;; Clear all tracked files.
(define (clear-loaded!)
  (set! *loaded-files* '())
  (display "Cleared all tracked files.\n"))

;;; ====
;;; Loader Banner (respects *quiet* mode)
;;; ====

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Tracked loader ready.\n")
  (display "  (load! \"path\")  - Load and track\n")
  (display "  (load!)         - Reload all tracked\n")
  (display "  (loaded)        - Show tracked files\n"))
