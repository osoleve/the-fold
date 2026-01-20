(doc 'module 'loader)
(doc 'description "Tracked loading with smart reload for interactive development")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Does NOT shadow Chez's load - use load! explicitly for tracking")

(doc 'section 'state)

(doc *loaded-files* 'type "(List String)")
(doc *loaded-files* 'description "Files loaded via load!, in load order. No duplicates")
(define *loaded-files* '())

(doc 'section 'core-implementation)

(doc normalize-path 'type "String -> String")
(doc normalize-path 'description "Normalize a file path for consistent tracking")
(define (normalize-path path)
  path)

(doc track-file! 'type "String -> Void")
(doc track-file! 'description "Add file to tracking (at end if not present, or move to end if present)")
(define (track-file! path)
  (let ([norm (normalize-path path)])
    (set! *loaded-files*
          (filter (lambda (f) (not (string=? f norm)))
                  *loaded-files*))
    (set! *loaded-files*
          (append *loaded-files* (list norm)))))

(doc load-with-error-handling 'type "String -> Any")
(doc load-with-error-handling 'description "Load a file with graceful error handling. Returns the result or prints error and returns #f")
(define (load-with-error-handling path)
  (guard (e [else
             (display (format "Error loading ~a: ~a\n" path
                              (if (condition? e)
                                  (condition-message e)
                                  e)))
             #f])
    (load path)))

(doc 'section 'public-api)

(doc load! 'type "[String] -> Any")
(doc load! 'description "Load and track a file, or reload all tracked files")
(doc load! 'example "(load! \"path\")  - Load file and track it for later reload")
(doc load! 'example "(load!)         - Reload all tracked files in order")
(define load!
  (case-lambda
    [()
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
     (let ([result (load-with-error-handling path)])
       (when result
         (track-file! path))
       result)]))

(doc loaded 'type "-> Void")
(doc loaded 'description "Show currently tracked files")
(define (loaded)
  (if (null? *loaded-files*)
      (display "No files tracked.\n")
      (begin
        (display "Tracked files (in load order):\n")
        (for-each
         (lambda (path)
           (display (format "  ~a\n" path)))
         *loaded-files*))))

(doc unload! 'type "String -> Void")
(doc unload! 'description "Remove a file from tracking (does not undefine its bindings)")
(define (unload! path)
  (let ([norm (normalize-path path)])
    (set! *loaded-files*
          (filter (lambda (f) (not (string=? f norm)))
                  *loaded-files*))
    (display (format "Untracked: ~a\n" norm))))

(doc clear-loaded! 'type "-> Void")
(doc clear-loaded! 'description "Clear all tracked files")
(define (clear-loaded!)
  (set! *loaded-files* '())
  (display "Cleared all tracked files.\n"))

(doc 'section 'startup-banner)

(unless (and (top-level-bound? '*quiet*) *quiet*)
  (display "Tracked loader ready.\n")
  (display "  (load! \"path\")  - Load and track\n")
  (display "  (load!)         - Reload all tracked\n")
  (display "  (loaded)        - Show tracked files\n"))
