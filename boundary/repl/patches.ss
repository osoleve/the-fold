(load "core/base/prelude.ss")

(doc 'module 'patches)
(doc 'description "The Fold Patch System - loadable packages that extend The Fold like fabric patches, software patches, or synth patches")
(doc 'layer 'boundary)
(doc 'purity 'partial)

(doc 'section 'configuration)

(define *patches-dir* "patches")
(define *applied-patches* '())

(doc 'section 'patch-registry)

(doc scan-patches 'type "-> (List Symbol)")
(doc scan-patches 'description "Scan the patches directory for available patches")
(define (scan-patches)
  (if (file-exists? *patches-dir*)
      (let ([files (directory-list *patches-dir*)])
           (filter-map
            (lambda (f)
                    (let ([name (string->symbol (path-strip-extension f))])
                         (if (string-ends-with? f ".ss")
                             name
                             #f)))
            files))
      '()))

(doc path-strip-extension 'type "String -> String")
(doc path-strip-extension 'description "Remove .ss extension from filename")
(define (path-strip-extension filename)
  (let ([len (string-length filename)])
       (if (and (>= len 3)
                (string=? ".ss" (substring filename (- len 3) len)))
           (substring filename 0 (- len 3))
           filename)))

;;; filter-map provided by prelude

(doc 'section 'input-validation)

(doc valid-patch-name? 'type "Symbol -> Boolean")
(doc valid-patch-name? 'description "Patch names must contain only safe characters (no path separators or ..). Prevents path traversal attacks in read-manifest")
(define (valid-patch-name? name)
  (let* ([s (symbol->string name)]
         [len (string-length s)])
        (and (> len 0)
             (<= len 64)
             (not (string-contains? s "/"))
             (not (string-contains? s "\\"))
             (not (string-contains? s ".."))
             (char-alphabetic? (string-ref s 0))
             (let loop ([i 0])
                  (if (>= i len)
                      #t
                      (let ([c (string-ref s i)])
                           (if (or (char-alphabetic? c)
                                   (char-numeric? c)
                                   (char=? c #\-)
                                   (char=? c #\_))
                               (loop (+ i 1))
                               #f)))))))

(doc 'section 'manifest-handling)

(doc read-manifest 'type "Symbol -> Alist | #f")
(doc read-manifest 'description "Read a patch manifest from the patches directory. Validates patch name to prevent path traversal")
(doc read-manifest 'note "SECURITY: Validates patch name before constructing path")
(define (read-manifest name)
  (unless (valid-patch-name? name)
          (display (format "  WARNING: Invalid patch name rejected: ~s\n" name))
          (error 'read-manifest "Invalid patch name" name))
  (let ([path (string-append *patches-dir* "/" (symbol->string name) ".ss")])
       (if (file-exists? path)
           (guard (e [else #f])
                  (call-with-input-file path
                                        (lambda (p)
                                                (read p))))
           #f)))

(doc manifest-get 'type "Alist Symbol Any -> Any")
(doc manifest-get 'description "Get a field from a manifest with default")
(define (manifest-get manifest key default)
  (let ([pair (assq key manifest)])
       (if pair (cdr pair) default)))

(doc 'section 'patch-loading)

(doc patch-applied? 'type "Symbol -> Bool")
(define (patch-applied? name)
  (memq name *applied-patches*))

(doc apply-patch 'type "Symbol -> Bool")
(doc apply-patch 'description "Load a patch by name. Returns #t on success. Validates patch name to prevent path traversal")
(doc apply-patch 'note "SECURITY: Validates patch name early")
(define (apply-patch name)
  (cond
   [(not (valid-patch-name? name))
    (display (format "  ERROR: Invalid patch name: ~a\n" name))
    (display "  Patch names must be alphanumeric with hyphens/underscores only.\n")
    #f]
   [(patch-applied? name)
    (display (format "  Patch '~a' is already applied.\n" name))
    #t]
   [else
    (let ([manifest (read-manifest name)])
         (if (not manifest)
             (begin
              (display (format "  ERROR: Patch '~a' not found.\n" name))
              (display (format "  Available: ~a\n" (scan-patches)))
              #f)
             (apply-patch-from-manifest name manifest)))]))

(doc apply-patch-from-manifest 'type "Symbol Alist -> Bool")
(define (apply-patch-from-manifest name manifest)
  (let ([requires (manifest-get manifest 'requires '())]
        [files (manifest-get manifest 'files '())]
        [description (manifest-get manifest 'description "No description")])

       (let ([missing (filter (lambda (dep) (not (patch-applied? dep))) requires)])
            (if (not (null? missing))
                (begin
                 (display (format "  Missing dependencies: ~a\n" missing))
                 (display "  Apply them first, or use (apply-patch-recursive 'name)\n")
                 #f)
                (begin
                 (display (format "  Applying patch: ~a\n" name))
                 (display (format "  ~a\n" description))
                 (let ([success (load-patch-files files)])
                      (if success
                          (begin
                           (set! *applied-patches* (cons name *applied-patches*))
                           (let ([provides (manifest-get manifest 'provides '())])
                                (display (format "  ✓ Loaded ~a functions\n" (length provides)))
                                (register-patch-symbols! provides))
                           #t)
                          (begin
                           (display "  ✗ Failed to load patch files\n")
                           #f))))))))

(doc load-patch-files 'type "(List String) -> Bool")
(doc load-patch-files 'description "Load a list of files in order")
(define (load-patch-files files)
  (guard (e [else
             (display (format "  Load error: ~a\n"
                              (if (condition? e) (condition-message e) e)))
             #f])
         (for-each
          (lambda (file)
                  (display (format "    Loading ~a...\n" file))
                  (load file))
          files)
         #t))

(doc apply-patch-recursive 'type "Symbol -> Bool")
(doc apply-patch-recursive 'description "Apply a patch and all its dependencies")
(define (apply-patch-recursive name)
  (let ([manifest (read-manifest name)])
       (if (not manifest)
           (begin
            (display (format "  ERROR: Patch '~a' not found.\n" name))
            #f)
           (let ([requires (manifest-get manifest 'requires '())])
                (let ([deps-ok (fold-left
                                (lambda (ok dep)
                                        (and ok
                                             (or (patch-applied? dep)
                                                 (apply-patch-recursive dep))))
                                #t
                                requires)])
                     (if deps-ok
                         (apply-patch name)
                         #f))))))

(doc 'section 'namespace-integration)

(doc register-patch-symbols! 'type "(List Symbol) -> void")
(doc register-patch-symbols! 'description "Add patch-provided symbols to the global whitelist")
(define (register-patch-symbols! symbols)
  (when (top-level-bound? 'add-global!)
        (for-each add-global! symbols)))

(doc 'section 'user-interface)

(doc patches 'type "-> void")
(doc patches 'description "List all available patches with status")
(define (patches)
  (display "\n")
  (display "╔══════════════════════════════════════════════════════════════╗\n")
  (display "║                    AVAILABLE PATCHES                        ║\n")
  (display "╚══════════════════════════════════════════════════════════════╝\n\n")

  (let ([available (scan-patches)])
       (if (null? available)
           (display "  No patches found in patches/ directory.\n")
           (for-each
            (lambda (name)
                    (let* ([manifest (read-manifest name)]
                           [applied? (patch-applied? name)]
                           [status (if applied? "✓" " ")]
                           [desc (if manifest
                                     (manifest-get manifest 'description "")
                                     "(manifest error)")])
                          (display (format "  [~a] ~a\n" status name))
                          (display (format "      ~a\n" desc))))
            available)))

  (display "\n")
  (display "  Use (apply-patch 'name) to load a patch.\n")
  (display "  Use (apply-patch-recursive 'name) to load with dependencies.\n")
  (display "  Use (patch-info 'name) for details.\n\n"))

(doc applied-patches 'type "-> (List Symbol)")
(doc applied-patches 'description "Return list of currently applied patches")
(define (applied-patches)
  *applied-patches*)

(doc patch-info 'type "Symbol -> void")
(doc patch-info 'description "Display detailed information about a patch")
(define (patch-info name)
  (let ([manifest (read-manifest name)])
       (if (not manifest)
           (display (format "  Patch '~a' not found.\n" name))
           (begin
            (display "\n")
            (display (format "┌─ ~a " name))
            (when (patch-applied? name)
                  (display "[APPLIED] "))
            (display "─────────────────────────────────────\n")

            (display (format "│ ~a\n"
                             (manifest-get manifest 'description "No description")))
            (display (format "│ Version: ~a\n"
                             (manifest-get manifest 'version "unknown")))

            (let ([requires (manifest-get manifest 'requires '())])
                 (unless (null? requires)
                         (display (format "│ Requires: ~a\n" requires))))

            (let ([provides (manifest-get manifest 'provides '())])
                 (display (format "│ Provides: ~a functions\n" (length provides)))
                 (when (< (length provides) 20)
                       (display (format "│   ~a\n" provides))))

            (let ([files (manifest-get manifest 'files '())])
                 (display (format "│ Files: ~a\n" (length files)))
                 (for-each
                  (lambda (f) (display (format "│   ~a\n" f)))
                  files))

            (display "└────────────────────────────────────────────────────\n")))))

(doc 'section 'convenience)

(doc patch 'type "Symbol -> Bool")
(doc patch 'description "Alias for apply-patch (shorter to type)")
(define patch apply-patch)
