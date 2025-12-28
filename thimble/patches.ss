;;; thimble/patches.ss — The Fold Patch System
;;;
;;; Patches are loadable packages of functionality that extend The Fold.
;;; Like fabric patches sewn onto cloth, software patches, or synth patches.
;;;
;;; Usage:
;;;   (patches)              ; List available patches
;;;   (apply-patch 'turtle)  ; Load a patch
;;;   (patch-info 'turtle)   ; Show patch details
;;;   (applied-patches)      ; List loaded patches
;;;
;;; Patch Manifest Format (in patches/<name>.ss):
;;;   ((name . <symbol>)
;;;    (description . <string>)
;;;    (version . <string>)
;;;    (provides . (<symbol> ...))    ; Functions/values provided
;;;    (requires . (<symbol> ...))    ; Other patches needed first
;;;    (files . (<path> ...)))        ; Files to load, in order
;;;
;;; This is Shell code: manages loading and state.

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *patches-dir* "patches")
(define *applied-patches* '())  ; List of applied patch names

;;; ============================================================
;;; Patch Registry
;;; ============================================================

;;; scan-patches : -> (List Symbol)
;;; Scan the patches directory for available patches.
(define (scan-patches)
  (if (file-exists? *patches-dir*)
      (let ([files (directory-list *patches-dir*)])
        (filter-map
          (lambda (f)
            (let ([name (string->symbol (path-strip-extension f))])
              (if (string-suffix? ".ss" f)
                  name
                  #f)))
          files))
      '()))

;;; path-strip-extension : String -> String
;;; Remove .ss extension from filename.
(define (path-strip-extension filename)
  (let ([len (string-length filename)])
    (if (and (>= len 3)
             (string=? ".ss" (substring filename (- len 3) len)))
        (substring filename 0 (- len 3))
        filename)))

;;; string-suffix? : String String -> Bool
(define (string-suffix? suffix str)
  (let ([slen (string-length suffix)]
        [len (string-length str)])
    (and (>= len slen)
         (string=? suffix (substring str (- len slen) len)))))

;;; filter-map : (A -> B | #f) (List A) -> (List B)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (if result
              (loop (cdr lst) (cons result acc))
              (loop (cdr lst) acc))))))

;;; ============================================================
;;; Manifest Handling
;;; ============================================================

;;; read-manifest : Symbol -> Alist | #f
;;; Read a patch manifest from the patches directory.
(define (read-manifest name)
  (let ([path (string-append *patches-dir* "/" (symbol->string name) ".ss")])
    (if (file-exists? path)
        (guard (e [else #f])
          (call-with-input-file path
            (lambda (p)
              (read p))))
        #f)))

;;; manifest-get : Alist Symbol Any -> Any
;;; Get a field from a manifest with default.
(define (manifest-get manifest key default)
  (let ([pair (assq key manifest)])
    (if pair (cdr pair) default)))

;;; ============================================================
;;; Patch Loading
;;; ============================================================

;;; patch-applied? : Symbol -> Bool
(define (patch-applied? name)
  (memq name *applied-patches*))

;;; apply-patch : Symbol -> Bool
;;; Load a patch by name. Returns #t on success.
(define (apply-patch name)
  (cond
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

;;; apply-patch-from-manifest : Symbol Alist -> Bool
(define (apply-patch-from-manifest name manifest)
  (let ([requires (manifest-get manifest 'requires '())]
        [files (manifest-get manifest 'files '())]
        [description (manifest-get manifest 'description "No description")])

    ;; Check dependencies
    (let ([missing (filter (lambda (dep) (not (patch-applied? dep))) requires)])
      (if (not (null? missing))
          (begin
            (display (format "  Missing dependencies: ~a\n" missing))
            (display "  Apply them first, or use (apply-patch-recursive 'name)\n")
            #f)
          ;; Load files
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

;;; load-patch-files : (List String) -> Bool
;;; Load a list of files in order.
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

;;; apply-patch-recursive : Symbol -> Bool
;;; Apply a patch and all its dependencies.
(define (apply-patch-recursive name)
  (let ([manifest (read-manifest name)])
    (if (not manifest)
        (begin
          (display (format "  ERROR: Patch '~a' not found.\n" name))
          #f)
        (let ([requires (manifest-get manifest 'requires '())])
          ;; Apply dependencies first
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

;;; ============================================================
;;; Namespace Integration
;;; ============================================================

;;; register-patch-symbols! : (List Symbol) -> void
;;; Add patch-provided symbols to the global whitelist.
(define (register-patch-symbols! symbols)
  (when (top-level-bound? 'add-global!)
    (for-each add-global! symbols)))

;;; ============================================================
;;; User Interface
;;; ============================================================

;;; patches : -> void
;;; List all available patches with status.
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

;;; applied-patches : -> (List Symbol)
;;; Return list of currently applied patches.
(define (applied-patches)
  *applied-patches*)

;;; patch-info : Symbol -> void
;;; Display detailed information about a patch.
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

;;; ============================================================
;;; Convenience
;;; ============================================================

;;; patch : Symbol -> Bool
;;; Alias for apply-patch (shorter to type).
(define patch apply-patch)

