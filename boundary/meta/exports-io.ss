;;; boundary/meta/exports-io.ss — I/O orchestrator for export scanning
;;; @module exports-io
;;; @requires file-io exports
;;;
;;; Handles file reading, directory scanning, and manifest I/O for exports.
;;; Pure pattern recognition stays in lattice/meta/exports.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/exports.ss")

(doc 'module 'exports-io)
(doc 'description "I/O layer for export scanning — file reading, directory walking, manifest sync")
(doc 'layer 'boundary)

;;; ====
;;; File Reading
;;; ====

;;; extract-exports-from-file : String -> (List Symbol)
;;; Extract all exported symbols from a source file
(define (extract-exports-from-file path)
  (let ([sexps (read-all-sexps path)])
    (if (null? sexps)
        '()
        (extract-exports-from-sexps sexps))))

;;; extract-module-info : String -> (module description) | #f
;;; Extract module name and description from a source file
(define (extract-module-info path)
  (let ([sexps (read-all-sexps path)])
    (if (null? sexps)
        #f
        (let ([module-name #f]
              [description #f])
          (for-each
           (lambda (sexp)
             (when (and (pair? sexp)
                        (eq? (car sexp) 'doc)
                        (pair? (cdr sexp))
                        (pair? (cadr sexp))
                        (eq? (caadr sexp) 'quote))
               (let ([tag (cadadr sexp)])
                 (cond
                   [(and (eq? tag 'module) (pair? (cddr sexp)))
                    (set! module-name (caddr sexp))]
                   [(and (eq? tag 'description) (pair? (cddr sexp)))
                    (set! description (caddr sexp))]))))
           sexps)
          (if module-name
              (list module-name description)
              #f)))))

;;; extract-all-exports-from-file : String -> (List Symbol)
;;; Extract all exports (both contextual, following, and targeted) from a file
(define (extract-all-exports-from-file path)
  (let ([sexps (read-all-sexps path)])
    (if (null? sexps)
        '()
        (let ([from-defines (extract-exports-from-sexps sexps)]
              [targeted (find-targeted-exports sexps)])
          (delete-duplicates (append from-defines targeted))))))

;;; ====
;;; Directory Scanning
;;; ====

;;; find-scheme-files-ex : String -> (List String)
;;; Find all .ss files under a directory (skips test files)
(define (find-scheme-files-ex root)
  (define (walk dir)
    (guard (e [else '()])
      (let ([entries (directory-list dir)])
        (append-map (lambda (entry)
                      (let ([path (string-append dir "/" entry)])
                        (cond
                          [(and (> (string-length entry) 3)
                                (string=? ".ss" (substring entry
                                                           (- (string-length entry) 3)
                                                           (string-length entry)))
                                (not (string-prefix-exports? "test-" entry)))
                           (list path)]
                          [(and (file-directory? path)
                                (not (char=? (string-ref entry 0) #\.)))
                           (walk path)]
                          [else '()])))
                    entries))))
  (if (file-directory? root)
      (walk root)
      (if (file-exists? root) (list root) '())))

;;; string-prefix-exports? : String x String -> Boolean
(define (string-prefix-exports? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; ====
;;; Skill Export Scanning
;;; ====

;;; scan-skill-exports : String -> ((module . exports) ...)
;;; Scan a skill directory and collect exports by module
(define (scan-skill-exports skill-dir)
  (let ([files (find-scheme-files-ex skill-dir)]
        [results '()])
    (for-each
     (lambda (path)
       (let ([exports (extract-exports-from-file path)]
             [info (extract-module-info path)])
         (when (and info (pair? exports))
           (let ([module-name (car info)]
                 [desc (cadr info)])
             (set! results (cons (list module-name exports desc path) results))))))
     files)
    (reverse results)))

;;; scan-skill-all-exports : String -> ((module exports desc path) ...)
;;; Scan skill directory, finding both contextual and targeted exports
(define (scan-skill-all-exports skill-dir)
  (let ([files (find-scheme-files-ex skill-dir)]
        [results '()])
    (for-each
     (lambda (path)
       (let ([exports (extract-all-exports-from-file path)]
             [info (extract-module-info path)])
         (when (pair? exports)
           (let ([module-name (if info (car info) (path->module path))]
                 [desc (if info (cadr info) "")])
             (set! results (cons (list module-name exports desc path) results))))))
     files)
    (reverse results)))

;;; ====
;;; Pretty Printing
;;; ====

;;; print-skill-exports : String -> Void
;;; Print exports for a skill directory
(define (print-skill-exports skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (printf "Exports for ~a:~n" skill-dir)
    (printf "~a~n" (make-string 60 #\-))
    (for-each
     (lambda (entry)
       (let ([module (car entry)]
             [exports (cadr entry)]
             [desc (caddr entry)]
             [path (cadddr entry)])
         (printf "~n~a (~a):~n" module (basename path))
         (when desc
           (printf "  ~a~n" (truncate-string desc 70)))
         (printf "  Exports: ~a~n" (map symbol->string exports))))
     results)
    (printf "~n~a~n" (make-string 60 #\-))
    (printf "Total modules: ~a~n" (length results))
    (printf "Total exports: ~a~n"
            (apply + (map (lambda (e) (length (cadr e))) results)))))

;;; ====
;;; Manifest Generation
;;; ====

;;; generate-exports-sexp : String -> SExp
;;; Generate the exports section for a manifest
(define (generate-exports-sexp skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (cons 'exports
          (append-map (lambda (entry)
                        (let ([module (car entry)]
                              [exports (cadr entry)])
                          (cons (string->symbol (format ";; ~a" module))
                                exports)))
                      results))))

;;; generate-modules-sexp : String -> SExp
;;; Generate the modules section for a manifest
(define (generate-modules-sexp skill-dir)
  (let ([results (scan-skill-exports skill-dir)])
    (cons 'modules
          (map (lambda (entry)
                 (let ([module (car entry)]
                       [desc (or (caddr entry) "")]
                       [path (cadddr entry)])
                   (list module (basename path) desc)))
               results))))

;;; generate-manifest-exports : String -> SExp
;;; Generate (exports ...) section grouped by module
(define (generate-manifest-exports skill-dir)
  (let ([results (scan-skill-all-exports skill-dir)])
    (cons 'exports
          (map (lambda (entry)
                 (cons (car entry)    ; module name
                       (cadr entry))) ; exports list
               results))))

;;; generate-manifest-modules : String -> SExp
;;; Generate (modules ...) section
(define (generate-manifest-modules skill-dir)
  (let ([results (scan-skill-all-exports skill-dir)])
    (cons 'modules
          (map (lambda (entry)
                 (let ([module (car entry)]
                       [desc (or (caddr entry) "")]
                       [path (cadddr entry)])
                   (list module (basename path) desc)))
               results))))

;;; ====
;;; Manifest Update
;;; ====

;;; update-manifest-from-source : String -> SExp | #f
;;; Read existing manifest and update exports/modules from source
(define (update-manifest-from-source skill-dir)
  (let ([manifest-path (string-append skill-dir "/manifest.sexp")]
        [new-exports (generate-manifest-exports skill-dir)]
        [new-modules (generate-manifest-modules skill-dir)])
    (let ([manifest (read-manifest-sexp manifest-path)])
      (if (not manifest)
          #f
          (let* ([m1 (update-manifest-entry manifest 'exports new-exports)]
                 [m2 (update-manifest-entry m1 'modules new-modules)])
            m2)))))

;;; ====
;;; Manifest Comparison
;;; ====

;;; compare-manifest-exports : String -> Void
;;; Compare manifest-declared exports vs source-defined exports
(define (compare-manifest-exports skill-dir)
  (let* ([manifest-path (string-append skill-dir "/manifest.sexp")]
         [manifest (read-manifest-sexp manifest-path)])
    (if (not manifest)
        (printf "No manifest found at ~a~n" manifest-path)
        (let* ([source-exports (scan-skill-all-exports skill-dir)]
               [source-symbols (delete-duplicates (append-map cadr source-exports))]
               [manifest-exports (manifest-exports-list manifest)]
               [in-source-not-manifest (filter (lambda (s) (not (member s manifest-exports))) source-symbols)]
               [in-manifest-not-source (filter (lambda (s) (not (member s source-symbols))) manifest-exports)])
          (printf "Export comparison for ~a:~n" skill-dir)
          (printf "  Source exports:   ~a~n" (length source-symbols))
          (printf "  Manifest exports: ~a~n" (length manifest-exports))
          (unless (null? in-source-not-manifest)
            (printf "~n  In source but not manifest (~a):~n" (length in-source-not-manifest))
            (for-each (lambda (s) (printf "    + ~a~n" s)) in-source-not-manifest))
          (unless (null? in-manifest-not-source)
            (printf "~n  In manifest but not source (~a):~n" (length in-manifest-not-source))
            (for-each (lambda (s) (printf "    - ~a~n" s)) in-manifest-not-source))
          (when (and (null? in-source-not-manifest) (null? in-manifest-not-source))
            (printf "~n  Exports are in sync~n"))))))

;;; ====
;;; Manifest File I/O
;;; ====

;;; write-manifest : String x SExp -> Void
;;; Write manifest to file with pretty formatting
(define (write-manifest path manifest)
  (call-with-output-file path
    (lambda (port)
      (pretty-print-manifest manifest port))
    'replace))

;;; sync-manifest : String -> Void
;;; Update manifest file from source exports
(define (sync-manifest skill-dir)
  (let ([manifest (update-manifest-from-source skill-dir)])
    (if (not manifest)
        (printf "Could not read manifest for ~a~n" skill-dir)
        (let ([path (string-append skill-dir "/manifest.sexp")])
          (write-manifest path manifest)
          (printf "Updated ~a~n" path)
          (compare-manifest-exports skill-dir)))))

;;; ====
;;; Convenience Interface
;;; ====

;;; lef : String -> (List Symbol)
;;; List exports from a file (similar to le but for any file)
(define (lef path)
  (let ([exports (extract-exports-from-file path)])
    (if (null? exports)
        (printf "No exports found in ~a~n" path)
        (begin
          (printf "Exports from ~a:~n" path)
          (for-each (lambda (e) (printf "  ~a~n" e)) exports)))
    exports))

;;; le-skill : String -> Void
;;; List exports for a skill directory (alias for print-skill-exports)
(define le-skill print-skill-exports)

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "exports-io.ss loaded.\n")
(meta-printf "  (lef \"file.ss\")                - List exports from any file\n")
(meta-printf "  (scan-skill-exports \"dir\")     - Scan skill directory\n")
(meta-printf "  (compare-manifest-exports \"dir\") - Compare manifest vs source\n")
(meta-printf "  (sync-manifest \"dir\")          - Update manifest from source\n")
