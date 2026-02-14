(define-syntax doc
  (syntax-rules ()
    [(_ args ...) (void)]))

(doc 'module 'module/manifest-scanner)
(doc 'description "Impure scanning for manifest.sexp files")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude))
(doc 'note "Uses only core primitives (no lattice deps) to avoid bootstrap issues")

(doc 'section 'file-reading)

(define (read-manifest-file filepath)
  (doc 'type (-> String (Maybe SExp)))
  (doc 'description "Read and parse a manifest.sexp file, returns the first S-expression in the file, or #f on error")
  (doc 'export #t)
  (guard (e [else #f])
         (call-with-input-file filepath
                               (lambda (port)
                                       ;; Skip comment lines at the start
                                       (let loop ()
                                            (let ([c (peek-char port)])
                                                 (cond
                                                  [(eof-object? c) #f]
                                                  [(char=? c #\;)
                                                   (get-line port)  ; Skip comment line
                                                   (loop)]
                                                  [(char-whitespace? c)
                                                   (get-char port)
                                                   (loop)]
                                                  [else (read port)])))))))

(doc 'section 'directory-scanning)

(define (manifest-string-starts-with? str prefix)
  (doc 'type (-> String String Bool))
  (doc 'description "Check if string starts with prefix")
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (>= str-len pre-len)
            (string=? (substring str 0 pre-len) prefix))))

(doc find-manifests 'type (-> String (List String)))
(doc find-manifests 'description "Find all manifest.sexp files under a directory (recursive), returns a list of file paths in sorted order for deterministic discovery")
(doc find-manifests 'export #t)
(define (find-manifests base-dir)
  ;; Use accumulator-based recursion to avoid O(N²) append
  (define (find-acc dir acc)
    ;; Check for manifest in this directory
    (let* ([manifest-path (string-append dir "/manifest.sexp")]
           [acc (if (file-exists? manifest-path)
                    (cons manifest-path acc)
                    acc)])
          ;; Recursively check subdirectories (sorted for determinism)
          (guard (e [else acc])
                 (let ([entries (list-sort string<? (directory-list dir))])
                      (fold-left
                       (lambda (acc entry)
                               (let ([path (string-append dir "/" entry)])
                                    (if (and (file-directory? path)
                                             (not (manifest-string-starts-with? entry ".")))
                                        (find-acc path acc)
                                        acc)))
                       acc
                       entries)))))
  (reverse (find-acc base-dir '())))

(doc 'section 'main-scanning-api)

(define (scan-manifests base-dir)
  (doc 'type (-> String (List (Pair String SExp))))
  (doc 'description "Find all manifests under a directory and read their contents, returns a list of (path . sexp) pairs where sexp is the parsed manifest, skips files that fail to parse")
  (doc 'export #t)
  (let* ([manifest-paths (find-manifests base-dir)]
         [results '()])
        (for-each
         (lambda (path)
                 (let ([sexp (read-manifest-file path)])
                      (when (and sexp (pair? sexp) (eq? (car sexp) 'skill))
                            (set! results (cons (cons path sexp) results)))))
         manifest-paths)
        results))

(define (scan-lattice-manifests)
  (doc 'type (-> (List (Pair String SExp))))
  (doc 'description "Scan the lattice/ directory for all manifests, convenience function for the common case")
  (doc 'export #t)
  (scan-manifests "lattice"))

(define (scan-all-manifests)
  (doc 'type (-> (List (Pair String SExp))))
  (doc 'description "Scan all known directories for manifests, currently just scans lattice/ since core modules are registered explicitly")
  (doc 'export #t)
  (scan-lattice-manifests))

(doc 'section 'debugging)

(define (list-manifest-files base-dir)
  (doc 'type (-> String (List String)))
  (doc 'description "List all manifest.sexp file paths under a directory (for debugging)")
  (doc 'export #t)
  (find-manifests base-dir))

(define (count-manifests base-dir)
  (doc 'type (-> String Nat))
  (doc 'description "Count manifest files under a directory")
  (doc 'export #t)
  (length (find-manifests base-dir)))
