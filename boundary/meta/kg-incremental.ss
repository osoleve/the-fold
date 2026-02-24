;;; Incremental KG Build Support
;;; Fingerprint-based change detection for manifest files.
;;; Avoids full KG rebuild when only some manifests have changed.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/kg.ss")

(doc 'module 'kg-incremental)
(doc 'description "Manifest fingerprinting and change detection for incremental KG builds.
Computes SHA-256 fingerprints of manifest files and their referenced source files.
Persists fingerprints alongside the KG root hash so subsequent builds can detect
which skills changed and skip expensive I/O for unchanged ones.")
(doc 'layer 'boundary)

;;; ====
;;; Fingerprint Storage
;;; ====

(define KG-FINGERPRINTS-PATH ".fold-repl/kg-fingerprints.sexp")

(doc kg-load-fingerprints 'type (-> (List (Pair Symbol String))))
(doc kg-load-fingerprints 'description "Load persisted manifest fingerprints from disk.
Returns alist of (skill-name . fingerprint-hex) or '() if not found.")
(define (kg-load-fingerprints)
  (guard (e [else '()])
    (if (file-exists? KG-FINGERPRINTS-PATH)
        (let ([data (call-with-input-file KG-FINGERPRINTS-PATH read)])
          (if (and (pair? data)
                   (assq 'version data)
                   (= (cdr (assq 'version data)) 1)
                   (assq 'fingerprints data))
              (cdr (assq 'fingerprints data))
              '()))
        '())))

(doc kg-save-fingerprints! 'type (-> (List (Pair Symbol String)) Void))
(doc kg-save-fingerprints! 'description "Persist manifest fingerprints to disk.")
(define (kg-save-fingerprints! fingerprints)
  (let ([dir (let ([idx (let loop ([i (- (string-length KG-FINGERPRINTS-PATH) 1)])
                          (cond [(< i 0) #f]
                                [(char=? (string-ref KG-FINGERPRINTS-PATH i) #\/) i]
                                [else (loop (- i 1))]))])
               (if idx (substring KG-FINGERPRINTS-PATH 0 idx) "."))])
    (unless (file-exists? dir) (mkdir dir))
    (call-with-output-file KG-FINGERPRINTS-PATH
      (lambda (port)
        (write `((version . 1)
                 (fingerprints . ,fingerprints))
               port)
        (newline port))
      'replace)))

;;; ====
;;; Manifest Fingerprinting
;;; ====

(doc kg-file-content-hash 'type (-> String String))
(doc kg-file-content-hash 'description "Compute SHA-256 hex hash of a file's contents.
Returns \"missing\" for non-existent files.")
(define (kg-file-content-hash filepath)
  (guard (e [else "missing"])
    (if (file-exists? filepath)
        (let* ([content (call-with-input-file filepath
                          (lambda (p) (get-string-all p)))]
               [bv (string->utf8 content)]
               [hash (sha256 bv)])
          (hash->hex hash))
        "missing")))

(doc kg-manifest-source-files 'type (-> String ManifestData (List String)))
(doc kg-manifest-source-files 'description "Get all source files referenced by a manifest.
Returns the manifest path itself plus all module source files.")
(define (kg-manifest-source-files manifest-path manifest-data)
  (let* ([skill-path (let ([p (assq 'path manifest-data)])
                       (if (and p (string? (cdr p))) (cdr p) ""))]
         [modules-raw (let ([m (assq 'modules manifest-data)])
                        (if (and m (list? (cdr m))) (cdr m) '()))]
         [source-files
          (apply append
                 (map (lambda (mod)
                        (let ([entries (parse-module-entry mod skill-path)])
                          (filter-map
                           (lambda (entry)
                             (let ([path (cdr entry)])
                               (and (file-exists? path) path)))
                           entries)))
                      modules-raw))])
    (cons manifest-path source-files)))

(doc kg-manifest-fingerprint 'type (-> String ManifestData String))
(doc kg-manifest-fingerprint 'description "Compute fingerprint for a manifest: SHA-256 of
concatenated content hashes of the manifest + all its source files, sorted.")
(define (kg-manifest-fingerprint manifest-path manifest-data)
  (let* ([all-files (kg-manifest-source-files manifest-path manifest-data)]
         [sorted (list-sort string<? all-files)]
         [hashes (map kg-file-content-hash sorted)]
         [combined (apply string-append hashes)]
         [fp-hash (sha256 (string->utf8 combined))])
    (hash->hex fp-hash)))

;;; ====
;;; Change Detection
;;; ====

(doc kg-compute-all-fingerprints 'type (-> (List String) (List (Pair Symbol String))))
(doc kg-compute-all-fingerprints 'description "Compute fingerprints for all given manifest paths.
Returns alist of (skill-name . fingerprint-hex).")
(define (kg-compute-all-fingerprints manifest-paths)
  (filter-map
   (lambda (manifest-path)
     (guard (e [else #f])
       (let* ([sexp (read-manifest-sexp manifest-path)]
              [data (if sexp (parse-manifest sexp) #f)])
         (and data
              (let ([name (cdr (assq 'name data))]
                    [fp (kg-manifest-fingerprint manifest-path data)])
                (cons name fp))))))
   manifest-paths))

(doc kg-detect-changes 'type (-> (List (Pair Symbol String))
                                 (List (Pair Symbol String))
                                 (values (List Symbol) (List Symbol) (List Symbol))))
(doc kg-detect-changes 'description "Compare current fingerprints against cached ones.
Returns three values: (changed added removed).
- changed: skills whose fingerprint differs
- added: skills in current but not in cache
- removed: skills in cache but not in current")
(define (kg-detect-changes current-fps cached-fps)
  (let ([cached-map (fold-left
                     (lambda (m pair) (hamt-assoc (car pair) (cdr pair) m))
                     hamt-empty
                     cached-fps)]
        [current-map (fold-left
                      (lambda (m pair) (hamt-assoc (car pair) (cdr pair) m))
                      hamt-empty
                      current-fps)]
        [changed '()]
        [added '()]
        [removed '()])
    ;; Check each current skill against cache
    (for-each
     (lambda (pair)
       (let* ([name (car pair)]
              [fp (cdr pair)]
              [cached-fp (hamt-lookup name cached-map)])
         (cond
           [(not cached-fp)
            (set! added (cons name added))]
           [(not (string=? fp cached-fp))
            (set! changed (cons name changed))])))
     current-fps)
    ;; Check for removed skills
    (for-each
     (lambda (pair)
       (let ([name (car pair)])
         (unless (hamt-lookup name current-map)
           (set! removed (cons name removed)))))
     cached-fps)
    (values changed added removed)))

(meta-printf "kg-incremental.ss loaded.\n")
(meta-printf "  (kg-load-fingerprints)      - Load cached fingerprints\n")
(meta-printf "  (kg-detect-changes cur old) - Detect changed manifests\n")
