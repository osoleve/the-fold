;;; boundary/meta/persist-io.ss — I/O orchestrator for lattice cache persistence
;;; @module persist-io
;;; @requires file-io persist
;;;
;;; Handles cache file read/write and manifest fingerprinting.
;;; Pure serialization logic stays in lattice/meta/persist.ss.

(load "boundary/meta/file-io.ss")
(load "lattice/meta/persist.ss")

;;; ====
;;; Manifest Fingerprinting (I/O — reads manifest files)
;;; ====

;;; file->string : String -> String
;;; Read entire file as string
(define (file->string path)
  (guard (e [else ""])
         (call-with-input-file path
                               (lambda (port)
                                       (get-string-all port)))))

;;; lattice-manifest-fingerprint : -> String
;;; Compute SHA256 of all manifest contents concatenated
(define (lattice-manifest-fingerprint)
  (let* ([manifests (find-manifests "lattice")]
         [sorted (sort string<? manifests)]
         [contents (map file->string sorted)]
         [combined (apply string-append contents)])
        (sha256-hex (string->utf8 combined))))

;;; ====
;;; Cache Directory Management
;;; ====

;;; ensure-cache-dir! : -> void
(define (ensure-cache-dir!)
  (let ([dir (path-directory LATTICE-CACHE-PATH)])
       (unless (file-exists? dir)
               (mkdir dir))))

;;; ====
;;; Cache File I/O
;;; ====

;;; read-cache-file : -> SExp | #f
;;; Read and parse cache file
(define (read-cache-file)
  (guard (e [else #f])
         (if (file-exists? LATTICE-CACHE-PATH)
             (call-with-input-file LATTICE-CACHE-PATH read)
             #f)))

;;; lattice-save-cache! : -> Bool
;;; Save current KG state to cache file
(define (lattice-save-cache!)
  (guard (e [else
             (let ([msg (guard (e2 [else (format "~a" e)])
                          (if (irritants-condition? e)
                              (apply format (condition-message e)
                                     (condition-irritants e))
                              (condition-message e)))])
               (printf "Cache save failed: ~a\n" msg))
             #f])
         (ensure-cache-dir!)
         (let* ([fingerprint (lattice-manifest-fingerprint)]
                [cache-data (serialize-cache fingerprint)])
               (call-with-output-file LATTICE-CACHE-PATH
                                       (lambda (port)
                                               (pretty-print cache-data port))
                                       'replace)
               (printf "Lattice cache saved (~a skills, fingerprint: ~a...)\n"
                       (length *kg-skills*)
                       (substring fingerprint 0 12))
               #t)))

;;; lattice-cache-valid? : -> Bool
;;; Check if cache exists and fingerprint matches
(define (lattice-cache-valid?)
  (let ([cache (read-cache-file)])
       (and cache
            (= (or (cache-field cache 'version) 0) LATTICE-CACHE-VERSION)
            (let ([cached-fp (cache-field cache 'fingerprint)]
                  [current-fp (lattice-manifest-fingerprint)])
                 (and cached-fp
                      (string=? cached-fp current-fp))))))

;;; lattice-load-cache! : -> Bool
;;; Load KG state from cache if valid
(define (lattice-load-cache!)
  (guard (e [else #f])
         (let ([cache (read-cache-file)])
              (cond
               [(not cache)
                #f]
               [(not (= (or (cache-field cache 'version) 0) LATTICE-CACHE-VERSION))
                #f]
               [else
                (let ([cached-fp (cache-field cache 'fingerprint)]
                      [current-fp (lattice-manifest-fingerprint)])
                     (if (and cached-fp (string=? cached-fp current-fp))
                         (let ([kg-state (cache-field cache 'kg-state)]
                               [docstrings (cache-field cache 'docstrings)]
                               [source-locs (cache-field cache 'source-locs)])
                              (when kg-state (restore-kg-state! kg-state))
                              (when docstrings (restore-docstrings! docstrings))
                              (when source-locs (restore-source-locs! source-locs))
                              #t)
                         #f))]))))

;;; ====
;;; Integrated Init
;;; ====

;;; lattice-init-cached! : -> void
;;; Initialize lattice, using cache if valid
(define (lattice-init-cached!)
  (if (lattice-load-cache!)
      ;; Cache loaded, just build search indices
      (begin
        (printf "Building search indices from cache...\n")
        (lattice-index!)
        (printf "Lattice ready (from cache).\n"))
      ;; No valid cache, full rebuild
      (begin
        (printf "Building knowledge graph from manifests...\n")
        (kg-build!)
        (lattice-index!)
        (lattice-save-cache!)
        (printf "Lattice ready.\n"))))

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "  (lattice-cache-valid?)         - Check cache validity\n")
(meta-printf "  (lattice-save-cache!)          - Save to cache\n")
(meta-printf "  (lattice-load-cache!)          - Load from cache\n")
(meta-printf "  (lattice-init-cached!)         - Init with caching\n")
(meta-printf "  (lattice-manifest-fingerprint) - Show fingerprint\n")
