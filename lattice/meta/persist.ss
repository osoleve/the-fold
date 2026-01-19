;;; lattice/meta/persist.ss — Lattice Index Persistence
;;;
;;; Saves and loads the knowledge graph and search indices to avoid
;;; rebuilding on every session. Uses a manifest fingerprint for
;;; cache invalidation.
;;;
;;; This is Lattice code: impure (file I/O).
;;;
;;; Usage:
;;;   (lattice-cache-path)           ; Get cache file path
;;;   (lattice-save-cache!)          ; Save current state to cache
;;;   (lattice-load-cache!)          ; Load from cache if valid
;;;   (lattice-cache-valid?)         ; Check if cache is usable
;;;   (lattice-manifest-fingerprint) ; Compute current fingerprint
;;;
;;; Dependencies:
;;;   core/base/prelude.ss
;;;   core/base/sha256.ss

(load "core/base/prelude.ss")
(load "core/base/sha256.ss")

;;; ====
;;; Cache Configuration
;;; ====

(define LATTICE-CACHE-PATH ".fold-repl/lattice-cache.sexp")
(define LATTICE-CACHE-VERSION 2)  ; Bump: now includes docstrings + source-locs

;;; ====
;;; Manifest Fingerprinting
;;; ====

;;; Note: find-manifests is defined in kg.ss (takes base-dir argument)
;;; We use it here: (find-manifests "lattice")

;;; lattice-manifest-fingerprint : -> String
;;; Compute SHA256 of all manifest contents concatenated
(define (lattice-manifest-fingerprint)
  (let* ([manifests (find-manifests "lattice")]
         [sorted (sort string<? manifests)]
         [contents (map file->string sorted)]
         [combined (apply string-append contents)])
        (sha256-hex (string->utf8 combined))))

;;; file->string : String -> String
;;; Read entire file as string
(define (file->string path)
  (guard (e [else ""])
         (call-with-input-file path
                               (lambda (port)
                                       (get-string-all port)))))

;;; ====
;;; Cache Path
;;; ====

;;; lattice-cache-path : -> String
(define (lattice-cache-path)
  LATTICE-CACHE-PATH)

;;; ensure-cache-dir! : -> void
(define (ensure-cache-dir!)
  (let ([dir (path-directory LATTICE-CACHE-PATH)])
       (unless (file-exists? dir)
               (mkdir dir))))

;;; path-directory : String -> String
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String Char -> Int | #f
(define (string-rindex str char)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (- i 1))])))

;;; ====
;;; State Serialization
;;; ====

;;; The cache format is:
;;; (lattice-cache
;;;   (version <int>)
;;;   (fingerprint <string>)
;;;   (kg-state
;;;     (skills <alist>)
;;;     (skill-data <alist>)
;;;     (modules <alist>)
;;;     (exports <alist>))
;;;   (search-state
;;;     (exports-list <list>)))

;;; serialize-kg-state : -> SExp
;;; Serialize current KG state for search index restoration
;;; Only stores what's needed for search, not actual block objects
(define (serialize-kg-state)
  `(kg-state
    (skill-names ,(map car *kg-skills*))
    (skill-data ,*kg-skill-data*)
    (module-names ,(map car *kg-modules*))
    (export-names ,(map car *kg-exports*))))

;;; serialize-docstrings : -> (List (Symbol . String))
;;; Convert *docstrings* hashtable to alist for serialization
(define (serialize-docstrings)
  (let-values ([(keys vals) (hashtable-entries *docstrings*)])
              (map cons (vector->list keys) (vector->list vals))))

;;; serialize-source-locs : -> (List (Symbol File Line))
;;; Convert *source-locations* hashtable to list for serialization
(define (serialize-source-locs)
  (let-values ([(keys vals) (hashtable-entries *source-locations*)])
              (map (lambda (k v) (list k (car v) (cdr v)))
                   (vector->list keys) (vector->list vals))))

;;; serialize-cache : String -> SExp
;;; Create full cache S-expression with fingerprint
(define (serialize-cache fingerprint)
  `(lattice-cache
    (version ,LATTICE-CACHE-VERSION)
    (fingerprint ,fingerprint)
    ,(serialize-kg-state)
    (docstrings ,(serialize-docstrings))
    (source-locs ,(serialize-source-locs))))

;;; ====
;;; Cache Writing
;;; ====

;;; lattice-save-cache! : -> Bool
;;; Save current KG state to cache file
(define (lattice-save-cache!)
  (guard (e [else (printf "Cache save failed: ~a\n" e) #f])
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

;;; ====
;;; Cache Reading
;;; ====

;;; read-cache-file : -> SExp | #f
;;; Read and parse cache file
(define (read-cache-file)
  (guard (e [else #f])
         (if (file-exists? LATTICE-CACHE-PATH)
             (call-with-input-file LATTICE-CACHE-PATH read)
             #f)))

;;; cache-field : SExp Symbol -> Any | #f
;;; For simple fields like (version 1), returns the value (1)
;;; For nested fields like (kg-state ...), returns the whole field
(define (cache-field cache field-name)
  (if (and (pair? cache) (eq? (car cache) 'lattice-cache))
      (let loop ([fields (cdr cache)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             ;; For kg-state, return the whole structure; for others return value
             (if (eq? field-name 'kg-state)
                 (car fields)  ; Return (kg-state ...)
                 (cadar fields))]  ; Return just the value
            [else (loop (cdr fields))]))
      #f))

;;; kg-state-field : SExp Symbol -> Any | #f
(define (kg-state-field kg-state field-name)
  (if (and (pair? kg-state) (eq? (car kg-state) 'kg-state))
      (let loop ([fields (cdr kg-state)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             (cadar fields)]
            [else (loop (cdr fields))]))
      #f))

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

;;; ====
;;; Cache Loading
;;; ====

;;; restore-kg-state! : SExp -> void
;;; Restore KG globals from cached state
;;; Creates placeholder entries (without blocks) sufficient for search
(define (restore-kg-state! kg-state)
  (let ([skill-names (kg-state-field kg-state 'skill-names)]
        [skill-data (kg-state-field kg-state 'skill-data)]
        [module-names (kg-state-field kg-state 'module-names)]
        [export-names (kg-state-field kg-state 'export-names)])
       ;; Restore skills as (name . #f) since we only need names for search
       (set! *kg-skills* (map (lambda (n) (cons n #f)) (or skill-names '())))
       (set! *kg-skill-data* (or skill-data '()))
       ;; Restore modules and exports as (name . #f)
       (set! *kg-modules* (map (lambda (n) (cons n #f)) (or module-names '())))
       (set! *kg-exports* (map (lambda (n) (cons n #f)) (or export-names '())))
       (set! *kg-deps* '())  ; Deps can be recomputed from skill-data if needed
       ))

;;; restore-docstrings! : (List (Symbol . String)) -> void
;;; Restore docstrings cache from alist
(define (restore-docstrings! alist)
  (set! *docstrings* (make-hashtable symbol-hash eq?))
  (for-each (lambda (pair)
                    (hashtable-set! *docstrings* (car pair) (cdr pair)))
            (or alist '())))

;;; restore-source-locs! : (List (Symbol File Line)) -> void
;;; Restore source locations cache from list
(define (restore-source-locs! locs)
  (set! *source-locations* (make-hashtable symbol-hash eq?))
  (for-each (lambda (entry)
                    (hashtable-set! *source-locations*
                                    (car entry)
                                    (cons (cadr entry) (caddr entry))))
            (or locs '())))

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

(meta-printf "persist.ss loaded.\n")
(meta-printf "  (lattice-cache-valid?)         - Check cache validity\n")
(meta-printf "  (lattice-save-cache!)          - Save to cache\n")
(meta-printf "  (lattice-load-cache!)          - Load from cache\n")
(meta-printf "  (lattice-init-cached!)         - Init with caching\n")
(meta-printf "  (lattice-manifest-fingerprint) - Show fingerprint\n")
