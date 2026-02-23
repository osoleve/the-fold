(load "boundary/meta/file-io.ss")
(load "lattice/meta/kg.ss")
(unless (top-level-bound? 'extract-docs-from-file)
  (load "boundary/meta/docs-io.ss"))

(doc 'module 'kg-io)
(doc 'description "I/O layer for knowledge graph — manifest discovery, reading, CAS persistence,
and build orchestration. In KG-first mode, the CAS is the source of truth.
Manifests are an import mechanism, not the primary store.")
(doc 'layer 'boundary)

;;; ====
;;; Root Hash Persistence
;;; ====

(define KG-ROOT-PATH ".fold-repl/kg-root.sexp")

(doc kg-save-root! 'type (-> Bytevector Void))
(doc kg-save-root! 'description "Persist the KG root hash to disk for fast reload")
(define (kg-save-root! root-hash)
  (let ([dir (let ([idx (let loop ([i (- (string-length KG-ROOT-PATH) 1)])
                          (cond [(< i 0) #f]
                                [(char=? (string-ref KG-ROOT-PATH i) #\/) i]
                                [else (loop (- i 1))]))])
               (if idx (substring KG-ROOT-PATH 0 idx) "."))])
    (unless (file-exists? dir) (mkdir dir))
    (call-with-output-file KG-ROOT-PATH
      (lambda (port)
        (write `(kg-root ,(hash->hex root-hash)) port)
        (newline port))
      'replace)))

(doc kg-load-root 'type (-> (Maybe Bytevector)))
(doc kg-load-root 'description "Load persisted KG root hash from disk. Returns #f if not found.")
(define (kg-load-root)
  (guard (e [else #f])
    (if (file-exists? KG-ROOT-PATH)
        (let* ([sexp (call-with-input-file KG-ROOT-PATH read)]
               [hex (and (pair? sexp) (eq? (car sexp) 'kg-root) (cadr sexp))])
          (if (and hex (string? hex))
              (hex->hash hex)
              #f))
        #f)))

;;; ====
;;; Build Pipeline
;;; ====

(doc kg-extract-type-sigs! 'type (-> Void))
(doc kg-extract-type-sigs! 'description "Scan lattice source files for (doc fn 'type ...) forms.
Extracts type annotations and populates the KG type signature map.
Only considers symbols that are known exports in the KG.")
(define (kg-extract-type-sigs!)
  (let ([known-exports (let loop ([exports (kg-exports)] [seen hamt-empty])
                         (if (null? exports) seen
                             (loop (cdr exports)
                                   (hamt-assoc (caar exports) #t seen))))]
        [type-pairs '()])
    ;; Scan source files under lattice/ and core/
    (for-each
     (lambda (root)
       (let ([files (find-scheme-files root)])
         (for-each
          (lambda (file)
            (guard (e [else (void)])  ; skip files that fail to parse
              (let ([docs (extract-docs-from-file file)])
                (for-each
                 (lambda (doc-entry)
                   ;; doc-entry: (file line tag content target?)
                   (let ([tag (caddr doc-entry)]
                         [content (cadddr doc-entry)]
                         [target (if (> (length doc-entry) 4) (list-ref doc-entry 4) #f)])
                     (when (and (eq? tag 'type)
                                target
                                (symbol? target)
                                (hamt-lookup target known-exports)
                                (pair? content))
                       (set! type-pairs
                             (cons (cons target (car content)) type-pairs)))))
                 docs))))
          files)))
     '("lattice" "core"))
    (kg-populate-types! type-pairs)))

(doc kg-build! 'type (-> Bytevector))
(doc kg-build! 'description "Build knowledge graph from all manifests in lattice/.
Discovers manifest files, parses them, creates blocks in CAS, extracts
concepts from keywords, type signatures from doc forms, and builds the root.
Returns root hash.")
(define (kg-build!)
  (kg-reset!)
  (let ([manifests (find-manifests "lattice")])
    (printf "Found ~a manifests\n" (length manifests))
    ;; Phase 1: Add all skills (creates skill/module/export blocks in CAS)
    (for-each
     (lambda (manifest-path)
       (let* ([sexp (read-manifest-sexp manifest-path)]
              [data (if sexp (parse-manifest sexp) #f)])
         (when data
           (printf "  Loading: ~a\n" (cdr (assq 'name data)))
           (kg-add-skill! data))))
     manifests)
    ;; Phase 2: Build dependency edges
    (kg-build-deps!)
    ;; Phase 3: Extract concepts from keywords (KG-first: concepts are first-class)
    (kg-extract-concepts!)
    ;; Phase 4: Extract type signatures from doc forms
    (kg-extract-type-sigs!)
    ;; Phase 5: Build root block (anchors the entire graph in CAS)
    (let ([root-hash (kg-build-root!)])
      ;; Phase 6: Persist root hash for fast reload
      (kg-save-root! root-hash)
      (printf "Knowledge graph built: ~a skills, ~a modules, ~a exports, ~a concepts, ~a type-sigs, ~a edges\n"
              (length *kg-skills*)
              (length *kg-modules*)
              (length *kg-exports*)
              (length *kg-concepts*)
              (hamt-size *kg-type-sigs*)
              (length *kg-edges*))
      root-hash)))

(doc kg-ensure! 'type (-> (Maybe Bytevector)))
(doc kg-ensure! 'description "Build knowledge graph only if not already initialized.")
(define (kg-ensure!)
  (if (kg-initialized?)
      #f
      (kg-build!)))

;;; ====
;;; CAS-First Loading
;;; ====

(doc kg-load-from-root! 'type (-> Boolean))
(doc kg-load-from-root! 'description "Try to load KG from persisted root hash via CAS.
This is the KG-first fast path: if the root hash exists and all blocks
are in the CAS, we skip manifest parsing entirely. Returns #t on success.")
(define (kg-load-from-root!)
  (let ([root-hash (kg-load-root)])
    (if root-hash
        (begin
          (printf "Loading KG from CAS (root: ~a...)...\n"
                  (substring (hash->hex root-hash) 0 12))
          (if (kg-load-from-cas! root-hash)
              (begin
                (printf "KG loaded from CAS: ~a skills, ~a concepts\n"
                        (length *kg-skills*)
                        (length *kg-concepts*))
                #t)
              (begin
                (printf "CAS load failed, falling back to manifest build\n")
                #f)))
        #f)))
