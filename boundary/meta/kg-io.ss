(load "boundary/meta/file-io.ss")
(load "lattice/meta/kg.ss")
(unless (top-level-bound? 'extract-docs-from-file)
  (load "boundary/meta/docs-io.ss"))
(unless (top-level-bound? 'scan-skill-all-exports)
  (load "boundary/meta/exports-io.ss"))
(unless (top-level-bound? 'exports-of)
  (load "boundary/introspect/exports.ss"))
(unless (top-level-bound? 'hydrate-cas!)
  (load "boundary/storage/cas-persist.ss"))
(unless (top-level-bound? 'kg-load-fingerprints)
  (load "boundary/meta/kg-incremental.ss"))
(unless (top-level-bound? 'install-concept-ontology!)
  (load "lattice/meta/concept-normalize.ss"))

(doc 'module 'kg-io)
(doc 'description "I/O layer for knowledge graph — manifest discovery, reading, CAS persistence,
and build orchestration. In KG-first mode, the CAS is the source of truth.
Manifests are an import mechanism, not the primary store.")
(doc 'layer 'boundary)

(doc *kg-enrich-from-source?* 'type Boolean)
(doc *kg-enrich-from-source?* 'description "When #t, enrich manifest exports/modules by scanning source files during kg-build!.")
(define *kg-enrich-from-source?* #t)

(doc *kg-export-inference-allowlist* 'type '(List Symbol))
(doc *kg-export-inference-allowlist* 'description "Skills allowed to use broad exports-of fallback when manifest exports are empty.
This is intentionally conservative to avoid flooding the KG with internal definitions.")
(define *kg-export-inference-allowlist* '(meta))

(doc *kg-hydrate-cas-on-load?* 'type Boolean)
(doc *kg-hydrate-cas-on-load?* 'description "When #t, kg-load-from-root! hydrates in-memory CAS from disk before root lookup if needed.")
(define *kg-hydrate-cas-on-load?* #t)

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

;;; alist-set : Alist Symbol Any -> Alist
;;; Set key in alist, preserving order when possible.
(define (alist-set alist key value)
  (let loop ([xs alist] [acc '()] [found #f])
    (cond
      [(null? xs)
       (let ([base (reverse acc)])
         (if found
             base
             (append base (list (cons key value)))))]
      [(and (pair? (car xs)) (eq? (caar xs) key))
       (loop (cdr xs) (cons (cons key value) acc) #t)]
      [else
       (loop (cdr xs) (cons (car xs) acc) found)])))

;;; dedupe-symbols : (List Symbol) -> (List Symbol)
(define (dedupe-symbols syms)
  (let loop ([xs syms] [seen hamt-empty] [acc '()])
    (if (null? xs)
        (reverse acc)
        (let ([s (car xs)])
          (if (or (not (symbol? s)) (hamt-lookup s seen))
              (loop (cdr xs) seen acc)
              (loop (cdr xs)
                    (hamt-assoc s #t seen)
                    (cons s acc)))))))

;;; string-prefix-kg? : String String -> Boolean
(define (string-prefix-kg? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; string-contains-kg? : String String -> Boolean
(define (string-contains-kg? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
    (if (> n-len h-len)
        #f
        (let loop ([i 0])
          (cond
            [(> (+ i n-len) h-len) #f]
            [(string=? (substring haystack i (+ i n-len)) needle) #t]
            [else (loop (+ i 1))])))))

;;; likely-public-export? : Symbol -> Boolean
;;; Conservative filter for inferred (non-annotated) exports.
(define (likely-public-export? sym)
  (and (symbol? sym)
       (let ([name (symbol->string sym)])
         (and (not (string-prefix-kg? "*" name))
              (not (string-prefix-kg? "%" name))
              (not (string-prefix-kg? "test-" name))
              (not (string-prefix-kg? "assert-" name))
              (not (string-prefix-kg? "debug-" name))
              (not (string-prefix-kg? "tmp-" name))
              (not (string-prefix-kg? "unsafe-" name))
              (not (string-contains-kg? name "internal"))
              (not (string-contains-kg? name "private"))))))

;;; meta-api-symbol? : Symbol -> Boolean
;;; Additional quality gate for inferred exports in the meta skill.
(define (meta-api-symbol? sym)
  (and (likely-public-export? sym)
       (let ([name (symbol->string sym)])
         (or (string-prefix-kg? "kg-" name)
             (string-prefix-kg? "lattice-" name)
             (string-prefix-kg? "bm25-" name)
             (string-prefix-kg? "xref-" name)
             (string-prefix-kg? "doc-" name)
             (string-prefix-kg? "lf" name)
             (string-prefix-kg? "li" name)
             (string-prefix-kg? "le" name)
             (string-prefix-kg? "lm" name)
             (string-prefix-kg? "ld" name)
             (string-prefix-kg? "lu" name)
             (string-prefix-kg? "ls" name)
             (string-prefix-kg? "lh" name)
             (string-prefix-kg? "lk" name)
             (string-prefix-kg? "lc" name)
             (string-prefix-kg? "lt" name)
             (string-prefix-kg? "lr" name)))))

;;; manifest-export-symbols : ManifestData -> (List Symbol)
;;; Flatten exports regardless of grouped/flat manifest format.
(define (manifest-export-symbols manifest-data)
  (let* ([exports-entry (assq 'exports manifest-data)]
         [exports-raw (if (and exports-entry (list? (cdr exports-entry)))
                          (cdr exports-entry)
                          '())])
    (dedupe-symbols
     (append-map
      (lambda (item)
        (cond
          [(symbol? item) (list item)]
          [(and (pair? item) (symbol? (car item)))
           (filter symbol? (cdr item))]
          [else '()]))
      exports-raw))))

;;; manifest-exports->groups : ManifestData -> (List (Pair (Maybe Symbol) (List Symbol)))
;;; Group exports by module. Flat exports use #f as module key.
(define (manifest-exports->groups manifest-data)
  (let* ([exports-entry (assq 'exports manifest-data)]
         [exports-raw (if (and exports-entry (list? (cdr exports-entry)))
                          (cdr exports-entry)
                          '())])
    (let loop ([items exports-raw] [groups '()])
      (if (null? items)
          groups
          (let ([it (car items)])
            (cond
              [(symbol? it)
               (loop (cdr items)
                     (group-add-exports groups #f (list it)))]
              [(and (pair? it) (symbol? (car it)))
               (loop (cdr items)
                     (group-add-exports groups (car it) (filter symbol? (cdr it))))]
              [else
               (loop (cdr items) groups)]))))))

;;; group-add-exports : Groups (Maybe Symbol) (List Symbol) -> Groups
;;; Add symbols to a module group, deduplicating while preserving order.
(define (group-add-exports groups module-name symbols)
  (let ([entry (assoc module-name groups)])
    (if entry
        (map (lambda (g)
               (if (equal? (car g) module-name)
                   (cons module-name (dedupe-symbols (append (cdr g) symbols)))
                   g))
             groups)
        (append groups (list (cons module-name (dedupe-symbols symbols)))))))

;;; merge-exports-from-scan : ManifestData ScanResults -> (List Any)
;;; Merge source-scanned exports into manifest exports while retaining existing entries.
(define (merge-exports-from-scan manifest-data scan-results)
  (let* ([base-groups (manifest-exports->groups manifest-data)]
         [merged
          (fold-left
           (lambda (groups entry)
             (let ([module-name (car entry)]
                   [symbols (if (and (pair? (cdr entry)) (list? (cadr entry)))
                                (cadr entry)
                                '())])
               (group-add-exports groups module-name (filter symbol? symbols))))
           base-groups
           scan-results)]
         [flat-entry (assoc #f merged)]
         [flat-syms (if flat-entry (dedupe-symbols (cdr flat-entry)) '())]
         [mod-groups (filter (lambda (g) (car g)) merged)]
         [mod-forms (map (lambda (g) (cons (car g) (dedupe-symbols (cdr g)))) mod-groups)])
    (if (null? mod-forms)
        flat-syms
        (append flat-syms mod-forms))))

;;; infer-exports-for-empty-manifest : ManifestData -> ScanResults
;;; Fallback: if manifest exports are empty, infer grouped exports from module files.
(define (infer-exports-for-empty-manifest manifest-data)
  (let ([skill-name (cdr (or (assq 'name manifest-data) '(name . unknown)))])
    (if (or (not (null? (manifest-export-symbols manifest-data)))
            (not (memq skill-name *kg-export-inference-allowlist*)))
      '()
      (let* ([path-entry (assq 'path manifest-data)]
             [skill-path (if (and path-entry (string? (cdr path-entry)))
                             (cdr path-entry)
                             "")]
             [mods-entry (assq 'modules manifest-data)]
             [mods-raw (if (and mods-entry (list? (cdr mods-entry)))
                           (cdr mods-entry)
                           '())]
             [results '()])
        (for-each
         (lambda (mod)
           (let ([entries (parse-module-entry mod skill-path)])
             (for-each
              (lambda (entry)
                (let* ([mod-name (car entry)]
                       [file-path (cdr entry)]
                       [symbols (if (eq? skill-name 'meta)
                                    (filter meta-api-symbol? (exports-of file-path))
                                    (filter likely-public-export? (exports-of file-path)))])
                  (when (pair? symbols)
                    (set! results
                          (cons (list mod-name symbols "" file-path) results)))))
              entries)))
         mods-raw)
        (reverse results)))))

;;; module-entry-name : Any -> (Maybe Symbol)
(define (module-entry-name entry)
  (and (pair? entry) (symbol? (car entry)) (car entry)))

;;; merge-modules-from-scan : ManifestData ScanResults -> (List Any)
;;; Add missing module entries discovered from source scan.
(define (merge-modules-from-scan manifest-data scan-results)
  (let* ([mods-entry (assq 'modules manifest-data)]
         [mods-raw (if (and mods-entry (list? (cdr mods-entry)))
                       (cdr mods-entry)
                       '())]
         [existing-names (fold-left
                          (lambda (m entry)
                            (let ([name (module-entry-name entry)])
                              (if name (hamt-assoc name #t m) m)))
                          hamt-empty
                          mods-raw)])
    (fold-left
     (lambda (mods entry)
       (let* ([mod-name (car entry)]
              [desc (or (caddr entry) "")]
              [path (cadddr entry)]
              [file (basename path)])
         (if (or (not (symbol? mod-name))
                 (hamt-lookup mod-name existing-names))
             mods
             (begin
               (set! existing-names (hamt-assoc mod-name #t existing-names))
               (append mods (list (list mod-name file desc)))))))
     mods-raw
     scan-results)))

;;; kg-enrich-manifest-from-source : ManifestData -> ManifestData
;;; Merge source-derived exports/modules into parsed manifest data.
(define (kg-enrich-manifest-from-source manifest-data)
  (if (not *kg-enrich-from-source?*)
      manifest-data
      (let* ([path-entry (assq 'path manifest-data)]
             [skill-dir (if (and path-entry (string? (cdr path-entry)))
                            (cdr path-entry)
                            #f)])
        (if (or (not skill-dir) (not (file-directory? skill-dir)))
            manifest-data
            (let* ([scan-results (scan-skill-all-exports skill-dir)]
                   [fallback-results (infer-exports-for-empty-manifest manifest-data)]
                   [all-scan-results (append scan-results fallback-results)])
              (if (null? scan-results)
                  (if (null? fallback-results)
                      manifest-data
                      (let* ([merged-exports (merge-exports-from-scan manifest-data fallback-results)]
                             [merged-modules (merge-modules-from-scan manifest-data fallback-results)]
                             [m1 (alist-set manifest-data 'exports merged-exports)]
                             [m2 (alist-set m1 'modules merged-modules)])
                        m2))
                  (let* ([merged-exports (merge-exports-from-scan manifest-data all-scan-results)]
                         [merged-modules (merge-modules-from-scan manifest-data all-scan-results)]
                         [m1 (alist-set manifest-data 'exports merged-exports)]
                         [m2 (alist-set m1 'modules merged-modules)])
                    m2)))))))

(doc kg-extract-type-sigs! 'type (-> Void))
(doc kg-extract-type-sigs! 'description "Scan lattice source files for (doc fn 'type ...) forms.
Also extracts contextual (doc 'type ...) forms from function define bodies.
Only considers symbols that are known exports in the KG.")
(define (doc-contextual-type-expr sexp)
  (if (and (pair? sexp)
           (eq? (car sexp) 'doc)
           (pair? (cdr sexp)))
      (let ([args (cdr sexp)])
        (if (and (pair? (car args))
                 (eq? (caar args) 'quote)
                 (eq? (cadar args) 'type)
                 (pair? (cdr args)))
            (cadr args)
            #f))
      #f))

(define (define-target-symbol sexp)
  (if (and (pair? sexp)
           (eq? (car sexp) 'define)
           (pair? (cdr sexp)))
      (let ([head (cadr sexp)])
        (cond
          [(symbol? head) head]
          [(and (pair? head) (symbol? (car head))) (car head)]
          [else #f]))
      #f))

(define (define-contextual-type-pair sexp)
  (let ([target (define-target-symbol sexp)])
    (if (not target)
        #f
        (let ([body (cddr sexp)])
          (let loop ([forms body])
            (if (null? forms)
                #f
                (let ([type-expr (doc-contextual-type-expr (car forms))])
                  (if type-expr
                      (cons target type-expr)
                      (loop (cdr forms))))))))))

(define (kg-extract-type-sigs!)
  (let ([known-exports (let loop ([exports (kg-exports)] [seen hamt-empty])
                         (if (null? exports) seen
                             (loop (cdr exports)
                                   (hamt-assoc (caar exports) #t seen))))]
        [seen-targets hamt-empty]
        [type-pairs '()])
    (define (normalize-type-expr type-expr)
      (if (and (pair? type-expr)
               (eq? (car type-expr) 'quote)
               (pair? (cdr type-expr)))
          (cadr type-expr)
          type-expr))
    (define (valid-type-expr? type-expr)
      (or (symbol? type-expr) (pair? type-expr)))
    (define (capture-type! target type-expr)
      (let ([norm (normalize-type-expr type-expr)])
        (when (and (symbol? target)
                   (hamt-lookup target known-exports)
                   (valid-type-expr? norm)
                   (not (hamt-lookup target seen-targets)))
          (set! seen-targets (hamt-assoc target #t seen-targets))
          (set! type-pairs (cons (cons target norm) type-pairs)))))
    ;; Scan source files under lattice/, core/, and boundary/.
    (for-each
     (lambda (root)
       (let ([files (find-scheme-files root)])
         (for-each
          (lambda (file)
            (guard (e [else (void)])  ; skip files that fail to parse
              ;; Pass 1: targeted doc forms: (doc fn 'type ...)
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
                                (pair? content))
                       (capture-type! target (car content)))))
                 docs))
              ;; Pass 2: contextual doc forms inside defines:
              ;; (define (fn ...) (doc 'type ...) ...)
              (let ([sexps (read-all-sexps file)])
                (when (and sexps (list? sexps))
                  (for-each
                   (lambda (sexp)
                     (let ([pair (define-contextual-type-pair sexp)])
                       (when pair
                         (capture-type! (car pair) (cdr pair)))))
                   sexps)))))
          files)))
     '("lattice" "core" "boundary"))
    (kg-populate-types! (reverse type-pairs))))

;;; kg-load-concept-ontology! : -> Void
;;; Load the concept ontology from disk and install it. Guarded — if the file
;;; is absent or malformed, prints a note and continues without ontology support.
;;; Called internally by kg-build! and kg-incremental-build! before concept extraction.
(define (kg-load-concept-ontology!)
  (guard (e [else
             (printf "  Note: concept ontology not loaded (~a)\n" (condition-message e))])
    (when (file-exists? "lattice/meta/concept-ontology.sexp")
      (let ([ontology (call-with-input-file "lattice/meta/concept-ontology.sexp" read)])
        (install-concept-ontology! ontology)
        (printf "  Concept ontology loaded: ~a concepts\n" (length (concept-all)))))))

(doc kg-build! 'type (-> Bytevector))
(doc kg-build! 'description "Build knowledge graph from all manifests in lattice/.
Discovers manifest files, parses them, creates blocks in CAS, extracts
concepts from keywords (normalized via ontology when available), type signatures
from doc forms, and builds the root. Returns root hash.")
(define (kg-build!)
  (kg-reset!)
  (let ([manifests (find-manifests "lattice")])
    (printf "Found ~a manifests\n" (length manifests))
    (let ([enriched-skills 0]
          [added-exports 0]
          [added-modules 0])
    ;; Phase 1: Add all skills (creates skill/module/export blocks in CAS)
    (for-each
     (lambda (manifest-path)
       (let* ([sexp (read-manifest-sexp manifest-path)]
              [data0 (if sexp (parse-manifest sexp) #f)])
         (let ([data (if data0 (kg-enrich-manifest-from-source data0) #f)])
         (when data
           (let* ([before-e (length (manifest-export-symbols data0))]
                  [after-e (length (manifest-export-symbols data))]
                  [mods0 (let ([m (assq 'modules data0)])
                           (if (and m (list? (cdr m))) (length (cdr m)) 0))]
                  [mods1 (let ([m (assq 'modules data)])
                           (if (and m (list? (cdr m))) (length (cdr m)) 0))])
             (when (> after-e before-e)
               (set! enriched-skills (+ enriched-skills 1))
               (set! added-exports (+ added-exports (- after-e before-e))))
             (when (> mods1 mods0)
               (set! added-modules (+ added-modules (- mods1 mods0)))))
           (printf "  Loading: ~a\n" (cdr (assq 'name data)))
           (kg-add-skill! data)))))
     manifests)
    (when (> enriched-skills 0)
      (printf "  Enriched ~a skills from source (+~a exports, +~a modules)\n"
              enriched-skills added-exports added-modules))
    ;; Phase 2: Build dependency edges
    (kg-build-deps!)
    ;; Phase 2b: Load concept ontology (must happen before concept extraction)
    (kg-load-concept-ontology!)
    ;; Phase 3: Extract concepts from keywords (KG-first: concepts are first-class)
    (kg-extract-concepts!)
    ;; Phase 4: Extract type signatures from doc forms
    (kg-extract-type-sigs!)
    ;; Phase 5: Build root block (anchors the entire graph in CAS)
    (let ([root-hash (kg-build-root!)])
      ;; Phase 6: Persist root hash + fingerprints for fast reload and incremental builds
      (kg-save-root! root-hash)
      (let ([fps (kg-compute-all-fingerprints manifests)])
        (kg-save-fingerprints! fps))
      (printf "Knowledge graph built: ~a skills, ~a modules, ~a exports, ~a concepts, ~a type-sigs, ~a edges\n"
              (length *kg-skills*)
              (length *kg-modules*)
              (length *kg-exports*)
              (length *kg-concepts*)
              (hamt-size *kg-type-sigs*)
              (length *kg-edges*))
      root-hash))))

(doc kg-ensure! 'type (-> (Maybe Bytevector)))
(doc kg-ensure! 'description "Initialize the knowledge graph. Tries three strategies in order:
1. Already initialized → no-op
2. CAS load + incremental update (fast path)
3. Full rebuild from manifests (fallback)")
(define (kg-ensure!)
  (cond
    [(kg-initialized?) #f]
    [(kg-incremental-build!) => (lambda (root) root)]
    [else (kg-build!)]))

;;; ====
;;; Incremental Build
;;; ====

(doc kg-extract-type-sigs-for-paths! 'type (-> (List String) Void))
(doc kg-extract-type-sigs-for-paths! 'description "Extract type signatures only from source files
under the given directory paths. Merges results into existing *kg-type-sigs* HAMT
rather than replacing. Used by incremental builds to scope type extraction.")
(define (kg-extract-type-sigs-for-paths! paths)
  (let ([known-exports (let loop ([exports (kg-exports)] [seen hamt-empty])
                         (if (null? exports) seen
                             (loop (cdr exports)
                                   (hamt-assoc (caar exports) #t seen))))]
        [seen-targets (let ([entries (hamt-entries *kg-type-sigs*)])
                        (fold-left
                         (lambda (m pair) (hamt-assoc (car pair) #t m))
                         hamt-empty
                         entries))]
        [type-pairs '()])
    (define (normalize-type-expr type-expr)
      (if (and (pair? type-expr)
               (eq? (car type-expr) 'quote)
               (pair? (cdr type-expr)))
          (cadr type-expr)
          type-expr))
    (define (valid-type-expr? type-expr)
      (or (symbol? type-expr) (pair? type-expr)))
    (define (capture-type! target type-expr)
      (let ([norm (normalize-type-expr type-expr)])
        (when (and (symbol? target)
                   (hamt-lookup target known-exports)
                   (valid-type-expr? norm)
                   (not (hamt-lookup target seen-targets)))
          (set! seen-targets (hamt-assoc target #t seen-targets))
          (set! type-pairs (cons (cons target norm) type-pairs)))))
    (for-each
     (lambda (root)
       (when (file-exists? root)
         (let ([files (find-scheme-files root)])
           (for-each
            (lambda (file)
              (guard (e [else (void)])
                (let ([docs (extract-docs-from-file file)])
                  (for-each
                   (lambda (doc-entry)
                     (let ([tag (caddr doc-entry)]
                           [content (cadddr doc-entry)]
                           [target (if (> (length doc-entry) 4) (list-ref doc-entry 4) #f)])
                       (when (and (eq? tag 'type) target (symbol? target) (pair? content))
                         (capture-type! target (car content)))))
                   docs))
                (let ([sexps (read-all-sexps file)])
                  (when (and sexps (list? sexps))
                    (for-each
                     (lambda (sexp)
                       (let ([pair (define-contextual-type-pair sexp)])
                         (when pair
                           (capture-type! (car pair) (cdr pair)))))
                     sexps)))))
            files))))
     paths)
    ;; Merge new types into existing map
    (for-each
     (lambda (pair)
       (unless (hamt-lookup (car pair) *kg-type-sigs*)
         (set! *kg-type-sigs* (hamt-assoc (car pair) (cdr pair) *kg-type-sigs*))))
     (reverse type-pairs))))

(doc kg-incremental-build! 'type (-> (Maybe Bytevector)))
(doc kg-incremental-build! 'description "Incrementally update the KG by detecting changed manifests.
Loads existing KG from CAS, computes current fingerprints, and only rebuilds
skills that changed, were added, or were removed. Returns root hash on success,
#f if incremental build is not possible (no existing KG or CAS load failure).")
(define (kg-incremental-build!)
  ;; Step 1: Try loading existing KG from CAS
  (let ([loaded (kg-load-from-root!)])
    (if (not loaded)
        #f  ; No existing KG — caller should fall back to full build
        ;; Step 1b: Load concept ontology (needed for hierarchy queries even if no changes)
        (begin
          (kg-load-concept-ontology!)
          ;; Step 2: Detect changes
          (let* ([manifests (find-manifests "lattice")]
               [current-fps (kg-compute-all-fingerprints manifests)]
               [cached-fps (kg-load-fingerprints)])
          (if (null? cached-fps)
              ;; No cached fingerprints — can't do incremental, but KG is loaded.
              ;; Save fingerprints for next time and return current root.
              (begin
                (kg-save-fingerprints! current-fps)
                (printf "KG: no cached fingerprints, saving baseline for next build\n")
                *kg-index-root*)
              (call-with-values
               (lambda () (kg-detect-changes current-fps cached-fps))
               (lambda (changed added removed)
                 (if (and (null? changed) (null? added) (null? removed))
                     ;; No changes — KG is up to date
                     (begin
                       (printf "KG: no changes detected, using cached build\n")
                       *kg-index-root*)
                     ;; Step 3: Apply changes
                     (begin
                       (printf "KG incremental: ~a changed, ~a added, ~a removed\n"
                               (length changed) (length added) (length removed))
                       ;; Remove old versions of changed + removed skills
                       (for-each kg-remove-skill! changed)
                       (for-each kg-remove-skill! removed)
                       ;; Rebuild manifest path lookup for changed + added skills
                       (let ([manifest-map
                              (fold-left
                               (lambda (m path)
                                 (guard (e [else m])
                                   (let* ([sexp (read-manifest-sexp path)]
                                          [data (if sexp (parse-manifest sexp) #f)])
                                     (if data
                                         (cons (cons (cdr (assq 'name data)) path) m)
                                         m))))
                               '()
                               manifests)]
                             [affected-paths '()])
                         ;; Re-add changed + added skills
                         (for-each
                          (lambda (skill-name)
                            (let ([entry (assq skill-name manifest-map)])
                              (when entry
                                (let* ([manifest-path (cdr entry)]
                                       [sexp (read-manifest-sexp manifest-path)]
                                       [data0 (if sexp (parse-manifest sexp) #f)]
                                       [data (if data0 (kg-enrich-manifest-from-source data0) #f)])
                                  (when data
                                    (printf "  Rebuilding: ~a\n" skill-name)
                                    (kg-add-skill! data)
                                    (let ([sp (assq 'path data)])
                                      (when (and sp (string? (cdr sp)))
                                        (set! affected-paths
                                              (cons (cdr sp) affected-paths)))))))))
                          (append changed added))
                         ;; Rebuild deps + concepts (cheap, in-memory)
                         ;; Clear deps and concepts first since they reference
                         ;; the old state, then rebuild from current skills
                         (set! *kg-deps* '())
                         (set! *kg-concepts* '())
                         (set! *kg-edges* '())
                         (set! *kg-skill-concepts* hamt-empty)
                         (set! *kg-concept-skills* hamt-empty)
                         (kg-build-deps!)
                         ;; Load ontology before concept extraction (idempotent if already loaded)
                         (kg-load-concept-ontology!)
                         (kg-extract-concepts!)
                         ;; Targeted type extraction (only scan changed skill dirs)
                         (unless (null? affected-paths)
                           (kg-extract-type-sigs-for-paths! affected-paths))
                         ;; Build new root
                         (let ([root-hash (kg-build-root!)])
                           (kg-save-root! root-hash)
                           (kg-save-fingerprints! current-fps)
                           (printf "KG incrementally updated: ~a skills, ~a concepts, ~a type-sigs\n"
                                   (length *kg-skills*)
                                   (length *kg-concepts*)
                                   (hamt-size *kg-type-sigs*))
                           root-hash))))))))))))

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
          (when (and *kg-hydrate-cas-on-load?*
                     (top-level-bound? 'hydrate-cas!)
                     (not (stored? root-hash)))
            (let ([loaded (hydrate-cas!)])
              (when (> loaded 0)
                (printf "Hydrated CAS from disk: ~a blocks\n" loaded))))
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
