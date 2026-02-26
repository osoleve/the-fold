(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))
(unless (top-level-bound? 'sha256) (load "core/base/sha256.ss"))
(unless (top-level-bound? 'make-block) (load "core/blocks/block.ss"))
(unless (top-level-bound? 'store!) (load "core/blocks/cas.ss"))
(unless (top-level-bound? 'read-manifest) (load "lattice/meta/manifest.ss"))
(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))
(unless (top-level-bound? 'concept-normalize) (load "lattice/meta/concept-normalize.ss"))

(doc 'module 'kg)
(doc 'description "Knowledge graph — the source of truth for the skill lattice.
Block-backed, CAS-stored, content-addressed. Skills, modules, exports,
concepts, and typed relationships are all blocks in the CAS. The root
hash is the identity of the entire lattice state.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Quiet Load Mode
;;; ====

(when (not (top-level-bound? '*meta-quiet*))
      (set-top-level-value! '*meta-quiet* #f))

(define (meta-printf . args)
  (unless (top-level-value '*meta-quiet*)
          (apply printf args)))

;;; ====
;;; Entity Types (Nodes)
;;; ====

(define KG-SKILL 'kg/skill)
(define KG-MODULE 'kg/module)
(define KG-EXPORT 'kg/export)
(define KG-CONCEPT 'kg/concept)

;;; ====
;;; Relationship Types (Edges)
;;; ====

(define KG-DEPENDS-ON 'kg/depends-on)
(define KG-PROVIDES-CONCEPT 'kg/provides-concept)
(define KG-BRIDGES 'kg/bridges)
(define KG-CONCEPT-IS-A 'kg/concept-is-a)

;;; ====
;;; Index Types (Structure)
;;; ====

(define KG-SKILL-INDEX 'kg/skill-index)
(define KG-CONCEPT-INDEX 'kg/concept-index)
(define KG-EDGE-INDEX 'kg/edge-index)
(define KG-TYPE-INDEX 'kg/type-index)
(define KG-ROOT 'kg/root)

;;; Legacy aliases for backward compatibility
(define KG-INDEX-ROOT KG-ROOT)

;;; ====
;;; Entity Creation
;;; ====

(doc make-skill-entity 'type (-> ManifestData Block))
(doc make-skill-entity 'description "Create a skill entity block from manifest data.
Stores the FULL manifest alist as payload so the KG can be reconstructed
entirely from CAS without needing manifest files.")
(define (make-skill-entity manifest-data)
  (let ([payload (format "~s" manifest-data)])
    (make-block KG-SKILL
                (string->utf8 payload)
                (vector))))

(doc make-module-entity 'type (-> Symbol String String Block Block))
(doc make-module-entity 'description "Create a module entity block linked to its skill")
(define (make-module-entity name file description skill-block)
  (let ([payload (format "~s" `((name . ,name)
                                (file . ,file)
                                (description . ,description)))])
       (make-block KG-MODULE
                   (string->utf8 payload)
                   (vector (hash-block skill-block)))))

(doc make-export-entity 'type (-> Symbol Block Block))
(doc make-export-entity 'description "Create an export entity block linked to its module")
(define (make-export-entity name module-block)
  (let ([payload (format "~s" `((name . ,name)))])
       (make-block KG-EXPORT
                   (string->utf8 payload)
                   (vector (hash-block module-block)))))

(doc make-concept-entity 'type (-> Symbol String (List Symbol) Block))
(doc make-concept-entity 'description "Create a concept node — an abstract idea that links skills.
Concepts are the cross-cutting themes of the lattice: 'gradient', 'optimization',
'factorization', etc. They exist independently of any skill and provide semantic
bridging that keyword matching cannot.")
(define (make-concept-entity name description keywords)
  (let ([payload (format "~s" `((name . ,name)
                                (description . ,description)
                                (keywords . ,keywords)))])
       (make-block KG-CONCEPT
                   (string->utf8 payload)
                   (vector))))

;;; ====
;;; Relationship Creation
;;; ====

(doc make-depends-on-relation 'type (-> Block Block Block))
(doc make-depends-on-relation 'description "Create a skill→skill dependency edge")
(define (make-depends-on-relation from-skill to-skill)
  (make-block KG-DEPENDS-ON
              (string->utf8 "")
              (vector (hash-block from-skill)
                      (hash-block to-skill))))

(doc make-provides-concept-relation 'type (-> Block Block Block))
(doc make-provides-concept-relation 'description "Create a skill→concept edge (skill provides/implements concept)")
(define (make-provides-concept-relation skill-block concept-block)
  (make-block KG-PROVIDES-CONCEPT
              (string->utf8 "")
              (vector (hash-block skill-block)
                      (hash-block concept-block))))

(doc make-bridges-relation 'type (-> Block Block Block Block))
(doc make-bridges-relation 'description "Create a module→(concept,concept) bridge edge.
A bridge module connects two domains. For example, traced-optics bridges
optics and autodiff. The module block is the bridge, and the two concept
blocks are the domains being bridged.")
(define (make-bridges-relation module-block concept-a concept-b)
  (make-block KG-BRIDGES
              (string->utf8 "")
              (vector (hash-block module-block)
                      (hash-block concept-a)
                      (hash-block concept-b))))

(doc make-concept-is-a-relation 'type (-> Block Block Block))
(doc make-concept-is-a-relation 'description "Create a concept→parent-concept hierarchy edge.
Encodes the is-a relationship from child concept to parent concept, enabling
transitive ancestor queries and cross-skill concept bridging through the hierarchy.")
(define (make-concept-is-a-relation child-block parent-block)
  (make-block KG-CONCEPT-IS-A
              (string->utf8 "")
              (vector (hash-block child-block)
                      (hash-block parent-block))))

;;; ====
;;; Index Creation
;;; ====

(doc make-skill-index 'type (-> (List Bytevector) Block))
(doc make-skill-index 'description "Create skill index block (refs all skill hashes)")
(define (make-skill-index skill-hashes)
  (make-block KG-SKILL-INDEX
              (string->utf8 (format "~a" (length skill-hashes)))
              (list->vector skill-hashes)))

(doc make-concept-index 'type (-> (List Bytevector) Block))
(doc make-concept-index 'description "Create concept index block (refs all concept hashes)")
(define (make-concept-index concept-hashes)
  (make-block KG-CONCEPT-INDEX
              (string->utf8 (format "~a" (length concept-hashes)))
              (list->vector concept-hashes)))

(doc make-edge-index 'type (-> (List Bytevector) Block))
(doc make-edge-index 'description "Create edge index block (refs all relationship hashes)")
(define (make-edge-index edge-hashes)
  (make-block KG-EDGE-INDEX
              (string->utf8 (format "~a" (length edge-hashes)))
              (list->vector edge-hashes)))

(doc make-kg-root 'type (-> Block Block Block Block Block))
(doc make-kg-root 'description "Create KG root block. refs[0]=skill-index, refs[1]=concept-index,
refs[2]=edge-index, refs[3]=type-index. The root hash IS the identity of the entire knowledge graph.")
(define (make-kg-root skill-idx concept-idx edge-idx type-idx)
  (make-block KG-ROOT
              (string->utf8 (format "~s" `((version . 3))))
              (vector (hash-block skill-idx)
                      (hash-block concept-idx)
                      (hash-block edge-idx)
                      (hash-block type-idx))))

;;; ====
;;; CAS Storage
;;; ====

(doc kg-store-block! 'type (-> Block Bytevector))
(doc kg-store-block! 'description "Store a KG block in the CAS and return its hash.
If CAS persistence is available, also persists to disk.")
(define (kg-store-block! blk)
  (if (top-level-bound? 'store-persistent!)
      (store-persistent! blk)
      (store! blk)))

;;; ====
;;; Knowledge Graph State
;;; ====

;;; In-memory knowledge graph — populated from CAS or built from manifests.
;;; These alists serve as the query-layer cache. All blocks are also in the CAS.
(define *kg-skills* '())        ; ((name . block) ...)
(define *kg-modules* '())       ; ((skill/module . block) ...)
(define *kg-exports* '())       ; ((symbol . block) ...)
(define *kg-deps* '())          ; (relation-block ...)
(define *kg-concepts* '())      ; ((name . block) ...)
(define *kg-edges* '())         ; (edge-block ...)
(define *kg-index-root* #f)     ; Root block hash (CAS identity)
(define *kg-skill-data* '())    ; ((name . manifest-data) ...) - for quick lookup
(define *kg-loaded* #f)         ; Explicit flag for initialization state

;;; Type signature map (populated from doc forms)
(define *kg-type-sigs* hamt-empty)   ; symbol → type-expr (e.g., make-vec2 → (-> Number Number Vec2))

;;; Concept reverse maps (populated during build)
(define *kg-skill-concepts* hamt-empty)   ; skill-name → (List concept-name)
(define *kg-concept-skills* hamt-empty)   ; concept-name → (List skill-name)

;;; Bridge detection cache (populated lazily by kg-detect-bridges)
(define *kg-bridge-cache* #f)             ; #f = not computed, list = cached results

(doc kg-reset! 'type (-> Void))
(doc kg-reset! 'description "Reset all knowledge graph state")
(define (kg-reset!)
  (set! *kg-skills* '())
  (set! *kg-modules* '())
  (set! *kg-exports* '())
  (set! *kg-deps* '())
  (set! *kg-concepts* '())
  (set! *kg-edges* '())
  (set! *kg-index-root* #f)
  (set! *kg-skill-data* '())
  (set! *kg-loaded* #f)
  (set! *kg-type-sigs* hamt-empty)
  (set! *kg-skill-concepts* hamt-empty)
  (set! *kg-concept-skills* hamt-empty)
  (set! *kg-bridge-cache* #f))

(doc kg-remove-skill! 'type (-> Symbol Void))
(doc kg-remove-skill! 'description "Remove a skill and all its associated state (modules, exports, edges)
from the in-memory KG. Used by incremental builds to replace changed skills.
Concept maps are NOT updated here — they are rebuilt in bulk after all changes.")
(define (kg-remove-skill! skill-name)
  (let* ([prefix (string-append (symbol->string skill-name) "/")]
         [skill-entry (assq skill-name *kg-skills*)]
         [skill-hash (and skill-entry (hash-block (cdr skill-entry)))])
    ;; Remove edges first (while we still have the skill block reference)
    (when skill-hash
      (set! *kg-deps*
            (filter (lambda (edge)
                      (let ([refs (block-refs edge)])
                        (not (or (bytevector=? (vector-ref refs 0) skill-hash)
                                 (bytevector=? (vector-ref refs 1) skill-hash)))))
                    *kg-deps*))
      (set! *kg-edges*
            (filter (lambda (edge)
                      (let ([refs (block-refs edge)])
                        (not (let check ([i 0])
                               (if (>= i (vector-length refs))
                                   #f
                                   (or (bytevector=? (vector-ref refs i) skill-hash)
                                       (check (+ i 1))))))))
                    *kg-edges*)))
    ;; Remove skill entity and manifest data
    (set! *kg-skills*
          (filter (lambda (e) (not (eq? (car e) skill-name))) *kg-skills*))
    (set! *kg-skill-data*
          (filter (lambda (e) (not (eq? (car e) skill-name))) *kg-skill-data*))
    ;; Remove modules belonging to this skill (keyed as "skill/module")
    (set! *kg-modules*
          (filter (lambda (e)
                    (not (string-starts-with? (symbol->string (car e)) prefix)))
                  *kg-modules*))))

;;; ====
;;; Graph Building
;;; ====

(doc kg-add-skill! 'type (-> ManifestData Block))
(doc kg-add-skill! 'description "Add a skill to the knowledge graph. Creates and stores blocks in CAS.")
(define (kg-add-skill! manifest-data)
  (let* ([name (cdr (assq 'name manifest-data))]
         [skill-block (make-skill-entity manifest-data)]
         [skill-hash (kg-store-block! skill-block)]
         [skill-path (cdr (assq 'path manifest-data))]
         [modules (cdr (assq 'modules manifest-data))]
         [exports (cdr (assq 'exports manifest-data))])
        ;; Add skill and its manifest data
        (set! *kg-skills* (cons (cons name skill-block) *kg-skills*))
        (set! *kg-skill-data* (cons (cons name manifest-data) *kg-skill-data*))
        ;; Add modules
        (for-each
         (lambda (mod)
                 (let ([entries (parse-module-entry mod skill-path)])
                      (for-each
                       (lambda (entry)
                               (let* ([mod-name (car entry)]
                                      [mod-file (cdr entry)]
                                      [mod-block (make-module-entity mod-name mod-file "" skill-block)]
                                      [mod-hash (kg-store-block! mod-block)]
                                      [mod-key (string->symbol (format "~a/~a" name mod-name))])
                                     (set! *kg-modules* (cons (cons mod-key mod-block) *kg-modules*))))
                       entries)))
         (if (list? modules) modules '()))
        ;; Add exports
        (for-each
         (lambda (export-group)
                 (when (pair? export-group)
                       (let* ([first-sym (car export-group)]
                              [mod-key (if (symbol? first-sym)
                                          (string->symbol (format "~a/~a" name first-sym))
                                          #f)]
                              [is-grouped (and mod-key (assq mod-key *kg-modules*))]
                              [mod-block (if is-grouped
                                            (cdr (assq mod-key *kg-modules*))
                                            skill-block)]
                              [export-syms (if is-grouped
                                              (cdr export-group)
                                              export-group)])
                             (for-each
                              (lambda (export-name)
                                      (when (symbol? export-name)
                                            (let* ([export-block (make-export-entity export-name mod-block)]
                                                   [export-hash (kg-store-block! export-block)])
                                                  (set! *kg-exports* (cons (cons export-name export-block) *kg-exports*)))))
                              export-syms))))
         (if (list? exports) exports '()))
        skill-block))

(doc kg-build-deps! 'type (-> Void))
(doc kg-build-deps! 'description "Build dependency relation blocks (call after all skills added)")
(define (kg-build-deps!)
  (for-each
   (lambda (skill-entry)
           (let* ([skill-name (car skill-entry)]
                  [skill-block (cdr skill-entry)]
                  [manifest-data (kg-skill-data skill-name)]
                  [deps (if manifest-data
                            (let ([d (assq 'deps manifest-data)])
                                 (if d (cdr d) '()))
                            '())])
                 (for-each
                  (lambda (dep-name)
                          (let ([dep-entry (assq dep-name *kg-skills*)])
                               (when dep-entry
                                     (let* ([dep-block (cdr dep-entry)]
                                            [rel (make-depends-on-relation skill-block dep-block)]
                                            [rel-hash (kg-store-block! rel)])
                                           (set! *kg-deps* (cons rel *kg-deps*))
                                           (set! *kg-edges* (cons rel *kg-edges*))))))
                  (if (list? deps) deps '()))))
   *kg-skills*))

;;; ====
;;; Concept Extraction
;;; ====

(doc kg-extract-concepts! 'type (-> Void))
(doc kg-extract-concepts! 'description "Extract concepts from skill keywords and aliases.
When the concept ontology is loaded (concept-ontology-loaded? is #t), keywords are
normalized to canonical concept names via concept-normalize, collapsing aliases into
a single canonical concept block and adding is-a hierarchy edges. Cross-cutting
concepts from the ontology are added even if no skill references them.
When the ontology is NOT loaded, falls back to flat keyword extraction (v2 behavior).
This is the key insight of the KG-first design: concepts exist
independently and skills are connected through them.")
(define (kg-extract-concepts!)
  (set! *kg-bridge-cache* #f)  ; invalidate — concept data is changing
  (if (concept-ontology-loaded?)
      ;; --- Ontology-aware path ---
      ;; Normalize keywords through the ontology, collapsing aliases to canonical
      ;; concept names before building the concept→skills map.
      (let ([concept-skills hamt-empty])  ; canonical-concept → (List skill-name)
        ;; Phase 1: collect canonical concept → skills mapping (with normalization)
        (for-each
         (lambda (skill-entry)
           (let* ([skill-name (car skill-entry)]
                  [data (kg-skill-data skill-name)]
                  [keywords (if data
                                (let ([k (assq 'keywords data)])
                                  (if (and k (list? (cdr k))) (cdr k) '()))
                                '())]
                  [aliases (if data
                               (let ([a (assq 'aliases data)])
                                 (if (and a (list? (cdr a))) (cdr a) '()))
                               '())]
                  [raw-tags (append keywords aliases)])
             (for-each
              (lambda (kw)
                (when (symbol? kw)
                  ;; Normalize to canonical concept — collapses aliases
                  (let* ([canonical (concept-normalize kw)]
                         [existing (or (hamt-lookup canonical concept-skills) '())])
                    (unless (memq skill-name existing)
                      (set! concept-skills
                            (hamt-assoc canonical (cons skill-name existing) concept-skills))))))
              raw-tags)))
         *kg-skills*)

        ;; Phase 2: create concept blocks for each canonical concept
        (let ([created-blocks hamt-empty])  ; canonical-name → block (for is-a edges)
          (for-each
           (lambda (kv)
             (let* ([concept-name (car kv)]
                    [skill-names (cdr kv)]
                    [desc (let ([d (concept-description concept-name)])
                            (if (string=? d "")
                                (format "Concept derived from manifest metadata: ~a" concept-name)
                                d))]
                    [concept-block (make-concept-entity concept-name desc (list concept-name))]
                    [concept-hash (kg-store-block! concept-block)])
               ;; Register concept
               (set! *kg-concepts* (cons (cons concept-name concept-block) *kg-concepts*))
               (set! created-blocks (hamt-assoc concept-name concept-block created-blocks))
               ;; Update reverse maps
               (set! *kg-concept-skills*
                     (hamt-assoc concept-name skill-names *kg-concept-skills*))
               (for-each
                (lambda (skill-name)
                  (let ([existing (or (hamt-lookup skill-name *kg-skill-concepts*) '())])
                    (set! *kg-skill-concepts*
                          (hamt-assoc skill-name
                                      (cons concept-name existing)
                                      *kg-skill-concepts*))))
                skill-names)
               ;; Create provides-concept edges
               (for-each
                (lambda (skill-name)
                  (let ([skill-entry (assq skill-name *kg-skills*)])
                    (when skill-entry
                      (let* ([rel (make-provides-concept-relation
                                   (cdr skill-entry) concept-block)]
                             [rel-hash (kg-store-block! rel)])
                        (set! *kg-edges* (cons rel *kg-edges*))))))
                skill-names)))
           (hamt-entries concept-skills))

          ;; Phase 3: add cross-cutting concepts and link them to their skills
          ;; hamt-entries returns ((concept-name . skill-list) ...) pairs
          (for-each
           (lambda (kv)
             (let* ([cc-concept (car kv)]
                    [cc-skills (cdr kv)]
                    ;; Only link skills that exist in the KG
                    [valid-skills (filter (lambda (s) (assq s *kg-skills*)) cc-skills)])
               ;; Create concept block if not already created
               (unless (assq cc-concept *kg-concepts*)
                 (let* ([desc (concept-description cc-concept)]
                        [concept-block (make-concept-entity cc-concept
                                                            (if (string=? desc "") (format "~a" cc-concept) desc)
                                                            (list cc-concept))]
                        [concept-hash (kg-store-block! concept-block)])
                   (set! *kg-concepts* (cons (cons cc-concept concept-block) *kg-concepts*))
                   (set! created-blocks (hamt-assoc cc-concept concept-block created-blocks))))
               ;; Update reverse maps: concept→skills and skill→concepts
               (let ([existing-skills (or (hamt-lookup cc-concept *kg-concept-skills*) '())])
                 (set! *kg-concept-skills*
                       (hamt-assoc cc-concept
                                   (fold-left (lambda (acc s) (if (memq s acc) acc (cons s acc)))
                                              existing-skills valid-skills)
                                   *kg-concept-skills*)))
               (for-each
                (lambda (skill-name)
                  (let ([existing (or (hamt-lookup skill-name *kg-skill-concepts*) '())])
                    (unless (memq cc-concept existing)
                      (set! *kg-skill-concepts*
                            (hamt-assoc skill-name
                                        (cons cc-concept existing)
                                        *kg-skill-concepts*)))))
                valid-skills)))
           (hamt-entries *cross-cutting-skills*))

          ;; Phase 4: create is-a hierarchy edges for all created concept blocks
          ;; For each concept block we created, if its parent is also a created block,
          ;; emit a KG-CONCEPT-IS-A edge.
          (for-each
           (lambda (kv)
             (let* ([concept-name (car kv)]
                    [concept-block (cdr kv)]
                    [parent (concept-parent concept-name)])
               (when parent
                 (let ([parent-block (hamt-lookup parent created-blocks)])
                   (when parent-block
                     (let* ([rel (make-concept-is-a-relation concept-block parent-block)]
                            [rel-hash (kg-store-block! rel)])
                       (set! *kg-edges* (cons rel *kg-edges*))))))))
           (hamt-entries created-blocks))))

      ;; --- Flat fallback path (ontology not loaded) ---
      ;; Identical to v2 behavior: no normalization, no hierarchy edges.
      (let ([keyword-skills hamt-empty])  ; keyword → (List skill-name)
        ;; Phase 1: collect concept → skills mapping
        (for-each
         (lambda (skill-entry)
           (let* ([skill-name (car skill-entry)]
                  [data (kg-skill-data skill-name)]
                  [keywords (if data
                                (let ([k (assq 'keywords data)])
                                  (if (and k (list? (cdr k))) (cdr k) '()))
                                '())]
                  [aliases (if data
                               (let ([a (assq 'aliases data)])
                                 (if (and a (list? (cdr a))) (cdr a) '()))
                               '())]
                  [concept-tags (append keywords aliases)])
             (for-each
              (lambda (kw)
                (when (symbol? kw)
                  (let ([existing (or (hamt-lookup kw keyword-skills) '())])
                    (unless (memq skill-name existing)
                      (set! keyword-skills
                            (hamt-assoc kw (cons skill-name existing) keyword-skills))))))
              concept-tags)))
         *kg-skills*)

        ;; Phase 2: create concept blocks for each concept tag
        (for-each
         (lambda (kv)
           (let* ([concept-name (car kv)]
                  [skill-names (cdr kv)]
                  [concept-block (make-concept-entity
                                  concept-name
                                  (format "Concept derived from manifest metadata: ~a" concept-name)
                                  (list concept-name))]
                  [concept-hash (kg-store-block! concept-block)])
             ;; Register concept
             (set! *kg-concepts* (cons (cons concept-name concept-block) *kg-concepts*))
             ;; Update reverse maps
             (set! *kg-concept-skills*
                   (hamt-assoc concept-name skill-names *kg-concept-skills*))
             (for-each
              (lambda (skill-name)
                (let ([existing (or (hamt-lookup skill-name *kg-skill-concepts*) '())])
                  (set! *kg-skill-concepts*
                        (hamt-assoc skill-name
                                    (cons concept-name existing)
                                    *kg-skill-concepts*))))
              skill-names)
             ;; Create provides-concept edges
             (for-each
              (lambda (skill-name)
                (let ([skill-entry (assq skill-name *kg-skills*)])
                  (when skill-entry
                    (let* ([rel (make-provides-concept-relation
                                 (cdr skill-entry) concept-block)]
                           [rel-hash (kg-store-block! rel)])
                      (set! *kg-edges* (cons rel *kg-edges*))))))
              skill-names)))
         (hamt-entries keyword-skills)))))

;;; ====
;;; Type Signature Population
;;; ====

(doc kg-populate-types! 'type (-> (List (Pair Symbol Any)) Void))
(doc kg-populate-types! 'description "Populate the type signature map from extracted (symbol . type-expr) pairs.
Called by boundary layer after scanning source files for (doc fn 'type ...) forms.")
(define (kg-populate-types! type-pairs)
  (set! *kg-type-sigs*
        (fold-left
         (lambda (m pair)
           (if (hamt-lookup (car pair) m)
               m  ; first-found wins
               (hamt-assoc (car pair) (cdr pair) m)))
         hamt-empty
         type-pairs)))

(doc make-type-index 'type (-> HAMT Block))
(doc make-type-index 'description "Create a type index block storing all type signatures.
Serializes the type HAMT as an alist in the payload.")
(define (make-type-index type-sigs-hamt)
  (let ([entries (hamt-entries type-sigs-hamt)])
    (make-block KG-TYPE-INDEX
                (string->utf8 (format "~s" entries))
                (vector))))

;;; ====
;;; Root Construction
;;; ====

(doc kg-build-root! 'type (-> Bytevector))
(doc kg-build-root! 'description "Build and store the root block. Returns root hash (the KG identity).")
(define (kg-build-root!)
  (let* ([skill-hashes (map (lambda (e) (hash-block (cdr e))) *kg-skills*)]
         [concept-hashes (map (lambda (e) (hash-block (cdr e))) *kg-concepts*)]
         [edge-hashes (map hash-block *kg-edges*)]
         [skill-idx (make-skill-index skill-hashes)]
         [concept-idx (make-concept-index concept-hashes)]
         [edge-idx (make-edge-index edge-hashes)]
         [type-idx (make-type-index *kg-type-sigs*)]
         [skill-idx-hash (kg-store-block! skill-idx)]
         [concept-idx-hash (kg-store-block! concept-idx)]
         [edge-idx-hash (kg-store-block! edge-idx)]
         [type-idx-hash (kg-store-block! type-idx)]
         [root (make-kg-root skill-idx concept-idx edge-idx type-idx)]
         [root-hash (kg-store-block! root)])
    (set! *kg-index-root* root-hash)
    (set! *kg-loaded* #t)
    root-hash))

;;; ====
;;; Query API
;;; ====

(doc kg-skill-data 'type (-> Symbol (Maybe ManifestData)))
(doc kg-skill-data 'description "Get manifest data for a skill")
(define (kg-skill-data skill-name)
  (let ([entry (assq skill-name *kg-skill-data*)])
       (if entry (cdr entry) #f)))

;;; NOTE: kg-build! and kg-ensure! are in boundary/meta/kg-io.ss

(doc kg-skills 'type (-> (List Symbol)))
(doc kg-skills 'description "Get list of all skill names")
(define (kg-skills)
  (map car *kg-skills*))

(doc kg-initialized? 'type (-> Boolean))
(doc kg-initialized? 'description "Check if the knowledge graph has been built")
(define (kg-initialized?)
  *kg-loaded*)

(doc kg-skill 'type (-> Symbol (Maybe Block)))
(doc kg-skill 'description "Get skill block by name")
(define (kg-skill name)
  (let ([entry (assq name *kg-skills*)])
       (if entry (cdr entry) #f)))

(doc kg-modules 'type (-> Symbol (List (Pair Symbol Block))))
(doc kg-modules 'description "Get modules for a skill")
(define (kg-modules skill-name)
  (filter
   (lambda (entry)
           (let ([key (symbol->string (car entry))])
                (string-starts-with? key (string-append (symbol->string skill-name) "/"))))
   *kg-modules*))

(doc kg-exports 'type (-> (List (Pair Symbol Block))))
(doc kg-exports 'description "Get all exports (deduplicated by name)")
(define (kg-exports)
  (dedupe-by-car *kg-exports*))

(define (dedupe-by-car lst)
  (let loop ([remaining lst] [seen hamt-empty] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([name (caar remaining)])
          (if (hamt-has-key? name seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining)
                    (hamt-assoc name #t seen)
                    (cons (car remaining) acc)))))))

(doc kg-deps 'type (-> Symbol (List Symbol)))
(doc kg-deps 'description "Get dependencies for a skill")
(define (kg-deps skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if data
           (let ([d (assq 'deps data)])
                (if (and d (list? (cdr d))) (cdr d) '()))
           '())))

(doc kg-uses 'type (-> Symbol (List Symbol)))
(doc kg-uses 'description "Get skills that depend on this skill")
(define (kg-uses skill-name)
  (filter-map
   (lambda (entry)
           (let* ([name (car entry)]
                  [deps (kg-deps name)])
                 (if (memq skill-name deps)
                     name
                     #f)))
   *kg-skills*))

;;; ====
;;; Concept Query API
;;; ====

(doc kg-concepts 'type (-> (List Symbol)))
(doc kg-concepts 'description "Get list of all concept names")
(define (kg-concepts)
  (map car *kg-concepts*))

(doc kg-concept 'type (-> Symbol (Maybe Block)))
(doc kg-concept 'description "Get concept block by name")
(define (kg-concept name)
  (let ([entry (assq name *kg-concepts*)])
    (if entry (cdr entry) #f)))

(doc kg-concept-skills 'type (-> Symbol (List Symbol)))
(doc kg-concept-skills 'description "Get skills that provide a given concept")
(define (kg-concept-skills concept-name)
  (or (hamt-lookup concept-name *kg-concept-skills*) '()))

(doc kg-skill-concepts 'type (-> Symbol (List Symbol)))
(doc kg-skill-concepts 'description "Get concepts provided by a given skill")
(define (kg-skill-concepts skill-name)
  (or (hamt-lookup skill-name *kg-skill-concepts*) '()))

(doc kg-shared-concepts 'type (-> Symbol Symbol (List Symbol)))
(doc kg-shared-concepts 'description "Get concepts shared between two skills (semantic overlap)")
(define (kg-shared-concepts skill-a skill-b)
  (let ([concepts-a (kg-skill-concepts skill-a)]
        [concepts-b (kg-skill-concepts skill-b)])
    (filter (lambda (c) (memq c concepts-b)) concepts-a)))

(doc kg-concept-bridge 'type (-> Symbol Symbol (List Symbol)))
(doc kg-concept-bridge 'description "Find concepts that bridge two skills (shared or transitive).
If skills share concepts directly, return those. Otherwise look for
concepts reachable through 1-hop skill connections.")
(define (kg-concept-bridge skill-a skill-b)
  (let ([direct (kg-shared-concepts skill-a skill-b)])
    (if (not (null? direct))
        direct
        ;; Try 1-hop: concepts of A's deps that overlap with B's concepts
        (let* ([a-deps (kg-deps skill-a)]
               [b-concepts (kg-skill-concepts skill-b)]
               [hop-concepts '()])
          (for-each
           (lambda (dep)
             (let ([dep-concepts (kg-skill-concepts dep)])
               (for-each
                (lambda (c)
                  (when (and (memq c b-concepts)
                             (not (memq c hop-concepts)))
                    (set! hop-concepts (cons c hop-concepts))))
                dep-concepts)))
           a-deps)
          hop-concepts))))

(doc kg-related-by-concept 'type (-> Symbol Int (List (Pair Symbol Int))))
(doc kg-related-by-concept 'description "Find skills related to this one through shared concepts.
Returns (skill-name . shared-concept-count) sorted by overlap.")
(define (kg-related-by-concept skill-name k)
  (let* ([my-concepts (kg-skill-concepts skill-name)]
         [scores hamt-empty])
    ;; For each of my concepts, count skills that share it
    (for-each
     (lambda (concept)
       (for-each
        (lambda (other-skill)
          (unless (eq? other-skill skill-name)
            (let ([current (or (hamt-lookup other-skill scores) 0)])
              (set! scores (hamt-assoc other-skill (+ current 1) scores)))))
        (kg-concept-skills concept)))
     my-concepts)
    ;; Sort by count descending, take top k
    (let* ([entries (hamt-entries scores)]
           [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) entries)])
      (if (<= (length sorted) k)
          sorted
          (let loop ([lst sorted] [n 0] [acc '()])
            (if (or (null? lst) (= n k))
                (reverse acc)
                (loop (cdr lst) (+ n 1) (cons (car lst) acc))))))))

;;; ====
;;; Hierarchical Concept Query API
;;; ====

(doc kg-concept-ancestors 'type (-> Symbol (List Symbol)))
(doc kg-concept-ancestors 'description "Return the transitive ancestor chain for a concept in the
ontology hierarchy, closest-first. Delegates to concept-ancestors from the concept-normalize module.
Returns '() if the ontology is not loaded or the concept is a root/unknown.")
(define (kg-concept-ancestors concept-name)
  (if (concept-ontology-loaded?)
      (concept-ancestors concept-name)
      '()))

(doc kg-concept-descendants 'type (-> Symbol (List Symbol)))
(doc kg-concept-descendants 'description "Return all transitive descendants of a concept in the
ontology hierarchy. Returns '() if the ontology is not loaded.
Iterative BFS — handles arbitrary depth without stack growth.")
(define (kg-concept-descendants concept-name)
  (if (not (concept-ontology-loaded?))
      '()
      (let loop ([queue (concept-children concept-name)]
                 [visited hamt-empty]
                 [acc '()])
        (if (null? queue)
            (reverse acc)
            (let ([child (car queue)])
              (if (hamt-has-key? child visited)
                  (loop (cdr queue) visited acc)
                  (loop (append (cdr queue) (concept-children child))
                        (hamt-assoc child #t visited)
                        (cons child acc))))))))

(doc kg-transitive-concept-skills 'type (-> Symbol (List Symbol)))
(doc kg-transitive-concept-skills 'description "Return all skills connected to this concept OR any
of its descendant concepts in the hierarchy. Useful for finding all skills that work
within a broad domain (e.g., 'linear-algebra' catches eigenvalue-theory, matrix-theory, etc.).
Returns direct concept-skills if ontology is not loaded.")
(define (kg-transitive-concept-skills concept-name)
  (let ([direct (kg-concept-skills concept-name)])
    (if (not (concept-ontology-loaded?))
        direct
        (let ([descendants (kg-concept-descendants concept-name)])
          (fold-left
           (lambda (acc desc)
             (let ([desc-skills (kg-concept-skills desc)])
               (fold-left
                (lambda (a s)
                  (if (memq s a) a (cons s a)))
                acc
                desc-skills)))
           direct
           descendants)))))

(doc kg-shared-concepts-transitive 'type (-> Symbol Symbol (List Symbol)))
(doc kg-shared-concepts-transitive 'description "Like kg-shared-concepts but also checks ancestor
concepts. If skill A provides 'eigenvalue-theory' and skill B provides 'linear-algebra',
the transitive version finds the connection through the hierarchy: eigenvalue-theory
is-a linear-algebra.
If ontology is not loaded, falls back to kg-shared-concepts (flat check).")
(define (kg-shared-concepts-transitive skill-a skill-b)
  (if (not (concept-ontology-loaded?))
      (kg-shared-concepts skill-a skill-b)
      (let ([concepts-a (kg-skill-concepts skill-a)]
            [concepts-b (kg-skill-concepts skill-b)])
        ;; Expand each concept list with its ancestors, then intersect
        ;; The inner (if (memq c acc) acc (cons c acc)) deduplicates concepts
        ;; that appear both as keywords and as ancestors of other keywords.
        (let ([expanded-a
               (fold-left
                (lambda (acc c)
                  (let ([ancs (kg-concept-ancestors c)]
                        [acc (if (memq c acc) acc (cons c acc))])
                    (fold-left (lambda (a x) (if (memq x a) a (cons x a))) acc ancs)))
                '()
                concepts-a)]
              [concepts-b-set
               (fold-left
                (lambda (acc c)
                  (let ([ancs (kg-concept-ancestors c)]
                        [acc (if (memq c acc) acc (cons c acc))])
                    (fold-left (lambda (a x) (if (memq x a) a (cons x a))) acc ancs)))
                '()
                concepts-b)])
          (filter (lambda (c) (memq c concepts-b-set)) expanded-a)))))


(load "lattice/meta/kg-bridge.ss")


;;; ====
;;; Type Query API
;;; ====

(doc kg-export-type 'type (-> Symbol (Maybe SExpr)))
(doc kg-export-type 'description "Get the type signature for an export. Returns the type expression
(e.g., (-> Matrix Vector)) or #f if no type annotation exists.")
(define (kg-export-type sym)
  (hamt-lookup sym *kg-type-sigs*))

(doc kg-typed-exports 'type (-> (List (Pair Symbol SExpr))))
(doc kg-typed-exports 'description "Get all exports that have type signatures")
(define (kg-typed-exports)
  (hamt-entries *kg-type-sigs*))

(doc kg-type-count 'type (-> Int))
(doc kg-type-count 'description "Number of exports with type annotations")
(define (kg-type-count)
  (hamt-size *kg-type-sigs*))

;;; ====
;;; Statistics
;;; ====

(doc kg-stats 'type (-> Alist))
(doc kg-stats 'description "Get knowledge graph statistics")
(define (kg-stats)
  `((skills . ,(length *kg-skills*))
    (modules . ,(length *kg-modules*))
    (exports . ,(length *kg-exports*))
    (concepts . ,(length *kg-concepts*))
    (type-sigs . ,(hamt-size *kg-type-sigs*))
    (dependencies . ,(length *kg-deps*))
    (edges . ,(length *kg-edges*))
    (root . ,*kg-index-root*)))

;;; ====
;;; CAS Hydration (load KG from stored blocks)
;;; ====

(doc kg-rebuild-concept-maps! 'type (-> Void))
(doc kg-rebuild-concept-maps! 'description "Rebuild concept reverse maps from manifest keywords and aliases.
Used during CAS hydration to restore *kg-skill-concepts* and *kg-concept-skills*
without traversing edge blocks. When the ontology is loaded, keywords are normalized
to canonical concept names before the lookup, matching the behavior of kg-extract-concepts!.")
(define (kg-rebuild-concept-maps!)
  (for-each
   (lambda (skill-entry)
     (let* ([skill-name (car skill-entry)]
            [data (kg-skill-data skill-name)]
            [keywords (if data
                         (let ([k (assq 'keywords data)])
                           (if (and k (list? (cdr k))) (cdr k) '()))
                         '())]
            [aliases (if data
                        (let ([a (assq 'aliases data)])
                          (if (and a (list? (cdr a))) (cdr a) '()))
                        '())]
            [raw-tags (append keywords aliases)])
       (for-each
        (lambda (kw)
          (when (symbol? kw)
            ;; Normalize if ontology loaded — must match kg-extract-concepts! behavior
            (let ([effective-kw (if (concept-ontology-loaded?)
                                    (concept-normalize kw)
                                    kw)])
              (when (assq effective-kw *kg-concepts*)
                ;; Update skill → concepts map
                (let ([existing (or (hamt-lookup skill-name *kg-skill-concepts*) '())])
                  (unless (memq effective-kw existing)
                    (set! *kg-skill-concepts*
                          (hamt-assoc skill-name (cons effective-kw existing) *kg-skill-concepts*))))
                ;; Update concept → skills map
                (let ([existing (or (hamt-lookup effective-kw *kg-concept-skills*) '())])
                  (unless (memq skill-name existing)
                    (set! *kg-concept-skills*
                          (hamt-assoc effective-kw (cons skill-name existing) *kg-concept-skills*))))))))
        raw-tags)))
   *kg-skills*))

(doc kg-load-from-cas! 'type (-> Bytevector Boolean))
(doc kg-load-from-cas! 'description "Load knowledge graph from CAS using root hash.
Traverses the block tree: root → indexes → entities.
Fully hydrates all state: skills, manifest data, modules, exports,
concepts, edges, and concept reverse maps.
Returns #t if successful, #f if root or required blocks not found.")
(define (kg-load-from-cas! root-hash)
  (let ([root-block (fetch root-hash)])
    (if (not root-block)
        #f
        (let* ([refs (block-refs root-block)]
               [skill-idx-hash (vector-ref refs 0)]
               [concept-idx-hash (vector-ref refs 1)]
               [edge-idx-hash (vector-ref refs 2)]
               [skill-idx (fetch skill-idx-hash)]
               [concept-idx (fetch concept-idx-hash)]
               [edge-idx (fetch edge-idx-hash)])
          (if (not (and skill-idx concept-idx edge-idx))
              #f
              (begin
                (kg-reset!)
                ;; Phase 1: Hydrate skills with full manifest data, modules, exports
                (let ([skill-refs (block-refs skill-idx)])
                  (do ([i 0 (+ i 1)])
                      ((= i (vector-length skill-refs)))
                    (let* ([skill-hash (vector-ref skill-refs i)]
                           [skill-block (fetch skill-hash)])
                      (when skill-block
                        (let* ([payload-str (utf8->string (block-payload skill-block))]
                               [port (open-input-string payload-str)]
                               [data (read port)]
                               [name (cdr (assq 'name data))]
                               [skill-path (let ([p (assq 'path data)])
                                             (if p (cdr p) ""))]
                               [modules-raw (let ([m (assq 'modules data)])
                                              (if (and m (list? (cdr m))) (cdr m) '()))]
                               [exports-raw (let ([e (assq 'exports data)])
                                              (if (and e (list? (cdr e))) (cdr e) '()))])
                          ;; Store skill + manifest data
                          (set! *kg-skills* (cons (cons name skill-block) *kg-skills*))
                          (set! *kg-skill-data* (cons (cons name data) *kg-skill-data*))
                          ;; Reconstruct modules from manifest data
                          (for-each
                           (lambda (mod)
                             (let ([entries (parse-module-entry mod skill-path)])
                               (for-each
                                (lambda (entry)
                                  (let* ([mod-name (car entry)]
                                         [mod-key (string->symbol
                                                   (format "~a/~a" name mod-name))])
                                    (set! *kg-modules*
                                          (cons (cons mod-key #f) *kg-modules*))))
                                entries)))
                           modules-raw)
                          ;; Reconstruct exports from manifest data
                          (for-each
                           (lambda (export-group)
                             (when (pair? export-group)
                               (let* ([first-sym (car export-group)]
                                      [mod-key (if (symbol? first-sym)
                                                   (string->symbol
                                                    (format "~a/~a" name first-sym))
                                                   #f)]
                                      [is-grouped (and mod-key
                                                       (assq mod-key *kg-modules*))]
                                      [export-syms (if is-grouped
                                                       (cdr export-group)
                                                       export-group)])
                                 (for-each
                                  (lambda (export-name)
                                    (when (symbol? export-name)
                                      (set! *kg-exports*
                                            (cons (cons export-name #f) *kg-exports*))))
                                  export-syms))))
                           exports-raw))))))
                ;; Phase 2: Hydrate concepts from concept index
                (let ([concept-refs (block-refs concept-idx)])
                  (do ([i 0 (+ i 1)])
                      ((= i (vector-length concept-refs)))
                    (let* ([concept-hash (vector-ref concept-refs i)]
                           [concept-block (fetch concept-hash)])
                      (when concept-block
                        (let* ([payload-str (utf8->string (block-payload concept-block))]
                               [port (open-input-string payload-str)]
                               [data (read port)]
                               [name (cdr (assq 'name data))])
                          (set! *kg-concepts*
                                (cons (cons name concept-block) *kg-concepts*)))))))
                ;; Phase 3: Hydrate edges from edge index
                ;; Handles v2 (depends-on, provides-concept, bridges) and
                ;; v3 (+ concept-is-a). Unknown tags are stored in *kg-edges* for
                ;; forward compat but not in type-specific lists.
                (let ([edge-refs (block-refs edge-idx)])
                  (do ([i 0 (+ i 1)])
                      ((= i (vector-length edge-refs)))
                    (let* ([edge-hash (vector-ref edge-refs i)]
                           [edge-block (fetch edge-hash)])
                      (when edge-block
                        (let ([tag (block-tag edge-block)])
                          (when (eq? tag KG-DEPENDS-ON)
                            (set! *kg-deps* (cons edge-block *kg-deps*)))
                          ;; KG-CONCEPT-IS-A edges land in *kg-edges* only —
                          ;; hierarchy queries delegate to concept-normalize module
                          (set! *kg-edges* (cons edge-block *kg-edges*)))))))
                ;; Phase 4: Rebuild concept reverse maps from keywords
                (kg-rebuild-concept-maps!)
                ;; Phase 5: Load type signatures (v2 root has 4th ref)
                (when (>= (vector-length refs) 4)
                  (let* ([type-idx-hash (vector-ref refs 3)]
                         [type-idx (fetch type-idx-hash)])
                    (when type-idx
                      (guard (e [else (void)])  ; graceful fallback if malformed
                        (let* ([payload-str (utf8->string (block-payload type-idx))]
                               [port (open-input-string payload-str)]
                               [entries (read port)])
                          (when (list? entries)
                            (kg-populate-types! entries)))))))
                ;; Done
                (set! *kg-index-root* root-hash)
                (set! *kg-loaded* #t)
                #t))))))

;;; ====
;;; REPL Interface
;;; ====

(unless (top-level-bound? '*kg-banner-shown*)
  (meta-printf "kg.ss loaded (KG-first architecture).\n")
  (meta-printf "  (kg-build!)           - Build KG from manifests\n")
  (meta-printf "  (kg-ensure!)          - Build only if not initialized\n")
  (meta-printf "  (kg-skills)           - List all skills\n")
  (meta-printf "  (kg-concepts)         - List all concepts\n")
  (meta-printf "  (kg-concept-skills 'c) - Skills providing concept\n")
  (meta-printf "  (kg-skill-concepts 's) - Concepts of a skill\n")
  (meta-printf "  (kg-export-type 'fn)  - Get type signature\n")
  (meta-printf "  (kg-stats)            - Get statistics\n"))
(set-top-level-value! '*kg-banner-shown* #t)
