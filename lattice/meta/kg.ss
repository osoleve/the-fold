(unless (top-level-bound? 'filter-map) (load "core/base/prelude.ss"))
(unless (top-level-bound? 'sha256) (load "core/base/sha256.ss"))
(unless (top-level-bound? 'make-block) (load "core/blocks/block.ss"))
(unless (top-level-bound? 'read-manifest) (load "lattice/meta/manifest.ss"))

(doc 'module 'kg)
(doc 'description "Lattice knowledge graph builder from manifest files")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Quiet Load Mode
;;; ====

;;; Set *meta-quiet* to #t before loading meta modules to suppress help text.
;;; Useful for tests and scripts that don't need the REPL interface messages.
;;; We check if it's already defined so callers can pre-set it before loading.
(when (not (top-level-bound? '*meta-quiet*))
      (set-top-level-value! '*meta-quiet* #f))

;;; meta-printf : like printf, but suppressed when *meta-quiet* is true
(define (meta-printf . args)
  (unless (top-level-value '*meta-quiet*)
          (apply printf args)))

;;; ====
;;; Entity Types
;;; ====

;;; Block tags for knowledge graph entities
(define KG-SKILL 'lattice/skill)
(define KG-MODULE 'lattice/module)
(define KG-EXPORT 'lattice/export)
(define KG-DEPENDS-ON 'lattice/depends-on)
(define KG-INDEX-ROOT 'lattice/index-root)

;;; ====
;;; Entity Creation
;;; ====

;;; make-skill-entity : ManifestData -> Block
;;; Create a skill entity block
(define (make-skill-entity manifest-data)
  (let* ([name (cdr (assq 'name manifest-data))]
         [version (cdr (assq 'version manifest-data))]
         [tier (cdr (assq 'tier manifest-data))]
         [purity (cdr (assq 'purity manifest-data))]
         [stability (cdr (assq 'stability manifest-data))]
         [description (cdr (assq 'description manifest-data))]
         [keywords (cdr (assq 'keywords manifest-data))]
         [aliases (cdr (assq 'aliases manifest-data))]
         [payload (format "~s"
                          `((name . ,name)
                            (version . ,version)
                            (tier . ,tier)
                            (purity . ,purity)
                            (stability . ,stability)
                            (description . ,description)
                            (keywords . ,keywords)
                            (aliases . ,aliases)))])
        (make-block KG-SKILL
                    (string->utf8 payload)
                    (vector))))

;;; make-module-entity : Symbol String String Block -> Block
;;; Create a module entity block
(define (make-module-entity name file description skill-block)
  (let ([payload (format "~s" `((name . ,name)
                                (file . ,file)
                                (description . ,description)))])
       (make-block KG-MODULE
                   (string->utf8 payload)
                   (vector (hash-block skill-block)))))

;;; make-export-entity : Symbol Block -> Block
;;; Create an export entity block
(define (make-export-entity name module-block)
  (let ([payload (format "~s" `((name . ,name)))])
       (make-block KG-EXPORT
                   (string->utf8 payload)
                   (vector (hash-block module-block)))))

;;; make-depends-on-relation : Block Block -> Block
;;; Create a dependency relation block
(define (make-depends-on-relation from-skill to-skill)
  (make-block KG-DEPENDS-ON
              (string->utf8 "")
              (vector (hash-block from-skill)
                      (hash-block to-skill))))

;;; ====
;;; Knowledge Graph State
;;; ====

;;; In-memory knowledge graph (mutable for building)
(define *kg-skills* '())        ; ((name block . manifest-data) ...)
(define *kg-modules* '())       ; ((skill/module . block) ...)
(define *kg-exports* '())       ; ((symbol . block) ...)
(define *kg-deps* '())          ; (relation-block ...)
(define *kg-index-root* #f)     ; Root block hash
(define *kg-skill-data* '())    ; ((name . manifest-data) ...) - for quick lookup
(define *kg-loaded* #f)         ; Explicit flag for initialization state

;;; kg-reset! : -> void
;;; Reset the knowledge graph state
(define (kg-reset!)
  (set! *kg-skills* '())
  (set! *kg-modules* '())
  (set! *kg-exports* '())
  (set! *kg-deps* '())
  (set! *kg-index-root* #f)
  (set! *kg-skill-data* '())
  (set! *kg-loaded* #f))

;;; ====
;;; Graph Building
;;; ====

;;; kg-add-skill! : ManifestData -> Block
;;; Add a skill to the knowledge graph
(define (kg-add-skill! manifest-data)
  (let* ([name (cdr (assq 'name manifest-data))]
         [skill-block (make-skill-entity manifest-data)]
         [skill-path (cdr (assq 'path manifest-data))]
         [modules (cdr (assq 'modules manifest-data))]
         [exports (cdr (assq 'exports manifest-data))])
        ;; Add skill and its manifest data
        (set! *kg-skills* (cons (cons name skill-block) *kg-skills*))
        (set! *kg-skill-data* (cons (cons name manifest-data) *kg-skill-data*))
        ;; Add modules — use parse-module-entry to handle both flat and nested formats
        (for-each
         (lambda (mod)
                 (let ([entries (parse-module-entry mod skill-path)])
                      (for-each
                       (lambda (entry)
                               (let* ([mod-name (car entry)]
                                      [mod-file (cdr entry)]
                                      [mod-block (make-module-entity mod-name mod-file "" skill-block)]
                                      [mod-key (string->symbol (format "~a/~a" name mod-name))])
                                     (set! *kg-modules* (cons (cons mod-key mod-block) *kg-modules*))))
                       entries)))
         (if (list? modules) modules '()))
        ;; Add exports — handle both grouped and flat formats
        ;; Grouped: ((module-name sym1 sym2 ...) ...) — car is a known module
        ;; Flat: ((sym1 sym2 sym3 ...)) — car is not a known module
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
                                            (let ([export-block (make-export-entity export-name mod-block)])
                                                 (set! *kg-exports* (cons (cons export-name export-block) *kg-exports*)))))
                              export-syms))))
         (if (list? exports) exports '()))
        skill-block))

;;; kg-build-deps! : -> void
;;; Build dependency relations (call after all skills added)
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
                                            [rel (make-depends-on-relation skill-block dep-block)])
                                           (set! *kg-deps* (cons rel *kg-deps*))))))
                  (if (list? deps) deps '()))))
   *kg-skills*))

;;; kg-skill-data : Symbol -> ManifestData | #f
;;; Get manifest data for a skill (from cached data)
(define (kg-skill-data skill-name)
  (let ([entry (assq skill-name *kg-skill-data*)])
       (if entry (cdr entry) #f)))

;;; ====
;;; Main API
;;; ====

;;; NOTE: kg-build! and kg-ensure! have moved to boundary/meta/kg-io.ss
;;; They orchestrate I/O (manifest discovery and file reading) and delegate
;;; to the pure functions below.

;;; kg-skills : -> (List Symbol)
;;; Get list of all skill names
(define (kg-skills)
  (map car *kg-skills*))

;;; kg-initialized? : -> Boolean
;;; Check if the knowledge graph has been built.
;;; Uses explicit flag rather than checking *kg-skills* to correctly
;;; handle the edge case of a truly empty lattice (0 skills).
(define (kg-initialized?)
  *kg-loaded*)

;;; kg-skill : Symbol -> Block | #f
;;; Get skill block by name
(define (kg-skill name)
  (let ([entry (assq name *kg-skills*)])
       (if entry (cdr entry) #f)))

;;; kg-modules : Symbol -> (List (Symbol . Block))
;;; Get modules for a skill
(define (kg-modules skill-name)
  (filter
   (lambda (entry)
           (let ([key (symbol->string (car entry))])
                (string-starts-with? key (string-append (symbol->string skill-name) "/"))))
   *kg-modules*))

;;; kg-exports : -> (List (Symbol . Block))
;;; Get all exports (deduplicated by name)
(define (kg-exports)
  (dedupe-by-car *kg-exports*))

;;; dedupe-by-car : (List (Symbol . Any)) -> (List (Symbol . Any))
;;; Remove entries with duplicate car values, keeping first occurrence
(define (dedupe-by-car lst)
  (let ([seen (make-eq-hashtable)])
    (filter (lambda (entry)
              (let ([name (car entry)])
                (if (hashtable-ref seen name #f)
                    #f
                    (begin (hashtable-set! seen name #t) #t))))
            lst)))

;;; kg-deps : Symbol -> (List Symbol)
;;; Get dependencies for a skill
(define (kg-deps skill-name)
  (let ([data (kg-skill-data skill-name)])
       (if data
           (let ([deps (cdr (assq 'deps data))])
                (if (list? deps) deps '()))
           '())))

;;; kg-uses : Symbol -> (List Symbol)
;;; Get skills that depend on this skill
(define (kg-uses skill-name)
  (filter-map
   (lambda (entry)
           (let* ([name (car entry)]
                  [deps (kg-deps name)])
                 (if (memq skill-name deps)
                     name
                     #f)))
   *kg-skills*))

;;; NOTE: filter-map is now provided by lattice/meta/manifest.ss

;;; ====
;;; Statistics
;;; ====

;;; kg-stats : -> Alist
;;; Get knowledge graph statistics
(define (kg-stats)
  `((skills . ,(length *kg-skills*))
    (modules . ,(length *kg-modules*))
    (exports . ,(length *kg-exports*))
    (dependencies . ,(length *kg-deps*))))

;;; ====
;;; REPL Interface
;;; ====

(unless (top-level-bound? '*kg-banner-shown*)
  (meta-printf "kg.ss loaded.\n")
  (meta-printf "  (kg-build!)           - Build KG from manifests\n")
  (meta-printf "  (kg-ensure!)          - Build only if not initialized\n")
  (meta-printf "  (kg-skills)           - List all skills\n")
  (meta-printf "  (kg-skill 'name)      - Get skill block\n")
  (meta-printf "  (kg-modules 'name)    - Get skill modules\n")
  (meta-printf "  (kg-deps 'name)       - Get skill dependencies\n")
  (meta-printf "  (kg-uses 'name)       - Get skills that use this\n")
  (meta-printf "  (kg-stats)            - Get statistics\n"))
(set-top-level-value! '*kg-banner-shown* #t)
