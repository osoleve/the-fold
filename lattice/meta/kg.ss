;;; lattice/meta/kg.ss — Lattice Knowledge Graph Builder
;;;
;;; Builds a CAS-backed knowledge graph from lattice manifest files.
;;; Skills, modules, and exports become entities linked by typed relations.
;;;
;;; This is Lattice code: pure, uses Core primitives.
;;;
;;; Usage:
;;;   (kg-build!)                    ; Build KG from all manifests
;;;   (kg-persist! fs root-hash)     ; Persist to store
;;;   (kg-load fs)                   ; Load from store
;;;   (kg-skills)                    ; List all skills
;;;   (kg-skill 'linalg)             ; Get skill entity
;;;   (kg-modules 'linalg)           ; Get modules for skill
;;;   (kg-deps 'linalg)              ; Get dependencies
;;;
;;; Dependencies:
;;;   core/base/prelude.ss
;;;   core/blocks/block.ss

(load "core/base/prelude.ss")
(load "core/base/sha256.ss")
(load "core/blocks/block.ss")

;;; ============================================================
;;; Entity Types
;;; ============================================================

;;; Block tags for knowledge graph entities
(define KG-SKILL 'lattice/skill)
(define KG-MODULE 'lattice/module)
(define KG-EXPORT 'lattice/export)
(define KG-DEPENDS-ON 'lattice/depends-on)
(define KG-INDEX-ROOT 'lattice/index-root)

;;; ============================================================
;;; Manifest Parsing
;;; ============================================================

;;; read-manifest : String -> SExp | #f
;;; Read and parse a manifest.sexp file
(define (read-manifest filepath)
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

;;; manifest-field : SExp Symbol -> Any | #f
;;; Extract a field from a manifest s-expression
(define (manifest-field manifest field-name)
  (if (and (pair? manifest)
           (eq? (car manifest) 'skill)
           (pair? (cdr manifest))      ; Has at least skill name
           (pair? (cddr manifest)))    ; Has at least one field
      (let loop ([fields (cddr manifest)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             (cdar fields)]
            [else (loop (cdr fields))]))
      #f))

;;; parse-manifest : SExp -> ManifestData | #f
;;; Parse manifest into structured data
(define (parse-manifest sexp)
  (if (and (pair? sexp) (eq? (car sexp) 'skill))
      (let ([name (cadr sexp)])
           `((name . ,name)
             (version . ,(car-or-default (manifest-field sexp 'version) "0.0.0"))
             (tier . ,(car-or-default (manifest-field sexp 'tier) 0))
             (path . ,(car-or-default (manifest-field sexp 'path) ""))
             (purity . ,(car-or-default (manifest-field sexp 'purity) 'total))
             (stability . ,(car-or-default (manifest-field sexp 'stability) 'experimental))
             (fuel-bound . ,(car-or-default (manifest-field sexp 'fuel-bound) "O(?)"))
             (deps . ,(flatten-single (manifest-field sexp 'deps)))
             (description . ,(car-or-default (manifest-field sexp 'description) ""))
             (keywords . ,(flatten-single (manifest-field sexp 'keywords)))
             (aliases . ,(flatten-single (manifest-field sexp 'aliases)))
             (exports . ,(or (manifest-field sexp 'exports) '()))
             (modules . ,(or (manifest-field sexp 'modules) '()))))
      #f))

;;; flatten-single : (List a) | #f -> (List a)
;;; If list contains single element that is itself a list, return that inner list
(define (flatten-single lst)
  (cond
   [(not lst) '()]
   [(null? lst) '()]
   [(and (= (length lst) 1)
         (list? (car lst)))
    (car lst)]
   [else lst]))

;;; car-or-default : (List a) | #f a -> a
(define (car-or-default lst default)
  (if (and lst (pair? lst))
      (car lst)
      default))

;;; ============================================================
;;; Entity Creation
;;; ============================================================

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

;;; ============================================================
;;; Knowledge Graph State
;;; ============================================================

;;; In-memory knowledge graph (mutable for building)
(define *kg-skills* '())        ; ((name block . manifest-data) ...)
(define *kg-modules* '())       ; ((skill/module . block) ...)
(define *kg-exports* '())       ; ((symbol . block) ...)
(define *kg-deps* '())          ; (relation-block ...)
(define *kg-index-root* #f)     ; Root block hash
(define *kg-skill-data* '())    ; ((name . manifest-data) ...) - for quick lookup

;;; kg-reset! : -> void
;;; Reset the knowledge graph state
(define (kg-reset!)
  (set! *kg-skills* '())
  (set! *kg-modules* '())
  (set! *kg-exports* '())
  (set! *kg-deps* '())
  (set! *kg-index-root* #f)
  (set! *kg-skill-data* '()))

;;; ============================================================
;;; Graph Building
;;; ============================================================

;;; kg-add-skill! : ManifestData -> Block
;;; Add a skill to the knowledge graph
(define (kg-add-skill! manifest-data)
  (let* ([name (cdr (assq 'name manifest-data))]
         [skill-block (make-skill-entity manifest-data)]
         [modules (cdr (assq 'modules manifest-data))]
         [exports (cdr (assq 'exports manifest-data))])
        ;; Add skill and its manifest data
        (set! *kg-skills* (cons (cons name skill-block) *kg-skills*))
        (set! *kg-skill-data* (cons (cons name manifest-data) *kg-skill-data*))
        ;; Add modules
        (for-each
         (lambda (mod)
                 (when (and (pair? mod) (>= (length mod) 3))
                       (let* ([mod-name (car mod)]
                              [mod-file (cadr mod)]
                              [mod-desc (caddr mod)]
                              [mod-block (make-module-entity mod-name mod-file mod-desc skill-block)]
                              [mod-key (string->symbol (format "~a/~a" name mod-name))])
                             (set! *kg-modules* (cons (cons mod-key mod-block) *kg-modules*)))))
         (if (list? modules) modules '()))
        ;; Add exports (grouped by module)
        (for-each
         (lambda (export-group)
                 (when (pair? export-group)
                       (let* ([mod-name (car export-group)]
                              [mod-key (string->symbol (format "~a/~a" name mod-name))]
                              [mod-entry (assq mod-key *kg-modules*)]
                              [mod-block (if mod-entry (cdr mod-entry) skill-block)])
                             (for-each
                              (lambda (export-name)
                                      (when (symbol? export-name)
                                            (let ([export-block (make-export-entity export-name mod-block)])
                                                 (set! *kg-exports* (cons (cons export-name export-block) *kg-exports*)))))
                              (cdr export-group)))))
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

;;; ============================================================
;;; Manifest Discovery
;;; ============================================================

;;; find-manifests : String -> (List String)
;;; Find all manifest.sexp files under a directory
(define (find-manifests base-dir)
  (let ([result '()])
       ;; Check immediate directory
       (let ([manifest-path (string-append base-dir "/manifest.sexp")])
            (when (file-exists? manifest-path)
                  (set! result (cons manifest-path result))))
       ;; Check subdirectories
       (guard (e [else result])
              (for-each
               (lambda (entry)
                       (let ([path (string-append base-dir "/" entry)])
                            (when (and (file-directory? path)
                                       (not (string-starts-with? entry ".")))
                                  (set! result (append (find-manifests path) result)))))
               (directory-list base-dir)))
       result))

;;; string-starts-with? : String String -> Bool
(define (string-starts-with? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (>= str-len pre-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; ============================================================
;;; Main API
;;; ============================================================

;;; kg-build! : -> Hash
;;; Build knowledge graph from all manifests in lattice/
(define (kg-build!)
  (kg-reset!)
  (let ([manifests (find-manifests "lattice")])
       (printf "Found ~a manifests\n" (length manifests))
       ;; Load all skills
       (for-each
        (lambda (manifest-path)
                (let* ([sexp (read-manifest manifest-path)]
                       [data (if sexp (parse-manifest sexp) #f)])
                      (when data
                            (printf "  Loading: ~a\n" (cdr (assq 'name data)))
                            (kg-add-skill! data))))
        manifests)
       ;; Build dependency relations
       (kg-build-deps!)
       ;; Create index root
       (let* ([skill-hashes (map (lambda (e) (hash-block (cdr e))) *kg-skills*)]
              [root (make-block KG-INDEX-ROOT
                                (string->utf8 (format "~a" (length *kg-skills*)))
                                (list->vector skill-hashes))])
             (set! *kg-index-root* (hash-block root))
             (printf "Knowledge graph built: ~a skills, ~a modules, ~a exports, ~a deps\n"
                     (length *kg-skills*)
                     (length *kg-modules*)
                     (length *kg-exports*)
                     (length *kg-deps*))
             *kg-index-root*)))

;;; kg-skills : -> (List Symbol)
;;; Get list of all skill names
(define (kg-skills)
  (map car *kg-skills*))

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

;;; filter-map helper
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; ============================================================
;;; Statistics
;;; ============================================================

;;; kg-stats : -> Alist
;;; Get knowledge graph statistics
(define (kg-stats)
  `((skills . ,(length *kg-skills*))
    (modules . ,(length *kg-modules*))
    (exports . ,(length *kg-exports*))
    (dependencies . ,(length *kg-deps*))))

;;; ============================================================
;;; REPL Interface
;;; ============================================================

(printf "kg.ss loaded.\n")
(printf "  (kg-build!)           - Build KG from manifests\n")
(printf "  (kg-skills)           - List all skills\n")
(printf "  (kg-skill 'name)      - Get skill block\n")
(printf "  (kg-modules 'name)    - Get skill modules\n")
(printf "  (kg-deps 'name)       - Get skill dependencies\n")
(printf "  (kg-uses 'name)       - Get skills that use this\n")
(printf "  (kg-stats)            - Get statistics\n")
