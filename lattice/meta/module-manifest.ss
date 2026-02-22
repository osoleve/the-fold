(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module meta/module-manifest
;;; @requires prelude sha256 sort
(require 'prelude)
(require 'sha256)
(require 'sort)

(doc 'module 'meta/module-manifest)
(doc 'description "Content-addressed module manifests. Builds canonical, hashable
representations of the module registry. Enables drift detection between the
static registry and manifest-derived module data, and CAS-backed module identity.
Pure analysis — no filesystem I/O.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Manifest Entries
;;; ====

(doc 'section 'manifest-entries)

(doc 'type '(-> Symbol String Alist))
(doc 'description "Create a manifest entry (name . path) pair.")
(define (make-manifest-entry name path)
  (cons name path))

(doc 'type '(-> Alist Symbol))
(define (manifest-entry-name entry) (car entry))

(doc 'type '(-> Alist String))
(define (manifest-entry-path entry) (cdr entry))

;;; ====
;;; Canonical Manifest Construction
;;; ====

(doc 'section 'canonical-manifest)

(doc 'type '(-> (List (Pair Symbol String)) Alist))
(doc 'description "Build a canonical module manifest from a list of (name . path) entries.
Sorts by name for deterministic ordering. Removes duplicates (last wins).
Returns a tagged alist: (module-manifest (version 1) (count N) (entries ...)).")
(define (build-manifest entries)
  (let* ([sorted (sort-by
                   (lambda (a b)
                     (string<? (symbol->string (car a))
                               (symbol->string (car b))))
                   entries)]
         [deduped (dedupe-by-name sorted)]
         [count (length deduped)])
    (list 'module-manifest
          (list 'version 1)
          (list 'count count)
          (list 'entries deduped))))

(doc 'type '(-> (List (Pair Symbol String)) (List (Pair Symbol String))))
(doc 'description "Remove duplicate names from sorted entries. Last occurrence wins.")
(define (dedupe-by-name sorted-entries)
  (if (null? sorted-entries)
      '()
      (let loop ([entries (cdr sorted-entries)]
                 [prev (car sorted-entries)]
                 [acc '()])
        (cond
          [(null? entries) (reverse (cons prev acc))]
          [(eq? (car (car entries)) (car prev))
           ;; Duplicate name — keep the new one (last wins)
           (loop (cdr entries) (car entries) acc)]
          [else
           (loop (cdr entries) (car entries) (cons prev acc))]))))

(doc 'type '(-> Any Boolean))
(define (module-manifest? x)
  (and (pair? x) (eq? (car x) 'module-manifest)))

(doc 'type '(-> Alist Symbol Any))
(doc 'description "Access a field from a manifest.")
(define (manifest-field* manifest key)
  (let loop ([fields (cdr manifest)])
    (cond
      [(null? fields) #f]
      [(and (pair? (car fields)) (eq? (caar fields) key))
       (cadar fields)]
      [else (loop (cdr fields))])))

(doc 'type '(-> Alist (List (Pair Symbol String))))
(doc 'description "Extract the entries list from a manifest.")
(define (manifest-entries manifest)
  (or (manifest-field* manifest 'entries) '()))

(doc 'type '(-> Alist Nat))
(doc 'description "Number of entries in a manifest.")
(define (manifest-count manifest)
  (or (manifest-field* manifest 'count) 0))

;;; ====
;;; Content Addressing
;;; ====

(doc 'section 'content-addressing)

(doc 'type '(-> Alist String))
(doc 'description "Serialize a manifest to its canonical string form for hashing.
Deterministic: same entries always produce the same string.")
(define (manifest-serialize manifest)
  (let ([entries (manifest-entries manifest)])
    ;; Serialize as sorted list of (name "path") forms
    ;; This is the canonical wire format for hashing
    (format "~s"
      (cons 'module-manifest-v1
            (map (lambda (e)
                   (list (car e) (cdr e)))
                 entries)))))

(doc 'type '(-> Alist String))
(doc 'description "Compute the SHA256 hex digest of a manifest's canonical form.
Two manifests with identical module mappings produce the same hash.")
(define (manifest-hash manifest)
  (sha256-hex (string->utf8 (manifest-serialize manifest))))

;;; ====
;;; Manifest Diffing
;;; ====

(doc 'section 'manifest-diff)

(doc 'type '(-> Alist Alist Alist))
(doc 'description "Compare two manifests and return a diff record.
Shows entries added, removed, or changed (same name, different path).
Returns: (manifest-diff (added ...) (removed ...) (changed ...) (same-count N)).")
(define (manifest-diff old-manifest new-manifest)
  (let* ([old-entries (manifest-entries old-manifest)]
         [new-entries (manifest-entries new-manifest)]
         [old-table (entries->table old-entries)]
         [new-table (entries->table new-entries)]
         [old-names (map car old-entries)]
         [new-names (map car new-entries)]
         [added '()]
         [removed '()]
         [changed '()]
         [same-count 0])
    ;; Find additions and changes
    (for-each
      (lambda (entry)
        (let* ([name (car entry)]
               [new-path (cdr entry)]
               [old-path (hashtable-ref old-table name #f)])
          (cond
            [(not old-path)
             (set! added (cons entry added))]
            [(not (string=? old-path new-path))
             (set! changed (cons (list name old-path new-path) changed))]
            [else
             (set! same-count (+ same-count 1))])))
      new-entries)
    ;; Find removals
    (for-each
      (lambda (entry)
        (let ([name (car entry)])
          (unless (hashtable-ref new-table name #f)
            (set! removed (cons entry removed)))))
      old-entries)
    (list 'manifest-diff
          (list 'added (reverse added))
          (list 'removed (reverse removed))
          (list 'changed (reverse changed))
          (list 'same-count same-count))))

(doc 'type '(-> (List (Pair Symbol String)) (Hashtable Symbol String)))
(define (entries->table entries)
  (let ([ht (make-eq-hashtable)])
    (for-each
      (lambda (e) (hashtable-set! ht (car e) (cdr e)))
      entries)
    ht))

(doc 'type '(-> Alist Boolean))
(doc 'description "True if the diff shows no changes.")
(define (manifest-diff-empty? diff)
  (and (null? (cadr (assq 'added (cdr diff))))
       (null? (cadr (assq 'removed (cdr diff))))
       (null? (cadr (assq 'changed (cdr diff))))))

;;; ====
;;; Registry Extraction
;;; ====

(doc 'section 'registry-extraction)

(doc 'type '(-> (Hashtable Symbol String) Alist))
(doc 'description "Build a canonical manifest from a module-paths hashtable.
Extracts all entries and returns a sorted, content-addressable manifest.")
(define (manifest-from-paths-table ht)
  (let ([entries (let-values ([(keys vals) (hashtable-entries ht)])
                   (let loop ([i 0] [acc '()])
                     (if (>= i (vector-length keys))
                         acc
                         (loop (+ i 1)
                               (cons (cons (vector-ref keys i)
                                           (vector-ref vals i))
                                     acc)))))])
    (build-manifest entries)))

;;; ====
;;; Core Bootstrap Set
;;; ====

(doc 'section 'core-bootstrap)

(doc 'type '(-> (List (Pair Symbol String))))
(doc 'description "The minimal set of modules that must be statically registered.
These modules are needed before manifest scanning can run. Everything else
should be derived from manifest.sexp files.")
(define (manifest-core-bootstrap)
  '(;; core/base — foundation
    (prelude   . "core/base/prelude.ss")
    (sha256    . "core/base/sha256.ss")
    (error     . "core/base/error.ss")
    ;; core/blocks — block machine
    (block     . "core/blocks/block.ss")
    (cas       . "core/blocks/cas.ss")
    (normalize . "core/blocks/normalize.ss")
    (expand    . "core/blocks/expand.ss")
    ;; core/lang — evaluator
    (parse     . "core/lang/parse.ss")
    (span      . "core/lang/span.ss")
    (fold-parse . "core/lang/fold-parse.ss")
    (prim      . "core/lang/prim.ss")
    (eval      . "core/lang/eval.ss")
    (compile   . "core/lang/compile.ss")
    (module    . "core/lang/module.ss")
    (nbe       . "core/lang/nbe.ss")
    ;; core/types — type system
    (types     . "core/types/types.ss")
    (kinds     . "core/types/kinds.ss")
    (infer     . "core/types/infer.ss")
    (resolve   . "core/types/resolve.ss")
    (annotate  . "core/types/annotate.ss")
    (dep-types . "core/types/dep-types.ss")
    ;; core/util
    (pretty-class . "core/util/pretty-class.ss")))

(doc 'type '(-> (Pair Symbol String) Boolean))
(doc 'description "True if entry is in the core bootstrap set.")
(define (core-bootstrap-entry? entry)
  (let ([name (car entry)])
    (let loop ([core (manifest-core-bootstrap)])
      (cond
        [(null? core) #f]
        [(eq? name (caar core)) #t]
        [else (loop (cdr core))]))))

(doc 'type '(-> Alist Alist))
(doc 'description "Split a manifest into core (bootstrap) and lattice (manifest-derivable) entries.
Returns: (manifest-split (core ...) (lattice ...)).")
(define (manifest-split manifest)
  (let* ([entries (manifest-entries manifest)]
         [core '()]
         [lattice '()])
    (for-each
      (lambda (e)
        (if (core-bootstrap-entry? e)
            (set! core (cons e core))
            (set! lattice (cons e lattice))))
      entries)
    (list 'manifest-split
          (list 'core (reverse core))
          (list 'lattice (reverse lattice)))))

;;; ====
;;; Consistency Checking
;;; ====

(doc 'section 'consistency)

(doc 'type '(-> Alist Alist Alist))
(doc 'description "Check consistency between static registry manifest and manifest-derived data.
Returns a report of discrepancies:
  - in-static-only: modules registered statically but absent from manifest.sexp files
  - in-manifest-only: modules in manifest.sexp but not in static registry
  - path-mismatch: modules in both but with different paths")
(define (manifest-consistency-check static-manifest derived-manifest)
  (let* ([diff (manifest-diff static-manifest derived-manifest)]
         [removed (cadr (assq 'removed (cdr diff)))]   ; in static, not derived
         [added (cadr (assq 'added (cdr diff)))]       ; in derived, not static
         [changed (cadr (assq 'changed (cdr diff)))])   ; different paths
    (list 'consistency-report
          (list 'in-static-only removed)
          (list 'in-manifest-only added)
          (list 'path-mismatch changed)
          (list 'consistent? (and (null? removed) (null? added) (null? changed))))))

;;; ====
;;; Code Generation
;;; ====

(doc 'section 'codegen)

(doc 'type '(-> Alist (List SExp)))
(doc 'description "Generate register-module-path! forms from a manifest.
Returns a list of s-expressions suitable for inclusion in module.ss.")
(define (manifest->registry-forms manifest)
  (map (lambda (e)
         `(register-module-path! ',(car e) ,(cdr e)))
       (manifest-entries manifest)))

(doc 'type '(-> Alist String))
(doc 'description "Generate a complete registry block as a formatted string.
Suitable for pasting into module.ss to replace the static registry.")
(define (manifest->registry-string manifest)
  (let ([forms (manifest->registry-forms manifest)])
    (apply string-append
      (map (lambda (f)
             (string-append " " (format "~s" f) "\n"))
           forms))))

;;; ====
;;; Bulk Loading Support
;;; ====

(doc 'section 'bulk-loading)

(doc 'type '(-> (List (Pair Symbol String)) Void))
(doc 'description "Register multiple module paths at once from a manifest entries list.
More efficient than individual register-module-path! calls and makes the
source of truth explicit.")
(define (register-manifest-entries! entries)
  (for-each
    (lambda (e)
      (register-module-path! (car e) (cdr e)))
    entries))

;;; ====
;;; Summary
;;; ====

(doc 'section 'summary)

(doc 'type '(-> Alist Alist))
(doc 'description "Produce a compact summary of a manifest for display.")
(define (manifest-summary manifest)
  (let* ([entries (manifest-entries manifest)]
         [split (manifest-split manifest)]
         [core-entries (cadr (assq 'core (cdr split)))]
         [lattice-entries (cadr (assq 'lattice (cdr split)))]
         ;; Count by layer prefix
         [layer-counts (count-by-layer entries)])
    (list 'manifest-summary
          (list 'total (length entries))
          (list 'core (length core-entries))
          (list 'lattice (length lattice-entries))
          (list 'hash (manifest-hash manifest))
          (list 'layers layer-counts))))

(doc 'type '(-> (List (Pair Symbol String)) (List (Pair String Nat))))
(define (count-by-layer entries)
  (let ([counts (make-hashtable string-hash string=?)])
    (for-each
      (lambda (e)
        (let* ([path (cdr e)]
               [layer (cond
                        [(string-starts-with? path "core/") "core"]
                        [(string-starts-with? path "lattice/") "lattice"]
                        [(string-starts-with? path "boundary/") "boundary"]
                        [else "other"])]
               [current (hashtable-ref counts layer 0)])
          (hashtable-set! counts layer (+ current 1))))
      entries)
    (let-values ([(keys vals) (hashtable-entries counts)])
      (let loop ([i 0] [acc '()])
        (if (>= i (vector-length keys))
            (sort-by (lambda (a b) (string<? (car a) (car b))) acc)
            (loop (+ i 1)
                  (cons (cons (vector-ref keys i) (vector-ref vals i)) acc)))))))
