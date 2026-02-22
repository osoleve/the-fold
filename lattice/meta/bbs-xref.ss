(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module meta/bbs-xref
;;; @requires prelude
(require 'prelude)

(doc 'module 'meta/bbs-xref)
(doc 'description "Bidirectional cross-references between BBS issues and lattice entities.
Uses a label convention: @skill:name, @module:name, @export:name. Pure
functions for parsing, creating, and indexing cross-reference labels.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Label Convention
;;; ============================================================
;;;
;;; Cross-reference labels follow the pattern:
;;;   @skill:name     — references a lattice skill
;;;   @module:name    — references a lattice module
;;;   @export:name    — references a lattice export
;;;
;;; These are regular BBS labels (symbols). The @ prefix distinguishes
;;; them from semantic labels like 'bug or 'enhancement.
;;;
;;; Examples:
;;;   @skill:egraph
;;;   @module:reverse-diff
;;;   @export:vec2-add

(doc 'section 'label-construction)

(doc 'type '(-> Symbol Symbol))
(doc 'description "Create a skill cross-reference label.")
(define (xref-skill name)
  (string->symbol (string-append "@skill:" (symbol->string name))))

(doc 'type '(-> Symbol Symbol))
(doc 'description "Create a module cross-reference label.")
(define (xref-module name)
  (string->symbol (string-append "@module:" (symbol->string name))))

(doc 'type '(-> Symbol Symbol))
(doc 'description "Create an export cross-reference label.")
(define (xref-export name)
  (string->symbol (string-append "@export:" (symbol->string name))))

;;; ============================================================
;;; Label Parsing
;;; ============================================================

(doc 'section 'label-parsing)

(doc 'type '(-> Symbol (Maybe (Pair Symbol Symbol))))
(doc 'description "Parse a cross-reference label. Returns (type . name) or #f.
Type is one of: skill, module, export.")
(define (xref-parse label)
  (let ([s (symbol->string label)])
    (cond
      [(xref-prefix? s "@skill:")
       (cons 'skill (string->symbol (substring s 7 (string-length s))))]
      [(xref-prefix? s "@module:")
       (cons 'module (string->symbol (substring s 8 (string-length s))))]
      [(xref-prefix? s "@export:")
       (cons 'export (string->symbol (substring s 8 (string-length s))))]
      [else #f])))

(doc 'type '(-> Symbol Boolean))
(doc 'description "Is this label a cross-reference?")
(define (xref-label? label)
  (let ([s (symbol->string label)])
    (or (xref-prefix? s "@skill:")
        (xref-prefix? s "@module:")
        (xref-prefix? s "@export:"))))

(doc 'type '(-> Symbol Symbol Boolean))
(doc 'description "Does this label reference the given type? (skill, module, export)")
(define (xref-type? label type)
  (let ([parsed (xref-parse label)])
    (and parsed (eq? type (car parsed)))))

;;; Helper: string prefix check
(define (xref-prefix? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; ============================================================
;;; Issue Cross-Reference Extraction
;;; ============================================================

(doc 'section 'extraction)

(doc 'type '(-> (List Symbol) (List (Pair Symbol Symbol))))
(doc 'description "Extract all cross-references from an issue's labels.
Returns a list of (type . name) pairs.")
(define (xref-extract-labels labels)
  (let loop ([remaining labels] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([parsed (xref-parse (car remaining))])
          (loop (cdr remaining)
                (if parsed (cons parsed acc) acc))))))

(doc 'type '(-> (List Symbol) (List Symbol)))
(doc 'description "Extract skill references from labels.")
(define (xref-skills labels)
  (map cdr (filter (lambda (x) (eq? 'skill (car x)))
                   (xref-extract-labels labels))))

(doc 'type '(-> (List Symbol) (List Symbol)))
(doc 'description "Extract module references from labels.")
(define (xref-modules labels)
  (map cdr (filter (lambda (x) (eq? 'module (car x)))
                   (xref-extract-labels labels))))

(doc 'type '(-> (List Symbol) (List Symbol)))
(doc 'description "Extract export references from labels.")
(define (xref-exports labels)
  (map cdr (filter (lambda (x) (eq? 'export (car x)))
                   (xref-extract-labels labels))))

;;; ============================================================
;;; Cross-Reference Index
;;; ============================================================

(doc 'section 'index)

;;; An xref index maps lattice entities to lists of issue IDs.
;;; Structure: (xref-index skill-table module-table export-table)

(define xref-index-tag 'xref-index)

(doc 'type '(-> XRefIndex))
(doc 'description "Create an empty cross-reference index.")
(define (make-xref-index)
  (vector xref-index-tag
          (make-eq-hashtable)    ; skill → issue-ids
          (make-eq-hashtable)    ; module → issue-ids
          (make-eq-hashtable)))  ; export → issue-ids

(define (xref-index? x)
  (and (vector? x)
       (>= (vector-length x) 4)
       (eq? xref-index-tag (vector-ref x 0))))

(define (xref-idx-skills idx) (vector-ref idx 1))
(define (xref-idx-modules idx) (vector-ref idx 2))
(define (xref-idx-exports idx) (vector-ref idx 3))

(doc 'type '(-> XRefIndex String (List Symbol) Void))
(doc 'description "Index an issue's labels into the cross-reference index.")
(define (xref-index-issue! idx issue-id labels)
  (for-each
    (lambda (label)
      (let ([parsed (xref-parse label)])
        (when parsed
          (let* ([type (car parsed)]
                 [name (cdr parsed)]
                 [table (case type
                          [(skill) (xref-idx-skills idx)]
                          [(module) (xref-idx-modules idx)]
                          [(export) (xref-idx-exports idx)]
                          [else #f])])
            (when table
              (let ([existing (hashtable-ref table name '())])
                (unless (member issue-id existing)
                  (hashtable-set! table name (cons issue-id existing)))))))))
    labels))

(doc 'type '(-> XRefIndex (List (Pair String (List Symbol))) XRefIndex))
(doc 'description "Build a cross-reference index from a list of (issue-id . labels) pairs.")
(define (xref-build-index issues)
  (let ([idx (make-xref-index)])
    (for-each
      (lambda (entry)
        (xref-index-issue! idx (car entry) (cdr entry)))
      issues)
    idx))

;;; ============================================================
;;; Queries
;;; ============================================================

(doc 'section 'queries)

(doc 'type '(-> XRefIndex Symbol (List String)))
(doc 'description "Find all issue IDs referencing a given skill.")
(define (xref-issues-for-skill idx skill-name)
  (hashtable-ref (xref-idx-skills idx) skill-name '()))

(doc 'type '(-> XRefIndex Symbol (List String)))
(doc 'description "Find all issue IDs referencing a given module.")
(define (xref-issues-for-module idx module-name)
  (hashtable-ref (xref-idx-modules idx) module-name '()))

(doc 'type '(-> XRefIndex Symbol (List String)))
(doc 'description "Find all issue IDs referencing a given export.")
(define (xref-issues-for-export idx export-name)
  (hashtable-ref (xref-idx-exports idx) export-name '()))

(doc 'type '(-> XRefIndex Alist))
(doc 'description "Summary statistics for the cross-reference index.")
(define (xref-summary idx)
  (list 'xref-summary
        (list 'skills (hashtable-size (xref-idx-skills idx)))
        (list 'modules (hashtable-size (xref-idx-modules idx)))
        (list 'exports (hashtable-size (xref-idx-exports idx)))
        (list 'total-refs
              (+ (hashtable-size (xref-idx-skills idx))
                 (hashtable-size (xref-idx-modules idx))
                 (hashtable-size (xref-idx-exports idx))))))

;;; ============================================================
;;; Label Merging
;;; ============================================================

(doc 'section 'merging)

(doc 'type '(-> (List Symbol) Symbol Symbol (List Symbol)))
(doc 'description "Add a skill cross-reference to a label list (idempotent).")
(define (xref-add-skill labels skill-name)
  (let ([label (xref-skill skill-name)])
    (if (memq label labels) labels (cons label labels))))

(doc 'type '(-> (List Symbol) Symbol Symbol (List Symbol)))
(doc 'description "Add a module cross-reference to a label list (idempotent).")
(define (xref-add-module labels module-name)
  (let ([label (xref-module module-name)])
    (if (memq label labels) labels (cons label labels))))

(doc 'type '(-> (List Symbol) Symbol Symbol (List Symbol)))
(doc 'description "Add an export cross-reference to a label list (idempotent).")
(define (xref-add-export labels export-name)
  (let ([label (xref-export export-name)])
    (if (memq label labels) labels (cons label labels))))

(doc 'type '(-> (List Symbol) (List Symbol)))
(doc 'description "Remove all cross-reference labels, keeping semantic labels.")
(define (xref-strip labels)
  (filter (lambda (l) (not (xref-label? l))) labels))

;;; ============================================================
;;; Auto-Detection
;;; ============================================================

(doc 'section 'auto-detect)

(doc 'type '(-> String (List Symbol) (List (Pair Symbol Symbol))))
(doc 'description "Scan text (issue title/description) for potential lattice references.
Takes a list of known skill names and returns (skill . name) pairs found in text.
Uses simple word-boundary matching.")
(define (xref-auto-detect text known-skills)
  (let ([words (text->words (string-downcase text))]
        [results '()])
    (for-each
      (lambda (skill)
        (when (member (string-downcase (symbol->string skill)) words)
          (set! results (cons (cons 'skill skill) results))))
      known-skills)
    (reverse results)))

;;; Split text into lowercase words (alphanumeric + hyphens).
(define (text->words text)
  (let loop ([i 0] [word-start #f] [acc '()])
    (cond
      [(>= i (string-length text))
       (reverse (if word-start
                    (cons (substring text word-start i) acc)
                    acc))]
      [(or (char-alphabetic? (string-ref text i))
           (char-numeric? (string-ref text i))
           (char=? (string-ref text i) #\-))
       (loop (+ i 1) (or word-start i) acc)]
      [else
       (loop (+ i 1) #f
             (if word-start
                 (cons (substring text word-start i) acc)
                 acc))])))

(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))
