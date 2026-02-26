(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

;;; @module meta/skill-map
;;; @requires prelude manifest

(unless (top-level-bound? 'parse-manifest)
  (load "lattice/meta/manifest.ss"))

(doc 'module 'meta/skill-map)
(doc 'description "Maps module names to their owning skill. Given parsed manifests,
builds a lookup table from module names to skill names. This is the
critical link for deriving skill-level dependencies from require chains:
module A requires module B → skill X depends on skill Y.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; Skill Map Construction
;;; ====

(doc 'section 'construction)

;;; build-skill-map : (List ManifestData) -> (Alist Symbol Symbol)
;;; For each manifest's modules list, map every module name to the skill name.
;;; Also maps namespaced forms (skill/module -> skill).
;;; Returns: ((module-name . skill-name) ...)
;;;
;;; Example: Given linalg manifest with modules (vec, matrix, sparse, ...),
;;; returns ((vec . linalg) (matrix . linalg) (sparse . linalg) ...)
(doc build-skill-map 'type '(-> (List ManifestData) (Alist Symbol Symbol)))
(doc build-skill-map 'export #t)
(define (build-skill-map manifests)
  (let ([result '()])
    (for-each
     (lambda (manifest)
       (let ([skill-name (cdr (assq 'name manifest))]
             [module-index (manifest->module-index manifest)])
         ;; Map each module to its skill
         (for-each
          (lambda (entry)
            ;; entry = (module-name . file-path)
            (set! result (cons (cons (car entry) skill-name) result)))
          module-index)))
     manifests)
    result))

;;; skill-map-lookup : (Alist Symbol Symbol) Symbol -> (Maybe Symbol)
;;; Look up which skill a module belongs to.
;;; Returns the skill name or #f if not found.
(doc skill-map-lookup 'type '(-> (Alist Symbol Symbol) Symbol (Maybe Symbol)))
(doc skill-map-lookup 'export #t)
(define (skill-map-lookup smap mod-name)
  (let ([entry (assq mod-name smap)])
    (if entry (cdr entry) #f)))

;;; ====
;;; Path-Based Fallback
;;; ====

(doc 'section 'path-fallback)

;;; path-based-skill : String (List ManifestData) -> (Maybe Symbol)
;;; Given a file path like "lattice/linalg/vec.ss", find which skill
;;; directory it belongs to by matching against known skill paths.
;;; Returns the skill name or #f if no match.
(doc path-based-skill 'type '(-> String (List ManifestData) (Maybe Symbol)))
(doc path-based-skill 'export #t)
(define (path-based-skill file-path manifests)
  (let loop ([remaining manifests])
    (if (null? remaining)
        #f
        (let* ([manifest (car remaining)]
               [skill-name (cdr (assq 'name manifest))]
               [skill-path (cdr (assq 'path manifest))])
          (if (and (string? skill-path)
                   (> (string-length skill-path) 0)
                   (string-prefix-sm? (string-append skill-path "/") file-path))
              skill-name
              (loop (cdr remaining)))))))

;;; string-prefix-sm? : String String -> Boolean
(define (string-prefix-sm? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; ====
;;; Skill Map Statistics
;;; ====

(doc 'section 'statistics)

;;; skill-map-stats : (Alist Symbol Symbol) -> Void
;;; Print statistics about the skill map.
(doc skill-map-stats 'type '(-> (Alist Symbol Symbol) Void))
(doc skill-map-stats 'export #t)
(define (skill-map-stats smap)
  (let* ([skills (deduplicate-sm (map cdr smap))]
         [skill-counts
          (map (lambda (skill)
                 (cons skill (length (filter (lambda (e) (eq? (cdr e) skill)) smap))))
               skills)]
         [sorted (sort-by-count skill-counts)])
    (printf "Skill map: ~a modules across ~a skills\n"
            (length smap) (length skills))
    (for-each
     (lambda (entry)
       (printf "  ~a: ~a modules\n" (car entry) (cdr entry)))
     sorted)))

(define (deduplicate-sm lst)
  (let loop ([remaining lst] [seen '()] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (let ([x (car remaining)])
          (if (memq x seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining) (cons x seen) (cons x acc)))))))

(define (sort-by-count alist)
  ;; Simple insertion sort by count descending
  (let loop ([remaining alist] [sorted '()])
    (if (null? remaining)
        sorted
        (loop (cdr remaining) (insert-by-count (car remaining) sorted)))))

(define (insert-by-count entry sorted)
  (cond
    [(null? sorted) (list entry)]
    [(> (cdr entry) (cdr (car sorted)))
     (cons entry sorted)]
    [else (cons (car sorted) (insert-by-count entry (cdr sorted)))]))
