(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'meta/search)
(require 'meta/dag)
(require 'hamt)

;;; @module meta/serendipity
;;; @requires meta/search meta/dag hamt
(doc 'module 'serendipity)
(doc 'description "Serendipitous discovery for lattice navigation. Given a symbol, surfaces
related exports, nearby skills, and cross-domain connections that the user
didn't know to search for. Turns point queries into neighborhood browsing.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Co-Module Related Exports
;;; ====

(doc 'section 'co-module)

(doc co-module-exports 'type (-> Symbol (List Symbol)))
(doc co-module-exports 'description "Find exports from the same module as the given export.
Returns up to 15 sibling exports (excluding the input symbol).")
(define (co-module-exports sym)
  (ensure-indexed!)
  (let ([mod (hamt-lookup sym *export-module-map*)])
    (if (not mod)
        '()
        ;; Find all exports mapped to the same module
        (let ([siblings '()])
          (for-each
            (lambda (export-entry)
              (let ([name (car export-entry)])
                (when (and (not (eq? name sym))
                           (eq? mod (hamt-lookup name *export-module-map*)))
                  (set! siblings (cons name siblings)))))
            (kg-exports))
          (take-at-most 15 (reverse siblings))))))

(doc export-module 'type (-> Symbol (Maybe Symbol)))
(doc export-module 'description "Find which module an export belongs to.")
(define (export-module sym)
  (ensure-indexed!)
  (hamt-lookup sym *export-module-map*))

(doc export-skill 'type (-> Symbol (Maybe Symbol)))
(doc export-skill 'description "Find which skill an export belongs to.")
(define (export-skill sym)
  (lattice-export-source sym))

;;; ====
;;; Co-Skill Related Exports
;;; ====

(doc 'section 'co-skill)

(doc co-skill-exports 'type (-> Symbol Nat (List (Pair Symbol Symbol))))
(doc co-skill-exports 'description "Find exports from the same skill but different modules.
Returns a list of (export-name . module-name) pairs, up to k results.
These are the 'nearby but not adjacent' functions — same domain, different facet.")
(define (co-skill-exports sym k)
  (ensure-indexed!)
  (let ([skill (lattice-export-source sym)]
        [own-mod (hamt-lookup sym *export-module-map*)])
    (if (not skill)
        '()
        (let ([results '()])
          (for-each
            (lambda (export-entry)
              (let* ([name (car export-entry)]
                     [mod (hamt-lookup name *export-module-map*)])
                (when (and (not (eq? name sym))
                           mod
                           (not (eq? mod own-mod))
                           (eq? skill (lattice-export-source name)))
                  (set! results (cons (cons name mod) results)))))
            (kg-exports))
          (take-at-most k (reverse results))))))

;;; ====
;;; Export Neighbors (BM25 Similarity)
;;; ====

(doc 'section 'export-neighbors)

(doc export-neighbors 'type (-> Symbol Nat (List (Pair Symbol Number))))
(doc export-neighbors 'description "Find exports with similar names/terms using BM25 scoring.
Tokenizes the input symbol and searches the export index. Filters out
exact matches. Returns (name . score) pairs ranked by relevance.")
(define (export-neighbors sym k)
  (ensure-indexed!)
  (let* ([terms (export->terms sym)]
         [results (bm25-search *export-index* terms (* k 2))]  ; over-fetch to filter
         [filtered (filter (lambda (r) (not (eq? (car r) sym))) results)])
    (take-at-most k filtered)))

;;; ====
;;; Related Skills
;;; ====

(doc 'section 'related-skills)

(doc related-skills 'type (-> Symbol Alist))
(doc related-skills 'description "Find skills related to the given skill through DAG proximity.
Returns a tagged alist with:
  - deps: direct dependencies
  - dependents: skills that depend on this one
  - siblings: skills sharing the same dependencies (co-dependents)
  - nearby: skills within 2 hops in the DAG")
(define (related-skills skill-name)
  (let* ([deps (lattice-deps skill-name)]
         [dependents (lattice-uses skill-name)]
         ;; Siblings: skills that share at least one dependency with us
         [siblings (find-siblings skill-name deps)]
         ;; Nearby: union of deps-of-deps and dependents-of-dependents (2-hop)
         [nearby (find-nearby skill-name deps dependents)])
    (list 'related-skills
          (list 'skill skill-name)
          (list 'deps deps)
          (list 'dependents dependents)
          (list 'siblings siblings)
          (list 'nearby nearby))))

(doc find-siblings 'type (-> Symbol (List Symbol) (List Symbol)))
(doc find-siblings 'description "Find skills that share at least one dependency with the given skill.")
(define (find-siblings skill-name own-deps)
  (if (null? own-deps)
      '()
      (let ([siblings (fold-left
                        (lambda (acc dep)
                          (fold-left
                            (lambda (acc2 user)
                              (if (eq? user skill-name)
                                  acc2
                                  (hamt-assoc user #t acc2)))
                            acc
                            (lattice-uses dep)))
                        hamt-empty
                        own-deps)])
        (hamt-keys siblings))))

(doc find-nearby 'type (-> Symbol (List Symbol) (List Symbol) (List Symbol)))
(doc find-nearby 'description "Find skills within 2 hops in the DAG (excluding self, deps, and dependents).")
(define (find-nearby skill-name deps dependents)
  (let* ([seen (hamt-assoc skill-name #t hamt-empty)]
         [seen (fold-left (lambda (acc d) (hamt-assoc d #t acc)) seen deps)]
         [seen (fold-left (lambda (acc d) (hamt-assoc d #t acc)) seen dependents)])
    ;; 2-hop: deps of deps, then dependents of dependents
    (let loop ([sources (append (append-map lattice-deps deps)
                                (append-map lattice-uses dependents))]
               [seen seen]
               [nearby '()])
      (if (null? sources)
          (reverse nearby)
          (let ([dep2 (car sources)])
            (if (hamt-has-key? dep2 seen)
                (loop (cdr sources) seen nearby)
                (loop (cdr sources)
                      (hamt-assoc dep2 #t seen)
                      (cons dep2 nearby))))))))

;;; ====
;;; Unified Browse
;;; ====

(doc 'section 'browse)

(doc browse-from 'type (-> Symbol Alist))
(doc browse-from 'description "Browse the neighborhood of any symbol. Works for exports, skills, or modules.
Returns a tagged alist with the symbol's context: what it is, where it lives,
and what's nearby. Designed for serendipitous exploration.")
(define (browse-from sym)
  (ensure-indexed!)
  (let ([exact (lattice-find-exact sym)])
    (if (not exact)
        (list 'browse-result
              (list 'found? #f)
              (list 'symbol sym)
              (list 'suggestions (map car (bm25-search *export-index*
                                            (export->terms sym) 5))))
        (let ([type (caddr exact)])
          (case type
            [(export) (browse-export sym)]
            [(skill) (browse-skill sym)]
            [(module) (browse-module sym)]
            [else (list 'browse-result
                        (list 'found? #t)
                        (list 'symbol sym)
                        (list 'type type))])))))

(doc browse-export 'type (-> Symbol Alist))
(define (browse-export sym)
  (let* ([mod (export-module sym)]
         [skill (export-skill sym)]
         [siblings (co-module-exports sym)]
         [skill-peers (co-skill-exports sym 10)]
         [neighbors (export-neighbors sym 8)])
    (list 'browse-result
          (list 'found? #t)
          (list 'symbol sym)
          (list 'type 'export)
          (list 'module mod)
          (list 'skill skill)
          (list 'require (or mod sym))
          (list 'same-module siblings)
          (list 'same-skill (map car skill-peers))
          (list 'similar (map car neighbors)))))

(doc browse-skill 'type (-> Symbol Alist))
(define (browse-skill sym)
  (let* ([related (related-skills sym)]
         [data (kg-skill-data sym)]
         [desc (if data
                   (let ([d (assq 'description data)])
                     (if d (cdr d) ""))
                   "")])
    (list 'browse-result
          (list 'found? #t)
          (list 'symbol sym)
          (list 'type 'skill)
          (list 'description desc)
          (list 'deps (cadr (assq 'deps (cdr related))))
          (list 'dependents (cadr (assq 'dependents (cdr related))))
          (list 'siblings (cadr (assq 'siblings (cdr related))))
          (list 'nearby (cadr (assq 'nearby (cdr related)))))))

(doc browse-module 'type (-> Symbol Alist))
(define (browse-module sym)
  (let* ([sym-str (symbol->string sym)]
         ;; Extract skill name from module key (e.g., linalg/vec -> linalg)
         [skill-name (if (string-contains? sym-str "/")
                         (string->symbol
                           (substring sym-str 0
                             (let loop ([i 0])
                               (if (char=? (string-ref sym-str i) #\/)
                                   i
                                   (loop (+ i 1))))))
                         #f)]
         [related (if skill-name (related-skills skill-name) #f)])
    (list 'browse-result
          (list 'found? #t)
          (list 'symbol sym)
          (list 'type 'module)
          (list 'skill skill-name)
          (list 'nearby-skills
                (if related
                    (append (cadr (assq 'deps (cdr related)))
                            (cadr (assq 'dependents (cdr related))))
                    '())))))

;;; ====
;;; Pretty Printing
;;; ====

(doc 'section 'display)

(doc print-browse 'type (-> Symbol Void))
(doc print-browse 'description "Pretty-print the browse result for a symbol. Shows the symbol's context,
co-located exports, and related discoveries.")
(define (print-browse sym)
  (let ([result (browse-from sym)])
    (let ([found? (cadr (assq 'found? (cdr result)))]
          [type (let ([t (assq 'type (cdr result))]) (if t (cadr t) #f))])
      (cond
        [(not found?)
         (printf "  ~a not found in lattice.\n" sym)
         (let ([suggestions (cadr (assq 'suggestions (cdr result)))])
           (unless (null? suggestions)
             (printf "  Did you mean: ~a\n"
               (apply string-append
                 (map (lambda (s) (string-append (symbol->string s) " "))
                      suggestions)))))]
        [(eq? type 'export) (print-browse-export result)]
        [(eq? type 'skill) (print-browse-skill result)]
        [(eq? type 'module) (print-browse-module result)]
        [else (printf "  ~a [~a]\n" sym type)]))))

(define (print-browse-export result)
  (let ([sym (cadr (assq 'symbol (cdr result)))]
        [mod (cadr (assq 'module (cdr result)))]
        [skill (cadr (assq 'skill (cdr result)))]
        [siblings (cadr (assq 'same-module (cdr result)))]
        [peers (cadr (assq 'same-skill (cdr result)))]
        [similar (cadr (assq 'similar (cdr result)))])
    (printf "\n  ~a" sym)
    (when mod (printf "  (require '~a)" mod))
    (when skill (printf "  [~a]" skill))
    (printf "\n")
    (unless (null? siblings)
      (printf "\n  Same module:  ~a\n"
        (format-symbol-list siblings 6)))
    (unless (null? peers)
      (printf "  Same skill:   ~a\n"
        (format-symbol-list peers 6)))
    (unless (null? similar)
      (printf "  Similar:      ~a\n"
        (format-symbol-list similar 6)))
    (printf "\n")))

(define (print-browse-skill result)
  (let ([sym (cadr (assq 'symbol (cdr result)))]
        [desc (cadr (assq 'description (cdr result)))]
        [deps (cadr (assq 'deps (cdr result)))]
        [dependents (cadr (assq 'dependents (cdr result)))]
        [siblings (cadr (assq 'siblings (cdr result)))]
        [nearby (cadr (assq 'nearby (cdr result)))])
    (printf "\n  ~a [skill]" sym)
    (when (and (string? desc) (> (string-length desc) 0))
      (printf " — ~a" (truncate-string desc 60)))
    (printf "\n")
    (unless (null? deps)
      (printf "\n  Depends on:   ~a\n" (format-symbol-list deps 20)))
    (unless (null? dependents)
      (printf "  Used by:      ~a\n" (format-symbol-list dependents 20)))
    (unless (null? siblings)
      (printf "  Co-dependents:~a\n" (format-symbol-list siblings 6)))
    (unless (null? nearby)
      (printf "  Nearby (2-hop):~a\n" (format-symbol-list nearby 6)))
    (printf "\n")))

(define (print-browse-module result)
  (let ([sym (cadr (assq 'symbol (cdr result)))]
        [skill (cadr (assq 'skill (cdr result)))]
        [nearby (cadr (assq 'nearby-skills (cdr result)))])
    (printf "\n  ~a [module]" sym)
    (when skill (printf "  part of ~a" skill))
    (printf "\n")
    (unless (null? nearby)
      (printf "\n  Related skills: ~a\n" (format-symbol-list nearby 10)))
    (printf "\n")))

(doc format-symbol-list 'type (-> (List Symbol) Nat String))
(doc format-symbol-list 'description "Format a list of symbols as a space-separated string, truncated to max-count.")
(define (format-symbol-list syms max-count)
  (let* ([shown (take-at-most max-count syms)]
         [hidden (- (length syms) (length shown))]
         [base (apply string-append
                 (map (lambda (s) (string-append (symbol->string s) " "))
                      shown))])
    (if (> hidden 0)
        (string-append base (format "(+~a more)" hidden))
        base)))

;;; ====
;;; Convenience
;;; ====

(doc 'section 'convenience)

(doc lr 'type (-> Symbol Void))
(doc lr 'description "Browse related: show the neighborhood of any lattice symbol.
Works for exports, skills, and modules.")
(define (lr sym)
  (print-browse sym))

(doc lrr 'type (-> Symbol Void))
(doc lrr 'description "Browse related (raw): return the browse data structure instead of printing.")
(define (lrr sym)
  (browse-from sym))
