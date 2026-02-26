;;; user/sft/extract/cloze.ss — Structural cloze/FIM SFT sample extraction
;;;
;;; Exploits homoiconicity for structural fill-in-the-middle tasks:
;;;   1. Expression cloze: Replace one subexpression with <HOLE>, fill it
;;;   2. Function cloze:   Remove one function from module, provide peers as context
;;;
;;; Requires walker.ss, emit.ss, spec-to-code.ss loaded first.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (load "user/sft/extract/emit.ss")
;;;   (load "user/sft/extract/spec-to-code.ss")
;;;   (load "user/sft/extract/cloze.ss")
;;;   (define ir (walk-all-manifests))
;;;   (define samples (generate-cloze-samples ir))

;;; ====
;;; Dependencies
;;; ====

(unless (top-level-bound? 'walk-all-manifests)
  (error 'cloze "walker.ss must be loaded first"))
(unless (top-level-bound? 'make-sample)
  (error 'cloze "emit.ss must be loaded first"))
(unless (top-level-bound? 's2c/build-verify-expr)
  (error 'cloze "spec-to-code.ss must be loaded first"))

;;; ====
;;; S-expression analysis
;;; ====

(define (cloze/sexp-size sexp)
  ;; Count atoms in an S-expression tree.
  (cond
    [(pair? sexp) (+ (cloze/sexp-size (car sexp))
                     (cloze/sexp-size (cdr sexp)))]
    [(null? sexp) 0]
    [else 1]))

(define (cloze/interesting-subexpr? sexp)
  ;; Good hole candidate: proper list, expression-like, right size.
  ;; Size >= 2 captures (f x) calls that were previously missed.
  (and (pair? sexp)
       (list? sexp)
       (> (length sexp) 1)
       (let ([size (cloze/sexp-size sexp)])
         (and (>= size 2) (<= size 40)))
       ;; Starts with symbol or nested form (looks like function call)
       (or (symbol? (car sexp)) (pair? (car sexp)))
       ;; Exclude structural forms that are too big/boring as holes
       (not (memq (car sexp) '(define lambda let let* letrec
                                 cond case begin quote
                                 define-record-type)))))

(define (cloze/collect-candidates body-sexp max-depth)
  ;; Walk body tree, collect subexpressions suitable for holing.
  ;; Returns list of subexpression references (identity-based).
  ;; Caps at 20 candidates; respects max-depth.
  ;; Skips quoted forms — (quote ...) contains literal data, not code.
  (let ([candidates '()])
    (let walk ([sexp body-sexp] [depth 0])
      (when (and (< (length candidates) 20)
                 (pair? sexp)
                 (< depth max-depth))
        ;; Skip quoted forms entirely — they're literal data, not executable code
        (unless (eq? (car sexp) 'quote)
          ;; Consider this form at depth > 0 (skip the body root)
          (when (and (> depth 0) (cloze/interesting-subexpr? sexp))
            (set! candidates (cons sexp candidates)))
          ;; Recurse into proper list elements
          (when (list? sexp)
            (for-each (lambda (elem) (walk elem (+ depth 1))) sexp)))))
    (reverse candidates)))

(define (cloze/score-candidate sexp deps)
  ;; Score a hole candidate. Higher = better for training.
  (let* ([size (cloze/sexp-size sexp)]
         ;; Prefer medium-sized holes (5-15 atoms)
         [size-score (cond [(< size 5) 1] [(< size 15) 3] [(< size 25) 2] [else 1])]
         ;; Bonus for referencing cross-function dependencies
         [dep-score (if (and (pair? deps)
                             (find (lambda (d) (cloze/tree-mentions? sexp d)) deps))
                        2 0)])
    (+ size-score dep-score)))

(define (cloze/tree-mentions? sexp sym)
  ;; Does sexp contain sym anywhere?
  (cond
    [(eq? sexp sym) #t]
    [(pair? sexp) (or (cloze/tree-mentions? (car sexp) sym)
                      (cloze/tree-mentions? (cdr sexp) sym))]
    [else #f]))

(define (cloze/best-candidate candidates deps)
  ;; Pick the best hole candidate by score. Returns the candidate or #f.
  (if (null? candidates) #f
      (let loop ([cs candidates] [best (car candidates)] [best-score -1])
        (if (null? cs)
            best
            (let ([score (cloze/score-candidate (car cs) deps)])
              (if (> score best-score)
                  (loop (cdr cs) (car cs) score)
                  (loop (cdr cs) best best-score)))))))

(define (cloze/top-n scored n)
  ;; Select top N scored candidates by score (car). Avoids `sort` which
  ;; gets poisoned by lattice/data/sort.ss during walker module loading.
  (let loop ([remaining scored] [best '()])
    (if (or (null? remaining) (>= (length best) n))
        (reverse best)
        (let* ([candidate (car remaining)]
               [score (car candidate)]
               ;; Find insertion point in best (sorted descending)
               [inserted
                 (let ins ([bs best] [acc '()])
                   (cond
                     [(null? bs) (reverse (cons candidate acc))]
                     [(> score (caar bs))
                      (append (reverse (cons candidate acc)) bs)]
                     [else (ins (cdr bs) (cons (car bs) acc))]))])
          (loop (cdr remaining)
                (if (> (length inserted) n)
                    (list-head inserted n)
                    inserted))))))

;;; ====
;;; Tree surgery
;;; ====

(define *cloze-hole-sym* (string->symbol "<HOLE>"))

(define (cloze/tree-replace tree target replacement)
  ;; Replace target (by eq? identity) with replacement in tree.
  (cond
    [(eq? tree target) replacement]
    [(pair? tree)
     (let ([new-car (cloze/tree-replace (car tree) target replacement)]
           [new-cdr (cloze/tree-replace (cdr tree) target replacement)])
       (if (and (eq? new-car (car tree)) (eq? new-cdr (cdr tree)))
           tree
           (cons new-car new-cdr)))]
    [else tree]))

;;; ====
;;; Expression cloze samples
;;; ====

(define (cloze/expression-sample ir hole-site)
  ;; Build one expression-cloze sample.
  ;; Shows the function with one subexpression replaced by <HOLE>.
  (let* ([name (cdr (assq 'name ir))]
         [skill (cdr (assq 'skill ir))]
         [module (cdr (assq 'module ir))]
         [file (cdr (assq 'file ir))]
         [clean-define (cdr (assq 'clean-define ir))]
         [tests (cdr (assq 'tests ir))]
         [desc (cdr (assq 'description ir))]
         [type-sig (cdr (assq 'type ir))]
         [difficulty (emit/ir-difficulty ir)]
         ;; Build holed version
         [holed-define (cloze/tree-replace clean-define hole-site *cloze-hole-sym*)]
         [holed-str (emit/sexp->pretty-string holed-define)]
         [skill-desc (s2c/skill-description skill)]
         [id (emit/next-id (format "~a_cloze_expr_~a" skill name))]
         [prompt-body
           (string-append
             (format "Complete the following Fold Scheme function by replacing `<HOLE>` with the correct expression.\n\nModule: ~a (~a)\n\n```scheme\n~a\n```\n"
                     file skill-desc holed-str)
             (if type-sig
                 (format "\nType: ~a\n" (s2c/format-type-sig type-sig))
                 "")
             (if desc (format "Brief: ~a\n" desc) "")
             "\nReturn the complete function definition with <HOLE> filled in.")]
         [gt-str (emit/sexp->pretty-string clean-define)]
         [ve (s2c/build-verify-expr ir)]
         [ve-str (if ve (emit/sexp->pretty-string ve) "")]
         [tags (emit/make-tags skill module "cloze-expr" name)]
         [sample (make-sample id "cloze" "implementation"
                              difficulty file
                              (if (pair? tests) file "")
                              name prompt-body gt-str ve-str tags)])
    (append sample
            `((ground_truth_sexp . ,clean-define)
              (verify_sexp . ,ve)))))

;;; ====
;;; Function-level cloze samples
;;; ====

(define (cloze/group-exported-by-file ir-records)
  ;; Group exported+tested functions by source file.
  ;; Returns ((file . (ir1 ir2 ...)) ...) — only modules with 3+ functions.
  (let loop ([rs ir-records] [groups '()])
    (if (null? rs)
        (filter (lambda (g) (>= (length (cdr g)) 3))
                (map (lambda (g) (cons (car g) (reverse (cdr g)))) groups))
        (let* ([ir (car rs)]
               [file (cdr (assq 'file ir))]
               [entry (find (lambda (g) (string=? (car g) file)) groups)])
          (if entry
              (begin (set-cdr! entry (cons ir (cdr entry)))
                     (loop (cdr rs) groups))
              (loop (cdr rs)
                    (cons (cons file (list ir)) groups)))))))

(define (cloze/format-peer-defines peers max-peers)
  ;; Format peer functions as context string (up to max-peers).
  (let ([selected (if (> (length peers) max-peers)
                      (list-head peers max-peers)
                      peers)])
    (apply string-append
      (map (lambda (ir)
             (string-append (emit/sexp->pretty-string (cdr (assq 'clean-define ir)))
                            "\n\n"))
           selected))))

(define (cloze/function-sample ir peers)
  ;; Build one function-level cloze sample.
  ;; Shows peer functions as context, asks to implement the missing one.
  ;; Skip value definitions — they have no parameters and produce broken sigs.
  (let* ([formals-check (cdr (assq 'formals ir))]
         [_ (unless (or (pair? formals-check) (symbol? formals-check)
                        (null? formals-check))
              (error 'cloze "skipping value def"))]
         ;; Check if original define was a function def (not a value binding)
         [clean-def (cdr (assq 'clean-define ir))]
         [_ (unless (pair? (cadr clean-def))
              (error 'cloze "skipping value definition — no function signature"))]
         [name (cdr (assq 'name ir))]
         [skill (cdr (assq 'skill ir))]
         [module (cdr (assq 'module ir))]
         [file (cdr (assq 'file ir))]
         [clean-define (cdr (assq 'clean-define ir))]
         [tests (cdr (assq 'tests ir))]
         [formals (cdr (assq 'formals ir))]
         [desc (cdr (assq 'description ir))]
         [type-sig (cdr (assq 'type ir))]
         [difficulty (emit/ir-difficulty ir)]
         [skill-desc (s2c/skill-description skill)]
         [context-str (cloze/format-peer-defines peers 5)]
         [flat-formals (walker/flatten-formals formals)]
         [sig-str (format "(define (~a~a) ...)"
                          name
                          (apply string-append
                            (map (lambda (f) (format " ~a" f)) flat-formals)))]
         [id (emit/next-id (format "~a_cloze_fn_~a" skill name))]
         [prompt-body
           (string-append
             (format "This module implements ~a utilities. One function is missing.\n\nModule: ~a\n\nExisting functions in this module:\n```scheme\n~a```\n"
                     skill-desc file context-str)
             (format "\nMissing function: `~a`\n" sig-str)
             (if type-sig
                 (format "Type: ~a\n" (s2c/format-type-sig type-sig))
                 "")
             (if desc (format "Brief: ~a\n" desc) "")
             (format "\nImplement `~a`. Return only the complete function definition."
                     name))]
         [gt-str (emit/sexp->pretty-string clean-define)]
         [ve (s2c/build-verify-expr ir)]
         [ve-str (if ve (emit/sexp->pretty-string ve) "")]
         [tags (emit/make-tags skill module "cloze-fn" name)]
         [sample (make-sample id "cloze" "implementation"
                              difficulty file
                              (if (pair? tests) file "")
                              name prompt-body gt-str ve-str tags)])
    (append sample
            `((ground_truth_sexp . ,clean-define)
              (verify_sexp . ,ve)))))

;;; ====
;;; Main generator
;;; ====

(define (generate-cloze-samples ir-records . opts)
  ;; Generate cloze samples from walker IR.
  ;; Returns flat list of samples.
  (let* ([exported-only? (if (pair? opts) (car opts) #t)]
         [candidates (filter
                       (lambda (ir)
                         (and (if exported-only? (cdr (assq 'exported? ir)) #t)
                              (pair? (cdr (assq 'tests ir)))
                              (pair? (cdr (assq 'body ir)))))
                       ir-records)]
         [_ (emit/reset-counter!)]
         [_ (printf "Generating cloze samples from ~a candidates...\n"
                    (length candidates))])
    (let ([expr-samples '()]
          [fn-samples '()]
          [expr-count 0]
          [fn-count 0])

      ;; 1. Expression cloze: up to 2 per function (different holes)
      (for-each
        (lambda (ir)
          (guard (exn [else #f])
            (let* ([body (cdr (assq 'body ir))]
                   [deps (let ([d (assq 'deps ir)]) (if d (cdr d) '()))]
                   [body-sexp (if (and (pair? body) (= (length body) 1))
                                  (car body)
                                  (cons 'begin body))]
                   [holes (cloze/collect-candidates body-sexp 6)]
                   ;; Pick top 2 by score (avoid `sort` — poisoned by lattice/data/sort.ss)
                   [scored (map (lambda (h) (cons (cloze/score-candidate h deps) h))
                                holes)]
                   [top-2 (cloze/top-n scored 2)])
              (for-each
                (lambda (scored-hole)
                  (let ([sample (cloze/expression-sample ir (cdr scored-hole))])
                    (when sample
                      (set! expr-samples (cons sample expr-samples))
                      (set! expr-count (+ expr-count 1)))))
                top-2))))
        candidates)

      ;; 2. Function-level cloze: up to 2 per module file
      (let ([file-groups (cloze/group-exported-by-file candidates)])
        (for-each
          (lambda (group)
            (let* ([fns (cdr group)]
                   [count 0])
              (for-each
                (lambda (ir)
                  (when (< count 2)
                    (guard (exn [else #f])
                      (let* ([name (cdr (assq 'name ir))]
                             [peers (filter (lambda (f)
                                             (not (eq? (cdr (assq 'name f)) name)))
                                           fns)]
                             [sample (cloze/function-sample ir peers)])
                        (when sample
                          (set! fn-samples (cons sample fn-samples))
                          (set! fn-count (+ fn-count 1))
                          (set! count (+ count 1)))))))
                fns)))
          file-groups))

      (let ([all (append (reverse expr-samples) (reverse fn-samples))])
        (printf "Generated ~a cloze samples:\n" (length all))
        (printf "  Expression cloze: ~a\n" expr-count)
        (printf "  Function cloze:   ~a\n" fn-count)
        all))))

;;; ====
;;; REPL interface
;;; ====

(printf "cloze.ss loaded.\n")
(printf "  (generate-cloze-samples ir)       - Generate from walker IR (exported only)\n")
(printf "  (generate-cloze-samples ir #f)    - Generate from ALL functions\n")
