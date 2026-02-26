;;; user/sft/extract/compose.ss — DAG-guided composition SFT sample extraction
;;;
;;; Walk the @requires DAG to find functions with cross-module dependencies.
;;; Generate composition tasks: given available functions from other modules,
;;; implement a function that composes them.
;;;
;;; Cross-module deps are found by resolving each function's `deps` against
;;; the full walker IR — if a dep is an exported function from a different
;;; source file, it's a cross-module dependency.
;;;
;;; Requires walker.ss, emit.ss, spec-to-code.ss loaded first.
;;;
;;; Usage:
;;;   (load "user/sft/extract/walker.ss")
;;;   (load "user/sft/extract/emit.ss")
;;;   (load "user/sft/extract/spec-to-code.ss")
;;;   (load "user/sft/extract/compose.ss")
;;;   (define ir (walk-all-manifests))
;;;   (define samples (generate-composition-samples ir))

;;; ====
;;; Dependencies
;;; ====

(unless (top-level-bound? 'walk-all-manifests)
  (error 'compose "walker.ss must be loaded first"))
(unless (top-level-bound? 'make-sample)
  (error 'compose "emit.ss must be loaded first"))
(unless (top-level-bound? 's2c/build-verify-expr)
  (error 'compose "spec-to-code.ss must be loaded first"))

;;; ====
;;; Cross-module dependency analysis
;;; ====

(define (compose/build-name-index ir-records)
  ;; Build name → list-of-IR-records index for exported functions.
  ;; Used to resolve dep symbols to their source modules.
  (let ([index '()])
    (for-each
      (lambda (ir)
        (when (cdr (assq 'exported? ir))
          (let* ([name (cdr (assq 'name ir))]
                 [existing (assq name index)])
            (if existing
                (set-cdr! existing (cons ir (cdr existing)))
                (set! index (cons (cons name (list ir)) index))))))
      ir-records)
    index))

(define (compose/cross-module-deps ir name-index)
  ;; Find deps of ir that are exported from other source files.
  ;; Returns list of dep IR records (one per unique dep function).
  (let* ([f-file (cdr (assq 'file ir))]
         [deps (let ([d (assq 'deps ir)]) (if d (cdr d) '()))])
    (filter-map
      (lambda (dep-name)
        (let ([entries (assq dep-name name-index)])
          (and entries
               ;; Find dep defined in a DIFFERENT file
               (find (lambda (dep-ir)
                       (not (string=? (cdr (assq 'file dep-ir)) f-file)))
                     (cdr entries)))))
      deps)))

(define (compose/format-available-fn dep-ir)
  ;; Format one available function for the prompt.
  ;; Shows signature, type, and description.
  (let* ([name (cdr (assq 'name dep-ir))]
         [type-sig (cdr (assq 'type dep-ir))]
         [desc (cdr (assq 'description dep-ir))]
         [formals (cdr (assq 'formals dep-ir))]
         [flat-formals (walker/flatten-formals formals)])
    (string-append
      (format "  (~a~a)"
              name
              (apply string-append
                (map (lambda (f) (format " ~a" f)) flat-formals)))
      (if type-sig (format " : ~a" (s2c/format-type-sig type-sig)) "")
      (if desc (format "\n    ;; ~a" desc) "")
      "\n")))

(define (compose/dep-skill-desc dep-ir)
  ;; Short origin label for a dependency.
  (let ([skill (cdr (assq 'skill dep-ir))]
        [module (cdr (assq 'module dep-ir))])
    (format "~a/~a" skill module)))

;;; ====
;;; Composition sample generator
;;; ====

(define (compose/make-sample ir cross-deps)
  ;; Build a composition sample from a function and its cross-module deps.
  (let* ([name (cdr (assq 'name ir))]
         [skill (cdr (assq 'skill ir))]
         [module (cdr (assq 'module ir))]
         [file (cdr (assq 'file ir))]
         [clean-define (cdr (assq 'clean-define ir))]
         [tests (cdr (assq 'tests ir))]
         [desc (cdr (assq 'description ir))]
         [type-sig (cdr (assq 'type ir))]
         [formals (cdr (assq 'formals ir))]
         [skill-desc (s2c/skill-description skill)]
         ;; Cap at 6 deps to keep prompts manageable
         [difficulty (emit/ir-difficulty ir)]
         [selected-deps (if (> (length cross-deps) 6)
                            (list-head cross-deps 6)
                            cross-deps)]
         [avail-block (apply string-append
                        (map compose/format-available-fn selected-deps))]
         [avail-names (map (lambda (d)
                             (symbol->string (cdr (assq 'name d))))
                           selected-deps)]
         ;; Collect unique source domains for the deps
         [dep-origins (let loop ([ds selected-deps] [seen '()] [acc '()])
                        (if (null? ds) (reverse acc)
                            (let ([origin (compose/dep-skill-desc (car ds))])
                              (if (member origin seen)
                                  (loop (cdr ds) seen acc)
                                  (loop (cdr ds)
                                        (cons origin seen)
                                        (cons origin acc))))))]
         [flat-formals (walker/flatten-formals formals)]
         [sig-str (format "(~a~a)"
                          name
                          (apply string-append
                            (map (lambda (f) (format " ~a" f)) flat-formals)))]
         [id (emit/next-id (format "~a_compose_~a" skill name))]
         [prompt-body
           (string-append
             (format "Implement `~a` by composing functions from the ~a lattice.\n\n"
                     name skill-desc)
             (if (> (length dep-origins) 1)
                 (format "This function bridges: ~a\n\n"
                         (apply string-append
                           (map (lambda (o) (string-append o ", ")) dep-origins)))
                 "")
             (format "Available functions:\n```scheme\n~a```\n\n" avail-block)
             (format "Target: `~a`\n" sig-str)
             (if type-sig (format "Type: ~a\n" (s2c/format-type-sig type-sig)) "")
             (if desc (format "Spec: ~a\n" desc) "")
             (format "\nWrite the complete definition for `~a`. Use the available functions above."
                     name))]
         [gt-str (emit/sexp->pretty-string clean-define)]
         [ve (s2c/build-verify-expr ir)]
         [ve-str (if ve (emit/sexp->pretty-string ve) "")]
         [tags (append (emit/make-tags skill module "compose" name)
                       (if (> (length dep-origins) 1) '("cross-domain") '()))]
         [sample (make-sample id "composition" "usage"
                              difficulty file
                              (if (pair? tests) file "")
                              name prompt-body gt-str ve-str tags
                              avail-names)])
    (append sample
            `((ground_truth_sexp . ,clean-define)
              (verify_sexp . ,ve)))))

;;; ====
;;; Main generator
;;; ====

(define (compose/augment-deps cross-deps name-index ir)
  ;; When only 1 cross-dep, augment with the dep's own cross-module deps
  ;; to ensure 2+ available functions in the prompt. Avoids showing the
  ;; target function itself as an available function.
  (if (>= (length cross-deps) 2)
      cross-deps
      ;; Single dep — follow its deps to show the neighborhood
      (let* ([dep-ir (car cross-deps)]
             [dep-deps (guard (exn [else '()])
                         (compose/cross-module-deps dep-ir name-index))]
             ;; Remove the target function itself from the augmented set
             [target-name (cdr (assq 'name ir))]
             [filtered (filter (lambda (d)
                                 (not (eq? (cdr (assq 'name d)) target-name)))
                               dep-deps)]
             ;; Combine: original dep + dep's deps (deduplicated by name)
             [combined (cons (car cross-deps)
                             (let loop ([ds filtered] [seen (list (cdr (assq 'name (car cross-deps))))] [acc '()])
                               (if (null? ds)
                                   (reverse acc)
                                   (let ([n (cdr (assq 'name (car ds)))])
                                     (if (memq n seen)
                                         (loop (cdr ds) seen acc)
                                         (loop (cdr ds) (cons n seen) (cons (car ds) acc)))))))])
        combined)))

(define (generate-composition-samples ir-records . opts)
  ;; Generate composition samples from walker IR.
  ;; Requires cross-module deps AND colocated tests.
  ;; Augments thin contexts (1 dep) with the dep's neighborhood.
  (let* ([exported-only? (if (pair? opts) (car opts) #t)]
         [name-index (compose/build-name-index ir-records)]
         [_ (printf "Built name index: ~a entries\n" (length name-index))]
         [candidates (filter
                       (lambda (ir)
                         (and (if exported-only? (cdr (assq 'exported? ir)) #t)
                              (pair? (cdr (assq 'tests ir)))
                              (pair? (cdr (assq 'body ir)))
                              (pair? (let ([d (assq 'deps ir)])(if d (cdr d) '())))))
                       ir-records)]
         [_ (emit/reset-counter!)]
         [_ (printf "Generating composition samples from ~a candidates...\n"
                    (length candidates))])
    (let loop ([remaining candidates] [acc '()] [skipped 0])
      (if (null? remaining)
          (begin
            (printf "Generated ~a composition samples (~a skipped: insufficient context)\n"
                    (length acc) skipped)
            (reverse acc))
          (let* ([ir (car remaining)]
                 [cross-deps (guard (exn [else '()])
                               (compose/cross-module-deps ir name-index))]
                 ;; Augment thin contexts
                 [augmented (if (null? cross-deps) '()
                                (compose/augment-deps cross-deps name-index ir))])
            (if (< (length augmented) 2)
                ;; Need 2+ available functions for a meaningful composition task
                (loop (cdr remaining) acc (+ skipped 1))
                (let ([sample (guard (exn [else #f])
                                (compose/make-sample ir augmented))])
                  (when (and sample (= (modulo (+ (length acc) 1) 200) 0))
                    (printf "  ... ~a samples so far\n" (+ (length acc) 1)))
                  (loop (cdr remaining)
                        (if sample (cons sample acc) acc)
                        skipped))))))))

;;; ====
;;; REPL interface
;;; ====

(printf "compose.ss loaded.\n")
(printf "  (generate-composition-samples ir)       - Generate from walker IR\n")
(printf "  (generate-composition-samples ir #f)    - Generate from ALL functions\n")
