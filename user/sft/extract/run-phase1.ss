;;; user/sft/extract/run-phase1.ss — Phase 1 extraction pipeline runner
;;;
;;; Walks the lattice for function IR, generates all sample types,
;;; verifies via subprocess isolation, and emits JSONL.
;;;
;;; Usage:
;;;   scheme --script user/sft/extract/run-phase1.ss           # Full pipeline
;;;   scheme --script user/sft/extract/run-phase1.ss no-verify  # Skip verification
;;;   scheme --script user/sft/extract/run-phase1.ss stats-only  # Just print IR stats

;;; Bootstrap
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))

;;; ====
;;; Load infrastructure
;;; ====

(printf "=== Phase 1 Extraction Pipeline ===\n\n")
(printf "Loading infrastructure...\n")

(load "user/sft/extract/walker.ss")
(load "user/sft/extract/emit.ss")
(load "user/sft/extract/verify.ss")
(load "user/sft/extract/spec-to-code.ss")
(load "user/sft/extract/mutate.ss")
(load "user/sft/extract/cloze.ss")
(load "user/sft/extract/compose.ss")
(load "user/sft/extract/type-tasks.ss")

;;; ====
;;; Configuration
;;; ====

(define *phase1-output-root* "data")
(define *phase1-combined-dir* "data/phase1-combined")

(define *run-mode*
  (let ([args (command-line-arguments)])
    (cond
      [(and (pair? args) (string=? (car args) "no-verify")) 'no-verify]
      [(and (pair? args) (string=? (car args) "stats-only")) 'stats-only]
      [else 'full])))

(printf "Mode: ~a\n\n" *run-mode*)

;;; ====
;;; Helpers
;;; ====

(define (ensure-dir! path)
  (guard (ex [else #f])
    (system (format "mkdir -p ~a" path))))

(define (count-by items key-fn)
  (let loop ([remaining items] [counts '()])
    (if (null? remaining)
        counts
        (let* ([s (car remaining)]
               [k (key-fn s)]
               [entry (assoc k counts)])
          (if entry
              (begin (set-cdr! entry (+ (cdr entry) 1))
                     (loop (cdr remaining) counts))
              (loop (cdr remaining)
                    (cons (cons k 1) counts)))))))

;;; ====
;;; Step 1: Walk lattice for function IR
;;; ====

(printf "--- Walking lattice manifests ---\n")
(define *all-ir* (walk-all-manifests))
(printf "Walker: ~a function IR records\n" (length *all-ir*))

;; Print IR stats
(let ([exported (length (filter (lambda (ir) (cdr (assq 'exported? ir))) *all-ir*))]
      [typed (length (filter (lambda (ir) (let ([t (assq 'type ir)]) (and t (cdr t)))) *all-ir*))]
      [tested (length (filter (lambda (ir) (pair? (cdr (assq 'tests ir)))) *all-ir*))]
      [bodied (length (filter (lambda (ir) (pair? (cdr (assq 'body ir)))) *all-ir*))])
  (printf "  Exported: ~a  Typed: ~a  Tested: ~a  Bodied: ~a\n\n"
          exported typed tested bodied))

(when (eq? *run-mode* 'stats-only)
  (printf "Stats-only mode. Done.\n")
  (exit 0))

;;; ====
;;; Step 2: Generate all sample types
;;; ====

(printf "--- Generating spec-to-code ---\n")
(emit/reset-counter!)
(define *spec2code* (generate-spec-to-code *all-ir*))
(printf "  Spec-to-code: ~a samples\n\n" (length *spec2code*))

(printf "--- Generating bugfix/mutation ---\n")
(emit/reset-counter!)
(define *bugfix* (generate-bugfix-samples *all-ir*))
(printf "  Bugfix: ~a samples\n\n" (length *bugfix*))

(printf "--- Generating cloze ---\n")
(emit/reset-counter!)
(define *cloze* (generate-cloze-samples *all-ir*))
(printf "  Cloze: ~a samples\n\n" (length *cloze*))

(printf "--- Generating composition ---\n")
(emit/reset-counter!)
(define *compose* (generate-composition-samples *all-ir*))
(printf "  Composition: ~a samples\n\n" (length *compose*))

(printf "--- Generating type tasks ---\n")
(emit/reset-counter!)
(define *type-tasks* (generate-type-task-samples *all-ir*))
(printf "  Type tasks: ~a samples\n\n" (length *type-tasks*))

;;; ====
;;; Step 3: Combine and deduplicate
;;; ====

(define *all-samples*
  (append *spec2code* *bugfix* *cloze* *compose* *type-tasks*))
(printf "Total before dedup: ~a\n" (length *all-samples*))

(define *deduped* (emit/deduplicate *all-samples*))
(printf "After dedup: ~a\n\n" (length *deduped*))

;;; ====
;;; Step 4: Verify (subprocess-isolated)
;;; ====

(define *verified*
  (if (eq? *run-mode* 'no-verify)
      (begin
        (printf "Skipping verification (no-verify mode)\n\n")
        *deduped*)
      (begin
        (printf "--- Verification (subprocess-isolated) ---\n")
        (let* ([verifiable
                (filter (lambda (s)
                          (let ([ve (assq 'verify_sexp s)])
                            (and ve (cdr ve))))
                        *deduped*)]
               [unverifiable
                (filter (lambda (s)
                          (let ([ve (assq 'verify_sexp s)])
                            (or (not ve) (not (cdr ve)))))
                        *deduped*)]
               [_ (printf "  ~a verifiable, ~a unverifiable (pass-through)\n"
                          (length verifiable) (length unverifiable))]
               [result (verify-batch-sessions verifiable)]
               [passed (car result)]
               [failed (cdr result)])
          (printf "  Passed: ~a  Failed: ~a  Rate: ~a%\n\n"
                  (length passed) failed
                  (if (> (+ (length passed) failed) 0)
                      (number->string
                        (inexact
                          (round (* 100 (/ (length passed)
                                           (+ (length passed) failed))))))
                      "N/A"))
          (append passed unverifiable)))))

;;; ====
;;; Step 5: Emit JSONL per family + combined
;;; ====

(printf "--- Emitting JSONL ---\n")

;; Per-family output
(let ([families (map car (count-by *verified* (lambda (s) (cdr (assq 'family s)))))])
  (for-each
    (lambda (fam)
      (let* ([family-samples (filter (lambda (s) (string=? (cdr (assq 'family s)) fam))
                                     *verified*)]
             [dir (format "~a/~a" *phase1-output-root* fam)])
        (when (pair? family-samples)
          (ensure-dir! dir)
          (emit-jsonl-split family-samples dir)
          (printf "  ~a: ~a samples → ~a/\n" fam (length family-samples) dir))))
    families))

;; Combined output
(ensure-dir! *phase1-combined-dir*)
(emit-jsonl-split *verified* *phase1-combined-dir*)

;; Also emit unfiltered (includes verify failures, for analysis)
(emit-jsonl *deduped* (format "~a/all-unfiltered.jsonl" *phase1-combined-dir*))

(printf "  Combined: ~a samples → ~a/\n\n" (length *verified*) *phase1-combined-dir*)

;;; ====
;;; Step 6: Summary
;;; ====

(printf "========================================\n")
(printf "Phase 1 Summary\n")
(printf "========================================\n")

(let ([by-family (count-by *verified* (lambda (s) (cdr (assq 'family s))))]
      [by-difficulty (count-by *verified* (lambda (s) (cdr (assq 'difficulty s))))]
      [by-category (count-by *verified* (lambda (s) (cdr (assq 'category s))))])

  (printf "\nBy family:\n")
  (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
            (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-family))

  (printf "\nBy difficulty:\n")
  (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
            (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-difficulty))

  (printf "\nBy category:\n")
  (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
            (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-category)))

(printf "\nTotal Phase 1 samples: ~a (verified) / ~a (generated)\n"
        (length *verified*) (length *deduped*))
(printf "Output: ~a/all.jsonl\n" *phase1-combined-dir*)
(printf "\nPhase 1 complete.\n")
