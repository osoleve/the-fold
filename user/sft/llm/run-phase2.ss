;;; user/sft/llm/run-phase2.ss — Phase 2 pipeline runner
;;;
;;; Orchestrates all Phase 2 SFT generators:
;;;   Stage 1 (mechanical): meta-template, meta-protocol, meta-refactor, meta-lattice
;;;   Stage 2 (LLM-based):  prompt rewriting, alternative implementations
;;;
;;; Each stage generates samples, optionally verifies them, and emits JSONL.
;;;
;;; Usage:
;;;   scheme --script user/sft/llm/run-phase2.ss          # Full pipeline
;;;   scheme --script user/sft/llm/run-phase2.ss meta-only # Stage 1 only
;;;   scheme --script user/sft/llm/run-phase2.ss llm-only  # Stage 2 only

;;; Bootstrap
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))

;;; ====
;;; Load infrastructure
;;; ====

(printf "=== Phase 2 Pipeline Runner ===\n\n")
(printf "Loading infrastructure...\n")

(load "user/sft/extract/walker.ss")
(load "user/sft/extract/emit.ss")
(load "user/sft/extract/verify.ss")

;;; ====
;;; Configuration
;;; ====

(define *phase2-output-root* "data/phase2")
(define *phase1-input* "data/phase1-combined/all.jsonl")
(define *rewrite-sample-count* 3000)
(define *alt-sample-count* 2000)
(define *verify-meta-samples* #t)   ; verify meta-tooling seeds?

;;; ====
;;; Argument parsing
;;; ====

(define *run-mode*
  (let ([args (command-line-arguments)])
    (cond
      [(and (pair? args) (string=? (car args) "meta-only")) 'meta-only]
      [(and (pair? args) (string=? (car args) "llm-only")) 'llm-only]
      [else 'full])))

(printf "Mode: ~a\n\n" *run-mode*)

;;; ====
;;; Helper: ensure output directory
;;; ====

(define (ensure-dir! path)
  (guard (ex [else #f])
    (system (format "mkdir -p ~a" path))))

;;; ====
;;; Stage 1: Mechanical meta-tooling seeds
;;; ====

(define (run-stage1!)
  (printf "========================================\n")
  (printf "Stage 1: Mechanical meta-tooling seeds\n")
  (printf "========================================\n\n")

  ;; Walk IR for template seeds
  (printf "--- Walking lattice for function IR ---\n")
  (define all-ir (walk-all-manifests))
  (printf "Walker: ~a function IR records\n\n" (length all-ir))

  ;; Generate meta-template seeds
  (printf "--- Meta-Template DSL ---\n")
  (load "user/sft/llm/meta-template.ss")
  (define mt-seeds (meta-template/generate-seeds all-ir))
  (printf "  Seeds: ~a\n\n" (length mt-seeds))

  ;; Generate meta-protocol seeds
  (printf "--- Meta-Protocol ---\n")
  (load "user/sft/llm/meta-protocol.ss")
  (define mp-seeds (meta-protocol/generate-seeds))
  (printf "  Seeds: ~a\n\n" (length mp-seeds))

  ;; Generate meta-refactor seeds
  (printf "--- Meta-Refactor ---\n")
  (load "user/sft/llm/meta-refactor.ss")
  (define mr-seeds (meta-refactor/generate-seeds))
  (printf "  Seeds: ~a\n\n" (length mr-seeds))

  ;; Generate meta-lattice seeds
  (printf "--- Meta-Lattice ---\n")
  (load "user/sft/llm/meta-lattice.ss")
  (define ml-seeds (meta-lattice/generate-seeds))
  (printf "  Seeds: ~a\n\n" (length ml-seeds))

  ;; Combine all meta seeds
  (define all-meta
    (append mt-seeds mp-seeds mr-seeds ml-seeds))
  (printf "Total meta-tooling seeds: ~a\n" (length all-meta))

  ;; Verify if configured
  (define verified-meta
    (if *verify-meta-samples*
        (begin
          (printf "\n--- Verifying meta-tooling seeds ---\n")
          (let* ([with-verify
                  (filter (lambda (s)
                            (let ([ve (cdr (assq 'verify_expr s))])
                              (and (string? ve)
                                   (> (string-length ve) 5))))
                          all-meta)]
                 [without-verify
                  (filter (lambda (s)
                            (let ([ve (cdr (assq 'verify_expr s))])
                              (or (not (string? ve))
                                  (<= (string-length ve) 5))))
                          all-meta)])
            (printf "  ~a with verify_expr, ~a without\n"
                    (length with-verify) (length without-verify))
            ;; For meta-tooling, many verify expressions need lattice/toolkit
            ;; loaded — skip subprocess verification for now, emit all
            (printf "  Skipping subprocess verification (meta-tooling needs special environments)\n")
            (printf "  All ~a seeds accepted\n" (length all-meta))
            all-meta))
        all-meta))

  ;; Emit JSONL per family
  (for-each
    (lambda (family-name)
      (let* ([family-samples (filter (lambda (s)
                                       (string=? (cdr (assq 'family s)) family-name))
                                     verified-meta)]
             [dir (format "~a/~a" *phase2-output-root* family-name)])
        (when (pair? family-samples)
          (ensure-dir! dir)
          (emit-jsonl-split family-samples dir))))
    '("meta_template" "meta_protocol" "meta_refactor" "meta_lattice"))

  ;; Return all meta seeds for combined output
  verified-meta)

;;; ====
;;; Stage 2: LLM-based generation
;;; ====

(define (run-stage2!)
  (printf "\n========================================\n")
  (printf "Stage 2: LLM-based generation\n")
  (printf "========================================\n\n")

  ;; Load LLM infrastructure
  (load "user/sft/llm/llm-gen.ss")

  ;; Discover providers
  (printf "--- Discovering LLM providers ---\n")
  (define providers (llm-gen/discover-providers))

  (when (null? providers)
    (printf "ERROR: No LLM providers available.\n")
    (printf "Ensure vLLM is running on elsie-1:8000 and/or elsie-2:8000\n")
    (printf "Skipping Stage 2.\n")
    (set! providers '()))

  (define rewritten-samples '())
  (define alt-samples '())

  (when (pair? providers)
    ;; --- Prompt Rewriting ---
    (printf "\n--- Prompt Rewriting ---\n")
    (load "user/sft/llm/rewrite.ss")
    (let* ([phase1 (rewrite/load-phase1 *phase1-input*)]
           [candidates (rewrite/stratified-sample phase1
                         *rewrite-sample-count* 42)]
           [resume-file (format "~a/rewrite-resume.txt" *phase2-output-root*)])
      (ensure-dir! *phase2-output-root*)
      (set! rewritten-samples
        (rewrite/run candidates providers resume-file))
      (printf "  Rewritten: ~a samples\n" (length rewritten-samples))

      (when (pair? rewritten-samples)
        (let ([dir (format "~a/rewrite" *phase2-output-root*)])
          (ensure-dir! dir)
          (emit-jsonl-split rewritten-samples dir))))

    ;; --- Alternative Implementations ---
    (printf "\n--- Alternative Implementations ---\n")
    (load "user/sft/llm/alternatives.ss")
    (let* ([candidates (alt/select-candidates *phase1-input*
                         *alt-sample-count*)]
           [resume-file (format "~a/alternatives-resume.txt" *phase2-output-root*)])
      (set! alt-samples
        (alt/run candidates providers resume-file))
      (printf "  Alternatives: ~a samples\n" (length alt-samples))

      (when (pair? alt-samples)
        ;; Prepare JSONL-loaded samples for verification
        ;; (parse string fields to sexp fields needed by verify-batch-sessions)
        (printf "\n--- Verifying alternative implementations ---\n")
        (let* ([prepared (verify/prepare-jsonl-samples alt-samples)]
               [_ (printf "  Prepared ~a/~a for verification\n"
                          (length prepared) (length alt-samples))]
               [result (verify-batch-sessions prepared)]
               [passed (car result)]
               [failed (cdr result)])
          (printf "  Verified: ~a passed, ~a failed\n"
                  (length passed) failed)
          (set! alt-samples passed))

        (let ([dir (format "~a/alternatives" *phase2-output-root*)])
          (ensure-dir! dir)
          (emit-jsonl-split alt-samples dir)))))

  ;; Return both
  (append rewritten-samples alt-samples))

;;; ====
;;; Combined output + summary
;;; ====

(define (run-pipeline!)
  (let* ([meta-samples
          (if (eq? *run-mode* 'llm-only) '() (run-stage1!))]
         [llm-samples
          (if (eq? *run-mode* 'meta-only) '() (run-stage2!))]
         [all-phase2 (append meta-samples llm-samples)])

    ;; Deduplicate
    (printf "\n========================================\n")
    (printf "Deduplication\n")
    (printf "========================================\n")
    (define deduped (emit/deduplicate all-phase2))
    (printf "After dedup: ~a samples\n" (length deduped))

    ;; Combined output
    (when (pair? deduped)
      (let ([dir (format "~a/combined" *phase2-output-root*)])
        (ensure-dir! dir)
        (emit-jsonl-split deduped dir)))

    ;; Summary
    (printf "\n========================================\n")
    (printf "Phase 2 Summary\n")
    (printf "========================================\n")
    (let ([by-family
           (let loop ([remaining deduped] [counts '()])
             (if (null? remaining)
                 counts
                 (let* ([s (car remaining)]
                        [fam (cdr (assq 'family s))]
                        [entry (assoc fam counts)])
                   (if entry
                       (begin (set-cdr! entry (+ (cdr entry) 1))
                              (loop (cdr remaining) counts))
                       (loop (cdr remaining)
                             (cons (cons fam 1) counts))))))]
          [by-difficulty
           (let loop ([remaining deduped] [counts '()])
             (if (null? remaining)
                 counts
                 (let* ([s (car remaining)]
                        [diff (cdr (assq 'difficulty s))]
                        [entry (assoc diff counts)])
                   (if entry
                       (begin (set-cdr! entry (+ (cdr entry) 1))
                              (loop (cdr remaining) counts))
                       (loop (cdr remaining)
                             (cons (cons diff 1) counts))))))]
          [by-split
           (let loop ([remaining deduped] [counts '()])
             (if (null? remaining)
                 counts
                 (let* ([s (car remaining)]
                        [split (cdr (assq 'split s))]
                        [entry (assoc split counts)])
                   (if entry
                       (begin (set-cdr! entry (+ (cdr entry) 1))
                              (loop (cdr remaining) counts))
                       (loop (cdr remaining)
                             (cons (cons split 1) counts))))))])

      (printf "\nBy family:\n")
      (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
                (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-family))

      (printf "\nBy difficulty:\n")
      (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
                by-difficulty)

      (printf "\nBy split:\n")
      (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
                by-split))

    (printf "\nTotal Phase 2 samples: ~a\n" (length deduped))
    (printf "Output: ~a/combined/all.jsonl\n" *phase2-output-root*)
    (printf "\nPhase 2 complete.\n")
    deduped))

;;; ====
;;; Run
;;; ====

(run-pipeline!)
