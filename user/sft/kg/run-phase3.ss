;;; user/sft/kg/run-phase3.ss — Phase 3 pipeline runner
;;;
;;; Orchestrates KG-guided generation:
;;;   1. Coverage analysis on existing data
;;;   2. Multi-hop sample generation (bridges, chains, SkillMix)
;;;   3. Session-based verification
;;;   4. JSONL emission
;;;
;;; Usage:
;;;   ./fold --timeout 600 -s phase3 '(load "user/sft/kg/run-phase3.ss")'

;;; Bootstrap
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))

;;; ====
;;; Load infrastructure
;;; ====

(printf "=== Phase 3 Pipeline Runner ===\n\n")
(printf "Loading infrastructure...\n")

(load "user/sft/extract/walker.ss")
(load "user/sft/extract/emit.ss")
(load "user/sft/extract/verify.ss")
(load "user/sft/kg/coverage.ss")
(load "user/sft/kg/multi-hop.ss")

;;; ====
;;; Configuration
;;; ====

(define *phase3-output-root* "data/phase3")
(define *phase1-input* "data/phase1-combined/all.jsonl")

;;; ====
;;; Helper
;;; ====

(define (ensure-dir! path)
  (guard (ex [else #f])
    (system (format "mkdir -p ~a" path))))

;;; ====
;;; Pipeline
;;; ====

(define (run-phase3!)
  ;; Step 1: Walk lattice for function IR
  (printf "\n--- Walking lattice for function IR ---\n")
  (let* ([all-ir (walk-all-manifests)]
         [_ (printf "Walker: ~a function IR records\n" (length all-ir))]

         ;; Step 2: Coverage analysis
         [_ (printf "\n--- Coverage Analysis ---\n")]
         [coverage-report (coverage/analyze *phase1-input*)]
         [_ (coverage/print-report coverage-report)]

         ;; Step 3: Multi-hop generation
         [_ (printf "\n--- Multi-Hop Generation ---\n")]
         [kg-samples (multi-hop/generate-all all-ir coverage-report)]
         [_ (printf "Generated ~a KG-guided samples\n" (length kg-samples))]

         ;; Step 4: Verify
         [_ (printf "\n--- Verification ---\n")]
         [verifiable (filter (lambda (s)
                               (let ([ve (assq 'verify_expr s)])
                                 (and ve (string? (cdr ve))
                                      (> (string-length (cdr ve)) 5))))
                             kg-samples)]
         [unverifiable (filter (lambda (s)
                                 (let ([ve (assq 'verify_expr s)])
                                   (or (not ve) (not (string? (cdr ve)))
                                       (<= (string-length (cdr ve)) 5))))
                               kg-samples)]
         [_ (printf "  ~a verifiable, ~a unverifiable\n"
                    (length verifiable) (length unverifiable))])

    (if (null? verifiable)
        (begin
          (printf "  No verifiable samples — skipping verification\n")
          (let ([all-output kg-samples])
            (multi-hop/emit-output! all-output)
            all-output))
        (let* ([result (verify-batch-sessions verifiable)]
               [passed (car result)]
               [failed-count (cdr result)]
               [_ (printf "  Verified: ~a passed, ~a failed\n"
                          (length passed) failed-count)]

               ;; Step 5: Deduplicate
               [all-output (append passed unverifiable)]
               [_ (printf "\n--- Deduplication ---\n")]
               [deduped (emit/deduplicate all-output)]
               [_ (printf "  After dedup: ~a samples\n" (length deduped))])

          (multi-hop/emit-output! deduped)
          deduped))))

(define (multi-hop/emit-output! samples)
  ;; Step 6: Write JSONL output
  (printf "\n--- Writing JSONL ---\n")
  (ensure-dir! *phase3-output-root*)

  ;; Per-family emission
  (for-each
    (lambda (family-name)
      (let* ([family-samples (filter (lambda (s)
                                       (string=? (cdr (assq 'family s)) family-name))
                                     samples)]
             [dir (format "~a/~a" *phase3-output-root* family-name)])
        (when (pair? family-samples)
          (ensure-dir! dir)
          (emit-jsonl-split family-samples dir))))
    '("kg_bridge" "kg_chain" "kg_skillmix" "kg_coverage"))

  ;; Combined output
  (let ([dir (format "~a/combined" *phase3-output-root*)])
    (ensure-dir! dir)
    (emit-jsonl-split samples dir))

  ;; Summary
  (printf "\n=== Phase 3 Summary ===\n")
  (let* ([by-family
          (let loop ([remaining samples] [counts '()])
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
          (let loop ([remaining samples] [counts '()])
            (if (null? remaining)
                counts
                (let* ([s (car remaining)]
                       [diff (cdr (assq 'difficulty s))]
                       [entry (assoc diff counts)])
                  (if entry
                      (begin (set-cdr! entry (+ (cdr entry) 1))
                             (loop (cdr remaining) counts))
                      (loop (cdr remaining)
                            (cons (cons diff 1) counts))))))])

    (printf "\nBy family:\n")
    (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
              (list-sort (lambda (a b) (> (cdr a) (cdr b))) by-family))

    (printf "\nBy difficulty:\n")
    (for-each (lambda (p) (printf "  ~a: ~a\n" (car p) (cdr p)))
              by-difficulty))

  (printf "\nTotal Phase 3 samples: ~a\n" (length samples))
  (printf "Output: ~a/combined/all.jsonl\n" *phase3-output-root*)
  (printf "\nPhase 3 complete.\n"))

;;; ====
;;; Run
;;; ====

(run-phase3!)
