;; Auto-generated comment triage report (v2 — full-file review)
;; Generated: 2026-02-05
;; Method: n=1 filter (197 files) → n=5 targeted (5 non-CLEAN files)
;; Clean files: 192/197
;;
;; Human review of high-confidence spans:
;;   cache-migration.ss:173 — clear delete (dead registration call)
;;   tagless-chronicle.ss:464-474 — borderline (orphaned usage examples)
;;   manifest-migration.ss:163-173 — borderline (migration templates)
;;   partial-eval.ss:244,257 — false positive (FIX annotations, not dead code)

(comment-triage
  (high
    (span (file "lattice/dsl/chronicle/tagless-chronicle.ss") (lines 464 474) (votes 5 5))
    (span (file "lattice/dsl/partial-eval.ss") (lines 244 244) (votes 5 5))
    (span (file "lattice/dsl/partial-eval.ss") (lines 257 257) (votes 5 5))
    (span (file "lattice/meta/cache-migration.ss") (lines 173 173) (votes 5 5))
    (span (file "lattice/meta/manifest-migration.ss") (lines 163 173) (votes 5 5))
  )
  (medium)
  (low)
)
