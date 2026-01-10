;;; core/types/rank-n-infer.ss — DEPRECATED: Backwards-compatibility stub
;;;
;;; This file has been merged into rank-n.ss.
;;; Load rank-n.ss for all rank-N type functionality including inference.
;;;
;;; This stub exists only for backwards compatibility with code that
;;; explicitly loads rank-n-infer.ss.

(load "core/types/rank-n.ss")

;;; All rank-N inference functions are now defined in rank-n.ss:
;;;   - forall-well-formed?
;;;   - forall-vars, forall-var-kinds, forall-body
;;;   - rank-n-instantiate
;;;   - rank-n-generalize
;;;   - rank-n-subsumes?, rank-n-subsumes-result
;;;   - fresh-rank-n-var, reset-rank-n-fresh!
;;;   - peek-argument-structure, should-delay-instantiation?
;;;   - rank-n-skolem?, type-contains-skolem?
;;;   - impredicative-unify-with
;;;   - rank-n-infer-synth, rank-n-check
;;;   - rank-n-typeof, rank-n-typecheck

