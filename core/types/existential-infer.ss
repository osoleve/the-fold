;;; core/types/existential-infer.ss — DEPRECATED: Backwards-compatibility stub
;;;
;;; This file has been merged into existential.ss.
;;; Load existential.ss for all existential type functionality including inference.
;;;
;;; This stub exists only for backwards compatibility with code that
;;; explicitly loads existential-infer.ss.

(load "core/types/existential.ss")

;;; All existential inference functions are now defined in existential.ss:
;;;   - existential-infer-synth
;;;   - existential-infer-synth-pack
;;;   - existential-infer-synth-unpack
;;;   - subst-types
;;;   - check-all-types
;;;   - extend-ctx-with-skolems
