;;; core/types/gadt-infer.ss — DEPRECATED: Backwards-compatibility stub
;;;
;;; This file has been merged into gadt.ss.
;;; Load gadt.ss for all GADT functionality including inference.
;;;
;;; This stub exists only for backwards compatibility with code that
;;; explicitly loads gadt-infer.ss.

(load "core/types/gadt.ss")

;;; All GADT inference functions are now defined in gadt.ss:
;;;   - gadt-infer-synth
;;;   - gadt-infer-synth-case
;;;   - gadt-infer-check-ctors
;;;   - gadt-infer-synth-clauses
;;;   - gadt-infer-synth-clause
;;;   - reset-gadt-registry!
;;;   - register-gadt!
;;;   - lookup-gadt
;;;   - gadt-define-test-gadts!
