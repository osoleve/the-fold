;;; core/types/type-annotation-check.ss — DEPRECATED: Backwards-compatibility stub
;;;
;;; This file has been merged into sig-check.ss.
;;; Load sig-check.ss for all signature parsing and validation functionality.
;;;
;;; This stub preserves the script interface for backwards compatibility.
;;; Usage:
;;;   scheme --script core/types/type-annotation-check.ss [file ...]
;;;   scheme --script core/types/type-annotation-check.ss          # checks all core files

(load "core/types/sig-check.ss")

;;; All type annotation checking functions are now defined in sig-check.ss:
;;;   - check-file, check-type-kind
;;;   - type-name-mapping, normalize-type-name
;;;   - sig-type->kind-type, collect-type-vars
;;;   - reset-counters!, record-parse-error!, record-kind-error!, record-valid!
;;;   - report-results, sig-check-main

;;; Script entry point (preserved for backwards compatibility)
(let ([args (cdr (command-line))])
     (exit (sig-check-main args)))

