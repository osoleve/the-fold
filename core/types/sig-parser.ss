;;; core/types/sig-parser.ss — DEPRECATED: Backwards-compatibility stub
;;;
;;; This file has been merged into sig-check.ss.
;;; Load sig-check.ss for all signature parsing and validation functionality.
;;;
;;; This stub exists only for backwards compatibility with code that
;;; explicitly loads sig-parser.ss.

(load "core/types/sig-check.ss")

;;; All signature parsing functions are now defined in sig-check.ss:
;;;   - tokenize-sig
;;;   - parse-type, parse-sig-line
;;;   - type->internal, format-type
;;;   - base-type-names, type-constructor-names, greek-type-vars
;;;   - type-var?

