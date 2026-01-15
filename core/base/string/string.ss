;;; core/base/string/string.ss — String Utilities Aggregator
;;;
;;; This module loads all string submodules, providing a single entry point.
;;; Load this file to get all string utilities.
;;;
;;; Submodules:
;;;   string-core.ss   — Core operations (split, join, trim, predicates)
;;;   string-search.ss — Search operations (contains, index-of, replace, edit-distance)
;;;   string-format.ss — Formatting operations (case conversion, padding)
;;;
;;; Exports:
;;;   From string-core:
;;;     whitespace?, string-join, string-split, string-reverse,
;;;     string-trim, string-trim-left, string-trim-right,
;;;     string-empty?, string-blank?, string-all-match?
;;;
;;;   From string-search:
;;;     string-starts-with?, string-prefix?, string-ends-with?,
;;;     string-contains?, string-index-of, string-last-index-of,
;;;     string-replace, edit-distance
;;;
;;;   From string-format:
;;;     string-upcase, string-downcase, string-pad-left, string-pad-right

(load "core/base/string/string-core.ss")
(load "core/base/string/string-search.ss")
(load "core/base/string/string-format.ss")
