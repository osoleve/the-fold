(doc 'module 'string)
(doc 'description "String utilities aggregator. Load this file to get all string utilities.")
(doc 'layer 'core)

(doc 'submodule 'string-core "Core operations: split, join, trim, predicates")
(doc 'submodule 'string-search "Search operations: contains, index-of, replace, edit-distance")
(doc 'submodule 'string-format "Formatting operations: case conversion, padding")

;; Exports are auto-derived from submodule (doc 'export #t) forms

(load "core/base/string/string-core.ss")
(load "core/base/string/string-search.ss")
(load "core/base/string/string-format.ss")
