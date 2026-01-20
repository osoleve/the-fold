(load "core/base/prelude.ss")
(load "core/util/help.ss")

(doc 'module 'quick-test)
(doc 'description "Quick scratch file for testing string primitive availability.")
(doc 'layer 'core)
(display (length (get-primitives-by-category 'string)))
(newline)
