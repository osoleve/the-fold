;;; lattice/dsl/template/manifest.sexp — Template DSL Skill Manifest

(skill template
  (version "0.1.0")
  (tier 1)
  (path "lattice/dsl/template")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n) for hole detection, O(n) for substitution")
  (deps ())

  (description
   "Grammar-driven code construction DSL for building S-expressions
    via EBNF-like production statements. Eliminates parenthesis tracking
    by using named holes ($name) that get filled incrementally.")

  (keywords (template dsl code-generation holes grammar
             parenthesis-free structural-editing ai-tooling))
  (aliases (grammar-dsl code-builder sexpr-template))

  (exports
   (template hole? hole-name find-holes
             new-template template? template-expr template-holes
             template-complete? template-hole-count
             fill-hole fill-holes
             compile-template try-compile-template
             template->string template-status holes->string
             wrap-if-multiple))

  (modules
   (template "template.ss" "Core template types, holes, filling, compilation")))
