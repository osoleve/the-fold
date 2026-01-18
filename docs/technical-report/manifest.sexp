;;; Technical Report Assembly Manifest
;;;
;;; Lists chapter files in order for assembly into docs/technical-report.md
;;; Edit this file to change chapter order or add new chapters.

(technical-report
  (title "The Fold: A Content-Addressable Homoiconic Universe")
  (output "../technical-report.md")

  (chapters
    "00-frontmatter.md"
    "00-abstract.md"
    "01-introduction.md"
    "02-system-architecture.md"
    "03-the-block-machine.md"
    "04-the-block-calculus.md"
    "05-the-type-theory.md"
    "06-the-module-system.md"
    "07-implementation.md"
    "08-developer-and-meta-tooling.md"
    "09-agent-substrate.md"
    "10-evaluation.md"
    "11-related-work.md"
    "12-limitations-and-non-goals.md"
    "13-future-work.md"
    "14-conclusion.md"
    "appendix-a-block-calculus-formal-syntax.md"
    "appendix-b-type-grammar.md"
    "appendix-c-kind-grammar.md"
    "appendix-d-manifest-schema.md"
    "appendix-e-comparison-with-unison.md"
    "99-references.md"))
