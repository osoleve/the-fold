;;; FOLD-2025-015: Documentation Philosophy
;;; Settled: 2025-12-29

((id . "FOLD-2025-015")
 (date . "2025-12-29T19:30:00Z")
 (settled-by . outsider)
 (category . taste)
 (context . "What is the relationship between code and documentation?
Should code be self-documenting, have inline docs, or rely on external docs?")
 (decision . "Docs live with code, and docgen supports an external wiki.
Docstrings and inline comments are primary; docgen extracts to browsable
external documentation.")
 (rationale . "Docs near code stay accurate. External wikis provide
narrative and tutorial structure. docgen bridges the two: extract from
source, render to wiki.")
 (implications . (";;; comments with signatures are canonical doc format"
                  "docgen extracts to structured data"
                  "Wiki/external docs can be generated from docgen output"
                  "Tutorials and guides live in scripture/ or wiki")))
