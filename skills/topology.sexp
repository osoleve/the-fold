;;; skills/topology.sexp — Skill curation file

(skill topology
  (description
    "Computational topology and topological data analysis.\nIncludes simplicial complexes, homology over Z₂, persistent homology,\nVietoris-Rips filtrations, persistence diagrams, and barcodes.")

  (keywords (topology simplicial-complex homology betti-numbers boundary euler-characteristic star link skeleton filtration tda z2-coefficients persistent-homology persistence-diagram barcode vietoris-rips bottleneck-distance))

  (aliases (topo simplicial persistent tda))

  (concepts (computational-topology simplicial-homology persistent-homology))

  (modules
    simplicial-complex
    homology
    persistent
    topo-landscape
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
