;;; skills/info.sexp — Skill curation file

(skill info
  (description
    "Information theory library implementing Shannon entropy, divergence measures,\n    channel capacity computation, rate-distortion theory, and coding algorithms.\n    Covers entropy variants (Renyi, min, collision), statistical divergences\n    (KL, JS, Hellinger, chi-squared), channel models (BSC, BEC, AWGN), and\n    source/channel coding (Huffman, arithmetic, LZ78, Hamming).")

  (keywords (information-theory entropy divergence channel-capacity rate-distortion huffman-coding arithmetic-coding compression error-correction shannon))

  (aliases (information entropy coding))

  (concepts (information-theory entropy-measures channel-theory coding-theory rate-distortion))

  (modules
    entropy
    statistical-measures
    channel-capacity
    rate-distortion
    coding
    epiplexity
    model-selection-info
    empirical-info
    partition-info
  )

  ;; Curated Q&A pairs for LLM context
  (prompts)

  ;; KG/search queries an agent should try
  (suggested-queries)
)
