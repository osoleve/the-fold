(skill dataset
  (version "0.1.0")
  (path "lattice/dataset")
  (purity partial)
  (stability experimental)
  (fuel-bound "O(n) where n = sample count")
  (deps (optics random))
  (description "Generic dataset SDK for visual reasoning problems. Provides reusable framework for presentation, sampling, distractor generation, and export.")
  (keywords (dataset ml benchmark visual-reasoning sampling export))
  (aliases (sample-gen))
  (concepts
    (concept dataset-engineering
      (description "Structured dataset construction: sample schemas, parameter sampling, distractor generation, and JSONL export.")
      (parent machine-learning)
      (synonyms dataset sample-gen)))
  (exports
    (sample make-sample sample? sample-id sample-frames sample-question sample-options sample-answer sample-metadata sample-lens-question sample-lens-answer)
    (parameter make-param-range param-range? param-sample param-interpolate make-param-set param-set-sample)
    (presentation make-layout layout-vstack layout-hstack layout-beside layout-above textbox frame-block render-layout)
    (distractor make-distractor-strategy distractor-nearby distractor-sign-flip distractor-off-by-one generate-distractors)
    (difficulty difficulty-score difficulty-tier)
    (sexp
      sample->canonical-sexp canonical-sexp->sample
      dataset->sexp sexp->dataset
      sexp->string sexp->pretty-string
      validate-sample-sexp)
    (jsonl
      make-json-object json-object? json-object-pairs
      make-json-array json-array? json-array-items
      sample->json json->sample
      sample-json-iso
      json->string
      samples->jsonl sample->jsonl-line
      sample->prompt-json samples->prompt-jsonl
      sample->openai-json samples->openai-jsonl))
  (modules
    (sample "sample.ss" "Sample record type with Q/A lenses into state")
    (parameter "parameter.ss" "Parameter ranges, sampling, and interpolation")
    (presentation "presentation.ss" "Layout combinators for frames + context")
    (distractor "distractor.ss" "Plausible wrong answer generation")
    (difficulty "difficulty.ss" "Difficulty scoring and calibration")
    (sexp "format/sexp.ss" "Canonical S-expression format")
    (jsonl "format/jsonl.ss" "JSONL export via bidirectional optics")))
