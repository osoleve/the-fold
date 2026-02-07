(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 11)
  (timestamp "2026-02-06T23:56:57Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . error) (time-ms . 200009) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output
        .
        "EngineCore encountered an issue. See stack trace (above) for the root cause.")
      (trajectory
        .
        "0054affe2cf4da660d8b1cbaa439d090f3b28fa0b303aeeb3940065005ad362f70"))))
