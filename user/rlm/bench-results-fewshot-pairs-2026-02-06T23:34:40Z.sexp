(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 5)
  (timestamp "2026-02-06T23:34:40Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 220123) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "00d1db49b53b0a1917c327d910ba21e7a5977de296b42065534c59c46683c55918"))))
