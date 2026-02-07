(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 10)
  (timestamp "2026-02-06T23:42:57Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 37123) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "00e23240d19df3c833d373a5cb5962e8905132732dafe691e2e1c9d94bff5f6e56"))))
