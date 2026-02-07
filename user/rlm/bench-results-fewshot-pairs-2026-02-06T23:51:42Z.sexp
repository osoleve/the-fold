(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 11)
  (timestamp "2026-02-06T23:51:42Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 60878) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "0056c12be271ca47605fec10e23e6900d035b427501baae64d66c43b0d19b18885"))))
