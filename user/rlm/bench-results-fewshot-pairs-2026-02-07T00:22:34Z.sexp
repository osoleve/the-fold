(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 11)
  (timestamp "2026-02-07T00:22:34Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 218143) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "0091158f7af41c32230d33688c6cec734f3464dbff8d1b299119aa01ef14e49147"))))
