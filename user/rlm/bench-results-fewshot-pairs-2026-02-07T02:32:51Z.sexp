(benchmark-results (model "Qwen/Qwen3-32B-FP8") (mode "few-shot-pairs")
  (n-fewshot 11) (timestamp "2026-02-07T02:32:51Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 2222427) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "001b8608cea55fd2ce58682d868dfb015d83271a5b0c7d4d032773fcb688d3c04f"))))
