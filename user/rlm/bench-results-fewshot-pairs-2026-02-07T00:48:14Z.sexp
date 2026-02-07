(benchmark-results (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (mode "few-shot-pairs") (n-fewshot 11)
  (timestamp "2026-02-07T00:48:14Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . exhausted) (time-ms . 482250) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output . "Resources exhausted")
      (trajectory
        .
        "000e12aa8aacf72fdc237b0179fdae2a49da489e708db0d9c4e37ad2405b909328"))))
