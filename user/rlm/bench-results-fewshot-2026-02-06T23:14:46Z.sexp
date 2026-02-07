(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot") (n-fewshot 5) (fewshot-chars 2415)
  (timestamp "2026-02-06T23:14:46Z")
  (oolong
    (((label . "OOLONG 100 few-shot") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 9970)
       (correct . #t) (output . "403")
       (trajectory
         .
         "0004df736dfb153afa167d2e315e066ee9bd624612f4e7c96b70f4b28a1144d189"))
      ((label . "OOLONG 200 few-shot") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 16449) (correct . #t) (output . "1070")
        (trajectory
          .
          "0027829dc96b0ee7c764286d751229f010991a142955df10d21265dfa5f0336182")))))
