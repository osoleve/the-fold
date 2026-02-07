(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot") (n-fewshot 5) (fewshot-chars 2415)
  (timestamp "2026-02-07T00:27:46Z")
  (oolong
    (((label . "OOLONG 100 few-shot") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 42755)
       (correct . #t) (output . "403")
       (trajectory
         .
         "0026da2db15d9de3e6907f52cd4e1616c2bd8803b537006b1ee76e409e3c23cdff"))
      ((label . "OOLONG 200 few-shot") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 14186) (correct . #t) (output . "1070")
        (trajectory
          .
          "00b2767e2d838792f80688b0378bc6fb9290589e73a39a2a01134be7f96f37f59b")))))
