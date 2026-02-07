(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-pairs") (n-fewshot 11)
  (timestamp "2026-02-07T00:15:58Z")
  (pairs
    ((label . "OOLONG-Pairs 15 users (few-shot)") (n-users . 15) (entries-per-user . 3)
      (haystack-chars . 30281) (expected-pairs . 16)
      (status . error) (time-ms . 3155) (f1 . 0.0)
      (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
      (output
        .
        "curl: (7) Failed to connect to localhost port 8000 after 0 ms: Couldn't connect to server\n")
      (trajectory
        .
        "00adc74851bf66c3665c0d68fe3f1dc676a07e0c25d3137fd11aaef5c2df35db4c"))))
