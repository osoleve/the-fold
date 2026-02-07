(benchmark-results
  (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (timestamp "2026-02-06T22:22:40Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . exhausted) (time-ms . 133772)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00f2373cc91e5f7507a33931f58a6c72ed4a5267d53c958b119080fbf4de4907bf"))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . exhausted)
        (time-ms . 62938) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "008ecd42523f88df326113be72d9ca365f0ce0d7e1217d58f4c1fd4b2359d4628c"))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (status . exhausted) (time-ms . 275529) (f1 . 0.0)
       (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
       (output . "Resources exhausted")
       (trajectory
         .
         "00d780bbc68ac46c66339a22cc7aa0c41b341ead43bbd8d0aeceb61d364a68cdcc")))))
