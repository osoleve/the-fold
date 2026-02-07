(benchmark-results
  (model "Qwen/Qwen3-32B-FP8")
  (mode "oolong-stats")
  (timestamp "2026-02-07T11:54:22Z")
  (stats
    (((label . "Stats 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954)
       (expected-mean . 57.57) (expected-sd . 24.6)
       (status . completed) (time-ms . 542369) (mean-correct . #f)
       (sd-correct . #f)
       (output
         .
         "(let* ((data '(81 51 86 52 70 13 50)) (mean (mean data)) (std-dev (std-dev data))) (format #f \"mean=~,.2f, std-dev=~,.2f\" mean std-dev))")
       (trajectory
         .
         "00858b65ca5060f125574b7588692dd2415e6d6dd7caf8427f8333567c175b9218"))
      ((label . "Stats 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected-mean . 62.94) (expected-sd . 28.34)
        (status . exhausted) (time-ms . 521447) (mean-correct . #f)
        (sd-correct . #f) (output . "Resources exhausted")
        (trajectory
          .
          "00c3622b4a6290f280b82636a18366afe506076dbd913062e9a727ffb2691621e9")))))
