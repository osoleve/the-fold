(benchmark-results
  (model "/models/qwen3-0.6b-rlm-sft")
  (timestamp "2026-02-08T18:11:36Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . exhausted) (time-ms . 27210)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00af18a3443a5ce45830cb8c7ce68fbe5619ce5312d1970294ce26b1f498fc0b8f"))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 35154) (correct . #t) (output . "1070")
        (trajectory
          .
          "00f4e34573c957ec886e39772895e9d343f3b391e617e5435b4ece21eda4de8ebc"))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (status . completed) (time-ms . 7058) (f1 . 0.0)
       (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
       (output . "(7 12)")
       (trajectory
         .
         "0004734a3b40131797c0771b294f76ad537945bed86917297401013e13dfb2e3f9")))))
