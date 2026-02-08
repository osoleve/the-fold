(benchmark-results
  (model "/models/qwen3-0.6b-rlm-sft")
  (timestamp "2026-02-08T18:11:09Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 9049)
       (correct . #t) (output . "403")
       (trajectory
         .
         "00b7c9ffb455ba1ea9de1e54de05da014bc87f8ae5cbcd9cfc33f0b843fcfac3c7"))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . exhausted)
        (time-ms . 20213) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "00d4d1b3c8180eb73cb089b9cf6fd732d096d8e0d7e5cf870f2b7daf969a6f678b"))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (status . completed) (time-ms . 27728) (f1 . 0.0)
       (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
       (output . "(17 17)")
       (trajectory
         .
         "007c8249f19be55281ac8ed53b5440da5541281c633ef82130218bcac823bd17f8")))))
