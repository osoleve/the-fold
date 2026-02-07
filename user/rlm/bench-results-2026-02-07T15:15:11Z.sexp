(benchmark-results
  (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (timestamp "2026-02-07T15:15:11Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 50208)
       (correct . #t) (output . "403")
       (trajectory
         .
         "00f0017d35fd0d3ebe41b60b400dd0d93e6bb34ab651dabc04a6ff25d12102ca8d"))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 14829) (correct . #t) (output . "1070")
        (trajectory
          .
          "00a46e3c4d289bbb12237e56b71f0dcfe6567d401f10cb9b182b686430b76869f0"))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (status . exhausted) (time-ms . 221884) (f1 . 0.0)
       (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
       (output . "Resources exhausted")
       (trajectory
         .
         "005e0a178f6556bb05d114d5837f0a26817b96acb7da142526b4a4961890b04568")))))
