(benchmark-results
  (model "/models/qwen3-0.6b-rlm-sft")
  (timestamp "2026-02-08T18:07:46Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 9285)
       (correct . #t) (output . "403")
       (trajectory
         .
         "006b031826f56e7013f98bbf1698f11ac7e3b3b1f204539cbf135e5372e4b79af6"))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . error)
        (time-ms . 13769) (correct . #f)
        (output . "curl: (52) Empty reply from server\n")
        (trajectory
          .
          "005467ab2b087a2e0560e368683b852fb6b74b6c50fac084f4778f70c468d852c8"))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (status . error) (time-ms . 3330) (f1 . 0.0)
       (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
       (output
         .
         "curl: (7) Failed to connect to localhost port 8000 after 0 ms: Couldn't connect to server\n")
       (trajectory
         .
         "0062fafbb080f7f0938f727d0ad59cd62022e59f8c28d2fb3418051f2da9b8d745")))))
