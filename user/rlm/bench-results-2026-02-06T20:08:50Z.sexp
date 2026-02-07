(benchmark-results
  (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (timestamp "2026-02-06T20:08:50Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7)
       (v1 (status . completed) (time-ms . 12181) (correct . #t)
           (output . "403")
           (trajectory
             .
             "00022c1157e0ab1929bfd2ef28844558dd8052a7f74c581861bb25aa4077f136a3"))
       (v2 (status . completed) (time-ms . 13837) (correct . #t)
           (output . "403")
           (trajectory
             .
             "0081da091a0a9a1bd7ecfb9061d95d54eabab2dcdbe53a74e561e7fd6154fe43c3")))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17)
        (v1 (status . completed) (time-ms . 15487) (correct . #t)
            (output . "1070")
            (trajectory
              .
              "007546a3c553b8d8dd48d2cf592fe2c4225097d21012351876b155c8815466da5b"))
        (v2 (status . completed) (time-ms . 16780) (correct . #t)
            (output . "1070")
            (trajectory
              .
              "0045f4c41e6b78dbbcc531b55ea952c88c69dd91d06bc2b640a6acb27baf4cb8bb")))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (v1 (status . exhausted) (time-ms . 476036) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Max steps reached")
           (trajectory
             .
             "00d53b1e4e5359d9803fff1cf11ac7cdb0c992f2ec828ed9fb582d9f531f4dab23"))
       (v2 (status . exhausted) (time-ms . 190604) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Resources exhausted")
           (trajectory
             .
             "00eba5ded1fe4656e5de47c727c2d314e60ab4d01488cb380e8c8269256677a6ab"))))))
