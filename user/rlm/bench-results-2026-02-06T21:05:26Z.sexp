(benchmark-results
  (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (timestamp "2026-02-06T21:05:26Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7)
       (v1 (status . exhausted) (time-ms . 89966) (correct . #f)
           (output . "Max steps reached")
           (trajectory
             .
             "0080e02b43cb1c55b43bd02d76717a8d93a369ec2330d4874c973eae009bda2956"))
       (v2 (status . completed) (time-ms . 58142) (correct . #f)
           (output
             .
             "(error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized error:parenthesized)")
           (trajectory
             .
             "00d554af2c568fcd3ac0c679b1d64fd22c5976e02c6d483437b1df60a7f0d49ca9")))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17)
        (v1 (status . completed) (time-ms . 23939) (correct . #f)
            (output
              .
              "(error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected error:unexpected)")
            (trajectory
              .
              "004a1c973313c984ae2cf59a1499ed3d8da80191702b4e2ed64001b69ad376cc0a"))
        (v2 (status . exhausted) (time-ms . 82868) (correct . #f)
            (output . "Resources exhausted")
            (trajectory
              .
              "00a9b58fc86a6799f9e64892aae25fee19ecb468c4730618caf68d0b45bc4acf68")))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (v1 (status . exhausted) (time-ms . 119631) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Max steps reached")
           (trajectory
             .
             "00796a273e773f54ac6391773f7881f72dc5776033d738105377c6072c64d2492d"))
       (v2 (status . exhausted) (time-ms . 209228) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Resources exhausted")
           (trajectory
             .
             "00fe50d76ef70a131fa4af4348c2bdb7c4eef2db0a38dd612f72503a61b4934fc8"))))))
