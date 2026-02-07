(benchmark-results
  (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (timestamp "2026-02-06T21:06:33Z")
  (oolong
    (((label . "OOLONG 100 entries / 50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7)
       (v1 (status . completed) (time-ms . 17638) (correct . #f)
           (output . "0")
           (trajectory
             .
             "002735bc2be369f73f265aff855689526feb5d0dbfb9a9e3c168cd130579b961d1"))
       (v2 (status . exhausted) (time-ms . 60439) (correct . #f)
           (output . "Resources exhausted")
           (trajectory
             .
             "005b3e1c9bb6a6783752a434b5a8aceff531de8380cc72e9da9709ee26b29f5a3c")))
      ((label . "OOLONG 200 entries / 100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17)
        (v1 (status . exhausted) (time-ms . 39348) (correct . #f)
            (output . "Max steps reached")
            (trajectory
              .
              "00c0761237df987defa86c309fdf70c3da0acfb5a33ffa2d3c63c6cda40aad6c06"))
        (v2 (status . completed) (time-ms . 24094) (correct . #f)
            (output
              .
              "(0 0 0 0 0 0 0 81 137 0 0 0 0 0 52 0 70 0 0 0 0 0 0 0 0 0 13 0 0 50 0 0 42 0 0 119 0 100 12 0 30 0 0 0 96 0 0 93 0 0 83 0 0 0 0 0 92 0 0 0)")
            (trajectory
              .
              "008d8f37123abe66eae408f69f5edd017b6e0cb444a69e18536597537b742c2154")))))
  (pairs
    (((label . "OOLONG-Pairs 15 users") (n-users . 15) (entries-per-user . 3)
       (haystack-chars . 30281) (expected-pairs . 16)
       (v1 (status . exhausted) (time-ms . 174127) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Max steps reached")
           (trajectory
             .
             "00e8098c5d5418d8381021c0319e9a6f7cc816b2da27c75346e9df8f05bbc175c4"))
       (v2 (status . exhausted) (time-ms . 194935) (f1 . 0.0)
           (precision . 0.0) (recall . 0.0) (tp . 0) (predicted . 0)
           (output . "Resources exhausted")
           (trajectory
             .
             "008f6dad05f12d2e66f2bbef5d623b93d755c6e0179d8f9c865c1e83b2665aea17"))))))
