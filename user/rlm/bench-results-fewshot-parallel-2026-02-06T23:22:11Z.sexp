(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-parallel") (n-trials 3) (n-fewshot 5)
  (timestamp "2026-02-06T23:22:11Z")
  (summary ((oolong-100 0 3 54119) (oolong-200 1 3 73628)))
  (trials
    (((label . "trial-1/100") (trial . 1) (n-entries . 100) (haystack-chars . 54954)
       (expected . 403) (match-count . 7) (status . exhausted)
       (time-ms . 68787) (correct . #f)
       (output . "Resources exhausted")
       (trajectory
         .
         "00bb1dcf4959adf281115f7ce5c30eae2b5030697301aaa2bde2b6ce1ae94b7d13"))
      ((label . "trial-1/200") (trial . 1) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 23956) (correct . #f)
        (output
          .
          "(0 error:timeout 0 0 error:fold-ipc-eval 0 137 0 0 81 0 0 0 0 52 error:timeout 52 52 error:timeout 0 0 70 70 error:fold-ipc-eval 0 error:fold-ipc-eval 13 0 0 50 13 error:timeout 0 50 error:timeout error:timeout error:timeout error:timeout 12 error:timeout error:fold-ipc-eval error:fold-ipc-eval error:timeout error:timeout error:timeout error:fold-ipc-eval 0 96 0 96 error:fold-ipc-eval 0 error:fold-ipc-eval error:fold-ipc-eval error:timeout error:fold-ipc-eval error:timeout 0 0 error:timeout)")
        (trajectory
          .
          "0041c6a3ad3662856e943a68851c255a3f2e0122e8550add28ba09f11974b3305d"))
      ((label . "trial-2/100") (trial . 2) (n-entries . 100) (haystack-chars . 54954)
        (expected . 403) (match-count . 7) (status . exhausted)
        (time-ms . 51600) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "007b658933017c106c8244a47de2bedde6e9e6db208ed05e0b90bf548efde44491"))
      ((label . "trial-2/200") (trial . 2) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . exhausted)
        (time-ms . 91762) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "00c5ca258abb1273809a7b285588f36bbaa36e9cc6c0d7b79efdedc7157622b4a9"))
      ((label . "trial-3/100") (trial . 3) (n-entries . 100) (haystack-chars . 54954)
        (expected . 403) (match-count . 7) (status . completed)
        (time-ms . 41971) (correct . #f) (output . "137")
        (trajectory
          .
          "005b0c257ab9d69a8bb0fbf8dac4078236a45d5f3960362e3069127aa66377dbec"))
      ((label . "trial-3/200") (trial . 3) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 105166) (correct . #t) (output . "1070")
        (trajectory
          .
          "002df993a6cbfa5762902327749d4073abacf4414e728424e9797e54cdc65d59a6")))))
