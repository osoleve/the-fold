(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (mode "few-shot-parallel") (n-trials 3) (n-fewshot 5)
  (timestamp "2026-02-06T23:29:26Z")
  (summary ((oolong-100 1 3 32158) (oolong-200 0 3 41790)))
  (trials
    (((label . "trial-1/100") (trial . 1) (n-entries . 100) (haystack-chars . 54954)
       (expected . 403) (match-count . 7) (status . exhausted)
       (time-ms . 67377) (correct . #f)
       (output . "Resources exhausted")
       (trajectory
         .
         "00931be817c34fac7f519d4f5fc482400198328f748b13360772e584a3ae4f75d4"))
      ((label . "trial-1/200") (trial . 1) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . exhausted)
        (time-ms . 38881) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "0046f1b7ebfdfb27400b38517576a249ab5de20400ea81ee4c2c5d9d81b02af714"))
      ((label . "trial-2/100") (trial . 2) (n-entries . 100) (haystack-chars . 54954)
        (expected . 403) (match-count . 7) (status . completed)
        (time-ms . 8700) (correct . #f) (output . "0")
        (trajectory
          .
          "0059e7c5029be463cf34f90ecfed4d6e16b4304186257aa42265be3b587d777bff"))
      ((label . "trial-2/200") (trial . 2) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . exhausted)
        (time-ms . 64217) (correct . #f)
        (output . "Resources exhausted")
        (trajectory
          .
          "0093e15edae2a5f42055ba60824df851765b39f3690a020e9c3785acea11b75bb8"))
      ((label . "trial-3/100") (trial . 3) (n-entries . 100) (haystack-chars . 54954)
        (expected . 403) (match-count . 7) (status . completed)
        (time-ms . 20399) (correct . #t) (output . "403")
        (trajectory
          .
          "00eb8069b94c1cb99908fb80488b19dba517969e007b2a588e2b59e4804d15ff6e"))
      ((label . "trial-3/200") (trial . 3) (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 22274) (correct . #f)
        (output
          .
          "(error:timeout 0 0 0 0 0 0 error:fold-ipc-eval 0 0 81 137 error:timeout 0 52 error:fold-ipc-eval 0 52 0 70 error:timeout 0 error:timeout 0 0 0 error:timeout 0 0 error:timeout 0 0 42 0 0 119 0 100 12 0 30 0 0 0 96 0 0 error:fold-ipc-eval 0 0 83 0 0 0 0 0 92 0 0 0)")
        (trajectory
          .
          "00494b8287347e6f1dcbb098da232657830e257eff13aef33cf5497c0ab117e4dc")))))
