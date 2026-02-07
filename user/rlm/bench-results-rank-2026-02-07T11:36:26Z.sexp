(benchmark-results
  (model "Qwen/Qwen3-32B-FP8")
  (mode "oolong-rank")
  (timestamp "2026-02-07T11:36:26Z")
  (rank
    (((label . "Rank Top-5 / 100 entries") (n-entries . 100) (k . 5) (haystack-chars . 54954)
       (expected ("0028" . 86) ("0023" . 81) ("0052" . 70)
         ("0049" . 52) ("0027" . 51))
       (status . completed) (time-ms . 111392) (found . 4)
       (total . 5)
       (output
         .
         "((0028 . 86) (0023 . 81) (0052 . 70) (0049 . 52) (0096 . 50))")
       (trajectory
         .
         "0015206b407fe8bc59b419c5ea04208270bd1d6de267dd3de2128124d4858c3e14"))
      ((label . "Rank Top-5 / 200 entries") (n-entries . 200) (k . 5) (haystack-chars . 109891)
        (expected ("0124" . 100) ("0147" . 96) ("0158" . 93)
          ("0188" . 92) ("0028" . 86))
        (status . completed) (time-ms . 109016) (found . 0)
        (total . 5)
        (output
          .
          "(take (sorted (retrieve 'alpha-north-values) (lambda (a b) (> (cdr a) (cdr b)))) 5)")
        (trajectory
          .
          "005927aead5b321e6af53ccc84b33704622f2ddbbb468ec2109c9bfb482518eccf")))))
