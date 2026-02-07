(benchmark-results
  (model "Qwen/Qwen3-32B-FP8")
  (mode "oolong")
  (timestamp "2026-02-07T10:51:57Z")
  (oolong
    (((label . "OOLONG 100/50K") (n-entries . 100) (haystack-chars . 54954) (expected . 403)
       (match-count . 7) (status . completed) (time-ms . 59517)
       (correct . #t) (output . "403")
       (trajectory
         .
         "001fbf3f2b854acaf69290ff61800e9ca25165ca6f307d2ee78a181850961076e2"))
      ((label . "OOLONG 200/100K") (n-entries . 200) (haystack-chars . 109891)
        (expected . 1070) (match-count . 17) (status . completed)
        (time-ms . 38788) (correct . #f) (output . "81")
        (trajectory
          .
          "0063d767371fd4da4430f438864f93dc692909248cc0b90b96f1568d2a8599ed7e")))))
