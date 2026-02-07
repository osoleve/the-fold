(benchmark-results (model "nvidia/NVIDIA-Nemotron-3-Nano-30B-A3B-NVFP4")
  (timestamp "2026-02-07T12:53:43Z") (oolong ())
  (gsm8k
    ((correct . 3)
      (total . 10)
      (accuracy . 0.3)
      (total-ms . 443077)
      (problems
        ((expected . 145) (status . exhausted) (time-ms . 125124) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00d1b44ea6981c594efff7cb719aff096bb592a58e9def0fd66e0b5fc8c3ce26da"))
        ((expected . 26) (status . exhausted) (time-ms . 27337) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00858f1cab4f66c6303b654dc62b06f55b66846ebb0af28de217f30ef7125635f5"))
        ((expected . 280) (status . completed) (time-ms . 27323) (correct . #t)
          (output . "280")
          (trajectory
            .
            "00be438a665bff003e08a9c51f5c9c61f54f959ea95d642d90c92a3ae4ec9f2485"))
        ((expected . 60) (status . exhausted) (time-ms . 102638) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00b539cc8f6a9b05bb60720733a260f12906bdd7cd55e3af70cf9f521bd0950f39"))
        ((expected . 26) (status . exhausted) (time-ms . 20205) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "008ef0d833df409c5e4e19060cb5b09317132d12d38aca0ce432789fb29856e386"))
        ((expected . 75) (status . completed) (time-ms . 16415) (correct . #t)
          (output . "75")
          (trajectory
            .
            "008f8c2f36a032de9fd0c4a37b89de520199f24f8f06f0437d5abd00b53f2e85a8"))
        ((expected . 700) (status . exhausted) (time-ms . 43648) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00a74b54266f693a0dbdea3ed4687e975469a9a25750dbfa8ec4587e4d9f6f9ab4"))
        ((expected . 1240) (status . exhausted) (time-ms . 38095) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "008cfb4f1e6b341b8b65fe7008d6f4747603bf20ec391d6cf64d24fa06147082e2"))
        ((expected . 160) (status . completed) (time-ms . 14046) (correct . #t)
          (output . "160")
          (trajectory
            .
            "008d3b72e1f906583cfec993fe211d838e7ddfae811c9d63b15715554aa043f078"))
        ((expected . 5) (status . exhausted) (time-ms . 28246) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "0084278c2e4babe87f2e602053f21fbb150e0abfe02a7369b5d9f0d0b5898d4d39")))))
  (drop
    ((correct . 0)
      (total . 10)
      (accuracy . 0.0)
      (total-ms . 206951)
      (problems
        ((question . "How many French out-posts were still there?") (expected . "25") (status . exhausted) (time-ms . 21667)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "000c21c2255adb58b4e0f67bb366e90da4940db6069d522a5aefde053f36df6279"))
        ((question
           .
           "How many years did the Lakewood Summer Theatre exist before being named the official summer theatre of Maine?") (expected . "66") (status . exhausted) (time-ms . 16282)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "009b3d197ccb9d2dd424a83922f1b9635f3d3653365cc31cbc203b5079a0c13153"))
        ((question . "How many percent were not Canadian?") (expected . "81.2") (status . exhausted) (time-ms . 17552)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00f245ca1cb5a294cc4a3364f6cfb4fe49483b0f6e5caaac5e90677d6b680498e4"))
        ((question
           .
           "How many fewer robberies were there in Harlem in 2010 compared to 2000?") (expected . "600") (status . exhausted) (time-ms . 21696)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "006bc7060883a68ad20011decdb806fd228fe8ce058954f766fceaf1312293d3e4"))
        ((question
           .
           "Which touchdown passes did Jason Campbell make?") (expected . "2") (status . exhausted) (time-ms . 17611)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00103ad288419c9bc91598abf3f6c22e366738ff8a06ba14ddaf0cd6c2379c20c4"))
        ((question
           .
           "How many of the ethnic groups listed had more than 10000 inhabitants in Skopje in 2002?") (expected . "4") (status . exhausted) (time-ms . 30117)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "005cc3afb146b8d4a2b14d2b8ea165c5211a28181c3b6e5becc56cb9c3ca6c3cb9"))
        ((question
           .
           "How many degrees Celsius difference, is the average temperature in January in Luang Prabang compared to the average temperature in April, in Vientiane?") (expected . "18.9") (status . completed) (time-ms . 10762)
          (correct . #f) (output . "6")
          (trajectory
            .
            "000c610d5525317e86103300c26159dc5af26d7efaaefbe686347862b02c1b1bb9"))
        ((question . "How many points in total were scored?") (expected . "57") (status . exhausted) (time-ms . 16669)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00b6009990c8a7ec1c96e3a4dfbd9f506e9445d2acb403959a2241deb0b3875501"))
        ((question . "How many yards did Rivers pass?") (expected . "231") (status . exhausted) (time-ms . 28419)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00e802ce299f43500ddff49b66d45c3f15130551fc47528e8e8d757ce3c7371228"))
        ((question
           .
           "How many total hamburgers did Kobayashi eat in the 2004, 2005 and 2006 Krystal Square offs?") (expected . "233") (status . exhausted) (time-ms . 26176)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "004f45475a0bcb4729e6866c226911b41d765da4987fa0e98d17dab3d46397abe3")))))
  (rank ()))
