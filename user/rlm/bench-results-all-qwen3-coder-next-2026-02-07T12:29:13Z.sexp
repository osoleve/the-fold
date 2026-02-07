(benchmark-results (model "Qwen/Qwen3-Coder-Next-FP8")
  (timestamp "2026-02-07T12:29:13Z") (oolong ())
  (gsm8k
    ((correct . 7)
      (total . 10)
      (accuracy . 0.7)
      (total-ms . 277403)
      (problems
        ((expected . 145) (status . exhausted) (time-ms . 54205) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00fe3c0e39095bcd88170671a48f5f95dc403aba5d9a82fe439e3b2353695926bb"))
        ((expected . 26) (status . completed) (time-ms . 26701) (correct . #t)
          (output . "26")
          (trajectory
            .
            "001b01faece2b697e63f79939adacb5ee738769af82e8e5fc8a88faf3ace1e1d95"))
        ((expected . 280) (status . exhausted) (time-ms . 30929) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "00b01b0a7169eeaa8a66a22b7f02e418a1ba043fd6bb0e2f1a7dbb3df09b0b8009"))
        ((expected . 60) (status . completed) (time-ms . 12815) (correct . #t)
          (output . "60")
          (trajectory
            .
            "00cb05e9dc34794f1757d5ed0be25e5718713d2e85b89917f90ddc99b39d9890d3"))
        ((expected . 26) (status . completed) (time-ms . 7714) (correct . #t)
          (output . "26")
          (trajectory
            .
            "008d7f59dabcefce0a2617e8b4cefcf81151b836dd8da23b197da477dfa36db746"))
        ((expected . 75) (status . completed) (time-ms . 26326) (correct . #t)
          (output . "75")
          (trajectory
            .
            "00e710badbffc2c8ad5e1a3d839a92352649d5e196b68fa7ba869a348466f6476f"))
        ((expected . 700) (status . exhausted) (time-ms . 57411) (correct . #f)
          (output . "Resources exhausted")
          (trajectory
            .
            "006e67273681690f24fb2be49139ac7ac0afbe13358dc3811ff5dd392ed13d854c"))
        ((expected . 1240) (status . completed) (time-ms . 33772) (correct . #t)
          (output . "1240")
          (trajectory
            .
            "005c366f75415a2a02fc4c7c4095a973f5c87b9aa32bc55ddbf7fd92ebfd2bdc79"))
        ((expected . 160) (status . completed) (time-ms . 9474) (correct . #t)
          (output . "160")
          (trajectory
            .
            "00841842b0d742bd54d9f51c8d11ba83597093b1ffce802d9eafd349e7f24e6aa4"))
        ((expected . 5) (status . completed) (time-ms . 18056) (correct . #t)
          (output . "5")
          (trajectory
            .
            "00f5e99efc601ea3fd7f2ab8f5893694c4515368cca8e3970da9e2fd04f80993aa")))))
  (drop
    ((correct . 1)
      (total . 10)
      (accuracy . 0.1)
      (total-ms . 266637)
      (problems
        ((question . "How many French out-posts were still there?") (expected . "25") (status . completed) (time-ms . 50278)
          (correct . #f) (output . "26")
          (trajectory
            .
            "008efba3a016c7bda2c45a79a03b7b0a782c232b711caf6eee565425df6bf0feb9"))
        ((question
           .
           "How many years did the Lakewood Summer Theatre exist before being named the official summer theatre of Maine?") (expected . "66") (status . exhausted) (time-ms . 26002)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "000189db6113cd62664651c65f4387c422d46ed2cd14f30378ba833fa97e9e52dd"))
        ((question . "How many percent were not Canadian?") (expected . "81.2") (status . exhausted) (time-ms . 20528)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00a24bcec53ee343c3899bb06a7326facd81eb8e4c5d5d7a1644ee033e2f1c6e99"))
        ((question
           .
           "How many fewer robberies were there in Harlem in 2010 compared to 2000?") (expected . "600") (status . exhausted) (time-ms . 28729)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00e1b6ffa172bab57d3dea65e7b450548270a92665bbc84aa645a8d6997b15ca2e"))
        ((question
           .
           "Which touchdown passes did Jason Campbell make?") (expected . "2") (status . completed) (time-ms . 22079)
          (correct . #t) (output . "2")
          (trajectory
            .
            "00aba952c5dc546a89ecefefec0368ba38ef42e9947d5b8c5276829c66d64d222b"))
        ((question
           .
           "How many of the ethnic groups listed had more than 10000 inhabitants in Skopje in 2002?") (expected . "4") (status . exhausted) (time-ms . 29602)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00f195128fe56c0fe5f73fe5ad496aa21051b8282006245ca0cbae7308114f7164"))
        ((question
           .
           "How many degrees Celsius difference, is the average temperature in January in Luang Prabang compared to the average temperature in April, in Vientiane?") (expected . "18.9") (status . exhausted) (time-ms . 25398)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "008e9f4224b8715aab05157062333b5cf6adf7bf28a62d71a6a1423db3ce751c83"))
        ((question . "How many points in total were scored?") (expected . "57") (status . completed) (time-ms . 24604)
          (correct . #f) (output . "51")
          (trajectory
            .
            "0003894b7da01a81334296737fb2eb5844235615776c0732cc041df722d58c35f1"))
        ((question . "How many yards did Rivers pass?") (expected . "231") (status . exhausted) (time-ms . 16752)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "008a1a09e6256e939fe11553960605a6cc77d1aec668415fb900e0f7c4d8506b89"))
        ((question
           .
           "How many total hamburgers did Kobayashi eat in the 2004, 2005 and 2006 Krystal Square offs?") (expected . "233") (status . exhausted) (time-ms . 22665)
          (correct . #f) (output . "Resources exhausted")
          (trajectory
            .
            "00158debf11984e066b8d6cc63226739e91b1415ea1eed899bcdc2357cf664c6fa")))))
  (rank ()))
