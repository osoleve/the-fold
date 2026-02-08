(benchmark-results (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (mode "gsm8k") (timestamp "2026-02-08T13:38:23Z")
  (n-problems 20) (correct 17) (accuracy 0.85)
  (total-ms 366938)
  (problems
    (((question
        .
        "Paul is at a train station and is waiting for his train. He isn't sure how long ...") (expected . 145) (status . completed) (time-ms . 56110)
       (correct . #t) (output . "145")
       (trajectory
         .
         "0035b396a39a8f9c71d220ca1cc0b0e40459bfbfa342feec6dcdfd6308810fe2db"))
      ((question
         .
         "Kyle bought last year's best-selling book for $19.50. This is with a 25% discoun...") (expected . 26) (status . completed) (time-ms . 7110)
        (correct . #t) (output . "26")
        (trajectory
          .
          "00bf52480c8955dad78fe47aff9033e728f34ead229ff1367c6a0df50ddb37d0c6"))
      ((question
         .
         "Royce takes 40 minutes more than double Rob to shingle a house. If Rob takes 2 h...") (expected . 280) (status . completed) (time-ms . 7266)
        (correct . #t) (output . "280")
        (trajectory
          .
          "005b58a9270966da4ed2849ca3df2596a1ab11aa7b7d59e3b6dc30f8d3f828dafd"))
      ((question
         .
         "Luke is spending time at the beach building sandcastles. He eventually notices t...") (expected . 60) (status . completed) (time-ms . 7177)
        (correct . #t) (output . "60")
        (trajectory
          .
          "00b2e83454b375e45888dcd0d481cea603152c09173ffa4efb36bed47250d196ac"))
      ((question
         .
         "Keegan was running a car wash with his friend Tashay to raise money for a baseba...") (expected . 26) (status . completed) (time-ms . 30344)
        (correct . #t) (output . "26")
        (trajectory
          .
          "003d9be18f867e99882ef43a51d29bcb43e2f1b0c6bc1a323a38ba3efa5f39bc93"))
      ((question
         .
         "A farmer has 900 eggs. He placed them on a tray, which holds 30 eggs each. How m...") (expected . 75) (status . completed) (time-ms . 13624)
        (correct . #t) (output . "75")
        (trajectory
          .
          "007a2c3515100d786fd0f628ca87ffd5538f20253b1063d85feccd82006fef34a8"))
      ((question
         .
         "Melanie's father opens up an animal farm starting with 50 cows and 20 chickens. ...") (expected . 700) (status . completed) (time-ms . 7838)
        (correct . #t) (output . "700")
        (trajectory
          .
          "0058b5becdd2e0456fe8488dac88b96bc9b5ca636dfd39e0a0af811e960ee3d7f2"))
      ((question
         .
         "At Ashley's school, they start a reforestation campaign where each child plants ...") (expected . 1240) (status . completed) (time-ms . 28040)
        (correct . #t) (output . "1240")
        (trajectory
          .
          "002b7b877c1ab1eba985db6bbf63538beb582eb218e3c39383518a02e162a360b0"))
      ((question
         .
         "How much does it cost you for lunch today at Subway if you pay $40 for a foot-lo...") (expected . 160) (status . completed) (time-ms . 6762)
        (correct . #t) (output . "160")
        (trajectory
          .
          "00ac599892533ccde48df3f96b01a6102f1c45191d1ab25e4964b93bd0722fcc12"))
      ((question
         .
         "Tyler wants to buy a dictionary that costs $18, a dinosaur book that costs $13, ...") (expected . 5) (status . exhausted) (time-ms . 46205)
        (correct . #f) (output . "Resources exhausted")
        (trajectory
          .
          "008b048124bea92315e6fa12f8610189127769571c7b43f767df1f74c6750baa45"))
      ((question
         .
         "A $2000 watch was put on sale so that Mr. Rogers bought it at 75% of its origina...") (expected . 10) (status . exhausted) (time-ms . 64435)
        (correct . #f) (output . "Resources exhausted")
        (trajectory
          .
          "00f933e266b89cc2ad0aa3128e293fd7a6808dc6043fb771bf849c922f15707e08"))
      ((question
         .
         "Tobias, Chikote, and Igneous are the three little wolves who live in the forest ...") (expected . 2) (status . completed) (time-ms . 8682)
        (correct . #t) (output . "2")
        (trajectory
          .
          "0026c38faff80dee14bb71790f22bfd8ae27f5d58331129bbb79e31f0905e5d5b4"))
      ((question
         .
         "Linus works for a trading company. He buys a mobile device for $20 and sells it ...") (expected . 120) (status . completed) (time-ms . 7471)
        (correct . #t) (output . "120")
        (trajectory
          .
          "0098545af4544a92960ca82f48aaa1ae096554cef50b3fe9ad987ca538b98ede96"))
      ((question
         .
         "I am three years younger than my brother, and I am 2 years older than my sister....") (expected . 13) (status . completed) (time-ms . 18232)
        (correct . #t) (output . "13")
        (trajectory
          .
          "00f899a59e3b251c2348653f16ba0ce53a0bff60dc2be4c4817ac0b4b91f0f55c8"))
      ((question
         .
         "After scoring 14 points, Erin now has three times more points than Sara, who sco...") (expected . 18) (status . completed) (time-ms . 7261)
        (correct . #f) (output . "10")
        (trajectory
          .
          "00f71f563493326eef85c9ea26a123ca713952946302b3b54aded6d341a8facbb4"))
      ((question
         .
         "In the first week, Judy read for 15 minutes each night before going to sleep. In...") (expected . 240) (status . completed) (time-ms . 19472)
        (correct . #t) (output . "240")
        (trajectory
          .
          "009f0aed33e342733008486c8522962a0905ddd42f4c69aff031f650b64a68da57"))
      ((question
         .
         "Mary is making ice cubes with fruit frozen in them for a cocktail party. She mak...") (expected . 96) (status . completed) (time-ms . 7287)
        (correct . #t) (output . "96")
        (trajectory
          .
          "00046ed400e6402f4a151228e4f715c8e6e87707ef20e84d79739d7f3488665625"))
      ((question
         .
         "Josh runs a car shop and services 3 cars a day.  He is open every day of the wee...") (expected . 120) (status . completed) (time-ms . 7954)
        (correct . #t) (output . "120")
        (trajectory
          .
          "00e54fee3af7c1642478e5cfe8831c6b4c4dbc167fe00b6eef61c776b7ae44b78a"))
      ((question
         .
         "Juan wants to add croissants to his bakery menu.  It takes 1/4 pound of butter t...") (expected . 7) (status . completed) (time-ms . 7284)
        (correct . #t) (output . "7")
        (trajectory
          .
          "00f2146079edfeed23cf8fdbb95341f3f6fab952e4c56345099ca4ff6dfa7c7a16"))
      ((question
         .
         "Craig has 2 twenty dollar bills. He buys six squirt guns for $2 each.  He also b...") (expected . 19) (status . completed) (time-ms . 8384)
        (correct . #t) (output . "19")
        (trajectory
          .
          "00afb4eec770c2be9faff30c0e86affc6bdeebab942f997938cc8f4f5bf241c112")))))
