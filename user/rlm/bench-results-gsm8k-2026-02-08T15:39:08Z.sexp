(benchmark-results (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (mode "gsm8k") (timestamp "2026-02-08T15:39:08Z")
  (n-problems 100) (correct 34) (accuracy 0.34)
  (total-ms 2481040)
  (problems
    (((question
        .
        "Greta and Celinda are baking cookies. Greta bakes 30 cookies and Celinda bakes t...") (expected . 80) (status . completed) (time-ms . 11989)
       (correct . #t) (output . "80")
       (trajectory
         .
         "00edd60b241f136e2e7595dace9689395b8638f06984162a82f26b33c96c377ef6"))
     ((question
        .
        "Tyler wants to buy a dictionary that costs $18, a dinosaur book that costs $13, ...") (expected . 5) (status . exhausted) (time-ms . 86829)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "006d17ed273be75c940638b2e1c7abed1dec01778bf471de377a25db19ba955e46"))
     ((question
        .
        "Carlos, Jim and Carrey were at the beach playing and they decided to gather some...") (expected . 20) (status . completed) (time-ms . 14524)
       (correct . #t) (output . "20")
       (trajectory
         .
         "00989b0852edb51786fe19607a76f574b8b31c4af59504249a161bfdb32771e1e5"))
     ((question
        .
        "Tom buys a bedroom set for $3000.  He sells his old bedroom for $1000 and uses t...") (expected . 200) (status . completed) (time-ms . 13681)
       (correct . #t) (output . "200")
       (trajectory
         .
         "00b68ffc51e2a72c103a3cf39f04128654b141fe0eb4633cca5e2587201d4c80e2"))
     ((question
        .
        "Mike's teacher, leaves as homework the reading of a 200-page book. The assignmen...") (expected . 10) (status . completed) (time-ms . 19640)
       (correct . #t) (output . "10")
       (trajectory
         .
         "00bfcde97f203d4220ff804d554e53f119f3d84b19a792c9791acc6eb1ff268854"))
     ((question
        .
        "I am three years younger than my brother, and I am 2 years older than my sister....") (expected . 13) (status . completed) (time-ms . 27130)
       (correct . #t) (output . "13")
       (trajectory
         .
         "0038e6e4742929b95a6442066b6128c68b86f5a586a3abcdc4a3cd976bef0c124e"))
     ((question
        .
        "Rose is out picking flowers for a vase she wants to fill.  She starts off by pic...") (expected . 79) (status . completed) (time-ms . 24402)
       (correct . #t) (output . "79")
       (trajectory
         .
         "004c20a2e72a748b14b8b790a7204a2ff79d9c01c82792668fdadfc09a6aa0d055"))
     ((question
        .
        "A phone tree is used to contact families and relatives of Ali's deceased coworke...") (expected . 81) (status . completed) (time-ms . 13262)
       (correct . #t) (output . "81")
       (trajectory
         .
         "00694c0e09ee5c587e7f3159a4b432a181d0ea9085676df7a54fad6c21405f1f3b"))
     ((question
        .
        "Tiffany is measuring how many surfers can ride a big wave without falling. She s...") (expected . 10) (status . completed) (time-ms . 13032)
       (correct . #t) (output . "10")
       (trajectory
         .
         "004ec2ecc9848eec51156480e82fdd8052492c6c83e477f8b7f3b83447dbce29a8"))
     ((question
        .
        "Jason was told he could earn $3.00 for doing his laundry,  $1.50 for cleaning hi...") (expected . 9) (status . completed) (time-ms . 59854)
       (correct . #t) (output . "9.0")
       (trajectory
         .
         "007c7089b6e82d20f8eddc13a517c24f4a360dd25ac8396ac7e7a94716fc650818"))
     ((question
        .
        "A news website publishes an average of 20 political and weather news articles ev...") (expected . 840) (status . completed) (time-ms . 17154)
       (correct . #f) (output . "261")
       (trajectory
         .
         "000405acd1350fd39155137262a73c4c8641a70f0a17c786b50cc5fa2be15e0ca9"))
     ((question
        .
        "Avery needs to buy a 3 piece place setting (dinner & salad plate and a bowl) for...") (expected . 180) (status . completed) (time-ms . 19002)
       (correct . #t) (output . "180")
       (trajectory
         .
         "00f69bf72e0565e57c1057f5967ccbd396708d962382af61895b19554703f32ff2"))
     ((question
        .
        "Hannah has a mental breakdown while studying for finals and starts smashing wind...") (expected . 112) (status . completed) (time-ms . 42263)
       (correct . #t) (output . "112")
       (trajectory
         .
         "0027c96fe55812e1d8980f7393bb6ed8787febd1102c1e4add174b9204a90f5af1"))
     ((question
        .
        "Adam went to a store to buy some sweets. He bought 7 candies of type A and 10 ca...") (expected . 4) (status . completed) (time-ms . 31563)
       (correct . #t) (output . "4.0")
       (trajectory
         .
         "00223a18e5a937bbbb2844057e7998fb57bb67415e415bac0a7fecd69326ad1546"))
     ((question
        .
        "A $2000 watch was put on sale so that Mr. Rogers bought it at 75% of its origina...") (expected . 10) (status . exhausted) (time-ms . 136243)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00d5f6d42db940e23fbc7b8612242fca478add40c068f3a40b495f689b82fce42d"))
     ((question
        .
        "How much does it cost you for lunch today at Subway if you pay $40 for a foot-lo...") (expected . 160) (status . completed) (time-ms . 14897)
       (correct . #t) (output . "160")
       (trajectory
         .
         "00c9e5350e0be480e176ae277c11844924e99d834ea49a3481b64aeccdfa522aa1"))
     ((question
        .
        "Linus works for a trading company. He buys a mobile device for $20 and sells it ...") (expected . 120) (status . completed) (time-ms . 18710)
       (correct . #t) (output . "120")
       (trajectory
         .
         "00e243cd34870c210c2c214d9ef045c3aee0314332d47ba22264783faa73f60504"))
     ((question
        .
        "John decides to do several activities while out on vacation.  He spends 6 hours ...") (expected . 20) (status . completed) (time-ms . 19852)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f59669c1638d1bdec5b6fc1a9c92241587371ac5dcf9dc82d55c5f0b16a4527b"))
     ((question
        .
        "Paul is at a train station and is waiting for his train. He isn't sure how long ...") (expected . 145) (status . completed) (time-ms . 14220)
       (correct . #f) (output . "20")
       (trajectory
         .
         "00cd6004daeecf5a512e413f3b86c6c1d90ccf65cdb876fd82990eaccf44082362"))
     ((question
        .
        "Well's mother sells watermelons, peppers, and oranges at the local store. A wate...") (expected . 880) (status . completed) (time-ms . 42732)
       (correct . #t) (output . "880")
       (trajectory
         .
         "00b81ec370fd46d98300812370f31b590925cbf207dd319faf5d6303b149ba6960"))
     ((question
        .
        "Josh runs a car shop and services 3 cars a day.  He is open every day of the wee...") (expected . 120) (status . completed) (time-ms . 6691)
       (correct . #f) (output . "")
       (trajectory
         .
         "0009d584fb8cc23ff1a70fc76b896842b6c508a88955ab37547364c4beca5bc80c"))
     ((question
        .
        "In the first week, Judy read for 15 minutes each night before going to sleep. In...") (expected . 240) (status . completed) (time-ms . 27346)
       (correct . #t) (output . "240.0")
       (trajectory
         .
         "00768f87e66216abf7092347dc5e1347e4e7bda40c49bfbfbca0084354bc1819ad"))
     ((question
        .
        "James runs 12 miles a day for 5 days a week.  If he runs 10 miles an hour how ma...") (expected . 6) (status . completed) (time-ms . 44857)
       (correct . #t) (output . "6")
       (trajectory
         .
         "00ae0097ad94c3eaed3a4f3b177edba4fc3caaac1da7a003fbf0df8b9f9eb7dde5"))
     ((question
        .
        "The vending machines sell chips for 40 cents and candy bars for 75 cents. George...") (expected . 5) (status . completed) (time-ms . 45172)
       (correct . #t) (output . "5.0")
       (trajectory
         .
         "005e96aa3cf8dba6de479ceea61ae6d9168133de9e104a73a4d1fbb91a73a93443"))
     ((question
        .
        "John buys 2 pairs of shoes for each of his 3 children.  They cost $60 each.  How...") (expected . 360) (status . completed) (time-ms . 7199)
       (correct . #f) (output . "")
       (trajectory
         .
         "00247f2d17e4eeb3430ee13e4cea2411e7374ebf41bc70229723e8158109ed6c31"))
     ((question
        .
        "After transferring to a new school, Amy made 20 more friends than Lily. If Lily ...") (expected . 120) (status . completed) (time-ms . 16175)
       (correct . #t) (output . "120")
       (trajectory
         .
         "00e90d3edc8c67440b16aa415b50f1c0951d587dfe91c3c279a0cb41f7ed9e44af"))
     ((question
        .
        "Ashley has an internet connection speed of 20kb per second. Knowing that 1 Mb ha...") (expected . 72) (status . completed) (time-ms . 8581)
       (correct . #f) (output . "")
       (trajectory
         .
         "000ee8ae0924b938626fc1bcc16a9ba93f7004930aa87e1ab14e1c3bd60a8080d8"))
     ((question
        .
        "Tobias, Chikote, and Igneous are the three little wolves who live in the forest ...") (expected . 2) (status . completed) (time-ms . 11647)
       (correct . #f) (output . "")
       (trajectory
         .
         "009c31ed7c9f6f6f476a979762899d6e024cfd1575de59a727152d94ba3a319849"))
     ((question
        .
        "Melanie's father opens up an animal farm starting with 50 cows and 20 chickens. ...") (expected . 700) (status . completed) (time-ms . 7776)
       (correct . #f) (output . "")
       (trajectory
         .
         "004b0293c4fbf7da79ebdeb05cfdbd2be963d128280dd90e2e5529761f1d957727"))
     ((question
        .
        "Shawnda decides that her neighborhood kids could really use a bike inflation ser...") (expected . 5) (status . completed) (time-ms . 26651)
       (correct . #f) (output . "(* 20 0.25)")
       (trajectory
         .
         "00e57963e350bfd6ebf63c41e733f41d5044d377e60aff5791e984d694901b44e6"))
     ((question
        .
        "There are three trees in Eddy's backyard. The shortest tree has a height of 6 fe...") (expected . 34) (status . completed) (time-ms . 9073)
       (correct . #f) (output . "")
       (trajectory
         .
         "004cf879619a009e700832b612317d0310e2f115fb4ecb66788a046770349c7685"))
     ((question
        .
        "Andrea has 8 more apples than Jamal and half as many bananas as Jamal. Jamal has...") (expected . 168) (status . completed) (time-ms . 11617)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f91e1cc61981ad6b598461d97c0ed10e08928e145d0f1636ba5fdefd5cb714d0"))
     ((question
        .
        "There are 10000 gallons of water in a pool. Using a water pump, Anthony and his ...") (expected . 2000) (status . completed) (time-ms . 10860)
       (correct . #f) (output . "")
       (trajectory
         .
         "00b7478ceb46648fccac0e221a4df0399ae4fb74795f9466a1119e7b992c9407df"))
     ((question
        .
        "Luke is spending time at the beach building sandcastles. He eventually notices t...") (expected . 60) (status . completed) (time-ms . 21763)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f000ae3b3831deeac07f1ba603c6574ed4046ee94cbc7ef7c42a96d584a8e9eb"))
     ((question
        .
        "A factory used to make tractors, but now makes silos. When they made tractors, t...") (expected . 10) (status . completed) (time-ms . 10823)
       (correct . #f) (output . "100")
       (trajectory
         .
         "005615f7e3d72808a8e39dc2e7868473726c614c03d2184feeec79713586beb9df"))
     ((question
        .
        "A bakery has 40 less than seven times as many loaves of bread as Sam had last Fr...") (expected . 450) (status . completed) (time-ms . 6152)
       (correct . #f) (output . "")
       (trajectory
         .
         "00b32723bc4eb159923f9df48c0821d346f61e0952da447889d1b4c69f969c12fc"))
     ((question
        .
        "Shiloh is 44 years old today.  In 7 years, he will be three times as old as his ...") (expected . 10) (status . completed) (time-ms . 15720)
       (correct . #f) (output . "")
       (trajectory
         .
         "0007cc545eb9868cf7186f51e52d39509ce53b566c646617b6a92e2c09a93158d3"))
     ((question
        .
        "Rosie can run 10 miles per hour for 3 hours. After that, she runs 5 miles per ho...") (expected . 50) (status . completed) (time-ms . 6391)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ac7d10d8f2252582b36fdd3f20c5d66963eced4ce7d15c65de9e86aab49221de"))
     ((question
        .
        "Four children are playing together—Akbar, Alessandro, Helene, and Wilfred. Helen...") (expected . 3) (status . completed) (time-ms . 32819)
       (correct . #t) (output . "3")
       (trajectory
         .
         "00392f7062bc62a48e9a97ec4a5df1fd00821c9157528bf85dc63028f06698db33"))
     ((question
        .
        "Max plans to watch two movies this weekend. The first movie is 1 hour and 30 min...") (expected . 215) (status . completed) (time-ms . 6063)
       (correct . #f) (output . "")
       (trajectory
         .
         "0041552a54609a5cb0451a60a3c368cae79ad0e9f4548a4a79f104f4b168779bca"))
     ((question
        .
        "A farmer has 900 eggs. He placed them on a tray, which holds 30 eggs each. How m...") (expected . 75) (status . completed) (time-ms . 32042)
       (correct . #f) (output . "30")
       (trajectory
         .
         "004c8026d2e0b118f1a01ccf3749ad1f472b6e7b716de255ddae3c9f9e502b0c02"))
     ((question
        .
        "Lauren is a cartoonist.  She can draw 5 large-sized picture scenes per day, or s...") (expected . 22) (status . completed) (time-ms . 8055)
       (correct . #f) (output . "7")
       (trajectory
         .
         "00125cf867232b92a9cce4eec74735d004768b7538ba917219cf3cf176b908b9ec"))
     ((question
        .
        "The great dragon, Perg, sat high atop mount Farbo, breathing fire upon anything ...") (expected . 200) (status . completed) (time-ms . 13259)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ec77fec25c4c1ef1bcdec7d00902d747905fa6197865c88239758967b2c5761c"))
     ((question
        .
        "Fred was preparing for a party to be held in four days.  So, he made 24 gallons ...") (expected . 2) (status . completed) (time-ms . 23233)
       (correct . #t) (output . "2")
       (trajectory
         .
         "0040857b0ea5fd05e25dc96520eb3050e4bcb723244368a611f254489786163f60"))
     ((question
        .
        "To make 1 liter of juice, Sam needs 5 kilograms of oranges. Each kilogram of ora...") (expected . 60) (status . completed) (time-ms . 7465)
       (correct . #f) (output . "")
       (trajectory
         .
         "00618624a364a58dec77f03828a4114ac79ff242488524a445d46f35659277a6ab"))
     ((question
        .
        "Terri is knitting a sweater with two sleeves, a collar, and a decorative rosette...") (expected . 315) (status . completed) (time-ms . 14486)
       (correct . #f) (output . "")
       (trajectory
         .
         "0020811c3f92f1a044d2ffa73af0bda15711921bea6d7ab940d76f4457fd14335f"))
     ((question
        .
        "On Easter Sunday Cindy went to the city park to participate in the Easter Egg Hu...") (expected . 27) (status . completed) (time-ms . 12106)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ba051bed022dd6df7dfa22bed739b6a38d864f748c3515689147d10eb8ef06d2"))
     ((question
        .
        "Kimberly bought 8 packages of cat food and 6 packages of dog food. Each package ...") (expected . 52) (status . completed) (time-ms . 7738)
       (correct . #f) (output . "36")
       (trajectory
         .
         "00ee0e8e13624a58597cd097aba427f50526a31d96485dfb5d69d3bc5e8da04116"))
     ((question
        .
        "Helga was the fastest clog dancer in all of Slovenia. With both hands at her sid...") (expected . 2450) (status . completed) (time-ms . 16542)
       (correct . #f) (output . "")
       (trajectory
         .
         "00d1e5a023cf71a7b1b08a8a69045584f6cc46e2a6404243b8640bff7e201093f7"))
     ((question
        .
        "Mr. Ruther sold 3/5 of his land and had 12.8 hectares left. How much land did he...") (expected . 32) (status . completed) (time-ms . 13068)
       (correct . #f) (output . "(/ 12.8 (/ 2 5))")
       (trajectory
         .
         "0051777871a0f23f06f7f38227dfd0c4e694bb719cee62a766462751dac6cd32b3"))
     ((question
        .
        "Peter wants to make different sized ice cubes with 32 ounces of water. He can ma...") (expected . 2) (status . completed) (time-ms . 7847)
       (correct . #f) (output . "")
       (trajectory
         .
         "00b96d3c7ff85f48518e7fa29ff5cbae917ee339cf81d143bf434748beff7d9d8c"))
     ((question
        .
        "Bailey starts with a certain amount of money. Then she receives a weekly allowan...") (expected . 60) (status . completed) (time-ms . 6319)
       (correct . #f) (output . "")
       (trajectory
         .
         "00839c01f7b75a4cc5804e57c2ee97787941644bfac8813a42b5763a1cbfda2fb2"))
     ((question
        .
        "Carla just gave birth to identical octuplets. She dresses 3/4 of them in purple ...") (expected . 50) (status . completed) (time-ms . 10096)
       (correct . #f) (output . "")
       (trajectory
         .
         "00220973b84d2039f9f3ca54fb9dae4215dbdb80a421887680ee3b85627a745936"))
     ((question
        .
        "Theo bought a pen for $2 and a piece of paper that cost $1 less than three times...") (expected . 3) (status . completed) (time-ms . 7972)
       (correct . #f) (output . "")
       (trajectory
         .
         "009ffd96a66670589d70a8dd23189f37eadeca80db7d57926078965b4a07a7136c"))
     ((question
        .
        "A chef bought 4 bags of onions. Each bag weighs 50 pounds. A pound of onions cos...") (expected . 300) (status . completed) (time-ms . 6246)
       (correct . #f) (output . "")
       (trajectory
         .
         "00db3d831dba134074981d39ec71ed12b9dc7371e984070a2bf2446b7e134f466e"))
     ((question
        .
        "Mary is making ice cubes with fruit frozen in them for a cocktail party. She mak...") (expected . 96) (status . completed) (time-ms . 38623)
       (correct . #f) (output . "(- (* 5 20) 4)")
       (trajectory
         .
         "00de67c56274e966425a5597a04bd0a1abb98d63b99711a081f8f1cfb1142c3581"))
     ((question
        .
        "Rani is obsessed with sports cars. She wonders what the faster car ever made can...") (expected . 750) (status . completed) (time-ms . 15198)
       (correct . #t) (output . "750.0")
       (trajectory
         .
         "00410f114f22d3aff6113c3ef8fb46f9501203d9977247d4a49eeab9d8ff267066"))
     ((question
        .
        "In a grocery store, four apples cost $5.20, and three oranges cost $3.30. How mu...") (expected . 12) (status . completed) (time-ms . 34315)
       (correct . #f) (output . "1.0999999999999999")
       (trajectory
         .
         "007f4254ad90b76d430b53c978b3492dd434dddd2cd5ad45b70302f4f3c0f34253"))
     ((question
        .
        "Keegan was running a car wash with his friend Tashay to raise money for a baseba...") (expected . 26) (status . completed) (time-ms . 11657)
       (correct . #f) (output . "174")
       (trajectory
         .
         "005a486699c19ef688a8c119c0580739ef18a4ac4270e4cdb6062bef34f007f4b0"))
     ((question
        .
        "Royce takes 40 minutes more than double Rob to shingle a house. If Rob takes 2 h...") (expected . 280) (status . completed) (time-ms . 7561)
       (correct . #f) (output . "")
       (trajectory
         .
         "009a91f8a5145d956d88e3c197ffda896ac82064fecb9acbabbbea85ba042003fc"))
     ((question
        .
        "There are 36 penguins sunbathing in the snow.  One-third of them jump in and swi...") (expected . 12) (status . completed) (time-ms . 17422)
       (correct . #t) (output . "12")
       (trajectory
         .
         "00c1fc9c14551ce2b9a036619363881053d89cc758097734e7dfccc45c253cdd67"))
     ((question
        .
        "Harold sleeps for 10 hours a night.  He works 2 hours less than he sleeps and he...") (expected . 5) (status . completed) (time-ms . 12714)
       (correct . #f) (output . "")
       (trajectory
         .
         "00911f6fb7fb359fc4c2642041193d7c82898b01d6b11ef640c2ef4555d82d214e"))
     ((question
        .
        "A train has 172 people traveling on it. At the first stop 47 people get off and ...") (expected . 100) (status . completed) (time-ms . 8667)
       (correct . #f) (output . "")
       (trajectory
         .
         "0050d680c96d7e4527385189063181d9add3d48d7dd59de02b1ed2b352224e763b"))
     ((question
        .
        "Together Lily, David, and Bodhi collected 43 insects. Lily found 7 more than Dav...") (expected . 16) (status . completed) (time-ms . 10269)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ece7ba5f365e13fd63d49c00fbf718c21f457d8bb91d43a191a0cc5281701b2d"))
     ((question
        .
        "The educational shop is selling notebooks for $1.50 each and a ballpen at $0.5 e...") (expected . 8) (status . completed) (time-ms . 6702)
       (correct . #f) (output . "")
       (trajectory
         .
         "004849d72e72efbb402dbc83da6768201912242707da2d8f76fa3529204e2af408"))
     ((question
        .
        "In a student council election, candidate A got 20% of the votes while candidate ...") (expected . 50) (status . completed) (time-ms . 9187)
       (correct . #f) (output . "")
       (trajectory
         .
         "006d009559a7c0c20c291431a9bb9305a35eb77b1711da094d1ff76f2a47618635"))
     ((question
        .
        "Carrie is planning the caroling schedule. The choir plans to sing \"Deck the Hall...") (expected . 540) (status . completed) (time-ms . 5189)
       (correct . #f) (output . "")
       (trajectory
         .
         "009f2d9ff9e4b597e6ee4da2c678c030618de60c45a135a5f6b29a9dee0f624371"))
     ((question
        .
        "There are 4 roses in the vase. There are 7 more dahlias than roses in the vase. ...") (expected . 15) (status . completed) (time-ms . 8436)
       (correct . #f) (output . "")
       (trajectory
         .
         "004809dd2b184ef5cff57ab823050eea89b64947899cdfc0d55ae684ccb9c08443"))
     ((question
        .
        "A car is on a road trip and drives 60 mph for 2 hours, and then 30 mph for 1 hou...") (expected . 50) (status . completed) (time-ms . 8246)
       (correct . #f) (output . "")
       (trajectory
         .
         "0042c028126b8e3ef307fed5eb0e2a0d2f3bd2b72464c8242b531f6a4d457ebe91"))
     ((question
        .
        "The tooth fairy left Sharon $5.00 in exchange for the first tooth Sharon lost.  ...") (expected . 9) (status . completed) (time-ms . 9788)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f12505f8eb4c4d52b2153599564bd7f9ec70161ad3c5b441849ad0c30745a4b6"))
     ((question
        .
        "The zookeeper feeds all the apes in the zoo. He orders all the bananas from a lo...") (expected . 1400) (status . completed) (time-ms . 5814)
       (correct . #f) (output . "")
       (trajectory
         .
         "00232b5f0c6ce3dde29114088a43ce52f0acf4fa8ea4896f36a787299c55bd1491"))
     ((question
        .
        "Charlotte went into the kitchen supply store knowing she wanted a set of pot and...") (expected . 132) (status . completed) (time-ms . 44022)
       (correct . #t) (output . "132.0")
       (trajectory
         .
         "00de1c578382bb13052b89be17ed3c0b1851d15cc0aa17ff0a95844d74f3fedf55"))
     ((question
        .
        "Andrew bakes 200 mini cinnamon rolls and 300 mini blueberry muffins. A normal ci...") (expected . 85000) (status . completed) (time-ms . 10467)
       (correct . #f) (output . "")
       (trajectory
         .
         "0045e2bfb9b5b1340d2111ff56ba23e123a33cfeb54836cd1ba92a1b1565308742"))
     ((question
        .
        "Bahati, Azibo, and Dinar each contributed to their team's 45 points. Bahati scor...") (expected . 5) (status . completed) (time-ms . 11639)
       (correct . #f) (output . "")
       (trajectory
         .
         "0066d58945aabe191a0ec7adb05315a31a5f1830ef79b73bcba2d72671fc2e7533"))
     ((question
        .
        "Jim decides to go to college to earn some more money.  It takes him 4 years to f...") (expected . 4) (status . completed) (time-ms . 131504)
       (correct . #f) (output . "6")
       (trajectory
         .
         "00a1f9f13bd2417c3af808d29e71985c44ebce1dd671ee1518a8a1bb637f08edbc"))
     ((question
        .
        "A bus has a capacity of 200 people. When it departed Chengli city, it had 20 peo...") (expected . 20) (status . exhausted) (time-ms . 116733)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00f69b8d07787aed18da62d4fa5150e10738abdc81ca1e9b7fcce471a70467e935"))
     ((question
        .
        "After scoring 14 points, Erin now has three times more points than Sara, who sco...") (expected . 18) (status . completed) (time-ms . 20851)
       (correct . #f) (output . "10")
       (trajectory
         .
         "00297f6cccb4b517e9cb7e80508ac586cf5d857be8fc5563f6ea28b019258c6d3a"))
     ((question
        .
        "A custodian has to clean a school with 80 classrooms. They have 5 days to get it...") (expected . 50) (status . exhausted) (time-ms . 122798)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00b95a265ac10742dc78f565ee35c0bc620568975c47d6565aacf4b79c17827863"))
     ((question
        .
        "John has 2 houses with 3 bedrooms each.  Each bedroom has 2 windows each.  There...") (expected . 20) (status . completed) (time-ms . 15142)
       (correct . #t) (output . "20")
       (trajectory
         .
         "00488ffbabc799710daa41c91ee29aa3180ae299c12544dd707bcf1454890789d2"))
     ((question
        .
        "Kyle bought last year's best-selling book for $19.50. This is with a 25% discoun...") (expected . 26) (status . completed) (time-ms . 6905)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ec93c3a0f35b2b26c9f6e27d943265a08a5dc783f9b9144955343f37d7766e83"))
     ((question
        .
        "In Tate’s garden pond, there are 4 male guppies, 7 female guppies, 3 male goldfi...") (expected . 5) (status . exhausted) (time-ms . 94485)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "0010ee70d10ad4879b566572af6480cb4a4cf9fa3924d881f547f1f116299151e7"))
     ((question
        .
        "Juan wants to add croissants to his bakery menu.  It takes 1/4 pound of butter t...") (expected . 7) (status . completed) (time-ms . 17830)
       (correct . #t) (output . "(* (/ 1 4) 4 7)")
       (trajectory
         .
         "00d34720f30a4c864b0f8e421fa33fd39274d944e066eff0cc3834bd8be2327012"))
     ((question
        .
        "Catherine goes to the grocery store. She buys 1 kilo of apples for $4, 2 kilos o...") (expected . 14) (status . completed) (time-ms . 6176)
       (correct . #f) (output . "")
       (trajectory
         .
         "00fdd2c228b0e31c99030081d243c4d14d92480986daa17d879b84ebc4b51b3001"))
     ((question
        .
        "A food truck only sells grilled cheeses.  They source their bread for $3.00 a lo...") (expected . 37) (status . completed) (time-ms . 25723)
       (correct . #t) (output . "37.0")
       (trajectory
         .
         "006b593589051c0ce8baf4956b064feb52783ebd425177fc5cee915c8d1c71d9d2"))
     ((question
        .
        "Tim gets a promotion that offers him a 5% raise on his $20000 a month salary.  I...") (expected . 262500) (status . completed) (time-ms . 10096)
       (correct . #f) (output . "")
       (trajectory
         .
         "006efce42368234a302cd8057795acbc359c753929d45ddc81f5740203fbde6f8c"))
     ((question
        .
        "Emily can peel 6 shrimp a minute and saute 30 shrimp in 10 minutes. How long wil...") (expected . 45) (status . completed) (time-ms . 9643)
       (correct . #f) (output . "")
       (trajectory
         .
         "002631b114dcce6f45ef84809722da17ef0bcd72151d7e15eb7310848b5d277399"))
     ((question
        .
        "Grandpa loves to eat jelly beans, but how many jelly beans he can eat depends on...") (expected . 450) (status . completed) (time-ms . 6260)
       (correct . #f) (output . "")
       (trajectory
         .
         "00bd6e9e9caac43d6e474109b5709a6d0c4701a1edafc79f0e4f06555f8c3ef0c5"))
     ((question
        .
        "A football team has 105 members.  There are twice as many players on the offense...") (expected . 30) (status . completed) (time-ms . 7116)
       (correct . #f) (output . "")
       (trajectory
         .
         "004c4a3cc605bed1d83aeb1e97335b7b88f06364a160ad79ad8516b89fc0a17af3"))
     ((question
        .
        "Rory orders 2 subs for $7.50 each, 2 bags of chips for $1.50 each and 2 cookies ...") (expected . 29) (status . exhausted) (time-ms . 123320)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00f3f53ddbd7ae43ea1677cd449651096d4ffc72e62917a47257db958909895943"))
     ((question
        .
        "A boy has 5 cards. His brother has 3 fewer cards than he has. How many cards do ...") (expected . 7) (status . completed) (time-ms . 8063)
       (correct . #f) (output . "")
       (trajectory
         .
         "00683d4a7cbf9a2d0fe645173c37d213d5f1114bed02258da2f420c0cf248bd704"))
     ((question
        .
        "Blake and Kelly are having a contest to see who can run the most in 15 minutes. ...") (expected . 80) (status . completed) (time-ms . 54177)
       (correct . #t) (output . "80")
       (trajectory
         .
         "00e0030ae31e5af1db1dbde8ab06fca1544cf22d04483ea96276f44c605785c04c"))
     ((question
        .
        "A food caterer was told to prepare gourmet hot dogs for 36 guests. While most pe...") (expected . 26) (status . completed) (time-ms . 20510)
       (correct . #f) (output . "")
       (trajectory
         .
         "005f285e7b0e063906a7af46d1ab17cb9afa1609efd7530e31a9438c42597fcacf"))
     ((question
        .
        "Craig has 2 twenty dollar bills. He buys six squirt guns for $2 each.  He also b...") (expected . 19) (status . completed) (time-ms . 19062)
       (correct . #t) (output . "19")
       (trajectory
         .
         "009a082e0e10521b19b72e192b5ea499c3232595bb61c8146d21c3a779272e4b9b"))
     ((question
        .
        "John collects garbage from 3 different apartment complexes.  The first two have ...") (expected . 1248) (status . completed) (time-ms . 34929)
       (correct . #t) (output . "1248.0")
       (trajectory
         .
         "00baba7a2d7757b38ff35d57ba8f1f18268c0ab3b46e560ace52b4871e40402e43"))
     ((question
        .
        "Bill bakes 300 rolls, 120 chocolate croissants, and 60 baguettes every day. Each...") (expected . 280) (status . completed) (time-ms . 7950)
       (correct . #f) (output . "")
       (trajectory
         .
         "00b61789da0cf5ed98dd96602683645b0135d51c9a4b483377233eae6925caeae3"))
     ((question
        .
        "Errol bought a computer, 2 monitors, and a printer for $2,400. He paid $400 less...") (expected . 300) (status . completed) (time-ms . 40524)
       (correct . #t) (output . "300")
       (trajectory
         .
         "006d4558a810d0c68f90690638959312d8e217d40d1d3a1a4867d6465df4d76421"))
     ((question
        .
        "Elise is learning to write and decides to keep re-writing the alphabet until she...") (expected . 130) (status . completed) (time-ms . 30822)
       (correct . #t) (output . "130.0")
       (trajectory
         .
         "00a5a382a917c3d55196dcf30a55e0f04873b6c44ec1cc12ae6b22713f28d50fe2"))
     ((question
        .
        "Toby is reading a book that is 45 pages long. It averages 200 words a page. Toby...") (expected . 20) (status . exhausted) (time-ms . 74365)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "004d94e58c7bfdb6a98ef89bcf2cc635698ca540eba891812300adfad45efd827d"))
     ((question
        .
        "On average Joe throws 25 punches per minute.  A fight lasts 5 rounds of 3 minute...") (expected . 375) (status . completed) (time-ms . 8424)
       (correct . #t) (output . "375")
       (trajectory
         .
         "00d9c106c349f765754cf47f4e47b56e046de66be2a86c9ab478d9a88b7bb826ee"))
     ((question
        .
        "At Ashley's school, they start a reforestation campaign where each child plants ...") (expected . 1240) (status . completed) (time-ms . 30887)
       (correct . #t) (output . "1240")
       (trajectory
         .
         "0090493fd410dc492f12be055c90fed77512df47149d52db8cb5ca281fbc66f727")))))
