(benchmark-results (model "/models/qwen3-0.6b-rlm-sft") (mode "gsm8k")
  (timestamp "2026-02-08T18:18:27Z") (n-problems 100)
  (correct 5) (accuracy 0.05) (total-ms 437613)
  (problems
    (((question
        .
        "Greta and Celinda are baking cookies. Greta bakes 30 cookies and Celinda bakes t...") (expected . 80) (status . completed) (time-ms . 4461)
       (correct . #f) (output . "140")
       (trajectory
         .
         "00b2fac11b376498added04b6ee35bd895d0b5e94d4b78340315b002ba4004723b"))
     ((question
        .
        "Tyler wants to buy a dictionary that costs $18, a dinosaur book that costs $13, ...") (expected . 5) (status . completed) (time-ms . 4107)
       (correct . #t) (output . "5")
       (trajectory
         .
         "005edfd6969009f882030d5f381a797a15449468095066b2f3e7da716765c914c0"))
     ((question
        .
        "Carlos, Jim and Carrey were at the beach playing and they decided to gather some...") (expected . 20) (status . completed) (time-ms . 3865)
       (correct . #f) (output . "78")
       (trajectory
         .
         "004146683d4876650ff1a1739d1702da0e38a6658e3a3d9bec281930ce1b413cbe"))
     ((question
        .
        "Tom buys a bedroom set for $3000.  He sells his old bedroom for $1000 and uses t...") (expected . 200) (status . completed) (time-ms . 3710)
       (correct . #f) (output . "150")
       (trajectory
         .
         "006d9a9a0b6b383e2139c5f79cd90018b3b01b0e87a8221a000370efe85d187dd9"))
     ((question
        .
        "Mike's teacher, leaves as homework the reading of a 200-page book. The assignmen...") (expected . 10) (status . completed) (time-ms . 3821)
       (correct . #f) (output . "20")
       (trajectory
         .
         "00ce88fff48137db95ca47e71c9bab3eed5c1c733f608c60a5368bb49c516e6c88"))
     ((question
        .
        "I am three years younger than my brother, and I am 2 years older than my sister....") (expected . 13) (status . completed) (time-ms . 3958)
       (correct . #f) (output . "6")
       (trajectory
         .
         "0087078f52748108c1e88932d45ce75e6a0a3c10a40b6c3f58d6423a79772a8290"))
     ((question
        .
        "Rose is out picking flowers for a vase she wants to fill.  She starts off by pic...") (expected . 79) (status . completed) (time-ms . 6399)
       (correct . #f) (output . "567")
       (trajectory
         .
         "00eb390da18ec6c8a7e86ef835f8e5b2ce09d9944b951279cd299d882e48415d2d"))
     ((question
        .
        "A phone tree is used to contact families and relatives of Ali's deceased coworke...") (expected . 81) (status . completed) (time-ms . 3633)
       (correct . #f) (output . "21")
       (trajectory
         .
         "00fde01c4a2a5aa62d381c3380e6a14bdd9d869108e1f54dd890604aae45978045"))
     ((question
        .
        "Tiffany is measuring how many surfers can ride a big wave without falling. She s...") (expected . 10) (status . completed) (time-ms . 4273)
       (correct . #f) (output . "85")
       (trajectory
         .
         "0020e043a88ca7e8590c46f0a59ab014d853338256a3b7ae42ca1ff6046b80c842"))
     ((question
        .
        "Jason was told he could earn $3.00 for doing his laundry,  $1.50 for cleaning hi...") (expected . 9) (status . completed) (time-ms . 4819)
       (correct . #f) (output . "1.5")
       (trajectory
         .
         "00b743642bd490b8e62b80bba0d7512b284150eff9f4c6db4cb1ad50a1c2966991"))
     ((question
        .
        "A news website publishes an average of 20 political and weather news articles ev...") (expected . 840) (status . completed) (time-ms . 3857)
       (correct . #t) (output . "840")
       (trajectory
         .
         "00dfd550ffb3cc188ccba30012bc81a79745f0a4366b493591805d688b470541f7"))
     ((question
        .
        "Avery needs to buy a 3 piece place setting (dinner & salad plate and a bowl) for...") (expected . 180) (status . completed) (time-ms . 4639)
       (correct . #f) (output . "19")
       (trajectory
         .
         "0058797fdf9921a6ee7a4c868ac5419da289b19d25e58a3e4381e03fcf56627180"))
     ((question
        .
        "Hannah has a mental breakdown while studying for finals and starts smashing wind...") (expected . 112) (status . completed) (time-ms . 4956)
       (correct . #f) (output . "16384")
       (trajectory
         .
         "00889f46589172807f1c7c12e17b4e773df0698643ac289436189334043dfc6a14"))
     ((question
        .
        "Adam went to a store to buy some sweets. He bought 7 candies of type A and 10 ca...") (expected . 4) (status . exhausted) (time-ms . 8979)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00c68d14d747ed30474247e77bb9288f2b4ac21aded7c16432f69489eddd326751"))
     ((question
        .
        "A $2000 watch was put on sale so that Mr. Rogers bought it at 75% of its origina...") (expected . 10) (status . completed) (time-ms . 4000)
       (correct . #f) (output . "1500")
       (trajectory
         .
         "006f75e75f9c5bda7d0c61f10717b80d6342cd640673217f1b4aa6b95ab47be24a"))
     ((question
        .
        "How much does it cost you for lunch today at Subway if you pay $40 for a foot-lo...") (expected . 160) (status . completed) (time-ms . 3870)
       (correct . #f) (output . "120")
       (trajectory
         .
         "0034e68732f613b239f50c38833ae9bce4ba543654f84f4a1c50eb5bf3deee08e5"))
     ((question
        .
        "Linus works for a trading company. He buys a mobile device for $20 and sells it ...") (expected . 120) (status . completed) (time-ms . 3963)
       (correct . #f) (output . "400")
       (trajectory
         .
         "00fbf007fbb32ea4c3f4bf48ce8fc45a09638583b8d53841511800d04e18602a25"))
     ((question
        .
        "John decides to do several activities while out on vacation.  He spends 6 hours ...") (expected . 20) (status . completed) (time-ms . 5873)
       (correct . #f) (output . "116.8")
       (trajectory
         .
         "002d8935f4a7fdf8a4d8f06ac62b19167baf80977633083376e9402c09eebe6de5"))
     ((question
        .
        "Paul is at a train station and is waiting for his train. He isn't sure how long ...") (expected . 145) (status . completed) (time-ms . 4219)
       (correct . #f) (output . "6000")
       (trajectory
         .
         "0096978114abb79c3a9dacfbea5ab376936156038f4471925ca091c9c5f470e75f"))
     ((question
        .
        "Well's mother sells watermelons, peppers, and oranges at the local store. A wate...") (expected . 880) (status . completed) (time-ms . 5113)
       (correct . #f) (output . "2700")
       (trajectory
         .
         "009b75588c9fbc1db624fcc39a88cabd68e8f0df1b523f124e79fdca89b58f0e87"))
     ((question
        .
        "Josh runs a car shop and services 3 cars a day.  He is open every day of the wee...") (expected . 120) (status . completed) (time-ms . 3861)
       (correct . #f) (output . "36")
       (trajectory
         .
         "006040e24dfe2d73d9d2b81e38ecc577b0af47907919aa6a4f97110b3809de0850"))
     ((question
        .
        "In the first week, Judy read for 15 minutes each night before going to sleep. In...") (expected . 240) (status . completed) (time-ms . 3777)
       (correct . #f) (output . "200")
       (trajectory
         .
         "00c1e7380fd3683f387466c7fb2e4fede8d8a4080edde7a592c0b3e3061579e894"))
     ((question
        .
        "James runs 12 miles a day for 5 days a week.  If he runs 10 miles an hour how ma...") (expected . 6) (status . completed) (time-ms . 3861)
       (correct . #f) (output . "100")
       (trajectory
         .
         "008cbf78ddfb1fed4918977016cd458ae6052d9f5eee01e541112623dc0ce9057a"))
     ((question
        .
        "The vending machines sell chips for 40 cents and candy bars for 75 cents. George...") (expected . 5) (status . completed) (time-ms . 3911)
       (correct . #f) (output . "666.27")
       (trajectory
         .
         "00324d1ee0b517ad0e1e6c636dd83ec4650a34f76e2b0df1b61d1019b583087c8a"))
     ((question
        .
        "John buys 2 pairs of shoes for each of his 3 children.  They cost $60 each.  How...") (expected . 360) (status . completed) (time-ms . 4750)
       (correct . #f) (output . "120")
       (trajectory
         .
         "003223fe6a48acc1482dfc53ad33ff095dc88960fe9503e38bc6e2e279952cec05"))
     ((question
        .
        "After transferring to a new school, Amy made 20 more friends than Lily. If Lily ...") (expected . 120) (status . completed) (time-ms . 3936)
       (correct . #f) (output . "30")
       (trajectory
         .
         "0011c42e7582a7ff115e304237843986a870c877d6cb91a694b1a8dc827d0e45f2"))
     ((question
        .
        "Ashley has an internet connection speed of 20kb per second. Knowing that 1 Mb ha...") (expected . 72) (status . completed) (time-ms . 3782)
       (correct . #f) (output . "1020")
       (trajectory
         .
         "0056c7603130c5132c4717705b2db2831d5a03090ca01f42ad145a21316e51e336"))
     ((question
        .
        "Tobias, Chikote, and Igneous are the three little wolves who live in the forest ...") (expected . 2) (status . completed) (time-ms . 3658)
       (correct . #f) (output . "40")
       (trajectory
         .
         "0025f19ce518a7d638ca538cd5b2ac6ab5e42ba6f9ded2cfe4723e11da6744fa9e"))
     ((question
        .
        "Melanie's father opens up an animal farm starting with 50 cows and 20 chickens. ...") (expected . 700) (status . completed) (time-ms . 4369)
       (correct . #f) (output . "2000")
       (trajectory
         .
         "0067da66515e5da67ac61b29767b7127beefdd468dfd5c43e3f4bd10096a1305ad"))
     ((question
        .
        "Shawnda decides that her neighborhood kids could really use a bike inflation ser...") (expected . 5) (status . completed) (time-ms . 4122)
       (correct . #f) (output . "125")
       (trajectory
         .
         "003f632b3ddfa765649be65e286497fabc2678aebbfe83852133ed3129f9b7e388"))
     ((question
        .
        "There are three trees in Eddy's backyard. The shortest tree has a height of 6 fe...") (expected . 34) (status . completed) (time-ms . 3744)
       (correct . #f) (output . "22")
       (trajectory
         .
         "001b91ff2f79e7dbfd26eeac3368bf79e5b9c053e4b3a63ad4dc7ee695555500e1"))
     ((question
        .
        "Andrea has 8 more apples than Jamal and half as many bananas as Jamal. Jamal has...") (expected . 168) (status . completed) (time-ms . 3692)
       (correct . #f) (output . "24")
       (trajectory
         .
         "0000354eef80fb5ba95ee9c14f961e69a119b0be85236d229d139eed0f74e40628"))
     ((question
        .
        "There are 10000 gallons of water in a pool. Using a water pump, Anthony and his ...") (expected . 2000) (status . completed) (time-ms . 3789)
       (correct . #f) (output . "5000")
       (trajectory
         .
         "00b806c1a62d6ddec395cd3a5da4234f414ea80ac77e806d0c5d57176b6fc31b9f"))
     ((question
        .
        "Luke is spending time at the beach building sandcastles. He eventually notices t...") (expected . 60) (status . completed) (time-ms . 3791)
       (correct . #f) (output . "32")
       (trajectory
         .
         "005eaae116bde9ced2ae2dcc8cf07c9e201c24a10ad2d620c3ddba338b61c60d0d"))
     ((question
        .
        "A factory used to make tractors, but now makes silos. When they made tractors, t...") (expected . 10) (status . completed) (time-ms . 3962)
       (correct . #f) (output . "320")
       (trajectory
         .
         "00ea488bfdb37670ba451adda3d1f000ac4388079951a9d2a0a99195c7da05a477"))
     ((question
        .
        "A bakery has 40 less than seven times as many loaves of bread as Sam had last Fr...") (expected . 450) (status . completed) (time-ms . 5934)
       (correct . #f) (output . "280")
       (trajectory
         .
         "005cea74010224547941e9aa70edf751ab5b7f0ebb725b939d859e4a0dd8dd65de"))
     ((question
        .
        "Shiloh is 44 years old today.  In 7 years, he will be three times as old as his ...") (expected . 10) (status . completed) (time-ms . 5210)
       (correct . #f) (output . "2")
       (trajectory
         .
         "003797ee8f3e7ab1a1dad5fe09c03e48bc43d602d53c5b51592eff9a240e278a07"))
     ((question
        .
        "Rosie can run 10 miles per hour for 3 hours. After that, she runs 5 miles per ho...") (expected . 50) (status . completed) (time-ms . 3871)
       (correct . #f) (output . "10")
       (trajectory
         .
         "002faa053201c44149da83e7e13c363cd4bfbac5946c2d1ef5103cbd3050e33331"))
     ((question
        .
        "Four children are playing together—Akbar, Alessandro, Helene, and Wilfred. Helen...") (expected . 3) (status . completed) (time-ms . 14851)
       (correct . #f) (output . "80")
       (trajectory
         .
         "007b89fcc9bce051e0477aa579786360bd788e64ae643de27251789d54a4c54e90"))
     ((question
        .
        "Max plans to watch two movies this weekend. The first movie is 1 hour and 30 min...") (expected . 215) (status . completed) (time-ms . 4213)
       (correct . #f) (output . "600")
       (trajectory
         .
         "00bb6d0795707fbfcafdb7663e7616bd23deabc6f42fb93f646714573962798557"))
     ((question
        .
        "A farmer has 900 eggs. He placed them on a tray, which holds 30 eggs each. How m...") (expected . 75) (status . completed) (time-ms . 3813)
       (correct . #f) (output . "825")
       (trajectory
         .
         "0032be1dfd0949532dc83c2f82f59df10dbf050bcb4a36bff661e6ed1a1452c265"))
     ((question
        .
        "Lauren is a cartoonist.  She can draw 5 large-sized picture scenes per day, or s...") (expected . 22) (status . completed) (time-ms . 5990)
       (correct . #t) (output . "22")
       (trajectory
         .
         "0069e970a9e5e992edad04c9d16a60981dea8016f999f26b4381fc30aacfc7400f"))
     ((question
        .
        "The great dragon, Perg, sat high atop mount Farbo, breathing fire upon anything ...") (expected . 200) (status . completed) (time-ms . 3710)
       (correct . #f) (output . "1200")
       (trajectory
         .
         "00f77c6ed9d37eaf3944094de5f8d9d225712565b02b61b4282599dcbae5401a16"))
     ((question
        .
        "Fred was preparing for a party to be held in four days.  So, he made 24 gallons ...") (expected . 2) (status . completed) (time-ms . 4167)
       (correct . #f) (output . "102")
       (trajectory
         .
         "000ffd28d0606b0f13862bec918c02a5870214867b878121a0690fd99f813ed8c5"))
     ((question
        .
        "To make 1 liter of juice, Sam needs 5 kilograms of oranges. Each kilogram of ora...") (expected . 60) (status . completed) (time-ms . 3652)
       (correct . #f) (output . "20")
       (trajectory
         .
         "003b1838905b639f8562783d16b23aa41ed70c97be319d5b062cfc23246a46df04"))
     ((question
        .
        "Terri is knitting a sweater with two sleeves, a collar, and a decorative rosette...") (expected . 315) (status . completed) (time-ms . 4077)
       (correct . #f) (output . "8700")
       (trajectory
         .
         "008ae9bf4af5c6257ec4610a6b551da11ceae1672e367d2c8b780ff3f7558854c0"))
     ((question
        .
        "On Easter Sunday Cindy went to the city park to participate in the Easter Egg Hu...") (expected . 27) (status . completed) (time-ms . 3769)
       (correct . #f) (output . "60")
       (trajectory
         .
         "00ba60d8d4e070b568cbbd7ec5ebf0e68577f09276a577856c2724b1b1dd5d16f3"))
     ((question
        .
        "Kimberly bought 8 packages of cat food and 6 packages of dog food. Each package ...") (expected . 52) (status . completed) (time-ms . 4505)
       (correct . #f) (output . "238")
       (trajectory
         .
         "009c8d9cdfed052eed276fa0caac58b4cfbbffa293e8cd97c115d4adeb58052ba4"))
     ((question
        .
        "Helga was the fastest clog dancer in all of Slovenia. With both hands at her sid...") (expected . 2450) (status . completed) (time-ms . 4051)
       (correct . #f) (output . "750")
       (trajectory
         .
         "00461b11250dc2860a20307de7ac04f6ddbe6c1e73ffaffc3b17c4b0f5cbc98c6b"))
     ((question
        .
        "Mr. Ruther sold 3/5 of his land and had 12.8 hectares left. How much land did he...") (expected . 32) (status . completed) (time-ms . 4077)
       (correct . #f) (output . "102.4")
       (trajectory
         .
         "00517531515ffc4505110ffe39055603930baa5b333056b43352dd7110b55965d1"))
     ((question
        .
        "Peter wants to make different sized ice cubes with 32 ounces of water. He can ma...") (expected . 2) (status . completed) (time-ms . 4423)
       (correct . #f) (output . "53")
       (trajectory
         .
         "00c24e69cb701f51d8b8c2c267d4049230596f57f752c8e265e6da0db7a4125931"))
     ((question
        .
        "Bailey starts with a certain amount of money. Then she receives a weekly allowan...") (expected . 60) (status . completed) (time-ms . 3716)
       (correct . #f) (output . "100")
       (trajectory
         .
         "0019f86376dd15f0f1a0bba934aa5dcdf1ba040d8e5a723648778bec87591c6956"))
     ((question
        .
        "Carla just gave birth to identical octuplets. She dresses 3/4 of them in purple ...") (expected . 50) (status . completed) (time-ms . 4080)
       (correct . #f) (output . "91.6666666667")
       (trajectory
         .
         "0007a83db762a59958fe84935e2fb2d2455f1ed67f614b1c57f79defb974030499"))
     ((question
        .
        "Theo bought a pen for $2 and a piece of paper that cost $1 less than three times...") (expected . 3) (status . completed) (time-ms . 6000)
       (correct . #f) (output . "8")
       (trajectory
         .
         "0026fa13eb5b2e4d3e9c74bf686c86bd0da6b5b2cf8e0df6ca5bb5ec74452c0529"))
     ((question
        .
        "A chef bought 4 bags of onions. Each bag weighs 50 pounds. A pound of onions cos...") (expected . 300) (status . completed) (time-ms . 3877)
       (correct . #f) (output . "275")
       (trajectory
         .
         "00868d24f4e71556dad6ba281e81c6dca7cc6f2ad9bc128ea2d1fd96b186b806fd"))
     ((question
        .
        "Mary is making ice cubes with fruit frozen in them for a cocktail party. She mak...") (expected . 96) (status . completed) (time-ms . 3883)
       (correct . #f) (output . "316")
       (trajectory
         .
         "00e77c50ada579ff982dc1cc91224f97e7e3a974d73811dfa676d4de961e47b739"))
     ((question
        .
        "Rani is obsessed with sports cars. She wonders what the faster car ever made can...") (expected . 750) (status . completed) (time-ms . 4444)
       (correct . #f) (output . "600")
       (trajectory
         .
         "007443ad6c5316453462a923bc51b67cacf6329743a6a62442d5ed25bd4553b62d"))
     ((question
        .
        "In a grocery store, four apples cost $5.20, and three oranges cost $3.30. How mu...") (expected . 12) (status . completed) (time-ms . 5006)
       (correct . #f) (output . "71.04")
       (trajectory
         .
         "00a70f5b589c75a59f846a9c6c1dbcabbe8db7979c17be86c49383314d8e409012"))
     ((question
        .
        "Keegan was running a car wash with his friend Tashay to raise money for a baseba...") (expected . 26) (status . completed) (time-ms . 4018)
       (correct . #f) (output . "249")
       (trajectory
         .
         "006fe83a5f9cb9b9d5c6bfe90c759dc9f3cc530ee9f3a3ec548d457cb91ac32de0"))
     ((question
        .
        "Royce takes 40 minutes more than double Rob to shingle a house. If Rob takes 2 h...") (expected . 280) (status . completed) (time-ms . 3883)
       (correct . #f) (output . "460")
       (trajectory
         .
         "00908bd77b7a555131dc70b1e53b7faa903d41d30dab476d1a730b6c4658682fc2"))
     ((question
        .
        "There are 36 penguins sunbathing in the snow.  One-third of them jump in and swi...") (expected . 12) (status . completed) (time-ms . 4712)
       (correct . #f) (output . "3")
       (trajectory
         .
         "00adea65a5fe8b3862bd9e8a356bc40d562a38d0e60e28324a2144a5ddb692850c"))
     ((question
        .
        "Harold sleeps for 10 hours a night.  He works 2 hours less than he sleeps and he...") (expected . 5) (status . completed) (time-ms . 3674)
       (correct . #f) (output . "4")
       (trajectory
         .
         "000790709d0742127443785c5a4e797cf220c44b2c1d42d42209364e0b6cfd21d1"))
     ((question
        .
        "A train has 172 people traveling on it. At the first stop 47 people get off and ...") (expected . 100) (status . completed) (time-ms . 3828)
       (correct . #f) (output . "87")
       (trajectory
         .
         "00409a48852c6290ca9efcebd56219a89e6be807ab92d08ca3c3525126f9985726"))
     ((question
        .
        "Together Lily, David, and Bodhi collected 43 insects. Lily found 7 more than Dav...") (expected . 16) (status . completed) (time-ms . 5748)
       (correct . #f) (output . "86")
       (trajectory
         .
         "004c15582f5700f22a9e76df146aff5bcc911d7f29a5620a441839d664a052d4da"))
     ((question
        .
        "The educational shop is selling notebooks for $1.50 each and a ballpen at $0.5 e...") (expected . 8) (status . completed) (time-ms . 4459)
       (correct . #f) (output . "9.0")
       (trajectory
         .
         "0050bc390d28f8519d019e12f07646edb26173b374ef4ed7c8072fdb1f133d900d"))
     ((question
        .
        "In a student council election, candidate A got 20% of the votes while candidate ...") (expected . 50) (status . completed) (time-ms . 3958)
       (correct . #f) (output . "99.8")
       (trajectory
         .
         "006c9e6cf1ac96c340e29d482cb85bfcb5fd3e632e413c6856a8db5be7b5d64d6e"))
     ((question
        .
        "Carrie is planning the caroling schedule. The choir plans to sing \"Deck the Hall...") (expected . 540) (status . completed) (time-ms . 4052)
       (correct . #f) (output . "930")
       (trajectory
         .
         "00ab00cb14196ab3e503912d005d9e9772a5bcc30a33601d10a167e0756a3fb39a"))
     ((question
        .
        "There are 4 roses in the vase. There are 7 more dahlias than roses in the vase. ...") (expected . 15) (status . completed) (time-ms . 3703)
       (correct . #f) (output . "56")
       (trajectory
         .
         "00522929bd70a44a62a01a4365d58ce6c7c8fbfa8ebf935befa87b06ba85dd12d3"))
     ((question
        .
        "A car is on a road trip and drives 60 mph for 2 hours, and then 30 mph for 1 hou...") (expected . 50) (status . completed) (time-ms . 5594)
       (correct . #f) (output . "360")
       (trajectory
         .
         "006d8ae49bb0329114160f26682e8316eedbc43969481ced9dc55b594d326faef1"))
     ((question
        .
        "The tooth fairy left Sharon $5.00 in exchange for the first tooth Sharon lost.  ...") (expected . 9) (status . completed) (time-ms . 3952)
       (correct . #f) (output . "20")
       (trajectory
         .
         "00106463ab1472e8257a794c136befaa12ebeae7d156952e00896f86a1cb178641"))
     ((question
        .
        "The zookeeper feeds all the apes in the zoo. He orders all the bananas from a lo...") (expected . 1400) (status . completed) (time-ms . 4995)
       (correct . #t) (output . "1400")
       (trajectory
         .
         "00752b4d4ee43fbd524a611c08ea9af038854167dfed0187167b9d311402d426a7"))
     ((question
        .
        "Charlotte went into the kitchen supply store knowing she wanted a set of pot and...") (expected . 132) (status . completed) (time-ms . 4040)
       (correct . #f) (output . "560")
       (trajectory
         .
         "001bc715268f55541441f1edc29809960b181307661855709a7b00705e44573024"))
     ((question
        .
        "Andrew bakes 200 mini cinnamon rolls and 300 mini blueberry muffins. A normal ci...") (expected . 85000) (status . completed) (time-ms . 4092)
       (correct . #f) (output . "255000")
       (trajectory
         .
         "004ccfca233c5240b78ae2e704afe0ca119e5e98d5fe96caf4a761daf4c80e507c"))
     ((question
        .
        "Bahati, Azibo, and Dinar each contributed to their team's 45 points. Bahati scor...") (expected . 5) (status . completed) (time-ms . 3696)
       (correct . #f) (output . "35")
       (trajectory
         .
         "0075a8577621315942961f9ae6fdfb9602a7b8c64378d90791e0259300b6efacd3"))
     ((question
        .
        "Jim decides to go to college to earn some more money.  It takes him 4 years to f...") (expected . 4) (status . completed) (time-ms . 3839)
       (correct . #f) (output . "600000")
       (trajectory
         .
         "0026a88c0c0898f54d6a43ada61cb62598b8dad5a0ae3498c9b2a475f27731bdd3"))
     ((question
        .
        "A bus has a capacity of 200 people. When it departed Chengli city, it had 20 peo...") (expected . 20) (status . completed) (time-ms . 3810)
       (correct . #f) (output . "160")
       (trajectory
         .
         "00e5a78f7d3db7e87366533627300a18d8110f3b760c92e92f1623e47d0a9f310f"))
     ((question
        .
        "After scoring 14 points, Erin now has three times more points than Sara, who sco...") (expected . 18) (status . completed) (time-ms . 6226)
       (correct . #f) (output . "42")
       (trajectory
         .
         "000d5e17d233caff6c561bd337b78abf0b50120f85906fca00d276a4d24371f0ca"))
     ((question
        .
        "A custodian has to clean a school with 80 classrooms. They have 5 days to get it...") (expected . 50) (status . completed) (time-ms . 4024)
       (correct . #f) (output . "8.95")
       (trajectory
         .
         "006f886e26192c0d38ec1a73bcfa9e2c7739e1cf4dabd1421fbcf02da2c7c5a4e1"))
     ((question
        .
        "John has 2 houses with 3 bedrooms each.  Each bedroom has 2 windows each.  There...") (expected . 20) (status . completed) (time-ms . 3830)
       (correct . #f) (output . "3")
       (trajectory
         .
         "002bbb4ffce7c37e09ee45bc4c45b7c52b5259ff2321dd7404ae8a9dd4442663aa"))
     ((question
        .
        "Kyle bought last year's best-selling book for $19.50. This is with a 25% discoun...") (expected . 26) (status . completed) (time-ms . 3839)
       (correct . #f) (output . "20.25")
       (trajectory
         .
         "00eb4c44568d341b9538e3aaee6192737d9643826268b665c7bd5ba9e4a42d06f3"))
     ((question
        .
        "In Tate’s garden pond, there are 4 male guppies, 7 female guppies, 3 male goldfi...") (expected . 5) (status . completed) (time-ms . 3700)
       (correct . #f) (output . "13")
       (trajectory
         .
         "00565cbe7d1348d153c86b438486115a9268fb5cb8b8ef9286089822be5ed7ec1e"))
     ((question
        .
        "Juan wants to add croissants to his bakery menu.  It takes 1/4 pound of butter t...") (expected . 7) (status . completed) (time-ms . 4793)
       (correct . #t) (output . "7.0")
       (trajectory
         .
         "00c1c70faa6c4b9ba2d90f95cde909a99f3e332c9b49d27785c2e8fbe5dec195ba"))
     ((question
        .
        "Catherine goes to the grocery store. She buys 1 kilo of apples for $4, 2 kilos o...") (expected . 14) (status . completed) (time-ms . 4149)
       (correct . #f) (output . "5")
       (trajectory
         .
         "0042214d254d4990047e2483b14cf5d9e85a41ab668e7dc8ee9419c77b839d5b90"))
     ((question
        .
        "A food truck only sells grilled cheeses.  They source their bread for $3.00 a lo...") (expected . 37) (status . completed) (time-ms . 3721)
       (correct . #f) (output . "660")
       (trajectory
         .
         "00b97bf9ea61aa7cd347877a7b4bdf1c4298716a1a011f351409e017c787cc3186"))
     ((question
        .
        "Tim gets a promotion that offers him a 5% raise on his $20000 a month salary.  I...") (expected . 262500) (status . completed) (time-ms . 3922)
       (correct . #f) (output . "11000")
       (trajectory
         .
         "000cab564d88e155c96c1d1567cc6090d207a73bd45a4b33f4bdd68a5483c14416"))
     ((question
        .
        "Emily can peel 6 shrimp a minute and saute 30 shrimp in 10 minutes. How long wil...") (expected . 45) (status . completed) (time-ms . 4172)
       (correct . #f) (output . "900")
       (trajectory
         .
         "0064463fad12e9ebffd9a6545937e78e49ca86c899d0c7768048e54aff3db18234"))
     ((question
        .
        "Grandpa loves to eat jelly beans, but how many jelly beans he can eat depends on...") (expected . 450) (status . completed) (time-ms . 3895)
       (correct . #f) (output . "75")
       (trajectory
         .
         "00849daafea7ca744df4f6921eac6cc57a6f864ce15c3aec60b6df44fcc18ec710"))
     ((question
        .
        "A football team has 105 members.  There are twice as many players on the offense...") (expected . 30) (status . completed) (time-ms . 3821)
       (correct . #f) (output . "155")
       (trajectory
         .
         "009824da7f6f7b93b2724ccd1b782d9eb310a68d363c1b6e69f7009a1fd346b007"))
     ((question
        .
        "Rory orders 2 subs for $7.50 each, 2 bags of chips for $1.50 each and 2 cookies ...") (expected . 29) (status . completed) (time-ms . 4908)
       (correct . #f) (output . "30")
       (trajectory
         .
         "006d35dd3094a18fb0c113e688cba819f88b43b706918c9e47327a5d2bf0194ea0"))
     ((question
        .
        "A boy has 5 cards. His brother has 3 fewer cards than he has. How many cards do ...") (expected . 7) (status . completed) (time-ms . 3620)
       (correct . #f) (output . "8")
       (trajectory
         .
         "00d365b54baab18a971b4f8eac36f7289d533242054d0fe4495079498466b5d1a3"))
     ((question
        .
        "Blake and Kelly are having a contest to see who can run the most in 15 minutes. ...") (expected . 80) (status . completed) (time-ms . 4081)
       (correct . #f) (output . "300")
       (trajectory
         .
         "0071038356b2f6bdfd8a482ed31fdb693aba6135bf94722c2ebfabb545a3ff6469"))
     ((question
        .
        "A food caterer was told to prepare gourmet hot dogs for 36 guests. While most pe...") (expected . 26) (status . completed) (time-ms . 3709)
       (correct . #f) (output . "4")
       (trajectory
         .
         "00747b1bca3640f28ddb9e66685fd81d52519180deac885bebe9dddb9180762dab"))
     ((question
        .
        "Craig has 2 twenty dollar bills. He buys six squirt guns for $2 each.  He also b...") (expected . 19) (status . completed) (time-ms . 3888)
       (correct . #f) (output . "21")
       (trajectory
         .
         "0006edbb441ff70bf05182f8077a2ab85b0017133a9f5d33a17a44dbd1d89e8f7a"))
     ((question
        .
        "John collects garbage from 3 different apartment complexes.  The first two have ...") (expected . 1248) (status . completed) (time-ms . 4255)
       (correct . #f) (output . "2400")
       (trajectory
         .
         "0067eef0377023a7209e2db52ae761af0315a8975a6a10e394520b69e52d3e11fe"))
     ((question
        .
        "Bill bakes 300 rolls, 120 chocolate croissants, and 60 baguettes every day. Each...") (expected . 280) (status . completed) (time-ms . 3720)
       (correct . #f) (output . "36")
       (trajectory
         .
         "005a77aff259be03075eccfc97c984df886d44cd0b00d5b70e2727dc170738472f"))
     ((question
        .
        "Errol bought a computer, 2 monitors, and a printer for $2,400. He paid $400 less...") (expected . 300) (status . completed) (time-ms . 3822)
       (correct . #f) (output . "2000")
       (trajectory
         .
         "00b51698e41d3974bd84226f7ed895e1008bc7938a1d94f32cd6b47c4e5b4ba0c6"))
     ((question
        .
        "Elise is learning to write and decides to keep re-writing the alphabet until she...") (expected . 130) (status . completed) (time-ms . 4271)
       (correct . #f) (output . "50")
       (trajectory
         .
         "007a1fed40aa6f2979dcc31ed4e15bb4d7a33f9b18de2848196e3a80fdb823bbc0"))
     ((question
        .
        "Toby is reading a book that is 45 pages long. It averages 200 words a page. Toby...") (expected . 20) (status . completed) (time-ms . 4198)
       (correct . #f) (output . "54000")
       (trajectory
         .
         "004c245d786c5f2a0f012e385989e80175dfb92231f4114a2a9d84c7952046308a"))
     ((question
        .
        "On average Joe throws 25 punches per minute.  A fight lasts 5 rounds of 3 minute...") (expected . 375) (status . completed) (time-ms . 4128)
       (correct . #f) (output . "170")
       (trajectory
         .
         "0079e97e626a4a9433cc638dc466be72fdea6d431cc62e8823526833433ee4614a"))
     ((question
        .
        "At Ashley's school, they start a reforestation campaign where each child plants ...") (expected . 1240) (status . completed) (time-ms . 4029)
       (correct . #f) (output . "6000")
       (trajectory
         .
         "000d49c1f357f3d9a31de5afc630b79857a0643f29de6afd6338c34e0afb6b6eff")))))
