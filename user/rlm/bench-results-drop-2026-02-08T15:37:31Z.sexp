(benchmark-results (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (mode "drop") (timestamp "2026-02-08T15:37:31Z")
  (n-problems 100) (correct 15) (accuracy 0.15)
  (total-ms 2382560)
  (problems
    (((question
        .
        "How many people are there per 2 square miles?") (expected . "248") (status . completed) (time-ms . 17760)
       (correct . #f) (output . "596")
       (trajectory
         .
         "00fd62395d6a2bd1c41f0b6caf2a386b2c856d09e9096cfae36897e2b898d9e46d"))
     ((question
        .
        "How many models ended their tenure with Barkers Beauties in the year 2000?") (expected . "2") (status . completed) (time-ms . 22517)
       (correct . #t) (output . "2")
       (trajectory
         .
         "00da25e52da6f75033a91e19427554f4cbe9ef3090217e9c177811b4a34737cc11"))
     ((question
        .
        "Of the active military personnel in May 2016, how many are either military specialists or civil servants?") (expected . "857") (status . completed) (time-ms . 29105)
       (correct . #t) (output . "857")
       (trajectory
         .
         "001acf1b50cc21d374d8a75f5a656e1e91bc892a1790cb8b15f198a8b9ce9597ac"))
     ((question
        .
        "By how many people did the Venetian population rise by 1708 compared to pre-war population?") (expected . "50000") (status . completed) (time-ms . 26584)
       (correct . #t) (output . "50000")
       (trajectory
         .
         "00ec749810fffd1575cfeb1373bbc8fda074062addb143b84392bfdfdef0f0079d"))
     ((question
        .
        "How many people living in Bolivia with HIV/AIDS in 2009 did not die from it?") (expected . "11000") (status . exhausted) (time-ms . 70226)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "001be1012c05ab2dba8dd8be9ebe96918802fda34ff38f97ec85d53ca7713f4b6d"))
     ((question
        .
        "How many more Serbian army members where there than how many Sahin Bey entered Serbia with?") (expected . "10000") (status . completed) (time-ms . 24802)
       (correct . #t) (output . "10000")
       (trajectory
         .
         "006ea93e5729eebdb52eee4dd747b6d7c80510d4df09054328be6b6ee5b7f21fb5"))
     ((question
        .
        "How many more housing units are there than households?") (expected . "5030") (status . completed) (time-ms . 18339)
       (correct . #t) (output . "5030")
       (trajectory
         .
         "00c260153113831150d539524017257f07ca68ce64e4b523762a05849dcffd1561"))
     ((question
        .
        "How many times did Detroit score in the first quarter?") (expected . "2") (status . completed) (time-ms . 19532)
       (correct . #t) (output . "2")
       (trajectory
         .
         "00f989c8810425e585dccea2eb2973b477bfa6bd325d26161a7bf5f1538431d0fe"))
     ((question
        .
        "How many ethnic groups resettled the depopulated Circassian lands?") (expected . "3") (status . completed) (time-ms . 26230)
       (correct . #f) (output . "5")
       (trajectory
         .
         "00eb39f22d49c3b715a064d209ba72ababd04696dc19eff509b9a81dbf74046eb6"))
     ((question
        .
        "How many days after arriving in Constantinople did it take Polin  to arrive back in Toulon?") (expected . "53") (status . completed) (time-ms . 21756)
       (correct . #t) (output . "53")
       (trajectory
         .
         "009ca39a55ee5db7664871457dab346448a7a7e9d5276117906309be115a52676c"))
     ((question
        .
        "How many touchdowns were scored in the first half?") (expected . "3") (status . completed) (time-ms . 20105)
       (correct . #f) (output . "2")
       (trajectory
         .
         "007eaa3f2e9032fe46836ab74bc22fdeffe535ad4b63c473b3bde06822974d6280"))
     ((question
        .
        "How many degrees Celsius difference, is the average temperature in January in Pakse compared to the average temperature in April, in Vientiane?") (expected . "15.5") (status . completed) (time-ms . 24332)
       (correct . #t) (output . "15.5")
       (trajectory
         .
         "00895aec7f335bb67bf22f0944d15cad423f0bd68ccf92d1ad2e86411c1a4576ce"))
     ((question
        .
        "How many more households are there than families?") (expected . "915") (status . completed) (time-ms . 18313)
       (correct . #f) (output . "270")
       (trajectory
         .
         "005e4cf69779cd38179e0baf175aa0ede2d1f1c7ab21f4e85826e616969536b127"))
     ((question
        .
        "How many years after the 5 October revolution was the Constitution approved?") (expected . "11") (status . completed) (time-ms . 27276)
       (correct . #f) (output . "4")
       (trajectory
         .
         "006827f1536dea8b822abc7ae83c95d8056fe00eada6ce2244fa0ba4bbdbd0a4a1"))
     ((question
        .
        "How many field goals did Graham kick in the second half?") (expected . "1") (status . completed) (time-ms . 43757)
       (correct . #t) (output . "1")
       (trajectory
         .
         "002e099994576dc59d0456c355a05927aaef9c8dcbfe2e02bc05ebfa38468f9588"))
     ((question . "When did Švitrigaila die?") (expected . "1452") (status . completed) (time-ms . 27213)
       (correct . #t) (output . "1452")
       (trajectory
         .
         "00a282279d3ef05fbba094a3f1792e35943d6185185afc0a5255f4a568de71cd8c"))
     ((question
        .
        "How many more touchdowns did Barber score compared to Romo?") (expected . "3") (status . completed) (time-ms . 27600)
       (correct . #f) (output . "1")
       (trajectory
         .
         "00250585d72458e8d9c76d3143e8e2c349e02866d4f94aeafb88c9b8fa08a3ed91"))
     ((question
        .
        "How many more touchdowns did Neil ODonnell have compared to interceptions?") (expected . "10") (status . completed) (time-ms . 19641)
       (correct . #t) (output . "10")
       (trajectory
         .
         "005522d68f76cfbfb3a405e27943ce040e847d5eb39e6be9b21f6b5d629d069abc"))
     ((question
        .
        "How many days did it take the Ente to approve the Foch Line?") (expected . "8") (status . completed) (time-ms . 20911)
       (correct . #t) (output . "8")
       (trajectory
         .
         "00723a91de1a4c977debb2646c48d967df69a5d4ec3a76a43b77e71131ba9e338e"))
     ((question
        .
        "How many months after the 10-year-10-month-and-10-day agreement was reached did it take for Edward to depart Acre for Sicily?") (expected . "4") (status . completed) (time-ms . 16878)
       (correct . #t) (output . "4")
       (trajectory
         .
         "006e175e6080760ca7100a14f4eaa5b0e12914b676f300d151ace9828956f9fc14"))
     ((question
        .
        "How many more cases of heart disease are predicted to be seen in Texas in 2030 compared to 2010?") (expected . "1389257") (status . completed)
       (time-ms . 33802) (correct . #f) (output . "4426828")
       (trajectory
         .
         "00d9dc5a1bd5d1f78fcf914df1d858c9f8c91fe4954672de8f2f46621456b5c1bc"))
     ((question
        .
        "How many in percent from the census weren't from two or more races?") (expected . "99.26") (status . completed) (time-ms . 32602)
       (correct . #t) (output . "99.26")
       (trajectory
         .
         "00cefe9e012064598f71cf240e32772815683eae4423fc86a6906e06daf02376d4"))
     ((question
        .
        "How many months did the Republic of Aras exist?") (expected . "6") (status . completed) (time-ms . 30897)
       (correct . #f)
       (output
         .
         "(I have 97 volvo xc90 (which I'm selling). My aunt has 88 audi a4. My boss has 68 children's books. I also have 80 fantasy books (which are on my bookshelf). I also have 74 tables (which are handcrafted). I also have 84 cherries(here's how I ended up with 84 of them: initially I had 81, then I lost 5 of them leaving me with 76, then I lost 5 of them leaving me with 71, then I lost 16 of them leaving me with 55, then I got 14 more making it 69, then I got 15 more making it 84). I also have 94 digital cameras. I also have 96 nightstands (which match the decor). I also have 65 beds (which match the decor). My sister has 60 sci-fi books. My aunt also has 72 plums. I also have 94 reference books (which are on my bookshelf). I also have 93 breadfruits(here's how I ended up with 93 of them: initially I had 86, then I got 7 more making it 93). I also have 72 dining tables (which are handcrafted). I also have 97 peaches (which I bought yesterday). I also have 75 nissan armada(here's how I ended up with 75 of them: initially I had 95, then I lost 32 of them leaving me with 63, then I got 12 more making it 75). My mother has 58 graphic novels. I also have 85 hamsters. My grandmother has 55 coffee tables. My grandfather has 95 smartphones. I also have 76 jonagold apples. I also have 61 mystery novels (which I borrowed from the library). My professor has 73 reference books. My friend has 82 stools. My professor also has 100 pineapples. I also have 71 smart TVs. I also have 97 hard drives(here's how I ended up with 97 of them: initially I had 83, then I lost 18 of them leaving me with 65, then I got 17 more making it 82, then I got 15 more making it 97). My sister also has 74 headphones. My mother also has 61 mystery novels. I also have 74 bookshelves (which need assembly). I also have 68 headphones (which were on sale). My colleague has 56 iguanas. My grandmother also has 65 persimmons. I also have 73 kia )")
       (trajectory
         .
         "0063eb694a096f8e863b32674d2018f526516d16a69f4b3db15fde22b61f9253ec"))
     ((question . "How many percent were not from 25 to 44?") (expected . "84.7") (status . completed) (time-ms . 9920)
       (correct . #f) (output . "()")
       (trajectory
         .
         "00a28958c9f53357957a808f45207a81fdac7408e4861416ab1bdc4c3a075330fd"))
     ((question
        .
        "How many field goals did Sebastian Janikowski kick?") (expected . "2") (status . completed) (time-ms . 39147)
       (correct . #f)
       (output
         .
         "(I have 97 volvo xc90 (which I'm selling). My aunt has 88 audi a4. My boss has 68 children's books. I also have 80 fantasy books (which are on my bookshelf). I also have 74 tables (which are handcrafted). I also have 84 cherries(here's how I ended up with 84 of them: initially I had 81, then I lost 5 of them leaving me with 76, then I lost 5 of them leaving me with 71, then I lost 16 of them leaving me with 55, then I got 14 more making it 69, then I got 15 more making it 84). I also have 94 digital cameras. I also have 96 nightstands (which match the decor). I also have 65 beds (which match the decor). My sister has 60 sci-fi books. My aunt also has 72 plums. I also have 94 reference books (which are on my bookshelf). I also have 93 breadfruits(here's how I ended up with 93 of them: initially I had 86, then I got 7 more making it 93). I also have 72 dining tables (which are handcrafted). I also have 97 peaches (which I bought yesterday). I also have 75 nissan armada(here's how I ended up with 75 of them: initially I had 95, then I lost 32 of them leaving me with 63, then I got 12 more making it 75). My mother has 58 graphic novels. I also have 85 hamsters. My grandmother has 55 coffee tables. My grandfather has 95 smartphones. I also have 76 jonagold apples. I also have 61 mystery novels (which I borrowed from the library). My professor has 73 reference books. My friend has 82 stools. My professor also has 100 pineapples. I also have 71 smart TVs. I also have 97 hard drives(here's how I ended up with 97 of them: initially I had 83, then I lost 18 of them leaving me with 65, then I got 17 more making it 82, then I got 15 more making it 97). My sister also has 74 headphones. My mother also has 61 mystery novels. I also have 74 bookshelves (which need assembly). I also have 68 headphones (which were on sale). My colleague has 56 iguanas. My grandmother also has 65 persimmons. I also have 73 kia )")
       (trajectory
         .
         "0070a633d61294803a889e59950e125040e65911272e21087c1136209eb327a8b6"))
     ((question
        .
        "How many years did the Enriquillos Revolt last for?") (expected . "14") (status . completed) (time-ms . 14459)
       (correct . #f) (output . "")
       (trajectory
         .
         "00cc1b1c9d1c0c9c07048d3b5e753865c5f5ad3aa809c4b6f82ef93d0473257dcd"))
     ((question . "How many points did the Saints win by?") (expected . "3") (status . completed) (time-ms . 8600)
       (correct . #f) (output . "2")
       (trajectory
         .
         "008104a13bea0f1b722f6f00ac22d1318590b55abece37e6a3033e80dac43c419c"))
     ((question
        .
        "How many years did the Ming dynasty first conquered Yunnan borderlands between?") (expected . "8") (status . completed) (time-ms . 19441)
       (correct . #f) (output . "")
       (trajectory
         .
         "00da54b2eb515021d89694e386e5ef3376b2e61790a5105a0f0e3281a73fe03592"))
     ((question
        .
        "How many percent were not 65 years of age or older?") (expected . "92.9") (status . completed) (time-ms . 20173)
       (correct . #f) (output . "")
       (trajectory
         .
         "0079f329fbc3d00e9a179920a113997e1d8d732ff49ebebd73393d7124d07b65e4"))
     ((question
        .
        "How many more of the people  in Aigle were under 39 years old than over?") (expected . "36") (status . completed) (time-ms . 17365)
       (correct . #f) (output . "7")
       (trajectory
         .
         "00fa42a09629e35f80fed5f04f184a7d0b55f226cedafb5906f27057fd3301cb37"))
     ((question
        .
        "How many pesos was the Mickey Mouse money created by the Japanese worth?") (expected . "1000") (status . completed) (time-ms . 8527)
       (correct . #f) (output . "(+ 12.0 55.2 15.3 10.3)")
       (trajectory
         .
         "00a2fd2911953604e29afeaa701512d77fc0183b13c90a0b4550a511cf1325ab91"))
     ((question
        .
        "How many years passed between Edward III pressing his claims to the French throne and the revolt of 1381?") (expected . "44") (status . completed) (time-ms . 9846)
       (correct . #f) (output . "4393")
       (trajectory
         .
         "008b7a632e075fd81dae99bc43d90832be965e00650a82335f9c8b4892d7940530"))
     ((question
        .
        "How many more average points per game did Wilkins get over Michael Jordan?") (expected . "2.7") (status . completed) (time-ms . 31878)
       (correct . #f) (output . "7")
       (trajectory
         .
         "005bd6572f0926461f6589a279a396454fb66e4ddaacc1e4ab861b3bbf45ec7fe3"))
     ((question
        .
        "how many yards did anderson complete to winslow") (expected . "14") (status . completed) (time-ms . 11600)
       (correct . #f) (output . "1000000000")
       (trajectory
         .
         "002b9cda809e59e4c986f7bed7b4eb8235fdae0b5b574629e3579e34c19194c95d"))
     ((question
        .
        "How many in percent of households weren't made up of individuals?") (expected . "62.3") (status . completed) (time-ms . 17314)
       (correct . #f) (output . "44")
       (trajectory
         .
         "00131ee3c9bcfda315f7608d33793b1290b7efffd854b46237d2d48738228e26db"))
     ((question
        .
        "How many years was the lower Boise River fully contained for?") (expected . "1") (status . completed) (time-ms . 18521)
       (correct . #f) (output . "2.700000000000003")
       (trajectory
         .
         "0042e3a0a38a11a38887ba4513c026f7f4f77bdc7caf1aed62fb2ede8a5a8f5b8f"))
     ((question
        .
        "How many months into the year of 1918 did they re-enter the war?") (expected . "11") (status . completed) (time-ms . 21420)
       (correct . #f) (output . "")
       (trajectory
         .
         "004c4f60a851249eac9cff27bc585c5bdd3a29c2378c957dfc44183abf5694cea1"))
     ((question
        .
        "How many years was it between when Ava ceded all northern Avan territory to Mohnyin and when the forces of the confederation and Prome took Ava?") (expected . "20") (status . completed) (time-ms . 17815)
       (correct . #f) (output . "")
       (trajectory
         .
         "0015df74058a139ca840a56612bdd606c53838d70026218dc5bbeb36e3f94f7f63"))
     ((question
        .
        "How many more percent of cosmetic procedures were performed on men in 2001 than in 2014") (expected . "4") (status . completed) (time-ms . 38315)
       (correct . #f) (output . "44")
       (trajectory
         .
         "00d8554de878a80e346483c5436c320b6f46ff56cd125aa6b4adfe7514dbb0412f"))
     ((question . "How many percent were not German?") (expected . "71.9") (status . completed) (time-ms . 19318)
       (correct . #f) (output . "1")
       (trajectory
         .
         "0040cd36c7b8a6bd71faac9794553f478c97fb9ff065a328c7b9360cc40d14e94f"))
     ((question
        .
        "How many percent are not someone living alone who was 65 years of age or older?") (expected . "90.5") (status . completed) (time-ms . 9720)
       (correct . #f) (output . "11")
       (trajectory
         .
         "00e25f7b051e7c8ec789643c00d3045350a67e36896f96ba85337bca5ba0c303c5"))
     ((question
        .
        "How many years passed between the general strike of 1905 and the power struggle of the Finnish state?") (expected . "12") (status . completed) (time-ms . 8935)
       (correct . #f) (output . "")
       (trajectory
         .
         "001d7c5eb07afd2b821f944bcd8384c9a4e7d0e6b031be3382dcb5b2eea9d91dd0"))
     ((question . "When did the Thirteen Years' War end?") (expected . "1606") (status . completed) (time-ms . 14546)
       (correct . #f) (output . "")
       (trajectory
         .
         "002da220b8ff16538c11b97ef94e8a9515f4db718aa421e072f22ef967d21d98ff"))
     ((question
        .
        "How many people of Sri Lanka's population in percentage, were not Tamil indentured labourers from south India?") (expected . "90") (status . completed) (time-ms . 17853)
       (correct . #t) (output . "90.5")
       (trajectory
         .
         "00233d67fc1c2afc7aa39b1e56dedf3a356e3bbb784cd969587c3beb339bcf03c7"))
     ((question
        .
        "How many more scribes than scholars were in the editorial board created in march of 1773?") (expected . "3465") (status . completed) (time-ms . 50479)
       (correct . #f) (output . "12")
       (trajectory
         .
         "00605e51d0f475d1b71d3e678b76a5c2c102fbfe0bc90739be03920a3e07cc461d"))
     ((question . "When were 37 Buick automobiles made?") (expected . "1904") (status . completed) (time-ms . 16287)
       (correct . #f) (output . "1606")
       (trajectory
         .
         "00e6f856a3ac439e0e0b8137d0dec0db34ab21170996b116f59e3e6fa02f5fb5e2"))
     ((question
        .
        "How many percent are not someone living alone who was 65 years of age or older?") (expected . "3.8") (status . completed) (time-ms . 38816)
       (correct . #f) (output . "90")
       (trajectory
         .
         "00b86b53851a25e2fda2945fe6e3c53108e5d5d1c58f0f78da74781e590e6e3a47"))
     ((question . "How many players got ejected from the game?") (expected . "2") (status . completed) (time-ms . 17364)
       (correct . #f) (output . "3465")
       (trajectory
         .
         "002fa17da02fd032ffa66877eaf44434e58648d75b49ae3f0dcb3e590dcea04c00"))
     ((question
        .
        "For how many years was Reichel a member of the SS?") (expected . "3") (status . completed) (time-ms . 38522)
       (correct . #f) (output . "")
       (trajectory
         .
         "00855b5c1864f6eb8e8fd33b9c5f4e6cccb841ccf6d9989635745789ee9b899809"))
     ((question
        .
        "How many more, in percentage, were households made up of were Marriage living together than had a female householder with no husband present?") (expected . "26.7") (status . completed) (time-ms . 8929)
       (correct . #f) (output . "")
       (trajectory
         .
         "006585f5be3a990bce113c30631b947d9636d09b07a9bacf472c860ec1f5fb54b5"))
     ((question
        .
        "How many years after her marriage to Ralph Boswell did Christian Widers die?") (expected . "7") (status . completed) (time-ms . 23956)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f4a889563b6c99150d4187fe382aeb1218cecc1417ff7f26ec1234953e8802e4"))
     ((question
        .
        "How many more procedures were performed in patients who were 18 years old or younger in 2005 than in 1996?") (expected . "319000") (status . completed)
       (time-ms . 19606) (correct . #f) (output . "9")
       (trajectory
         .
         "004971241aae7d850652e2aca99258dee37dfe82520e25a9c511a67814b8c5215e"))
     ((question
        .
        "How many years did the Ukraine agree to lease major parts of its facilities to the Russian Black Sea fleet for?") (expected . "20") (status . completed) (time-ms . 26667)
       (correct . #f) (output . "7")
       (trajectory
         .
         "00074c7fcbdfbbaa47089972ebcac0eea76b350643cd67082820d8d9638f852204"))
     ((question
        .
        "How many 6-yard touchdown passes did Brett Favre have?") (expected . "2") (status . completed) (time-ms . 25605)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f09472c70a0dad9c2c937aaa466bfb13d1e1f2bbb204962a431b22e12f374f4f"))
     ((question
        .
        "How many total military personnel are in the Croatian Armed Forces, active and reserves combined?") (expected . "20506") (status . completed) (time-ms . 13436)
       (correct . #f) (output . "")
       (trajectory
         .
         "0020e2d2e27d696534c7ef5a3f3f8204d6b8a306f69e0f571f6f63340fa73eb4fb"))
     ((question
        .
        "How many of the 200 reports did not have improperly recorded information?") (expected . "33") (status . completed) (time-ms . 14027)
       (correct . #f) (output . "45")
       (trajectory
         .
         "00f978c12a6afe8e039eb41708075c4247c77c2b289e2d751ae6ed65c16e4880b4"))
     ((question
        .
        "How many touchdown passes did Roy Williams catch?") (expected . "1") (status . completed) (time-ms . 18346)
       (correct . #f) (output . "")
       (trajectory
         .
         "0023f3205b889f04c8bb7b2a60b06f25729f200ee04edd44ba57b5cf25ee9f8575"))
     ((question
        .
        "How many of the Cardinals mentioned died in September 1361?") (expected . "1") (status . completed) (time-ms . 19162)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ea40705e1b96fa503f72ef643f82657958e5539ece77d32e01f8caab921a89ab"))
     ((question
        .
        "According to Searchlight, in which years did the Flag group have 10000 or more members?") (expected . "1974") (status . completed) (time-ms . 22518)
       (correct . #f) (output . "20506")
       (trajectory
         .
         "00747eb22a7c47904199b31e4fa60af507e4a1d609401270f711cea36b5386798b"))
     ((question . "How many yards was the longest field goal?") (expected . "48") (status . completed) (time-ms . 18985)
       (correct . #f) (output . "33")
       (trajectory
         .
         "0031ea1e7044f36ba84a795e5c4c3617ff465e9fb4f73001c420bc40e431ec209c"))
     ((question . "How many total points were scored?") (expected . "47") (status . completed) (time-ms . 36164)
       (correct . #f) (output . "")
       (trajectory
         .
         "00bcd6e1811181c271a84b27838aa6a451c1010844c1861a57595a850460a73814"))
     ((question . "How many passes did Lewis catch?") (expected . "1") (status . completed) (time-ms . 15335)
       (correct . #f) (output . "")
       (trajectory
         .
         "009fac0e93f1485db53cf085f43b68180854b7a746a3c6c7d21c33ecd0f2cd196b"))
     ((question
        .
        "How many less Republican voters are there than Democratic voters?") (expected . "4482") (status . completed) (time-ms . 21481)
       (correct . #f) (output . "48")
       (trajectory
         .
         "00029baccaa7e9d09feb46f3adf899a83331f7e38dada8e8d021881f73cc77d7d6"))
     ((question
        .
        "How many yards longer was the longest passing touchdown than the shortest?") (expected . "5") (status . completed) (time-ms . 8459)
       (correct . #f) (output . "44")
       (trajectory
         .
         "00833ebc18925f01c5fbd7f90da64cd3a9c9c4a320bbaff078136f119290d6c308"))
     ((question
        .
        "How many more Vietnam Veterans were members of the Royal Australian Navy than members of the Royal Australian Air Force?") (expected . "8700") (status . completed) (time-ms . 22583)
       (correct . #f) (output . "")
       (trajectory
         .
         "006cc5c409df5375565a4dcad5e131fa6568b4199d836c56176fb732e8a1d2ce72"))
     ((question
        .
        "How many percent of the population did not live in owner-occupied housing units or rental housing units?") (expected . "9") (status . exhausted) (time-ms . 52353)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00d7da1b49e7c28a1ce49a43f833f40e8b0a408f2a7c67d8bee6e033ca84e0a363"))
     ((question
        .
        "How many more points did the Bray Wanderers win than St Francis in the 1989-90 FAI Cup?") (expected . "3") (status . exhausted) (time-ms . 54813)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00f5413eaa84093abbbab6547ab0a651269fd4bca1274d9367b85bb013606671d7"))
     ((question
        .
        "How many more touchdowns scored compared to scored field goals?") (expected . "1") (status . completed) (time-ms . 41038)
       (correct . #f) (output . "5")
       (trajectory
         .
         "0030c5e3054bd04bf0b077c60aec4af603be8a3860c9997e4cbdba2d4e483431b1"))
     ((question
        .
        "How many rushing touchdowns were scored from less than 5 yards?") (expected . "1") (status . completed) (time-ms . 31344)
       (correct . #f) (output . "")
       (trajectory
         .
         "002a83fd580e83e5233798842a1f889a5bee4c5f07e16e95be9752755d0a8153a3"))
     ((question
        .
        "How many total Italians fought from both sides?") (expected . "1500") (status . exhausted) (time-ms . 57996)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "00259308fc0484a0b20672696e3f4e485cbc2d5e97b9e3f4dae37285f32f3f2e78"))
     ((question
        .
        "How many percent did not have a male householder with no wife present?") (expected . "94.9") (status . completed) (time-ms . 20086)
       (correct . #f) (output . "")
       (trajectory
         .
         "0049a21d19d32edfcacdca33b2acc17e5143620147b40b35012af8808394d9afbb"))
     ((question
        .
        "How many people were either Buddhists or Hindus?") (expected . "14000") (status . exhausted) (time-ms . 37220)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "0030507a20a34eab8fd96bcbfeb01f0b417a3126b14c634dde2bc858d4755551ef"))
     ((question . "How many in percent weren't 25 to 44?") (expected . "68.3") (status . completed) (time-ms . 21941)
       (correct . #f) (output . "")
       (trajectory
         .
         "00f1bd80ccfdcf2d131c687d251e76689bee581e568406f492fbfea0f36cdcd5f9"))
     ((question . "How many points did the Bears win by?") (expected . "31") (status . completed) (time-ms . 17837)
       (correct . #f) (output . "")
       (trajectory
         .
         "00c6d7b6e315c51c40a3c24f0f903180b7bc28aabc63627c8389fd3f1f1572f8d1"))
     ((question
        .
        "How many points were scored in the first quarter?") (expected . "7") (status . completed) (time-ms . 13199)
       (correct . #f) (output . "94.9")
       (trajectory
         .
         "00a98f4ed5e4f783a6a6be9cfcea116b787f250a2ca7eb544309ebebcba928d457"))
     ((question
        .
        "How many more voters are registered as Ameican Independent compared to Green Party?") (expected . "449") (status . completed) (time-ms . 19379)
       (correct . #f) (output . "68.3")
       (trajectory
         .
         "00268a843d458de47e6f0853c06f971a5e3444ad28af84acbf922d76bd280719ca"))
     ((question . "How many percent are not non-families?") (expected . "66.6") (status . completed) (time-ms . 24863)
       (correct . #f) (output . "")
       (trajectory
         .
         "008fa3da9bd8215c3ba9c05987088295d09559b9e3291b758bbbdba6f69d79df6f"))
     ((question
        .
        "How many yards was the longest touchdown pass?") (expected . "14") (status . completed) (time-ms . 11870)
       (correct . #f) (output . "")
       (trajectory
         .
         "00bfd307c069ba13aa6118e5ab90f5dc496345dbd829397ae94b543db736164eb7"))
     ((question
        .
        "How many more Buick vehicles were produced in 1906 than in 1905?") (expected . "650") (status . completed) (time-ms . 22377)
       (correct . #f) (output . "449")
       (trajectory
         .
         "00af7d29c430177d6097f871e6af7b289ae3c24bd17d04db42427a7ede6d539df1"))
     ((question
        .
        "How many combined Mechanized Infantry Battalions and Infantry Battalions were part of the Libyan Army in 2009?") (expected . "28") (status . completed) (time-ms . 23783)
       (correct . #f) (output . "")
       (trajectory
         .
         "0043e1d7909678fa1ef44b33bdf41055f17255d1627b93058bc2f398db8c625c4b"))
     ((question . "How many field goals did Jason Elam make?") (expected . "1") (status . completed) (time-ms . 9577)
       (correct . #f) (output . "")
       (trajectory
         .
         "00b00d268e5ee19bba60b25bc1858e0ac231eb439121791b526c5c929009b00692"))
     ((question
        .
        "How many total points were scored in the game?") (expected . "43") (status . completed) (time-ms . 20575)
       (correct . #f) (output . "")
       (trajectory
         .
         "003a033d7635de4121f4fc49a04b5da649a789867d36cece0f8804eec4efecb6ca"))
     ((question . "How many percent of people were not Asian?") (expected . "99") (status . completed) (time-ms . 21326)
       (correct . #f) (output . "")
       (trajectory
         .
         "00d8a73459ea78e0ce858dcb0e07ca29829e97d2011eaf17b76500a67406cfb396"))
     ((question . "How many in percent weren't 45 to 64?") (expected . "89.4") (status . completed) (time-ms . 24235)
       (correct . #f) (output . "27")
       (trajectory
         .
         "00be28778e770a573aa47169d927c33080c6a4f503c69f22b1314fdd8a17de5a1b"))
     ((question
        .
        "How many percent higher was the margin between Trump and Hillary than that between Obama and Romney?") (expected . "2.3") (status . completed) (time-ms . 43054)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ce7d418d213d198f29a7c81d9507a99ff60e6193fd0d021d991f58060f76e2d5"))
     ((question . "How many percent did not speak Vietnamese?") (expected . "98.8") (status . completed) (time-ms . 15635)
       (correct . #f) (output . "(- 100 10.5)")
       (trajectory
         .
         "00fb40b8f8d714bb2d36c4fd693a8d82a693c1bc8186ccc8aeb8cb84ac1b306667"))
     ((question
        .
        "How many more whites are there than Native Americans?") (expected . "69.7") (status . completed) (time-ms . 17875)
       (correct . #f) (output . "2.700000000000003")
       (trajectory
         .
         "00467ba855dd51116b38901fb455d410e3f977c990a0088eec9262903e5a61b02d"))
     ((question
        .
        "How many more yards did Donovan McNabb accumulate on passing touchdowns relative to rushing touchdowns?") (expected . "9") (status . completed) (time-ms . 20110)
       (correct . #f) (output . "98.8")
       (trajectory
         .
         "00cd3c2ad1c367de64ecf56745af1874619d49dc02bae9ae631f4e4fa31dad4b80"))
     ((question
        .
        "How many percent larger is the average family size than household size?") (expected . ".52") (status . completed) (time-ms . 11661)
       (correct . #f) (output . "24298")
       (trajectory
         .
         "00e013db805cbda58290ffb5273161f4bd0e2651070a94a9d8af4b4945a8e33b7c"))
     ((question
        .
        "How many more than the top five procedures does the American Society for Aesthetic Plastic Surgery looks at the statistics for?") (expected . "24") (status . completed) (time-ms . 13896)
       (correct . #f) (output . "21")
       (trajectory
         .
         "00c922b67bf3199d777d54e1fec099a2179a67274a877638ac661a7703df85e3a0"))
     ((question
        .
        "How many years passed between the preparation for relocating Beiping and it's construction being completed?") (expected . "17") (status . completed) (time-ms . 12045)
       (correct . #f) (output . "")
       (trajectory
         .
         "0024e07133db008ce0231edf29fed185c3f87f46ce4f5627d40a17a165b52824a9"))
     ((question
        .
        "How many people and households are reported according to the census?") (expected . "55959") (status . completed) (time-ms . 27967)
       (correct . #f) (output . "")
       (trajectory
         .
         "008c8a02b17ddecbd8e7732f226932689494ca90cbc838cd68329df270f1c8312e"))
     ((question . "How many years did the Hotaks rule") (expected . "7") (status . completed) (time-ms . 17944)
       (correct . #f) (output . "")
       (trajectory
         .
         "00db5b9841193ca6a570abff32172a3e3cf1380ad77fa5c89787e56b5acd6837e3"))
     ((question
        .
        "How many percent were not 65 years of age or older?") (expected . "90.4") (status . completed) (time-ms . 23106)
       (correct . #f) (output . "")
       (trajectory
         .
         "0049b691ebbf8ca8fb2af72c30d236a4ecb875e3692dc6dda0e6ef302c911261bd"))
     ((question
        .
        "How many percent of people spoke the smallest three languages considered to be linguistic minorities?") (expected . "2.74") (status . completed) (time-ms . 18552)
       (correct . #f) (output . "")
       (trajectory
         .
         "00975d7feae96ca6bfa73a7b35405b2646fd5a41e3bad1911d8b82af84f30aac31"))
     ((question
        .
        "How many years after the 10th Cavalry returned from the Philippines did the 9th Cavalry Regiment become Park Rangers for Yosemite National Park?") (expected . "0") (status . completed) (time-ms . 19125)
       (correct . #f) (output . "")
       (trajectory
         .
         "008f34bc1e8531f9baac21a43f27ee0990b6740120c535ef42ce09847ff3aa3403"))
     ((question
        .
        "How many more housing units are there than families?") (expected . "8150") (status . completed) (time-ms . 14838)
       (correct . #f) (output . "")
       (trajectory
         .
         "00ea02b6690c8ef6839e67b0c83a8ec5b11f9b17774a20667db6bf20ea75d63ad8"))
     ((question
        .
        "How many years was it from when MacGill was appointed as provost to when he died?") (expected . "9") (status . exhausted) (time-ms . 56498)
       (correct . #f) (output . "Resources exhausted")
       (trajectory
         .
         "002d5a658408ada6ac8e7e4327a53694d5807bb8c9270e068ba293408e552ae438"))
     ((question
        .
        "How many percent of Forth Worth commuters did not carpool in 2009?") (expected . "88.3") (status . completed) (time-ms . 47613)
       (correct . #f) (output . "")
       (trajectory
         .
         "00e4cc51d89662c33992866901af8dcd8d23601b762f5c7d0ed9214e6d75882fac"))
     ((question . "how many yards did stover kick in the third?") (expected . "42") (status . completed) (time-ms . 25211)
       (correct . #f) (output . "7")
       (trajectory
         .
         "004054f07ecad318c8951f7ade0a06c297ba22c433135267ff38892e1cf637caf9")))))
