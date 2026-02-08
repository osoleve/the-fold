(benchmark-results (model "Qwen/Qwen3-Next-80B-A3B-Instruct-FP8")
  (mode "hotpotqa") (timestamp "2026-02-08T14:56:59Z")
  (n-problems 50) (correct 28) (accuracy 0.56)
  (total-ms 1233419)
  (problems
    (((question
        .
        "The 2011–12 VCU Rams men's basketball team, led by third year head coach Shaka Smart, represented Virginia Commonwealth University which was founded in what year?") (expected . "1838") (level . "hard") (status . completed)
       (time-ms . 11677) (correct . #t) (output . "1838")
       (trajectory
         .
         "00b446cc58b3a9a870585acc5f47bbe19e935b80951a5f6f282a9439d3877fb2e1"))
     ((question
        .
        "In which region is the town in which  Fort Kongenstein is located ?") (expected . "the Greater Accra Region") (level . "hard")
       (status . completed) (time-ms . 9372) (correct . #t)
       (output . "Greater Accra Region")
       (trajectory
         .
         "00c6f02f1f7cac4a5bba4266f13dff552ed7e618a01b638f3d2e754cc8de8b8243"))
     ((question
        .
        "What was the 2010 population of the town where Lake George Avenue Historic District is located?") (expected . "5,042") (level . "hard") (status . completed)
       (time-ms . 11716) (correct . #t) (output . "5,042")
       (trajectory
         .
         "00df4f1de73c60b72f4d26af4278ad902471b84d80a060f1d08000724e78a45802"))
     ((question
        .
        "what 6-story building was listed on the National Register of Historic Places in 1980?")
       (expected . "Worcester Cold Storage and Warehouse Co. fire")
       (level . "hard") (status . completed) (time-ms . 30950)
       (correct . #t)
       (output . "Worcester Cold Storage and Warehouse Co.")
       (trajectory
         .
         "00ef8f7ddcf68e610e59976435b81fa8f271c162a1cc1a5f7e4e3a1a8bf4be02ef"))
     ((question
        .
        "Hostel: Part III is the first film in the series to be neither written nor directed by a director born in which year ?") (expected . "1972") (level . "hard") (status . completed)
       (time-ms . 17766) (correct . #t) (output . "1972")
       (trajectory
         .
         "00cf11377135ed4b3729a048c4732ec6840d6e1901de83ef14c18036439f2a1147"))
     ((question
        .
        "What park is just south of the location where the New York Jets played prior to moving to Shea Stadium? ") (expected . "Central Park") (level . "hard")
       (status . completed) (time-ms . 12262) (correct . #f)
       (output . "Yankee Stadium")
       (trajectory
         .
         "004c926779623fd005b216c9b48dc0120e6aed0ad2b69ea87519e8c0834345ae83"))
     ((question
        .
        "Which author is an economist, Czesław Miłosz or Bhabananda Deka?") (expected . "Prof Bhabananda Deka") (level . "hard")
       (status . completed) (time-ms . 9684) (correct . #t)
       (output . "Bhabananda Deka")
       (trajectory
         .
         "0047221808d1e6d7b4993e855cb9e4fd84f3ddaea41d54c11072b5449da26e7929"))
     ((question
        .
        "When did the author of the book One More River is based on win the Nobel Prize?") (expected . "1932") (level . "hard") (status . completed)
       (time-ms . 13054) (correct . #t) (output . "1932")
       (trajectory
         .
         "009ed4f814d30d9ce0bd98a92dc41861757b08f4434d0320bdcf195f4ac862d2d3"))
     ((question
        .
        "Are Marilyn Manson and Joe Lynn Turner both American singer, songwriters?") (expected . "yes") (level . "hard") (status . exhausted)
       (time-ms . 59036) (correct . #f)
       (output . "Resources exhausted")
       (trajectory
         .
         "006756c1f8438a9dc5b6c3a9ce243bedd077bfda19e9e761245cb71943ed04bd9a"))
     ((question
        .
        "in what year did the Israeli presidential elections take place after the resignation of the seventh president of Israel?") (expected . "2000") (level . "hard") (status . completed)
       (time-ms . 18274) (correct . #t) (output . "2000")
       (trajectory
         .
         "009b7fa38121b46551ee1c8516886db49c889758f9464801755b7268824bcd88ea"))
     ((question
        .
        "What is type of philosophies is The Structure of Liberty about?") (expected . "political") (level . "hard")
       (status . completed) (time-ms . 9959) (correct . #f)
       (output . "classical liberalism")
       (trajectory
         .
         "0017a6dbf6135fe4929fcf35b4e0d2ea520089bc56a42724f8a70726b7ff612ecf"))
     ((question
        .
        "When was the football club founded in which Walter Otto Davisplayed at centre forward?") (expected . "1885") (level . "hard") (status . completed)
       (time-ms . 9842) (correct . #t) (output . "1885")
       (trajectory
         .
         "00de67cc3a897cea16e1fbf6bc51c02a243ac05e4aaee6f5914368fe3f281f234f"))
     ((question
        .
        "Which male singer performed together with a female American singer, who was born in 1954 and whose third studio album was named Through His Eyes, have a hit duet with in 1985?") (expected . "Phil Collins") (level . "hard")
       (status . completed) (time-ms . 15225) (correct . #f)
       (output . "Emilio Estefan")
       (trajectory
         .
         "00a699eb1a4364bff553381a8f1cfe0ed7b738ae025b9371a2d197c601c99ad54b"))
     ((question
        .
        "What group released a song called \"Stinkfist\" on September 17, 1996?") (expected . "Tool") (level . "hard") (status . completed)
       (time-ms . 10476) (correct . #t) (output . "Tool")
       (trajectory
         .
         "00f1aa67efeb8ec642a4bb83185b8a5d4eb25a418979cd1ceb3a2c1e3a202bbe4f"))
     ((question
        .
        "Elle Royal's video \"What Can I Say\" went viral after she was featured as “Female Artist of the Week” by a video blog founded in what year?") (expected . "2005") (level . "hard") (status . completed)
       (time-ms . 12000) (correct . #f) (output . "2010")
       (trajectory
         .
         "00f98c00c5a23dcbbfaafb25243e369438c4751f80f3448da99b77a528f86e02de"))
     ((question
        .
        "The founder of this Canadian owned, American manufacturer of business jets for civilian and military developed this portable tape system.") (expected . "8-track") (level . "hard")
       (status . completed) (time-ms . 11421) (correct . #t)
       (output . "8-track cartridge")
       (trajectory
         .
         "00152ebe1fcbb54f442ddcceee4c88c262978187b3fb6a3cb92aaf6f8abda49ed5"))
     ((question
        .
        "What city does Saint Paul School of Theology and United Methodist Church of the Resurrection have in common?") (expected . "Kansas") (level . "hard") (status . completed)
       (time-ms . 13648) (correct . #t)
       (output . "Overland Park, Kansas")
       (trajectory
         .
         "0017aef0875bee36e019dda493b05943be6f32558727f564059c8c00d63e80be30"))
     ((question
        .
        "What year was the company, who released Forever Kingdom for PlayStation 2, founded?") (expected . "1986") (level . "hard") (status . completed)
       (time-ms . 11783) (correct . #f) (output . "1978")
       (trajectory
         .
         "0097fe0ebba07f6ebce0e5a86f0d4237de9e8ba6dcfedb2067656a66b7f203d2ee"))
     ((question
        .
        "Snoop Dogg's Doggystyle and Snoop Dogg's Hustlaz: Diary of a Pimp are hip-hop music videos that feature what film genre?") (expected . "hardcore pornography") (level . "hard")
       (status . completed) (time-ms . 15285) (correct . #f)
       (output . "gangster")
       (trajectory
         .
         "00ef62f43de6ff194a345ade31a3a08c0732f4311e5e33876d108c8fa300edd1ab"))
     ((question
        .
        "In what year was the Golden State NBA player, who was part of the Cavaliers-Warriors rivalry, named NBA Finals Most Valuable Player?") (expected . "2015") (level . "hard") (status . completed)
       (time-ms . 17776) (correct . #f) (output . "2017")
       (trajectory
         .
         "00ff2fb34044fdb7e79747afb5efd6132331c0665fe0333b928ad1d79cdecfc6ce"))
     ((question
        .
        "What is the name of the national level academy for performing arts that Bhupen Hazarika won his award in 2008? ") (expected . "Sangeet Natak Akademi") (level . "hard")
       (status . completed) (time-ms . 16406) (correct . #t)
       (output . "Sangeet Natak Akademi")
       (trajectory
         .
         "0049a61f876503f19226e6db804c0523be9e5534df44faa5d69f7c37c51a8a8dcf"))
     ((question
        .
        "Which Western action-adventure video game by Rockstar did Tyler Bunch act for?") (expected . "Red Dead Redemption") (level . "hard")
       (status . completed) (time-ms . 10541) (correct . #t)
       (output . "Red Dead Redemption")
       (trajectory
         .
         "007ed08dda2d7925629409d4ac221c4019d40907363fa4d2fa63b0c1f94b85330c"))
     ((question
        .
        "Both Truth in Science and Discovery embrace what campaign?") (expected . "\"Teach the Controversy\" campaign")
       (level . "hard") (status . completed) (time-ms . 10645)
       (correct . #f) (output . "intelligent design")
       (trajectory
         .
         "009802d40dd55606e43c3eb905bbd4cd182ae7834c52f4ddbe7cb0600ed261500b"))
     ((question
        .
        "What flag features 13 stars, and has a creator who also has a memorial bridge named after her connecting Pennsylvania and New Jersey?") (expected . "Betsy Ross") (level . "hard")
       (status . completed) (time-ms . 15369) (correct . #t)
       (output . "Betsy Ross Bridge")
       (trajectory
         .
         "0087787dfd2e67e89f3966ab91b2fb7cf023c981a35e0570e5cd04b4b60c8a908f"))
     ((question
        .
        "In what year was the writer of the drama \"Ingenious\" born?") (expected . "1959") (level . "hard") (status . completed)
       (time-ms . 14351) (correct . #t) (output . "1959")
       (trajectory
         .
         "005cdf3c1dc78c2db2eec6ddeda524a06489b11c91dfe1475af777bf8baf1045cb"))
     ((question
        .
        "The HMS Seraph is most famous for a WWII operation intended to cover the allied invasion of Sicily in what year?") (expected . "1943") (level . "hard") (status . completed)
       (time-ms . 12560) (correct . #t) (output . "1943")
       (trajectory
         .
         "009b38487936c52724fd5b0d948986ffb342304e336dcca3fcd95df87702493e39"))
     ((question
        .
        "How many Academy Awards did the film, in which Jimmy Bryant provided the singing voice for the character Tony, win ?") (expected . "10") (level . "hard") (status . completed)
       (time-ms . 15986) (correct . #t) (output . "10")
       (trajectory
         .
         "00b3e6eb2494e01fcf9615c312568c8bd79c48d80d71a662148d69756bba4cc686"))
     ((question
        .
        "When was the actor in the CW show developed by Greg Berlanti who also appears in \"Gods and Generals\" born?") (expected . "1970") (level . "hard") (status . completed)
       (time-ms . 24909) (correct . #f) (output . "April 22, 1949")
       (trajectory
         .
         "00939dd3f52bb2c8f61ea96215e18127b1d0a51654ce73b26eaae65afedea8ba40"))
     ((question
        .
        "What was the 2016 population of the city on the Bay Fundy which had an office of the Bank of British North America?") (expected . "67,575") (level . "hard") (status . completed)
       (time-ms . 11720) (correct . #t) (output . "67,575")
       (trajectory
         .
         "00ae9ab80e989e2cab65722cf3021b691e8a7316b08c4db338010cdb1f8eafd11b"))
     ((question
        .
        "In what year was the scientific journal for which Danilo Erricolo is currently Editor-in-Chief established?") (expected . "1952") (level . "hard") (status . completed)
       (time-ms . 11657) (correct . #t) (output . "1952")
       (trajectory
         .
         "000859fda9fa321d5e3d3f66c9d8dcae506e6bcffc331677bcb707ab4873bd71d1"))
     ((question
        .
        "What was the first role in television for the actress who played Lindsay Lohan's best  friend in the Disney Channel's  original movie \"Get a Clue\"?") (expected . "\"Fudge\"") (level . "hard")
       (status . completed) (time-ms . 34904) (correct . #f)
       (output . "Lizzie McGuire")
       (trajectory
         .
         "009014c8131a8f878ec8754e7472ee3bca6927806ac7e7b9d767d5fd6717a5e930"))
     ((question
        .
        "The Synod of Chester led to the battle of the same name that took place in what time period?") (expected . "early 7th century") (level . "hard")
       (status . completed) (time-ms . 16762) (correct . #f)
       (output . "circa 616 AD")
       (trajectory
         .
         "00f6553352d50023cd8ec8a840645780262ac00561f53c6fc1f629a79f1567af0c"))
     ((question
        .
        "How many times has the national team Joseph Raich Garriga played for in 1941 participated in FIFA World Cups?") (expected . "14") (level . "hard") (status . completed)
       (time-ms . 21736) (correct . #f) (output . "2")
       (trajectory
         .
         "00c452571fa809717433bfe6fe9fbef55b24c71a31edbc44ea5a9ba3a4c7a2231f"))
     ((question
        .
        "What exit of Garden State Parkway should you take in order to get to the Seacourt Pavilion?") (expected . "Exit 82") (level . "hard")
       (status . completed) (time-ms . 13632) (correct . #f)
       (output . "Exit 98")
       (trajectory
         .
         "002de9abcf983de76d56b2496dfaedac7720643a64e56cfa7e2738fd98ea635b08"))
     ((question
        .
        "What is the birthplace of YouTube's 2016 overview featured creator Wengie?") (expected . "Guangzhou, China") (level . "hard")
       (status . completed) (time-ms . 11861) (correct . #t)
       (output . "Guangzhou, China")
       (trajectory
         .
         "00af8304d5b5a413d656ce73ac6656ac859ee51fb6aedb46150c7cc161f98f6120"))
     ((question
        .
        "Where is stadium that was named after Rodney Cline Carew?") (expected . "Panama City") (level . "hard")
       (status . completed) (time-ms . 10711) (correct . #t)
       (output . "Panama City")
       (trajectory
         .
         "007d115f33a7265e49d0d703dae2a84ea8d4980c3d1862091c5e91ab254fd2e9a6"))
     ((question
        .
        "3TEETH was the debut album for the opening act for the2016 tour with a headlining band that was formed in which California city?") (expected . "Los Angeles") (level . "hard")
       (status . completed) (time-ms . 11377) (correct . #f)
       (output . "Oakland")
       (trajectory
         .
         "005aea7ba8341afddcfc993fe7a67d819960499a90b7898c6dc694e4b23ec78bc0"))
     ((question
        .
        "In 2010, what was the population for a state which is known for Annalee Dolls?") (expected . "6,241") (level . "hard") (status . completed)
       (time-ms . 12867) (correct . #f) (output . "968,591")
       (trajectory
         .
         "00e09e890011b69af23fd6db48d9c6490886d2f7899c35f9dc46ee64c6ed5b9acc"))
     ((question
        .
        "Theme Park Inc is a video game that created what sequel in North America in 1999?") (expected . "Sim Theme Park") (level . "hard")
       (status . completed) (time-ms . 98758) (correct . #f)
       (output . "Theme Park World")
       (trajectory
         .
         "000a5d18a04ca4352b662c46159334461a1bbeaaa7a75a21c56acb4e5f355680a1"))
     ((question
        .
        "When was the New Orleans Pelicans player featured on the NBA 2K16 cover first drafted?") (expected . "2012") (level . "hard") (status . completed)
       (time-ms . 15562) (correct . #t) (output . "2012")
       (trajectory
         .
         "001651cbaaeca850d5a3204a6255b82765be0c423d07b0c6b20178f0baea7ad96b"))
     ((question
        .
        "Which University was founded earlier, Pennsylvania State University or Queen's University?") (expected . "Queen's University") (level . "hard")
       (status . completed) (time-ms . 15739) (correct . #t)
       (output . "Queen's University")
       (trajectory
         .
         "00b376bda3ebebf4c2cabcecc2e9802d4e22d9e5bd82512e71b515976cfceead18"))
     ((question
        .
        "This expansion of the 2008 magazine article \"Is Google Making Us Stoopid?\" was a finalist for what award?") (expected . "Pulitzer Prize") (level . "hard")
       (status . completed) (time-ms . 18816) (correct . #t)
       (output . "2011 Pulitzer Prize in General Nonfiction")
       (trajectory
         .
         "00947957600a6ec5371bfbe73cca53dd026d7aa2875a27b0f75d93c65dfcb2f760"))
     ((question
        .
        "The actor who appeared on Charlotte's Shorts and Weeds was born in what year?") (expected . "1966") (level . "hard") (status . completed)
       (time-ms . 46745) (correct . #f) (output . "1888")
       (trajectory
         .
         "00c762ddb1cc930424d75e728aacda98bb623f85b876431ae58f0fac2aa07bffba"))
     ((question
        .
        "What year was the inventor of Rejuvelac born?") (expected . "1909") (level . "hard") (status . completed)
       (time-ms . 13151) (correct . #t) (output . "1909")
       (trajectory
         .
         "00d3f1c26d2c919271111e708e8a1843e96333b7aef9efd7dd24645b66433945e5"))
     ((question
        .
        "What song written by Jimmy Webb inspired the name of a supergroup including Johnny Cash?") (expected . "Highwayman") (level . "hard")
       (status . completed) (time-ms . 9710) (correct . #f)
       (output . "Wichita Lineman")
       (trajectory
         .
         "000e5c882f52ba30344188ff2e5d47eead1e8c2c7908d478b3f4f760833510b921"))
     ((question
        .
        "Vincas Kudirka is the author of both the music and lyrics of a national anthem which has how many words?") (expected . "fifty-word") (level . "hard")
       (status . completed) (time-ms . 11173) (correct . #f)
       (output . "128")
       (trajectory
         .
         "0046ece74d6ebc3bec1ef1052bebee9d5238bc44092a426f8e41e3b396cdfde5ad"))
     ((question
        .
        "The large subunit and small subunit that use two types of RNA are major components that make up what?") (expected . "Ribosomes") (level . "hard")
       (status . completed) (time-ms . 14181) (correct . #t)
       (output . "ribosome")
       (trajectory
         .
         "0097595971e742688632fe8e4d297af35fdbc7b03e19a4577193a7842ae38fd0b3"))
     ((question
        .
        "Which feature film was released earlier, One Magic Christmas or Muppet Treasure Island?") (expected . "One Magic Christmas") (level . "hard")
       (status . completed) (time-ms . 9002) (correct . #t)
       (output . "One Magic Christmas")
       (trajectory
         .
         "00e9bb7973d6ae03d59d5e73a16134ec26c18ee9ef075f8a82dc5c22a787576cc1"))
     ((question
        .
        "When was the person who did the music for Manru born?") (expected . "18 November") (level . "hard")
       (status . completed) (time-ms . 68468) (correct . #f)
       (output . "6 November 1860")
       (trajectory
         .
         "004062f7ace5254aa48fa1e44016fa455ce25bad95c78734c31205b6f63a903f9a"))
     ((question
        .
        "Matt McKenzie had a role on the television show that focused on the lives of teenage friends from what Wisconsin town?") (expected . "Point Place") (level . "hard")
       (status . completed) (time-ms . 312914) (correct . #f)
       (output . "River City")
       (trajectory
         .
         "00ac9d23f4432162466b43e7f8582cbfc92674f95daf6a1875aa297a740955a9ab")))))
