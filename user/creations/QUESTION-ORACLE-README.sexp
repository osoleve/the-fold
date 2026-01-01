(question-oracle-documentation
  (title "The Question Oracle")
  (author "Haiku")
  (created "2025-12-27")
  (description
    "An interactive oracle that answers yes/no questions with mystical computational flair."
    "Ask the oracle anything, and it divines the truth using pure randomness and the wisdom"
    "of the functional programming aether. Responses range from definitively yes/no to"
    "mysteriously uncertain, reflecting the oracle's changing moods and the unpredictability"
    "of fate.")

  (features
    "Interactive question-answering gameplay"
    "Three response types: positive (70%), negative (20%), uncertain (10%)"
    "Beautiful ASCII art oracle display"
    "Fast, single-file implementation in pure Scheme"
    "Works with the standard Scheme REPL"
    "No external dependencies"
    "Amusing programming-themed responses")

  (usage
    (title "Loading the Module")
    (code "(load \"user/creations/question-oracle.ss\")")
    (description "Load the oracle into your Scheme environment."))

    (title "Getting a Single Answer")
    (code "(get-answer)")
    (description "Display a single oracle prediction without interaction."))

    (title "Interactive Oracle Shell")
    (code "(oracle-shell)")
    (description "Start the interactive oracle game.")
    (commands
      "(ask)   - Ask the oracle a question"
      "(quit)  - Leave the oracle's presence"
      "(help)  - Show oracle wisdom"))

    (title "Manual Answer")
    (code "(display-oracle (random-element *positive-responses*))")
    (description "Display any custom oracle response text."))

  (examples
    (example-1
      (description "Get a quick answer")
      (code
        "(load \"user/creations/question-oracle.ss\")"
        "(get-answer)"))

    (example-2
      (description "Use the interactive oracle")
      (code
        "(load \"user/creations/question-oracle.ss\")"
        "(oracle-shell)"
        "; then type: (ask)"
        "; then enter your question"))

    (example-3
      (description "Ask multiple questions in a row")
      (code
        "(load \"user/creations/question-oracle.ss\")"
        "(oracle-shell)"
        "; Type (ask) multiple times"
        "; Each time get a different answer")))

  (functions
    (function
      (name "oracle-shell")
      (signature "→ void")
      (description "Start the interactive oracle game with commands."))

    (function
      (name "get-answer")
      (signature "→ void")
      (description "Display a single oracle answer."))

    (function
      (name "display-oracle")
      (signature "String → void")
      (description "Display any text in oracle's ASCII art format."))

    (function
      (name "predict-answer")
      (signature "→ String")
      (description "Generate an answer: 70% positive, 20% negative, 10% uncertain."))

    (function
      (name "random-element")
      (signature "(List α) → α")
      (description "Pick a random element from a list."))

    (function
      (name "pad-center")
      (signature "String × Nat → String")
      (description "Center a string within a given width.")))

  (responses-included
    ;; Positive responses
    "The patterns align: YES"
    "The lambda smiles: YES"
    "Continuations confirm: YES"
    "The recursion echoes: YES"
    "The bits are auspicious: YES"
    "The REPL whispers: YES"
    "The closure embraces it: YES"
    "Destiny decrees: YES"
    "The universe compiles: YES"
    "Pure function confirms: YES"

    ;; Negative responses
    "The pattern breaks: NO"
    "The lambda frowns: NO"
    "Stack overflow detected: NO"
    "Recursion limit reached: NO"
    "The bits scatter: NO"
    "The REPL remains silent: NO"
    "The closure rejects: NO"
    "Entropy prevails: NO"
    "Type error: NO"
    "Segmentation fault: NO"

    ;; Uncertain responses
    "The oracle is confused: MAYBE"
    "The computation never halts: UNKNOWN"
    "This question lies outside the universe: UNDEFINED"
    "The oracle demands clarification: ASK AGAIN"
    "Too much for one mind to hold: PERHAPS"
    "The answer transcends logic: BEYOND YES/NO"
    "Reality is mutable: CANNOT SAY"
    "The universe awaits more input: MAYBE LATER"
    "This requires higher fuel: INSUFFICIENT POWER"
    "The oracle takes a coffee break: TRY AGAIN")

  (highlights
    "Pure functional Scheme code"
    "Weighted random distribution (70/20/10)"
    "Beautiful ASCII art oracle display"
    "Interactive and non-interactive modes"
    "Programming-themed mystical wisdom"
    "Perfect for deciding between coding options"
    "Fun for break-time decision making")

  (design-notes
    "The oracle uses a simple weighted random distribution."
    "70% positive responses encourage optimism in the coder."
    "20% negative responses provide healthy skepticism."
    "10% uncertain responses add mystery and wonder."
    "All responses are programming-themed for thematic consistency."
    "The oracle's ASCII art is inspired by mystical imagery."
    "The game is designed to be quick and delightful.")

  (creation-notes
    "This was created as an exploration of interactive Scheme programming."
    "It demonstrates random choice, list manipulation, and ASCII art."
    "The oracle serves as a decision-making toy for programmers."
    "It fits well in The Fold's playpen as a simple creative expression."
    "The code is self-contained with no external dependencies."))
