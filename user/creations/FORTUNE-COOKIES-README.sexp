(fortune-cookies-documentation
  (title "Scheme Wisdom Fortune Cookies")
  (author "Utility Crafter")
  (created "2025-12-26")
  (description
    "An interactive utility that cracks open digital fortune cookies filled with"
    "Scheme programming wisdom, Lisp philosophy, and functional programming"
    "enlightenment! Each cookie contains a fortune about Scheme, continuations,"
    "recursion, lambda expressions, and the beauty of functional programming.")

  (features
    "30+ unique Scheme-themed fortunes"
    "Beautiful ASCII art fortune cookies"
    "Interactive shell with multiple commands"
    "Random fortune selection"
    "Batch spinning (get 3 random cookies at once)"
    "Well-documented Scheme code with type signatures")

  (usage
    (title "Loading the Module")
    (code "(load \"user/creations/fortune-cookies.ss\")")
    (description "Load the module into your Scheme environment."))

    (title "Getting a Single Fortune")
    (code "(display-cookie (random-fortune))")
    (description "Display a single random fortune cookie with ASCII art."))

    (title "Interactive Shell")
    (code "(fortune-shell)")
    (description "Start the interactive fortune cookie generator shell.")
    (commands
      "(fortune)  - Get a new fortune cookie"
      "(spin)     - Spin the wheel (3 random cookies)"
      "(quit)     - Exit the cookie jar"))

    (title "Direct Cookie Display")
    (code "(display-cookie \"Your custom fortune text here\")")
    (description "Display any custom text in a fortune cookie format."))

  (examples
    (example-1
      (description "Get a random fortune")
      (code
        "(load \"user/creations/fortune-cookies.ss\")"
        "(display-cookie (random-fortune))"))

    (example-2
      (description "Use the interactive shell")
      (code
        "(load \"user/creations/fortune-cookies.ss\")"
        "(fortune-shell)"))

    (example-3
      (description "Create your own fortune cookie")
      (code
        "(load \"user/creations/fortune-cookies.ss\")"
        "(display-cookie \"May your lambda expressions be pure and your cons cells eternal\")")))

  (fortunes-included
    "In Scheme, all is list. All is function. All is possibility."
    "A continuation is a reified return address. Use it wisely."
    "Why mutate when you can transform? Immutability is enlightenment."
    "Let over lambda: write it down, bind it hard."
    "The lambda calculus is complete. Everything else is commentary."
    "Recursion is the spiral of thought made manifest in code."
    "car and cdr: the heartbeat of Lisp programs everywhere."
    "First-class functions: treat them as data, and they will set you free."
    "cons cells are the atoms of the Lisp universe."
    "A macro is code that writes code. Be humble, be wise."
    "The empty list is both nothing and everything. Ponder this."
    "To quote is to defer. To unquote is to unleash power."
    "Higher-order functions: when your functions have functions."
    "Tail recursion: eliminate the stack, embrace the loop."
    "The REPL is your meditation chamber. Type, think, evolve."
    "Functional purity: a promise you make to yourself."
    "Pattern matching is life's greatest joy. Master it."
    "Closure: carrying your environment with you, always."
    "A deep test of your understanding: write a Y combinator."
    "Lexical scope is your shield against chaos."
    "Dynamic scope: with great power comes great responsibility."
    "The call stack is sacred. Respect it, or optimize it away."
    "Continuations make time travel possible. Use them carefully."
    "Error handling: exceptions are not the Scheme way."
    "A pure function is reproducible. A pure function is testable."
    "Refactoring: small changes, big impact. Be patient."
    "Comments are love letters to your future self."
    "Functional composition: elegant, powerful, eternal."
    "A list is just nested pairs. Simple. Beautiful. Infinite."
    "The best code is the code you don't write.")

  (functions
    (function
      (name "display-cookie")
      (signature "String → void")
      (description "Display a fortune in beautiful ASCII art cookie format."))

    (function
      (name "random-fortune")
      (signature "→ String")
      (description "Select and return a random fortune from the database."))

    (function
      (name "fortune-shell")
      (signature "→ void")
      (description "Start the interactive fortune cookie generator shell."))

    (function
      (name "string-split-simple")
      (signature "String → (List String)")
      (description "Split a string by spaces into a list of words."))

    (function
      (name "take-words")
      (signature "(List String) × Nat → String")
      (description "Take the first n words and join them with spaces."))

    (function
      (name "drop-words")
      (signature "(List String) × Nat → (List String)")
      (description "Drop the first n words from a list."))

    (function
      (name "string-join")
      (signature "(List String) → String")
      (description "Join a list of strings with spaces."))

    (function
      (name "pad-string")
      (signature "String × Nat → String")
      (description "Pad a string to a given length with spaces on the right.")))

  (highlights
    "Pure functional Scheme code"
    "No external dependencies"
    "Well-documented with type signatures"
    "Beautiful ANSI art display"
    "Interactive and non-interactive modes"
    "Great for meditation or inspiration during coding sessions")

  (fun-facts
    "Each fortune was carefully crafted to reflect Scheme wisdom"
    "The cookies are inspired by real fortune cookies but filled with functional wisdom"
    "Use this when you need a quick bit of inspiration or motivation"
    "Perfect for sharing with fellow Schemers and Lispers!"))
