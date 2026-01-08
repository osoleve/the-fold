((name "parsing")
 (description "Parser combinators and finite state machines for text processing.
  Provides composable parsing primitives that can be combined to build
  complex parsers from simple pieces.")

 (modules
  ((file "parser.ss")
   (purpose "Monadic parser combinators")
   (exports
    ;; Core types
    (make-parser "Wrap a parsing function into a parser")
    (run-parser "Execute parser on input string")
    (parse-result "Extract successful parse result")
    (parse-remaining "Extract remaining unparsed input")
    (parse-success? "Test for successful parse")
    (parse-failure? "Test for failed parse")
    (parse-error "Get error message from failed parse")
    ;; Primitive parsers
    (parse-char "Parse a specific character")
    (parse-any-char "Parse any single character")
    (parse-satisfy "Parse character satisfying predicate")
    (parse-string "Parse exact string literal")
    (parse-eof "Parse end of input")
    (parse-epsilon "Parse empty string (always succeeds)")
    ;; Character class parsers
    (parse-digit "Parse a digit 0-9")
    (parse-letter "Parse a letter a-z A-Z")
    (parse-alphanumeric "Parse letter or digit")
    (parse-whitespace "Parse single whitespace char")
    (parse-spaces "Parse zero or more spaces")
    (parse-spaces1 "Parse one or more spaces")
    ;; Combinators
    (parse-map "Transform result: (parse-map f p)")
    (parse-bind "Sequence parsers: (parse-bind p f)")
    (parse-pure "Lift value into parser")
    (parse-fail "Parser that always fails")
    (parse-or "Try first parser, then second on failure")
    (parse-seq "Sequence parsers, return list of results")
    (parse-choice "Try parsers in order until one succeeds")
    (parse-many "Zero or more repetitions")
    (parse-many1 "One or more repetitions")
    (parse-optional "Zero or one occurrence")
    (parse-sep-by "Parse items separated by delimiter")
    (parse-sep-by1 "One or more items separated by delimiter")
    (parse-between "Parse content between open and close")
    (parse-not "Negative lookahead")
    (parse-lookahead "Positive lookahead (doesn't consume)")
    (parse-try "Backtracking parser")
    ;; Numeric parsers
    (parse-integer "Parse integer (with optional sign)")
    (parse-decimal "Parse decimal number")
    (parse-number "Parse integer or decimal"))
   (example
    ";; Parse comma-separated integers
     (define csv-int
       (parse-sep-by parse-integer (parse-char #\\,)))
     (run-parser csv-int \"1,2,3,4,5\")
     ;; => (success (1 2 3 4 5) \"\")

     ;; Parse simple arithmetic expression
     (define expr
       (parse-or
         (parse-bind parse-integer
           (lambda (x)
             (parse-bind (parse-char #\\+)
               (lambda (_)
                 (parse-map (lambda (y) (+ x y))
                            parse-integer)))))
         parse-integer))
     (run-parser expr \"3+4\")
     ;; => (success 7 \"\")"))

  ((file "fsm.ss")
   (purpose "Finite state machines for pattern recognition")
   (exports
    (make-fsm "Create FSM from states and transitions")
    (fsm-add-state "Add a state to FSM")
    (fsm-add-transition "Add transition on symbol")
    (fsm-set-accepting "Mark state as accepting")
    (fsm-run "Run FSM on input sequence")
    (fsm-accepts? "Test if FSM accepts input")
    (fsm-current-state "Get current state during run")
    (nfa->dfa "Convert NFA to DFA via subset construction")
    (fsm-minimize "Minimize DFA states")
    (fsm-union "Union of two FSMs")
    (fsm-concat "Concatenation of two FSMs")
    (fsm-star "Kleene star of an FSM")
    (regex->nfa "Compile regex to NFA"))))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("core/base/error.ss" "Error handling")
  ("core/fp/meta/combinators.ss" "Maybe type for optional results"))

 (notes
  "Parser combinators provide a compositional approach to parsing:
   build complex parsers by combining simple primitives with
   sequencing, choice, and repetition combinators.

   The parsers are monadic: parse-bind sequences parsers where
   later parsers can depend on earlier results. parse-or provides
   backtracking choice.

   For performance-critical parsing of regular languages, the FSM
   module provides compiled finite automata that can be minimized
   and composed."))
