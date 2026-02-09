;;; README.sexp --- The Fold Examples Gallery
;;;
;;; A curated collection of idiomatic examples demonstrating The Fold's
;;; content-addressable, homoiconic programming model.
;;;
;;; Run any example with: scheme --script docs/tutorials/examples/<category>/<file>.ss

(readme
 (title "The Fold Examples Gallery")
 (description
  "Learn The Fold through working code. Each example is self-contained,
   well-commented, and demonstrates idiomatic patterns.")

 (categories
  ((name "getting-started")
   (description "First steps: expressions, functions, and basic IO")
   (difficulty beginner)
   (examples
    ("hello-world.ss" . "The classic first program")
    ("basic-math.ss" . "Arithmetic and numeric operations")
    ("functions.ss" . "Defining and calling functions")
    ("conditionals.ss" . "if, cond, and boolean logic")))

  ((name "data-structures")
   (description "Lists, vectors, pairs, and blocks")
   (difficulty beginner)
   (examples
    ("lists.ss" . "List construction and manipulation")
    ("vectors.ss" . "Fixed-size mutable vectors")
    ("pairs-and-alists.ss" . "Association lists and key-value data")
    ("blocks.ss" . "The universal primitive: tag, payload, refs")))

  ((name "patterns")
   (description "Recursion, higher-order functions, and idiomatic patterns")
   (difficulty intermediate)
   (examples
    ("recursion.ss" . "Factorial, fibonacci, and recursive thinking")
    ("higher-order.ss" . "map, filter, fold, and compose")
    ("tail-recursion.ss" . "Efficient iteration via tail calls")
    ("pattern-matching.ss" . "Destructuring with match")))

  ((name "content-addressing")
   (description "The heart of The Fold: content-addressed storage")
   (difficulty intermediate)
   (examples
    ("store-and-fetch.ss" . "Storing and retrieving blocks by hash")
    ("building-chains.ss" . "Linked structures via content references")
    ("deduplication.ss" . "How identical content shares identity")))

  ((name "scientific-computing")
   (description "Autodiff, optimization, and symbolic computation")
   (difficulty advanced)
   (see "scientific-computing/README.sexp")))

 (how-to-run
  "Each example can be run directly:

     scheme --script docs/tutorials/examples/getting-started/hello-world.ss

   Or load interactively in the REPL:

     (load \"docs/tutorials/examples/getting-started/hello-world.ss\")")

 (conventions
  ((file-structure "Each file is self-contained with all needed loads")
   (comments ";;; for section headers, ;; for explanations, ; for inline")
   (output "Examples print their results for verification")
   (fuel "Examples use reasonable fuel bounds, noted when relevant")))

 (see-also
  ("docs/tutorials/" . "Step-by-step tutorials")
  ("docs/technical-report.md" . "Technical report")
  ("user/" . "Playground with experiments and demos")))
