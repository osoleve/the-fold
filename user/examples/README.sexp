;;; user/examples/README.sexp — Examples Gallery
;;;
;;; Curated collection of idiomatic Fold examples.
;;; Each example includes commented code, expected output, and is runnable in the REPL.

(directory examples
  (purpose "Interactive code gallery for learning Fold idioms")

  (usage
    "(load \"user/examples/examples.ss\")"
    "(examples)           ; List all available examples"
    "(example 'factorial) ; Run a specific example"
    "(example-code 'name) ; Get code for copying")

  (categories
    (getting-started "Hello world, basic math, local bindings")
    (recursion "Factorial, fibonacci, tail recursion")
    (higher-order "Map, filter, fold, composition, currying")
    (data-structures "Lists, vectors, association lists")
    (content-addressing "Store, fetch, block chains"))

  (files
    (examples.ss "Main examples module with registry and runner")))
