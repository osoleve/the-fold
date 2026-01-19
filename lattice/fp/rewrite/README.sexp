;;; core/fp/rewrite/README.sexp — Equational Reasoning Module
;;;
;;; This directory implements fold-com: Equational Reasoning + Rewrite Assistant

((module . "rewrite")
 (description . "Term rewriting system for algebraic transformations and law application")
 (author . "shepherd")
 (created . "2026-01-08")
 (status . "complete")

 (purpose . "Assist with algebraic/functional rewrites: apply laws (monoid, functor, monad), show transformation steps, and verify equivalence.")

 (files
  . ((rule.ss . "Rewrite rule data structures with metavariable patterns")
     (goals.ss . "Goal types and management for proof sketcher")
     (trace.ss . "Transformation trace structures for step-by-step display")
     (engine.ss . "Pattern matching, rule application, and strategy combinators")
     (laws.ss . "Standard FP law library (monoid, functor, monad, etc.)")
     (verify.ss . "Equivalence verification via alpha-normalization and NbE")
     (test-rewrite.ss . "Comprehensive test suite (72 tests)")))

 (dependencies
  . ((core/base/prelude.ss . "Base utilities")
     (core/blocks/normalize.ss . "De Bruijn normalization for alpha-equivalence")
     (core/lang/nbe.ss . "Normalization by Evaluation (optional advanced verification)")))

 (shell-integration . "boundary/tools/rewrite-repl.ss")

 (test-count . 72)

 (key-concepts
  . ((metavariables . "Pattern variables using (?name) syntax that match any subexpression")
     (rules . "LHS → RHS transformations with named pattern variables")
     (goals . "Proof objectives: eq-goal, assoc-goal, id-goal, inv-goal with decomposition")
     (strategies . "Composable rewrite tactics: seq, choice, try, repeat, topdown, bottomup")
     (traces . "Step-by-step records of rule applications")
     (verification . "Checking that rewrites preserve semantic equivalence")))

 (law-categories
  . ((monoid . "Identity and associativity: mempty <> x = x, (x <> y) <> z = x <> (y <> z)")
     (semigroup . "Associativity laws")
     (functor . "fmap id = id, fmap fusion")
     (applicative . "Identity, homomorphism, interchange")
     (monad . "Left identity, right identity, associativity")
     (lambda . "Beta and eta reduction")
     (arithmetic . "Identity elements for +, *, absorption by 0")
     (list . "map identity and fusion")))

 (usage-examples
  . (((comment . "Apply monad left identity law")
      (code . "(rewrite '(bind (pure x) f) 'monad-left-id)")
      (result . "(f x)"))

     ((comment . "Simplify arithmetic")
      (code . "(simplify '(+ 0 (* 1 x)))")
      (result . "x"))

     ((comment . "Check equivalence")
      (code . "(verify '(fn (a) a) '(fn (b) b))")
      (result . "Equivalent (alpha)"))

     ((comment . "List available laws")
      (code . "(laws 'monad)")
      (result . "monad-left-id, monad-right-id, monad-assoc, monad-assoc-rev"))

     ((comment . "Suggest applicable rules")
      (code . "(suggest-rules '(bind (pure x) f))")
      (result . "monad-left-id → (f x)"))))

 (blocks . "fold-com")
 (blocked-by . "fold-z7y (closed)")
 (blocking . ("fold-k1z" "fold-y2f")))
