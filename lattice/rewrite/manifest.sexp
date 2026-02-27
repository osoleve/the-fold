;;; lattice/rewrite/manifest.sexp — Term Rewriting Skill Manifest
;;;
;;; Pattern matching, rewriting, proof tactics, fusion rules, and program sketching.
;;; Extracted from fp skill (fold-zy24).

(skill rewrite
  (version "0.1.0")
  (path "lattice/rewrite")
  (purity total)
  (stability stable)
  (fuel-bound "O(n*r) per rewrite pass where n=term size, r=rules")

  (deps (fp data))  ; fp for combinators/tree-zipper/zipper-lens; data for hamt

  (description
   "Term rewriting systems and equational reasoning. Provides pattern matching
and rewriting engine, algebraic law definitions, fusion optimization rules,
proof tactics for equational reasoning, program sketching with holes, and
S-expression zipper navigation. Infrastructure for symbolic manipulation
used by category theory (free algebras) and DSL template systems.")

  (keywords (term-rewriting pattern-matching proof-tactics fusion
             equational-reasoning program-sketching rewrite-rules))

  (aliases (rewrite term-rewrite))

  (concepts
    (concept rewriting
      (description "Term rewriting systems, rewrite rules, fusion optimization, and proof tactics for equational reasoning.")
      (parent computation)
      (synonyms term-rewriting rewrite-rules pattern-matching)))

  (exports
   (rule
     make-rule rule? rule-name rule-lhs rule-rhs
     rule-category rule-direction rule-conditions
     metavar? metavar-name metavar-constraint
     pattern-vars pattern-vars-unique rule->string valid-rule?)

   (engine
     match-pattern substitute-template
     apply-rule apply-rule-name apply-rule-at apply-rule-anywhere
     find-all-positions
     id-strategy fail-strategy seq choice try repeat repeat-n map-children))

  (modules
   (rule "rule.ss" "Rule data structures and accessors")
   (trace "trace.ss" "Rewrite trace visualization")
   (engine "engine.ss" "Pattern matching and rewriting engine")
   (rewrite/sexp-zipper "sexp-zipper.ss" "S-expression zipper for term navigation")
   (verify "verify.ss" "Rule verification")
   (goals "goals.ss" "Goal-directed rewriting")
   (rewrite/laws "laws.ss" "Algebraic laws (monoid, functor, monad)")
   (rewrite/fusion-rules "fusion-rules.ss" "Fusion optimization rules")
   (rewrite/proof-tactics "proof-tactics.ss" "Proof tactics for equational reasoning")
   (rewrite/sketch "sketch.ss" "Program sketching with holes")))
