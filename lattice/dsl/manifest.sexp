;;; lattice/dsl/manifest.sexp — Domain-Specific Language Tools Skill Manifest

(skill dsl
  (version "0.1.0")
  (tier 1)
  (path "lattice/dsl")
  (purity total)
  (stability stable)
  (fuel-bound "O(n) for interpretation, O(n^2) for pattern compilation, O(n) for PE")
  (deps (data))

  (description
   "Tools for building domain-specific languages using tagless final style,
    pattern matching compilation, partial evaluation, functor composition,
    reader macros, quasiquotation, and multi-stage programming.")

  (keywords (dsl tagless-final pattern-matching partial-evaluation
             functor-composition reader-macros quasiquote staging
             meta-programming language-design interpreters))
  (aliases (meta-dsl language-tools tagless))

  (exports
   (tagless make-dict dict-ref dict-extend dict-merge
            make-expr-dict expr-lit expr-add expr-neg eval-expr-dict
            make-bool-dict bool-lit bool-and bool-or bool-not
            make-cond-dict cond-if make-let-dict tl-let tl-var
            make-stateful-dict state-get state-put state-pure state-bind
            make-chronicle-dict ch-scene ch-choice ch-start ch-choose
            with-logging with-tracing with-memoization
            tagless-to-free free-to-tagless optimize-expr)
   (match make-clause parse-pattern-extended compile-match compile-match-expr
          make-first-class-pattern register-pattern! lookup-pattern
          pat-and pat-not pat-repeat register-active-pattern!)
   (partial-eval partial-eval partial-eval-offline specialize specialize-function
                 memo-specialize futamura-1 simplify bta bta-annotate
                 bt-static bt-dynamic pe-online pe-offline)
   (compose inl inr inl? inr? coproduct-fmap make-functor-row
            functor-row-extend functor-row-union inject
            make-handler-stack run-with-stack combine-effect-handlers
            make-interface dict-satisfies? assert-interface
            make-interpreter combine-interpreters
            make-transformer chain-transformers extend-dsl restrict-dsl
            tagless->free free->tagless verify-composition)
   (reader make-readtable readtable-extend readtable-lookup
           make-reader-macro vector-reader-macro matrix-reader-macro
           regex-reader-macro set-reader-macro hash-reader-macro
           default-readtable expand-reader-macro compose-readtables)
   (quasi quasiquote? unquote? unquote-splicing? qq-expand expand-quasiquote
          make-syntax syntax-datum syntax-source datum->syntax syntax->datum
          syntax-match pattern-match instantiate-template)
   (staging make-code code? code-stage code-expr stage-expand
            code-combine code-lift code-unlift code-well-staged?
            stage-add stage-mul stage-if stage-let stage-lambda stage-app
            power-staged power-specialized compile-staged run-staged
            stage-match stage-fix infer-staged-type inline-staged)
   (template hole? hole-name find-holes new-template template?
             template-expr template-holes template-complete? template-hole-count
             fill-hole fill-holes compile-template try-compile-template
             template->string template-status holes->string wrap-if-multiple))

  (modules
   (tagless "tagless.ss" "Tagless final style interpreter/compiler composition")
   (match "match.ss" "Pattern matching compilation to decision trees")
   (partial-eval "partial-eval.ss" "Online and offline partial evaluation")
   (compose "compose.ss" "Functor row composition and effect handling")
   (reader "reader.ss" "Extensible reader macros and readtables")
   (quasi "quasi.ss" "Quasiquotation expansion and syntax objects")
   (staging "staging.ss" "Multi-stage programming with typed code")
   (template "template/template.ss" "Grammar-driven code construction with holes")))
