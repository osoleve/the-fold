;;; lattice/symbolic/manifest.sexp — Symbolic Computation Skill Manifest
;;;
;;; Computer algebra system: symbolic expressions, differentiation,
;;; simplification, integration, equation solving, and polynomial canonicalization.
;;; Extracted from fp skill (fold-zy24).

(skill symbolic
  (version "0.1.0")
  (path "lattice/symbolic")
  (purity total)
  (stability stable)
  (fuel-bound "O(n^2) simplification, O(n) differentiation")

  (deps (algebra egraph))  ; algebra for polynomial/field, egraph for egraph-simplify bridge

  (description
   "Symbolic computation and computer algebra. Provides symbolic expression
representation, differentiation, algebraic simplification, integration,
equation solving, and polynomial canonical form conversion. The egraph-simplify
module bridges to the e-graph subsystem for equality-saturation-based optimization.")

  (keywords (symbolic-computation computer-algebra differentiation
             integration simplification equation-solving polynomial))

  (aliases (symbolic cas))

  (concepts
    (concept symbolic-computation
      (description "Computer algebra: symbolic expressions, differentiation, integration, simplification, and equation solving.")
      (parent computation)
      (synonyms cas computer-algebra symbolic-math)))

  (exports
   (expr
     num var
     make-sum make-product make-diff make-neg make-div make-pow make-app
     sum product difference division power
     num? var? sum? product? difference? quotient?)

   (diff
     deriv partial gradient jacobian hessian
     deriv-n directional-derivative curl divergence laplacian)

   (simplify
     simplify simplify-step simplify-full)

   (integrate
     integrate definite-integral)

   (solve
     solve solve-system)

   (poly-canonical
     expr->polynomial polynomial->expr poly-simplify))

  (modules
   (expr "expr.ss" "Symbolic expression representation")
   (diff "diff.ss" "Symbolic differentiation")
   (simplify "simplify.ss" "Algebraic simplification")
   (integrate "integrate.ss" "Symbolic integration")
   (solve "solve.ss" "Symbolic equation solving")
   (poly-canonical "poly-canonical.ss" "Polynomial canonical form conversion")
   (egraph-simplify "egraph-simplify.ss" "E-graph based simplification bridge")))
