;;; lattice/algebra/test-export-surface-properties.ss — QuickCheck export-surface wiring for algebra

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'group)
(require 'ring)
(require 'field)
(require 'polynomial)
(require 'multivariate)
(require 'groebner)
(require 'poly-bridge)
(require 'galois)
(require 'tropical)

;;; ============================================================================
;;; Export Surface
;;; ============================================================================

;; Keep this list aligned with lattice/algebra/manifest.sexp exports.
(define algebra-export-groups
  '((group
      make-group group? group-elements group-op group-identity
      group-inverse-fn group-equal-fn group-order group-compose
      group-inverse group-power group-equal? element-order
      verify-closure verify-associativity verify-identity
      verify-inverses verify-group-axioms
      make-cyclic-group Z cyclic-generator
      permutation-apply permutation-compose permutation-inverse
      permutation-identity permutation-equal? all-permutations
      make-symmetric-group S cycle-to-permutation transposition
      permutation-parity make-dihedral-group D
      is-subgroup? generate-subgroup make-homomorphism
      homomorphism? homomorphism-source homomorphism-target
      homomorphism-mapping homomorphism-apply verify-homomorphism
      is-isomorphism? kernel image cayley-table)

    (ring
      make-ring ring? ring-elements ring-add-op ring-mul-op
      ring-zero ring-one ring-neg-fn ring-equal-fn ring-order
      ring-add ring-mul ring-neg ring-sub ring-equal?
      ring-power ring-sum ring-product
      is-commutative-ring? is-zero-divisor? has-zero-divisors?
      is-integral-domain? is-unit? ring-units
      make-ring-zn make-ring-z
      make-ring-homomorphism ring-hom? ring-hom-source
      ring-hom-target ring-hom-phi ring-hom-apply
      is-valid-ring-homomorphism? ring-hom-kernel ring-hom-image
      make-ideal ideal? ideal-ring ideal-elements is-valid-ideal?
      make-principal-ideal ideal-sum ideal-product
      is-prime-ideal? is-maximal-ideal?)

    (field
      make-field field? field-elements field-add-op field-mul-op
      field-zero field-one field-neg-fn field-div-fn field-equal-fn
      field-add field-mul field-neg field-sub field-div field-inv
      field-equal? field-power field->ring
      Q-field R-field make-field-zp mod-inverse)

    (polynomial
      make-polynomial polynomial? poly-field poly-ring poly-coeffs
      poly-degree poly-leading-coeff poly-coeff-at poly-zero?
      poly-zero-over poly-one-over poly-constant poly-monomial poly-x
      poly-add poly-neg poly-sub poly-scale poly-mul poly-power
      poly-equal? poly-divmod poly-div poly-mod poly-divides?
      poly-gcd poly-make-monic poly-extended-gcd poly-lcm
      poly-eval poly-derivative poly-square-free
      poly-square-free-factorization
      poly-lagrange-interpolate poly-newton-interpolate
      make-polynomial-ring poly->string)

    (multivariate
      make-monomial mono-one mono-var mono-power
      mono-degree mono-degree-in mono-vars
      mono-mul mono-divides? mono-div mono-lcm mono-gcd mono-equal?
      mono-compare-lex mono-compare-grlex mono-compare-grevlex
      make-ordering
      make-mpoly mpoly? mpoly-field mpoly-ring mpoly-vars mpoly-ordering mpoly-terms
      mpoly-zero mpoly-one mpoly-constant mpoly-var mpoly-from-terms
      mpoly-zero? mpoly-degree mpoly-degree-in
      mpoly-leading-term mpoly-leading-coeff mpoly-leading-mono
      mpoly-add mpoly-neg mpoly-sub mpoly-scale mpoly-mul-term
      mpoly-mul mpoly-power mpoly-equal? mpoly-divmod
      mpoly-eval mpoly->string)

    (groebner
      s-polynomial reduce-poly reduce-poly-full
      buchberger reduce-basis mpoly-make-monic
      minimize-basis interreduce
      ideal-membership? solve-system normal-form
      is-groebner-basis? groebner->string)

    (poly-bridge
      numeric->algebra numeric->algebra-with-field algebra->numeric
      alg-make alg-coeffs alg-degree alg-field alg-zero? alg-leading
      alg-add alg-sub alg-mul alg-scale alg-divmod alg-div alg-mod
      alg-gcd alg-extended-gcd alg-derivative alg-eval alg-monic
      alg-poly-from-roots
      bridge-gcd bridge-divmod bridge-simplify bridge-coprime?
      make-numeric-from-roots make-numeric-from-ascending make-algebra-from-descending
      alg->string numeric->string
      sig-make-polynomial sig-poly-field sig-poly-coeffs sig-poly-degree
      sig-poly-zero? sig-poly-leading sig-poly-add sig-poly-sub
      sig-poly-mul sig-poly-scale sig-poly-div sig-poly-mod
      sig-poly-gcd sig-poly-divmod)

    (galois
      gf-prime gf-order gf-characteristic
      make-gf-extension make-gf-extension-large
      gf-ext-element? gf-ext-coeffs gf-ext-to-poly
      make-gf-ext-element gf-ext-base-field gf-ext-modulus
      make-gf2n make-gf2n-large
      gf2n-element? gf2n-value gf2n-degree gf2n-irred
      gf2n-from-int gf2n-to-int gf2n-to-poly
      make-gf2-4 make-gf2-8-aes make-gf2-16 make-gf2-32 make-gf-p2
      gf2-4-irred gf2-8-aes-irred gf2-16-irred gf2-32-irred
      poly-irreducible? rabin-irreducible? poly-power-mod poly-frobenius-power
      gf-primitive? gf-find-primitive
      gf-minimal-poly
      gf-trace gf-norm)

    (field-extra
      make-field* field-metadata field-meta-ref
      make-field-zp-large)

    (tropical
      make-semiring semiring? semiring-add semiring-mul semiring-zero semiring-one semiring-eq?
      min-plus-semiring max-plus-semiring
      tropical-add tropical-mul tropical-power
      tropical-matrix-identity tropical-matrix-mul tropical-matrix-add
      tropical-matrix-power tropical-matrix-closure
      tropical-eigenvalue tropical-critical-edges
      tropical-poly-eval tropical-poly-roots newton-polygon
      adjacency->tropical tropical->adjacency)))

(define (collect-export-symbols groups)
  (let outer ([gs groups] [acc '()])
    (if (null? gs)
        (reverse acc)
        (let inner ([syms (cdr (car gs))] [acc* acc])
          (if (null? syms)
              (outer (cdr gs) acc*)
              (inner (cdr syms) (cons (car syms) acc*)))))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-small-int
  (gen-int-range -20 20))

(define gen-z11
  (gen-int-range 0 10))

(define gen-z11-nonzero
  (gen-int-range 1 10))

;; Shared finite instances to avoid repeated expensive construction inside properties.
(define G11 (Z 11))
(define R11 (make-ring-zn 11))
(define F11 (make-field-zp 11))
(define GF7 (gf-prime 7))
(define GF2-8-AES (make-gf2-8-aes))
(define SR-MINPLUS min-plus-semiring)

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group algebra-export-surface-properties

  (define-property "manifest algebra export surface list is present and comprehensive"
    (gen-pure #t)
    (lambda (_)
      (let ([syms (collect-export-symbols algebra-export-groups)])
        (> (length syms) 300)))
    'tests 3)

  (define-property "group/ring/field core operations are coherent on small finite instances"
    (gen-bind gen-z11
      (lambda (a)
        (gen-bind gen-z11
          (lambda (b)
            (gen-map (lambda (c) (list a b c)) gen-z11-nonzero)))))
    (lambda (abc)
      (let* ([a (car abc)]
             [b (cadr abc)]
             [c (caddr abc)])
        (and (group? G11)
             (ring? R11)
             (field? F11)
             (= (group-compose G11 a b) (modulo (+ a b) 11))
             (= (ring-sub R11 a b) (modulo (- a b) 11))
             (= (field-mul F11 c (field-inv F11 c)) 1))))
    'tests 32)

  (define-property "polynomial and bridge helpers produce consistent algebraic values"
    gen-small-int
    (lambda (n)
      (let* ([p (make-polynomial Q-field (list 1 n))]
             [q (poly-add p p)]
             [x 2]
             [pv (poly-eval p x)]
             [qv (poly-eval q x)]
             [num (make-numeric-from-ascending (list 1 n 1))]
             [alg (numeric->algebra num)]
             [num-back (algebra->numeric alg)])
        (and (polynomial? p)
             (= qv (+ pv pv))
             (string? (alg->string alg 'x))
             (string? (numeric->string num-back 'x)))))
    'tests 24)

  (define-property "galois and tropical helpers are well-formed on representative values"
    (gen-bind (gen-int-range 0 63)
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) (gen-int-range 0 63))))
    (lambda (ab)
      (let* ([a (car ab)]
             [b (cdr ab)]
             [ea (gf2n-from-int GF2-8-AES a)]
             [eb (gf2n-from-int GF2-8-AES b)]
             [sum (field-add GF2-8-AES ea eb)]
             [tadd (tropical-add SR-MINPLUS a b)]
             [tmul (tropical-mul SR-MINPLUS a b)])
        (and (field? GF7)
             (gf2n-element? ea)
             (integer? (gf2n-to-int sum))
             (semiring? SR-MINPLUS)
             (number? tadd)
             (number? tmul))))
    'tests 24)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
