;;; multivariate.sexp — Development tasks for lattice/algebra/multivariate.ss
;;;
;;; Module: multivariate (tier 0, depends on prelude, field, sort)
;;; 707 lines, ~35 exported functions
;;; Multivariate polynomial algebra: sparse monomial representation,
;;; monomial orderings (lex, grlex, grevlex), polynomial arithmetic,
;;; multivariate division algorithm. Foundation for Groebner bases.
;;;
;;; Git history:
;;;   a3abed54 refactor: Reorganize into core/lattice/shell three-layer architecture
;;;   710fc341 feat(module): migrate lattice/algebra/ to require

(tasks

  ;; ──────────────────────────────────────────────
  ;; Task 1: Implement mono-degree
  ;; ──────────────────────────────────────────────
  ;; Total degree of a monomial.

  (task
    (id "algebra/multivariate/mono-degree-impl")
    (module multivariate)
    (tier 0)
    (type implement)
    (description "Implement mono-degree: compute the total degree of a monomial. A monomial is represented as an association list of (variable . exponent) pairs — for example, x^2*y*z^3 is ((x . 2) (y . 1) (z . 3)). The total degree is the sum of all exponents. The constant monomial 1 is the empty list () and has degree 0. Use map to extract the exponents (cdr of each pair), then apply + to sum them.")
    (context "Available: apply, +, map, cdr. Single expression: (apply + (map cdr m)).")
    (before "(define (mono-degree m)
  ;; TODO: sum of exponents
  0)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; x^2*y*z^3 has total degree 6
(assert-equal 6 (mono-degree (make-monomial '((x . 2) (y . 1) (z . 3)))))"
                 ";; Constant 1 has degree 0
(assert-equal 0 (mono-degree (mono-one)))"
                 ";; Single variable has degree 1
(assert-equal 1 (mono-degree (mono-var 'x)))"
                 ";; x^5 has degree 5
(assert-equal 5 (mono-degree (mono-power 'x 5)))"))
    (available-modules (prelude field sort))
    (hints ("Exponents are the cdr of each pair: (map cdr m)"
            "Sum: (apply + exponents)"
            "Empty list: (apply + '()) = 0, which is correct for degree of 1"))
    (reference-solution "(define (mono-degree m)
  (apply + (map cdr m)))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

  ;; ──────────────────────────────────────────────
  ;; Task 2: Implement mono-divides?
  ;; ──────────────────────────────────────────────
  ;; Check if one monomial divides another.

  (task
    (id "algebra/multivariate/mono-divides-impl")
    (module multivariate)
    (tier 0)
    (type implement)
    (description "Implement mono-divides?: check whether monomial m1 divides monomial m2. Monomial m1 divides m2 iff every variable in m1 appears in m2 with an exponent at least as large. Both monomials are sorted alists of (variable . exponent) pairs. Walk both lists in parallel: if m1 is empty, return #t (1 divides everything). If m2 is empty but m1 is not, return #f. Compare the current variables using symbol<? to maintain sorted order. If the same variable appears in both, check that the exponent in m1 is <= the exponent in m2. If a variable in m1 doesn't appear in m2 (m1's variable comes first in sort order), return #f.")
    (context "Available: null?, caar, cdar, cdr, symbol<?, <=, and, cond, let. Named let with parallel traversal of sorted alists.")
    (before "(define (mono-divides? m1 m2)
  ;; TODO: does m1 divide m2?
  #f)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; x divides x^2*y
(assert-true (mono-divides? (make-monomial '((x . 1))) (make-monomial '((x . 2) (y . 1)))))"
                 ";; x^3 does not divide x^2
(assert-false (mono-divides? (make-monomial '((x . 3))) (make-monomial '((x . 2)))))"
                 ";; 1 divides everything
(assert-true (mono-divides? (mono-one) (make-monomial '((x . 5)))))"
                 ";; z does not divide x^2 (different variable)
(assert-false (mono-divides? (make-monomial '((z . 1))) (make-monomial '((x . 2)))))"))
    (available-modules (prelude field sort))
    (hints ("Named let: (let loop ([m1 m1] [m2 m2]) ...)"
            "Base: (null? m1) -> #t; (null? m2) -> #f"
            "Extract: (let ([v1 (caar m1)] [e1 (cdar m1)] [v2 (caar m2)] [e2 (cdar m2)]) ...)"
            "Compare vars: (symbol<? v1 v2) -> #f (v1 not in m2); (symbol<? v2 v1) -> skip v2; same -> check (<= e1 e2)"))
    (reference-solution "(define (mono-divides? m1 m2)
  (let loop ([m1 m1] [m2 m2])
    (cond
      [(null? m1) #t]
      [(null? m2) #f]
      [else
       (let ([v1 (caar m1)] [e1 (cdar m1)]
             [v2 (caar m2)] [e2 (cdar m2)])
         (cond
           [(symbol<? v1 v2) #f]
           [(symbol<? v2 v1) (loop m1 (cdr m2))]
           [else (and (<= e1 e2) (loop (cdr m1) (cdr m2)))]))])))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 3: Implement mono-lcm
  ;; ──────────────────────────────────────────────
  ;; Least common multiple of two monomials.

  (task
    (id "algebra/multivariate/mono-lcm-impl")
    (module multivariate)
    (tier 0)
    (type implement)
    (description "Implement mono-lcm: compute the least common multiple of two monomials. The LCM takes the maximum exponent for each variable that appears in either monomial. For example, lcm(x^2*y, x*z^3) = x^2*y*z^3. Both monomials are sorted alists. Walk them in parallel: if one is empty, append the other. If the same variable appears in both, take max of exponents. If different variables, take the one that comes first in sort order and recurse. This is used in Buchberger's algorithm to compute S-polynomials.")
    (context "Available: null?, caar, cdar, cdr, cons, max, symbol<?, cond, let. Recursive merge of sorted alists taking max exponents.")
    (before "(define (mono-lcm m1 m2)
  ;; TODO: max of each exponent
  '())")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; lcm(x^2*y, x*z^3) = x^2*y*z^3
(assert-equal '((x . 2) (y . 1) (z . 3))
  (mono-lcm (make-monomial '((x . 2) (y . 1)))
            (make-monomial '((x . 1) (z . 3)))))"
                 ";; lcm(1, m) = m
(assert-equal '((x . 3))
  (mono-lcm (mono-one) (make-monomial '((x . 3)))))"
                 ";; lcm(m, m) = m
(let ([m (make-monomial '((x . 2) (y . 1)))])
  (assert-equal m (mono-lcm m m)))"
                 ";; Disjoint variables: union
(assert-equal '((x . 1) (y . 1))
  (mono-lcm (make-monomial '((x . 1))) (make-monomial '((y . 1)))))"))
    (available-modules (prelude field sort))
    (hints ("Base: (null? m1) -> m2; (null? m2) -> m1"
            "Extract vars and exponents from both heads"
            "If v1 < v2: take (v1 . e1), recurse on (cdr m1) and m2"
            "If v2 < v1: take (v2 . e2), recurse on m1 and (cdr m2)"
            "Same variable: (cons (cons v1 (max e1 e2)) (mono-lcm (cdr m1) (cdr m2)))"))
    (reference-solution "(define (mono-lcm m1 m2)
  (cond
    [(null? m1) m2]
    [(null? m2) m1]
    [else
     (let ([v1 (caar m1)] [e1 (cdar m1)]
           [v2 (caar m2)] [e2 (cdar m2)])
       (cond
         [(symbol<? v1 v2)
          (cons (car m1) (mono-lcm (cdr m1) m2))]
         [(symbol<? v2 v1)
          (cons (car m2) (mono-lcm m1 (cdr m2)))]
         [else
          (cons (cons v1 (max e1 e2))
                (mono-lcm (cdr m1) (cdr m2)))]))]))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 4: Implement mono-compare-lex
  ;; ──────────────────────────────────────────────
  ;; Lexicographic monomial ordering.

  (task
    (id "algebra/multivariate/mono-compare-lex-impl")
    (module multivariate)
    (tier 0)
    (type implement)
    (description "Implement mono-compare-lex: compare two monomials in lexicographic order with respect to a given variable ordering. Lexicographic order compares the exponent of the first variable; if equal, moves to the second variable, and so on. Returns 1 if m1 > m2, -1 if m1 < m2, 0 if equal. For variable ordering (x y z): x^2*y > x*y^5 because the x-exponent is larger. Walk through the variable list, comparing the degree of each variable in both monomials using mono-degree-in. Return as soon as a difference is found.")
    (context "Available: null?, car, cdr, mono-degree-in, >, <, cond, let. Named let over variable list.")
    (before "(define (mono-compare-lex m1 m2 vars)
  ;; TODO: lexicographic comparison
  0)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; x^2 > x*y^3 in lex with vars (x y z)
(assert-equal 1 (mono-compare-lex (make-monomial '((x . 2))) (make-monomial '((x . 1) (y . 3))) '(x y z)))"
                 ";; Equal monomials
(assert-equal 0 (mono-compare-lex (make-monomial '((x . 1))) (make-monomial '((x . 1))) '(x y z)))"
                 ";; y < x in lex
(assert-equal -1 (mono-compare-lex (make-monomial '((y . 1))) (make-monomial '((x . 1))) '(x y z)))"))
    (available-modules (prelude field sort))
    (hints ("Named let over vars: (let loop ([vs vars]) ...)"
            "Base: (null? vs) -> 0 (equal)"
            "Get exponents: (let ([e1 (mono-degree-in m1 v)] [e2 (mono-degree-in m2 v)]) ...)"
            "Compare: (> e1 e2) -> 1, (< e1 e2) -> -1, else -> (loop (cdr vs))"))
    (reference-solution "(define (mono-compare-lex m1 m2 vars)
  (let loop ([vs vars])
    (if (null? vs)
        0
        (let ([v (car vs)])
          (let ([e1 (mono-degree-in m1 v)]
                [e2 (mono-degree-in m2 v)])
            (cond
              [(> e1 e2) 1]
              [(< e1 e2) -1]
              [else (loop (cdr vs))]))))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 2))

  ;; ──────────────────────────────────────────────
  ;; Task 5: Implement mpoly-zero?
  ;; ──────────────────────────────────────────────
  ;; Check if multivariate polynomial is zero.

  (task
    (id "algebra/multivariate/mpoly-zero-impl")
    (module multivariate)
    (tier 0)
    (type implement)
    (description "Implement mpoly-zero?: check if a multivariate polynomial is the zero polynomial. A normalized mpoly stores its terms as a list of (coefficient . monomial) pairs. The zero polynomial is represented as a single term with zero coefficient and the constant monomial (empty list). So check: (1) there is exactly one term, (2) its monomial is the empty list (constant monomial), and (3) its coefficient equals the field zero under the field's equality test. Access the field with mpoly-field, the terms with mpoly-terms.")
    (context "Available: mpoly-field, mpoly-terms, field-equal-fn, field-zero, length, =, and, null?, caar, cdar, let. Three conditions combined with and.")
    (before "(define (mpoly-zero? p)
  ;; TODO: is this the zero polynomial?
  #f)")
    (test-file "lattice/algebra/test-multivariate-groebner.ss")
    (test-forms (";; Zero polynomial is zero
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [z (mpoly-zero F '(x y) ord)])
  (assert-true (mpoly-zero? z)))"
                 ";; Variable is not zero
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [p (mpoly-var F '(x y) ord 'x)])
  (assert-false (mpoly-zero? p)))"
                 ";; Constant 1 is not zero
(let* ([F Q-field]
       [ord (make-ordering 'lex '(x y))]
       [one (mpoly-one F '(x y) ord)])
  (assert-false (mpoly-zero? one)))"))
    (available-modules (prelude field sort))
    (hints ("Get field and terms: (let* ([F (mpoly-field p)] [terms (mpoly-terms p)]) ...)"
            "Get zero and equality: (let ([eq-fn (field-equal-fn F)] [zero (field-zero F)]) ...)"
            "Check: (and (= (length terms) 1) (null? (cdar terms)) (eq-fn (caar terms) zero))"
            "(caar terms) is the coefficient, (cdar terms) is the monomial"))
    (reference-solution "(define (mpoly-zero? p)
  (let* ([F (mpoly-field p)]
         [terms (mpoly-terms p)]
         [eq-fn (field-equal-fn F)]
         [zero (field-zero F)])
    (and (= (length terms) 1)
         (null? (cdar terms))
         (eq-fn (caar terms) zero))))")
    (alternatives ())
    (source-commit "a3abed54")
    (difficulty 1))

) ;; end tasks
