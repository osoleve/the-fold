;;; lattice/egraph/test-match.ss — Tests for E-Graph Pattern Matching
;;;
;;; Run with: scheme --script lattice/egraph/test-match.ss

(load "core/testing/test-framework.ss")
(load "lattice/egraph/match.ss")

(display "\n====\n")
(display "E-Graph Pattern Matching Tests\n")
(display "====\n\n")

(test-group "pattern-variables"

  (define-test "pattern-var? recognizes ?x"
    (assert-true (pattern-var? '?x)))

  (define-test "pattern-var? recognizes ?foo"
    (assert-true (pattern-var? '?foo)))

  (define-test "pattern-var? rejects plain symbols"
    (assert-false (pattern-var? 'x)))

  (define-test "pattern-var? rejects numbers"
    (assert-false (pattern-var? 42)))

  (define-test "pattern-var? rejects single ?"
    (assert-false (pattern-var? '?))))

(test-group "substitutions"

  (define-test "empty-subst is empty"
    (assert-equal '() (empty-subst)))

  (define-test "subst-lookup finds binding"
    (let ([s (subst-extend (empty-subst) '?x 5)])
      (assert-equal 5 (subst-lookup s '?x))))

  (define-test "subst-lookup returns #f for missing"
    (assert-false (subst-lookup (empty-subst) '?x)))

  (define-test "subst-try-extend succeeds for new var"
    (let ([s (subst-try-extend (empty-subst) '?x 5)])
      (assert-true (if s #t #f))
      (assert-equal 5 (subst-lookup s '?x))))

  (define-test "subst-try-extend succeeds for same value"
    (let* ([s1 (subst-extend (empty-subst) '?x 5)]
           [s2 (subst-try-extend s1 '?x 5)])
      (assert-true (if s2 #t #f))))

  (define-test "subst-try-extend fails for conflict"
    (let* ([s1 (subst-extend (empty-subst) '?x 5)]
           [s2 (subst-try-extend s1 '?x 6)])
      (assert-false s2)))

  (define-test "subst-merge combines compatible"
    (let* ([s1 (subst-extend (empty-subst) '?x 1)]
           [s2 (subst-extend (empty-subst) '?y 2)]
           [merged (subst-merge s1 s2)])
      (assert-true (if merged #t #f))
      (assert-equal 1 (subst-lookup merged '?x))
      (assert-equal 2 (subst-lookup merged '?y))))

  (define-test "subst-merge fails on conflict"
    (let* ([s1 (subst-extend (empty-subst) '?x 1)]
           [s2 (subst-extend (empty-subst) '?x 2)])
      (assert-false (subst-merge s1 s2)))))

(test-group "ematch-basic"

  (define-test "match variable against leaf"
    (let ([eg (make-egraph)])
      (let ([x (egraph-add-term! eg 'x)])
        (let ([matches (ematch eg '?a x)])
          (assert-equal 1 (length matches))
          (assert-equal x (subst-lookup (car matches) '?a))))))

  (define-test "match leaf pattern against leaf"
    (let ([eg (make-egraph)])
      (let ([x (egraph-add-term! eg 'x)])
        (let ([matches (ematch eg 'x x)])
          (assert-equal 1 (length matches))))))

  (define-test "match leaf pattern fails on mismatch"
    (let ([eg (make-egraph)])
      (let ([x (egraph-add-term! eg 'x)])
        (let ([matches (ematch eg 'y x)])
          (assert-equal 0 (length matches))))))

  (define-test "match number pattern"
    (let ([eg (make-egraph)])
      (let ([n (egraph-add-term! eg 42)])
        (let ([matches (ematch eg 42 n)])
          (assert-equal 1 (length matches))))))

  (define-test "match number pattern fails on mismatch"
    (let ([eg (make-egraph)])
      (let ([n (egraph-add-term! eg 42)])
        (let ([matches (ematch eg 99 n)])
          (assert-equal 0 (length matches)))))))

(test-group "ematch-compound"

  (define-test "match (+ ?x ?y) against (+ a b)"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(+ a b))])
        (let ([matches (ematch eg '(+ ?x ?y) term)])
          (assert-equal 1 (length matches))
          (let* ([s (car matches)]
                 [x-id (subst-lookup s '?x)]
                 [y-id (subst-lookup s '?y)])
            ;; ?x should be bound to class of 'a'
            ;; ?y should be bound to class of 'b'
            (assert-true (integer? x-id))
            (assert-true (integer? y-id))
            (assert-true (not (= x-id y-id))))))))

  (define-test "match fails on wrong operator"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(+ a b))])
        (let ([matches (ematch eg '(* ?x ?y) term)])
          (assert-equal 0 (length matches))))))

  (define-test "match fails on wrong arity"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(+ a b))])
        (let ([matches (ematch eg '(+ ?x) term)])
          (assert-equal 0 (length matches))))))

  (define-test "match nested pattern"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(f (g x)))])
        (let ([matches (ematch eg '(f ?inner) term)])
          (assert-equal 1 (length matches))
          ;; ?inner should match the (g x) class
          (let ([inner-id (subst-lookup (car matches) '?inner)])
            (assert-true (integer? inner-id)))))))

  (define-test "match deeply nested"
    (let ([eg (make-egraph)])
      (let ([term (egraph-add-term! eg '(f (g (h x))))])
        (let ([matches (ematch eg '(f (g ?x)) term)])
          (assert-equal 1 (length matches)))))))

(test-group "ematch-multiple"

  (define-test "repeated variable must match same class"
    (let ([eg (make-egraph)])
      ;; (+ x x) - both children are same class
      (let ([term (egraph-add-term! eg '(+ x x))])
        (let ([matches (ematch eg '(+ ?a ?a) term)])
          ;; Should succeed - both ?a bind to same class
          (assert-equal 1 (length matches))))))

  (define-test "repeated variable fails on different classes"
    (let ([eg (make-egraph)])
      ;; (+ x y) - children are different classes
      (let ([term (egraph-add-term! eg '(+ x y))])
        (let ([matches (ematch eg '(+ ?a ?a) term)])
          ;; Should fail - can't bind ?a to two different classes
          (assert-equal 0 (length matches))))))

  (define-test "multiple matches in merged class"
    (let ([eg (make-egraph)])
      ;; Add two nodes to same class via merge
      (let ([fx (egraph-add-term! eg '(f x))]
            [fy (egraph-add-term! eg '(f y))])
        (egraph-merge! eg fx fy)
        (egraph-saturate-rebuild! eg)
        ;; Now class contains both (f x) and (f y)
        ;; Pattern (f ?a) should match both ways
        (let ([matches (ematch eg '(f ?a) fx)])
          (assert-equal 2 (length matches)))))))

(test-group "ematch-all"

  (define-test "ematch-all finds all matching classes"
    (let ([eg (make-egraph)])
      (let ([fx (egraph-add-term! eg '(f x))]
            [fy (egraph-add-term! eg '(f y))]
            [gx (egraph-add-term! eg '(g x))])
        ;; Pattern (f ?a) should match fx and fy but not gx
        (let ([matches (ematch-all eg '(f ?a))])
          (assert-equal 2 (length matches)))))))

(test-group "pattern-apply"

  (define-test "apply to variable"
    (let ([s (subst-extend (empty-subst) '?x 5)])
      (assert-equal 5 (pattern-apply s '?x))))

  (define-test "apply to compound"
    (let ([s (subst-extend (subst-extend (empty-subst) '?x 1) '?y 2)])
      (let ([result (pattern-apply s '(+ ?x ?y))])
        (assert-equal '(+ 1 2) result))))

  (define-test "apply preserves literals"
    (let ([s (subst-extend (empty-subst) '?x 1)])
      (let ([result (pattern-apply s '(f ?x 42))])
        (assert-equal '(f 1 42) result)))))

(test-group "rules"

  (define-test "make-rule creates rule"
    (let ([r (make-rule '(+ ?x 0) '?x)])
      (assert-true (rule? r))
      (assert-equal '(+ ?x 0) (rule-lhs r))
      (assert-equal '?x (rule-rhs r))))

  (define-test "apply-rule adds rewrite"
    (let ([eg (make-egraph)]
          ;; Rule: (+ ?x 0) => ?x
          [r (make-rule '(+ ?x 0) '?x)])
      ;; Add (+ a 0)
      (let ([term (egraph-add-term! eg '(+ a 0))]
            [a-id (egraph-add-term! eg 'a)]
            [zero (egraph-add-term! eg 0)])
        ;; Apply rule
        (let ([matched (apply-rule eg r term)])
          (assert-true matched)
          ;; After rule application, (+ a 0) should be equivalent to a
          (egraph-saturate-rebuild! eg)
          (assert-equal (egraph-find eg term) (egraph-find eg a-id))))))

  (define-test "apply-rules counts matches"
    (let ([eg (make-egraph)]
          [rules (list (make-rule '(+ ?x 0) '?x))])
      ;; Add two terms that match the rule
      (egraph-add-term! eg '(+ a 0))
      (egraph-add-term! eg '(+ b 0))
      (let ([count (apply-rules eg rules)])
        (assert-equal 2 count)))))

(test-group "rules-algebraic"

  (define-test "commutativity rule"
    (let ([eg (make-egraph)]
          ;; Rule: (+ ?x ?y) => (+ ?y ?x)
          [comm (make-rule '(+ ?x ?y) '(+ ?y ?x))])
      (let ([term (egraph-add-term! eg '(+ a b))])
        (apply-rule eg comm term)
        (egraph-saturate-rebuild! eg)
        ;; Class should now contain both (+ a b) and (+ b a)
        (let ([nodes (egraph-class-nodes eg term)])
          (assert-equal 2 (length nodes))))))

  (define-test "identity rule"
    (let ([eg (make-egraph)]
          ;; Rule: (* ?x 1) => ?x
          [ident (make-rule '(* ?x 1) '?x)])
      (let ([term (egraph-add-term! eg '(* a 1))]
            [a (egraph-add-term! eg 'a)])
        (apply-rule eg ident term)
        (egraph-saturate-rebuild! eg)
        ;; (* a 1) should be equivalent to a
        (assert-equal (egraph-find eg term) (egraph-find eg a))))))

(run-all-tests)

