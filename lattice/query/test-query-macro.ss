;;; lattice/query/test-query-macro.ss — Tests for Declarative Query DSL
;;;
;;; Tests the macro-based query language that compiles to optic-query primitives.

(load "core/testing/test-framework.ss")
(load "lattice/query/query-macro.ss")

;;; ============================================================
;;; Test Data Setup
;;; ============================================================

;; Reuse the body/world structure from test-optic-query.ss
(define (make-body name x y vx vy)
  `((name . ,name)
    (pos . (,x . ,y))
    (vel . (,vx . ,vy))))

;; Lenses for body access
(define body-name-lens
  (make-lens
   (lambda (b) (cdr (assq 'name b)))
   (lambda (n b)
     (map (lambda (pair)
            (if (eq? (car pair) 'name)
                (cons 'name n)
                pair))
          b))))

(define body-pos-lens
  (make-lens
   (lambda (b) (cdr (assq 'pos b)))
   (lambda (p b)
     (map (lambda (pair)
            (if (eq? (car pair) 'pos)
                (cons 'pos p)
                pair))
          b))))

(define body-vel-lens
  (make-lens
   (lambda (b) (cdr (assq 'vel b)))
   (lambda (v b)
     (map (lambda (pair)
            (if (eq? (car pair) 'vel)
                (cons 'vel v)
                pair))
          b))))

(define pair-x-lens
  (make-lens car (lambda (x p) (cons x (cdr p)))))

(define pair-y-lens
  (make-lens cdr (lambda (y p) (cons (car p) y))))

;; Composed lenses
(define body-pos-x (>>> body-pos-lens pair-x-lens))
(define body-pos-y (>>> body-pos-lens pair-y-lens))
(define body-vel-x (>>> body-vel-lens pair-x-lens))
(define body-vel-y (>>> body-vel-lens pair-y-lens))

;; World structure
(define (make-world bodies)
  `((bodies . ,bodies)
    (time . 0)))

(define world-bodies-lens
  (make-lens
   (lambda (w) (cdr (assq 'bodies w)))
   (lambda (bs w)
     (map (lambda (pair)
            (if (eq? (car pair) 'bodies)
                (cons 'bodies bs)
                pair))
          w))))

(define world-each-body
  (>>> world-bodies-lens traversal-each))

;; Test data
(define test-bodies
  (list (make-body "alpha" 0 5 1 2)
        (make-body "beta" 10 -3 -1 4)
        (make-body "gamma" 5 0 0 0)
        (make-body "delta" -5 10 3 -2)))

(define test-world (make-world test-bodies))

;;; ============================================================
;;; Part 1: Predicate Combinator Tests
;;; ============================================================

(test-group "Predicate combinators"

  (define-test "=? creates equality predicate"
    (let ([pred (=? body-name-lens "alpha")])
      (assert-true (pred (car test-bodies)))
      (assert-false (pred (cadr test-bodies)))))

  (define-test "/=? creates inequality predicate"
    (let ([pred (/=? body-name-lens "alpha")])
      (assert-false (pred (car test-bodies)))
      (assert-true (pred (cadr test-bodies)))))

  (define-test ">? creates greater-than predicate"
    (let ([pred (>? body-pos-y 3)])
      ;; alpha (y=5), delta (y=10) have y > 3
      (assert-true (pred (car test-bodies)))    ; alpha
      (assert-false (pred (cadr test-bodies)))  ; beta (y=-3)
      (assert-true (pred (cadddr test-bodies))) ; delta
      ))

  (define-test "<? creates less-than predicate"
    (let ([pred (<? body-pos-y 3)])
      ;; beta (y=-3), gamma (y=0) have y < 3
      (assert-false (pred (car test-bodies)))   ; alpha (y=5)
      (assert-true (pred (cadr test-bodies)))   ; beta (y=-3)
      (assert-true (pred (caddr test-bodies)))  ; gamma (y=0)
      ))

  (define-test ">=? creates greater-or-equal predicate"
    (let ([pred (>=? body-pos-y 0)])
      ;; alpha (5), gamma (0), delta (10) have y >= 0
      (assert-true (pred (car test-bodies)))
      (assert-false (pred (cadr test-bodies)))  ; beta (-3)
      (assert-true (pred (caddr test-bodies)))  ; gamma (0)
      ))

  (define-test "<=? creates less-or-equal predicate"
    (let ([pred (<=? body-pos-y 0)])
      ;; beta (-3), gamma (0) have y <= 0
      (assert-false (pred (car test-bodies)))   ; alpha (5)
      (assert-true (pred (cadr test-bodies)))   ; beta (-3)
      (assert-true (pred (caddr test-bodies)))  ; gamma (0)
      ))

  (define-test "between? creates range predicate"
    (let ([pred (between? body-pos-y -1 6)])
      ;; gamma (0), alpha (5) are in [-1, 6]
      (assert-true (pred (car test-bodies)))    ; alpha (5)
      (assert-false (pred (cadr test-bodies)))  ; beta (-3)
      (assert-true (pred (caddr test-bodies)))  ; gamma (0)
      (assert-false (pred (cadddr test-bodies))) ; delta (10)
      ))

  (define-test "in? creates set membership predicate"
    (let ([pred (in? body-name-lens '("alpha" "gamma"))])
      (assert-true (pred (car test-bodies)))    ; alpha
      (assert-false (pred (cadr test-bodies)))  ; beta
      (assert-true (pred (caddr test-bodies)))  ; gamma
      ))

  (define-test "and? combines predicates with AND"
    (let ([pred (and? (>? body-pos-x 0) (>? body-pos-y 0))])
      ;; Only delta has both x>0 and y>0? No wait:
      ;; alpha: x=0, y=5 -> x not > 0
      ;; beta: x=10, y=-3 -> y not > 0
      ;; gamma: x=5, y=0 -> y not > 0
      ;; delta: x=-5, y=10 -> x not > 0
      ;; Actually none satisfy both! Let me check differently.
      ;; Let's use >= for x
      (assert-false (pred (car test-bodies)))))  ; alpha x=0, not > 0

  (define-test "and? with different predicates"
    (let ([pred (and? (>=? body-pos-x 0) (>=? body-pos-y 0))])
      ;; alpha: x=0, y=5 -> both >= 0, TRUE
      ;; beta: x=10, y=-3 -> y not >= 0, FALSE
      ;; gamma: x=5, y=0 -> both >= 0, TRUE
      ;; delta: x=-5, y=10 -> x not >= 0, FALSE
      (assert-true (pred (car test-bodies)))    ; alpha
      (assert-false (pred (cadr test-bodies)))  ; beta
      (assert-true (pred (caddr test-bodies)))  ; gamma
      (assert-false (pred (cadddr test-bodies)))))

  (define-test "or? combines predicates with OR"
    (let ([pred (or? (=? body-name-lens "alpha")
                     (=? body-name-lens "delta"))])
      (assert-true (pred (car test-bodies)))    ; alpha
      (assert-false (pred (cadr test-bodies)))  ; beta
      (assert-true (pred (cadddr test-bodies))))) ; delta

  (define-test "not? negates predicate"
    (let ([pred (not? (=? body-name-lens "alpha"))])
      (assert-false (pred (car test-bodies)))
      (assert-true (pred (cadr test-bodies)))))

  (define-test "null-at? checks for missing target"
    ;; All bodies have names, so null-at? should return false
    (let ([pred (null-at? body-name-lens)])
      (assert-false (pred (car test-bodies)))))

  (define-test "exists-at? checks for present target"
    ;; All bodies have names, so exists-at? should return true
    (let ([pred (exists-at? body-name-lens)])
      (assert-true (pred (car test-bodies)))))

  (define-test "like? with needle longer than haystack returns false"
    (let ([pred (like? body-name-lens "verylongnamethatdoesnotexist")])
      (assert-false (pred (car test-bodies)))))

  (define-test "like? is case-insensitive"
    (let ([pred (like? body-name-lens "ALPHA")])
      (assert-true (pred (car test-bodies))))))

;;; ============================================================
;;; Part 2: @ Operator Tests
;;; ============================================================

(test-group "@ operator"

  (define-test "@ views through single optic"
    (let ([body (car test-bodies)])
      (assert-equal "alpha" (@ body body-name-lens))))

  (define-test "@ views through composed optics"
    (let ([body (car test-bodies)])
      (assert-equal 5 (@ body body-pos-lens pair-y-lens))))

  (define-test "@? previews through single optic"
    (let ([body (car test-bodies)])
      (assert-true (just? (@? body body-name-lens)))
      (assert-equal "alpha" (from-just (@? body body-name-lens))))))

;;; ============================================================
;;; Part 3: Query Builder Tests
;;; ============================================================

(test-group "Query builder"

  (define-test "from creates query record"
    (let ([q (from test-world world-each-body)])
      (assert-true (query-record? q))
      (assert-equal test-world (qr-source q))
      (assert-equal world-each-body (qr-optic q))))

  (define-test "where-clause adds predicate"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 0))])
      (assert-true (procedure? (qr-where q2)))))

  (define-test "select-clause adds projection"
    (let* ([q (from test-world world-each-body)]
           [q2 (select-clause q (pluck body-name-lens))])
      (assert-true (procedure? (qr-select q2)))))

  (define-test "limit-clause sets limit"
    (let* ([q (from test-world world-each-body)]
           [q2 (limit-clause q 2)])
      (assert-equal 2 (qr-limit q2))))

  (define-test "offset-clause sets offset"
    (let* ([q (from test-world world-each-body)]
           [q2 (offset-clause q 1)])
      (assert-equal 1 (qr-offset q2))))

  (define-test "order-by-clause sets ordering"
    (let* ([q (from test-world world-each-body)]
           [q2 (order-by-clause q (pluck body-pos-y))])
      (assert-true (procedure? (qr-order-key q2)))
      (assert-equal 'asc (qr-order-dir q2))))

  (define-test "order-by-clause with desc direction"
    (let* ([q (from test-world world-each-body)]
           [q2 (order-by-clause q (pluck body-pos-y) 'desc)])
      (assert-equal 'desc (qr-order-dir q2)))))

;;; ============================================================
;;; Part 4: Query Execution Tests
;;; ============================================================

(test-group "Query execution"

  (define-test "run-query returns all targets without clauses"
    (let* ([q (from test-world world-each-body)]
           [results (run-query q)])
      (assert-equal 4 (length results))))

  (define-test "run-query filters with where clause"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 0))]
           [results (run-query q2)])
      ;; alpha (y=5), delta (y=10) have positive y
      (assert-equal 2 (length results))))

  (define-test "run-query projects with select clause"
    (let* ([q (from test-world world-each-body)]
           [q2 (select-clause q (pluck body-name-lens))]
           [results (run-query q2)])
      (assert-equal '("alpha" "beta" "gamma" "delta") results)))

  (define-test "run-query filters then projects"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 0))]
           [q3 (select-clause q2 (pluck body-name-lens))]
           [results (run-query q3)])
      (assert-equal 2 (length results))
      (assert-true (pair? (member "alpha" results)))
      (assert-true (pair? (member "delta" results)))))

  (define-test "run-query applies limit"
    (let* ([q (from test-world world-each-body)]
           [q2 (limit-clause q 2)]
           [results (run-query q2)])
      (assert-equal 2 (length results))))

  (define-test "run-query applies offset"
    (let* ([q (from test-world world-each-body)]
           [q2 (offset-clause q 2)]
           [results (run-query q2)])
      (assert-equal 2 (length results))
      ;; Should be gamma and delta
      (assert-equal "gamma" (@ (car results) body-name-lens))))

  (define-test "run-query applies limit and offset together"
    (let* ([q (from test-world world-each-body)]
           [q2 (offset-clause q 1)]
           [q3 (limit-clause q2 2)]
           [results (run-query q3)])
      (assert-equal 2 (length results))
      ;; Should be beta and gamma (skip alpha, take 2)
      (assert-equal "beta" (@ (car results) body-name-lens))
      (assert-equal "gamma" (@ (cadr results) body-name-lens))))

  (define-test "run-query sorts ascending"
    (let* ([q (from test-world world-each-body)]
           [q2 (order-by-clause q (pluck body-pos-y))]
           [results (run-query q2)])
      ;; Order: beta (-3), gamma (0), alpha (5), delta (10)
      (assert-equal "beta" (@ (car results) body-name-lens))
      (assert-equal "delta" (@ (car (reverse results)) body-name-lens))))

  (define-test "run-query sorts descending"
    (let* ([q (from test-world world-each-body)]
           [q2 (order-by-clause q (pluck body-pos-y) 'desc)]
           [results (run-query q2)])
      ;; Order: delta (10), alpha (5), gamma (0), beta (-3)
      (assert-equal "delta" (@ (car results) body-name-lens))
      (assert-equal "beta" (@ (car (reverse results)) body-name-lens)))))

;;; ============================================================
;;; Part 5: q> Pipeline Tests
;;; ============================================================

(test-group "q> pipeline operator"

  (define-test "q> executes simple query"
    (let ([results (q> (from test-world world-each-body))])
      (assert-equal 4 (length results))))

  (define-test "q> chains where clause"
    (let ([results (q> (from test-world world-each-body)
                       (lambda (q) (where-clause q (>? body-pos-y 0))))])
      (assert-equal 2 (length results))))

  (define-test "q> chains multiple clauses"
    (let ([results (q> (from test-world world-each-body)
                       (lambda (q) (where-clause q (>? body-pos-y 0)))
                       (lambda (q) (select-clause q (pluck body-name-lens))))])
      (assert-equal 2 (length results))
      (assert-true (pair? (member "alpha" results)))
      (assert-true (pair? (member "delta" results))))))

;;; ============================================================
;;; Part 6: Aggregation Tests
;;; ============================================================

(test-group "Query aggregations"

  (define-test "count-query counts results"
    (let ([q (from test-world world-each-body)])
      (assert-equal 4 (count-query q))))

  (define-test "count-query with filter"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 0))])
      (assert-equal 2 (count-query q2))))

  (define-test "sum-query sums values"
    (let ([q (from test-world world-each-body)])
      ;; Sum of y coords: 5 + (-3) + 0 + 10 = 12
      (assert-equal 12 (sum-query q (pluck body-pos-y)))))

  (define-test "avg-query averages values"
    (let ([q (from test-world world-each-body)])
      ;; Avg of y coords: (5 + (-3) + 0 + 10) / 4 = 3
      (assert-equal 3 (avg-query q (pluck body-pos-y)))))

  (define-test "min-query finds minimum"
    (let ([q (from test-world world-each-body)])
      (let ([result (min-query q (pluck body-pos-y))])
        (assert-true (just? result))
        (assert-equal -3 (from-just result)))))

  (define-test "max-query finds maximum"
    (let ([q (from test-world world-each-body)])
      (let ([result (max-query q (pluck body-pos-y))])
        (assert-true (just? result))
        (assert-equal 10 (from-just result)))))

  (define-test "group-query groups by key"
    (let* ([q (from test-world world-each-body)]
           [groups (group-query q (lambda (b)
                                    (if (> (@ b body-pos-y) 0)
                                        'positive
                                        'non-positive)))])
      (assert-equal 2 (length groups)))))

;;; ============================================================
;;; Part 7: First/Any/All Tests
;;; ============================================================

(test-group "Result checking"

  (define-test "first-result returns first"
    (let* ([q (from test-world world-each-body)]
           [result (first-result q)])
      (assert-true (just? result))
      (assert-equal "alpha" (@ (from-just result) body-name-lens))))

  (define-test "first-result returns nothing on empty"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 100))]
           [result (first-result q2)])
      (assert-true (nothing? result))))

  (define-test "any-result? returns true when results exist"
    (let ([q (from test-world world-each-body)])
      (assert-true (any-result? q))))

  (define-test "any-result? returns false when empty"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-y 100))])
      (assert-false (any-result? q2))))

  (define-test "all-match? checks all results"
    (let ([q (from test-world world-each-body)])
      ;; All bodies have numeric x coords
      (assert-true (all-match? q (lambda (b) (number? (@ b body-pos-x)))))
      ;; Not all have positive y
      (assert-false (all-match? q (lambda (b) (> (@ b body-pos-y) 0)))))))

;;; ============================================================
;;; Part 8: Complex Query Scenarios
;;; ============================================================

(test-group "Complex query scenarios"

  (define-test "top 2 bodies by y position descending"
    (let* ([q (from test-world world-each-body)]
           [q2 (order-by-clause q (pluck body-pos-y) 'desc)]
           [q3 (limit-clause q2 2)]
           [results (run-query q3)])
      (assert-equal 2 (length results))
      ;; delta (10) and alpha (5)
      (assert-equal "delta" (@ (car results) body-name-lens))
      (assert-equal "alpha" (@ (cadr results) body-name-lens))))

  (define-test "filter, sort, limit, project"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>=? body-pos-y 0))]
           [q3 (order-by-clause q2 (pluck body-pos-y))]
           [q4 (limit-clause q3 2)]
           [q5 (select-clause q4 (pluck body-name-lens))]
           [results (run-query q5)])
      ;; Filter to y >= 0: gamma (0), alpha (5), delta (10)
      ;; Sort by y: gamma (0), alpha (5), delta (10)
      ;; Limit to 2: gamma, alpha
      ;; Project names
      (assert-equal '("gamma" "alpha") results)))

  (define-test "multiple where conditions (composed)"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (>? body-pos-x 0))]
           [q3 (where-clause q2 (>? body-pos-y 0))]
           ;; x > 0: beta (10), gamma (5)
           ;; then y > 0: none of those! beta has y=-3, gamma has y=0
           [results (run-query q3)])
      (assert-equal 0 (length results))))

  (define-test "combining predicates with and?"
    (let* ([q (from test-world world-each-body)]
           [q2 (where-clause q (and? (>=? body-pos-x -5)
                                     (<? body-pos-x 8)
                                     (>? body-pos-y 0)))]
           [results (run-query q2)])
      ;; x in [-5, 8) and y > 0:
      ;; alpha: x=0, y=5 -> YES
      ;; beta: x=10, y=-3 -> NO (x too big)
      ;; gamma: x=5, y=0 -> NO (y not > 0)
      ;; delta: x=-5, y=10 -> YES
      (assert-equal 2 (length results)))))

;;; ============================================================
;;; Part 9: Edge Cases
;;; ============================================================

(test-group "Edge cases"

  (define-test "empty source returns empty"
    (let* ([empty-world (make-world '())]
           [q (from empty-world world-each-body)]
           [results (run-query q)])
      (assert-equal '() results)))

  (define-test "limit 0 returns empty"
    (let* ([q (from test-world world-each-body)]
           [q2 (limit-clause q 0)]
           [results (run-query q2)])
      (assert-equal '() results)))

  (define-test "offset beyond count returns empty"
    (let* ([q (from test-world world-each-body)]
           [q2 (offset-clause q 100)]
           [results (run-query q2)])
      (assert-equal '() results)))

  (define-test "like? with empty pattern matches all"
    (let ([pred (like? body-name-lens "")])
      (assert-true (pred (car test-bodies)))))

  (define-test "select-fields extracts multiple fields"
    (let* ([proj (select-fields (cons 'name body-name-lens)
                                (cons 'x body-pos-x))]
           [body (car test-bodies)]
           [result (proj body)])
      ;; alpha has name="alpha", x=0
      (assert-equal 2 (length result))
      (assert-equal "alpha" (cdr (assq 'name result)))
      (assert-equal 0 (cdr (assq 'x result))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
