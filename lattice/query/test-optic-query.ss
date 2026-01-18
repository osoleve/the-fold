;;; lattice/query/test-optic-query.ss — Tests for Optic Query Language
;;;
;;; Comprehensive test suite for optic-based queries.

(load "core/testing/test-framework.ss")
(load "lattice/query/optic-query.ss")

;;; ============================================================
;;; Test Data Setup
;;; ============================================================

;; A simple "body" structure: ((name . "foo") (pos . (x . y)) (vel . (vx . vy)))
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

;; A "world" is a list of bodies wrapped in a structure
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

;; Traversal for each body in world
(define world-each-body
  (>>> world-bodies-lens traversal-each))

;; Test world
(define test-bodies
  (list (make-body "alpha" 0 5 1 2)
        (make-body "beta" 10 -3 -1 4)
        (make-body "gamma" 5 0 0 0)
        (make-body "delta" -5 10 3 -2)))

(define test-world (make-world test-bodies))

;;; ============================================================
;;; Part 1: Core Query Function Tests
;;; ============================================================

(test-group "Core oquery functions"

  (define-test "oquery returns all targets"
    (let ([bodies (oquery test-world world-each-body)])
      (assert-equal 4 (length bodies))))

  (define-test "oquery-where filters by predicate"
    (let ([positive-y (oquery-where test-world world-each-body
                        (lambda (b) (> (^. b body-pos-y) 0)))])
      ;; alpha (y=5), delta (y=10) have positive y
      (assert-equal 2 (length positive-y))))

  (define-test "oquery-select projects through function"
    (let ([names (oquery-select test-world world-each-body
                   (lambda (b) (^. b body-name-lens)))])
      (assert-equal '("alpha" "beta" "gamma" "delta") names)))

  (define-test "oquery-pipe filters then projects"
    (let ([names (oquery-pipe test-world world-each-body
                   (lambda (b) (> (^. b body-pos-y) 0))
                   (lambda (b) (^. b body-name-lens)))])
      ;; Should return names of bodies with positive y
      (assert-true (pair? (member "alpha" names)))
      (assert-true (pair? (member "delta" names)))
      (assert-equal 2 (length names))))

  (define-test "oquery-first returns first target"
    (let ([first (oquery-first test-world world-each-body)])
      (assert-true (just? first))
      (assert-equal "alpha" (^. (from-just first) body-name-lens))))

  (define-test "oquery-first-where finds first match"
    (let ([found (oquery-first-where test-world world-each-body
                   (lambda (b) (< (^. b body-pos-y) 0)))])
      ;; beta has y=-3
      (assert-true (just? found))
      (assert-equal "beta" (^. (from-just found) body-name-lens))))

  (define-test "oquery-first-where returns nothing when no match"
    (let ([found (oquery-first-where test-world world-each-body
                   (lambda (b) (> (^. b body-pos-x) 100)))])
      (assert-true (nothing? found)))))

;;; ============================================================
;;; Part 2: Optic Combinator Tests
;;; ============================================================

(test-group "Optic combinators"

  (define-test "optic-where creates filtered traversal"
    (let* ([fast-bodies (optic-where world-each-body
                          (lambda (b) (> (abs (^. b body-vel-x)) 0)))]
           [results (^.. test-world fast-bodies)])
      ;; alpha (vx=1), beta (vx=-1), delta (vx=3) have non-zero vx
      (assert-equal 3 (length results))))

  (define-test "optic-having filters by nested value"
    (let* ([upward (optic-having world-each-body body-vel-y
                     (lambda (vy) (> vy 0)))]
           [results (^.. test-world upward)])
      ;; alpha (vy=2), beta (vy=4) have positive vy
      (assert-equal 2 (length results))))

  (define-test "optic-select creates projected fold"
    (let* ([pos-fold (optic-select world-each-body
                       (lambda (b) (^. b body-pos-lens)))]
           [positions (^.. test-world pos-fold)])
      (assert-equal 4 (length positions))
      (assert-equal '(0 . 5) (car positions))))

  (define-test "optic-limit takes first n targets"
    (let* ([limited (optic-limit world-each-body 2)]
           [results (^.. test-world limited)])
      (assert-equal 2 (length results))))

  (define-test "optic-skip drops first n targets"
    (let* ([skipped (optic-skip world-each-body 2)]
           [results (^.. test-world skipped)])
      (assert-equal 2 (length results))
      ;; Should be gamma and delta
      (assert-equal "gamma" (^. (car results) body-name-lens))))

  ;; Boundary conditions
  (define-test "optic-limit with n > count returns all"
    (let* ([limited (optic-limit world-each-body 100)]
           [results (^.. test-world limited)])
      (assert-equal 4 (length results))))

  (define-test "optic-skip with n > count returns empty"
    (let* ([skipped (optic-skip world-each-body 100)]
           [results (^.. test-world skipped)])
      (assert-equal 0 (length results))))

  (define-test "optic-limit with n = 0 returns empty"
    (let* ([limited (optic-limit world-each-body 0)]
           [results (^.. test-world limited)])
      (assert-equal 0 (length results)))))

;;; ============================================================
;;; Part 3: Aggregation Tests
;;; ============================================================

(test-group "Aggregation functions"

  (define-test "oquery-count counts all targets"
    (assert-equal 4 (oquery-count test-world world-each-body)))

  (define-test "oquery-count-where counts matching targets"
    (let ([count (oquery-count-where test-world world-each-body
                   (lambda (b) (>= (^. b body-pos-x) 0)))])
      ;; alpha (0), beta (10), gamma (5) have x >= 0
      (assert-equal 3 count)))

  (define-test "oquery-sum sums numeric targets"
    (let* ([x-coords (optic-select world-each-body
                       (lambda (b) (^. b body-pos-x)))]
           [sum (oquery-sum test-world x-coords)])
      ;; 0 + 10 + 5 + (-5) = 10
      (assert-equal 10 sum)))

  (define-test "oquery-sum-by sums extracted values"
    (let ([sum (oquery-sum-by test-world world-each-body
                 (lambda (b) (^. b body-pos-y)))])
      ;; 5 + (-3) + 0 + 10 = 12
      (assert-equal 12 sum)))

  (define-test "oquery-any finds matching element"
    (assert-true (oquery-any test-world world-each-body
                   (lambda (b) (equal? "gamma" (^. b body-name-lens))))))

  (define-test "oquery-any returns false when none match"
    (assert-false (oquery-any test-world world-each-body
                    (lambda (b) (equal? "omega" (^. b body-name-lens))))))

  (define-test "oquery-all checks all elements"
    (assert-true (oquery-all test-world world-each-body
                   (lambda (b) (number? (^. b body-pos-x))))))

  (define-test "oquery-all returns false when one fails"
    (assert-false (oquery-all test-world world-each-body
                    (lambda (b) (> (^. b body-pos-y) 0)))))

  (define-test "oquery-min finds minimum"
    (let* ([y-coords (optic-select world-each-body
                       (lambda (b) (^. b body-pos-y)))]
           [min-y (oquery-min test-world y-coords)])
      (assert-true (just? min-y))
      (assert-equal -3 (from-just min-y))))

  (define-test "oquery-max finds maximum"
    (let* ([y-coords (optic-select world-each-body
                       (lambda (b) (^. b body-pos-y)))]
           [max-y (oquery-max test-world y-coords)])
      (assert-true (just? max-y))
      (assert-equal 10 (from-just max-y))))

  (define-test "oquery-min-by finds element with min value"
    (let ([lowest (oquery-min-by test-world world-each-body
                    (lambda (b) (^. b body-pos-y)))])
      (assert-true (just? lowest))
      ;; beta has lowest y (-3)
      (assert-equal "beta" (^. (from-just lowest) body-name-lens))))

  (define-test "oquery-max-by finds element with max value"
    (let ([highest (oquery-max-by test-world world-each-body
                     (lambda (b) (^. b body-pos-y)))])
      (assert-true (just? highest))
      ;; delta has highest y (10)
      (assert-equal "delta" (^. (from-just highest) body-name-lens))))

  ;; Edge cases for empty results
  (define-test "oquery-min returns nothing on empty"
    (let* ([empty-world (make-world '())]
           [result (oquery-min empty-world
                     (optic-select world-each-body
                       (lambda (b) (^. b body-pos-y))))])
      (assert-true (nothing? result))))

  (define-test "oquery-max returns nothing on empty"
    (let* ([empty-world (make-world '())]
           [result (oquery-max empty-world
                     (optic-select world-each-body
                       (lambda (b) (^. b body-pos-y))))])
      (assert-true (nothing? result))))

  (define-test "oquery-min-by returns nothing on empty"
    (let* ([empty-world (make-world '())]
           [result (oquery-min-by empty-world world-each-body
                     (lambda (b) (^. b body-pos-y)))])
      (assert-true (nothing? result))))

  (define-test "oquery-count on empty returns 0"
    (let ([empty-world (make-world '())])
      (assert-equal 0 (oquery-count empty-world world-each-body)))))

;;; ============================================================
;;; Part 4: Grouping and Partitioning Tests
;;; ============================================================

(test-group "Grouping and partitioning"

  (define-test "oquery-group-by groups by key"
    (let ([grouped (oquery-group-by test-world world-each-body
                     (lambda (b)
                       (if (> (^. b body-pos-y) 0) 'positive 'non-positive)))])
      ;; Should have two groups
      (assert-equal 2 (length grouped))
      (let ([pos-group (cdr (assq 'positive grouped))]
            [neg-group (cdr (assq 'non-positive grouped))])
        ;; alpha, delta are positive; beta, gamma are non-positive
        (assert-equal 2 (length pos-group))
        (assert-equal 2 (length neg-group)))))

  (define-test "oquery-partition splits by predicate"
    (let ([parts (oquery-partition test-world world-each-body
                   (lambda (b) (> (^. b body-pos-x) 0)))])
      (let ([yes (car parts)]
            [no (cdr parts)])
        ;; beta (10), gamma (5) have x > 0
        (assert-equal 2 (length yes))
        ;; alpha (0), delta (-5) have x <= 0
        (assert-equal 2 (length no))))))

;;; ============================================================
;;; Part 5: Join Tests
;;; ============================================================

(test-group "Join operations"

  (define-test "oquery-join cross-joins with predicate"
    ;; Join bodies with themselves where x coords sum to 10 and different names
    (let ([joined (oquery-join test-world world-each-body world-each-body
                    (lambda (a b)
                      (and (not (equal? (^. a body-name-lens)
                                        (^. b body-name-lens)))
                           (= (+ (^. a body-pos-x) (^. b body-pos-x)) 10))))])
      ;; alpha (0) + beta (10) = 10, beta (10) + alpha (0) = 10
      ;; gamma (5) + gamma (5) = 10 but same name so excluded
      (assert-equal 2 (length joined))))

  (define-test "oquery-zip zips two optic results"
    (let* ([names (optic-select world-each-body
                    (lambda (b) (^. b body-name-lens)))]
           [xs (optic-select world-each-body
                 (lambda (b) (^. b body-pos-x)))]
           [zipped (oquery-zip test-world names xs)])
      (assert-equal 4 (length zipped))
      (assert-equal '("alpha" . 0) (car zipped))))

  (define-test "oquery-union combines two optic results"
    (let* ([first-two (optic-limit world-each-body 2)]
           [last-two (optic-skip world-each-body 2)]
           [all (oquery-union test-world first-two last-two)])
      (assert-equal 4 (length all)))))

;;; ============================================================
;;; Part 6: Sorting Tests
;;; ============================================================

(test-group "Sorting operations"

  (define-test "oquery-sort-by sorts ascending"
    (let ([sorted (oquery-sort-by test-world world-each-body
                    (lambda (b) (^. b body-pos-y)))])
      ;; Order should be: beta (-3), gamma (0), alpha (5), delta (10)
      (assert-equal "beta" (^. (car sorted) body-name-lens))
      (assert-equal "delta" (^. (car (reverse sorted)) body-name-lens))))

  (define-test "oquery-sort-by-desc sorts descending"
    (let ([sorted (oquery-sort-by-desc test-world world-each-body
                    (lambda (b) (^. b body-pos-y)))])
      ;; Order should be: delta (10), alpha (5), gamma (0), beta (-3)
      (assert-equal "delta" (^. (car sorted) body-name-lens))
      (assert-equal "beta" (^. (car (reverse sorted)) body-name-lens)))))

;;; ============================================================
;;; Part 7: Query Builder Tests
;;; ============================================================

(test-group "Query builder DSL"

  (define-test "make-query creates query from optic"
    (let ([q (make-query world-each-body)])
      (assert-true (query-builder? q))))

  (define-test "q-where adds filter"
    (let* ([q (make-query world-each-body)]
           [q2 (q-where q (lambda (b) (> (^. b body-pos-y) 0)))])
      (assert-equal 1 (length (query-filters q2)))))

  (define-test "q-map adds transform"
    (let* ([q (make-query world-each-body)]
           [q2 (q-map q (lambda (b) (^. b body-name-lens)))])
      (assert-equal 1 (length (query-transforms q2)))))

  (define-test "q-run executes query"
    (let* ([q (make-query world-each-body)]
           [q2 (q-where q (lambda (b) (> (^. b body-pos-y) 0)))]
           [q3 (q-map q2 (lambda (b) (^. b body-name-lens)))]
           [results (q-run test-world q3)])
      ;; alpha and delta have positive y
      (assert-equal 2 (length results))
      (assert-true (pair? (member "alpha" results)))
      (assert-true (pair? (member "delta" results)))))

  (define-test "q-count counts results"
    (let* ([q (make-query world-each-body)]
           [q2 (q-where q (lambda (b) (>= (^. b body-pos-x) 0)))])
      (assert-equal 3 (q-count test-world q2))))

  (define-test "q-first gets first result"
    (let* ([q (make-query world-each-body)]
           [q2 (q-where q (lambda (b) (< (^. b body-pos-y) 0)))]
           [first (q-first test-world q2)])
      (assert-true (just? first))
      (assert-equal "beta" (^. (from-just first) body-name-lens)))))

;;; ============================================================
;;; Part 8: Predicate Helper Tests
;;; ============================================================

(test-group "Predicate helpers"

  (define-test "optic-eq? creates equality predicate"
    (let* ([pred (optic-eq? body-name-lens "gamma")]
           [results (filter pred test-bodies)])
      (assert-equal 1 (length results))))

  (define-test "optic-matches? creates matching predicate"
    (let* ([pred (optic-matches? body-pos-x (lambda (x) (> x 3)))]
           [results (filter pred test-bodies)])
      ;; beta (10), gamma (5) have x > 3
      (assert-equal 2 (length results))))

  (define-test "optic-exists? checks for target presence"
    ;; All bodies have name, so should all pass
    (let* ([pred (optic-exists? body-name-lens)]
           [results (filter pred test-bodies)])
      (assert-equal 4 (length results))))

  (define-test "optic-gt? creates greater-than predicate"
    (let* ([pred (optic-gt? body-pos-y 3)]
           [results (filter pred test-bodies)])
      ;; alpha (5), delta (10) have y > 3
      (assert-equal 2 (length results))))

  (define-test "optic-between? creates range predicate"
    (let* ([pred (optic-between? body-pos-y -1 6)]
           [results (filter pred test-bodies)])
      ;; gamma (0), alpha (5) are in range [-1, 6]
      (assert-equal 2 (length results)))))

;;; ============================================================
;;; Part 9: Complex Query Composition Tests
;;; ============================================================

(test-group "Complex compositions"

  (define-test "chain multiple filters"
    (let* ([q (make-query world-each-body)]
           [q2 (q-where q (lambda (b) (> (^. b body-pos-x) -10)))]
           [q3 (q-where q2 (lambda (b) (< (^. b body-pos-x) 8)))]
           [q4 (q-where q3 (lambda (b) (>= (^. b body-pos-y) 0)))]
           [results (q-run test-world q4)])
      ;; alpha (0,5), gamma (5,0), delta (-5,10) satisfy all conditions
      ;; beta (10,-3) fails: x=10 is not < 8
      (assert-equal 3 (length results))))

  (define-test "compose filtered optic with another optic"
    (let* ([moving (optic-where world-each-body
                     (lambda (b) (not (and (= 0 (^. b body-vel-x))
                                           (= 0 (^. b body-vel-y))))))]
           [velocities (>>> moving body-vel-lens)]
           [vels (^.. test-world velocities)])
      ;; alpha, beta, delta are moving (gamma has 0 velocity)
      (assert-equal 3 (length vels))))

  (define-test "project after complex filter"
    (let ([names (oquery-pipe test-world
                   (optic-having world-each-body body-vel-y
                     (lambda (vy) (> vy 0)))
                   (lambda (b) #t)
                   (lambda (b) (^. b body-name-lens)))])
      ;; alpha (vy=2), beta (vy=4) have positive vy
      (assert-equal 2 (length names))
      (assert-true (pair? (member "alpha" names)))
      (assert-true (pair? (member "beta" names))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
