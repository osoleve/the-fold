;;; lattice/fp/rewrite/test-sexp-zipper.ss — Tests for S-expression Zipper
;;;
;;; Tests the zipper-based navigation for S-expressions.

(load "lattice/fp/rewrite/sexp-zipper.ss")
(load "core/testing/test-framework.ss")

;;; ====
;;; Test Fixtures
;;; ====

;;; Simple expression: (+ 1 2)
(define simple-expr '(+ 1 2))

;;; Nested expression: (+ (* 2 3) 4)
(define nested-expr '(+ (* 2 3) 4))

;;; Deeply nested: (+ (* (- 5 1) 3) (/ 8 2))
(define deep-expr '(+ (* (- 5 1) 3) (/ 8 2)))

;;; Atom
(define atom-expr 42)

;;; ====
;;; Basic Navigation Tests
;;; ====

(test-group "sexp-zipper-navigation"

  (define-test "sexp->zipper creates zipper at root"
    (let ([sz (sexp->zipper simple-expr)])
      (assert-true (sexp-zipper? sz))
      (assert-equal '(+ 1 2) (sexp-zipper-get sz))
      (assert-true (sexp-zipper-at-root? sz))))

  (define-test "sexp-zipper-down moves to first element"
    (let* ([sz (sexp->zipper simple-expr)]
           [down (sexp-zipper-down sz)])
      (assert-true (just? down))
      (assert-equal '+ (sexp-zipper-get (from-just down)))))

  (define-test "sexp-zipper-right moves to sibling"
    (let* ([sz (sexp->zipper simple-expr)]
           [plus-z (from-just (sexp-zipper-down sz))]
           [one-z (from-just (sexp-zipper-right plus-z))])
      (assert-equal 1 (sexp-zipper-get one-z))))

  (define-test "sexp-zipper-up moves to parent"
    (let* ([sz (sexp->zipper simple-expr)]
           [plus-z (from-just (sexp-zipper-down sz))]
           [back-up (sexp-zipper-up plus-z)])
      (assert-true (just? back-up))
      (assert-equal '(+ 1 2) (sexp-zipper-get (from-just back-up)))))

  (define-test "atom has no children"
    (let ([sz (sexp->zipper atom-expr)])
      (assert-true (sexp-zipper-at-leaf? sz))
      (assert-true (nothing? (sexp-zipper-down sz)))))

  (define-test "sexp-zipper-nth navigates to nth element"
    (let* ([sz (sexp->zipper simple-expr)]
           [third (sexp-zipper-nth sz 2)])
      (assert-true (just? third))
      (assert-equal 2 (sexp-zipper-get (from-just third))))))

;;; ====
;;; Position-Based Navigation Tests
;;; ====

(test-group "sexp-zipper-positions"

  (define-test "root position is empty"
    (let ([sz (sexp->zipper simple-expr)])
      (assert-equal '() (sexp-zipper-position sz))))

  (define-test "first element position is (0)"
    (let* ([sz (sexp->zipper simple-expr)]
           [first (from-just (sexp-zipper-nth sz 0))])
      (assert-equal '(0) (sexp-zipper-position first))))

  (define-test "nested position is correct"
    (let* ([sz (sexp->zipper nested-expr)]
           ;; Navigate to (* 2 3) at position (1)
           [mul-z (from-just (sexp-zipper-nth sz 1))]
           ;; Navigate to 3 at position (1 2)
           [three-z (from-just (sexp-zipper-nth mul-z 2))])
      (assert-equal '(1 2) (sexp-zipper-position three-z))))

  (define-test "sexp-zipper-goto navigates to position"
    (let* ([sz (sexp->zipper nested-expr)]
           [at-pos (sexp-zipper-goto sz '(1 2))])
      (assert-true (just? at-pos))
      (assert-equal 3 (sexp-zipper-get (from-just at-pos)))))

  (define-test "sexp-zipper-goto returns nothing for invalid position"
    (let* ([sz (sexp->zipper simple-expr)]
           [at-pos (sexp-zipper-goto sz '(10))])
      (assert-true (nothing? at-pos)))))

;;; ====
;;; Modification Tests
;;; ====

(test-group "sexp-zipper-modification"

  (define-test "sexp-zipper-set replaces atom"
    (let* ([sz (sexp->zipper simple-expr)]
           [one-z (from-just (sexp-zipper-nth sz 1))]
           [modified (sexp-zipper-set one-z 100)]
           [result (zipper->sexp modified)])
      (assert-equal '(+ 100 2) result)))

  (define-test "sexp-zipper-set replaces subtree"
    (let* ([sz (sexp->zipper nested-expr)]
           [mul-z (from-just (sexp-zipper-nth sz 1))]
           [modified (sexp-zipper-set mul-z '(+ 5 5))]
           [result (zipper->sexp modified)])
      (assert-equal '(+ (+ 5 5) 4) result)))

  (define-test "sexp-zipper-modify transforms value"
    (let* ([sz (sexp->zipper simple-expr)]
           [two-z (from-just (sexp-zipper-nth sz 2))]
           [modified (sexp-zipper-modify two-z (lambda (x) (* x 10)))]
           [result (zipper->sexp modified)])
      (assert-equal '(+ 1 20) result))))

;;; ====
;;; Engine Compatibility Tests
;;; ====

(test-group "sexp-zipper-engine-compat"

  (define-test "sexp-at gets root"
    (assert-equal '(+ 1 2) (sexp-at simple-expr '())))

  (define-test "sexp-at gets element"
    (assert-equal 1 (sexp-at simple-expr '(1))))

  (define-test "sexp-at gets nested element"
    (assert-equal 3 (sexp-at nested-expr '(1 2))))

  (define-test "sexp-set-at replaces element"
    (assert-equal '(+ 1 99) (sexp-set-at simple-expr '(2) 99)))

  (define-test "sexp-set-at replaces nested element"
    (assert-equal '(+ (* 2 100) 4) (sexp-set-at nested-expr '(1 2) 100)))

  (define-test "sexp-all-positions returns all positions"
    (let ([positions (sexp-all-positions simple-expr)])
      ;; Should have: (), (0), (1), (2)
      (assert-equal 4 (length positions))
      (assert-true (and (member '() positions) #t))
      (assert-true (and (member '(0) positions) #t))
      (assert-true (and (member '(1) positions) #t))
      (assert-true (and (member '(2) positions) #t))))

  (define-test "sexp-all-positions handles nested"
    (let ([positions (sexp-all-positions nested-expr)])
      ;; Should have: (), (0), (1), (1 0), (1 1), (1 2), (2)
      (assert-equal 7 (length positions))
      (assert-true (and (member '(1 2) positions) #t)))))

;;; ====
;;; Traversal Tests
;;; ====

(test-group "sexp-zipper-traversal"

  (define-test "preorder visits all nodes"
    (let* ([sz (sexp->zipper simple-expr)]
           [all (sexp-zipper-preorder sz)])
      ;; (+ 1 2) has 4 nodes: root, +, 1, 2
      (assert-equal 4 (length all))))

  (define-test "sexp-zipper-next iterates correctly"
    (let* ([sz (sexp->zipper simple-expr)]
           [positions (let loop ([current (just sz)] [acc '()])
                        (if (nothing? current)
                            (reverse acc)
                            (loop (sexp-zipper-next (from-just current))
                                  (cons (sexp-zipper-get (from-just current)) acc))))])
      (assert-equal 4 (length positions))
      (assert-equal '(+ 1 2) (car positions))
      (assert-equal '+ (cadr positions))))

  (define-test "sexp-zipper-find-match finds matching node"
    (let* ([sz (sexp->zipper nested-expr)]
           [found (sexp-zipper-find-match sz (lambda (x) (equal? x 3)))])
      (assert-true (just? found))
      (assert-equal 3 (sexp-zipper-get (from-just found)))))

  (define-test "sexp-zipper-collect-matches collects all"
    (let* ([sz (sexp->zipper '(+ 1 (+ 2 3)))]
           [pluses (sexp-zipper-collect-matches sz (lambda (x) (equal? x '+)))])
      (assert-equal 2 (length pluses)))))

;;; ====
;;; Round-trip Tests
;;; ====

(test-group "sexp-zipper-roundtrip"

  (define-test "simple expression round-trip"
    (let ([result (zipper->sexp (sexp->zipper simple-expr))])
      (assert-equal simple-expr result)))

  (define-test "nested expression round-trip"
    (let ([result (zipper->sexp (sexp->zipper nested-expr))])
      (assert-equal nested-expr result)))

  (define-test "deep expression round-trip"
    (let ([result (zipper->sexp (sexp->zipper deep-expr))])
      (assert-equal deep-expr result)))

  (define-test "atom round-trip"
    (let ([result (zipper->sexp (sexp->zipper atom-expr))])
      (assert-equal atom-expr result))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
