;;; lattice/fp/category/test-kan-extension.ss --- Tests for Kan Extensions
;;;
;;; Tests for Right Kan Extension, Left Kan Extension, and Codensity monad.

(load "core/testing/test-framework.ss")
(load "lattice/fp/category/kan-extension.ss")

;;; ============================================================
;;; Right Kan Extension Tests
;;; ============================================================

(test-group "Right Kan Extension (Ran)"

  (define-test "make-ran creates valid Ran structure"
    (let ([ran (make-ran functor-list functor-maybe (lambda (k) (just (k 42))))])
      (assert-true (ran? ran))
      (assert-true (functor? (ran-k ran)))
      (assert-true (functor? (ran-f ran)))
      (assert-true (procedure? (ran-computation ran)))))

  (define-test "ran-apply applies K-morphism correctly"
    (let ([ran (make-ran functor-list functor-maybe (lambda (k) (just (k 42))))])
      ;; Apply identity: should get (just 42)
      (assert-equal (just 42) (ran-apply ran identity))
      ;; Apply doubling: should get (just 84)
      (assert-equal (just 84) (ran-apply ran (lambda (x) (* x 2))))))

  (define-test "ran-fmap satisfies functor identity law"
    (let ([ran (make-ran functor-list functor-maybe (lambda (k) (just (k 10))))])
      ;; fmap id = id
      (let ([mapped (ran-fmap identity ran)])
        (assert-equal (ran-apply ran identity)
                      (ran-apply mapped identity)))))

  (define-test "ran-fmap satisfies functor composition law"
    (let ([ran (make-ran functor-list functor-maybe (lambda (k) (just (k 10))))]
          [f (lambda (x) (+ x 1))]
          [g (lambda (x) (* x 2))])
      ;; fmap (g . f) = fmap g . fmap f
      (let ([mapped-composed (ran-fmap (compose2 g f) ran)]
            [mapped-separate (ran-fmap g (ran-fmap f ran))])
        (assert-equal (ran-apply mapped-composed identity)
                      (ran-apply mapped-separate identity))))))

;;; ============================================================
;;; Ran Lifting Tests
;;; ============================================================

(test-group "Ran Lifting"

  (define-test "ran-lift errors with helpful message"
    ;; ran-lift is deprecated and should error
    (assert-error (lambda () (ran-lift functor-list functor-maybe (just 42)))))

  (define-test "ran-lift-identity works for identity case"
    ;; For K = Id, ran-lift-identity is valid (though codensity-lift is preferred)
    (let* ([ran (ran-lift-identity functor-id functor-maybe (just 42))])
      (assert-true (ran? ran))
      ;; When applied with identity, returns the original value
      (assert-equal (just 42) (ran-apply ran identity))))

  (define-test "ran-lift-with-transform allows proper lifting"
    ;; Example: lifting with a transform that uses F's fmap
    ;; If we have F = Maybe with fmap, and K has some action,
    ;; we can define how to transform F a given (a -> K b) -> F b
    ;;
    ;; Trivial transform for testing: just returns the original (like identity case)
    (let* ([trivial-transform (lambda (k fa) fa)]
           [ran (ran-lift-with-transform trivial-transform
                                          functor-list
                                          functor-maybe
                                          (just 42))])
      (assert-true (ran? ran))
      (assert-equal (just 42) (ran-apply ran identity))))

  (define-test "ran-lift-with-transform can apply actual transformations"
    ;; A more realistic transform: F = List, and we use list-fmap
    ;; Given k : a -> b, transform = (lambda (k fa) (map k fa))
    (let* ([transform (lambda (k fa) (map k fa))]
           [ran (ran-lift-with-transform transform
                                          functor-id  ; K = Id
                                          functor-list
                                          '(1 2 3))])
      ;; Apply with doubling function
      (assert-equal '(2 4 6) (ran-apply ran (lambda (x) (* x 2))))
      ;; Apply with identity
      (assert-equal '(1 2 3) (ran-apply ran identity)))))

;;; ============================================================
;;; Left Kan Extension Tests
;;; ============================================================

(test-group "Left Kan Extension (Lan)"

  (define-test "make-lan creates valid Lan structure"
    (let ([lan (make-lan functor-list functor-maybe identity (just 42))])
      (assert-true (lan? lan))
      (assert-true (functor? (lan-k lan)))
      (assert-true (functor? (lan-f lan)))
      (assert-true (procedure? (lan-morphism lan)))
      (assert-equal (just 42) (lan-value lan))))

  (define-test "lan-inject creates Lan from morphism and value"
    (let ([lan (lan-inject functor-list functor-maybe (just 42) (lambda (x) (* x 2)))])
      (assert-true (lan? lan))
      (assert-equal 84 ((lan-morphism lan) 42))))

  (define-test "lan-fmap satisfies functor identity law"
    (let ([lan (make-lan functor-list functor-maybe identity (just 10))])
      ;; fmap id = id
      (let ([mapped (lan-fmap identity lan)])
        (assert-equal (lan-value lan) (lan-value mapped))
        (assert-equal ((lan-morphism lan) 5) ((lan-morphism mapped) 5)))))

  (define-test "lan-fmap satisfies functor composition law"
    (let ([lan (make-lan functor-list functor-maybe identity (just 10))]
          [f (lambda (x) (+ x 1))]
          [g (lambda (x) (* x 2))])
      ;; fmap (g . f) = fmap g . fmap f
      (let ([mapped-composed (lan-fmap (compose2 g f) lan)]
            [mapped-separate (lan-fmap g (lan-fmap f lan))])
        ;; Values should be the same
        (assert-equal (lan-value mapped-composed) (lan-value mapped-separate))
        ;; Morphisms should be equivalent
        (assert-equal ((lan-morphism mapped-composed) 5)
                      ((lan-morphism mapped-separate) 5))))))

;;; ============================================================
;;; Codensity Monad Tests
;;; ============================================================

(test-group "Codensity Monad Basics"

  (define-test "codensity-return creates valid Codensity"
    (let ([c (codensity-return just 42)])
      (assert-true (codensity? c))
      (assert-true (procedure? (codensity-return-fn c)))
      (assert-true (procedure? (codensity-run c)))))

  (define-test "codensity-lower extracts value correctly"
    (let ([c (codensity-return just 42)])
      (assert-equal (just 42) (codensity-lower c))))

  (define-test "codensity satisfies left identity: return x >>= f = f x"
    (let* ([x 10]
           [f (lambda (a) (codensity-return just (* a 2)))]
           [left-side (codensity-bind (codensity-return just x) f)]
           [right-side (f x)])
      (assert-equal (codensity-lower left-side)
                    (codensity-lower right-side))))

  (define-test "codensity satisfies right identity: m >>= return = m"
    (let* ([m (codensity-return just 42)]
           [bound (codensity-bind m (lambda (x) (codensity-return just x)))])
      (assert-equal (codensity-lower m)
                    (codensity-lower bound))))

  (define-test "codensity satisfies associativity: (m >>= f) >>= g = m >>= (x -> f x >>= g)"
    (let* ([m (codensity-return just 5)]
           [f (lambda (x) (codensity-return just (+ x 1)))]
           [g (lambda (x) (codensity-return just (* x 2)))]
           [left-side (codensity-bind (codensity-bind m f) g)]
           [right-side (codensity-bind m (lambda (x) (codensity-bind (f x) g)))])
      (assert-equal (codensity-lower left-side)
                    (codensity-lower right-side)))))

(test-group "Codensity Map"

  (define-test "codensity-map applies function"
    (let* ([c (codensity-return just 10)]
           [mapped (codensity-map (lambda (x) (* x 2)) c)])
      (assert-equal (just 20) (codensity-lower mapped))))

  (define-test "codensity-map satisfies identity: fmap id = id"
    (let* ([c (codensity-return just 42)]
           [mapped (codensity-map identity c)])
      (assert-equal (codensity-lower c)
                    (codensity-lower mapped))))

  (define-test "codensity-map satisfies composition: fmap (g . f) = fmap g . fmap f"
    (let* ([c (codensity-return just 5)]
           [f (lambda (x) (+ x 1))]
           [g (lambda (x) (* x 2))]
           [left-side (codensity-map (compose2 g f) c)]
           [right-side (codensity-map g (codensity-map f c))])
      (assert-equal (codensity-lower left-side)
                    (codensity-lower right-side)))))

;;; ============================================================
;;; Codensity Lift/Lower Round-trip
;;; ============================================================

(test-group "Codensity Lift/Lower"

  (define-test "lower . lift = id for Maybe"
    (let* ([m-return just]
           [m-bind maybe-bind]
           [ma (just 42)]
           [lifted (codensity-lift m-return m-bind ma)]
           [lowered (codensity-lower lifted)])
      (assert-equal ma lowered)))

  (define-test "lower . lift = id for various values"
    (let* ([m-return just]
           [m-bind maybe-bind])
      ;; Test with different values
      (for-each
       (lambda (val)
         (let* ([ma (just val)]
                [round-tripped (codensity-lower (codensity-lift m-return m-bind ma))])
           (assert-equal ma round-tripped)))
       '(0 1 42 -5 100)))))

;;; ============================================================
;;; Codensity List
;;; ============================================================

(test-group "Codensity List"

  (define-test "codensity-list-singleton creates single element"
    (let ([c (codensity-list-singleton 42)])
      (assert-equal '(42) (codensity-list-lower c))))

  (define-test "codensity-list with bind composes monadic computations"
    ;; Codensity provides O(1) bind, not O(1) append
    (let* ([c1 (codensity-list-singleton 1)]
           [c2 (codensity-bind c1 (lambda (x)
                                    (codensity-list-singleton (+ x 10))))])
      (assert-equal '(11) (codensity-list-lower c2)))))

;;; ============================================================
;;; Difference Lists (True O(1) Append)
;;; ============================================================

(test-group "Difference Lists"

  (define-test "dlist-singleton creates single element"
    (assert-equal '(42) (dlist-to-list (dlist-singleton 42))))

  (define-test "dlist-append combines lists in O(1)"
    (let* ([d1 (dlist-singleton 1)]
           [d2 (dlist-singleton 2)]
           [combined (dlist-append d1 d2)])
      (assert-equal '(1 2) (dlist-to-list combined))))

  (define-test "dlist-append is associative"
    (let* ([d1 (dlist-singleton 1)]
           [d2 (dlist-singleton 2)]
           [d3 (dlist-singleton 3)]
           [left-assoc (dlist-append (dlist-append d1 d2) d3)]
           [right-assoc (dlist-append d1 (dlist-append d2 d3))])
      (assert-equal (dlist-to-list left-assoc)
                    (dlist-to-list right-assoc))))

  (define-test "many dlist appends produce correct result"
    (let* ([singles (map dlist-singleton '(1 2 3 4 5))]
           [combined (fold-left dlist-append
                                (dlist-singleton 0)
                                singles)])
      (assert-equal '(0 1 2 3 4 5) (dlist-to-list combined))))

  (define-test "dlist-from-list round trips"
    (assert-equal '(a b c) (dlist-to-list (dlist-from-list '(a b c)))))

  (define-test "dlist-cons prepends element"
    (let ([dl (dlist-cons 1 (dlist-cons 2 (dlist-singleton 3)))])
      (assert-equal '(1 2 3) (dlist-to-list dl))))

  (define-test "dlist-snoc appends element"
    (let ([dl (dlist-snoc (dlist-snoc (dlist-singleton 1) 2) 3)])
      (assert-equal '(1 2 3) (dlist-to-list dl))))

  (define-test "dlist-empty is identity for append"
    (let ([d (dlist-singleton 42)])
      (assert-equal (dlist-to-list d)
                    (dlist-to-list (dlist-append dlist-empty d)))
      (assert-equal (dlist-to-list d)
                    (dlist-to-list (dlist-append d dlist-empty))))))

;;; ============================================================
;;; Codensity Maybe
;;; ============================================================

(test-group "Codensity Maybe"

  (define-test "codensity-maybe-return wraps value"
    (let ([c (codensity-maybe-return 42)])
      (assert-equal (just 42) (codensity-lower c))))

  (define-test "codensity-maybe-fail produces nothing"
    (assert-equal nothing (codensity-lower codensity-maybe-fail)))

  (define-test "codensity-maybe-bind chains computations"
    (let* ([c1 (codensity-maybe-return 10)]
           [c2 (codensity-maybe-bind c1
                 (lambda (x) (codensity-maybe-return (* x 2))))])
      (assert-equal (just 20) (codensity-lower c2))))

  (define-test "codensity-maybe-bind short-circuits on fail"
    (let* ([c1 codensity-maybe-fail]
           [c2 (codensity-maybe-bind c1
                 (lambda (x) (codensity-maybe-return (* x 2))))])
      (assert-equal nothing (codensity-lower c2)))))

;;; ============================================================
;;; Codensity Monad Builder
;;; ============================================================

(test-group "Codensity Monad Builder"

  (define-test "make-codensity-monad creates working monad"
    (let* ([cm (make-codensity-monad just maybe-bind)]
           [ret (codensity-monad-return cm)]
           [bnd (codensity-monad-bind cm)]
           [lift (codensity-monad-lift cm)]
           [lower (codensity-monad-lower cm)])
      ;; Test return
      (assert-equal (just 42) (lower (ret 42)))
      ;; Test bind
      (assert-equal (just 84)
                    (lower (bnd (ret 42)
                               (lambda (x) (ret (* x 2))))))
      ;; Test lift/lower round-trip
      (assert-equal (just 10) (lower (lift (just 10)))))))

;;; ============================================================
;;; Performance Characteristics (Structural)
;;; ============================================================

(test-group "Codensity Performance Structure"

  (define-test "multiple binds don't increase nesting depth"
    ;; This test verifies the structure, not actual performance
    (let* ([c0 (codensity-return just 0)]
           ;; Chain 10 binds
           [c10 (fold-left
                 (lambda (c _)
                   (codensity-bind c (lambda (x) (codensity-return just (+ x 1)))))
                 c0
                 (make-list 10 #f))])
      ;; Should still produce correct result
      (assert-equal (just 10) (codensity-lower c10))))

  (define-test "left-associative bind chain works correctly"
    ;; ((((c >>= f1) >>= f2) >>= f3) >>= f4)
    ;; This is the worst case for naive Free monad
    (let* ([c (codensity-return just 1)]
           [double (lambda (x) (codensity-return just (* x 2)))]
           [result (codensity-bind
                    (codensity-bind
                     (codensity-bind
                      (codensity-bind c double)
                      double)
                     double)
                    double)])
      ;; 1 * 2 * 2 * 2 * 2 = 16
      (assert-equal (just 16) (codensity-lower result)))))

;;; ============================================================
;;; Connection to Free Monad Pattern
;;; ============================================================

(test-group "Free Monad Pattern Connection"

  ;; This test group documents the connection between Codensity
  ;; and the free-queue/eff-queue patterns used in free.ss and effects.ss

  (define-test "codensity matches free-queue behavior"
    ;; The pattern in free.ss:
    ;;   ('free-queue base fmap conts)
    ;; Is equivalent to:
    ;;   Codensity (Free f) with accumulated continuations
    ;;
    ;; We verify the essential property: O(1) bind structure
    (let* ([n 100]
           [binds (make-list n (lambda (x) (codensity-return just (+ x 1))))]
           [result (fold-left codensity-bind
                             (codensity-return just 0)
                             binds)])
      (assert-equal (just n) (codensity-lower result))))

  (define-test "codensity correctly reifies continuation queue"
    ;; The continuation queue pattern: instead of
    ;;   m >>= f >>= g >>= h
    ;; which builds ((m >>= f) >>= g) >>= h (left-nested),
    ;; Codensity stores:
    ;;   base: m, conts: [f, g, h]
    ;; And applies all at once during lower.
    ;;
    ;; We verify by checking that the result is equivalent to
    ;; applying all functions in sequence.
    (let* ([funcs (list (lambda (x) (+ x 1))
                        (lambda (x) (* x 2))
                        (lambda (x) (- x 3)))]
           [codensity-result
            (codensity-lower
             (fold-left (lambda (c f)
                          (codensity-bind c (lambda (x) (codensity-return just (f x)))))
                        (codensity-return just 10)
                        funcs))]
           [direct-result (fold-left (lambda (acc f) (f acc)) 10 funcs)])
      ;; (10 + 1) * 2 - 3 = 19
      (assert-equal (just direct-result) codensity-result))))

;;; ============================================================
;;; Difference Lists (True O(1) Append)
;;; ============================================================

(test-group "Difference Lists"

  (define-test "dlist-empty produces empty list"
    (assert-equal '() (dlist-to-list dlist-empty)))

  (define-test "dlist-singleton creates single-element list"
    (assert-equal '(42) (dlist-to-list (dlist-singleton 42))))

  (define-test "dlist-from-list roundtrips"
    (assert-equal '(1 2 3) (dlist-to-list (dlist-from-list '(1 2 3)))))

  (define-test "dlist-append concatenates lists"
    (let* ([d1 (dlist-from-list '(1 2 3))]
           [d2 (dlist-from-list '(4 5 6))]
           [combined (dlist-append d1 d2)])
      (assert-equal '(1 2 3 4 5 6) (dlist-to-list combined))))

  (define-test "dlist-append with empty"
    (let ([d (dlist-from-list '(1 2 3))])
      (assert-equal '(1 2 3) (dlist-to-list (dlist-append dlist-empty d)))
      (assert-equal '(1 2 3) (dlist-to-list (dlist-append d dlist-empty)))))

  (define-test "dlist-cons prepends element"
    (let* ([d (dlist-from-list '(2 3))]
           [result (dlist-cons 1 d)])
      (assert-equal '(1 2 3) (dlist-to-list result))))

  (define-test "dlist-snoc appends element"
    (let* ([d (dlist-from-list '(1 2))]
           [result (dlist-snoc d 3)])
      (assert-equal '(1 2 3) (dlist-to-list result))))

  (define-test "dlist-concat flattens list of dlists"
    (let* ([d1 (dlist-from-list '(1 2))]
           [d2 (dlist-from-list '(3 4))]
           [d3 (dlist-from-list '(5 6))]
           [combined (dlist-concat (list d1 d2 d3))])
      (assert-equal '(1 2 3 4 5 6) (dlist-to-list combined))))

  (define-test "multiple dlist-append is O(1) per operation"
    ;; Chain many appends - if this were O(n) per append, it would be O(n^2)
    ;; With true difference lists, it should be O(n) total
    (let* ([dlists (map dlist-singleton '(1 2 3 4 5 6 7 8 9 10))]
           [combined (fold-left dlist-append dlist-empty dlists)])
      (assert-equal '(1 2 3 4 5 6 7 8 9 10) (dlist-to-list combined))))

  (define-test "left-associative appends work correctly"
    ;; ((a ++ b) ++ c) ++ d - worst case for regular lists
    (let* ([a (dlist-singleton 'a)]
           [b (dlist-singleton 'b)]
           [c (dlist-singleton 'c)]
           [d (dlist-singleton 'd)]
           [result (dlist-append (dlist-append (dlist-append a b) c) d)])
      (assert-equal '(a b c d) (dlist-to-list result)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(run-all-tests)
