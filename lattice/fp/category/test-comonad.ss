;;; lattice/fp/category/test-comonad.ss — Tests for Comonad Infrastructure
;;;
;;; Tests comonad type class, Store, Env, Traced comonads, and law verification.

(load "core/testing/test-framework.ss")
(load "lattice/fp/category/comonad.ss")

;;; ====
;;; Store Comonad Tests
;;; ====

(test-group "Store Comonad"

  (define-test "store creation and accessors"
    (let* ([accessor (lambda (x) (* x x))]  ; squares
           [st (make-store accessor 3)])
      (assert-true (store? st))
      (assert-equal 3 (store-position st))
      (assert-equal 9 (store-extract st))))  ; 3² = 9

  (define-test "store-peek at different position"
    (let* ([accessor (lambda (x) (* x x))]
           [st (make-store accessor 3)])
      (assert-equal 16 (store-peek st 4))    ; 4² = 16
      (assert-equal 25 (store-peek st 5))))  ; 5² = 25

  (define-test "store-seek moves position"
    (let* ([accessor (lambda (x) (* x x))]
           [st (make-store accessor 3)]
           [st2 (store-seek st 5)])
      (assert-equal 5 (store-position st2))
      (assert-equal 25 (store-extract st2))))

  (define-test "store-seeks applies function to position"
    (let* ([accessor (lambda (x) (* x x))]
           [st (make-store accessor 3)]
           [st2 (store-seeks st (lambda (x) (+ x 2)))])
      (assert-equal 5 (store-position st2))))

  (define-test "store-extend applies contextual function"
    ;; accessor: x → x²
    ;; Function: sum of current and neighbor values
    (let* ([accessor (lambda (x) (* x x))]
           [st (make-store accessor 3)]
           ;; f: get current value and value at position+1
           [f (lambda (s)
                (+ (store-extract s)
                   (store-peek s (+ (store-position s) 1))))]
           [extended (store-extend f st)])
      ;; At position 3: 3² + 4² = 9 + 16 = 25
      (assert-equal 25 (store-extract extended))
      ;; At position 5: 5² + 6² = 25 + 36 = 61
      (assert-equal 61 (store-peek extended 5))))

  (define-test "store-duplicate creates nested store"
    (let* ([accessor (lambda (x) (* x x))]
           [st (make-store accessor 3)]
           [duped (store-duplicate st)])
      ;; extract of duplicate gives original store at that position
      (let ([inner (store-extract duped)])
        (assert-true (store? inner))
        (assert-equal 3 (store-position inner))
        (assert-equal 9 (store-extract inner)))))

  (define-test "store comonad law 1: extend extract = id"
    (let* ([accessor (lambda (x) (+ x 10))]
           [st (make-store accessor 5)])
      (assert-true
       (verify-comonad-law-1
        store-comonad
        st
        (lambda (a b)
          (and (store? a) (store? b)
               (equal? (store-position a) (store-position b))
               (equal? (store-extract a) (store-extract b))))))))

  (define-test "store comonad law 2: extract . extend f = f"
    (let* ([accessor (lambda (x) (+ x 10))]
           [st (make-store accessor 5)]
           [f (lambda (s) (* 2 (store-extract s)))])
      (assert-true (verify-comonad-law-2 store-comonad f st)))))

;;; ====
;;; Env Comonad Tests
;;; ====

(test-group "Env Comonad"

  (define-test "env creation and accessors"
    (let ([e (make-env 'config 42)])
      (assert-true (env? e))
      (assert-equal 'config (env-environment e))
      (assert-equal 42 (env-value e))))

  (define-test "env-extract gets value"
    (let ([e (make-env 'context 100)])
      (assert-equal 100 (env-extract e))))

  (define-test "env-ask gets environment"
    (let ([e (make-env '(debug . #t) "data")])
      (assert-equal '(debug . #t) (env-ask e))))

  (define-test "env-local modifies environment"
    (let* ([e (make-env 10 "value")]
           [e2 (env-local (lambda (x) (* x 2)) e)])
      (assert-equal 20 (env-environment e2))
      (assert-equal "value" (env-value e2))))

  (define-test "env-extend applies function with context"
    (let* ([e (make-env 100 5)]  ; env=100, value=5
           [f (lambda (env) (+ (env-environment env) (env-value env)))]
           [extended (env-extend f e)])
      ;; Function uses both env and value: 100 + 5 = 105
      (assert-equal 105 (env-extract extended))
      ;; Environment preserved
      (assert-equal 100 (env-environment extended))))

  (define-test "env-duplicate creates nested env"
    (let* ([e (make-env 'ctx 42)]
           [duped (env-duplicate e)])
      (assert-equal 'ctx (env-environment duped))
      (let ([inner (env-value duped)])
        (assert-true (env? inner))
        (assert-equal 'ctx (env-environment inner))
        (assert-equal 42 (env-value inner)))))

  (define-test "env comonad law 1: extend extract = id"
    (let ([e (make-env 'test 99)])
      (assert-true
       (verify-comonad-law-1
        env-comonad
        e
        (lambda (a b)
          (and (env? a) (env? b)
               (equal? (env-environment a) (env-environment b))
               (equal? (env-value a) (env-value b))))))))

  (define-test "env comonad law 2: extract . extend f = f"
    (let* ([e (make-env 10 5)]
           [f (lambda (env) (+ (env-environment env) (env-value env)))])
      (assert-true (verify-comonad-law-2 env-comonad f e)))))

;;; ====
;;; Traced Comonad Tests
;;; ====

;;; Simple additive monoid for testing
(define monoid-sum
  (list 'monoid 0 +))

(define (monoid-mempty m) (cadr m))
(define (monoid-mappend m) (caddr m))

(test-group "Traced Comonad"

  (define-test "traced creation and run"
    (let* ([run-fn (lambda (acc) (+ acc 100))]
           [t (make-traced run-fn monoid-sum)])
      (assert-true (traced? t))
      (assert-equal 100 (run-traced t 0))
      (assert-equal 150 (run-traced t 50))))

  (define-test "traced-extract runs with identity"
    (let* ([run-fn (lambda (acc) (* acc 2))]
           [t (make-traced run-fn monoid-sum)])
      ;; Identity of sum monoid is 0
      (assert-equal 0 (traced-extract t))))  ; 0 * 2 = 0

  (define-test "traced-extend shifts accumulator"
    (let* ([run-fn (lambda (acc) (+ acc 10))]  ; base: acc + 10
           [t (make-traced run-fn monoid-sum)]
           ;; f: get value and double it
           [f (lambda (tr) (* 2 (traced-extract tr)))]
           [extended (traced-extend f t)])
      ;; At position 0: f runs t with 0, gets (0+10)=10, doubles to 20
      (assert-equal 20 (run-traced extended 0))
      ;; At position 5: f runs t with 5, gets (5+10)=15, doubles to 30
      (assert-equal 30 (run-traced extended 5))))

  (define-test "traced comonad law 2: extract . extend f = f"
    (let* ([run-fn (lambda (acc) (+ acc 1))]
           [t (make-traced run-fn monoid-sum)]
           [f (lambda (tr) (+ 100 (traced-extract tr)))])
      (assert-true (verify-comonad-law-2 (traced-comonad monoid-sum) f t)))))

;;; ====
;;; Comonad Derivation from Adjunction Tests
;;; ====

;;; Product-exponential adjunction: (−×S) ⊣ (S→−)
;;; Produces the Store comonad. Use this to rigorously verify comonad-from-adjunction.
(define test-functor-pair-S
  (make-functor (lambda (f pair) (list (f (car pair)) (cadr pair)))))

(define test-functor-func-S
  (make-functor (lambda (f func) (lambda (s) (f (func s))))))

(define adj-store-test
  (let* ([FG-functor (make-functor
                      (lambda (f x)
                        ((functor-fmap test-functor-pair-S)
                         (lambda (y) ((functor-fmap test-functor-func-S) f y)) x)))]
         [unit (make-nat-transform 'unit-store functor-id FG-functor
                 (lambda (a) (lambda (s) (list a s))))]
         [counit (make-nat-transform 'counit-store FG-functor functor-id
                   (lambda (pair) ((car pair) (cadr pair))))])
    (make-adjunction 'store test-functor-pair-S test-functor-func-S unit counit)))

(define store-derived (comonad-from-adjunction adj-store-test))

(test-group "Comonad from Adjunction"

  (define-test "derive comonad from free-list adjunction"
    ;; adj-free-list is List ⊣ Id
    ;; The comonad would be List ∘ Id = List
    ;; But this is a bit degenerate... let's just verify it constructs
    (let ([derived (comonad-from-adjunction adj-free-list)])
      (assert-true (comonad? derived))))

  (define-test "derived store extract works correctly"
    (let* ([test-val (list (lambda (x) (* x x)) 3)]  ; accessor=square, pos=3
           [result ((comonad-extract store-derived) test-val)])
      (assert-equal 9 result)))  ; 3² = 9

  (define-test "derived store extend works correctly"
    (let* ([test-val (list (lambda (x) (* x x)) 3)]
           [f (lambda (st) (+ ((car st) (cadr st)) 1))]  ; extract + 1
           [extended ((comonad-extend store-derived) f test-val)])
      ;; At position 3: 3² + 1 = 10
      (assert-equal 10 ((car extended) 3))
      ;; At position 5: 5² + 1 = 26
      (assert-equal 26 ((car extended) 5))))

  (define-test "derived store satisfies law 1: extend extract = id"
    (let* ([test-val (list (lambda (x) (* x x)) 3)]
           [ext (comonad-extend store-derived)]
           [extr (comonad-extract store-derived)]
           [result (ext extr test-val)])
      ;; Position preserved
      (assert-equal (cadr test-val) (cadr result))
      ;; Values preserved
      (assert-equal ((car test-val) 3) ((car result) 3))
      (assert-equal ((car test-val) 7) ((car result) 7))))

  (define-test "derived store satisfies law 2: extract . extend f = f"
    (let* ([test-val (list (lambda (x) (* x x)) 3)]
           [f (lambda (st) (* 2 ((car st) (cadr st))))]
           [ext (comonad-extend store-derived)]
           [extr (comonad-extract store-derived)]
           [lhs (extr (ext f test-val))]
           [rhs (f test-val)])
      (assert-equal rhs lhs)))

  (define-test "derived store satisfies law 3: extend f . extend g = extend (f . extend g)"
    (let* ([test-val (list (lambda (x) (* x x)) 3)]
           [f (lambda (st) (* 2 ((car st) (cadr st))))]
           [g (lambda (st) (+ ((car st) (cadr st)) 1))]
           [ext (comonad-extend store-derived)]
           [lhs (ext f (ext g test-val))]
           [rhs (ext (lambda (w) (f (ext g w))) test-val)])
      ;; Position preserved
      (assert-equal (cadr lhs) (cadr rhs))
      ;; Values equal at multiple positions
      (assert-equal ((car lhs) 3) ((car rhs) 3))
      (assert-equal ((car lhs) 5) ((car rhs) 5))
      (assert-equal ((car lhs) 0) ((car rhs) 0)))))

;;; ====
;;; Integration with Existing Zipper
;;; ====

(test-group "Comonad Consistency"

  ;; Verify that our comonad structure matches zipper's ad-hoc implementation
  (define-test "store models position-based access like zipper"
    ;; Store over list indices
    (let* ([lst '(10 20 30 40 50)]
           [accessor (lambda (i) (list-ref lst i))]
           [st (make-store accessor 2)])  ; focused on index 2
      (assert-equal 30 (store-extract st))
      (assert-equal 10 (store-peek st 0))
      (assert-equal 50 (store-peek st 4)))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
