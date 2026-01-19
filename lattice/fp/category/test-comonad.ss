;;; lattice/fp/category/test-comonad.ss — Tests for Comonad Infrastructure
;;;
;;; Tests comonad type class, Store, Env, Traced comonads, and law verification.

(load "core/testing/test-framework.ss")
(load "lattice/fp/templates.ss")  ; For monoid-sum, monoid-mempty, monoid-mappend
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
      (assert-true (verify-comonad-law-2 store-comonad f st))))

  (define-test "store comonad law 3: extend f . extend g = extend (f . extend g)"
    (let* ([accessor (lambda (x) (* x x))]  ; squares
           [st (make-store accessor 3)]
           [f (lambda (s) (* 2 (store-extract s)))]   ; double
           [g (lambda (s) (+ (store-extract s) 1))])  ; add 1
      (assert-true
       (verify-comonad-law-3
        store-comonad f g st
        (lambda (a b)
          (and (store? a) (store? b)
               (equal? (store-position a) (store-position b))
               ;; Check values at multiple positions
               (equal? (store-peek a 0) (store-peek b 0))
               (equal? (store-peek a 3) (store-peek b 3))
               (equal? (store-peek a 7) (store-peek b 7)))))))))

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
      (assert-true (verify-comonad-law-2 env-comonad f e))))

  (define-test "env comonad law 3: extend f . extend g = extend (f . extend g)"
    (let* ([e (make-env 100 5)]  ; env=100, value=5
           [f (lambda (env) (* 2 (env-value env)))]   ; double value
           [g (lambda (env) (+ (env-environment env) (env-value env)))])  ; env + value
      (assert-true
       (verify-comonad-law-3
        env-comonad f g e
        (lambda (a b)
          (and (env? a) (env? b)
               (equal? (env-environment a) (env-environment b))
               (equal? (env-value a) (env-value b)))))))))

;;; ====
;;; Traced Comonad Tests
;;; ====

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

  (define-test "traced comonad law 1: extend extract = id"
    (let* ([run-fn (lambda (acc) (+ acc 10))]
           [t (make-traced run-fn monoid-sum)])
      (assert-true
       (verify-comonad-law-1
        (traced-comonad monoid-sum)
        t
        (lambda (a b)
          (and (traced? a) (traced? b)
               ;; Check values at multiple accumulator positions
               (equal? (run-traced a 0) (run-traced b 0))
               (equal? (run-traced a 10) (run-traced b 10))
               (equal? (run-traced a 25) (run-traced b 25))))))))

  (define-test "traced comonad law 2: extract . extend f = f"
    (let* ([run-fn (lambda (acc) (+ acc 1))]
           [t (make-traced run-fn monoid-sum)]
           [f (lambda (tr) (+ 100 (traced-extract tr)))])
      (assert-true (verify-comonad-law-2 (traced-comonad monoid-sum) f t))))

  (define-test "traced comonad law 3: extend f . extend g = extend (f . extend g)"
    (let* ([run-fn (lambda (acc) (+ acc 10))]  ; base value is acc + 10
           [t (make-traced run-fn monoid-sum)]
           [f (lambda (tr) (* 2 (traced-extract tr)))]   ; double
           [g (lambda (tr) (+ (traced-extract tr) 5))])  ; add 5
      (assert-true
       (verify-comonad-law-3
        (traced-comonad monoid-sum) f g t
        (lambda (a b)
          (and (traced? a) (traced? b)
               ;; Check values at multiple positions
               (equal? (run-traced a 0) (run-traced b 0))
               (equal? (run-traced a 10) (run-traced b 10))
               (equal? (run-traced a 25) (run-traced b 25)))))))))

;;; ====
;;; Comonad Derivation from Adjunction Tests
;;; ====

;;; Product-exponential adjunction: (−×S) ⊣ (S→−)
;;; Produces the Store comonad. Use this to rigorously verify comonad-from-adjunction.
(define test-functor-pair-S
  (make-named-functor '−×S (lambda (f pair) (list (f (car pair)) (cadr pair)))))

(define test-functor-func-S
  (make-named-functor 'S→− (lambda (f func) (lambda (s) (f (func s))))))

(define adj-store-test
  (let* ([FG-functor (make-named-functor '−×S∘S→−
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
;;; Comonad Composition Tests
;;; ====
;;;
;;; Test compose-comonads-with-dist and compose-comonads-with-dist*.
;;; The key test is that the composed comonad satisfies the comonad laws.

;;; Distributive law: Store(S, Env(E, a)) → Env(E, Store(S, a))
;;; For composing Env ∘ Store: W1=Env, W2=Store, so dist : W2(W1) → W1(W2)
;;; dist ((λs. (e_s, v_s)), s0) = (e_{s0}, ((λs. v_s), s0))
;;; Assumes e_s is constant (same environment at all positions).
(define (dist-store-to-env store-env)
  (let* ([accessor (store-accessor store-env)]
         [pos (store-position store-env)]
         [env-at-pos (accessor pos)]
         [the-env (env-environment env-at-pos)])
    (make-env the-env
              (make-store (lambda (s) (env-value (accessor s))) pos))))

;;; Distributive law: Env(E, Store(S, a)) → Store(S, Env(E, a))
;;; For composing Store ∘ Env: W1=Store, W2=Env, so dist : W2(W1) → W1(W2)
;;; dist (e, (acc, s)) = ((λs'. (e, acc(s'))), s)
(define (dist-env-to-store env-store)
  (let* ([the-env (env-environment env-store)]
         [the-store (env-value env-store)]
         [accessor (store-accessor the-store)]
         [pos (store-position the-store)])
    (make-store (lambda (s) (make-env the-env (accessor s))) pos)))

(test-group "Comonad Composition with Distributive Law"

  ;; Test Env ∘ Store composition (Env outside, Store inside)
  ;; W1=Env, W2=Store. Since W2=Store has non-trivial position, we need store-copeek.
  (define-test "env-store composition: extract works correctly"
    (let* ([env-store-comonad (compose-comonads-with-dist*
                                env-comonad
                                store-comonad
                                dist-store-to-env
                                store-copeek)]
           ;; Env(E=100, Store(accessor=square, pos=3))
           [w (make-env 100 (make-store (lambda (x) (* x x)) 3))]
           [result ((comonad-extract env-store-comonad) w)])
      ;; extract should give: 3² = 9
      (assert-equal 9 result)))

  (define-test "env-store composition: extend applies f at each position"
    (let* ([env-store-comonad (compose-comonads-with-dist*
                                env-comonad
                                store-comonad
                                dist-store-to-env
                                store-copeek)]
           ;; Env(E=100, Store(accessor=square, pos=3))
           [w (make-env 100 (make-store (lambda (x) (* x x)) 3))]
           ;; f: sum of environment and extracted value
           [f (lambda (es)
                (+ (env-environment es)
                   (store-extract (env-value es))))]
           [extended ((comonad-extend env-store-comonad) f w)]
           [inner-store (env-value extended)])
      ;; Environment should be preserved
      (assert-equal 100 (env-environment extended))
      ;; At position 3: 100 + 3² = 100 + 9 = 109
      (assert-equal 109 (store-extract inner-store))
      ;; At position 5: 100 + 5² = 100 + 25 = 125
      (assert-equal 125 (store-peek inner-store 5))))

  (define-test "env-store composition: law 1 (extend extract = id)"
    (let* ([comonad (compose-comonads-with-dist*
                      env-comonad
                      store-comonad
                      dist-store-to-env
                      store-copeek)]
           [w (make-env 42 (make-store (lambda (x) (+ x 10)) 5))]
           [ext (comonad-extend comonad)]
           [extr (comonad-extract comonad)]
           [result (ext extr w)])
      ;; Check environment preserved
      (assert-equal (env-environment w) (env-environment result))
      ;; Check store position preserved
      (assert-equal (store-position (env-value w))
                    (store-position (env-value result)))
      ;; Check values at multiple positions
      (assert-equal (store-peek (env-value w) 0)
                    (store-peek (env-value result) 0))
      (assert-equal (store-peek (env-value w) 5)
                    (store-peek (env-value result) 5))
      (assert-equal (store-peek (env-value w) 10)
                    (store-peek (env-value result) 10))))

  (define-test "env-store composition: law 2 (extract . extend f = f)"
    (let* ([comonad (compose-comonads-with-dist*
                      env-comonad
                      store-comonad
                      dist-store-to-env
                      store-copeek)]
           [w (make-env 100 (make-store (lambda (x) (* x x)) 3))]
           [f (lambda (es)
                (* 2 (+ (env-environment es)
                        (store-extract (env-value es)))))]
           [ext (comonad-extend comonad)]
           [extr (comonad-extract comonad)]
           [lhs (extr (ext f w))]
           [rhs (f w)])
      (assert-equal rhs lhs)))

  (define-test "env-store composition: law 3 (extend f . extend g = extend (f . extend g))"
    (let* ([comonad (compose-comonads-with-dist*
                      env-comonad
                      store-comonad
                      dist-store-to-env
                      store-copeek)]
           [w (make-env 10 (make-store (lambda (x) (* x x)) 3))]
           [f (lambda (es) (* 2 (store-extract (env-value es))))]
           [g (lambda (es) (+ (env-environment es) (store-extract (env-value es))))]
           [ext (comonad-extend comonad)]
           [lhs (ext f (ext g w))]
           [rhs (ext (lambda (es) (f (ext g es))) w)]
           [lhs-store (env-value lhs)]
           [rhs-store (env-value rhs)])
      ;; Check environments match
      (assert-equal (env-environment lhs) (env-environment rhs))
      ;; Check store positions match
      (assert-equal (store-position lhs-store) (store-position rhs-store))
      ;; Check values at multiple positions
      (assert-equal (store-peek lhs-store 0) (store-peek rhs-store 0))
      (assert-equal (store-peek lhs-store 3) (store-peek rhs-store 3))
      (assert-equal (store-peek lhs-store 7) (store-peek rhs-store 7)))))

(test-group "Comonad Composition with Env (trivial position)"

  ;; Test Store ∘ Env composition (Store outside, Env inside)
  ;; W1=Store, W2=Env. Since W2=Env has trivial position, the default copeek works.
  (define-test "store-env composition: extract works correctly"
    (let* ([store-env-comonad (compose-comonads-with-dist
                                store-comonad
                                env-comonad
                                dist-env-to-store)]
           ;; Store(accessor=returns Env(E=e, val=e*s), pos=3)
           [w (make-store (lambda (s) (make-env (* s 10) (* s s))) 3)]
           [result ((comonad-extract store-env-comonad) w)])
      ;; extract should give: 3² = 9
      (assert-equal 9 result)))

  (define-test "store-env composition: extend applies f correctly"
    (let* ([store-env-comonad (compose-comonads-with-dist
                                store-comonad
                                env-comonad
                                dist-env-to-store)]
           ;; Store(accessor=returns Env(E=s*10, val=s²), pos=3)
           [w (make-store (lambda (s) (make-env (* s 10) (* s s))) 3)]
           ;; f: extract the inner env's value
           [f (lambda (se)
                (let ([inner-env (store-extract se)])
                  (env-value inner-env)))]
           [extended ((comonad-extend store-env-comonad) f w)])
      ;; At position 3: inner env value = 3² = 9
      (assert-equal 9 (env-value (store-extract extended)))
      ;; At position 5: inner env value = 5² = 25
      (assert-equal 25 (env-value (store-peek extended 5)))))

  (define-test "store-env composition: law 1 (extend extract = id)"
    (let* ([comonad (compose-comonads-with-dist
                      store-comonad
                      env-comonad
                      dist-env-to-store)]
           [w (make-store (lambda (s) (make-env (* s 10) (+ s 5))) 3)]
           [ext (comonad-extend comonad)]
           [extr (comonad-extract comonad)]
           [result (ext extr w)])
      ;; Check store position preserved
      (assert-equal (store-position w) (store-position result))
      ;; Check inner env values at multiple positions
      (assert-equal (env-environment (store-peek w 0))
                    (env-environment (store-peek result 0)))
      (assert-equal (env-value (store-peek w 0))
                    (env-value (store-peek result 0)))
      (assert-equal (env-environment (store-peek w 3))
                    (env-environment (store-peek result 3)))
      (assert-equal (env-value (store-peek w 3))
                    (env-value (store-peek result 3)))
      (assert-equal (env-environment (store-peek w 7))
                    (env-environment (store-peek result 7)))
      (assert-equal (env-value (store-peek w 7))
                    (env-value (store-peek result 7)))))

  (define-test "store-env composition: law 2 (extract . extend f = f)"
    (let* ([comonad (compose-comonads-with-dist
                      store-comonad
                      env-comonad
                      dist-env-to-store)]
           [w (make-store (lambda (s) (make-env s (* s s))) 4)]
           [f (lambda (se)
                (+ (env-environment (store-extract se))
                   (env-value (store-extract se))))]
           [ext (comonad-extend comonad)]
           [extr (comonad-extract comonad)]
           [lhs (extr (ext f w))]
           [rhs (f w)])
      ;; At position 4: env=4, val=16, so f = 4 + 16 = 20
      (assert-equal rhs lhs)
      (assert-equal 20 lhs)))

  (define-test "store-env composition: law 3 (extend f . extend g = extend (f . extend g))"
    (let* ([comonad (compose-comonads-with-dist
                      store-comonad
                      env-comonad
                      dist-env-to-store)]
           [w (make-store (lambda (s) (make-env (* s 10) (* s s))) 3)]
           [f (lambda (se) (* 2 (env-value (store-extract se))))]  ; double inner value
           [g (lambda (se) (+ (env-environment (store-extract se))
                              (env-value (store-extract se))))]    ; env + val
           [ext (comonad-extend comonad)]
           [lhs (ext f (ext g w))]
           [rhs (ext (lambda (se) (f (ext g se))) w)])
      ;; Check store positions match
      (assert-equal (store-position lhs) (store-position rhs))
      ;; Check inner env values at multiple positions
      (assert-equal (env-value (store-peek lhs 0))
                    (env-value (store-peek rhs 0)))
      (assert-equal (env-value (store-peek lhs 3))
                    (env-value (store-peek rhs 3)))
      (assert-equal (env-value (store-peek lhs 7))
                    (env-value (store-peek rhs 7))))))

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
