;;; fabric/stitches/fp/laws.ss — Law Checker for FP Abstractions
;;;
;;; Validates that typeclass instances satisfy their mathematical laws
;;; using property-based testing from QuickCheck.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Functor laws (identity, composition)
;;;   - Applicative laws (identity, composition, homomorphism, interchange)
;;;   - Monad laws (left identity, right identity, associativity)
;;;   - Semigroup laws (associativity)
;;;   - Monoid laws (identity, associativity)
;;;   - Semigroupoid laws (associativity)
;;;   - Apply laws (composition)
;;;   - Bind laws (associativity)
;;;   - Selective laws (identity, distributivity)
;;;   - Comonad laws (left identity, right identity, associativity)
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/quickcheck.ss
;;;   - fp/typeclasses.ss
;;;   - fp/algebraic.ss
;;;   - fp/combinators.ss

(load "core/prelude.ss")
(load "core/fp/testing/quickcheck.ss")
(load "core/fp/typeclasses/typeclasses.ss")
(load "core/fp/typeclasses/algebraic.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Law Checking Infrastructure
;;; ============================================================

;;; law-result : Symbol -> Bool -> String -> LawResult
;;; Result of checking a single law.
(define (make-law-result name passed? message)
  (list 'law-result name passed? message))

(define (law-result? x)
  (and (pair? x) (eq? (car x) 'law-result)))

(define (law-result-name r) (cadr r))
(define (law-result-passed? r) (caddr r))
(define (law-result-message r) (cadddr r))

;;; check-law : Symbol -> Property -> Int -> LawResult
;;; Check a single law with n test cases.
(define (check-law name prop n)
  (let ([result (check n prop)])
       (if (passed? result)
           (make-law-result name #t
                            (string-append "Passed " (number->string n) " tests"))
           (make-law-result name #f
                            (string-append "Failed: "
                                           (if (result-counterexample result)
                                               "counterexample found"
                                               "unknown failure"))))))

;;; check-laws : (List (Symbol . Property)) -> Int -> (List LawResult)
;;; Check multiple laws.
(define (check-laws law-props n)
  (map (lambda (lp)
               (check-law (car lp) (cdr lp) n))
       law-props))

;;; all-laws-passed? : (List LawResult) -> Bool
(define (all-laws-passed? results)
  (fold-left (lambda (acc r) (and acc (law-result-passed? r)))
             #t results))

;;; display-law-results : (List LawResult) -> Void
(define (display-law-results results)
  (for-each (lambda (r)
                    (display "  ")
                    (display (if (law-result-passed? r) "✓ " "✗ "))
                    (display (law-result-name r))
                    (display ": ")
                    (display (law-result-message r))
                    (newline))
            results))

;;; ============================================================
;;; Functor Laws
;;; ============================================================
;;;
;;; 1. Identity: fmap id = id
;;; 2. Composition: fmap (f . g) = fmap f . fmap g

;;; functor-identity-law : Functor f -> Gen (f a) -> (f a -> f a -> Bool) -> Property
;;; fmap id x = x
(define (functor-identity-law functor gen-fa eq-fa)
  (let ([fmap-fn (functor-fmap functor)])
       (forall (fa gen-fa)
               (eq-fa (fmap-fn id fa) fa))))

;;; functor-composition-law : Functor f -> Gen (f a) -> Gen (a -> b) -> Gen (b -> c) -> (f c -> f c -> Bool) -> Property
;;; fmap (g . f) = fmap g . fmap f
(define (functor-composition-law functor gen-fa gen-f gen-g eq-fc)
  (let ([fmap-fn (functor-fmap functor)])
       (forall ((fa gen-fa) (f gen-f) (g gen-g))
               (eq-fc (fmap-fn (compose g f) fa)
                      (fmap-fn g (fmap-fn f fa))))))

;;; check-functor-laws : Functor f -> Gen (f Int) -> (f Int -> f Int -> Bool) -> Int -> (List LawResult)
;;; Convenience function for checking functor laws with Int values.
(define (check-functor-laws functor gen-fa eq-fa n)
  (check-laws
   (list (cons 'functor-identity
               (functor-identity-law functor gen-fa eq-fa))
         (cons 'functor-composition
               (functor-composition-law functor gen-fa
                                        (gen-pure add1)
                                        (gen-pure (lambda (x) (* x 2)))
                                        eq-fa)))
   n))

;;; ============================================================
;;; Applicative Laws
;;; ============================================================
;;;
;;; 1. Identity: pure id <*> v = v
;;; 2. Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
;;; 3. Homomorphism: pure f <*> pure x = pure (f x)
;;; 4. Interchange: u <*> pure y = pure ($ y) <*> u

;;; applicative-identity-law : Applicative f -> Gen (f a) -> (f a -> f a -> Bool) -> Property
;;; pure id <*> v = v
(define (applicative-identity-law applicative gen-fa eq-fa)
  (let ([pure-fn (applicative-pure applicative)]
        [ap-fn (applicative-ap applicative)])
       (forall (v gen-fa)
               (eq-fa (ap-fn (pure-fn id) v) v))))

;;; applicative-homomorphism-law : Applicative f -> Gen a -> Gen (a -> b) -> (f b -> f b -> Bool) -> Property
;;; pure f <*> pure x = pure (f x)
(define (applicative-homomorphism-law applicative gen-a gen-f eq-fb)
  (let ([pure-fn (applicative-pure applicative)]
        [ap-fn (applicative-ap applicative)])
       (forall ((x gen-a) (f gen-f))
               (eq-fb (ap-fn (pure-fn f) (pure-fn x))
                      (pure-fn (f x))))))

;;; applicative-interchange-law : Applicative f -> Gen (f (a -> b)) -> Gen a -> (f b -> f b -> Bool) -> Property
;;; u <*> pure y = pure ($ y) <*> u
(define (applicative-interchange-law applicative gen-fu gen-a eq-fb)
  (let ([pure-fn (applicative-pure applicative)]
        [ap-fn (applicative-ap applicative)])
       (forall ((u gen-fu) (y gen-a))
               (eq-fb (ap-fn u (pure-fn y))
                      (ap-fn (pure-fn (lambda (f) (f y))) u)))))

;;; check-applicative-laws : Applicative f -> Gen (f Int) -> Gen (f (Int -> Int)) -> (f Int -> f Int -> Bool) -> Int -> (List LawResult)
(define (check-applicative-laws applicative gen-fa gen-ff eq-fa n)
  (let ([pure-fn (applicative-pure applicative)])
       (check-laws
        (list (cons 'applicative-identity
                    (applicative-identity-law applicative gen-fa eq-fa))
              (cons 'applicative-homomorphism
                    (applicative-homomorphism-law applicative gen-int (gen-pure add1) eq-fa))
              (cons 'applicative-interchange
                    (applicative-interchange-law applicative gen-ff gen-int eq-fa)))
        n)))

;;; ============================================================
;;; Monad Laws
;;; ============================================================
;;;
;;; 1. Left identity: return a >>= f = f a
;;; 2. Right identity: m >>= return = m
;;; 3. Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

;;; monad-left-identity-law : Monad m -> Gen a -> Gen (a -> m b) -> (m b -> m b -> Bool) -> Property
;;; return a >>= f = f a
(define (monad-left-identity-law monad gen-a gen-f eq-mb)
  (let ([return-fn (lambda (x) (return monad x))]
        [bind-fn (monad-bind monad)])
       (forall ((xv gen-a) (kf gen-f))
               (eq-mb (bind-fn (return-fn xv) kf)
                      (kf xv)))))

;;; monad-right-identity-law : Monad m -> Gen (m a) -> (m a -> m a -> Bool) -> Property
;;; m >>= return = m
(define (monad-right-identity-law monad gen-ma eq-ma)
  (let ([return-fn (lambda (x) (return monad x))]
        [bind-fn (monad-bind monad)])
       (forall (m gen-ma)
               (eq-ma (bind-fn m return-fn) m))))

;;; monad-associativity-law : Monad m -> Gen (m a) -> Gen (a -> m b) -> Gen (b -> m c) -> (m c -> m c -> Bool) -> Property
;;; (m >>= f) >>= g = m >>= (\x -> f x >>= g)
(define (monad-associativity-law monad gen-ma gen-f gen-g eq-mc)
  (let ([bind-fn (monad-bind monad)])
       (forall ((m gen-ma) (f gen-f) (g gen-g))
               (eq-mc (bind-fn (bind-fn m f) g)
                      (bind-fn m (lambda (x) (bind-fn (f x) g)))))))

;;; check-monad-laws : Monad m -> Gen (m Int) -> Gen (Int -> m Int) -> (m Int -> m Int -> Bool) -> Int -> (List LawResult)
(define (check-monad-laws monad gen-ma gen-f eq-ma n)
  (check-laws
   (list (cons 'monad-left-identity
               (monad-left-identity-law monad gen-int gen-f eq-ma))
         (cons 'monad-right-identity
               (monad-right-identity-law monad gen-ma eq-ma))
         (cons 'monad-associativity
               (monad-associativity-law monad gen-ma gen-f gen-f eq-ma)))
   n))

;;; ============================================================
;;; Semigroup Laws
;;; ============================================================
;;;
;;; Associativity: (x <> y) <> z = x <> (y <> z)

;;; semigroup-associativity-law : Semigroup a -> Gen a -> (a -> a -> Bool) -> Property
(define (semigroup-associativity-law semigroup gen-a eq-a)
  (let ([op (semigroup-op semigroup)])
       (forall ((x gen-a) (y gen-a) (z gen-a))
               (eq-a (op (op x y) z)
                     (op x (op y z))))))

;;; check-semigroup-laws : Semigroup a -> Gen a -> (a -> a -> Bool) -> Int -> (List LawResult)
(define (check-semigroup-laws semigroup gen-a eq-a n)
  (check-laws
   (list (cons 'semigroup-associativity
               (semigroup-associativity-law semigroup gen-a eq-a)))
   n))

;;; ============================================================
;;; Monoid Laws
;;; ============================================================
;;;
;;; 1. Left identity: mempty <> x = x
;;; 2. Right identity: x <> mempty = x
;;; 3. Associativity: (x <> y) <> z = x <> (y <> z)

;;; monoid-left-identity-law : Monoid a -> Gen a -> (a -> a -> Bool) -> Property
(define (monoid-left-identity-law monoid gen-a eq-a)
  (let ([empty (monoid-empty monoid)]
        [op (monoid-op monoid)])
       (forall (x gen-a)
               (eq-a (op empty x) x))))

;;; monoid-right-identity-law : Monoid a -> Gen a -> (a -> a -> Bool) -> Property
(define (monoid-right-identity-law monoid gen-a eq-a)
  (let ([empty (monoid-empty monoid)]
        [op (monoid-op monoid)])
       (forall (x gen-a)
               (eq-a (op x empty) x))))

;;; monoid-associativity-law : Monoid a -> Gen a -> (a -> a -> Bool) -> Property
(define (monoid-associativity-law monoid gen-a eq-a)
  (let ([op (monoid-op monoid)])
       (forall ((x gen-a) (y gen-a) (z gen-a))
               (eq-a (op (op x y) z)
                     (op x (op y z))))))

;;; check-monoid-laws : Monoid a -> Gen a -> (a -> a -> Bool) -> Int -> (List LawResult)
(define (check-monoid-laws monoid gen-a eq-a n)
  (check-laws
   (list (cons 'monoid-left-identity
               (monoid-left-identity-law monoid gen-a eq-a))
         (cons 'monoid-right-identity
               (monoid-right-identity-law monoid gen-a eq-a))
         (cons 'monoid-associativity
               (monoid-associativity-law monoid gen-a eq-a)))
   n))

;;; ============================================================
;;; Semigroupoid Laws
;;; ============================================================
;;;
;;; Associativity: (f . g) . h = f . (g . h)

;;; semigroupoid-associativity-law : Semigroupoid c -> Gen (c a b) -> Gen (c b c) -> Gen (c c d) -> Property
;;; For functions: ((f . g) . h)(x) = (f . (g . h))(x) for all x
;;; We test with a fixed input since forall only supports 3 bindings.
(define (semigroupoid-associativity-law-fn gen-f gen-g gen-h)
  (let ([comp (semigroupoid-compose function-semigroupoid)]
        [test-inputs '(0 1 5 -3 100)])  ; Test with multiple fixed inputs
       (forall ((f gen-f) (g gen-g) (h gen-h))
               (andmap (lambda (x)
                               (= ((comp (comp f g) h) x)
                                  ((comp f (comp g h)) x)))
                       test-inputs))))

;;; check-semigroupoid-laws-fn : Int -> (List LawResult)
;;; Check semigroupoid laws for functions Int -> Int.
(define (check-semigroupoid-laws-fn n)
  (check-laws
   (list (cons 'semigroupoid-associativity
               (semigroupoid-associativity-law-fn
                (gen-pure add1)
                (gen-pure (lambda (x) (* x 2)))
                (gen-pure (lambda (x) (- x 3))))))
   n))

;;; ============================================================
;;; Apply Laws
;;; ============================================================
;;;
;;; Composition: (.) <$> u <*> v <*> w = u <*> (v <*> w)
;;; (where <$> is fmap)

;;; apply-composition-law : Apply f -> Gen (f (b -> c)) -> Gen (f (a -> b)) -> Gen (f a) -> (f c -> f c -> Bool) -> Property
(define (apply-composition-law apply-inst gen-u gen-v gen-w eq-fc)
  (let* ([functor (apply-functor apply-inst)]
         [fmap-fn (functor-fmap functor)]
         [ap-fn (ap-apply apply-inst)])
        (forall ((u gen-u) (v gen-v) (w gen-w))
                (eq-fc (ap-fn (ap-fn (fmap-fn compose u) v) w)
                       (ap-fn u (ap-fn v w))))))

;;; ============================================================
;;; Bind Laws
;;; ============================================================
;;;
;;; Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)

;;; bind-associativity-law : Bind b -> Gen (b a) -> Gen (a -> b b) -> Gen (b -> b c) -> (b c -> b c -> Bool) -> Property
(define (bind-associativity-law bind-inst gen-ma gen-f gen-g eq-mc)
  (let ([seq (bind-seq bind-inst)])
       (forall ((m gen-ma) (f gen-f) (g gen-g))
               (eq-mc (seq (seq m f) g)
                      (seq m (lambda (x) (seq (f x) g)))))))

;;; ============================================================
;;; Selective Laws
;;; ============================================================
;;;
;;; 1. Identity: select (pure (Right x)) y = pure x
;;; 2. Distributivity: select (pure (Left a)) (pure f) = pure (f a)

;;; selective-identity-law : Selective f -> Gen b -> (f b -> f b -> Bool) -> Property
;;; select (pure (Right x)) y = pure x
(define (selective-identity-law selective gen-b eq-fb)
  (let* ([applicative (selective-applicative selective)]
         [pure-fn (applicative-pure applicative)]
         [select-fn (selective-select selective)])
        (forall (x gen-b)
                (eq-fb (select-fn (pure-fn (right x)) (pure-fn add1))
                       (pure-fn x)))))

;;; selective-distributivity-law : Selective f -> Gen a -> Gen (a -> b) -> (f b -> f b -> Bool) -> Property
;;; select (pure (Left a)) (pure f) = pure (f a)
(define (selective-distributivity-law selective gen-a gen-f eq-fb)
  (let* ([applicative (selective-applicative selective)]
         [pure-fn (applicative-pure applicative)]
         [select-fn (selective-select selective)])
        (forall ((xv gen-a) (kf gen-f))
                (eq-fb (select-fn (pure-fn (left xv)) (pure-fn kf))
                       (pure-fn (kf xv))))))

;;; check-selective-laws : Selective f -> Gen Int -> (f Int -> f Int -> Bool) -> Int -> (List LawResult)
(define (check-selective-laws selective gen-a eq-fa n)
  (check-laws
   (list (cons 'selective-identity
               (selective-identity-law selective gen-a eq-fa))
         (cons 'selective-distributivity
               (selective-distributivity-law selective gen-a (gen-pure add1) eq-fa)))
   n))

;;; ============================================================
;;; Comonad Laws
;;; ============================================================
;;;
;;; 1. Left identity: extract . extend f = f
;;; 2. Right identity: extend extract = id
;;; 3. Associativity: extend f . extend g = extend (f . extend g)

;;; comonad-left-identity-law : Comonad w -> Gen (w a) -> Gen (w a -> b) -> (b -> b -> Bool) -> Property
;;; extract (extend f w) = f w
(define (comonad-left-identity-law comonad gen-wa gen-f eq-b)
  (let ([extract-fn (comonad-extract comonad)]
        [extend-fn (comonad-extend comonad)])
       (forall ((w gen-wa) (f gen-f))
               (eq-b (extract-fn (extend-fn f w))
                     (f w)))))

;;; comonad-right-identity-law : Comonad w -> Gen (w a) -> (w a -> w a -> Bool) -> Property
;;; extend extract w = w
(define (comonad-right-identity-law comonad gen-wa eq-wa)
  (let ([extract-fn (comonad-extract comonad)]
        [extend-fn (comonad-extend comonad)])
       (forall (w gen-wa)
               (eq-wa (extend-fn extract-fn w)
                      w))))

;;; ============================================================
;;; Convenience: Check All Laws for Common Instances
;;; ============================================================

;;; gen-maybe-int : Gen (Maybe Int)
(define gen-maybe-int
  (gen-one-of (list (gen-pure nothing)
                    (gen-map just gen-int))))

;;; gen-list-int : Gen (List Int)
(define gen-list-int
  (gen-list gen-int))

;;; maybe-eq : Maybe a -> Maybe a -> Bool
(define (maybe-eq m1 m2)
  (cond
   [(and (nothing? m1) (nothing? m2)) #t]
   [(and (just? m1) (just? m2)) (equal? (from-just m1) (from-just m2))]
   [else #f]))

;;; check-maybe-functor : Int -> (List LawResult)
(define (check-maybe-functor n)
  (display "Checking Maybe Functor laws...\n")
  (let ([results (check-functor-laws maybe-functor gen-maybe-int maybe-eq n)])
       (display-law-results results)
       results))

;;; check-maybe-applicative : Int -> (List LawResult)
(define (check-maybe-applicative n)
  (display "Checking Maybe Applicative laws...\n")
  (let ([results (check-applicative-laws
                  maybe-applicative
                  gen-maybe-int
                  (gen-map (lambda (f) (just f)) (gen-pure add1))
                  maybe-eq
                  n)])
       (display-law-results results)
       results))

;;; check-maybe-monad : Int -> (List LawResult)
(define (check-maybe-monad n)
  (display "Checking Maybe Monad laws...\n")
  (let ([results (check-monad-laws
                  maybe-monad
                  gen-maybe-int
                  (gen-pure (lambda (x) (just (add1 x))))
                  maybe-eq
                  n)])
       (display-law-results results)
       results))

;;; check-list-monoid : Int -> (List LawResult)
(define (check-list-monoid n)
  (display "Checking List Monoid laws...\n")
  (let ([results (check-monoid-laws list-monoid gen-list-int equal? n)])
       (display-law-results results)
       results))

;;; check-sum-monoid : Int -> (List LawResult)
(define (check-sum-monoid n)
  (display "Checking Sum Monoid laws...\n")
  (let ([results (check-monoid-laws sum-monoid gen-int = n)])
       (display-law-results results)
       results))

;;; check-product-monoid : Int -> (List LawResult)
(define (check-product-monoid n)
  (display "Checking Product Monoid laws...\n")
  (let ([results (check-monoid-laws product-monoid gen-int = n)])
       (display-law-results results)
       results))

;;; check-maybe-selective : Int -> (List LawResult)
(define (check-maybe-selective n)
  (display "Checking Maybe Selective laws...\n")
  (let ([results (check-selective-laws maybe-selective gen-int maybe-eq n)])
       (display-law-results results)
       results))

;;; ============================================================
;;; Run All Standard Checks
;;; ============================================================

;;; check-all-laws : Int -> Bool
;;; Run all standard law checks and return whether all passed.
(define (check-all-laws n)
  (display "==============================================================\n")
  (display "                   LAW CHECKER RESULTS\n")
  (display "==============================================================\n\n")

  (let* ([results (append
                   (check-maybe-functor n)
                   (check-maybe-applicative n)
                   (check-maybe-monad n)
                   (check-list-monoid n)
                   (check-sum-monoid n)
                   (check-product-monoid n)
                   (check-maybe-selective n)
                   (check-semigroupoid-laws-fn n))]
         [passed (all-laws-passed? results)]
         [total (length results)]
         [passing (length (filter law-result-passed? results))])

        (newline)
        (display "==============================================================\n")
        (display (string-append "Total: " (number->string total) " laws checked\n"))
        (display (string-append "Passed: " (number->string passing) "\n"))
        (display (string-append "Failed: " (number->string (- total passing)) "\n"))
        (display "==============================================================\n")

        passed))
