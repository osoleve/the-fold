;;; fabric/stitches/fp/property.ss — Property-Based Testing (QuickCheck-style)
;;;
;;; Generates random test cases to find counterexamples.
;;; Inspired by QuickCheck, SmallCheck, and Hypothesis.
;;;
;;; This is Core code: pure (except for random seed), assumes reasonable input.
;;;
;;; Features:
;;;   - Random generators (Gen monad)
;;;   - Shrinking for minimal counterexamples
;;;   - Property combinators (forall, implies, ==>, label)
;;;   - Built-in generators for common types
;;;   - Sized generation
;;;   - Frequency-based choice
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Random Number Generation
;;; ============================================================
;;;
;;; Simple linear congruential generator (LCG).
;;; Pure: takes seed, returns (value . new-seed).

;;; lcg-a, lcg-c, lcg-m : LCG parameters (same as glibc)
(define lcg-a 1103515245)
(define lcg-c 12345)
(define lcg-m 2147483648)  ; 2^31

;;; lcg-next : Seed -> (Int . Seed)
;;; Generate next random integer and new seed.
(define (lcg-next seed)
  (let ([next (modulo (+ (* lcg-a seed) lcg-c) lcg-m)])
    (cons next next)))

;;; random-int : Seed -> Int -> Int -> (Int . Seed)
;;; Generate random integer in [lo, hi].
(define (random-int seed lo hi)
  (let* ([r (lcg-next seed)]
         [val (car r)]
         [new-seed (cdr r)]
         [range (+ 1 (- hi lo))]
         [scaled (+ lo (modulo val range))])
    (cons scaled new-seed)))

;;; random-bool : Seed -> (Boolean . Seed)
(define (random-bool seed)
  (let ([r (random-int seed 0 1)])
    (cons (= (car r) 1) (cdr r))))

;;; ============================================================
;;; Generator Type
;;; ============================================================
;;;
;;; A Gen is a function: (Seed, Size) -> (Value . Seed)
;;; Size controls the complexity of generated values.

;;; make-gen : ((Seed, Size) -> (Value . Seed)) -> Gen a
(define (make-gen f)
  (list 'gen f))

;;; gen? : Any -> Boolean
(define (gen? x)
  (and (pair? x) (eq? (car x) 'gen)))

;;; run-gen : Gen a -> Seed -> Size -> (Value . Seed)
(define (run-gen gen seed size)
  ((cadr gen) seed size))

;;; sample-gen : Gen a -> Seed -> Size -> Value
;;; Run generator and return just the value.
(define (sample-gen gen seed size)
  (car (run-gen gen seed size)))

;;; samples : Gen a -> Seed -> Int -> List Value
;;; Generate n samples, increasing size.
(define (samples gen seed n)
  (let loop ([i 0] [s seed] [acc '()])
    (if (>= i n)
        (reverse acc)
        (let* ([size (+ 1 i)]
               [result (run-gen gen s size)]
               [val (car result)]
               [new-seed (cdr result)])
          (loop (+ i 1) new-seed (cons val acc))))))

;;; ============================================================
;;; Generator Monad
;;; ============================================================

;;; gen-pure : a -> Gen a
;;; Generator that always produces the same value.
(define (gen-pure x)
  (make-gen (lambda (seed size) (cons x seed))))

;;; gen-bind : Gen a -> (a -> Gen b) -> Gen b
;;; Monadic bind for generators.
(define (gen-bind gen f)
  (make-gen
   (lambda (seed size)
     (let* ([result (run-gen gen seed size)]
            [val (car result)]
            [new-seed (cdr result)])
       (run-gen (f val) new-seed size)))))

;;; gen-map : (a -> b) -> Gen a -> Gen b
;;; Functor map.
(define (gen-map f gen)
  (gen-bind gen (lambda (x) (gen-pure (f x)))))

;;; gen-ap : Gen (a -> b) -> Gen a -> Gen b
;;; Applicative apply.
(define (gen-ap gf ga)
  (gen-bind gf (lambda (f)
    (gen-bind ga (lambda (a)
      (gen-pure (f a)))))))

;;; gen-seq : List (Gen a) -> Gen (List a)
;;; Sequence a list of generators.
(define (gen-seq gens)
  (if (null? gens)
      (gen-pure '())
      (gen-bind (car gens) (lambda (x)
        (gen-bind (gen-seq (cdr gens)) (lambda (xs)
          (gen-pure (cons x xs))))))))

;;; ============================================================
;;; Basic Generators
;;; ============================================================

;;; gen-int : Int -> Int -> Gen Int
;;; Generate integer in range [lo, hi].
(define (gen-int lo hi)
  (make-gen
   (lambda (seed size)
     (random-int seed lo hi))))

;;; gen-nat : Gen Int
;;; Generate natural number (0 to size).
(define gen-nat
  (make-gen
   (lambda (seed size)
     (random-int seed 0 size))))

;;; gen-pos : Gen Int
;;; Generate positive integer (1 to size+1).
(define gen-pos
  (make-gen
   (lambda (seed size)
     (random-int seed 1 (+ size 1)))))

;;; gen-neg : Gen Int
;;; Generate negative integer.
(define gen-neg
  (gen-map (lambda (x) (- x)) gen-pos))

;;; gen-small-int : Gen Int
;;; Generate small integer around zero.
(define gen-small-int
  (make-gen
   (lambda (seed size)
     (random-int seed (- size) size))))

;;; gen-bool : Gen Boolean
(define gen-bool
  (make-gen
   (lambda (seed size)
     (random-bool seed))))

;;; gen-char : Gen Char
;;; Generate printable ASCII character.
(define gen-char
  (gen-map integer->char (gen-int 32 126)))

;;; gen-alpha : Gen Char
;;; Generate alphabetic character.
(define gen-alpha
  (gen-bind gen-bool (lambda (upper?)
    (if upper?
        (gen-map integer->char (gen-int 65 90))   ; A-Z
        (gen-map integer->char (gen-int 97 122)))))) ; a-z

;;; gen-digit : Gen Char
;;; Generate digit character.
(define gen-digit
  (gen-map integer->char (gen-int 48 57)))  ; 0-9

;;; ============================================================
;;; Combinators
;;; ============================================================

;;; gen-one-of : List (Gen a) -> Gen a
;;; Choose uniformly from a list of generators.
(define (gen-one-of gens)
  (if (null? gens)
      (error 'gen-one-of "empty list of generators")
      (gen-bind (gen-int 0 (- (length gens) 1))
                (lambda (i) (list-ref gens i)))))

;;; gen-alnum : Gen Char
;;; Generate alphanumeric character.
(define gen-alnum
  (gen-one-of (list gen-alpha gen-digit)))

;;; gen-elements : List a -> Gen a
;;; Choose uniformly from a list of values.
(define (gen-elements xs)
  (if (null? xs)
      (error 'gen-elements "empty list")
      (gen-map (lambda (i) (list-ref xs i))
               (gen-int 0 (- (length xs) 1)))))

;;; gen-frequency : List (Int . Gen a) -> Gen a
;;; Choose generator with weighted probability.
(define (gen-frequency weighted-gens)
  (let* ([total (fold-left (lambda (acc wg) (+ acc (car wg))) 0 weighted-gens)])
    (gen-bind (gen-int 1 total)
              (lambda (n)
                (let loop ([wgs weighted-gens] [remaining n])
                  (let ([weight (caar wgs)]
                        [gen (cdar wgs)])
                    (if (<= remaining weight)
                        gen
                        (loop (cdr wgs) (- remaining weight)))))))))

;;; gen-sized : (Size -> Gen a) -> Gen a
;;; Access the size parameter.
(define (gen-sized f)
  (make-gen
   (lambda (seed size)
     (run-gen (f size) seed size))))

;;; gen-resize : Size -> Gen a -> Gen a
;;; Run generator with different size.
(define (gen-resize new-size gen)
  (make-gen
   (lambda (seed size)
     (run-gen gen seed new-size))))

;;; gen-scale : (Size -> Size) -> Gen a -> Gen a
;;; Scale the size parameter.
(define (gen-scale f gen)
  (gen-sized (lambda (size)
    (gen-resize (f size) gen))))

;;; ============================================================
;;; Collection Generators
;;; ============================================================

;;; gen-list : Gen a -> Gen (List a)
;;; Generate a list (length based on size).
(define (gen-list elem-gen)
  (gen-sized (lambda (size)
    (gen-bind (gen-int 0 size)
              (lambda (n)
                (gen-list-n n elem-gen))))))

;;; gen-list-n : Int -> Gen a -> Gen (List a)
;;; Generate list of exactly n elements.
(define (gen-list-n n elem-gen)
  (gen-seq (make-list n elem-gen)))

;;; gen-nonempty-list : Gen a -> Gen (List a)
;;; Generate non-empty list.
(define (gen-nonempty-list elem-gen)
  (gen-bind elem-gen (lambda (x)
    (gen-bind (gen-list elem-gen) (lambda (xs)
      (gen-pure (cons x xs)))))))

;;; gen-string : Gen String
;;; Generate string of printable characters.
(define gen-string
  (gen-map list->string (gen-list gen-char)))

;;; gen-alpha-string : Gen String
;;; Generate alphabetic string.
(define gen-alpha-string
  (gen-map list->string (gen-list gen-alpha)))

;;; gen-pair : Gen a -> Gen b -> Gen (a . b)
;;; Generate a pair.
(define (gen-pair gen-a gen-b)
  (gen-bind gen-a (lambda (a)
    (gen-bind gen-b (lambda (b)
      (gen-pure (cons a b)))))))

;;; gen-triple : Gen a -> Gen b -> Gen c -> Gen (a b c)
;;; Generate a triple (list of 3).
(define (gen-triple gen-a gen-b gen-c)
  (gen-seq (list gen-a gen-b gen-c)))

;;; gen-maybe : Gen a -> Gen (Maybe a)
;;; Generate Just or Nothing.
(define (gen-maybe gen)
  (gen-frequency
   (list (cons 1 (gen-pure nothing))
         (cons 3 (gen-map just gen)))))

;;; gen-either : Gen a -> Gen b -> Gen (Either a b)
;;; Generate Left or Right.
(define (gen-either gen-left gen-right)
  (gen-one-of
   (list (gen-map left gen-left)
         (gen-map right gen-right))))

;;; ============================================================
;;; Shrinking
;;; ============================================================
;;;
;;; Shrinking produces smaller counterexamples.
;;; A shrinker is: a -> List a

;;; shrink-int : Int -> List Int
;;; Shrink an integer towards zero.
(define (shrink-int n)
  (if (= n 0)
      '()
      (let ([half (quotient n 2)])
        (cons 0
              (if (> (abs half) 0)
                  (list half (- n half))
                  '())))))

;;; shrink-list* : (a -> List a) -> List a -> List (List a)
;;; Shrink a list by removing elements or shrinking elements.
(define (shrink-list* shrink-elem lst)
  (append
   ;; Remove one element at a time
   (let loop ([prefix '()] [rest lst])
     (if (null? rest)
         '()
         (cons (append (reverse prefix) (cdr rest))
               (loop (cons (car rest) prefix) (cdr rest)))))
   ;; Shrink individual elements
   (let loop ([prefix '()] [rest lst])
     (if (null? rest)
         '()
         (append
          (map (lambda (shrunk)
                 (append (reverse prefix) (cons shrunk (cdr rest))))
               (shrink-elem (car rest)))
          (loop (cons (car rest) prefix) (cdr rest)))))))

;;; shrink-list : (a -> List a) -> (List a -> List (List a))
;;; Curried version that returns a shrinker function.
(define (shrink-list shrink-elem)
  (lambda (lst) (shrink-list* shrink-elem lst)))

;;; shrink-string : String -> List String
(define (shrink-string s)
  (map list->string (shrink-list* (lambda (c) '()) (string->list s))))

;;; shrink-pair* : (a -> List a) -> (b -> List b) -> (a . b) -> List (a . b)
(define (shrink-pair* shrink-a shrink-b p)
  (append
   (map (lambda (a) (cons a (cdr p))) (shrink-a (car p)))
   (map (lambda (b) (cons (car p) b)) (shrink-b (cdr p)))))

;;; shrink-pair : (a -> List a) -> (b -> List b) -> ((a . b) -> List (a . b))
;;; Curried version that returns a shrinker function.
(define (shrink-pair shrink-a shrink-b)
  (lambda (p) (shrink-pair* shrink-a shrink-b p)))

;;; no-shrink : a -> List a
;;; Never shrink.
(define (no-shrink x) '())

;;; ============================================================
;;; Property Type
;;; ============================================================
;;;
;;; A property is either:
;;;   - (success)
;;;   - (failure counterexample)
;;;   - (discarded) - precondition not met

;;; make-success : -> Result
(define (make-success)
  (list 'success))

;;; make-failure : Any -> Result
(define (make-failure counterexample)
  (list 'failure counterexample))

;;; make-discarded : -> Result
(define (make-discarded)
  (list 'discarded))

;;; success? : Result -> Boolean
(define (success? r)
  (and (pair? r) (eq? (car r) 'success)))

;;; failure? : Result -> Boolean
(define (failure? r)
  (and (pair? r) (eq? (car r) 'failure)))

;;; discarded? : Result -> Boolean
(define (discarded? r)
  (and (pair? r) (eq? (car r) 'discarded)))

;;; failure-counterexample : Result -> Any
(define (failure-counterexample r)
  (cadr r))

;;; ============================================================
;;; Property Constructors
;;; ============================================================

;;; prop-bool : Boolean -> Result
;;; Convert boolean to property result.
(define (prop-bool b)
  (if b (make-success) (make-failure #f)))

;;; forall : Gen a -> (a -> List a) -> (a -> Result) -> Property
;;; Universal quantification with shrinking.
(define (make-forall gen shrink prop)
  (list 'forall gen shrink prop))

;;; forall? : Any -> Boolean
(define (forall? x)
  (and (pair? x) (eq? (car x) 'forall)))

;;; forall-gen : Property -> Gen
(define (forall-gen p) (list-ref p 1))

;;; forall-shrink : Property -> Shrinker
(define (forall-shrink p) (list-ref p 2))

;;; forall-prop : Property -> (a -> Result)
(define (forall-prop p) (list-ref p 3))

;;; prop-forall : Gen a -> (a -> Boolean) -> Property
;;; Simple forall without shrinking.
(define (prop-forall gen pred)
  (make-forall gen no-shrink (lambda (x) (prop-bool (pred x)))))

;;; prop-forall/shrink : Gen a -> (a -> List a) -> (a -> Boolean) -> Property
;;; Forall with custom shrinker.
(define (prop-forall/shrink gen shrink pred)
  (make-forall gen shrink (lambda (x) (prop-bool (pred x)))))

;;; ==> : Boolean -> Result -> Result
;;; Implication: discard if precondition fails.
(define (==> precondition result)
  (if precondition
      result
      (make-discarded)))

;;; ============================================================
;;; Running Properties
;;; ============================================================

;;; check-result : Result -> Boolean
;;; True if result is success or discarded.
(define (check-result result)
  (or (success? result) (discarded? result)))

;;; run-property : Property -> Seed -> Size -> Result
;;; Run a property once.
(define (run-property prop seed size)
  (if (forall? prop)
      (let* ([gen (forall-gen prop)]
             [shrink (forall-shrink prop)]
             [test (forall-prop prop)]
             [result (run-gen gen seed size)]
             [val (car result)]
             [test-result (test val)])
        (if (failure? test-result)
            ;; Try to shrink
            (shrink-find val shrink test)
            test-result))
      (prop-bool prop)))  ; Treat as boolean

;;; shrink-find : a -> (a -> List a) -> (a -> Result) -> Result
;;; Find minimal counterexample.
(define (shrink-find val shrink test)
  (let loop ([current val] [fuel 100])
    (if (<= fuel 0)
        (make-failure current)
        (let ([candidates (shrink current)])
          (let try-next ([cs candidates])
            (if (null? cs)
                (make-failure current)  ; Can't shrink further
                (let ([result (test (car cs))])
                  (if (failure? result)
                      (loop (car cs) (- fuel 1))  ; Found smaller failure
                      (try-next (cdr cs))))))))))

;;; ============================================================
;;; Quick Check
;;; ============================================================

;;; quick-check : Property -> Int -> Seed -> CheckResult
;;; Run property n times.
;;; Returns: (passed count) or (failed counterexample count)
(define (quick-check prop n seed)
  (let loop ([i 0] [s seed] [passed 0] [discarded 0])
    (cond
      [(>= i n)
       (list 'passed passed discarded)]
      [(>= discarded (* 10 n))
       (list 'gave-up discarded)]
      [else
       (let* ([size (+ 1 (modulo i 100))]
              [result (run-property prop s size)]
              [next-seed (cdr (lcg-next s))])
         (cond
           [(success? result)
            (loop (+ i 1) next-seed (+ passed 1) discarded)]
           [(discarded? result)
            (loop i next-seed passed (+ discarded 1))]
           [(failure? result)
            (list 'failed (failure-counterexample result) (+ i 1))]))])))

;;; check-passed? : CheckResult -> Boolean
(define (check-passed? result)
  (eq? (car result) 'passed))

;;; check-failed? : CheckResult -> Boolean
(define (check-failed? result)
  (eq? (car result) 'failed))

;;; check-gave-up? : CheckResult -> Boolean
(define (check-gave-up? result)
  (eq? (car result) 'gave-up))

;;; check-counterexample : CheckResult -> Any
(define (check-counterexample result)
  (if (check-failed? result)
      (cadr result)
      #f))

;;; ============================================================
;;; Convenience: Default Seed
;;; ============================================================

;;; default-seed : Seed
(define default-seed 42)

;;; qc : Property -> CheckResult
;;; Quick check with 100 tests and default seed.
(define (qc prop)
  (quick-check prop 100 default-seed))

;;; qc-verbose : Property -> CheckResult
;;; Quick check with progress output.
(define (qc-verbose prop)
  (let ([result (quick-check prop 100 default-seed)])
    (cond
      [(check-passed? result)
       (display "+++ OK, passed ")
       (display (cadr result))
       (display " tests")
       (if (> (caddr result) 0)
           (begin
             (display " (")
             (display (caddr result))
             (display " discarded)"))
           (void))
       (newline)]
      [(check-failed? result)
       (display "*** Failed after ")
       (display (caddr result))
       (display " tests!\nCounterexample: ")
       (display (cadr result))
       (newline)]
      [(check-gave-up? result)
       (display "*** Gave up after ")
       (display (cadr result))
       (display " discarded tests")
       (newline)])
    result))

;;; ============================================================
;;; Standard Properties
;;; ============================================================

;;; prop-reflexive : Gen a -> (a -> a -> Boolean) -> Property
;;; Test reflexivity: x rel x
(define (prop-reflexive gen rel)
  (prop-forall gen (lambda (x) (rel x x))))

;;; prop-symmetric : Gen a -> (a -> a -> Boolean) -> Property
;;; Test symmetry: x rel y => y rel x
(define (prop-symmetric gen rel)
  (make-forall
   (gen-pair gen gen)
   (shrink-pair no-shrink no-shrink)
   (lambda (p)
     (let ([x (car p)] [y (cdr p)])
       (==> (rel x y) (prop-bool (rel y x)))))))

;;; prop-transitive : Gen a -> (a -> a -> Boolean) -> Property
;;; Test transitivity: x rel y and y rel z => x rel z
(define (prop-transitive gen rel)
  (make-forall
   (gen-triple gen gen gen)
   (lambda (t) '())
   (lambda (t)
     (let ([x (car t)] [y (cadr t)] [z (caddr t)])
       (==> (and (rel x y) (rel y z))
            (prop-bool (rel x z)))))))

;;; prop-commutative : Gen a -> (a -> a -> b) -> (b -> b -> Boolean) -> Property
;;; Test commutativity: f x y = f y x
(define (prop-commutative gen op eq?)
  (make-forall
   (gen-pair gen gen)
   (shrink-pair no-shrink no-shrink)
   (lambda (p)
     (let ([x (car p)] [y (cdr p)])
       (prop-bool (eq? (op x y) (op y x)))))))

;;; prop-associative : Gen a -> (a -> a -> a) -> (a -> a -> Boolean) -> Property
;;; Test associativity: (x op y) op z = x op (y op z)
(define (prop-associative gen op eq?)
  (make-forall
   (gen-triple gen gen gen)
   (lambda (t) '())
   (lambda (t)
     (let ([x (car t)] [y (cadr t)] [z (caddr t)])
       (prop-bool (eq? (op (op x y) z)
                       (op x (op y z))))))))

;;; prop-identity : Gen a -> a -> (a -> a -> a) -> (a -> a -> Boolean) -> Property
;;; Test identity element: id op x = x = x op id
(define (prop-identity gen identity op eq?)
  (prop-forall gen
               (lambda (x)
                 (and (eq? (op identity x) x)
                      (eq? (op x identity) x)))))

;;; prop-inverse : Gen a -> a -> (a -> a) -> (a -> a -> a) -> (a -> a -> Boolean) -> Property
;;; Test inverse: x op inv(x) = identity
(define (prop-inverse gen identity inv op eq?)
  (prop-forall gen
               (lambda (x)
                 (and (eq? (op x (inv x)) identity)
                      (eq? (op (inv x) x) identity)))))

;;; ============================================================
;;; List Properties
;;; ============================================================

;;; prop-reverse-reverse : Property
;;; reverse(reverse(xs)) = xs
(define prop-reverse-reverse
  (prop-forall/shrink
   (gen-list gen-small-int)
   (shrink-list shrink-int)
   (lambda (xs) (equal? (reverse (reverse xs)) xs))))

;;; prop-length-append : Property
;;; length(xs ++ ys) = length(xs) + length(ys)
(define prop-length-append
  (make-forall
   (gen-pair (gen-list gen-nat) (gen-list gen-nat))
   (shrink-pair (shrink-list no-shrink) (shrink-list no-shrink))
   (lambda (p)
     (let ([xs (car p)] [ys (cdr p)])
       (prop-bool (= (length (append xs ys))
                     (+ (length xs) (length ys))))))))

;;; prop-map-length : Property
;;; length(map f xs) = length(xs)
(define prop-map-length
  (prop-forall/shrink
   (gen-list gen-nat)
   (shrink-list shrink-int)
   (lambda (xs)
     (= (length (map (lambda (x) (* x 2)) xs))
        (length xs)))))

