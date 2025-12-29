;;; fabric/stitches/fp/quickcheck.ss --- QuickCheck-Style Property-Based Testing
;;;
;;; A property-based testing library inspired by Haskell's QuickCheck.
;;; Generates random test cases and shrinks counterexamples.
;;;
;;; This is Core code: pure (except for random seed threading), total where possible.
;;;
;;; Features:
;;;   - Generators (Gen monad): gen-int, gen-bool, gen-list, gen-char, gen-string
;;;   - Generator combinators: gen-one-of, gen-frequency, gen-such-that, gen-map, gen-bind
;;;   - forall macro for defining properties
;;;   - check function that runs N tests with random inputs
;;;   - Shrinking for minimal counterexamples
;;;   - Arbitrary type class pattern
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;
;;; Example Usage:
;;;   (check 100 (forall (xs gen-list-int)
;;;                (equal? (reverse (reverse xs)) xs)))

(load "fabric/stitches/prelude.ss")

;;; ============================================================
;;; Random Number Generation (Pure LCG)
;;; ============================================================
;;;
;;; Linear Congruential Generator with parameters from glibc.
;;; Pure: takes seed, returns (value . new-seed).

(define *lcg-a* 1103515245)
(define *lcg-c* 12345)
(define *lcg-m* 2147483648)  ; 2^31

;;; rng-next : Seed -> (Int . Seed)
;;; Generate next random integer and new seed.
(define (rng-next seed)
  (let ([next (modulo (+ (* *lcg-a* seed) *lcg-c*) *lcg-m*)])
       (cons next next)))

;;; rng-split : Seed -> (Seed . Seed)
;;; Split a seed into two independent seeds.
;;; Useful for parallel generation.
(define (rng-split seed)
  (let* ([r1 (rng-next seed)]
         [r2 (rng-next (cdr r1))])
        (cons (car r1) (car r2))))

;;; rng-int-range : Seed -> Int -> Int -> (Int . Seed)
;;; Generate random integer in [lo, hi].
(define (rng-int-range seed lo hi)
  (let* ([r (rng-next seed)]
         [val (car r)]
         [new-seed (cdr r)]
         [range (+ 1 (- hi lo))]
         [scaled (+ lo (modulo val range))])
        (cons scaled new-seed)))

;;; rng-bool : Seed -> (Bool . Seed)
(define (rng-bool seed)
  (let ([r (rng-int-range seed 0 1)])
       (cons (= (car r) 1) (cdr r))))

;;; rng-double : Seed -> (Double . Seed)
;;; Generate random double in [0, 1).
(define (rng-double seed)
  (let ([r (rng-next seed)])
       (cons (/ (car r) *lcg-m*) (cdr r))))

;;; ============================================================
;;; Generator Type (Gen Monad)
;;; ============================================================
;;;
;;; Gen a = (Size, Seed) -> (a, Seed)
;;; Size controls complexity; grows with test count.

;;; make-gen : ((Size, Seed) -> (a, Seed)) -> Gen a
(define (make-gen run-fn)
  (list 'gen run-fn))

;;; gen? : Any -> Bool
(define (gen? x)
  (and (pair? x) (eq? (car x) 'gen)))

;;; run-gen : Gen a -> Size -> Seed -> (a . Seed)
(define (run-gen gen size seed)
  ((cadr gen) size seed))

;;; gen-sample : Gen a -> Size -> Seed -> a
;;; Run generator and return just the value.
(define (gen-sample gen size seed)
  (car (run-gen gen size seed)))

;;; ============================================================
;;; Gen Monad Operations
;;; ============================================================

;;; gen-pure : a -> Gen a
;;; Generator that always produces the same value (return/pure).
(define (gen-pure x)
  (make-gen (lambda (size seed) (cons x seed))))

;;; gen-bind : Gen a -> (a -> Gen b) -> Gen b
;;; Monadic bind (>>=).
(define (gen-bind gen f)
  (make-gen
   (lambda (size seed)
           (let* ([result (run-gen gen size seed)]
                  [val (car result)]
                  [new-seed (cdr result)])
                 (run-gen (f val) size new-seed)))))

;;; gen-map : (a -> b) -> Gen a -> Gen b
;;; Functor map (fmap).
(define (gen-map f gen)
  (gen-bind gen (lambda (x) (gen-pure (f x)))))

;;; gen-ap : Gen (a -> b) -> Gen a -> Gen b
;;; Applicative apply (<*>).
(define (gen-ap gf ga)
  (gen-bind gf (lambda (f)
                       (gen-bind ga (lambda (a)
                                            (gen-pure (f a)))))))

;;; gen-sequence : (List (Gen a)) -> Gen (List a)
;;; Sequence list of generators into generator of list.
(define (gen-sequence gens)
  (if (null? gens)
      (gen-pure '())
      (gen-bind (car gens) (lambda (x)
                                   (gen-bind (gen-sequence (cdr gens)) (lambda (xs)
                                                                               (gen-pure (cons x xs))))))))

;;; gen-traverse : (a -> Gen b) -> (List a) -> Gen (List b)
;;; Map each element through a generator and sequence.
(define (gen-traverse f xs)
  (gen-sequence (map f xs)))

;;; ============================================================
;;; Basic Generators
;;; ============================================================

;;; gen-int : Gen Int
;;; Generate integer in range [-size, size].
(define gen-int
  (make-gen
   (lambda (size seed)
           (rng-int-range seed (- size) size))))

;;; gen-nat : Gen Nat
;;; Generate natural number in [0, size].
(define gen-nat
  (make-gen
   (lambda (size seed)
           (rng-int-range seed 0 size))))

;;; gen-pos : Gen Pos
;;; Generate positive integer in [1, size+1].
(define gen-pos
  (make-gen
   (lambda (size seed)
           (rng-int-range seed 1 (+ size 1)))))

;;; gen-int-range : Int -> Int -> Gen Int
;;; Generate integer in fixed range [lo, hi].
(define (gen-int-range lo hi)
  (make-gen
   (lambda (size seed)
           (rng-int-range seed lo hi))))

;;; gen-bool : Gen Bool
(define gen-bool
  (make-gen
   (lambda (size seed)
           (rng-bool seed))))

;;; gen-char : Gen Char
;;; Generate printable ASCII character (32-126).
(define gen-char
  (gen-map integer->char (gen-int-range 32 126)))

;;; gen-char-alpha : Gen Char
;;; Generate alphabetic character.
(define gen-char-alpha
  (gen-bind gen-bool (lambda (upper?)
                             (if upper?
                                 (gen-map integer->char (gen-int-range 65 90))   ; A-Z
                                 (gen-map integer->char (gen-int-range 97 122)))))) ; a-z

;;; gen-char-digit : Gen Char
;;; Generate digit character.
(define gen-char-digit
  (gen-map integer->char (gen-int-range 48 57)))  ; 0-9

;;; ============================================================
;;; Generator Combinators
;;; ============================================================

;;; gen-one-of : (List (Gen a)) -> Gen a
;;; Choose uniformly from a list of generators.
(define (gen-one-of gens)
  (if (null? gens)
      (error 'gen-one-of "empty list of generators")
      (gen-bind (gen-int-range 0 (- (length gens) 1))
                (lambda (i) (list-ref gens i)))))

;;; gen-elements : (List a) -> Gen a
;;; Choose uniformly from a list of values.
(define (gen-elements xs)
  (if (null? xs)
      (error 'gen-elements "empty list")
      (gen-map (lambda (i) (list-ref xs i))
               (gen-int-range 0 (- (length xs) 1)))))

;;; gen-frequency : (List (Int . Gen a)) -> Gen a
;;; Choose generator with weighted probability.
;;; Input: ((weight1 . gen1) (weight2 . gen2) ...)
(define (gen-frequency weighted-gens)
  (if (null? weighted-gens)
      (error 'gen-frequency "empty list")
      (let ([total (fold-left (lambda (acc wg) (+ acc (car wg))) 0 weighted-gens)])
           (gen-bind (gen-int-range 1 total)
                     (lambda (n)
                             (let loop ([wgs weighted-gens] [remaining n])
                                  (let ([weight (caar wgs)]
                                        [gen (cdar wgs)])
                                       (if (<= remaining weight)
                                           gen
                                           (loop (cdr wgs) (- remaining weight))))))))))

;;; gen-such-that : (a -> Bool) -> Gen a -> Gen a
;;; Filter generator output. WARNING: May loop forever if predicate rarely holds.
;;; Uses fuel to ensure totality.
(define (gen-such-that pred gen)
  (gen-such-that* pred gen 100))

;;; gen-such-that* : (a -> Bool) -> Gen a -> Int -> Gen a
;;; gen-such-that with explicit fuel.
(define (gen-such-that* pred gen fuel)
  (make-gen
   (lambda (size seed)
           (let loop ([s seed] [remaining fuel])
                (if (<= remaining 0)
                    ;; Give up: return whatever we got last
                    (run-gen gen size s)
                    (let* ([result (run-gen gen size s)]
                           [val (car result)]
                           [new-seed (cdr result)])
                          (if (pred val)
                              result
                              (loop new-seed (- remaining 1)))))))))

;;; gen-sized : (Size -> Gen a) -> Gen a
;;; Access the size parameter.
(define (gen-sized f)
  (make-gen
   (lambda (size seed)
           (run-gen (f size) size seed))))

;;; gen-resize : Size -> Gen a -> Gen a
;;; Run generator with specific size.
(define (gen-resize new-size gen)
  (make-gen
   (lambda (size seed)
           (run-gen gen new-size seed))))

;;; gen-scale : (Size -> Size) -> Gen a -> Gen a
;;; Scale the size parameter.
(define (gen-scale f gen)
  (gen-sized (lambda (size)
                     (gen-resize (f size) gen))))

;;; gen-no-shrink : Gen a -> Gen a
;;; Mark generator as non-shrinkable (for combinators that need it).
(define (gen-no-shrink gen)
  gen)  ; Shrinking is handled separately

;;; gen-char-alnum : Gen Char
;;; Generate alphanumeric character.
(define gen-char-alnum
  (gen-one-of (list gen-char-alpha gen-char-digit)))

;;; ============================================================
;;; Collection Generators
;;; ============================================================

;;; gen-list : Gen a -> Gen (List a)
;;; Generate list with length [0, size].
(define (gen-list elem-gen)
  (gen-sized (lambda (size)
                     (gen-bind (gen-int-range 0 size)
                               (lambda (n)
                                       (gen-list-n n elem-gen))))))

;;; gen-list-n : Int -> Gen a -> Gen (List a)
;;; Generate list of exactly n elements.
(define (gen-list-n n elem-gen)
  (if (<= n 0)
      (gen-pure '())
      (gen-bind elem-gen (lambda (x)
                                 (gen-bind (gen-list-n (- n 1) elem-gen) (lambda (xs)
                                                                                 (gen-pure (cons x xs))))))))

;;; gen-list-range : Int -> Int -> Gen a -> Gen (List a)
;;; Generate list with length in [lo, hi].
(define (gen-list-range lo hi elem-gen)
  (gen-bind (gen-int-range lo hi)
            (lambda (n) (gen-list-n n elem-gen))))

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

;;; gen-string-alpha : Gen String
;;; Generate alphabetic string.
(define gen-string-alpha
  (gen-map list->string (gen-list gen-char-alpha)))

;;; gen-string-n : Int -> Gen String
;;; Generate string of exactly n characters.
(define (gen-string-n n)
  (gen-map list->string (gen-list-n n gen-char)))

;;; gen-pair : Gen a -> Gen b -> Gen (a . b)
;;; Generate a pair (cons cell).
(define (gen-pair gen-a gen-b)
  (gen-bind gen-a (lambda (a)
                          (gen-bind gen-b (lambda (b)
                                                  (gen-pure (cons a b)))))))

;;; gen-tuple : Gen a -> Gen b -> Gen (a b)
;;; Generate a 2-element list.
(define (gen-tuple gen-a gen-b)
  (gen-bind gen-a (lambda (a)
                          (gen-bind gen-b (lambda (b)
                                                  (gen-pure (list a b)))))))

;;; gen-triple : Gen a -> Gen b -> Gen c -> Gen (a b c)
;;; Generate a 3-element list.
(define (gen-triple gen-a gen-b gen-c)
  (gen-sequence (list gen-a gen-b gen-c)))

;;; gen-vector : Int -> Gen a -> Gen (Vector a)
;;; Generate a vector of n elements.
(define (gen-vector n elem-gen)
  (gen-map list->vector (gen-list-n n elem-gen)))

;;; ============================================================
;;; Option/Maybe Generator
;;; ============================================================

;;; gen-maybe : Gen a -> Gen (Maybe a)
;;; Generate nothing (#f) or (just value).
(define (gen-maybe gen)
  (gen-frequency
   (list (cons 1 (gen-pure #f))  ; nothing
         (cons 3 (gen-map (lambda (x) (list 'just x)) gen)))))

;;; gen-option : Gen a -> Gen (Option a)
;;; Alias for gen-maybe using (some x) / 'none convention.
(define (gen-option gen)
  (gen-frequency
   (list (cons 1 (gen-pure 'none))
         (cons 3 (gen-map (lambda (x) (list 'some x)) gen)))))

;;; ============================================================
;;; Convenience Generators (Arbitrary pattern)
;;; ============================================================

;;; gen-list-int : Gen (List Int)
;;; Convenient generator for lists of integers.
(define gen-list-int
  (gen-list gen-int))

;;; gen-list-nat : Gen (List Nat)
;;; Convenient generator for lists of natural numbers.
(define gen-list-nat
  (gen-list gen-nat))

;;; gen-list-bool : Gen (List Bool)
;;; Convenient generator for lists of booleans.
(define gen-list-bool
  (gen-list gen-bool))

;;; ============================================================
;;; Shrinking
;;; ============================================================
;;;
;;; A Shrinker produces smaller values from a given value.
;;; Shrinker a = a -> (List a)
;;;
;;; Shrinking finds minimal counterexamples.

;;; shrink-int : Int -> (List Int)
;;; Shrink integer toward 0.
(define (shrink-int n)
  (cond
   [(= n 0) '()]
   [(< n 0)
    ;; For negative: try 0, half-way, and negate
    (filter (lambda (x) (and (< x 0) (> x n)))
            (list 0 (quotient n 2) (- n 1)))]
   [else
    ;; For positive: try 0, half-way, and decrement
    (filter (lambda (x) (and (>= x 0) (< x n)))
            (list 0 (quotient n 2) (- n 1)))]))

;;; shrink-nat : Nat -> (List Nat)
;;; Shrink natural number toward 0.
(define (shrink-nat n)
  (filter (lambda (x) (>= x 0)) (shrink-int n)))

;;; shrink-bool : Bool -> (List Bool)
;;; Shrink boolean toward #f.
(define (shrink-bool b)
  (if b '(#f) '()))

;;; shrink-char : Char -> (List Char)
;;; Shrink char toward 'a'.
(define (shrink-char c)
  (let ([n (char->integer c)])
       (if (= n 97)  ; 'a'
           '()
           (list #))))

;;; shrink-list : (a -> (List a)) -> (List a) -> (List (List a))
;;; Shrink a list by:
;;; 1. Removing elements
;;; 2. Shrinking individual elements
;;; Curried: (shrink-list shrink-elem) returns a shrinker for lists.
(define (shrink-list shrink-elem)
  (lambda (lst)
          (append
           ;; Remove elements (try smaller sublists)
           (shrink-list-removes lst)
           ;; Shrink individual elements
           (shrink-list-elements shrink-elem lst))))

;;; shrink-list-removes : (List a) -> (List (List a))
;;; All ways to remove one element from a list.
(define (shrink-list-removes lst)
  (if (null? lst)
      '()
      ;; Remove each element
      (let loop ([prefix '()] [rest lst])
           (if (null? rest)
               '()
               (cons (append (reverse prefix) (cdr rest))
                     (loop (cons (car rest) prefix) (cdr rest)))))))

;;; shrink-list-elements : (a -> (List a)) -> (List a) -> (List (List a))
;;; Shrink each element of a list.
(define (shrink-list-elements shrink-elem lst)
  (let loop ([prefix '()] [rest lst])
       (if (null? rest)
           '()
           (append
            (map (lambda (shrunk)
                         (append (reverse prefix) (cons shrunk (cdr rest))))
                 (shrink-elem (car rest)))
            (loop (cons (car rest) prefix) (cdr rest))))))

;;; shrink-string : String -> (List String)
;;; Shrink string by removing characters.
(define (shrink-string s)
  (map list->string ((shrink-list shrink-char) (string->list s))))

;;; shrink-pair : (a -> (List a)) -> (b -> (List b)) -> ((a . b) -> (List (a . b)))
;;; Shrink a pair by shrinking each component.
;;; Curried: (shrink-pair shrink-a shrink-b) returns a shrinker for pairs.
(define (shrink-pair shrink-a shrink-b)
  (lambda (p)
          (append
           (map (lambda (a) (cons a (cdr p))) (shrink-a (car p)))
           (map (lambda (b) (cons (car p) b)) (shrink-b (cdr p))))))

;;; shrink-tuple : (a -> (List a)) -> (b -> (List b)) -> ((a b) -> (List (a b)))
;;; Shrink a 2-tuple by shrinking each component.
;;; Curried: (shrink-tuple shrink-a shrink-b) returns a shrinker for tuples.
(define (shrink-tuple shrink-a shrink-b)
  (lambda (t)
          (append
           (map (lambda (a) (list a (cadr t))) (shrink-a (car t)))
           (map (lambda (b) (list (car t) b)) (shrink-b (cadr t))))))

;;; no-shrink : a -> (List a)
;;; Never shrink (use when shrinking is not needed).
(define (no-shrink x) '())

;;; ============================================================
;;; Test Result Type
;;; ============================================================

;;; Results:
;;;   (passed n-tests)
;;;   (failed counterexample n-tests)
;;;   (gave-up n-discarded)

(define (make-passed n)
  (list 'passed n))

(define (make-failed counterexample n)
  (list 'failed counterexample n))

(define (make-gave-up n)
  (list 'gave-up n))

(define (passed? r)
  (and (pair? r) (eq? (car r) 'passed)))

(define (failed? r)
  (and (pair? r) (eq? (car r) 'failed)))

(define (gave-up? r)
  (and (pair? r) (eq? (car r) 'gave-up)))

(define (result-counterexample r)
  (and (failed? r) (cadr r)))

(define (result-num-tests r)
  (cadr r))

;;; ============================================================
;;; Property Type
;;; ============================================================
;;;
;;; A Property is: (property gen shrink test-fn)
;;; where:
;;;   gen : Gen a
;;;   shrink : a -> (List a)
;;;   test-fn : a -> Bool

(define (make-property gen shrink test-fn)
  (list 'property gen shrink test-fn))

(define (property? x)
  (and (pair? x) (eq? (car x) 'property)))

(define (property-gen p) (list-ref p 1))
(define (property-shrink p) (list-ref p 2))
(define (property-test p) (list-ref p 3))

;;; ============================================================
;;; forall Macro
;;; ============================================================

;;; forall : Property constructor macro
;;; Usage: (forall (x gen-int) (>= (* x x) 0))
;;; Usage: (forall ((x gen-int) (y gen-int)) (= (+ x y) (+ y x)))
;;; Usage: (forall (xs gen-list-int #:shrink (shrink-list shrink-int)) ...)
(define-syntax forall
  (syntax-rules ()
                ;; Three bindings (most specific first)
                [(_ ((var1 gen1) (var2 gen2) (var3 gen3)) body ...)
                 (make-property
                  (gen-triple gen1 gen2 gen3)
                  (lambda (t) '())
                  (lambda (t)
                          (let ([var1 (car t)]
                                [var2 (cadr t)]
                                [var3 (caddr t)])
                               body ...)))]
                ;; Two bindings - generate tuple
                [(_ ((var1 gen1) (var2 gen2)) body ...)
                 (make-property
                  (gen-tuple gen1 gen2)
                  (shrink-tuple no-shrink no-shrink)
                  (lambda (t)
                          (let ([var1 (car t)]
                                [var2 (cadr t)])
                               body ...)))]
                ;; Single binding without shrink
                [(_ (var gen) body ...)
                 (make-property gen no-shrink (lambda (var) body ...))]))

;;; forall/shrink : Property constructor with explicit shrinker
;;; Usage: (forall/shrink (x gen-int shrink-int) body ...)
(define-syntax forall/shrink
  (syntax-rules ()
                [(_ (var gen shrink) body ...)
                 (make-property gen shrink (lambda (var) body ...))]
                [(_ ((var1 gen1 shrink1) (var2 gen2 shrink2)) body ...)
                 (make-property
                  (gen-tuple gen1 gen2)
                  (shrink-tuple shrink1 shrink2)
                  (lambda (t)
                          (let ([var1 (car t)]
                                [var2 (cadr t)])
                               body ...)))]))

;;; ============================================================
;;; Implication (Conditional Properties)
;;; ============================================================

;;; ==> : Bool -> Bool -> Bool
;;; Logical implication. If precondition is false, returns 'discard.
(define (==> precondition conclusion)
  (if precondition
      conclusion
      'discard))

;;; ============================================================
;;; Shrinking Algorithm
;;; ============================================================

;;; shrink-find : a -> (a -> (List a)) -> (a -> Bool) -> Int -> a
;;; Find minimal counterexample by iteratively shrinking.
;;; Returns the smallest value that still fails the test.
(define (shrink-find val shrink test-fn fuel)
  (let loop ([current val] [remaining fuel])
       (if (<= remaining 0)
           current
           (let ([candidates (shrink current)])
                (let try-next ([cs candidates])
                     (if (null? cs)
                         current  ; Can't shrink further
                         (let ([result (test-fn (car cs))])
                              (if (or (eq? result #f) (eq? result 'fail))
                                  ;; Found smaller failing case, recurse
                                  (loop (car cs) (- remaining 1))
                                  ;; This candidate passes, try next
                                  (try-next (cdr cs))))))))))

;;; ============================================================
;;; Check Function
;;; ============================================================

;;; check : Int -> Property -> Result
;;; Run property n times with random inputs.
(define (check n prop)
  (check-with-seed n prop 42))

;;; check-with-seed : Int -> Property -> Seed -> Result
;;; Run property n times with specific seed.
(define (check-with-seed n prop seed)
  (let ([gen (property-gen prop)]
        [shrink (property-shrink prop)]
        [test-fn (property-test prop)])
       (let loop ([i 0] [s seed] [discarded 0])
            (cond
             ;; All tests passed
             [(>= i n)
              (make-passed n)]
             ;; Too many discards
             [(>= discarded (* 10 n))
              (make-gave-up discarded)]
             [else
              (let* ([size (+ 1 (modulo i 100))]  ; Size grows with test count
                     [result (run-gen gen size s)]
                     [val (car result)]
                     [new-seed (cdr result)]
                     [test-result (test-fn val)])
                    (cond
                     ;; Property holds
                     [(eq? test-result #t)
                      (loop (+ i 1) new-seed discarded)]
                     ;; Precondition not met, discard
                     [(eq? test-result 'discard)
                      (loop i new-seed (+ discarded 1))]
                     ;; Property fails - shrink and report
                     [else
                      (let ([minimal (shrink-find val shrink test-fn 100)])
                           (make-failed minimal (+ i 1)))]))]))))

;;; quickcheck : Property -> Result
;;; Run 100 tests with default seed.
(define (quickcheck prop)
  (check 100 prop))

;;; qc : Property -> Result
;;; Alias for quickcheck.
(define qc quickcheck)

;;; ============================================================
;;; Verbose Checking
;;; ============================================================

;;; check-verbose : Int -> Property -> Result
;;; Run check and print result.
(define (check-verbose n prop)
  (let ([result (check n prop)])
       (cond
        [(passed? result)
         (display "+++ OK, passed ")
         (display (result-num-tests result))
         (display " tests.
")]
        [(failed? result)
         (display "*** Failed! Counterexample: ")
         (write (result-counterexample result))
         (display " (after ")
         (display (result-num-tests result))
         (display " tests)
")]
        [(gave-up? result)
         (display "*** Gave up after ")
         (display (result-num-tests result))
         (display " discarded tests.
")])
       result))

;;; ============================================================
;;; Sampling (for debugging generators)
;;; ============================================================

;;; sample : Gen a -> (List a)
;;; Generate 10 sample values with increasing sizes.
(define (sample gen)
  (sample-n gen 10))

;;; sample-n : Gen a -> Int -> (List a)
;;; Generate n sample values.
(define (sample-n gen n)
  (let loop ([i 0] [seed 42] [acc '()])
       (if (>= i n)
           (reverse acc)
           (let* ([size (+ 1 i)]
                  [result (run-gen gen size seed)]
                  [val (car result)]
                  [new-seed (cdr result)])
                 (loop (+ i 1) new-seed (cons val acc))))))

;;; sample-print : Gen a -> Unit
;;; Generate and print 10 samples.
(define (sample-print gen)
  (for-each (lambda (x)
                    (write x)
                    (newline))
            (sample gen)))

;;; ============================================================
;;; Standard Properties (Reusable Patterns)
;;; ============================================================

;;; prop-reflexive : Gen a -> (a -> a -> Bool) -> Property
;;; Test reflexivity: forall x. x R x
(define (prop-reflexive gen rel)
  (forall (x gen) (rel x x)))

;;; prop-symmetric : Gen a -> (a -> a -> Bool) -> Property
;;; Test symmetry: forall x y. x R y => y R x
(define (prop-symmetric gen rel)
  (make-property
   (gen-tuple gen gen)
   (shrink-tuple no-shrink no-shrink)
   (lambda (t)
           (let ([x (car t)] [y (cadr t)])
                (==> (rel x y) (rel y x))))))

;;; prop-transitive : Gen a -> (a -> a -> Bool) -> Property
;;; Test transitivity: forall x y z. x R y && y R z => x R z
(define (prop-transitive gen rel)
  (make-property
   (gen-triple gen gen gen)
   (lambda (t) '())
   (lambda (t)
           (let ([x (car t)] [y (cadr t)] [z (caddr t)])
                (==> (and (rel x y) (rel y z)) (rel x z))))))

;;; prop-commutative : Gen a -> (a -> a -> b) -> Property
;;; Test commutativity: forall x y. f x y = f y x
(define (prop-commutative gen op)
  (make-property
   (gen-tuple gen gen)
   (shrink-tuple no-shrink no-shrink)
   (lambda (t)
           (let ([x (car t)] [y (cadr t)])
                (equal? (op x y) (op y x))))))

;;; prop-associative : Gen a -> (a -> a -> a) -> Property
;;; Test associativity: forall x y z. (x op y) op z = x op (y op z)
(define (prop-associative gen op)
  (make-property
   (gen-triple gen gen gen)
   (lambda (t) '())
   (lambda (t)
           (let ([x (car t)] [y (cadr t)] [z (caddr t)])
                (equal? (op (op x y) z) (op x (op y z)))))))

;;; prop-identity-left : Gen a -> a -> (a -> a -> a) -> Property
;;; Test left identity: forall x. e op x = x
(define (prop-identity-left gen identity op)
  (forall (x gen) (equal? (op identity x) x)))

;;; prop-identity-right : Gen a -> a -> (a -> a -> a) -> Property
;;; Test right identity: forall x. x op e = x
(define (prop-identity-right gen identity op)
  (forall (x gen) (equal? (op x identity) x)))

;;; prop-identity : Gen a -> a -> (a -> a -> a) -> Property
;;; Test two-sided identity: forall x. e op x = x = x op e
(define (prop-identity gen identity op)
  (forall (x gen)
          (and (equal? (op identity x) x)
               (equal? (op x identity) x))))

;;; prop-inverse : Gen a -> a -> (a -> a) -> (a -> a -> a) -> Property
;;; Test inverse: forall x. x op inv(x) = e = inv(x) op x
(define (prop-inverse gen identity inv op)
  (forall (x gen)
          (and (equal? (op x (inv x)) identity)
               (equal? (op (inv x) x) identity))))

;;; ============================================================
;;; List Properties
;;; ============================================================

;;; prop-reverse-reverse : Property
;;; reverse(reverse(xs)) = xs
(define prop-reverse-reverse
  (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                 (equal? (reverse (reverse xs)) xs)))

;;; prop-length-append : Property
;;; length(xs ++ ys) = length(xs) + length(ys)
(define prop-length-append
  (forall/shrink ((xs gen-list-nat (shrink-list shrink-nat))
                  (ys gen-list-nat (shrink-list shrink-nat)))
                 (= (length (append xs ys))
                    (+ (length xs) (length ys)))))

;;; prop-map-length : Property
;;; length(map f xs) = length(xs)
(define prop-map-length
  (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                 (= (length (map (lambda (x) (* x 2)) xs))
                    (length xs))))

;;; prop-filter-length : Property
;;; length(filter p xs) <= length(xs)
(define prop-filter-length
  (forall/shrink (xs gen-list-int (shrink-list shrink-int))
                 (<= (length (filter even? xs))
                     (length xs))))

;;; prop-append-assoc : Property
;;; (xs ++ ys) ++ zs = xs ++ (ys ++ zs)
(define prop-append-assoc
  (forall ((xs gen-list-nat) (ys gen-list-nat) (zs gen-list-nat))
          (equal? (append (append xs ys) zs)
                  (append xs (append ys zs)))))
