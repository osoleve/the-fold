;;; fabric/stitches/fp/stream.ss — Lazy Streams and Generators
;;;
;;; Lazy streams enable infinite sequences and demand-driven evaluation.
;;; This is useful for processing large or infinite data sets efficiently.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Lazy stream type (thunks for delayed computation)
;;;   - Stream constructors (cons, iterate, repeat, cycle)
;;;   - Stream transformers (map, filter, take, drop)
;;;   - Stream combinators (zip, interleave, merge)
;;;   - Unfold and generators
;;;   - Memoized streams
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "core/base/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Stream Type
;;; ============================================================
;;;
;;; A stream is either:
;;;   - stream-nil: the empty stream
;;;   - (stream-cons head thunk): head value with lazy tail
;;;
;;; The thunk is a nullary procedure that, when called, produces
;;; the rest of the stream.

;;; stream-nil : Stream a
(define stream-nil '(stream-nil))

;;; stream-nil? : Stream a -> Boolean
(define (stream-nil? s)
  (and (pair? s) (eq? (car s) 'stream-nil)))

;;; stream-cons : a -> (() -> Stream a) -> Stream a
;;; Construct a stream with a head and lazy tail.
(define (stream-cons head tail-thunk)
  (list 'stream-cons head tail-thunk))

;;; stream-cons? : Stream a -> Boolean
(define (stream-cons? s)
  (and (pair? s)
       (or (eq? (car s) 'stream-cons)
           (eq? (car s) 'memo-stream-cons))))

;;; stream-head : Stream a -> a
;;; Get the first element (partial on empty stream).
(define (stream-head s)
  (if (stream-cons? s)
      (list-ref s 1)
      (error 'stream-head "empty stream")))

;;; stream-tail : Stream a -> Stream a
;;; Force and return the tail.
(define (stream-tail s)
  (if (stream-cons? s)
      ((list-ref s 2))
      (error 'stream-tail "empty stream")))

;;; ============================================================
;;; Stream Constructors
;;; ============================================================

;;; list->stream : List a -> Stream a
;;; Convert a list to a stream.
(define (list->stream lst)
  (if (null? lst)
      stream-nil
      (stream-cons (car lst) (lambda () (list->stream (cdr lst))))))

;;; stream->list : Int -> Stream a -> List a
;;; Take n elements from stream and convert to list.
(define (stream->list n s)
  (if (or (<= n 0) (stream-nil? s))
      '()
      (cons (stream-head s)
            (stream->list (- n 1) (stream-tail s)))))

;;; stream-iterate : (a -> a) -> a -> Stream a
;;; Generate infinite stream: x, f(x), f(f(x)), ...
(define (stream-iterate f x)
  (stream-cons x (lambda () (stream-iterate f (f x)))))

;;; stream-repeat : a -> Stream a
;;; Infinite stream of a single repeated value.
(define (stream-repeat x)
  (stream-cons x (lambda () (stream-repeat x))))

;;; stream-cycle : List a -> Stream a
;;; Infinite stream cycling through a list.
(define (stream-cycle lst)
  (if (null? lst)
      stream-nil
      (stream-cycle-helper lst lst)))

(define (stream-cycle-helper original current)
  (if (null? current)
      (stream-cycle-helper original original)
      (stream-cons (car current)
                   (lambda () (stream-cycle-helper original (cdr current))))))

;;; stream-from : Int -> Stream Int
;;; Infinite stream of integers starting from n.
(define (stream-from n)
  (stream-iterate (lambda (x) (+ x 1)) n))

;;; stream-range : Int -> Int -> Stream Int
;;; Stream of integers from start (inclusive) to end (exclusive).
(define (stream-range start end)
  (if (>= start end)
      stream-nil
      (stream-cons start (lambda () (stream-range (+ start 1) end)))))

;;; naturals : Stream Int
;;; The natural numbers: 0, 1, 2, 3, ...
(define naturals (stream-from 0))

;;; stream-unfold : (s -> Maybe (a, s)) -> s -> Stream a
;;; Build a stream from a seed using a step function.
;;; Step function returns nothing to end, or (just (value . new-seed)) to continue.
(define (stream-unfold step seed)
  (let ([result (step seed)])
       (if (nothing? result)
           stream-nil
           (let ([pair (from-just result)])
                (stream-cons (car pair)
                             (lambda () (stream-unfold step (cdr pair))))))))

;;; ============================================================
;;; Stream Transformers
;;; ============================================================

;;; stream-map : (a -> b) -> Stream a -> Stream b
;;; Map a function over a stream.
(define (stream-map f s)
  (if (stream-nil? s)
      stream-nil
      (stream-cons (f (stream-head s))
                   (lambda () (stream-map f (stream-tail s))))))

;;; stream-filter : (a -> Boolean) -> Stream a -> Stream a
;;; Filter a stream by a predicate.
(define (stream-filter pred s)
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s))
    (stream-cons (stream-head s)
                 (lambda () (stream-filter pred (stream-tail s))))]
   [else (stream-filter pred (stream-tail s))]))

;;; stream-take : Int -> Stream a -> Stream a
;;; Take the first n elements.
(define (stream-take n s)
  (if (or (<= n 0) (stream-nil? s))
      stream-nil
      (stream-cons (stream-head s)
                   (lambda () (stream-take (- n 1) (stream-tail s))))))

;;; stream-drop : Int -> Stream a -> Stream a
;;; Drop the first n elements.
(define (stream-drop n s)
  (if (or (<= n 0) (stream-nil? s))
      s
      (stream-drop (- n 1) (stream-tail s))))

;;; stream-take-while : (a -> Boolean) -> Stream a -> Stream a
;;; Take elements while predicate holds.
(define (stream-take-while pred s)
  (if (stream-nil? s)
      stream-nil
      (let ([h (stream-head s)])
           (if (pred h)
               (stream-cons h (lambda () (stream-take-while pred (stream-tail s))))
               stream-nil))))

;;; stream-drop-while : (a -> Boolean) -> Stream a -> Stream a
;;; Drop elements while predicate holds.
(define (stream-drop-while pred s)
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s)) (stream-drop-while pred (stream-tail s))]
   [else s]))

;;; stream-nth : Int -> Stream a -> a
;;; Get the nth element (0-indexed).
(define (stream-nth n s)
  (stream-head (stream-drop n s)))

;;; stream-scan : (b -> a -> b) -> b -> Stream a -> Stream b
;;; Running fold over a stream.
(define (stream-scan f init s)
  (stream-cons init
               (lambda ()
                       (if (stream-nil? s)
                           stream-nil
                           (stream-scan f (f init (stream-head s)) (stream-tail s))))))

;;; stream-concat : Stream a -> Stream a -> Stream a
;;; Concatenate two streams (second stream is only accessed when first ends).
(define (stream-concat s1 s2)
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-concat (stream-tail s1) s2)))))

;;; stream-flatten : Stream (Stream a) -> Stream a
;;; Flatten a stream of streams.
(define (stream-flatten ss)
  (if (stream-nil? ss)
      stream-nil
      (let ([head (stream-head ss)])
           (if (stream-nil? head)
               (stream-flatten (stream-tail ss))
               (stream-cons (stream-head head)
                            (lambda ()
                                    (stream-flatten
                                     (stream-cons (stream-tail head)
                                                  (lambda () (stream-tail ss))))))))))

;;; stream-flatmap : (a -> Stream b) -> Stream a -> Stream b
;;; Map and flatten.
(define (stream-flatmap f s)
  (stream-flatten (stream-map f s)))

;;; ============================================================
;;; Stream Combinators
;;; ============================================================

;;; stream-zip : Stream a -> Stream b -> Stream (a . b)
;;; Zip two streams together.
(define (stream-zip s1 s2)
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (cons (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip (stream-tail s1) (stream-tail s2))))))

;;; stream-zip-with : (a -> b -> c) -> Stream a -> Stream b -> Stream c
;;; Zip with a combining function.
(define (stream-zip-with f s1 s2)
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (f (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip-with f (stream-tail s1) (stream-tail s2))))))

;;; stream-interleave : Stream a -> Stream a -> Stream a
;;; Interleave two streams: a1, b1, a2, b2, ...
(define (stream-interleave s1 s2)
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-interleave s2 (stream-tail s1))))))

;;; stream-merge : (a -> a -> Boolean) -> Stream a -> Stream a -> Stream a
;;; Merge two sorted streams maintaining order.
;;; pred should return #t if first arg should come before second.
(define (stream-merge pred s1 s2)
  (cond
   [(stream-nil? s1) s2]
   [(stream-nil? s2) s1]
   [else
    (let ([h1 (stream-head s1)]
          [h2 (stream-head s2)])
         (if (pred h1 h2)
             (stream-cons h1 (lambda () (stream-merge pred (stream-tail s1) s2)))
             (stream-cons h2 (lambda () (stream-merge pred s1 (stream-tail s2))))))]))

;;; ============================================================
;;; Stream Folds
;;; ============================================================

;;; stream-fold : (b -> a -> b) -> b -> Int -> Stream a -> b
;;; Left fold over first n elements of stream.
(define (stream-fold f init n s)
  (if (or (<= n 0) (stream-nil? s))
      init
      (stream-fold f (f init (stream-head s)) (- n 1) (stream-tail s))))

;;; stream-any : (a -> Boolean) -> Int -> Stream a -> Boolean
;;; Check if any of first n elements satisfies predicate.
(define (stream-any pred n s)
  (cond
   [(<= n 0) #f]
   [(stream-nil? s) #f]
   [(pred (stream-head s)) #t]
   [else (stream-any pred (- n 1) (stream-tail s))]))

;;; stream-all : (a -> Boolean) -> Int -> Stream a -> Boolean
;;; Check if all of first n elements satisfy predicate.
(define (stream-all pred n s)
  (cond
   [(<= n 0) #t]
   [(stream-nil? s) #t]
   [(not (pred (stream-head s))) #f]
   [else (stream-all pred (- n 1) (stream-tail s))]))

;;; stream-find : (a -> Boolean) -> Int -> Stream a -> Maybe a
;;; Find first element matching predicate within n elements.
(define (stream-find pred n s)
  (cond
   [(<= n 0) nothing]
   [(stream-nil? s) nothing]
   [(pred (stream-head s)) (just (stream-head s))]
   [else (stream-find pred (- n 1) (stream-tail s))]))

;;; ============================================================
;;; Memoized Streams
;;; ============================================================
;;;
;;; Memoized streams cache computed values to avoid recomputation.

;;; memo-stream-cons : a -> (() -> Stream a) -> Stream a
;;; Create a memoized stream cons cell.
(define (memo-stream-cons head tail-thunk)
  (let ([cached #f]
        [computed #f])
       (list 'memo-stream-cons
             head
             (lambda ()
                     (if computed
                         cached
                         (begin
                          (set! cached (tail-thunk))
                          (set! computed #t)
                          cached))))))

;;; memo-stream? : Any -> Boolean
(define (memo-stream? s)
  (and (pair? s) (eq? (car s) 'memo-stream-cons)))

;;; For convenience, re-define stream operations that work with memo streams
;;; (They already work since we just check for cons pattern)

;;; stream-force : Int -> Stream a -> Stream a
;;; Force computation of the stream to a given depth, returning the original.
(define (stream-force n s)
  (stream-force-helper n s)
  s)

(define (stream-force-helper n s)
  (if (or (<= n 0) (stream-nil? s))
      (void)
      (begin
       (stream-head s)  ; force head
       (stream-force-helper (- n 1) (stream-tail s)))))

;;; ============================================================
;;; Classic Streams
;;; ============================================================

;;; fibonacci : Stream Int
;;; The Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, ...
(define fibonacci
  (letrec ([fibs (lambda (a b)
                         (stream-cons a (lambda () (fibs b (+ a b)))))])
          (fibs 0 1)))

;;; primes : Stream Int
;;; Prime numbers using sieve of Eratosthenes.
(define primes
  (letrec ([sieve (lambda (s)
                          (let ([p (stream-head s)])
                               (stream-cons p
                                            (lambda ()
                                                    (sieve (stream-filter
                                                            (lambda (n) (not (= 0 (modulo n p))))
                                                            (stream-tail s)))))))])
          (sieve (stream-from 2))))

;;; powers-of : Int -> Stream Int
;;; Powers of n: 1, n, n^2, n^3, ...
(define (powers-of n)
  (stream-iterate (lambda (x) (* x n)) 1))

;;; factorials : Stream Int
;;; Factorial sequence: 1, 1, 2, 6, 24, 120, ...
(define factorials
  (stream-scan * 1 (stream-from 1)))

;;; triangular : Stream Int
;;; Triangular numbers: 0, 1, 3, 6, 10, 15, ...
(define triangular
  (stream-scan + 0 (stream-from 1)))

;;; ============================================================
;;; Generator Pattern
;;; ============================================================
;;;
;;; Generators are functions that produce streams on demand.

;;; make-generator : (() -> Maybe a) -> Stream a
;;; Create a stream from a generator function.
;;; Generator returns nothing when exhausted, or (just value) to continue.
(define (make-generator gen)
  (let ([result (gen)])
       (if (nothing? result)
           stream-nil
           (stream-cons (from-just result)
                        (lambda () (make-generator gen))))))

;;; counter-generator : Int -> Int -> (() -> Maybe Int)
;;; Create a generator that counts from start up to (not including) end.
(define (counter-generator start end)
  (let ([current start])
       (lambda ()
               (if (>= current end)
                   nothing
                   (let ([val current])
                        (set! current (+ current 1))
                        (just val))))))

;;; random-stream : Int -> Int -> Int -> Stream Int
;;; Pseudo-random number stream using linear congruential generator.
;;; Parameters: seed, multiplier, modulus
(define (random-stream seed mult mod)
  (stream-iterate (lambda (x) (modulo (* x mult) mod)) seed))

;;; ============================================================
;;; Practical Utilities
;;; ============================================================

;;; stream-partition : (a -> Boolean) -> Stream a -> (Stream a . Stream a)
;;; Split stream into two based on predicate.
(define (stream-partition pred s)
  (cons (stream-filter pred s)
        (stream-filter (lambda (x) (not (pred x))) s)))

;;; stream-group : Int -> Stream a -> Stream (List a)
;;; Group elements into chunks of size n.
(define (stream-group n s)
  (if (stream-nil? s)
      stream-nil
      (stream-cons (stream->list n s)
                   (lambda () (stream-group n (stream-drop n s))))))

;;; stream-distinct : Stream a -> Stream a
;;; Remove consecutive duplicates.
(define (stream-distinct s)
  (if (stream-nil? s)
      stream-nil
      (stream-distinct-helper (stream-head s) (stream-tail s))))

(define (stream-distinct-helper prev s)
  (if (stream-nil? s)
      (stream-cons prev (lambda () stream-nil))
      (let ([h (stream-head s)])
           (if (equal? h prev)
               (stream-distinct-helper prev (stream-tail s))
               (stream-cons prev (lambda () (stream-distinct-helper h (stream-tail s))))))))

;;; stream-enumerate : Stream a -> Stream (Int . a)
;;; Pair each element with its index.
(define (stream-enumerate s)
  (stream-zip naturals s))

;;; ============================================================
;;; Delay/Force Primitives
;;; ============================================================
;;;
;;; These provide explicit delay/force semantics for lazy evaluation.
;;; While Scheme thunks already provide laziness, these forms make
;;; the intent clearer and can be extended with memoization.

;;; delay : (() -> a) -> Delayed a
;;; Create a delayed computation (a thunk wrapped with a tag).
(define (delay thunk)
  (list 'delayed thunk #f #f))  ; (tag, thunk, computed?, cached-value)

;;; force : Delayed a -> a
;;; Force evaluation of a delayed computation.
;;; Memoizes the result for subsequent forces.
(define (force delayed)
  (if (and (pair? delayed) (eq? (car delayed) 'delayed))
      (if (caddr delayed)  ; already computed?
          (cadddr delayed)  ; return cached value
          (let ([val ((cadr delayed))])
               (set-car! (cddr delayed) #t)
               (set-car! (cdddr delayed) val)
               val))
      (error 'force "not a delayed value")))

;;; delayed? : Any -> Boolean
(define (delayed? x)
  (and (pair? x) (eq? (car x) 'delayed)))

;;; ============================================================
;;; Type Class Instances (Dictionary-Passing Style)
;;; ============================================================
;;;
;;; Following The Fold's convention, type classes are represented
;;; as dictionaries (records with operations). This enables
;;; polymorphic code without Haskell-style implicit instances.

;;; ============================================================
;;; Functor Instance for Stream
;;; ============================================================
;;;
;;; Functor laws:
;;;   1. fmap id = id (identity)
;;;   2. fmap (f . g) = fmap f . fmap g (composition)
;;;
;;; Note: stream-map already satisfies the Functor interface.
;;; We provide an explicit dictionary for consistency with the
;;; type class system.

;;; stream-functor : Functor Stream
;;; The Functor dictionary for Stream.
(define stream-functor
  (list 'functor stream-map))

;;; functor-fmap : Functor f -> (a -> b) -> f a -> f b
;;; Generic fmap accessor.
(define (functor-fmap functor)
  (cadr functor))

;;; stream-fmap : (a -> b) -> Stream a -> Stream b
;;; Alias for stream-map, emphasizing the Functor interface.
(define stream-fmap stream-map)

;;; ============================================================
;;; Applicative Instance for Stream
;;; ============================================================
;;;
;;; Applicative laws:
;;;   1. pure id <*> v = v (identity)
;;;   2. pure (.) <*> u <*> v <*> w = u <*> (v <*> w) (composition)
;;;   3. pure f <*> pure x = pure (f x) (homomorphism)
;;;   4. u <*> pure y = pure ($ y) <*> u (interchange)
;;;
;;; For infinite streams, we use the ZipList semantics:
;;;   pure x = repeat x (infinite stream of x)
;;;   fs <*> xs = zipWith ($) fs xs

;;; stream-pure : a -> Stream a
;;; Lift a value into an infinite stream (ZipList-style).
(define stream-pure stream-repeat)

;;; stream-ap : Stream (a -> b) -> Stream a -> Stream b
;;; Apply a stream of functions to a stream of values.
;;; Uses ZipList semantics: apply pointwise.
(define (stream-ap fs xs)
  (stream-zip-with (lambda (f x) (f x)) fs xs))

;;; stream-applicative : Applicative Stream
;;; The Applicative dictionary for Stream.
(define stream-applicative
  (list 'applicative
        stream-functor     ; parent Functor
        stream-pure        ; pure
        stream-ap))        ; <*>

;;; applicative-pure : Applicative f -> a -> f a
(define (applicative-pure app)
  (caddr app))

;;; applicative-ap : Applicative f -> f (a -> b) -> f a -> f b
(define (applicative-ap app)
  (cadddr app))

;;; stream-lift2 : (a -> b -> c) -> Stream a -> Stream b -> Stream c
;;; Lift a binary function to operate on streams.
(define (stream-lift2 f xs ys)
  (stream-ap (stream-map (lambda (x) (lambda (y) (f x y))) xs) ys))

;;; stream-lift3 : (a -> b -> c -> d) -> Stream a -> Stream b -> Stream c -> Stream d
(define (stream-lift3 f xs ys zs)
  (stream-ap
   (stream-ap (stream-map (lambda (x) (lambda (y) (lambda (z) (f x y z)))) xs) ys)
   zs))

;;; ============================================================
;;; Monad Instance for Stream
;;; ============================================================
;;;
;;; Monad laws:
;;;   1. return a >>= f = f a (left identity)
;;;   2. m >>= return = m (right identity)
;;;   3. (m >>= f) >>= g = m >>= (\x -> f x >>= g) (associativity)
;;;
;;; For streams, we use the diagonal semantics for bind:
;;; Conceptually, stream-bind produces a stream of streams,
;;; then we extract the diagonal to get a flat stream.
;;;
;;; This is different from the ZipList Applicative but provides
;;; a valid Monad instance.

;;; stream-return : a -> Stream a
;;; Lift a value into a singleton-like stream.
;;; For monad consistency, we return a repeating stream.
(define stream-return stream-repeat)

;;; stream-bind : Stream a -> (a -> Stream b) -> Stream b
;;; Monadic bind using diagonal extraction.
;;; Takes nth element from the nth stream produced by f.
(define (stream-bind s f)
  (stream-diagonal (stream-map f s)))

;;; stream-diagonal : Stream (Stream a) -> Stream a
;;; Extract the diagonal from a stream of streams.
;;; Returns: 1st elem of 1st stream, 2nd elem of 2nd stream, etc.
(define (stream-diagonal ss)
  (if (stream-nil? ss)
      stream-nil
      (let ([inner (stream-head ss)])
           (if (stream-nil? inner)
               (stream-diagonal (stream-map stream-tail (stream-tail ss)))
               (stream-cons (stream-head inner)
                            (lambda ()
                                    (stream-diagonal
                                     (stream-map stream-tail (stream-tail ss)))))))))

;;; stream-join : Stream (Stream a) -> Stream a
;;; Flatten a stream of streams (monadic join).
;;; Alias for stream-diagonal.
(define stream-join stream-diagonal)

;;; stream-monad : Monad Stream
;;; The Monad dictionary for Stream.
(define stream-monad
  (list 'monad
        stream-applicative  ; parent Applicative
        stream-return       ; return
        stream-bind))       ; >>=

;;; monad-return : Monad m -> a -> m a
(define (monad-return monad)
  (caddr monad))

;;; monad-bind : Monad m -> m a -> (a -> m b) -> m b
(define (monad-bind monad)
  (cadddr monad))

;;; stream-then : Stream a -> Stream b -> Stream b
;;; Sequence two streams, discarding the first result.
(define (stream-then s1 s2)
  (stream-bind s1 (lambda (_) s2)))

;;; ============================================================
;;; Codata and Coinductive Patterns
;;; ============================================================
;;;
;;; Streams are the canonical example of CODATA - they are defined
;;; by their observations (destructors) rather than construction.
;;;
;;; Data (inductive): defined by how we BUILD it
;;;   - Lists: nil | cons head tail
;;;   - Finite, structural recursion terminates
;;;
;;; Codata (coinductive): defined by how we OBSERVE it
;;;   - Streams: head and tail observations
;;;   - Potentially infinite, corecursion produces values
;;;
;;; The key insight: we don't ask "how was this stream built?"
;;; but rather "what do we observe when we examine it?"

;;; coalgebra-step : (s -> (a . s)) -> s -> Stream a
;;; Build a stream from a coalgebra (unfold step function).
;;; The coalgebra returns (value . next-state) pairs.
(define (coalgebra-step step seed)
  (let ([pair (step seed)])
       (stream-cons (car pair)
                    (lambda () (coalgebra-step step (cdr pair))))))

;;; anamorphism : (s -> Maybe (a . s)) -> s -> Stream a
;;; Build a stream using an anamorphism (unfold).
;;; Returns empty stream when step returns nothing.
;;; This is just stream-unfold with a different name emphasizing
;;; the recursion scheme perspective.
(define anamorphism stream-unfold)

;;; coiterate : (s -> s) -> s -> Stream s
;;; Coiterate: produce stream of states by repeated application.
;;; This is the coinductive version of iterate.
(define (coiterate f seed)
  (stream-cons seed (lambda () (coiterate f (f seed)))))

;;; bisimulation-equal? : Int -> Stream a -> Stream a -> Boolean
;;; Check if two streams are equal up to depth n.
;;; Full stream equality is undecidable for infinite streams,
;;; so we use bounded bisimulation.
(define (bisimulation-equal? n s1 s2)
  (cond
   [(<= n 0) #t]
   [(and (stream-nil? s1) (stream-nil? s2)) #t]
   [(or (stream-nil? s1) (stream-nil? s2)) #f]
   [(not (equal? (stream-head s1) (stream-head s2))) #f]
   [else (bisimulation-equal? (- n 1) (stream-tail s1) (stream-tail s2))]))

;;; ============================================================
;;; Additional Infinite Stream Generators
;;; ============================================================

;;; squares : Stream Int
;;; Perfect squares: 0, 1, 4, 9, 16, 25, ...
(define squares
  (stream-map (lambda (n) (* n n)) naturals))

;;; cubes : Stream Int
;;; Perfect cubes: 0, 1, 8, 27, 64, 125, ...
(define cubes
  (stream-map (lambda (n) (* n n n)) naturals))

;;; evens : Stream Int
;;; Even numbers: 0, 2, 4, 6, 8, ...
(define evens
  (stream-filter even? naturals))

;;; odds : Stream Int
;;; Odd numbers: 1, 3, 5, 7, 9, ...
(define odds
  (stream-filter odd? naturals))

;;; ============================================================
;;; Fuel-Based Operations (For Termination Safety)
;;; ============================================================
;;;
;;; Operations on potentially infinite streams need fuel limits
;;; to guarantee termination. These complement stream-fold.

;;; stream-for-each/fuel : (a -> ()) -> Int -> Stream a -> ()
;;; Apply effectful operation to each element up to fuel limit.
(define (stream-for-each/fuel f fuel s)
  (when (and (> fuel 0) (not (stream-nil? s)))
        (f (stream-head s))
        (stream-for-each/fuel f (- fuel 1) (stream-tail s))))

;;; stream-length/fuel : Int -> Stream a -> Int
;;; Compute length of stream up to fuel limit.
;;; Returns fuel if stream is longer than fuel.
(define (stream-length/fuel fuel s)
  (let loop ([n 0] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) n]
        [(stream-nil? s) n]
        [else (loop (+ n 1) (- fuel 1) (stream-tail s))])))

;;; stream-reverse/fuel : Int -> Stream a -> Stream a
;;; Reverse the first n elements of a stream.
(define (stream-reverse/fuel fuel s)
  (list->stream (reverse (stream->list fuel s))))

;;; stream-last/fuel : Int -> Stream a -> Maybe a
;;; Get the last element within fuel elements.
(define (stream-last/fuel fuel s)
  (let loop ([last nothing] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) last]
        [(stream-nil? s) last]
        [else (loop (just (stream-head s)) (- fuel 1) (stream-tail s))])))

;;; ============================================================
;;; Stream Comprehensions (via Monad)
;;; ============================================================
;;;
;;; Using the monad interface, we can express list-comprehension
;;; style patterns for streams.

;;; stream-guard : Boolean -> Stream ()
;;; Filter guard for stream comprehensions.
;;; Returns singleton () if true, empty if false.
(define (stream-guard condition)
  (if condition
      (stream-cons '() (lambda () stream-nil))
      stream-nil))

;;; stream-cartesian : Stream a -> Stream b -> Stream (a . b)
;;; Cartesian product of two streams (via monad bind).
;;; Note: This interleaves elements to ensure fair enumeration
;;; of the infinite product.
(define (stream-cartesian xs ys)
  (stream-bind xs
               (lambda (x)
                       (stream-map (lambda (y) (cons x y)) ys))))
