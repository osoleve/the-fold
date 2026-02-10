;;; lattice/fp/data/stream.ss — Lazy Streams
;;; @module stream
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'stream)
(doc 'description "Stream Type A stream is either: - stream-nil: the empty stream - (stream-cons head thunk): head value with lazy tail The thunk is a nullary procedure that, when called, produces")
(doc 'layer 'lattice)
(define stream-nil '(stream-nil))

;;; stream-nil? : (Stream α) → Bool
(define (stream-nil? s)
  (and (pair? s) (eq? (car s) 'stream-nil)))

;;; stream-cons : α × (() → (Stream α)) → (Stream α)
;;; Construct a stream with a head and lazy tail.
(define (stream-cons head tail-thunk)
  (list 'stream-cons head tail-thunk))

;;; stream-cons? : (Stream α) → Bool
(define (stream-cons? s)
  (and (pair? s)
       (or (eq? (car s) 'stream-cons)
           (eq? (car s) 'memo-stream-cons))))

;;; stream-head : (Stream α) → α
;;; Get the first element (partial on empty stream).
(define (stream-head s)
  (if (stream-cons? s)
      (list-ref s 1)
      (error 'stream-head "empty stream")))

;;; stream-tail : (Stream α) → (Stream α)
;;; Force and return the tail.
(define (stream-tail s)
  (if (stream-cons? s)
      ((list-ref s 2))
      (error 'stream-tail "empty stream")))

;;; ====
;;; Stream Constructors
;;; ====

;;; list->stream : (List α) → (Stream α)
;;; Convert a list to a stream.
(define (list->stream lst)
  (if (null? lst)
      stream-nil
      (stream-cons (car lst) (lambda () (list->stream (cdr lst))))))

;;; stream->list : Nat × (Stream α) → (List α)
;;; Take n elements from stream and convert to list.
(define (stream->list n s)
  (if (or (<= n 0) (stream-nil? s))
      '()
      (cons (stream-head s)
            (stream->list (- n 1) (stream-tail s)))))

;;; stream-iterate : (α → α) × α → (Stream α)
;;; Generate infinite stream: x, f(x), f(f(x)), ...
(define (stream-iterate f x)
  (stream-cons x (lambda () (stream-iterate f (f x)))))

;;; stream-repeat : α → (Stream α)
;;; Infinite stream of a single repeated value.
(define (stream-repeat x)
  (stream-cons x (lambda () (stream-repeat x))))

;;; stream-cycle : (List α) → (Stream α)
;;; Infinite stream cycling through a list.
(define (stream-cycle lst)
  (if (null? lst)
      stream-nil
      (stream-cycle-helper lst lst)))

;;; stream-cycle-helper : (List α) × (List α) → (Stream α)
(define (stream-cycle-helper original current)
  (if (null? current)
      (stream-cycle-helper original original)
      (stream-cons (car current)
                   (lambda () (stream-cycle-helper original (cdr current))))))

;;; stream-from : Int → (Stream Int)
;;; Infinite stream of integers starting from n.
(define (stream-from n)
  (stream-iterate (lambda (x) (+ x 1)) n))

;;; stream-range : Int × Int → (Stream Int)
;;; Stream of integers from start (inclusive) to end (exclusive).
(define (stream-range start end)
  (if (>= start end)
      stream-nil
      (stream-cons start (lambda () (stream-range (+ start 1) end)))))

;;; naturals : (Stream Int)
;;; The natural numbers: 0, 1, 2, 3, ...
(define naturals (stream-from 0))

;;; stream-unfold : (σ → (Maybe (α × σ))) × σ → (Stream α)
;;; Build a stream from a seed using a step function.
;;; Step function returns nothing to end, or (just (value . new-seed)) to continue.
(define (stream-unfold step seed)
  (let ([result (step seed)])
       (if (nothing? result)
           stream-nil
           (let ([pair (from-just result)])
                (stream-cons (car pair)
                             (lambda () (stream-unfold step (cdr pair))))))))

;;; ====
;;; Stream Transformers
;;; ====

;;; stream-map : (α → β) × (Stream α) → (Stream β)
;;; Map a function over a stream.
(define (stream-map f s)
  (if (stream-nil? s)
      stream-nil
      (stream-cons (f (stream-head s))
                   (lambda () (stream-map f (stream-tail s))))))

;;; stream-filter : (α → Bool) × (Stream α) → (Stream α)
;;; Filter a stream by a predicate.
(define (stream-filter pred s)
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s))
    (stream-cons (stream-head s)
                 (lambda () (stream-filter pred (stream-tail s))))]
   [else (stream-filter pred (stream-tail s))]))

;;; stream-take : Nat × (Stream α) → (Stream α)
;;; Take the first n elements.
(define (stream-take n s)
  (if (or (<= n 0) (stream-nil? s))
      stream-nil
      (stream-cons (stream-head s)
                   (lambda () (stream-take (- n 1) (stream-tail s))))))

;;; stream-drop : Nat × (Stream α) → (Stream α)
;;; Drop the first n elements.
(define (stream-drop n s)
  (if (or (<= n 0) (stream-nil? s))
      s
      (stream-drop (- n 1) (stream-tail s))))

;;; stream-take-while : (α → Bool) × (Stream α) → (Stream α)
;;; Take elements while predicate holds.
(define (stream-take-while pred s)
  (if (stream-nil? s)
      stream-nil
      (let ([h (stream-head s)])
           (if (pred h)
               (stream-cons h (lambda () (stream-take-while pred (stream-tail s))))
               stream-nil))))

;;; stream-drop-while : (α → Bool) × (Stream α) → (Stream α)
;;; Drop elements while predicate holds.
(define (stream-drop-while pred s)
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s)) (stream-drop-while pred (stream-tail s))]
   [else s]))

;;; stream-nth : Nat × (Stream α) → α
;;; Get the nth element (0-indexed).
(define (stream-nth n s)
  (stream-head (stream-drop n s)))

;;; stream-scan : (β × α → β) × β × (Stream α) → (Stream β)
;;; Running fold over a stream.
(define (stream-scan f init s)
  (stream-cons init
               (lambda ()
                       (if (stream-nil? s)
                           stream-nil
                           (stream-scan f (f init (stream-head s)) (stream-tail s))))))

;;; stream-concat : (Stream α) × (Stream α) → (Stream α)
;;; Concatenate two streams (second stream is only accessed when first ends).
(define (stream-concat s1 s2)
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-concat (stream-tail s1) s2)))))

;;; stream-flatten : (Stream (Stream α)) → (Stream α)
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

;;; stream-flatmap : (α → (Stream β)) × (Stream α) → (Stream β)
;;; Map and flatten.
(define (stream-flatmap f s)
  (stream-flatten (stream-map f s)))

;;; ====
;;; Stream Combinators
;;; ====

;;; stream-zip : (Stream α) × (Stream β) → (Stream (Pair α β))
;;; Zip two streams together.
(define (stream-zip s1 s2)
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (cons (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip (stream-tail s1) (stream-tail s2))))))

;;; stream-zip-with : (α × β → γ) × (Stream α) × (Stream β) → (Stream γ)
;;; Zip with a combining function.
(define (stream-zip-with f s1 s2)
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (f (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip-with f (stream-tail s1) (stream-tail s2))))))

;;; stream-interleave : (Stream α) × (Stream α) → (Stream α)
;;; Interleave two streams: a1, b1, a2, b2, ...
(define (stream-interleave s1 s2)
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-interleave s2 (stream-tail s1))))))

;;; stream-merge : (α × α → Bool) × (Stream α) × (Stream α) → (Stream α)
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

;;; ====
;;; Stream Folds
;;; ====

;;; stream-fold : (β × α → β) × β × Nat × (Stream α) → β
;;; Left fold over first n elements of stream.
(define (stream-fold f init n s)
  (if (or (<= n 0) (stream-nil? s))
      init
      (stream-fold f (f init (stream-head s)) (- n 1) (stream-tail s))))

;;; stream-any : (α → Bool) × Nat × (Stream α) → Bool
;;; Check if any of first n elements satisfies predicate.
(define (stream-any pred n s)
  (cond
   [(<= n 0) #f]
   [(stream-nil? s) #f]
   [(pred (stream-head s)) #t]
   [else (stream-any pred (- n 1) (stream-tail s))]))

;;; stream-all : (α → Bool) × Nat × (Stream α) → Bool
;;; Check if all of first n elements satisfy predicate.
(define (stream-all pred n s)
  (cond
   [(<= n 0) #t]
   [(stream-nil? s) #t]
   [(not (pred (stream-head s))) #f]
   [else (stream-all pred (- n 1) (stream-tail s))]))

;;; stream-find : (α → Bool) × Nat × (Stream α) → (Maybe α)
;;; Find first element matching predicate within n elements.
(define (stream-find pred n s)
  (cond
   [(<= n 0) nothing]
   [(stream-nil? s) nothing]
   [(pred (stream-head s)) (just (stream-head s))]
   [else (stream-find pred (- n 1) (stream-tail s))]))

;;; ====
;;; Memoized Streams
;;; ====
;;;
;;; Memoized streams cache computed values to avoid recomputation.

;;; memo-stream-cons : α × (() → (Stream α)) → (Stream α)
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

;;; memo-stream? : α → Bool
(define (memo-stream? s)
  (and (pair? s) (eq? (car s) 'memo-stream-cons)))

;;; For convenience, re-define stream operations that work with memo streams
;;; (They already work since we just check for cons pattern)

;;; stream-force : Nat × (Stream α) → (Stream α)
;;; Force computation of the stream to a given depth, returning the original.
(define (stream-force n s)
  (stream-force-helper n s)
  s)

;;; stream-force-helper : Nat × (Stream α) → Unit
(define (stream-force-helper n s)
  (if (or (<= n 0) (stream-nil? s))
      (void)
      (begin
       (stream-head s)  ; force head
       (stream-force-helper (- n 1) (stream-tail s)))))

;;; ====
;;; Classic Streams
;;; ====

;;; fibonacci : (Stream Int)
;;; The Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, ...
(define fibonacci
  (letrec ([fibs (lambda (a b)
                         (stream-cons a (lambda () (fibs b (+ a b)))))])
          (fibs 0 1)))

;;; primes : (Stream Int)
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

;;; powers-of : Int → (Stream Int)
;;; Powers of n: 1, n, n^2, n^3, ...
(define (powers-of n)
  (stream-iterate (lambda (x) (* x n)) 1))

;;; factorials : (Stream Int)
;;; Factorial sequence: 1, 1, 2, 6, 24, 120, ...
(define factorials
  (stream-scan * 1 (stream-from 1)))

;;; triangular : (Stream Int)
;;; Triangular numbers: 0, 1, 3, 6, 10, 15, ...
(define triangular
  (stream-scan + 0 (stream-from 1)))

;;; ====
;;; Generator Pattern
;;; ====
;;;
;;; Generators are functions that produce streams on demand.

;;; make-generator : (() → (Maybe α)) → (Stream α)
;;; Create a stream from a generator function.
;;; Generator returns nothing when exhausted, or (just value) to continue.
(define (make-generator gen)
  (let ([result (gen)])
       (if (nothing? result)
           stream-nil
           (stream-cons (from-just result)
                        (lambda () (make-generator gen))))))

;;; counter-generator : Int × Int → (() → (Maybe Int))
;;; Create a generator that counts from start up to (not including) end.
(define (counter-generator start end)
  (let ([current start])
       (lambda ()
               (if (>= current end)
                   nothing
                   (let ([val current])
                        (set! current (+ current 1))
                        (just val))))))

;;; random-stream : Int × Int × Int → (Stream Int)
;;; Pseudo-random number stream using linear congruential generator.
;;; Parameters: seed, multiplier, modulus
(define (random-stream seed mult mod)
  (stream-iterate (lambda (x) (modulo (* x mult) mod)) seed))

;;; ====
;;; Practical Utilities
;;; ====

;;; stream-partition : (α → Bool) × (Stream α) → (Pair (Stream α) (Stream α))
;;; Split stream into two based on predicate.
(define (stream-partition pred s)
  (cons (stream-filter pred s)
        (stream-filter (lambda (x) (not (pred x))) s)))

;;; stream-group : Nat × (Stream α) → (Stream (List α))
;;; Group elements into chunks of size n.
(define (stream-group n s)
  (if (stream-nil? s)
      stream-nil
      (stream-cons (stream->list n s)
                   (lambda () (stream-group n (stream-drop n s))))))

;;; stream-distinct : (Stream α) → (Stream α)
;;; Remove consecutive duplicates.
(define (stream-distinct s)
  (if (stream-nil? s)
      stream-nil
      (stream-distinct-helper (stream-head s) (stream-tail s))))

;;; stream-distinct-helper : α × (Stream α) → (Stream α)
(define (stream-distinct-helper prev s)
  (if (stream-nil? s)
      (stream-cons prev (lambda () stream-nil))
      (let ([h (stream-head s)])
           (if (equal? h prev)
               (stream-distinct-helper prev (stream-tail s))
               (stream-cons prev (lambda () (stream-distinct-helper h (stream-tail s))))))))

;;; stream-enumerate : (Stream α) → (Stream (Pair Int α))
;;; Pair each element with its index.
(define (stream-enumerate s)
  (stream-zip naturals s))

;;; ====
;;; Delay/Force Primitives
;;; ====
;;;
;;; These provide explicit delay/force semantics for lazy evaluation.
;;; While Scheme thunks already provide laziness, these forms make
;;; the intent clearer and can be extended with memoization.

;;; delay : (() → α) → (Delayed α)
;;; Create a delayed computation (a thunk wrapped with a tag).
(define (delay thunk)
  (list 'delayed thunk #f #f))  ; (tag, thunk, computed?, cached-value)

;;; force : (Delayed α) → α
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

;;; delayed? : α → Bool
(define (delayed? x)
  (and (pair? x) (eq? (car x) 'delayed)))

;;; ====
;;; Type Class Instances (Dictionary-Passing Style)
;;; ====
;;;
;;; Following The Fold's convention, type classes are represented
;;; as dictionaries (records with operations). This enables
;;; polymorphic code without Haskell-style implicit instances.

;;; ====
;;; Functor Instance for Stream
;;; ====
;;;
;;; Functor laws:
;;;   1. fmap id = id (identity)
;;;   2. fmap (f . g) = fmap f . fmap g (composition)
;;;
;;; Note: stream-map already satisfies the Functor interface.
;;; We provide an explicit dictionary for consistency with the
;;; type class system.

;;; stream-functor : (List α)
;;; The Functor dictionary for Stream.
;;; Structure: ('functor, fmap)
(define stream-functor
  (list 'functor stream-map))

;;; functor-fmap : (List α) → (α → β) → (f α) → (f β)
;;; Generic fmap accessor.
(define (functor-fmap functor)
  (cadr functor))

;;; stream-fmap : (α → β) × (Stream α) → (Stream β)
;;; Alias for stream-map, emphasizing the Functor interface.
(define stream-fmap stream-map)

;;; ====
;;; Applicative Instance for Stream
;;; ====
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

;;; stream-pure : α → (Stream α)
;;; Lift a value into an infinite stream (ZipList-style).
(define stream-pure stream-repeat)

;;; stream-ap : (Stream (α → β)) × (Stream α) → (Stream β)
;;; Apply a stream of functions to a stream of values.
;;; Uses ZipList semantics: apply pointwise.
(define (stream-ap fs xs)
  (stream-zip-with (lambda (f x) (f x)) fs xs))

;;; stream-applicative : (List α)
;;; The Applicative dictionary for Stream.
;;; Structure: ('applicative, functor, pure, ap)
(define stream-applicative
  (list 'applicative
        stream-functor     ; parent Functor
        stream-pure        ; pure
        stream-ap))        ; <*>

;;; applicative-pure : (List α) → β
;;; Extract pure function from Applicative dictionary.
(define (applicative-pure app)
  (caddr app))

;;; applicative-ap : (List α) → β
;;; Extract ap function from Applicative dictionary.
(define (applicative-ap app)
  (cadddr app))

;;; stream-lift2 : (α × β → γ) × (Stream α) × (Stream β) → (Stream γ)
;;; Lift a binary function to operate on streams.
(define (stream-lift2 f xs ys)
  (stream-ap (stream-map (lambda (x) (lambda (y) (f x y))) xs) ys))

;;; stream-lift3 : (α × β × γ → δ) × (Stream α) × (Stream β) × (Stream γ) → (Stream δ)
(define (stream-lift3 f xs ys zs)
  (stream-ap
   (stream-ap (stream-map (lambda (x) (lambda (y) (lambda (z) (f x y z)))) xs) ys)
   zs))

;;; ====
;;; Monad Instance for Stream
;;; ====
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

;;; stream-return : α → (Stream α)
;;; Lift a value into a singleton-like stream.
;;; For monad consistency, we return a repeating stream.
(define stream-return stream-repeat)

;;; stream-bind : (Stream α) × (α → (Stream β)) → (Stream β)
;;; Monadic bind using diagonal extraction.
;;; Takes nth element from the nth stream produced by f.
(define (stream-bind s f)
  (stream-diagonal (stream-map f s)))

;;; stream-diagonal : (Stream (Stream α)) → (Stream α)
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

;;; stream-join : (Stream (Stream α)) → (Stream α)
;;; Flatten a stream of streams (monadic join).
;;; Alias for stream-diagonal.
(define stream-join stream-diagonal)

;;; stream-monad : (List α)
;;; The Monad dictionary for Stream.
;;; Structure: ('monad, applicative, return, bind)
(define stream-monad
  (list 'monad
        stream-applicative  ; parent Applicative
        stream-return       ; return
        stream-bind))       ; >>=

;;; monad-return : (List α) → β
;;; Extract return function from Monad dictionary.
(define (monad-return monad)
  (caddr monad))

;;; monad-bind : (List α) → β
;;; Extract bind function from Monad dictionary.
(define (monad-bind monad)
  (cadddr monad))

;;; stream-then : (Stream α) × (Stream β) → (Stream β)
;;; Sequence two streams, discarding the first result.
(define (stream-then s1 s2)
  (stream-bind s1 (lambda (_) s2)))

;;; ====
;;; Codata and Coinductive Patterns
;;; ====
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

;;; coalgebra-step : (σ → (Pair α σ)) × σ → (Stream α)
;;; Build a stream from a coalgebra (unfold step function).
;;; The coalgebra returns (value . next-state) pairs.
(define (coalgebra-step step seed)
  (let ([pair (step seed)])
       (stream-cons (car pair)
                    (lambda () (coalgebra-step step (cdr pair))))))

;;; anamorphism : (σ → (Maybe (Pair α σ))) × σ → (Stream α)
;;; Build a stream using an anamorphism (unfold).
;;; Returns empty stream when step returns nothing.
;;; This is just stream-unfold with a different name emphasizing
;;; the recursion scheme perspective.
(define anamorphism stream-unfold)

;;; coiterate : (σ → σ) × σ → (Stream σ)
;;; Coiterate: produce stream of states by repeated application.
;;; This is the coinductive version of iterate.
(define (coiterate f seed)
  (stream-cons seed (lambda () (coiterate f (f seed)))))

;;; bisimulation-equal? : Nat × (Stream α) × (Stream α) → Bool
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

;;; ====
;;; Additional Infinite Stream Generators
;;; ====

;;; squares : (Stream Int)
;;; Perfect squares: 0, 1, 4, 9, 16, 25, ...
(define squares
  (stream-map (lambda (n) (* n n)) naturals))

;;; cubes : (Stream Int)
;;; Perfect cubes: 0, 1, 8, 27, 64, 125, ...
(define cubes
  (stream-map (lambda (n) (* n n n)) naturals))

;;; evens : (Stream Int)
;;; Even numbers: 0, 2, 4, 6, 8, ...
(define evens
  (stream-filter even? naturals))

;;; odds : (Stream Int)
;;; Odd numbers: 1, 3, 5, 7, 9, ...
(define odds
  (stream-filter odd? naturals))

;;; ====
;;; Fuel-Based Operations (For Termination Safety)
;;; ====
;;;
;;; Operations on potentially infinite streams need fuel limits
;;; to guarantee termination. These complement stream-fold.

;;; stream-for-each/fuel : (α → Unit) × Nat × (Stream α) → Unit
;;; Apply effectful operation to each element up to fuel limit.
(define (stream-for-each/fuel f fuel s)
  (when (and (> fuel 0) (not (stream-nil? s)))
        (f (stream-head s))
        (stream-for-each/fuel f (- fuel 1) (stream-tail s))))

;;; stream-length/fuel : Nat × (Stream α) → Nat
;;; Compute length of stream up to fuel limit.
;;; Returns fuel if stream is longer than fuel.
(define (stream-length/fuel fuel s)
  (let loop ([n 0] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) n]
        [(stream-nil? s) n]
        [else (loop (+ n 1) (- fuel 1) (stream-tail s))])))

;;; stream-reverse/fuel : Nat × (Stream α) → (Stream α)
;;; Reverse the first n elements of a stream.
(define (stream-reverse/fuel fuel s)
  (list->stream (reverse (stream->list fuel s))))

;;; stream-last/fuel : Nat × (Stream α) → (Maybe α)
;;; Get the last element within fuel elements.
(define (stream-last/fuel fuel s)
  (let loop ([last nothing] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) last]
        [(stream-nil? s) last]
        [else (loop (just (stream-head s)) (- fuel 1) (stream-tail s))])))

;;; ====
;;; Stream Comprehensions (via Monad)
;;; ====
;;;
;;; Using the monad interface, we can express list-comprehension
;;; style patterns for streams.

;;; stream-guard : Bool → (Stream Unit)
;;; Filter guard for stream comprehensions.
;;; Returns singleton () if true, empty if false.
(define (stream-guard condition)
  (if condition
      (stream-cons '() (lambda () stream-nil))
      stream-nil))

;;; stream-cartesian : (Stream α) × (Stream β) → (Stream (Pair α β))
;;; Fair Cartesian product using Cantor's diagonal enumeration.
;;; Enumerates pairs by diagonal sum: (0,0), (0,1), (1,0), (0,2), (1,1), (2,0), ...
;;; This ensures every pair (i,j) eventually appears, even for infinite streams.
(define (stream-cartesian xs ys)
  ;; Convert to vectors for O(1) indexed access (memoizes as we go)
  (let ([xs-vec (make-vector 0)]
        [ys-vec (make-vector 0)]
        [xs-done #f]
        [ys-done #f])
       ;; Ensure element at index i is cached, return #f if stream exhausted before i
       (define (ensure-cached! vec-box done-box stream i)
         (let ([vec (unbox vec-box)])
              (if (< i (vector-length vec))
                  #t  ; Already cached
                  (if (unbox done-box)
                      #f  ; Stream exhausted
                      ;; Extend cache
                      (let loop ([s (if (= (vector-length vec) 0)
                                        stream
                                        (stream-drop (vector-length vec) stream))]
                                 [v vec])
                           (if (stream-nil? s)
                               (begin
                                (set-box! done-box #t)
                                (set-box! vec-box v)
                                (< i (vector-length v)))
                               (if (> (vector-length v) i)
                                   (begin
                                    (set-box! vec-box v)
                                    #t)
                                   (loop (stream-tail s)
                                         (vector-append v (vector (stream-head s)))))))))))
       ;; Boxes for mutable state
       (let ([xs-vec-box (box (vector))]
             [ys-vec-box (box (vector))]
             [xs-done-box (box #f)]
             [ys-done-box (box #f)])
            ;; Generate pairs by diagonal
            (define (diagonal-stream diag)
              (let ([pairs (diagonal-pairs diag)])
                   (if (null? pairs)
                       ;; Check if we should continue to next diagonal
                       (if (and (unbox xs-done-box) (unbox ys-done-box))
                           stream-nil
                           (diagonal-stream (+ diag 1)))
                       (stream-append (list->stream pairs)
                                      (lambda () (diagonal-stream (+ diag 1)))))))
            ;; Get valid pairs for diagonal k: (0,k), (1,k-1), ..., (k,0)
            (define (diagonal-pairs k)
              (let loop ([i 0] [acc '()])
                   (if (> i k)
                       (reverse acc)
                       (let ([j (- k i)])
                            (if (and (ensure-cached! xs-vec-box xs-done-box xs j)
                                     (ensure-cached! ys-vec-box ys-done-box ys i))
                                (loop (+ i 1)
                                      (cons (cons (vector-ref (unbox xs-vec-box) j)
                                                  (vector-ref (unbox ys-vec-box) i))
                                            acc))
                                (loop (+ i 1) acc))))))
            (diagonal-stream 0))))

;;; stream-append : (Stream α) × (() → (Stream α)) → (Stream α)
;;; Append a stream with a lazily-evaluated second stream.
(define (stream-append s1 thunk)
  (if (stream-nil? s1)
      (thunk)
      (stream-cons (stream-head s1)
                   (lambda () (stream-append (stream-tail s1) thunk)))))
