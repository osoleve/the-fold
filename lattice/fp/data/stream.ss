;;; lattice/fp/data/stream.ss — Lazy Streams
;;; @module stream
;;; @requires prelude combinators
;;; @purity mixed
;;; @stability stable

(require 'prelude)
(require 'combinators)

(doc 'module 'stream)
(doc 'purity 'partial)
(doc 'description "Lazy streams: codata defined by head/tail observations. Supports infinite sequences, memoization, Functor/Applicative/Monad instances, and fuel-bounded operations.")
(doc 'layer 'lattice)

(doc 'section 'core-type)

(define stream-nil '(stream-nil))
(doc stream-nil 'export #t)
(doc stream-nil 'type '(Stream a))
(doc stream-nil 'description "The empty stream")

(define (stream-nil? s)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if stream is empty")
  (and (pair? s) (eq? (car s) 'stream-nil)))

(define (stream-cons head tail-thunk)
  (doc 'export #t)
  (doc 'type '(-> a (-> (Stream a)) (Stream a)))
  (doc 'description "Construct a stream with a head and lazy tail thunk")
  (list 'stream-cons head tail-thunk))

(define (stream-cons? s)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if stream has at least one element")
  (and (pair? s)
       (or (eq? (car s) 'stream-cons)
           (eq? (car s) 'memo-stream-cons))))

(define (stream-head s)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) a))
  (doc 'description "Get the first element; error on empty stream")
  (if (stream-cons? s)
      (list-ref s 1)
      (error 'stream-head "empty stream")))

(define (stream-tail s)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream a)))
  (doc 'description "Force and return the tail; error on empty stream")
  (if (stream-cons? s)
      ((list-ref s 2))
      (error 'stream-tail "empty stream")))

(doc 'section 'constructors)

(define (list->stream lst)
  (doc 'export #t)
  (doc 'type '(-> (List a) (Stream a)))
  (doc 'description "Convert a list to a stream")
  (if (null? lst)
      stream-nil
      (stream-cons (car lst) (lambda () (list->stream (cdr lst))))))

(define (stream->list n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (List a)))
  (doc 'description "Take n elements from stream and convert to list")
  (if (or (<= n 0) (stream-nil? s))
      '()
      (cons (stream-head s)
            (stream->list (- n 1) (stream-tail s)))))

(define (stream-iterate f x)
  (doc 'export #t)
  (doc 'type '(-> (-> a a) a (Stream a)))
  (doc 'description "Infinite stream: x, f(x), f(f(x)), ...")
  (stream-cons x (lambda () (stream-iterate f (f x)))))

(define (stream-repeat x)
  (doc 'export #t)
  (doc 'type '(-> a (Stream a)))
  (doc 'description "Infinite stream of a single repeated value")
  (stream-cons x (lambda () (stream-repeat x))))

(define (stream-cycle lst)
  (doc 'export #t)
  (doc 'type '(-> (List a) (Stream a)))
  (doc 'description "Infinite stream cycling through a list; empty list gives empty stream")
  (if (null? lst)
      stream-nil
      (stream-cycle-helper lst lst)))

(define (stream-cycle-helper original current)
  (doc 'export #t)
  (doc 'type '(-> (List a) (List a) (Stream a)))
  (doc 'description "Internal helper for stream-cycle")
  (if (null? current)
      (stream-cycle-helper original original)
      (stream-cons (car current)
                   (lambda () (stream-cycle-helper original (cdr current))))))

(define (stream-from n)
  (doc 'export #t)
  (doc 'type '(-> Int (Stream Int)))
  (doc 'description "Infinite stream of integers starting from n")
  (stream-iterate (lambda (x) (+ x 1)) n))

(define (stream-range start end)
  (doc 'export #t)
  (doc 'type '(-> Int Int (Stream Int)))
  (doc 'description "Stream of integers from start (inclusive) to end (exclusive)")
  (if (>= start end)
      stream-nil
      (stream-cons start (lambda () (stream-range (+ start 1) end)))))

(define naturals (stream-from 0))
(doc naturals 'export #t)
(doc naturals 'type '(Stream Int))
(doc naturals 'description "The natural numbers: 0, 1, 2, 3, ...")

(define (stream-unfold step seed)
  (doc 'export #t)
  (doc 'type '(-> (-> s (Maybe (Pair a s))) s (Stream a)))
  (doc 'description "Build stream from seed; step returns nothing to end, (just (value . next-seed)) to continue")
  (let ([result (step seed)])
       (if (nothing? result)
           stream-nil
           (let ([pair (from-just result)])
                (stream-cons (car pair)
                             (lambda () (stream-unfold step (cdr pair))))))))

(doc 'section 'transformers)

(define (stream-map f s)
  (doc 'export #t)
  (doc 'type '(-> (-> a b) (Stream a) (Stream b)))
  (doc 'description "Map a function over a stream")
  (if (stream-nil? s)
      stream-nil
      (stream-cons (f (stream-head s))
                   (lambda () (stream-map f (stream-tail s))))))

(define (stream-filter pred s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) (Stream a) (Stream a)))
  (doc 'description "Filter a stream by a predicate")
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s))
    (stream-cons (stream-head s)
                 (lambda () (stream-filter pred (stream-tail s))))]
   [else (stream-filter pred (stream-tail s))]))

(define (stream-take n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream a)))
  (doc 'description "Take the first n elements as a stream")
  (if (or (<= n 0) (stream-nil? s))
      stream-nil
      (stream-cons (stream-head s)
                   (lambda () (stream-take (- n 1) (stream-tail s))))))

(define (stream-drop n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream a)))
  (doc 'description "Drop the first n elements")
  (if (or (<= n 0) (stream-nil? s))
      s
      (stream-drop (- n 1) (stream-tail s))))

(define (stream-take-while pred s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) (Stream a) (Stream a)))
  (doc 'description "Take elements while predicate holds")
  (if (stream-nil? s)
      stream-nil
      (let ([h (stream-head s)])
           (if (pred h)
               (stream-cons h (lambda () (stream-take-while pred (stream-tail s))))
               stream-nil))))

(define (stream-drop-while pred s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) (Stream a) (Stream a)))
  (doc 'description "Drop elements while predicate holds")
  (cond
   [(stream-nil? s) stream-nil]
   [(pred (stream-head s)) (stream-drop-while pred (stream-tail s))]
   [else s]))

(define (stream-nth n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) a))
  (doc 'description "Get the nth element (0-indexed)")
  (stream-head (stream-drop n s)))

(define (stream-scan f init s)
  (doc 'export #t)
  (doc 'type '(-> (-> b a b) b (Stream a) (Stream b)))
  (doc 'description "Running fold over a stream; produces stream of intermediate results")
  (stream-cons init
               (lambda ()
                       (if (stream-nil? s)
                           stream-nil
                           (stream-scan f (f init (stream-head s)) (stream-tail s))))))

(define (stream-concat s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream a) (Stream a)))
  (doc 'description "Concatenate two streams; second accessed only when first ends")
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-concat (stream-tail s1) s2)))))

(define (stream-flatten ss)
  (doc 'export #t)
  (doc 'type '(-> (Stream (Stream a)) (Stream a)))
  (doc 'description "Flatten a stream of streams")
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

(define (stream-flatmap f s)
  (doc 'export #t)
  (doc 'type '(-> (-> a (Stream b)) (Stream a) (Stream b)))
  (doc 'description "Map and flatten (monadic bind via concatenation)")
  (stream-flatten (stream-map f s)))

(doc 'section 'combinators)

(define (stream-zip s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream b) (Stream (Pair a b))))
  (doc 'description "Zip two streams into a stream of pairs")
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (cons (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip (stream-tail s1) (stream-tail s2))))))

(define (stream-zip-with f s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (-> a b c) (Stream a) (Stream b) (Stream c)))
  (doc 'description "Zip with a combining function")
  (if (or (stream-nil? s1) (stream-nil? s2))
      stream-nil
      (stream-cons (f (stream-head s1) (stream-head s2))
                   (lambda () (stream-zip-with f (stream-tail s1) (stream-tail s2))))))

(define (stream-interleave s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream a) (Stream a)))
  (doc 'description "Interleave two streams: a1, b1, a2, b2, ...")
  (if (stream-nil? s1)
      s2
      (stream-cons (stream-head s1)
                   (lambda () (stream-interleave s2 (stream-tail s1))))))

(define (stream-merge pred s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (-> a a Boolean) (Stream a) (Stream a) (Stream a)))
  (doc 'description "Merge two sorted streams maintaining order by predicate")
  (cond
   [(stream-nil? s1) s2]
   [(stream-nil? s2) s1]
   [else
    (let ([h1 (stream-head s1)]
          [h2 (stream-head s2)])
         (if (pred h1 h2)
             (stream-cons h1 (lambda () (stream-merge pred (stream-tail s1) s2)))
             (stream-cons h2 (lambda () (stream-merge pred s1 (stream-tail s2))))))]))

(doc 'section 'folds)

(define (stream-fold f init n s)
  (doc 'export #t)
  (doc 'type '(-> (-> b a b) b Nat (Stream a) b))
  (doc 'description "Left fold over first n elements of stream")
  (if (or (<= n 0) (stream-nil? s))
      init
      (stream-fold f (f init (stream-head s)) (- n 1) (stream-tail s))))

(define (stream-any pred n s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) Nat (Stream a) Boolean))
  (doc 'description "Check if any of first n elements satisfies predicate")
  (cond
   [(<= n 0) #f]
   [(stream-nil? s) #f]
   [(pred (stream-head s)) #t]
   [else (stream-any pred (- n 1) (stream-tail s))]))

(define (stream-all pred n s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) Nat (Stream a) Boolean))
  (doc 'description "Check if all of first n elements satisfy predicate")
  (cond
   [(<= n 0) #t]
   [(stream-nil? s) #t]
   [(not (pred (stream-head s))) #f]
   [else (stream-all pred (- n 1) (stream-tail s))]))

(define (stream-find pred n s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) Nat (Stream a) (Maybe a)))
  (doc 'description "Find first element matching predicate within n elements")
  (cond
   [(<= n 0) nothing]
   [(stream-nil? s) nothing]
   [(pred (stream-head s)) (just (stream-head s))]
   [else (stream-find pred (- n 1) (stream-tail s))]))

(doc 'section 'memoized-streams)
;;; Memoized streams cache computed values to avoid recomputation.

(define (memo-stream-cons head tail-thunk)
  (doc 'export #t)
  (doc 'type '(-> a (-> (Stream a)) (Stream a)))
  (doc 'description "Create a memoized stream cons cell; tail is computed at most once")
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

(define (memo-stream? s)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a memoized stream cons cell")
  (and (pair? s) (eq? (car s) 'memo-stream-cons)))

(define (stream-force n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream a)))
  (doc 'description "Force computation to depth n, returning the original stream")
  (stream-force-helper n s)
  s)

(define (stream-force-helper n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) Void))
  (doc 'description "Internal: eagerly force stream to depth n")
  (if (or (<= n 0) (stream-nil? s))
      (void)
      (begin
       (stream-head s)
       (stream-force-helper (- n 1) (stream-tail s)))))

(doc 'section 'classic-streams)

(define fibonacci
  (letrec ([fibs (lambda (a b)
                         (stream-cons a (lambda () (fibs b (+ a b)))))])
          (fibs 0 1)))
(doc fibonacci 'type '(Stream Int))
(doc fibonacci 'description "The Fibonacci sequence: 0, 1, 1, 2, 3, 5, 8, ...")

(define primes
  (letrec ([sieve (lambda (s)
                          (let ([p (stream-head s)])
                               (stream-cons p
                                            (lambda ()
                                                    (sieve (stream-filter
                                                            (lambda (n) (not (= 0 (modulo n p))))
                                                            (stream-tail s)))))))])
          (sieve (stream-from 2))))
(doc primes 'type '(Stream Int))
(doc primes 'description "Prime numbers via sieve of Eratosthenes")

(define (powers-of n)
  (doc 'export #t)
  (doc 'type '(-> Int (Stream Int)))
  (doc 'description "Powers of n: 1, n, n^2, n^3, ...")
  (stream-iterate (lambda (x) (* x n)) 1))

(define factorials
  (stream-scan * 1 (stream-from 1)))
(doc factorials 'type '(Stream Int))
(doc factorials 'description "Factorial sequence: 1, 1, 2, 6, 24, 120, ...")

(define triangular
  (stream-scan + 0 (stream-from 1)))
(doc triangular 'type '(Stream Int))
(doc triangular 'description "Triangular numbers: 0, 1, 3, 6, 10, 15, ...")

(doc 'section 'generator-pattern)

(define (make-generator gen)
  (doc 'export #t)
  (doc 'type '(-> (-> (Maybe a)) (Stream a)))
  (doc 'description "Create stream from a generator; generator returns nothing when exhausted")
  (let ([result (gen)])
       (if (nothing? result)
           stream-nil
           (stream-cons (from-just result)
                        (lambda () (make-generator gen))))))

(define (counter-generator start end)
  (doc 'export #t)
  (doc 'type '(-> Int Int (-> (Maybe Int))))
  (doc 'description "Create a stateful generator counting from start up to (not including) end")
  (let ([current start])
       (lambda ()
               (if (>= current end)
                   nothing
                   (let ([val current])
                        (set! current (+ current 1))
                        (just val))))))

(define (random-stream seed mult mod)
  (doc 'export #t)
  (doc 'type '(-> Int Int Int (Stream Int)))
  (doc 'description "Pseudo-random stream via linear congruential generator")
  (stream-iterate (lambda (x) (modulo (* x mult) mod)) seed))

(doc 'section 'utilities)

(define (stream-partition pred s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Boolean) (Stream a) (Pair (Stream a) (Stream a))))
  (doc 'description "Split stream into passing and failing sub-streams")
  (cons (stream-filter pred s)
        (stream-filter (lambda (x) (not (pred x))) s)))

(define (stream-group n s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream (List a))))
  (doc 'description "Group elements into chunks of size n")
  (if (stream-nil? s)
      stream-nil
      (stream-cons (stream->list n s)
                   (lambda () (stream-group n (stream-drop n s))))))

(define (stream-distinct s)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream a)))
  (doc 'description "Remove consecutive duplicates")
  (if (stream-nil? s)
      stream-nil
      (stream-distinct-helper (stream-head s) (stream-tail s))))

(define (stream-distinct-helper prev s)
  (doc 'export #t)
  (doc 'type '(-> a (Stream a) (Stream a)))
  (doc 'description "Internal helper for stream-distinct")
  (if (stream-nil? s)
      (stream-cons prev (lambda () stream-nil))
      (let ([h (stream-head s)])
           (if (equal? h prev)
               (stream-distinct-helper prev (stream-tail s))
               (stream-cons prev (lambda () (stream-distinct-helper h (stream-tail s))))))))

(define (stream-enumerate s)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream (Pair Int a))))
  (doc 'description "Pair each element with its 0-based index")
  (stream-zip naturals s))

(doc 'section 'delay-force)
;;; Explicit delay/force with memoization for lazy evaluation.

(define (delay thunk)
  (doc 'export #t)
  (doc 'type '(-> (-> a) (Delayed a)))
  (doc 'description "Create a delayed computation; memoized on first force")
  (list 'delayed thunk #f #f))

(define (force delayed)
  (doc 'export #t)
  (doc 'type '(-> (Delayed a) a))
  (doc 'description "Force evaluation of a delayed computation; caches result")
  (if (and (pair? delayed) (eq? (car delayed) 'delayed))
      (if (caddr delayed)
          (cadddr delayed)
          (let ([val ((cadr delayed))])
               (set-car! (cddr delayed) #t)
               (set-car! (cdddr delayed) val)
               val))
      (error 'force "not a delayed value")))

(define (delayed? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a delayed computation")
  (and (pair? x) (eq? (car x) 'delayed)))

(doc 'section 'type-class-instances)
;;; Type Class Instances (Dictionary-Passing Style)
;;;
;;; Following The Fold's convention, type classes are represented
;;; as dictionaries (records with operations). This enables
;;; polymorphic code without Haskell-style implicit instances.

;;; Functor Instance for Stream
;;;
;;; Functor laws:
;;;   1. fmap id = id (identity)
;;;   2. fmap (f . g) = fmap f . fmap g (composition)
;;;
;;; Note: stream-map already satisfies the Functor interface.
;;; We provide an explicit dictionary for consistency with the
;;; type class system.

(doc stream-functor 'type '(FunctorDict Stream))
(doc stream-functor 'description "Functor dictionary for Stream; wraps stream-map as fmap")
(define stream-functor
  (list 'functor stream-map))

(define (functor-fmap functor)
  (doc 'export #t)
  (doc 'type '(-> (FunctorDict f) (-> a b) (f a) (f b)))
  (doc 'description "Extract fmap from a Functor dictionary")
  (cadr functor))

(doc stream-fmap 'type '(-> (-> a b) (Stream a) (Stream b)))
(doc stream-fmap 'description "Alias for stream-map, emphasizing the Functor interface")
(define stream-fmap stream-map)

;;; Applicative Instance for Stream
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

(doc stream-pure 'type '(-> a (Stream a)))
(doc stream-pure 'description "Lift a value into an infinite stream (ZipList-style pure)")
(define stream-pure stream-repeat)

(define (stream-ap fs xs)
  (doc 'export #t)
  (doc 'type '(-> (Stream (-> a b)) (Stream a) (Stream b)))
  (doc 'description "Apply a stream of functions pointwise to a stream of values (ZipList <*>)")
  (stream-zip-with (lambda (f x) (f x)) fs xs))

(doc stream-applicative 'type '(ApplicativeDict Stream))
(doc stream-applicative 'description "Applicative dictionary for Stream; ZipList semantics")
(define stream-applicative
  (list 'applicative
        stream-functor     ; parent Functor
        stream-pure        ; pure
        stream-ap))        ; <*>

(define (applicative-pure app)
  (doc 'export #t)
  (doc 'type '(-> (ApplicativeDict f) (-> a (f a))))
  (doc 'description "Extract pure function from Applicative dictionary")
  (caddr app))

(define (applicative-ap app)
  (doc 'export #t)
  (doc 'type '(-> (ApplicativeDict f) (-> (f (-> a b)) (f a) (f b))))
  (doc 'description "Extract ap function from Applicative dictionary")
  (cadddr app))

(define (stream-lift2 f xs ys)
  (doc 'export #t)
  (doc 'type '(-> (-> a b c) (Stream a) (Stream b) (Stream c)))
  (doc 'description "Lift a binary function to operate on two streams pointwise")
  (stream-ap (stream-map (lambda (x) (lambda (y) (f x y))) xs) ys))

(define (stream-lift3 f xs ys zs)
  (doc 'export #t)
  (doc 'type '(-> (-> a b c d) (Stream a) (Stream b) (Stream c) (Stream d)))
  (doc 'description "Lift a ternary function to operate on three streams pointwise")
  (stream-ap
   (stream-ap (stream-map (lambda (x) (lambda (y) (lambda (z) (f x y z)))) xs) ys)
   zs))

;;; Monad Instance for Stream
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

(doc stream-return 'type '(-> a (Stream a)))
(doc stream-return 'description "Monadic return; lifts a value into a repeating stream")
(define stream-return stream-repeat)

(define (stream-bind s f)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (-> a (Stream b)) (Stream b)))
  (doc 'description "Monadic bind via diagonal extraction; takes nth element from nth sub-stream")
  (stream-diagonal (stream-map f s)))

(define (stream-diagonal ss)
  (doc 'export #t)
  (doc 'type '(-> (Stream (Stream a)) (Stream a)))
  (doc 'description "Extract diagonal from stream of streams: 1st of 1st, 2nd of 2nd, etc.")
  (if (stream-nil? ss)
      stream-nil
      (let ([inner (stream-head ss)])
           (if (stream-nil? inner)
               (stream-diagonal (stream-map stream-tail (stream-tail ss)))
               (stream-cons (stream-head inner)
                            (lambda ()
                                    (stream-diagonal
                                     (stream-map stream-tail (stream-tail ss)))))))))

(doc stream-join 'type '(-> (Stream (Stream a)) (Stream a)))
(doc stream-join 'description "Flatten a stream of streams (monadic join); alias for stream-diagonal")
(define stream-join stream-diagonal)

(doc stream-monad 'type '(MonadDict Stream))
(doc stream-monad 'description "Monad dictionary for Stream; diagonal bind semantics")
(define stream-monad
  (list 'monad
        stream-applicative  ; parent Applicative
        stream-return       ; return
        stream-bind))       ; >>=

(define (monad-return monad)
  (doc 'export #t)
  (doc 'type '(-> (MonadDict m) (-> a (m a))))
  (doc 'description "Extract return function from Monad dictionary")
  (caddr monad))

(define (monad-bind monad)
  (doc 'export #t)
  (doc 'type '(-> (MonadDict m) (-> (m a) (-> a (m b)) (m b))))
  (doc 'description "Extract bind function from Monad dictionary")
  (cadddr monad))

(define (stream-then s1 s2)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream b) (Stream b)))
  (doc 'description "Sequence two streams, discarding first's values (monadic >>)")
  (stream-bind s1 (lambda (_) s2)))

(doc 'section 'codata)
;;; Codata and Coinductive Patterns
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

(define (coalgebra-step step seed)
  (doc 'export #t)
  (doc 'type '(-> (-> s (Pair a s)) s (Stream a)))
  (doc 'description "Build a stream from a coalgebra; step returns (value . next-state) pairs")
  (let ([pair (step seed)])
       (stream-cons (car pair)
                    (lambda () (coalgebra-step step (cdr pair))))))

(doc anamorphism 'type '(-> (-> s (Maybe (Pair a s))) s (Stream a)))
(doc anamorphism 'description "Anamorphism (unfold); alias for stream-unfold emphasizing the recursion scheme")
(define anamorphism stream-unfold)

(define (coiterate f seed)
  (doc 'export #t)
  (doc 'type '(-> (-> s s) s (Stream s)))
  (doc 'description "Coinductive iterate: produce stream of states by repeated application")
  (stream-cons seed (lambda () (coiterate f (f seed)))))

(define (bisimulation-equal? n s1 s2)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream a) Boolean))
  (doc 'description "Bounded bisimulation equality; checks streams agree up to depth n")
  (cond
   [(<= n 0) #t]
   [(and (stream-nil? s1) (stream-nil? s2)) #t]
   [(or (stream-nil? s1) (stream-nil? s2)) #f]
   [(not (equal? (stream-head s1) (stream-head s2))) #f]
   [else (bisimulation-equal? (- n 1) (stream-tail s1) (stream-tail s2))]))

(doc 'section 'infinite-generators)

(doc squares 'type '(Stream Int))
(doc squares 'description "Perfect squares: 0, 1, 4, 9, 16, 25, ...")
(define squares
  (stream-map (lambda (n) (* n n)) naturals))

(doc cubes 'type '(Stream Int))
(doc cubes 'description "Perfect cubes: 0, 1, 8, 27, 64, 125, ...")
(define cubes
  (stream-map (lambda (n) (* n n n)) naturals))

(doc evens 'type '(Stream Int))
(doc evens 'description "Even natural numbers: 0, 2, 4, 6, 8, ...")
(define evens
  (stream-filter even? naturals))

(doc odds 'type '(Stream Int))
(doc odds 'description "Odd natural numbers: 1, 3, 5, 7, 9, ...")
(define odds
  (stream-filter odd? naturals))

(doc 'section 'fuel-operations)
;;; Fuel-Based Operations (For Termination Safety)
;;;
;;; Operations on potentially infinite streams need fuel limits
;;; to guarantee termination. These complement stream-fold.

(define (stream-for-each/fuel f fuel s)
  (doc 'export #t)
  (doc 'type '(-> (-> a Void) Nat (Stream a) Void))
  (doc 'description "Apply effectful operation to each element up to fuel limit")
  (when (and (> fuel 0) (not (stream-nil? s)))
        (f (stream-head s))
        (stream-for-each/fuel f (- fuel 1) (stream-tail s))))

(define (stream-length/fuel fuel s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) Nat))
  (doc 'description "Count stream elements up to fuel limit; returns fuel if stream is longer")
  (let loop ([n 0] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) n]
        [(stream-nil? s) n]
        [else (loop (+ n 1) (- fuel 1) (stream-tail s))])))

(define (stream-reverse/fuel fuel s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Stream a)))
  (doc 'description "Reverse the first fuel elements of a stream")
  (list->stream (reverse (stream->list fuel s))))

(define (stream-last/fuel fuel s)
  (doc 'export #t)
  (doc 'type '(-> Nat (Stream a) (Maybe a)))
  (doc 'description "Get the last element within fuel elements; returns nothing if empty")
  (let loop ([last nothing] [fuel fuel] [s s])
       (cond
        [(<= fuel 0) last]
        [(stream-nil? s) last]
        [else (loop (just (stream-head s)) (- fuel 1) (stream-tail s))])))

(doc 'section 'stream-comprehensions)
;;; Stream Comprehensions (via Monad)
;;;
;;; Using the monad interface, we can express list-comprehension
;;; style patterns for streams.

(define (stream-guard condition)
  (doc 'export #t)
  (doc 'type '(-> Boolean (Stream Unit)))
  (doc 'description "Filter guard for stream comprehensions; singleton if true, empty if false")
  (if condition
      (stream-cons '() (lambda () stream-nil))
      stream-nil))

(define (stream-cartesian xs ys)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (Stream b) (Stream (Pair a b))))
  (doc 'description "Fair Cartesian product via Cantor diagonal; every pair (i,j) eventually appears")
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

(define (stream-append s1 thunk)
  (doc 'export #t)
  (doc 'type '(-> (Stream a) (-> (Stream a)) (Stream a)))
  (doc 'description "Append a stream with a lazily-evaluated second stream")
  (if (stream-nil? s1)
      (thunk)
      (stream-cons (stream-head s1)
                   (lambda () (stream-append (stream-tail s1) thunk)))))
