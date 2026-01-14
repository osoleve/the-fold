;;; fabric/stitches/fp/combinators.ss — Practical FP Combinators
;;;
;;; Higher-order function utilities and combinators for functional
;;; programming patterns without requiring full type class machinery.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Function composition (compose, pipe)
;;;   - Currying and partial application
;;;   - Common combinators (id, const, flip, on)
;;;   - Tuple operations
;;;   - Function lifting and application
;;;   - Logical combinators
;;;   - Maybe/Option operations
;;;   - Either/Result operations
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Pair Convention
;;; ====
;;;
;;; Throughout this module, 2-element pairs use Scheme's native cons cells:
;;;   (cons a b) produces (a . b)
;;;   car extracts the first element
;;;   cdr extracts the second element
;;;
;;; For 3+ element tuples, we use lists: (list a b c)
;;;
;;; This is consistent with the rest of the FP toolkit.

;;; ====
;;; Classic Combinators (SKI + friends)
;;; ====

;;; id : a → a
;;; Identity function (I combinator).
;;; Note: `identity` is also available from prelude.ss
(define (id x) x)

;;; identity : a → a
;;; Alias for id, for consistency with other modules.
(define identity id)

;;; const : a → b → a
;;; Constant function (K combinator).
(define (const x)
  (lambda (y) x))

;;; flip : (a → b → c) → (b → a → c)
;;; Swap first two arguments.
(define (flip f)
  (lambda (x y) (f y x)))

;;; apply : (a → b) → a → b
;;; Apply function to argument.
(define (apply-fn f x) (f x))

;;; ====
;;; Function Composition
;;; ====

;;; compose2 : (b → c) → (a → b) → (a → c)
;;; Compose two functions (g after f).
(define (compose2 g f)
  (lambda (x) (g (f x))))

;;; compose : (b → c) × (a → b) × ... → (a → result)
;;; Compose multiple functions right-to-left.
;;; (compose g f) = g ∘ f = λx. g(f(x))
(define (compose . fns)
  (if (null? fns)
      id
      (fold-right compose2 id fns)))

;;; pipe2 : (a → b) → (b → c) → (a → c)
;;; Compose two functions (f then g) - pipe order.
(define (pipe2 f g)
  (lambda (x) (g (f x))))

;;; pipe : (a → b) × (b → c) × ... → (a → result)
;;; Compose multiple functions left-to-right (pipeline order).
;;; (pipe f g h) = λx. h(g(f(x)))
(define (pipe . fns)
  (if (null? fns)
      id
      (fold-left pipe2 id fns)))

;;; >>> : Alias for pipe (threading operator)
(define >>> pipe)

;;; <<< : Alias for compose
(define <<< compose)

;;; ====
;;; Currying and Partial Application
;;; ====

;;; curry2 : ((a × b) → c) → a → b → c
;;; Curry a binary function.
(define (curry2 f)
  (lambda (x)
          (lambda (y)
                  (f x y))))

;;; uncurry2 : (a → b → c) → ((a × b) → c)
;;; Uncurry a curried binary function.
;;; Note: pairs are (cons a b), so cdr gets second element.
(define (uncurry2 f)
  (lambda (xy)
          ((f (car xy)) (cdr xy))))

;;; curry3 : ((a × b × c) → d) → a → b → c → d
;;; Curry a ternary function.
(define (curry3 f)
  (lambda (x)
          (lambda (y)
                  (lambda (z)
                          (f x y z)))))

;;; uncurry3 : (a → b → c → d) → ((a × b × c) → d)
;;; Uncurry a curried ternary function.
(define (uncurry3 f)
  (lambda (xyz)
          (((f (car xyz)) (cadr xyz)) (caddr xyz))))

;;; partial : (a → b → ... → z) → a → (b → ... → z)
;;; Partially apply first argument.
(define (partial f x)
  (lambda args (apply f (cons x args))))

;;; partial2 : (a → b → c → ... → z) → a → b → (c → ... → z)
;;; Partially apply first two arguments.
(define (partial2 f x y)
  (lambda args (apply f (cons x (cons y args)))))

;;; rpartial : (a → b → ... → z) → z → (a → b → ... → result)
;;; Partially apply last argument.
(define (rpartial f z)
  (lambda args (apply f (append args (list z)))))

;;; ====
;;; Higher-Order Function Utilities
;;; ====

;;; on : (b → b → c) → (a → b) → (a → a → c)
;;; Apply transformation before binary operation.
;;; (on compare length) compares lists by their lengths.
(define (on f g)
  (lambda (x y) (f (g x) (g y))))

;;; both : (a → b) → (a × a) → (b × b)
;;; Apply function to both elements of a pair.
;;; Note: pairs are (cons a b), returns (cons (f a) (f b)).
(define (both f)
  (lambda (pair)
          (cons (f (car pair)) (f (cdr pair)))))

;;; pair-first : (a → b) → (a × c) → (b × c)
;;; Apply function to first element of pair.
(define (pair-first f)
  (lambda (pair)
          (cons (f (car pair)) (cdr pair))))

;;; pair-second : (b → c) → (a × b) → (a × c)
;;; Apply function to second element of pair.
;;; Note: pairs are (cons a b), returns (cons a (f b)).
(define (pair-second f)
  (lambda (pair)
          (cons (car pair) (f (cdr pair)))))


;;; bimap : (a → c) → (b → d) → (a × b) → (c × d)
;;; Apply two functions to elements of a pair.
;;; Note: pairs are (cons a b), returns (cons (f a) (g b)).
(define (bimap f g)
  (lambda (pair)
          (cons (f (car pair)) (g (cdr pair)))))

;;; ====
;;; Logical Combinators
;;; ====

;;; complement : (a → Bool) → (a → Bool)
;;; Negate a predicate.
(define (complement pred)
  (lambda (x) (not (pred x))))

;;; conjoin : ((a → Bool) ...) → (a → Bool)
;;; Combine predicates with AND.
(define (conjoin . preds)
  (lambda (x)
          (let loop ([ps preds])
               (or (null? ps)
                   (and ((car ps) x)
                        (loop (cdr ps)))))))

;;; disjoin : ((a → Bool) ...) → (a → Bool)
;;; Combine predicates with OR.
(define (disjoin . preds)
  (lambda (x)
          (let loop ([ps preds])
               (and (not (null? ps))
                    (or ((car ps) x)
                        (loop (cdr ps)))))))

;;; ====
;;; Tuple/List Operations
;;; ====

;;; fst : (a × b) → a
;;; Note: pairs are (cons a b).
(define fst car)

;;; snd : (a × b) → b
;;; Note: pairs are (cons a b).
(define snd cdr)

;;; swap : (a × b) → (b × a)
;;; Note: pairs are (cons a b).
(define (swap pair)
  (cons (cdr pair) (car pair)))

;;; dup : a → (a × a)
;;; Note: returns (cons x x).
(define (dup x)
  (cons x x))

;;; pair : a → b → (a × b)
;;; Note: returns (cons x y).
(define (pair x y)
  (cons x y))

;;; ====
;;; Maybe/Option Type Operations
;;; ====

;;; Representation: #f for Nothing, (just value) for Just

;;; nothing : Maybe a
(define nothing #f)

;;; just : a → Maybe a
(define (just x)
  (list 'just x))

;;; just? : Maybe a → Boolean
(define (just? m)
  (and (pair? m) (eq? (car m) 'just)))

;;; nothing? : Maybe a → Boolean
(define nothing? not)

;;; from-just : Maybe a → a
;;; Extract value (undefined for Nothing).
(define (from-just m)
  (cadr m))

;;; maybe : b → (a → b) → Maybe a → b
;;; Maybe eliminator.
(define (maybe default f m)
  (if (just? m)
      (f (from-just m))
      default))

;;; maybe-fmap : (a → b) → Maybe a → Maybe b
;;; Functor map for Maybe.
(define (maybe-fmap f m)
  (if (just? m)
      (just (f (from-just m)))
      nothing))

;;; maybe-bind : Maybe a → (a → Maybe b) → Maybe b
;;; Monadic bind for Maybe.
(define (maybe-bind m f)
  (if (just? m)
      (f (from-just m))
      nothing))


;;; filter-maybe : (a → Boolean) → Maybe a → Maybe a
;;; Filter a Maybe value.
(define (filter-maybe pred m)
  (if (and (just? m) (pred (from-just m)))
      m
      nothing))

;;; cat-maybes : (List (Maybe a)) → (List a)
;;; Extract all Just values from a list.
(define (cat-maybes ms)
  (fold-right (lambda (m acc)
                      (if (just? m)
                          (cons (from-just m) acc)
                          acc))
              '()
              ms))

;;; sequence-maybe : (List (Maybe a)) → Maybe (List a)
;;; Collect list of Maybes into Maybe of list.
(define (sequence-maybe ms)
  (if (null? ms)
      (just '())
      (maybe-bind (car ms)
                  (lambda (x)
                          (maybe-bind (sequence-maybe (cdr ms))
                                      (lambda (xs)
                                              (just (cons x xs))))))))

;;; ====
;;; Either/Result Type Operations
;;; ====

;;; Representation: (left value) or (right value)

;;; left : a → Either a b
(define (left x)
  (list 'left x))

;;; right : b → Either a b
(define (right x)
  (list 'right x))

;;; left? : Either a b → Boolean
(define (left? e)
  (and (pair? e) (eq? (car e) 'left)))

;;; right? : Either a b → Boolean
(define (right? e)
  (and (pair? e) (eq? (car e) 'right)))

;;; from-left : Either a b → a
(define (from-left e)
  (cadr e))

;;; from-right : Either a b → b
(define (from-right e)
  (cadr e))

;;; either : (a → c) → (b → c) → Either a b → c
;;; Either eliminator.
(define (either f g e)
  (if (left? e)
      (f (from-left e))
      (g (from-right e))))

;;; either-fmap : (b → c) → Either a b → Either a c
;;; Functor map for Either (standard: operates on Right).
(define (either-fmap f e)
  (if (right? e)
      (right (f (from-right e)))
      e))

;;; either-fmap-left : (a → c) → Either a b → Either c b
;;; Map over Left side of Either.
(define (either-fmap-left f e)
  (if (left? e)
      (left (f (from-left e)))
      e))

;;; either-bind : Either a b → (b → Either a c) → Either a c
;;; Monadic bind for Either (standard: operates on Right).
(define (either-bind e f)
  (if (right? e)
      (f (from-right e))
      e))

;;; from-either : (a → c) → (b → c) → Either a b → c
;;; Alias for either.
(define from-either either)

;;; rights : (List (Either a b)) → (List b)
;;; Extract all Right values.
(define (rights es)
  (fold-right (lambda (e acc)
                      (if (right? e)
                          (cons (from-right e) acc)
                          acc))
              '()
              es))

;;; lefts : (List (Either a b)) → (List a)
;;; Extract all Left values.
(define (lefts es)
  (fold-right (lambda (e acc)
                      (if (left? e)
                          (cons (from-left e) acc)
                          acc))
              '()
              es))

;;; partition-eithers : (List (Either a b)) → ((List a) × (List b))
;;; Partition eithers into lefts and rights.
(define (partition-eithers es)
  (fold-right (lambda (e acc)
                      (if (left? e)
                          (list (cons (from-left e) (car acc)) (cadr acc))
                          (list (car acc) (cons (from-right e) (cadr acc)))))
              '(() ())
              es))

;;; ====
;;; List Utilities
;;; ====

;;; head : (List a) → a
(define head car)

;;; tail : (List a) → (List a)
(define tail cdr)

;;; cons-if : Boolean → a → (List a) → (List a)
;;; Conditionally cons an element.
(define (cons-if pred x xs)
  (if pred (cons x xs) xs))

;;; intersperse : a → (List a) → (List a)
;;; Place element between each pair of elements.
(define (intersperse sep xs)
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (cons (car xs)
            (fold-right (lambda (x acc) (cons sep (cons x acc)))
                        '()
                        (cdr xs)))))

;;; intercalate : (List a) → (List (List a)) → (List a)
;;; Insert list between lists and flatten.
(define (intercalate sep xss)
  (apply append (intersperse sep xss)))

;;; group-by : (a → b) → (List a) → (List (List a))
;;; Group consecutive elements with same key.
(define (group-by key xs)
  (if (null? xs)
      '()
      (let loop ([xs (cdr xs)]
                 [current-key (key (car xs))]
                 [current-group (list (car xs))]
                 [groups '()])
           (if (null? xs)
               (reverse (cons (reverse current-group) groups))
               (let ([k (key (car xs))])
                    (if (equal? k current-key)
                        (loop (cdr xs) current-key (cons (car xs) current-group) groups)
                        (loop (cdr xs) k (list (car xs)) (cons (reverse current-group) groups))))))))

;;; sort-by : (a → b) → (List a) → (List a)
;;; Sort by key function (stable).
(define (sort-by key xs)
  (list-sort (lambda (a b) (< (key a) (key b))) xs))

;;; dedup-consecutive : (List a) → (List a)
;;; Remove consecutive duplicates (like Unix uniq).
;;; For general duplicate removal, use prelude's unique.
(define (dedup-consecutive xs)
  (if (null? xs)
      '()
      (let loop ([xs (cdr xs)] [prev (car xs)] [result (list (car xs))])
           (if (null? xs)
               (reverse result)
               (if (equal? (car xs) prev)
                   (loop (cdr xs) prev result)
                   (loop (cdr xs) (car xs) (cons (car xs) result)))))))

;;; dedup-consecutive-by : (a → b) → (List a) → (List a)
;;; Remove consecutive elements with same key.
;;; For general key-based deduplication, use prelude's distinct-by.
(define (dedup-consecutive-by key xs)
  (if (null? xs)
      '()
      (let loop ([xs (cdr xs)] [prev-key (key (car xs))] [result (list (car xs))])
           (if (null? xs)
               (reverse result)
               (let ([k (key (car xs))])
                    (if (equal? k prev-key)
                        (loop (cdr xs) prev-key result)
                        (loop (cdr xs) k (cons (car xs) result))))))))

;;; ====
;;; Iteration Combinators
;;; ====

;;; iterate-n : (α → α) × α × Nat → (List α)
;;; Generate list by repeated function application.
;;; Returns first n elements.
(define (iterate-n f x n)
  (let loop ([i 0] [x x] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (f x) (cons x acc)))))

;;; unfold : (β → (Maybe (α × β))) × β → (List α)
;;; Build list from seed using generator.
(define (unfold f seed)
  (let loop ([s seed] [acc '()])
       (let ([result (f s)])
            (if (nothing? result)
                (reverse acc)
                (let ([val (car (from-just result))]
                      [next (cadr (from-just result))])
                     (loop next (cons val acc)))))))

;;; fix-with-tolerance : (α → α) × α × α × Nat → α
;;; Find fixed point of function (within tolerance).
(define (fix-with-tolerance f x tolerance max-iters)
  (let loop ([x x] [i 0])
       (let ([x-next (f x)])
            (if (or (>= i max-iters)
                    (< (abs (- x-next x)) tolerance))
                x-next
                (loop x-next (+ i 1))))))

;;; ====
;;; Memoization
;;; ====

;;; memoize : (α → β) → (α → β)
;;; Memoize a function (using eq? hash).
(define (memoize f)
  (let ([cache (make-hashtable equal-hash equal?)])
       (lambda (x)
               (let ([cached (hashtable-ref cache x 'not-found)])
                    (if (eq? cached 'not-found)
                        (let ([result (f x)])
                             (hashtable-set! cache x result)
                             result)
                        cached)))))

;;; ====
;;; Fixpoint Combinator
;;; ====

;;; Y : ((α → β) → (α → β)) → (α → β)
;;; Y combinator (for defining recursive anonymous functions).
(define (Y f)
  ((lambda (x) (f (lambda (v) ((x x) v))))
   (lambda (x) (f (lambda (v) ((x x) v))))))

;;; ====
;;; Monadic Do-Notation
;;; ====

;;; do-monad : Macro for monadic computation
;;;
;;; Transforms:
;;;   (do-monad bind
;;;     [x <- mx]
;;;     [y <- (f x)]
;;;     (pure (+ x y)))
;;;
;;; Into:
;;;   (bind mx (lambda (x)
;;;     (bind (f x) (lambda (y)
;;;       (pure (+ x y))))))
;;;
;;; Syntax:
;;;   (do-monad bind clause ... final-expr)
;;;   where clause is either:
;;;     [var <- monadic-expr]   ; bind the result
;;;     monadic-expr            ; sequence (discard result)

(define-syntax do-monad
  (syntax-rules (<-)
                ;; Base case: just the final expression
                [(_ bind expr)
                 expr]
                ;; Binding form: [var <- mexpr]
                [(_ bind [var <- mexpr] rest ...)
                 (bind mexpr (lambda (var) (do-monad bind rest ...)))]
                ;; Sequencing form: mexpr (no binding)
                [(_ bind mexpr rest ...)
                 (bind mexpr (lambda (_) (do-monad bind rest ...)))]))

;;; do-monad* : Variant that passes pure along with bind
;;; Useful when you need pure inside the computation.
;;;
;;; (do-monad* bind pure
;;;   [x <- mx]
;;;   (pure x))

(define-syntax do-monad*
  (syntax-rules (<-)
                [(_ bind pure expr)
                 expr]
                [(_ bind pure [var <- mexpr] rest ...)
                 (bind mexpr (lambda (var) (do-monad* bind pure rest ...)))]
                [(_ bind pure mexpr rest ...)
                 (bind mexpr (lambda (_) (do-monad* bind pure rest ...)))]))
