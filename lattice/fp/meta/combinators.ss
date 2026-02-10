;;; lattice/fp/meta/combinators.ss — FP Combinators
;;; @module combinators
;;; @requires prelude sort

(require 'prelude)
(require 'sort)

(doc 'module 'combinators)
(doc 'description "Practical FP combinators — Higher-order function utilities and combinators for functional programming patterns without requiring full type class machinery. Features: function composition (compose, pipe), currying and partial application, common combinators (id, const, flip, on), tuple operations, function lifting and application, logical combinators, Maybe/Option operations, Either/Result operations.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'pair-convention)
(doc 'note "Throughout this module, 2-element pairs use Scheme's native cons cells: (cons a b) produces (a . b), car extracts the first element, cdr extracts the second element. For 3+ element tuples, we use lists: (list a b c). This is consistent with the rest of the FP toolkit.")

(doc 'section 'classic-combinators)

(define (id x)
  (doc 'type '(-> a a))
  (doc 'description "Identity function (I combinator)")
  (doc 'note "`identity` is also available from prelude.ss")
  x)

(define identity id)
(doc identity 'type '(-> a a))
(doc identity 'description "Alias for id, for consistency with other modules")

(define (const x)
  (doc 'type '(-> a (-> b a)))
  (doc 'description "Constant function (K combinator)")
  (lambda (y) x))

(define (flip f)
  (doc 'type '(-> (-> a b c) (-> b a c)))
  (doc 'description "Swap first two arguments")
  (lambda (x y) (f y x)))

(define (apply-fn f x)
  (doc 'type '(-> (-> a b) a b))
  (doc 'description "Apply function to argument")
  (f x))

(doc 'section 'function-composition)

(define (compose2 g f)
  (doc 'type '(-> (-> b c) (-> a b) (-> a c)))
  (doc 'description "Compose two functions (g after f)")
  (lambda (x) (g (f x))))

(define (compose . fns)
  (doc 'type '(-> (* (-> b c) (-> a b)) (-> a result)))
  (doc 'description "Compose multiple functions right-to-left")
  (doc 'note "(compose g f) = g ∘ f = λx. g(f(x))")
  (if (null? fns)
      id
      (fold-right compose2 id fns)))

(define (pipe2 f g)
  (doc 'type '(-> (-> a b) (-> b c) (-> a c)))
  (doc 'description "Compose two functions (f then g) - pipe order")
  (lambda (x) (g (f x))))

(define (pipe . fns)
  (doc 'type '(-> (* (-> a b) (-> b c)) (-> a result)))
  (doc 'description "Compose multiple functions left-to-right (pipeline order)")
  (doc 'note "(pipe f g h) = λx. h(g(f(x)))")
  (if (null? fns)
      id
      (fold-left pipe2 id fns)))

(define >>> pipe)
(doc >>> 'description "Alias for pipe (threading operator)")

(define <<< compose)
(doc <<< 'description "Alias for compose")

(doc 'section 'currying-and-partial-application)

(define (curry2 f)
  (doc 'type '(-> (-> (* a b) c) (-> a (-> b c))))
  (doc 'description "Curry a binary function")
  (lambda (x)
          (lambda (y)
                  (f x y))))

(define (uncurry2 f)
  (doc 'type '(-> (-> a (-> b c)) (-> (* a b) c)))
  (doc 'description "Uncurry a curried binary function")
  (doc 'note "pairs are (cons a b), so cdr gets second element")
  (lambda (xy)
          ((f (car xy)) (cdr xy))))

(define (curry3 f)
  (doc 'type '(-> (-> (* a b c) d) (-> a (-> b (-> c d)))))
  (doc 'description "Curry a ternary function")
  (lambda (x)
          (lambda (y)
                  (lambda (z)
                          (f x y z)))))

(define (uncurry3 f)
  (doc 'type '(-> (-> a (-> b (-> c d))) (-> (* a b c) d)))
  (doc 'description "Uncurry a curried ternary function")
  (lambda (xyz)
          (((f (car xyz)) (cadr xyz)) (caddr xyz))))

(define (partial f x)
  (doc 'type '(-> (-> a b (* rest) z) a (-> b (* rest) z)))
  (doc 'description "Partially apply first argument")
  (lambda args (apply f (cons x args))))

(define (partial2 f x y)
  (doc 'type '(-> (-> a b c (* rest) z) a b (-> c (* rest) z)))
  (doc 'description "Partially apply first two arguments")
  (lambda args (apply f (cons x (cons y args)))))

(define (rpartial f z)
  (doc 'type '(-> (-> a b (* rest) result) result (-> a b (* rest) result)))
  (doc 'description "Partially apply last argument")
  (lambda args (apply f (append args (list z)))))

(doc 'section 'higher-order-utilities)

(define (on f g)
  (doc 'type '(-> (-> b b c) (-> a b) (-> a a c)))
  (doc 'description "Apply transformation before binary operation")
  (doc 'note "(on compare length) compares lists by their lengths")
  (lambda (x y) (f (g x) (g y))))

(define (both f)
  (doc 'type '(-> (-> a b) (-> (* a a) (* b b))))
  (doc 'description "Apply function to both elements of a pair")
  (doc 'note "pairs are (cons a b), returns (cons (f a) (f b))")
  (lambda (pair)
          (cons (f (car pair)) (f (cdr pair)))))

(define (pair-first f)
  (doc 'type '(-> (-> a b) (-> (* a c) (* b c))))
  (doc 'description "Apply function to first element of pair")
  (lambda (pair)
          (cons (f (car pair)) (cdr pair))))

(define (pair-second f)
  (doc 'type '(-> (-> b c) (-> (* a b) (* a c))))
  (doc 'description "Apply function to second element of pair")
  (doc 'note "pairs are (cons a b), returns (cons a (f b))")
  (lambda (pair)
          (cons (car pair) (f (cdr pair)))))


(define (bimap f g)
  (doc 'type '(-> (-> a c) (-> b d) (-> (* a b) (* c d))))
  (doc 'description "Apply two functions to elements of a pair")
  (doc 'note "pairs are (cons a b), returns (cons (f a) (g b))")
  (lambda (pair)
          (cons (f (car pair)) (g (cdr pair)))))

(doc 'section 'logical-combinators)

(define (complement pred)
  (doc 'type '(-> (-> a Bool) (-> a Bool)))
  (doc 'description "Negate a predicate")
  (lambda (x) (not (pred x))))

(define (conjoin . preds)
  (doc 'type '(-> (* (-> a Bool)) (-> a Bool)))
  (doc 'description "Combine predicates with AND")
  (lambda (x)
          (let loop ([ps preds])
               (or (null? ps)
                   (and ((car ps) x)
                        (loop (cdr ps)))))))

(define (disjoin . preds)
  (doc 'type '(-> (* (-> a Bool)) (-> a Bool)))
  (doc 'description "Combine predicates with OR")
  (lambda (x)
          (let loop ([ps preds])
               (and (not (null? ps))
                    (or ((car ps) x)
                        (loop (cdr ps)))))))

(doc 'section 'tuple-operations)

(define fst car)
(doc fst 'type '(-> (* a b) a))
(doc fst 'note "pairs are (cons a b)")

(define snd cdr)
(doc snd 'type '(-> (* a b) b))
(doc snd 'note "pairs are (cons a b)")

(define (swap pair)
  (doc 'type '(-> (* a b) (* b a)))
  (doc 'note "pairs are (cons a b)")
  (cons (cdr pair) (car pair)))

(define (dup x)
  (doc 'type '(-> a (* a a)))
  (doc 'note "returns (cons x x)")
  (cons x x))

(define (pair x y)
  (doc 'type '(-> a b (* a b)))
  (doc 'note "returns (cons x y)")
  (cons x y))

(doc 'section 'maybe-operations)
(doc 'note "Representation: #f for Nothing, (just value) for Just")

(define nothing #f)
(doc nothing 'type 'Maybe-a)

(define (just x)
  (doc 'type '(-> a Maybe-a))
  (list 'just x))

(define (just? m)
  (doc 'type '(-> Maybe-a Boolean))
  (and (pair? m) (eq? (car m) 'just)))

(define nothing? not)
(doc nothing? 'type '(-> Maybe-a Boolean))

(define (from-just m)
  (doc 'type '(-> Maybe-a a))
  (doc 'description "Extract value (undefined for Nothing)")
  (cadr m))

(define (maybe default f m)
  (doc 'type '(-> b (-> a b) Maybe-a b))
  (doc 'description "Maybe eliminator")
  (if (just? m)
      (f (from-just m))
      default))

(define (maybe-fmap f m)
  (doc 'type '(-> (-> a b) Maybe-a Maybe-b))
  (doc 'description "Functor map for Maybe")
  (if (just? m)
      (just (f (from-just m)))
      nothing))

(define (maybe-bind m f)
  (doc 'type '(-> Maybe-a (-> a Maybe-b) Maybe-b))
  (doc 'description "Monadic bind for Maybe")
  (if (just? m)
      (f (from-just m))
      nothing))


(define (filter-maybe pred m)
  (doc 'type '(-> (-> a Boolean) Maybe-a Maybe-a))
  (doc 'description "Filter a Maybe value")
  (if (and (just? m) (pred (from-just m)))
      m
      nothing))

(define (cat-maybes ms)
  (doc 'type '(-> (List Maybe-a) (List a)))
  (doc 'description "Extract all Just values from a list")
  (fold-right (lambda (m acc)
                      (if (just? m)
                          (cons (from-just m) acc)
                          acc))
              '()
              ms))

(define (sequence-maybe ms)
  (doc 'type '(-> (List Maybe-a) Maybe-(List a)))
  (doc 'description "Collect list of Maybes into Maybe of list")
  (if (null? ms)
      (just '())
      (maybe-bind (car ms)
                  (lambda (x)
                          (maybe-bind (sequence-maybe (cdr ms))
                                      (lambda (xs)
                                              (just (cons x xs))))))))

(doc 'section 'either-operations)
(doc 'note "Representation: (left value) or (right value)")

(define (left x)
  (doc 'type '(-> a Either-a-b))
  (list 'left x))

(define (right x)
  (doc 'type '(-> b Either-a-b))
  (list 'right x))

(define (left? e)
  (doc 'type '(-> Either-a-b Boolean))
  (and (pair? e) (eq? (car e) 'left)))

(define (right? e)
  (doc 'type '(-> Either-a-b Boolean))
  (and (pair? e) (eq? (car e) 'right)))

(define (from-left e)
  (doc 'type '(-> Either-a-b a))
  (cadr e))

(define (from-right e)
  (doc 'type '(-> Either-a-b b))
  (cadr e))

(define (either f g e)
  (doc 'type '(-> (-> a c) (-> b c) Either-a-b c))
  (doc 'description "Either eliminator")
  (if (left? e)
      (f (from-left e))
      (g (from-right e))))

(define (either-fmap f e)
  (doc 'type '(-> (-> b c) Either-a-b Either-a-c))
  (doc 'description "Functor map for Either (standard: operates on Right)")
  (if (right? e)
      (right (f (from-right e)))
      e))

(define (either-fmap-left f e)
  (doc 'type '(-> (-> a c) Either-a-b Either-c-b))
  (doc 'description "Map over Left side of Either")
  (if (left? e)
      (left (f (from-left e)))
      e))

(define (either-bind e f)
  (doc 'type '(-> Either-a-b (-> b Either-a-c) Either-a-c))
  (doc 'description "Monadic bind for Either (standard: operates on Right)")
  (if (right? e)
      (f (from-right e))
      e))

(define from-either either)
(doc from-either 'description "Alias for either")

(define (rights es)
  (doc 'type '(-> (List Either-a-b) (List b)))
  (doc 'description "Extract all Right values")
  (fold-right (lambda (e acc)
                      (if (right? e)
                          (cons (from-right e) acc)
                          acc))
              '()
              es))

(define (lefts es)
  (doc 'type '(-> (List Either-a-b) (List a)))
  (doc 'description "Extract all Left values")
  (fold-right (lambda (e acc)
                      (if (left? e)
                          (cons (from-left e) acc)
                          acc))
              '()
              es))

(define (partition-eithers es)
  (doc 'type '(-> (List Either-a-b) (* (List a) (List b))))
  (doc 'description "Partition eithers into lefts and rights")
  (fold-right (lambda (e acc)
                      (if (left? e)
                          (list (cons (from-left e) (car acc)) (cadr acc))
                          (list (car acc) (cons (from-right e) (cadr acc)))))
              '(() ())
              es))

(doc 'section 'list-utilities)

(define head car)
(doc head 'type '(-> (List a) a))

(define tail cdr)
(doc tail 'type '(-> (List a) (List a)))

(define (cons-if pred x xs)
  (doc 'type '(-> Boolean a (List a) (List a)))
  (doc 'description "Conditionally cons an element")
  (if pred (cons x xs) xs))

(define (intersperse sep xs)
  (doc 'type '(-> a (List a) (List a)))
  (doc 'description "Place element between each pair of elements")
  (if (or (null? xs) (null? (cdr xs)))
      xs
      (cons (car xs)
            (fold-right (lambda (x acc) (cons sep (cons x acc)))
                        '()
                        (cdr xs)))))

(define (intercalate sep xss)
  (doc 'type '(-> (List a) (List (List a)) (List a)))
  (doc 'description "Insert list between lists and flatten")
  (flatten (intersperse sep xss)))

(define (group-by key xs)
  (doc 'type '(-> (-> a b) (List a) (List (List a))))
  (doc 'description "Group consecutive elements with same key")
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


(define (dedup-consecutive xs)
  (doc 'type '(-> (List a) (List a)))
  (doc 'description "Remove consecutive duplicates (like Unix uniq)")
  (doc 'note "For general duplicate removal, use prelude's unique")
  (if (null? xs)
      '()
      (let loop ([xs (cdr xs)] [prev (car xs)] [result (list (car xs))])
           (if (null? xs)
               (reverse result)
               (if (equal? (car xs) prev)
                   (loop (cdr xs) prev result)
                   (loop (cdr xs) (car xs) (cons (car xs) result)))))))

(define (dedup-consecutive-by key xs)
  (doc 'type '(-> (-> a b) (List a) (List a)))
  (doc 'description "Remove consecutive elements with same key")
  (doc 'note "For general key-based deduplication, use prelude's distinct-by")
  (if (null? xs)
      '()
      (let loop ([xs (cdr xs)] [prev-key (key (car xs))] [result (list (car xs))])
           (if (null? xs)
               (reverse result)
               (let ([k (key (car xs))])
                    (if (equal? k prev-key)
                        (loop (cdr xs) prev-key result)
                        (loop (cdr xs) k (cons (car xs) result))))))))

(doc 'section 'iteration-combinators)

(define (iterate-n f x n)
  (doc 'type '(-> (-> α α) α Nat (List α)))
  (doc 'description "Generate list by repeated function application")
  (doc 'returns "First n elements")
  (let loop ([i 0] [x x] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1) (f x) (cons x acc)))))

(define (unfold f seed)
  (doc 'type '(-> (-> β Maybe-(* α β)) β (List α)))
  (doc 'description "Build list from seed using generator")
  (let loop ([s seed] [acc '()])
       (let ([result (f s)])
            (if (nothing? result)
                (reverse acc)
                (let ([val (car (from-just result))]
                      [next (cadr (from-just result))])
                     (loop next (cons val acc)))))))

(define (fix-with-tolerance f x tolerance max-iters)
  (doc 'type '(-> (-> α α) α α Nat α))
  (doc 'description "Find fixed point of function (within tolerance)")
  (let loop ([x x] [i 0])
       (let ([x-next (f x)])
            (if (or (>= i max-iters)
                    (< (abs (- x-next x)) tolerance))
                x-next
                (loop x-next (+ i 1))))))

(doc 'section 'memoization)

(define (memoize f)
  (doc 'type '(-> (-> α β) (-> α β)))
  (doc 'description "Memoize a function (using eq? hash)")
  (let ([cache (make-hashtable equal-hash equal?)])
       (lambda (x)
               (let ([cached (hashtable-ref cache x 'not-found)])
                    (if (eq? cached 'not-found)
                        (let ([result (f x)])
                             (hashtable-set! cache x result)
                             result)
                        cached)))))

(doc 'section 'fixpoint-combinator)

(define (Y f)
  (doc 'type '(-> (-> (-> α β) (-> α β)) (-> α β)))
  (doc 'description "Y combinator (for defining recursive anonymous functions)")
  ((lambda (x) (f (lambda (v) ((x x) v))))
   (lambda (x) (f (lambda (v) ((x x) v))))))

(doc 'section 'monadic-do-notation)

(define-syntax do-monad
  (syntax-rules (<-)
                [(_ bind expr)
                 expr]
                [(_ bind [var <- mexpr] rest ...)
                 (bind mexpr (lambda (var) (do-monad bind rest ...)))]
                [(_ bind mexpr rest ...)
                 (bind mexpr (lambda (_) (do-monad bind rest ...)))]))

(define-syntax do-monad*
  (syntax-rules (<-)
                [(_ bind pure expr)
                 expr]
                [(_ bind pure [var <- mexpr] rest ...)
                 (bind mexpr (lambda (var) (do-monad* bind pure rest ...)))]
                [(_ bind pure mexpr rest ...)
                 (bind mexpr (lambda (_) (do-monad* bind pure rest ...)))]))
(doc do-monad* 'description "Variant that passes pure along with bind - useful when you need pure inside the computation")
