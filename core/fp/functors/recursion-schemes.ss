;;; fabric/stitches/fp/recursion-schemes.ss — Recursion Schemes
;;;
;;; Recursion schemes factor out recursion patterns from algorithms,
;;; separating the "shape" of recursion from the actual computation.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Fix: Fixed-point of a functor
;;;   - Catamorphism (cata): Fold/consume recursive structure
;;;   - Anamorphism (ana): Unfold/build recursive structure
;;;   - Hylomorphism (hylo): Build then consume (ana then cata)
;;;   - Paramorphism (para): Fold with access to original subtrees
;;;   - Apomorphism (apo): Unfold with early termination
;;;   - Histomorphism (histo): Fold with history (course-of-values)
;;;   - Futumorphism (futu): Unfold with lookahead
;;;   - Zygomorphism (zygo): Two folds in one pass
;;;   - Dynamorphism (dyna): Hylomorphism with memoization
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Pattern:
;;;   A functor F captures the "one layer" shape of a recursive type.
;;;   Fix F is the recursive type itself.
;;;
;;;   For a list: ListF a r = NilF | ConsF a r
;;;   Fix (ListF a) ~ [a]
;;;
;;; Laws:
;;;   cata alg . ana coalg = hylo alg coalg
;;;   cata (fmap f . alg) = f . cata alg (fusion)

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Fix - Fixed Point of Functor
;;; ============================================================
;;;
;;; Fix f = f (Fix f)
;;; The recursive type is the fixed point of its pattern functor.

;;; make-fix : f (Fix f) -> Fix f
;;; Wrap one layer of the functor.
(define (make-fix layer)
  (list 'fix layer))

;;; fix? : Any -> Boolean
(define (fix? x)
  (and (pair? x) (eq? (car x) 'fix)))

;;; unfix : Fix f -> f (Fix f)
;;; Unwrap one layer.
(define (unfix fx)
  (list-ref fx 1))

;;; ============================================================
;;; ListF - Pattern Functor for Lists
;;; ============================================================
;;;
;;; ListF a r = NilF | ConsF a r
;;; Where 'r' is the recursion point.

;;; nil-f : ListF a r
(define nil-f (list 'nil-f))

;;; nil-f? : ListF a r -> Boolean
(define (nil-f? x)
  (and (pair? x) (eq? (car x) 'nil-f)))

;;; cons-f : a -> r -> ListF a r
(define (cons-f head tail)
  (list 'cons-f head tail))

;;; cons-f? : ListF a r -> Boolean
(define (cons-f? x)
  (and (pair? x) (eq? (car x) 'cons-f)))

;;; cons-f-head : ConsF a r -> a
(define (cons-f-head cf) (list-ref cf 1))

;;; cons-f-tail : ConsF a r -> r
(define (cons-f-tail cf) (list-ref cf 2))

;;; list-f-fmap : (a -> b) -> ListF x a -> ListF x b
;;; Functor instance for ListF.
(define (list-f-fmap f lf)
  (cond
   [(nil-f? lf) lf]
   [(cons-f? lf) (cons-f (cons-f-head lf) (f (cons-f-tail lf)))]))

;;; ============================================================
;;; MaybeF - Pattern Functor for Maybe
;;; ============================================================

;;; nothing-f : MaybeF r
(define nothing-f (list 'nothing-f))

;;; nothing-f? : MaybeF r -> Boolean
(define (nothing-f? x)
  (and (pair? x) (eq? (car x) 'nothing-f)))

;;; just-f : a -> MaybeF a
(define (just-f val)
  (list 'just-f val))

;;; just-f? : MaybeF a -> Boolean
(define (just-f? x)
  (and (pair? x) (eq? (car x) 'just-f)))

;;; just-f-value : JustF a -> a
(define (just-f-value jf) (list-ref jf 1))

;;; maybe-f-fmap : (a -> b) -> MaybeF a -> MaybeF b
(define (maybe-f-fmap f mf)
  (cond
   [(nothing-f? mf) mf]
   [(just-f? mf) (just-f (f (just-f-value mf)))]))

;;; ============================================================
;;; NatF - Pattern Functor for Natural Numbers
;;; ============================================================
;;;
;;; NatF r = ZeroF | SuccF r
;;; Peano naturals as a recursive type.

;;; zero-f : NatF r
(define zero-f (list 'zero-f))

;;; zero-f? : NatF r -> Boolean
(define (zero-f? x)
  (and (pair? x) (eq? (car x) 'zero-f)))

;;; succ-f : r -> NatF r
(define (succ-f pred)
  (list 'succ-f pred))

;;; succ-f? : NatF r -> Boolean
(define (succ-f? x)
  (and (pair? x) (eq? (car x) 'succ-f)))

;;; succ-f-pred : SuccF r -> r
(define (succ-f-pred sf) (list-ref sf 1))

;;; nat-f-fmap : (a -> b) -> NatF a -> NatF b
(define (nat-f-fmap f nf)
  (cond
   [(zero-f? nf) nf]
   [(succ-f? nf) (succ-f (f (succ-f-pred nf)))]))

;;; ============================================================
;;; TreeF - Pattern Functor for Binary Trees
;;; ============================================================
;;;
;;; TreeF a r = LeafF | BranchF a r r

;;; leaf-f : TreeF a r
(define leaf-f (list 'leaf-f))

;;; leaf-f? : TreeF a r -> Boolean
(define (leaf-f? x)
  (and (pair? x) (eq? (car x) 'leaf-f)))

;;; branch-f : a -> r -> r -> TreeF a r
(define (branch-f value left right)
  (list 'branch-f value left right))

;;; branch-f? : TreeF a r -> Boolean
(define (branch-f? x)
  (and (pair? x) (eq? (car x) 'branch-f)))

;;; branch-f-value : BranchF a r -> a
(define (branch-f-value bf) (list-ref bf 1))

;;; branch-f-left : BranchF a r -> r
(define (branch-f-left bf) (list-ref bf 2))

;;; branch-f-right : BranchF a r -> r
(define (branch-f-right bf) (list-ref bf 3))

;;; tree-f-fmap : (a -> b) -> TreeF x a -> TreeF x b
(define (tree-f-fmap f tf)
  (cond
   [(leaf-f? tf) tf]
   [(branch-f? tf) (branch-f (branch-f-value tf)
                             (f (branch-f-left tf))
                             (f (branch-f-right tf)))]))

;;; ============================================================
;;; Catamorphism (cata) - Generalized Fold
;;; ============================================================
;;;
;;; cata : (f a -> a) -> Fix f -> a
;;; The algebra (f a -> a) tells how to collapse one layer.

;;; cata : Fmap f -> (f a -> a) -> Fix f -> a
(define (cata fmap alg fx)
  (alg (fmap (lambda (child) (cata fmap alg child))
             (unfix fx))))

;;; ============================================================
;;; Anamorphism (ana) - Generalized Unfold
;;; ============================================================
;;;
;;; ana : (a -> f a) -> a -> Fix f
;;; The coalgebra (a -> f a) tells how to generate one layer.

;;; ana : Fmap f -> (a -> f a) -> a -> Fix f
(define (ana fmap coalg seed)
  (make-fix (fmap (lambda (child) (ana fmap coalg child))
                  (coalg seed))))

;;; ============================================================
;;; Hylomorphism (hylo) - Refold
;;; ============================================================
;;;
;;; hylo : (f b -> b) -> (a -> f a) -> a -> b
;;; Unfold with coalg, then fold with alg.
;;; More efficient than (cata alg (ana coalg seed)).

;;; hylo : Fmap f -> (f b -> b) -> (a -> f a) -> a -> b
(define (hylo fmap alg coalg seed)
  (alg (fmap (lambda (child) (hylo fmap alg coalg child))
             (coalg seed))))

;;; ============================================================
;;; Paramorphism (para) - Fold with Original
;;; ============================================================
;;;
;;; para : (f (Fix f, a) -> a) -> Fix f -> a
;;; The algebra gets both the recursive result AND the original subtree.

;;; para : Fmap f -> (f (Pair (Fix f) a) -> a) -> Fix f -> a
(define (para fmap alg fx)
  (alg (fmap (lambda (child)
                     (cons child (para fmap alg child)))
             (unfix fx))))

;;; ============================================================
;;; Apomorphism (apo) - Unfold with Short-circuit
;;; ============================================================
;;;
;;; apo : (a -> f (Either (Fix f) a)) -> a -> Fix f
;;; Coalgebra can return Left to short-circuit with a finished tree.

;;; apo : Fmap f -> (a -> f (Either (Fix f) a)) -> a -> Fix f
(define (apo fmap coalg seed)
  (make-fix
   (fmap (lambda (either-child)
                 (if (left? either-child)
                     (from-left either-child)
                     (apo fmap coalg (from-right either-child))))
         (coalg seed))))

;;; ============================================================
;;; Histomorphism (histo) - Fold with History
;;; ============================================================
;;;
;;; Provides access to all previously computed values (course-of-values recursion).
;;; Uses a Cofree comonad to track history.

;;; Cofree f a = (a, f (Cofree f a))
;;; make-cofree : a -> f (Cofree f a) -> Cofree f a
(define (make-cofree head tail)
  (list 'cofree head tail))

;;; cofree? : Any -> Boolean
(define (cofree? x)
  (and (pair? x) (eq? (car x) 'cofree)))

;;; cofree-head : Cofree f a -> a
(define (cofree-head cf) (list-ref cf 1))

;;; cofree-tail : Cofree f a -> f (Cofree f a)
(define (cofree-tail cf) (list-ref cf 2))

;;; histo : Fmap f -> (f (Cofree f a) -> a) -> Fix f -> a
(define (histo fmap alg fx)
  (cofree-head (histo-worker fmap alg fx)))

;;; histo-worker : Fmap f -> (f (Cofree f a) -> a) -> Fix f -> Cofree f a
(define (histo-worker fmap alg fx)
  (let ([fcofree (fmap (lambda (child) (histo-worker fmap alg child))
                       (unfix fx))])
       (make-cofree (alg fcofree) fcofree)))

;;; ============================================================
;;; Futumorphism (futu) - Unfold with Lookahead
;;; ============================================================
;;;
;;; Uses Free monad for multi-step lookahead.

;;; Free f a = Pure a | Roll (f (Free f a))
;;; make-pure : a -> Free f a
(define (make-pure val)
  (list 'pure val))

;;; pure? : Free f a -> Boolean
(define (pure? x)
  (and (pair? x) (eq? (car x) 'pure)))

;;; pure-value : Pure a -> a
(define (pure-value p) (list-ref p 1))

;;; make-roll : f (Free f a) -> Free f a
(define (make-roll layer)
  (list 'roll layer))

;;; roll? : Free f a -> Boolean
(define (roll? x)
  (and (pair? x) (eq? (car x) 'roll)))

;;; roll-layer : Roll (f (Free f a)) -> f (Free f a)
(define (roll-layer r) (list-ref r 1))

;;; futu : Fmap f -> (a -> f (Free f a)) -> a -> Fix f
(define (futu fmap coalg seed)
  (make-fix
   (fmap (lambda (free-child)
                 (futu-worker fmap coalg free-child))
         (coalg seed))))

;;; futu-worker : Fmap f -> (a -> f (Free f a)) -> Free f a -> Fix f
(define (futu-worker fmap coalg free)
  (cond
   [(pure? free) (futu fmap coalg (pure-value free))]
   [(roll? free) (make-fix
                  (fmap (lambda (fc) (futu-worker fmap coalg fc))
                        (roll-layer free)))]))

;;; ============================================================
;;; Zygomorphism (zygo) - Two Folds in One Pass
;;; ============================================================
;;;
;;; zygo : (f b -> b) -> (f (b, a) -> a) -> Fix f -> a
;;; Run an auxiliary fold alongside the main one.

;;; zygo : Fmap f -> (f b -> b) -> (f (Pair b a) -> a) -> Fix f -> a
(define (zygo fmap aux alg fx)
  (cdr (zygo-worker fmap aux alg fx)))

;;; zygo-worker : Fmap f -> (f b -> b) -> (f (Pair b a) -> a) -> Fix f -> (Pair b a)
(define (zygo-worker fmap aux alg fx)
  (let ([fpairs (fmap (lambda (child) (zygo-worker fmap aux alg child))
                      (unfix fx))])
       (cons (aux (fmap car fpairs))
             (alg fpairs))))

;;; ============================================================
;;; Conversion Utilities
;;; ============================================================

;;; list->fix : List a -> Fix (ListF a)
;;; Convert a Scheme list to fixed-point form.
(define (list->fix lst)
  (if (null? lst)
      (make-fix nil-f)
      (make-fix (cons-f (car lst) (list->fix (cdr lst))))))

;;; fix->list : Fix (ListF a) -> List a
;;; Convert fixed-point form back to Scheme list.
(define (fix->list fx)
  (let ([layer (unfix fx)])
       (cond
        [(nil-f? layer) '()]
        [(cons-f? layer) (cons (cons-f-head layer)
                               (fix->list (cons-f-tail layer)))])))

;;; nat->fix : Number -> Fix NatF
;;; Convert a number to Peano natural.
(define (nat->fix n)
  (if (= n 0)
      (make-fix zero-f)
      (make-fix (succ-f (nat->fix (- n 1))))))

;;; fix->nat : Fix NatF -> Number
;;; Convert Peano natural back to number.
(define (fix->nat fx)
  (let ([layer (unfix fx)])
       (cond
        [(zero-f? layer) 0]
        [(succ-f? layer) (+ 1 (fix->nat (succ-f-pred layer)))])))

;;; ============================================================
;;; Example Algebras and Coalgebras
;;; ============================================================

;;; length-alg : ListF a Number -> Number
;;; Algebra for computing list length.
(define (length-alg lf)
  (cond
   [(nil-f? lf) 0]
   [(cons-f? lf) (+ 1 (cons-f-tail lf))]))

;;; sum-alg : ListF Number Number -> Number
;;; Algebra for summing a list of numbers.
(define (sum-alg lf)
  (cond
   [(nil-f? lf) 0]
   [(cons-f? lf) (+ (cons-f-head lf) (cons-f-tail lf))]))

;;; product-alg : ListF Number Number -> Number
;;; Algebra for multiplying list elements.
(define (product-alg lf)
  (cond
   [(nil-f? lf) 1]
   [(cons-f? lf) (* (cons-f-head lf) (cons-f-tail lf))]))

;;; map-alg : (a -> b) -> ListF a (List b) -> List b
;;; Algebra for mapping a function.
(define (map-alg f)
  (lambda (lf)
          (cond
           [(nil-f? lf) '()]
           [(cons-f? lf) (cons (f (cons-f-head lf)) (cons-f-tail lf))])))

;;; filter-alg : (a -> Bool) -> ListF a (List a) -> List a
;;; Algebra for filtering.
(define (filter-alg pred)
  (lambda (lf)
          (cond
           [(nil-f? lf) '()]
           [(cons-f? lf)
            (if (pred (cons-f-head lf))
                (cons (cons-f-head lf) (cons-f-tail lf))
                (cons-f-tail lf))])))

;;; range-coalg : (Number, Number) -> ListF Number (Number, Number)
;;; Coalgebra for generating a range.
(define (range-coalg pair)
  (let ([lo (car pair)] [hi (cdr pair)])
       (if (> lo hi)
           nil-f
           (cons-f lo (cons (+ lo 1) hi)))))

;;; replicate-coalg : (Number, a) -> ListF a (Number, a)
;;; Coalgebra for replicating a value.
(define (replicate-coalg pair)
  (let ([n (car pair)] [x (cdr pair)])
       (if (<= n 0)
           nil-f
           (cons-f x (cons (- n 1) x)))))

;;; ============================================================
;;; Tree Algebras
;;; ============================================================

;;; tree-size-alg : TreeF a Number -> Number
;;; Count nodes in a tree.
(define (tree-size-alg tf)
  (cond
   [(leaf-f? tf) 0]
   [(branch-f? tf) (+ 1 (branch-f-left tf) (branch-f-right tf))]))

;;; tree-depth-alg : TreeF a Number -> Number
;;; Compute tree depth.
(define (tree-depth-alg tf)
  (cond
   [(leaf-f? tf) 0]
   [(branch-f? tf) (+ 1 (max (branch-f-left tf) (branch-f-right tf)))]))

;;; tree-sum-alg : TreeF Number Number -> Number
;;; Sum values in tree.
(define (tree-sum-alg tf)
  (cond
   [(leaf-f? tf) 0]
   [(branch-f? tf) (+ (branch-f-value tf)
                      (branch-f-left tf)
                      (branch-f-right tf))]))

;;; flatten-alg : TreeF a (List a) -> List a
;;; Flatten tree to list (in-order).
(define (flatten-alg tf)
  (cond
   [(leaf-f? tf) '()]
   [(branch-f? tf) (append (branch-f-left tf)
                           (list (branch-f-value tf))
                           (branch-f-right tf))]))

;;; ============================================================
;;; Natural Number Examples
;;; ============================================================

;;; factorial-alg : NatF (Number, Number) -> (Number, Number)
;;; Para-style algebra for factorial (needs current n).
;;; Returns (current-n, factorial-so-far).
(define (fact-para-alg nf)
  (cond
   [(zero-f? nf) 1]
   [(succ-f? nf)
    (let ([pair (succ-f-pred nf)])
         ;; pair = (original-subtree . result)
         (* (+ 1 (fix->nat (car pair)))
            (cdr pair)))]))

;;; fib-histo-alg : NatF (Cofree NatF Number) -> Number
;;; Fibonacci using histomorphism (access to history).
(define (fib-histo-alg nf)
  (cond
   [(zero-f? nf) 0]
   [(succ-f? nf)
    (let ([prev-cofree (succ-f-pred nf)])
         (let ([prev-val (cofree-head prev-cofree)]
               [prev-tail (cofree-tail prev-cofree)])
              (if (zero-f? prev-tail)
                  1  ; fib(1) = 1
                  (+ prev-val (cofree-head (succ-f-pred prev-tail))))))]))

;;; ============================================================
;;; Practical Combinators
;;; ============================================================

;;; list-cata : (ListF a b -> b) -> List a -> b
;;; Catamorphism over regular Scheme lists.
(define (list-cata alg lst)
  (cata list-f-fmap alg (list->fix lst)))

;;; list-ana : (b -> ListF a b) -> b -> List a
;;; Anamorphism producing regular Scheme lists.
(define (list-ana coalg seed)
  (fix->list (ana list-f-fmap coalg seed)))

;;; list-hylo : (ListF a b -> b) -> (c -> ListF a c) -> c -> b
;;; Hylomorphism on lists.
(define (list-hylo alg coalg seed)
  (hylo list-f-fmap alg coalg seed))

;;; nat-cata : (NatF a -> a) -> Number -> a
;;; Catamorphism over natural numbers.
(define (nat-cata alg n)
  (cata nat-f-fmap alg (nat->fix n)))

;;; ============================================================
;;; Mendler-Style Recursion (more general)
;;; ============================================================
;;;
;;; Mendler-style doesn't require explicit fmap.

;;; mcata : (forall x. (x -> a) -> f x -> a) -> Fix f -> a
;;; Mendler-style catamorphism.
(define (mcata alg fx)
  (alg (lambda (child) (mcata alg child))
       (unfix fx)))

;;; mana : (forall x. (a -> x) -> a -> f x) -> a -> Fix f
;;; Mendler-style anamorphism.
(define (mana coalg seed)
  (make-fix (coalg (lambda (child) (mana coalg child)) seed)))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Sum a list using catamorphism
;;; (list-cata sum-alg '(1 2 3 4 5))  ; => 15
;;;
;;; ;; Generate a range using anamorphism
;;; (list-ana range-coalg (cons 1 5))  ; => (1 2 3 4 5)
;;;
;;; ;; Map using catamorphism
;;; (list-cata (map-alg (lambda (x) (* x 2))) '(1 2 3))  ; => (2 4 6)
;;;
;;; ;; Factorial using hylomorphism
;;; (list-hylo product-alg range-coalg (cons 1 5))  ; => 120 (5!)
;;;
;;; ;; Fibonacci using histomorphism
;;; (histo nat-f-fmap fib-histo-alg (nat->fix 10))  ; => 55
