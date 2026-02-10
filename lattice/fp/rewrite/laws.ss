;;; @module rewrite/laws
;;; @requires prelude rule engine

(require 'prelude)
(require 'rule)
(require 'engine)

(doc 'module 'rewrite/laws)
(doc 'description "Standard FP Law Library")
(doc 'layer 'lattice)

(doc 'description "Encodes algebraic laws as rewrite rules:
  - Monoid laws (identity, associativity)
  - Semigroup laws (associativity)
  - Functor laws (identity, composition)
  - Applicative laws (identity, composition, homomorphism, interchange)
  - Monad laws (left identity, right identity, associativity)
  - Lambda calculus (beta reduction, eta reduction)

Each law is a named rewrite rule that can be applied to transform
expressions according to well-known algebraic identities.

This is Lattice code: pure, total, assumes reasonable input.")

(doc 'section "Law Registry")

(define *law-registry* (make-eq-hashtable))
(doc *law-registry* 'description "Global registry of all laws")

(define (register-law! law)
  (doc 'type (-> Rule Void))
  (doc 'description "Add a law to the registry.")
  (hashtable-set! *law-registry* (rule-name law) law))

;;; get-law : Symbol → Rule | #f
;;; Retrieve a law by name.
(define (get-law name)
  (hashtable-ref *law-registry* name #f))

;;; all-laws : → (List Rule)
;;; Get all registered laws.
(define (all-laws)
  (vector->list (hashtable-values *law-registry*)))

;;; laws-by-category : Symbol → (List Rule)
;;; Get all laws in a category.
(define (laws-by-category category)
  (filter (lambda (law) (eq? (rule-category law) category))
          (all-laws)))

;;; law-names : → (List Symbol)
;;; Get all law names.
(define (law-names)
  (vector->list (hashtable-keys *law-registry*)))

;;; law-categories : → (List Symbol)
;;; Get all unique categories.
(define (law-categories)
  (let ([cats (map rule-category (all-laws))])
       (let loop ([cs cats] [seen '()] [acc '()])
            (cond
             [(null? cs) (reverse acc)]
             [(memq (car cs) seen) (loop (cdr cs) seen acc)]
             [else (loop (cdr cs) (cons (car cs) seen) (cons (car cs) acc))]))))

;;; ====
;;; Semigroup Laws
;;; ====

;;; Associativity: (x <> y) <> z = x <> (y <> z)
;;; Right-associative is canonical form for simplification
(define semigroup-assoc
  (make-rule 'semigroup-assoc
             '(mappend (mappend (?x) (?y)) (?z))
             '(mappend (?x) (mappend (?y) (?z)))
             'category 'semigroup
             'direction 'forward))

;;; Associativity (reverse): x <> (y <> z) = (x <> y) <> z
;;; Use 'expansion category to avoid oscillation with semigroup-assoc
(define semigroup-assoc-rev
  (make-rule 'semigroup-assoc-rev
             '(mappend (?x) (mappend (?y) (?z)))
             '(mappend (mappend (?x) (?y)) (?z))
             'category 'expansion
             'direction 'forward))

;;; ====
;;; Monoid Laws
;;; ====

;;; Left identity: mempty <> x = x
(define monoid-left-id
  (make-rule 'monoid-left-id
             '(mappend mempty (?x))
             '(?x)
             'category 'monoid))

;;; Right identity: x <> mempty = x
(define monoid-right-id
  (make-rule 'monoid-right-id
             '(mappend (?x) mempty)
             '(?x)
             'category 'monoid))

;;; Monoid associativity (same as semigroup)
(define monoid-assoc
  (make-rule 'monoid-assoc
             '(mappend (mappend (?x) (?y)) (?z))
             '(mappend (?x) (mappend (?y) (?z)))
             'category 'monoid
             'direction 'bidirectional))

;;; ====
;;; Functor Laws
;;; ====

;;; Identity: fmap id = id
;;; Using literal 'id symbol for simplicity
(define functor-id
  (make-rule 'functor-id
             '(fmap id (?fa))
             '(?fa)
             'category 'functor))

;;; Composition: fmap (f . g) = fmap f . fmap g
;;; Expands composed fmap - use 'expansion to avoid oscillation
(define functor-comp
  (make-rule 'functor-comp
             '(fmap (compose (?f) (?g)) (?fa))
             '(fmap (?f) (fmap (?g) (?fa)))
             'category 'expansion
             'direction 'forward))

;;; Composition fusion: fmap f (fmap g fa) = fmap (compose f g) fa
;;; Contracts nested fmaps - this is the simplifying direction
(define functor-fuse
  (make-rule 'functor-fuse
             '(fmap (?f) (fmap (?g) (?fa)))
             '(fmap (compose (?f) (?g)) (?fa))
             'category 'functor))

;;; ====
;;; Applicative Laws
;;; ====

;;; Identity: pure id <*> v = v
(define applicative-id
  (make-rule 'applicative-id
             '(ap (pure id) (?v))
             '(?v)
             'category 'applicative))

;;; Homomorphism: pure f <*> pure x = pure (f x)
(define applicative-homo
  (make-rule 'applicative-homo
             '(ap (pure (?f)) (pure (?x)))
             '(pure ((?f) (?x)))
             'category 'applicative))

;;; Interchange: u <*> pure y = pure ($ y) <*> u
(define applicative-interchange
  (make-rule 'applicative-interchange
             '(ap (?u) (pure (?y)))
             '(ap (pure (fn ((?f)) ((?f) (?y)))) (?u))
             'category 'applicative
             'direction 'bidirectional))

;;; ====
;;; Monad Laws
;;; ====

;;; Left identity: return a >>= f = f a
(define monad-left-id
  (make-rule 'monad-left-id
             '(bind (pure (?a)) (?f))
             '((?f) (?a))
             'category 'monad))

;;; Right identity: m >>= return = m
(define monad-right-id
  (make-rule 'monad-right-id
             '(bind (?m) pure)
             '(?m)
             'category 'monad))

;;; Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
;;; Expands nested binds - use 'expansion to avoid oscillation
(define monad-assoc
  (make-rule 'monad-assoc
             '(bind (bind (?m) (?f)) (?g))
             '(bind (?m) (fn ((?x)) (bind ((?f) (?x)) (?g))))
             'category 'expansion
             'direction 'forward))

;;; Associativity (reverse for flattening)
;;; Contracts to nested binds - this is the simplifying direction
(define monad-assoc-rev
  (make-rule 'monad-assoc-rev
             '(bind (?m) (fn ((?x)) (bind ((?f) (?x)) (?g))))
             '(bind (bind (?m) (?f)) (?g))
             'category 'monad))

;;; ====
;;; Lambda Calculus
;;; ====

;;; Beta reduction: (\x -> body) arg = body[x := arg]
;;; Note: This rule's RHS uses substitution which is handled specially
(define beta-reduce
  (make-rule 'beta-reduce
             '((fn ((?x)) (?body)) (?arg))
             '(subst (?body) (?x) (?arg))
             'category 'lambda))

;;; Eta reduction: \x -> f x = f  (when x not free in f)
;;; Note: Requires side condition check
(define eta-reduce
  (make-rule 'eta-reduce
             '(fn ((?x)) ((?f) (?x)))
             '(?f)
             'category 'lambda
             'conditions '((not-free? (?x) (?f)))))

;;; ====
;;; Arithmetic Identities
;;; ====

;;; Addition identity: 0 + x = x
(define arith-add-left-id
  (make-rule 'arith-add-left-id
             '(+ 0 (?x))
             '(?x)
             'category 'arithmetic))

;;; Addition identity: x + 0 = x
(define arith-add-right-id
  (make-rule 'arith-add-right-id
             '(+ (?x) 0)
             '(?x)
             'category 'arithmetic))

;;; Multiplication identity: 1 * x = x
(define arith-mul-left-id
  (make-rule 'arith-mul-left-id
             '(* 1 (?x))
             '(?x)
             'category 'arithmetic))

;;; Multiplication identity: x * 1 = x
(define arith-mul-right-id
  (make-rule 'arith-mul-right-id
             '(* (?x) 1)
             '(?x)
             'category 'arithmetic))

;;; Multiplication by zero: 0 * x = 0
(define arith-mul-zero-left
  (make-rule 'arith-mul-zero-left
             '(* 0 (?x))
             '0
             'category 'arithmetic))

;;; Multiplication by zero: x * 0 = 0
(define arith-mul-zero-right
  (make-rule 'arith-mul-zero-right
             '(* (?x) 0)
             '0
             'category 'arithmetic))

;;; ====
;;; List Laws
;;; ====

;;; map id = id
(define list-map-id
  (make-rule 'list-map-id
             '(map id (?xs))
             '(?xs)
             'category 'list))

;;; map fusion: map f (map g xs) = map (compose f g) xs
(define list-map-fuse
  (make-rule 'list-map-fuse
             '(map (?f) (map (?g) (?xs)))
             '(map (compose (?f) (?g)) (?xs))
             'category 'list))

;;; foldr with cons and nil
(define list-foldr-id
  (make-rule 'list-foldr-id
             '(foldr cons () (?xs))
             '(?xs)
             'category 'list))

;;; ====
;;; Initialize Standard Laws
;;; ====

(define (init-standard-laws!)
  ;; Semigroup
  (register-law! semigroup-assoc)
  (register-law! semigroup-assoc-rev)
  
  ;; Monoid
  (register-law! monoid-left-id)
  (register-law! monoid-right-id)
  (register-law! monoid-assoc)
  
  ;; Functor
  (register-law! functor-id)
  (register-law! functor-comp)
  (register-law! functor-fuse)
  
  ;; Applicative
  (register-law! applicative-id)
  (register-law! applicative-homo)
  (register-law! applicative-interchange)
  
  ;; Monad
  (register-law! monad-left-id)
  (register-law! monad-right-id)
  (register-law! monad-assoc)
  (register-law! monad-assoc-rev)
  
  ;; Lambda
  (register-law! beta-reduce)
  (register-law! eta-reduce)
  
  ;; Arithmetic
  (register-law! arith-add-left-id)
  (register-law! arith-add-right-id)
  (register-law! arith-mul-left-id)
  (register-law! arith-mul-right-id)
  (register-law! arith-mul-zero-left)
  (register-law! arith-mul-zero-right)
  
  ;; List
  (register-law! list-map-id)
  (register-law! list-map-fuse)
  (register-law! list-foldr-id))

;;; Initialize on load
(init-standard-laws!)

;;; ====
;;; Convenience Strategies
;;; ====

;;; monoid-simplify : Strategy
;;; Apply monoid laws exhaustively.
(define monoid-simplify
  (innermost (rules->strategy (laws-by-category 'monoid))))

;;; functor-simplify : Strategy
;;; Apply functor laws exhaustively.
(define functor-simplify
  (innermost (rules->strategy (laws-by-category 'functor))))

;;; monad-simplify : Strategy
;;; Apply monad laws exhaustively.
(define monad-simplify
  (innermost (rules->strategy (laws-by-category 'monad))))

;;; arith-simplify : Strategy
;;; Apply arithmetic identity laws.
(define arith-simplify
  (innermost (rules->strategy (laws-by-category 'arithmetic))))

;;; all-simplify : Strategy
;;; Apply all simplification laws.
(define all-simplify
  (innermost (rules->strategy (all-laws))))
