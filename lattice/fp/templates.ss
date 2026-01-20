(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

(doc 'module 'templates)
(doc 'description "Template generator for common FP patterns including: Monoids, Foldable, Traversable, Lenses, Functors, Applicatives")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "- Template generators for type class instances
  - Code snippets with best-practice defaults
  - Customizable templates with hooks
  - Laws verification templates")
(doc 'dependencies '(core/base/prelude.ss lattice/fp/meta/combinators.ss))

;;; ====
;;; Monoid Templates
;;; ====
;;;
;;; A monoid is a type with:
;;;   - mempty : M             (identity element)
;;;   - mappend : M → M → M    (associative binary operation)
;;;
;;; Laws:
;;;   - Left identity:  mappend mempty x = x
;;;   - Right identity: mappend x mempty = x
;;;   - Associativity:  mappend (mappend x y) z = mappend x (mappend y z)

;;; make-monoid : Value × (Value × Value → Value) → Monoid
;;; Create a monoid from identity and binary operation.
(define (make-monoid mempty mappend)
  (list 'monoid mempty mappend))

;;; monoid? : α → Boolean
(define (monoid? x)
  (and (pair? x) (eq? (car x) 'monoid)))

;;; monoid-mempty : Monoid → Value
(define (monoid-mempty m)
  (cadr m))

;;; monoid-mappend : Monoid → (Value × Value → Value)
(define (monoid-mappend m)
  (caddr m))

;;; ====
;;; Common Monoid Instances
;;; ====

;;; monoid-sum : Monoid for numbers under addition
(define monoid-sum
  (make-monoid 0 +))

;;; monoid-product : Monoid for numbers under multiplication
(define monoid-product
  (make-monoid 1 *))

;;; monoid-list : Monoid for lists under append
(define monoid-list
  (make-monoid '() append))

;;; monoid-string : Monoid for strings under concatenation
(define monoid-string
  (make-monoid "" string-append))

;;; monoid-all : Monoid for booleans under AND
(define monoid-all
  (make-monoid #t (lambda (a b) (and a b))))

;;; monoid-any : Monoid for booleans under OR
(define monoid-any
  (make-monoid #f (lambda (a b) (or a b))))

;;; monoid-first : Monoid taking first non-nothing value
(define monoid-first
  (make-monoid nothing
               (lambda (a b) (if (just? a) a b))))

;;; monoid-last : Monoid taking last non-nothing value
(define monoid-last
  (make-monoid nothing
               (lambda (a b) (if (just? b) b a))))

;;; monoid-max : Monoid taking maximum (with -inf identity)
(define monoid-max
  (make-monoid -inf.0 max))

;;; monoid-min : Monoid taking minimum (with +inf identity)
(define monoid-min
  (make-monoid +inf.0 min))

;;; monoid-endo : Monoid for endofunctions under composition
(define monoid-endo
  (make-monoid identity compose2))

;;; ====
;;; Monoid Operations
;;; ====

;;; mconcat : Monoid × (List Value) → Value
;;; Fold a list using the monoid.
(define (mconcat m xs)
  (fold-left (monoid-mappend m) (monoid-mempty m) xs))

;;; mtimes : Monoid × Nat × Value → Value
;;; Repeat value n times using monoid.
(define (mtimes m n x)
  (if (<= n 0)
      (monoid-mempty m)
      (let loop ([n n] [acc x])
           (if (= n 1)
               acc
               (loop (- n 1) ((monoid-mappend m) acc x))))))

;;; ====
;;; Monoid Law Verification
;;; ====

;;; verify-monoid-laws : Monoid × (List Value) → Boolean
;;; Check monoid laws hold for given test values.
(define (verify-monoid-laws m test-values)
  (let ([e (monoid-mempty m)]
        [op (monoid-mappend m)])
       (and
        ;; Left identity: op e x = x
        (andmap (lambda (x) (equal? (op e x) x)) test-values)
        ;; Right identity: op x e = x
        (andmap (lambda (x) (equal? (op x e) x)) test-values)
        ;; Associativity (for pairs)
        (let check-assoc ([vs test-values])
             (if (< (length vs) 3)
                 #t
                 (let ([x (car vs)]
                       [y (cadr vs)]
                       [z (caddr vs)])
                      (and (equal? (op (op x y) z)
                                   (op x (op y z)))
                           (check-assoc (cdr vs)))))))))

;;; ====
;;; Foldable Templates
;;; ====
;;;
;;; A foldable type can be reduced to a single value.
;;;   - foldMap : (a → m) → F a → m   (given Monoid m)
;;;   - foldr : (a → b → b) → b → F a → b

;;; make-foldable : ((α → Monoid → β) → Container α → β) × ... → Foldable
;;; Create a foldable instance.
(define (make-foldable fold-map-fn foldr-fn foldl-fn)
  (list 'foldable fold-map-fn foldr-fn foldl-fn))

;;; foldable? : α → Boolean
(define (foldable? x)
  (and (pair? x) (eq? (car x) 'foldable)))

;;; foldable-fold-map : Foldable → ((α → β) → Monoid → Container α → β)
(define (foldable-fold-map f)
  (cadr f))

;;; foldable-foldr : Foldable → ((α → β → β) → β → Container α → β)
(define (foldable-foldr f)
  (caddr f))

;;; foldable-foldl : Foldable → ((β → α → β) → β → Container α → β)
(define (foldable-foldl f)
  (cadddr f))

;;; ====
;;; Common Foldable Instances
;;; ====

;;; foldable-list : Foldable instance for lists
(define foldable-list
  (make-foldable
   ;; fold-map: (a -> m) -> [a] -> m
   (lambda (f m xs)
           (fold-left (lambda (acc x) ((monoid-mappend m) acc (f x)))
                      (monoid-mempty m)
                      xs))
   ;; foldr
   fold-right
   ;; foldl
   fold-left))

;;; foldable-maybe : Foldable instance for Maybe
(define foldable-maybe
  (make-foldable
   ;; fold-map
   (lambda (f m mx)
           (if (just? mx)
               (f (from-just mx))
               (monoid-mempty m)))
   ;; foldr
   (lambda (f z mx)
           (if (just? mx)
               (f (from-just mx) z)
               z))
   ;; foldl
   (lambda (f z mx)
           (if (just? mx)
               (f z (from-just mx))
               z))))

;;; ====
;;; Foldable Operations
;;; ====

;;; fold-with : Foldable × Monoid × Container → Value
;;; Fold container using monoid.
(define (fold-with fld m container)
  ((foldable-fold-map fld) identity m container))

;;; to-list : Foldable × Container → (List α)
;;; Convert foldable to list.
(define (to-list fld container)
  ((foldable-foldr fld) cons '() container))

;;; null-foldable? : Foldable × Container → Boolean
;;; Check if container is empty.
(define (null-foldable? fld container)
  ((foldable-foldr fld) (lambda (x _) #f) #t container))

;;; length-foldable : Foldable × Container → Nat
;;; Get length of container.
(define (length-foldable fld container)
  ((foldable-foldl fld) (lambda (n _) (+ n 1)) 0 container))

;;; elem-foldable : Foldable × Value × Container → Boolean
;;; Check if element is in container.
(define (elem-foldable fld x container)
  ((foldable-foldr fld)
   (lambda (y found) (or found (equal? x y)))
   #f
   container))

;;; ====
;;; Traversable Templates
;;; ====
;;;
;;; Traversable types can be traversed with effects.
;;;   - traverse : (a → F b) → T a → F (T b)
;;;   - sequenceA : T (F a) → F (T a)

;;; make-traversable : (Applicative → (α → F β) → Container α → F (Container β)) → Traversable
(define (make-traversable traverse-fn)
  (list 'traversable traverse-fn))

;;; traversable? : α → Boolean
(define (traversable? x)
  (and (pair? x) (eq? (car x) 'traversable)))

;;; traversable-traverse : Traversable → (Applicative → (α → F β) → Container α → F (Container β))
(define (traversable-traverse t)
  (cadr t))

;;; ====
;;; Functor Templates
;;; ====
;;;
;;; A functor allows mapping a function over a container.
;;;   - fmap : (a → b) × F(a) → F(b)
;;;
;;; Laws:
;;;   - Identity: fmap id = id
;;;   - Composition: fmap (g . f) = fmap g . fmap f
;;;
;;; Note: Unlike Haskell's curried fmap, our fmap takes TWO arguments:
;;;   ((functor-fmap F) func container)  ; correct - 2 args
;;;   ((functor-fmap F) func)            ; returns partial application if func supports it

;;; make-functor : ((α → β) × F(α) → F(β)) → Functor
;;; The fmap-fn should be a 2-argument function: (lambda (f container) ...).
(define (make-functor fmap-fn)
  (list 'functor fmap-fn))

;;; make-named-functor : Symbol × ((α → β) × F(α) → F(β)) → Functor
;;; Create a functor with a name for debugging.
(define (make-named-functor name fmap-fn)
  (list 'functor fmap-fn name))

;;; functor? : α → Boolean
(define (functor? x)
  (and (pair? x) (eq? (car x) 'functor)))

;;; functor-fmap : Functor → ((α → β) × F(α) → F(β))
;;; Returns the fmap function, which takes 2 arguments: (func container).
(define (functor-fmap f)
  (cadr f))

;;; functor-name : Functor × [Symbol] → Symbol
;;; Get the name of the functor, or default if unnamed.
(define (functor-name f . args)
  (let ([default (if (null? args) 'F (car args))])
    (if (and (functor? f) (> (length f) 2))
        (caddr f)
        default)))

;;; ====
;;; Common Functor Instances
;;; ====

;;; functor-list : Functor for lists
(define functor-list
  (make-named-functor 'list map))

;;; functor-maybe : Functor for Maybe
(define functor-maybe
  (make-named-functor 'maybe maybe-fmap))

;;; functor-either : Functor for Either (maps over Right)
(define functor-either
  (make-named-functor 'either either-fmap))

;;; functor-id : Identity functor
;;; Id(f)(x) = f(x) — applies function directly to value
(define functor-id
  (make-named-functor 'id (lambda (f x) (f x))))

;;; ====
;;; Functor Operations
;;; ====

;;; fmap-with : Functor × (α → β) × Container → Container
;;; Apply fmap using given functor.
(define (fmap-with func f container)
  ((functor-fmap func) f container))

;;; replace-with : Functor × Value × Container → Container
;;; Replace all values in container.
(define (replace-with func val container)
  (fmap-with func (const val) container))

;;; void-with : Functor × Container → Container ()
;;; Discard values, keeping structure.
(define (void-with func container)
  (replace-with func '() container))

;;; ====
;;; Applicative Templates
;;; ====
;;;
;;; Applicative functors allow applying functions in context.
;;;   - pure : a → F a
;;;   - <*> : F (a → b) → F a → F b
;;;
;;; Laws:
;;;   - Identity: pure id <*> v = v
;;;   - Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
;;;   - Homomorphism: pure f <*> pure x = pure (f x)
;;;   - Interchange: u <*> pure y = pure ($ y) <*> u

;;; make-applicative : ((α → F α) × (F (α → β) × F α → F β)) → Applicative
(define (make-applicative pure-fn ap-fn)
  (list 'applicative pure-fn ap-fn))

;;; applicative? : α → Boolean
(define (applicative? x)
  (and (pair? x) (eq? (car x) 'applicative)))

;;; applicative-pure : Applicative → (α → F α)
(define (applicative-pure ap)
  (cadr ap))

;;; applicative-ap : Applicative → (F (α → β) × F α → F β)
(define (applicative-ap ap)
  (caddr ap))

;;; ====
;;; Common Applicative Instances
;;; ====

;;; applicative-list : Applicative for lists (all combinations)
(define applicative-list
  (make-applicative
   ;; pure: a -> [a]
   list
   ;; <*>: [a -> b] -> [a] -> [b]
   (lambda (fs xs)
           (apply append
                  (map (lambda (f) (map f xs)) fs)))))

;;; applicative-maybe : Applicative for Maybe
(define applicative-maybe
  (make-applicative
   ;; pure
   just
   ;; <*>
   (lambda (mf mx)
           (if (and (just? mf) (just? mx))
               (just ((from-just mf) (from-just mx)))
               nothing))))

;;; applicative-either : Applicative for Either
(define applicative-either
  (make-applicative
   ;; pure
   right
   ;; <*>
   (lambda (ef ex)
           (cond
            [(left? ef) ef]
            [(left? ex) ex]
            [else (right ((from-right ef) (from-right ex)))]))))

;;; ====
;;; Applicative Operations
;;; ====

;;; ap-with : Applicative × F (α → β) × F α → F β
(define (ap-with ap ff fx)
  ((applicative-ap ap) ff fx))

;;; lift-a2 : Applicative × (α → β → γ) × F α × F β → F γ
;;; Lift binary function to applicative.
;;; lift-a2 f fx fy = pure f <*> fx <*> fy
(define (lift-a2 ap f fx fy)
  (let* ([pure (applicative-pure ap)]
         [ap-fn (applicative-ap ap)]
         ;; fmap f fx = pure f <*> fx
         [ff (ap-fn (pure f) fx)])
        (ap-fn ff fy)))

;;; sequence-a : Applicative × (List (F α)) → F (List α)
;;; Sequence list of applicative actions.
(define (sequence-a ap actions)
  ;; Use curried cons: x -> xs -> (cons x xs)
  (let ([cons-curried (lambda (x) (lambda (xs) (cons x xs)))])
       (fold-right (lambda (action acc)
                           (lift-a2 ap cons-curried action acc))
                   ((applicative-pure ap) '())
                   actions)))

;;; traverse-a : Applicative × (α → F β) × (List α) → F (List β)
;;; Traverse list with applicative action.
(define (traverse-a ap f xs)
  (sequence-a ap (map f xs)))

;;; ====
;;; Lens Templates
;;; ====
;;;
;;; Lenses are composable first-class getters and setters.
;;;   - view : Lens s a → s → a
;;;   - set : Lens s a → a → s → s
;;;   - over : Lens s a → (a → a) → s → s
;;;
;;; Lens laws:
;;;   - Get-Put: set l (view l s) s = s
;;;   - Put-Get: view l (set l a s) = a
;;;   - Put-Put: set l a' (set l a s) = set l a' s

;;; make-lens : (s → a) × (a → s → s) → Lens
;;; Create a lens from getter and setter.
(define (make-lens getter setter)
  (list 'lens getter setter))

;;; lens? : α → Boolean
(define (lens? x)
  (and (pair? x) (eq? (car x) 'lens)))

;;; lens-getter : Lens → (s → a)
(define (lens-getter l)
  (cadr l))

;;; lens-setter : Lens → (a → s → s)
(define (lens-setter l)
  (caddr l))

;;; ====
;;; Lens Operations
;;; ====

;;; view : Lens × s → a
;;; Get the focused value.
(define (view lens s)
  ((lens-getter lens) s))

;;; set : Lens × a × s → s
;;; Set the focused value.
(define (set-lens lens a s)
  ((lens-setter lens) a s))

;;; over : Lens × (a → a) × s → s
;;; Modify the focused value.
(define (over lens f s)
  (set-lens lens (f (view lens s)) s))

;;; lens-compose : Lens s a × Lens a b → Lens s b
;;; Compose two lenses.
(define (lens-compose outer inner)
  (make-lens
   ;; getter: s -> b
   (lambda (s)
           (view inner (view outer s)))
   ;; setter: b -> s -> s
   (lambda (b s)
           (over outer (lambda (a) (set-lens inner b a)) s))))

;;; ====
;;; Common Lens Constructors
;;; ====

;;; lens-fst : Lens (Pair a b) a
;;; Lens focusing on first element of pair.
(define lens-fst
  (make-lens car (lambda (a p) (cons a (cdr p)))))

;;; lens-snd : Lens (Pair a b) b
;;; Lens focusing on second element of pair.
(define lens-snd
  (make-lens cdr (lambda (b p) (cons (car p) b))))

;;; lens-head : Lens (List a) a
;;; Lens focusing on head of list.
(define lens-head
  (make-lens car (lambda (a xs) (cons a (cdr xs)))))

;;; lens-tail : Lens (List a) (List a)
;;; Lens focusing on tail of list.
(define lens-tail
  (make-lens cdr (lambda (t xs) (cons (car xs) t))))

;;; lens-nth : Nat → Lens (List a) a
;;; Lens focusing on nth element.
(define (lens-nth n)
  (make-lens
   (lambda (xs) (list-ref xs n))
   (lambda (a xs)
           (let loop ([i 0] [xs xs] [acc '()])
                (if (null? xs)
                    (reverse acc)
                    (if (= i n)
                        (loop (+ i 1) (cdr xs) (cons a acc))
                        (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))))

;;; lens-key : Symbol → Lens (Alist s v) v
;;; Lens focusing on value for given key in association list.
(define (lens-key key)
  (make-lens
   (lambda (alist)
           (let ([entry (assq key alist)])
                (if entry (cdr entry) #f)))
   (lambda (v alist)
           (cons (cons key v)
                 (filter (lambda (p) (not (eq? (car p) key))) alist)))))

;;; ====
;;; Prism Templates
;;; ====
;;;
;;; Prisms focus on parts of sum types (like case analysis).
;;;   - preview : Prism s a → s → Maybe a
;;;   - review : Prism s a → a → s

;;; make-prism : (s → Maybe a) × (a → s) → Prism
(define (make-prism match build)
  (list 'prism match build))

;;; prism? : α → Boolean
(define (prism? x)
  (and (pair? x) (eq? (car x) 'prism)))

;;; prism-match : Prism → (s → Maybe a)
(define (prism-match p)
  (cadr p))

;;; prism-build : Prism → (a → s)
(define (prism-build p)
  (caddr p))

;;; preview : Prism × s → Maybe a
(define (preview prism s)
  ((prism-match prism) s))

;;; review : Prism × a → s
(define (review prism a)
  ((prism-build prism) a))

;;; ====
;;; Common Prisms
;;; ====

;;; prism-just : Prism (Maybe a) a
;;; Prism focusing on Just values.
(define prism-just
  (make-prism
   (lambda (m) (if (just? m) m nothing))
   just))

;;; prism-left : Prism (Either a b) a
;;; Prism focusing on Left values.
(define prism-left
  (make-prism
   (lambda (e) (if (left? e) (just (from-left e)) nothing))
   left))

;;; prism-right : Prism (Either a b) b
;;; Prism focusing on Right values.
(define prism-right
  (make-prism
   (lambda (e) (if (right? e) (just (from-right e)) nothing))
   right))

;;; ====
;;; Template Code Generation
;;; ====
;;;
;;; Generate code templates for common patterns.

;;; gen-monoid-template : Symbol → Symbol → Symbol → Sexp
;;; Generate monoid instance code template.
(define (gen-monoid-template type-name identity-expr append-expr)
  `(define ,(string->symbol (format "monoid-~a" type-name))
    (make-monoid ,identity-expr ,append-expr)))

;;; gen-functor-template : Symbol → Symbol → Sexp
;;; Generate functor instance code template.
(define (gen-functor-template type-name map-fn)
  `(define ,(string->symbol (format "functor-~a" type-name))
    (make-functor ,map-fn)))

;;; gen-lens-template : Symbol → Symbol → Sexp
;;; Generate lens code template for struct field.
(define (gen-lens-template struct-name field-name)
  (let ([getter (string->symbol (format "~a-~a" struct-name field-name))]
        [setter (string->symbol (format "set-~a-~a" struct-name field-name))])
       `(define ,(string->symbol (format "lens-~a-~a" struct-name field-name))
         (make-lens ,getter (lambda (v s) (,setter v s))))))

;;; ====
;;; Best Practice Templates
;;; ====

;;; new-type-checklist : List of things to implement for new type
(define new-type-checklist
  '(;; Core type class instances
    ("Functor" . "fmap for mapping over container")
    ("Foldable" . "foldr/foldl for reducing to value")
    ("Monoid" . "identity and combine operation if applicable")
    ("Applicative" . "pure and <*> for sequencing")
    
    ;; Access patterns
    ("Lenses" . "composable getters/setters for fields")
    ("Prisms" . "pattern matching for sum types")
    
    ;; Derivable
    ("Show" . "convert to string representation")
    ("Eq" . "structural equality")))

;;; pattern-catalog : Common FP patterns with templates
(define pattern-catalog
  `((fold-pattern
     "Reduce collection to single value"
     (lambda (f init xs)
             (if (null? xs)
                 init
                 (f (car xs) (fold-pattern f init (cdr xs))))))
    
    (map-pattern
     "Transform each element"
     (lambda (f xs)
             (if (null? xs)
                 '()
                 (cons (f (car xs)) (map-pattern f (cdr xs))))))
    
    (filter-pattern
     "Keep elements matching predicate"
     (lambda (p xs)
             (cond
              [(null? xs) '()]
              [(p (car xs)) (cons (car xs) (filter-pattern p (cdr xs)))]
              [else (filter-pattern p (cdr xs))])))
    
    (bind-pattern
     "Chain effectful computations"
     (lambda (mx f)
             (if (nothing? mx)
                 nothing
                 (f (from-just mx)))))
    
    (traverse-pattern
     "Map with effects and sequence"
     (lambda (f xs ap)
             (sequence-a ap (map f xs))))
    
    (lens-modify-pattern
     "Modify nested value"
     (lambda (lens f s)
             (set-lens lens (f (view lens s)) s)))))

;;; ====
;;; Exports
;;; ====
;;;
;;; Monoid:
;;;   - make-monoid, monoid?, monoid-mempty, monoid-mappend
;;;   - monoid-sum, monoid-product, monoid-list, monoid-string
;;;   - monoid-all, monoid-any, monoid-first, monoid-last
;;;   - monoid-max, monoid-min, monoid-endo
;;;   - mconcat, mtimes, verify-monoid-laws
;;;
;;; Foldable:
;;;   - make-foldable, foldable?, foldable-fold-map, foldable-foldr, foldable-foldl
;;;   - foldable-list, foldable-maybe
;;;   - fold-with, to-list, null-foldable?, length-foldable, elem-foldable
;;;
;;; Functor:
;;;   - make-functor, functor?, functor-fmap
;;;   - functor-list, functor-maybe, functor-either
;;;   - fmap-with, replace-with, void-with
;;;
;;; Applicative:
;;;   - make-applicative, applicative?, applicative-pure, applicative-ap
;;;   - applicative-list, applicative-maybe, applicative-either
;;;   - ap-with, lift-a2, sequence-a, traverse-a
;;;
;;; Lens:
;;;   - make-lens, lens?, lens-getter, lens-setter
;;;   - view, set-lens, over, lens-compose
;;;   - lens-fst, lens-snd, lens-head, lens-tail, lens-nth, lens-key
;;;
;;; Prism:
;;;   - make-prism, prism?, prism-match, prism-build
;;;   - preview, review
;;;   - prism-just, prism-left, prism-right
;;;
;;; Code Generation:
;;;   - gen-monoid-template, gen-functor-template, gen-lens-template
;;;   - new-type-checklist, pattern-catalog
