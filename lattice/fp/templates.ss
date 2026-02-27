;;; lattice/fp/templates.ss — FP Pattern Templates
;;; @module templates
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'templates)
(doc 'description "Template generator for common FP patterns including: Monoids, Foldable, Traversable, Lenses, Functors, Applicatives")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'features "- Template generators for type class instances
  - Code snippets with best-practice defaults
  - Customizable templates with hooks
  - Laws verification templates")
(doc 'dependencies '(core/base/prelude.ss lattice/fp/meta/combinators.ss))

(doc 'section 'monoid-templates)
;;; A monoid is a type with:
;;;   - mempty : M             (identity element)
;;;   - mappend : M → M → M    (associative binary operation)
;;;
;;; Laws:
;;;   - Left identity:  mappend mempty x = x
;;;   - Right identity: mappend x mempty = x
;;;   - Associativity:  mappend (mappend x y) z = mappend x (mappend y z)

(define (make-monoid mempty mappend)
  (doc 'type '(-> a (-> a a a) (Monoid a)))
  (doc 'description "Create a monoid from identity element and associative binary operation")
  (list 'monoid mempty mappend))

(define (monoid? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Monoid dictionary")
  (and (pair? x) (eq? (car x) 'monoid)))

(define (monoid-mempty m)
  (doc 'type '(-> (Monoid a) a))
  (doc 'description "Extract identity element from Monoid dictionary")
  (cadr m))

(define (monoid-mappend m)
  (doc 'type '(-> (Monoid a) (-> a a a)))
  (doc 'description "Extract binary operation from Monoid dictionary")
  (caddr m))

(doc 'section 'monoid-instances)

(doc monoid-sum 'type '(Monoid Number))
(doc monoid-sum 'description "Monoid for numbers under addition (identity: 0)")
(define monoid-sum
  (make-monoid 0 +))

(doc monoid-product 'type '(Monoid Number))
(doc monoid-product 'description "Monoid for numbers under multiplication (identity: 1)")
(define monoid-product
  (make-monoid 1 *))

(doc monoid-list 'type '(Monoid (List a)))
(doc monoid-list 'description "Monoid for lists under append (identity: empty list)")
(define monoid-list
  (make-monoid '() append))

(doc monoid-string 'type '(Monoid String))
(doc monoid-string 'description "Monoid for strings under concatenation (identity: empty string)")
(define monoid-string
  (make-monoid "" string-append))

(doc monoid-all 'type '(Monoid Boolean))
(doc monoid-all 'description "Monoid for booleans under AND (identity: #t)")
(define monoid-all
  (make-monoid #t (lambda (a b) (and a b))))

(doc monoid-any 'type '(Monoid Boolean))
(doc monoid-any 'description "Monoid for booleans under OR (identity: #f)")
(define monoid-any
  (make-monoid #f (lambda (a b) (or a b))))

(doc monoid-first 'type '(Monoid (Maybe a)))
(doc monoid-first 'description "Monoid taking first non-nothing value (identity: nothing)")
(define monoid-first
  (make-monoid nothing
               (lambda (a b) (if (just? a) a b))))

(doc monoid-last 'type '(Monoid (Maybe a)))
(doc monoid-last 'description "Monoid taking last non-nothing value (identity: nothing)")
(define monoid-last
  (make-monoid nothing
               (lambda (a b) (if (just? b) b a))))

(doc monoid-max 'type '(Monoid Number))
(doc monoid-max 'description "Monoid taking maximum (identity: -inf.0)")
(define monoid-max
  (make-monoid -inf.0 max))

(doc monoid-min 'type '(Monoid Number))
(doc monoid-min 'description "Monoid taking minimum (identity: +inf.0)")
(define monoid-min
  (make-monoid +inf.0 min))

(doc monoid-endo 'type '(Monoid (-> a a)))
(doc monoid-endo 'description "Monoid for endofunctions under composition (identity: id)")
(define monoid-endo
  (make-monoid identity compose2))

(doc 'section 'monoid-operations)

(define (mconcat m xs)
  (doc 'type '(-> (Monoid a) (List a) a))
  (doc 'description "Fold a list using the monoid's binary operation and identity")
  (fold-left (monoid-mappend m) (monoid-mempty m) xs))

(define (mtimes m n x)
  (doc 'type '(-> (Monoid a) Nat a a))
  (doc 'description "Combine value with itself n times using monoid")
  (if (<= n 0)
      (monoid-mempty m)
      (let loop ([n n] [acc x])
           (if (= n 1)
               acc
               (loop (- n 1) ((monoid-mappend m) acc x))))))

(doc 'section 'monoid-verification)

(define (verify-monoid-laws m test-values)
  (doc 'type '(-> (Monoid a) (List a) Boolean))
  (doc 'description "Check monoid laws (identity, associativity) hold for given test values")
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

(doc 'section 'foldable-templates)
;;; A foldable type can be reduced to a single value.
;;;   - foldMap : (a → m) → F a → m   (given Monoid m)
;;;   - foldr : (a → b → b) → b → F a → b

(define (make-foldable fold-map-fn foldr-fn foldl-fn)
  (doc 'type '(-> (-> (-> a m) (Monoid m) (f a) m) (-> (-> a b b) b (f a) b) (-> (-> b a b) b (f a) b) (Foldable f)))
  (doc 'description "Create a Foldable instance from fold-map, foldr, and foldl functions")
  (list 'foldable fold-map-fn foldr-fn foldl-fn))

(define (foldable? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Foldable dictionary")
  (and (pair? x) (eq? (car x) 'foldable)))

(define (foldable-fold-map f)
  (doc 'type '(-> (Foldable f) (-> (-> a m) (Monoid m) (f a) m)))
  (doc 'description "Extract fold-map function from Foldable dictionary")
  (cadr f))

(define (foldable-foldr f)
  (doc 'type '(-> (Foldable f) (-> (-> a b b) b (f a) b)))
  (doc 'description "Extract foldr function from Foldable dictionary")
  (caddr f))

(define (foldable-foldl f)
  (doc 'type '(-> (Foldable f) (-> (-> b a b) b (f a) b)))
  (doc 'description "Extract foldl function from Foldable dictionary")
  (cadddr f))

(doc 'section 'foldable-instances)

(doc foldable-list 'type '(Foldable List))
(doc foldable-list 'description "Foldable instance for lists")
(define foldable-list
  (make-foldable
   (lambda (f m xs)
           (fold-left (lambda (acc x) ((monoid-mappend m) acc (f x)))
                      (monoid-mempty m)
                      xs))
   fold-right
   fold-left))

(doc foldable-maybe 'type '(Foldable Maybe))
(doc foldable-maybe 'description "Foldable instance for Maybe")
(define foldable-maybe
  (make-foldable
   (lambda (f m mx)
           (if (just? mx)
               (f (from-just mx))
               (monoid-mempty m)))
   (lambda (f z mx)
           (if (just? mx)
               (f (from-just mx) z)
               z))
   (lambda (f z mx)
           (if (just? mx)
               (f z (from-just mx))
               z))))

(doc 'section 'foldable-operations)

(define (fold-with fld m container)
  (doc 'type '(-> (Foldable f) (Monoid a) (f a) a))
  (doc 'description "Fold container using monoid (fold-map with identity)")
  ((foldable-fold-map fld) identity m container))

(define (to-list fld container)
  (doc 'type '(-> (Foldable f) (f a) (List a)))
  (doc 'description "Convert any foldable container to a list")
  ((foldable-foldr fld) cons '() container))

(define (null-foldable? fld container)
  (doc 'type '(-> (Foldable f) (f a) Boolean))
  (doc 'description "Check if foldable container is empty")
  ((foldable-foldr fld) (lambda (x _) #f) #t container))

(define (length-foldable fld container)
  (doc 'type '(-> (Foldable f) (f a) Nat))
  (doc 'description "Count elements in foldable container")
  ((foldable-foldl fld) (lambda (n _) (+ n 1)) 0 container))

(define (elem-foldable fld x container)
  (doc 'type '(-> (Foldable f) a (f a) Boolean))
  (doc 'description "Check if element is in foldable container")
  ((foldable-foldr fld)
   (lambda (y found) (or found (equal? x y)))
   #f
   container))

(doc 'section 'traversable-templates)
;;; Traversable types can be traversed with effects.
;;;   - traverse : (a → F b) → T a → F (T b)
;;;   - sequenceA : T (F a) → F (T a)

(define (make-traversable traverse-fn)
  (doc 'type '(-> (-> (ApplicativeDict f) (-> a (f b)) (t a) (f (t b))) (Traversable t)))
  (doc 'description "Create a Traversable instance from a traverse function")
  (list 'traversable traverse-fn))

(define (traversable? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Traversable dictionary")
  (and (pair? x) (eq? (car x) 'traversable)))

(define (traversable-traverse t)
  (doc 'type '(-> (Traversable t) (-> (ApplicativeDict f) (-> a (f b)) (t a) (f (t b)))))
  (doc 'description "Extract traverse function from Traversable dictionary")
  (cadr t))

(doc 'section 'functor-templates)
;;; A functor allows mapping a function over a container.
;;;   - fmap : (a → b) × F(a) → F(b)
;;;
;;; Laws:
;;;   - Identity: fmap id = id
;;;   - Composition: fmap (g . f) = fmap g . fmap f
;;;
;;; Note: Unlike Haskell's curried fmap, our fmap takes TWO arguments:
;;;   ((functor-fmap F) func container)  ; correct - 2 args

(define (make-functor fmap-fn)
  (doc 'type '(-> (-> (-> a b) (f a) (f b)) (FunctorDict f)))
  (doc 'description "Create a Functor dictionary from a 2-argument fmap function")
  (list 'functor fmap-fn))

(define (make-named-functor name fmap-fn)
  (doc 'type '(-> Symbol (-> (-> a b) (f a) (f b)) (FunctorDict f)))
  (doc 'description "Create a named Functor dictionary for debugging")
  (list 'functor fmap-fn name))

(define (functor? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Functor dictionary")
  (and (pair? x) (eq? (car x) 'functor)))

(define (functor-fmap f)
  (doc 'type '(-> (FunctorDict f) (-> (-> a b) (f a) (f b))))
  (doc 'description "Extract fmap function from Functor dictionary")
  (cadr f))

(define (functor-name f . args)
  (doc 'type '(-> (FunctorDict f) Symbol))
  (doc 'description "Get the name of the functor, or default if unnamed")
  (let ([default (if (null? args) 'F (car args))])
    (if (and (functor? f) (> (length f) 2))
        (caddr f)
        default)))

(doc 'section 'functor-instances)

(doc functor-list 'type '(FunctorDict List))
(doc functor-list 'description "Functor instance for lists (fmap = map)")
(define functor-list
  (make-named-functor 'list map))

(doc functor-maybe 'type '(FunctorDict Maybe))
(doc functor-maybe 'description "Functor instance for Maybe")
(define functor-maybe
  (make-named-functor 'maybe maybe-fmap))

(doc functor-either 'type '(FunctorDict Either))
(doc functor-either 'description "Functor instance for Either (maps over Right)")
(define functor-either
  (make-named-functor 'either either-fmap))

(doc functor-id 'type '(FunctorDict Id))
(doc functor-id 'description "Identity functor; Id(f)(x) = f(x)")
(define functor-id
  (make-named-functor 'id (lambda (f x) (f x))))

(doc 'section 'functor-operations)

(define (fmap-with func f container)
  (doc 'type '(-> (FunctorDict t) (-> a b) (t a) (t b)))
  (doc 'description "Apply fmap using given functor dictionary")
  ((functor-fmap func) f container))

(define (replace-with func val container)
  (doc 'type '(-> (FunctorDict t) b (t a) (t b)))
  (doc 'description "Replace all values in container, keeping structure")
  (fmap-with func (const val) container))

(define (void-with func container)
  (doc 'type '(-> (FunctorDict t) (t a) (t Unit)))
  (doc 'description "Discard all values, keeping structure")
  (replace-with func '() container))

(doc 'section 'applicative-templates)
;;; Applicative functors allow applying functions in context.
;;;   - pure : a → F a
;;;   - <*> : F (a → b) → F a → F b
;;;
;;; Laws:
;;;   - Identity: pure id <*> v = v
;;;   - Composition: pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
;;;   - Homomorphism: pure f <*> pure x = pure (f x)
;;;   - Interchange: u <*> pure y = pure ($ y) <*> u

(define (make-applicative pure-fn ap-fn)
  (doc 'type '(-> (-> a (f a)) (-> (f (-> a b)) (f a) (f b)) (ApplicativeDict f)))
  (doc 'description "Create an Applicative dictionary from pure and ap functions")
  (list 'applicative pure-fn ap-fn))

(define (applicative? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is an Applicative dictionary")
  (and (pair? x) (eq? (car x) 'applicative)))

(define (applicative-pure ap)
  (doc 'type '(-> (ApplicativeDict f) (-> a (f a))))
  (doc 'description "Extract pure function from Applicative dictionary")
  (cadr ap))

(define (applicative-ap ap)
  (doc 'type '(-> (ApplicativeDict f) (-> (f (-> a b)) (f a) (f b))))
  (doc 'description "Extract ap (<*>) function from Applicative dictionary")
  (caddr ap))

(doc 'section 'applicative-instances)

(doc applicative-list 'type '(ApplicativeDict List))
(doc applicative-list 'description "Applicative for lists (all combinations / nondeterminism)")
(define applicative-list
  (make-applicative
   list
   (lambda (fs xs)
           (append-map (lambda (f) (map f xs)) fs))))

(doc applicative-maybe 'type '(ApplicativeDict Maybe))
(doc applicative-maybe 'description "Applicative for Maybe (short-circuits on nothing)")
(define applicative-maybe
  (make-applicative
   just
   (lambda (mf mx)
           (if (and (just? mf) (just? mx))
               (just ((from-just mf) (from-just mx)))
               nothing))))

(doc applicative-either 'type '(ApplicativeDict Either))
(doc applicative-either 'description "Applicative for Either (short-circuits on Left)")
(define applicative-either
  (make-applicative
   right
   (lambda (ef ex)
           (cond
            [(left? ef) ef]
            [(left? ex) ex]
            [else (right ((from-right ef) (from-right ex)))]))))

(doc 'section 'applicative-operations)

(define (ap-with ap ff fx)
  (doc 'type '(-> (ApplicativeDict f) (f (-> a b)) (f a) (f b)))
  (doc 'description "Apply wrapped function to wrapped value using Applicative dictionary")
  ((applicative-ap ap) ff fx))

(define (lift-a2 ap f fx fy)
  (doc 'type '(-> (ApplicativeDict f) (-> a b c) (f a) (f b) (f c)))
  (doc 'description "Lift binary function into applicative context: pure f <*> fx <*> fy")
  (let* ([pure (applicative-pure ap)]
         [ap-fn (applicative-ap ap)]
         [ff (ap-fn (pure f) fx)])
        (ap-fn ff fy)))

(define (sequence-a ap actions)
  (doc 'type '(-> (ApplicativeDict f) (List (f a)) (f (List a))))
  (doc 'description "Sequence list of applicative actions into action producing list")
  (let ([cons-curried (lambda (x) (lambda (xs) (cons x xs)))])
       (fold-right (lambda (action acc)
                           (lift-a2 ap cons-curried action acc))
                   ((applicative-pure ap) '())
                   actions)))

(define (traverse-a ap f xs)
  (doc 'type '(-> (ApplicativeDict f) (-> a (f b)) (List a) (f (List b))))
  (doc 'description "Map each element with an applicative action, then sequence")
  (sequence-a ap (map f xs)))

(doc 'section 'lens-templates)
;;; Lenses are composable first-class getters and setters.
;;;   - view : Lens s a → s → a
;;;   - set : Lens s a → a → s → s
;;;   - over : Lens s a → (a → a) → s → s
;;;
;;; Lens laws:
;;;   - Get-Put: set l (view l s) s = s
;;;   - Put-Get: view l (set l a s) = a
;;;   - Put-Put: set l a' (set l a s) = set l a' s

(define (make-lens getter setter)
  (doc 'type '(-> (-> s a) (-> a s s) (Lens s a)))
  (doc 'description "Create a lens from getter and setter functions")
  (list 'lens getter setter))

(define (lens? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Lens")
  (and (pair? x) (eq? (car x) 'lens)))

(define (lens-getter l)
  (doc 'type '(-> (Lens s a) (-> s a)))
  (doc 'description "Extract getter function from Lens")
  (cadr l))

(define (lens-setter l)
  (doc 'type '(-> (Lens s a) (-> a s s)))
  (doc 'description "Extract setter function from Lens")
  (caddr l))

(doc 'section 'lens-operations)

(define (view lens s)
  (doc 'type '(-> (Lens s a) s a))
  (doc 'description "Get the focused value through a lens")
  ((lens-getter lens) s))

(define (set-lens lens a s)
  (doc 'type '(-> (Lens s a) a s s))
  (doc 'description "Set the focused value through a lens")
  ((lens-setter lens) a s))

(define (over lens f s)
  (doc 'type '(-> (Lens s a) (-> a a) s s))
  (doc 'description "Modify the focused value by applying a function")
  (set-lens lens (f (view lens s)) s))

(define (lens-compose outer inner)
  (doc 'type '(-> (Lens s a) (Lens a b) (Lens s b)))
  (doc 'description "Compose two lenses; outer focuses on intermediate, inner focuses deeper")
  (make-lens
   (lambda (s)
           (view inner (view outer s)))
   (lambda (b s)
           (over outer (lambda (a) (set-lens inner b a)) s))))

(doc 'section 'lens-constructors)

(doc lens-fst 'type '(Lens (Pair a b) a))
(doc lens-fst 'description "Lens focusing on first element of pair")
(define lens-fst
  (make-lens car (lambda (a p) (cons a (cdr p)))))

(doc lens-snd 'type '(Lens (Pair a b) b))
(doc lens-snd 'description "Lens focusing on second element of pair")
(define lens-snd
  (make-lens cdr (lambda (b p) (cons (car p) b))))

(doc lens-head 'type '(Lens (List a) a))
(doc lens-head 'description "Lens focusing on head of list")
(define lens-head
  (make-lens car (lambda (a xs) (cons a (cdr xs)))))

(doc lens-tail 'type '(Lens (List a) (List a)))
(doc lens-tail 'description "Lens focusing on tail of list")
(define lens-tail
  (make-lens cdr (lambda (t xs) (cons (car xs) t))))

(define (lens-nth n)
  (doc 'type '(-> Nat (Lens (List a) a)))
  (doc 'description "Create a lens focusing on the nth element of a list")
  (make-lens
   (lambda (xs) (list-ref xs n))
   (lambda (a xs)
           (let loop ([i 0] [xs xs] [acc '()])
                (if (null? xs)
                    (reverse acc)
                    (if (= i n)
                        (loop (+ i 1) (cdr xs) (cons a acc))
                        (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))))

(define (lens-key key)
  (doc 'type '(-> Symbol (Lens (Alist Symbol v) v)))
  (doc 'description "Create a lens focusing on the value for a key in an association list")
  (make-lens
   (lambda (alist)
           (let ([entry (assq key alist)])
                (if entry (cdr entry) #f)))
   (lambda (v alist)
           (cons (cons key v)
                 (filter (lambda (p) (not (eq? (car p) key))) alist)))))

(doc 'section 'prism-templates)
;;; Prisms focus on parts of sum types (like case analysis).
;;;   - preview : Prism s a → s → Maybe a
;;;   - review : Prism s a → a → s

(define (make-prism match build)
  (doc 'type '(-> (-> s (Maybe a)) (-> a s) (Prism s a)))
  (doc 'description "Create a prism from match (partial getter) and build (constructor) functions")
  (list 'prism match build))

(define (prism? x)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Test if value is a Prism")
  (and (pair? x) (eq? (car x) 'prism)))

(define (prism-match p)
  (doc 'type '(-> (Prism s a) (-> s (Maybe a))))
  (doc 'description "Extract match function from Prism")
  (cadr p))

(define (prism-build p)
  (doc 'type '(-> (Prism s a) (-> a s)))
  (doc 'description "Extract build function from Prism")
  (caddr p))

(define (preview prism s)
  (doc 'type '(-> (Prism s a) s (Maybe a)))
  (doc 'description "Try to extract focused value from sum type via prism")
  ((prism-match prism) s))

(define (review prism a)
  (doc 'type '(-> (Prism s a) a s))
  (doc 'description "Construct sum type value from focused part via prism")
  ((prism-build prism) a))

(doc 'section 'prism-instances)

(doc prism-just 'type '(Prism (Maybe a) a))
(doc prism-just 'description "Prism focusing on Just values")
(define prism-just
  (make-prism
   (lambda (m) (if (just? m) m nothing))
   just))

(doc prism-left 'type '(Prism (Either a b) a))
(doc prism-left 'description "Prism focusing on Left values")
(define prism-left
  (make-prism
   (lambda (e) (if (left? e) (just (from-left e)) nothing))
   left))

(doc prism-right 'type '(Prism (Either a b) b))
(doc prism-right 'description "Prism focusing on Right values")
(define prism-right
  (make-prism
   (lambda (e) (if (right? e) (just (from-right e)) nothing))
   right))

(doc 'section 'code-generation)
;;; Generate code templates for common patterns.

(define (gen-monoid-template type-name identity-expr append-expr)
  (doc 'type '(-> Symbol Any Any Sexp))
  (doc 'description "Generate monoid instance code template as an S-expression")
  `(define ,(string->symbol (format "monoid-~a" type-name))
    (make-monoid ,identity-expr ,append-expr)))

(define (gen-functor-template type-name map-fn)
  (doc 'type '(-> Symbol Symbol Sexp))
  (doc 'description "Generate functor instance code template as an S-expression")
  `(define ,(string->symbol (format "functor-~a" type-name))
    (make-functor ,map-fn)))

(define (gen-lens-template struct-name field-name)
  (doc 'type '(-> Symbol Symbol Sexp))
  (doc 'description "Generate lens code template for a struct field as an S-expression")
  (let ([getter (string->symbol (format "~a-~a" struct-name field-name))]
        [setter (string->symbol (format "set-~a-~a" struct-name field-name))])
       `(define ,(string->symbol (format "lens-~a-~a" struct-name field-name))
         (make-lens ,getter (lambda (v s) (,setter v s))))))

(doc 'section 'best-practices)

(doc new-type-checklist 'type '(List (Pair String String)))
(doc new-type-checklist 'description "Checklist of type class instances to implement for a new type")
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

(doc pattern-catalog 'type '(List (List Symbol String Procedure)))
(doc pattern-catalog 'description "Catalog of common FP patterns with names, descriptions, and template implementations")
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
