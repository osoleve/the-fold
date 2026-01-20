(load "core/base/prelude.ss")
(load "core/types/types.ss")

(doc 'module 'kinds)
(doc 'description "Higher-Kinded Types for The Fold. Types have types - we call them Kinds: * for ordinary types, kind arrows for type constructors, and Constraint for type classes.")
(doc 'layer 'core)
(doc 'note "Notation: ⇒ for kind arrows, → for type arrows. Functor : (* → *) → Constraint")

(doc 'section 'kind-representation)

(doc K* 'type Kind)
(doc K* 'description "The base kind: ordinary types (Nat, Bool, List Nat).")
(doc K* 'export #t)
(define K* '*)

(doc K-constraint 'type Kind)
(doc K-constraint 'description "The constraint kind: type class evidence.")
(doc K-constraint 'export #t)
(define K-constraint 'Constraint)

(doc K-row 'type Kind)
(doc K-row 'description "The row kind: for extensible records/variants.")
(doc K-row 'export #t)
(define K-row 'Row)

(define (K=> k1 k2)
  (doc 'type (-> Kind Kind Kind))
  (doc 'description "Construct a kind arrow: K=> K* K* gives * → *.")
  (doc 'export #t)
  `(⇒ ,k1 ,k2))

(define (K=>* . kinds)
  (doc 'type (-> Kind ... Kind))
  (doc 'description "Multi-argument kind arrow (right-associative).")
  (doc 'export #t)
  (if (null? (cdr kinds))
      (car kinds)
      (K=> (car kinds) (apply K=>* (cdr kinds)))))

(define (K-forall vars body)
  (doc 'type (-> (List Symbol) Kind Kind))
  (doc 'description "Construct kind polymorphism: κ∀ (vars ...) body.")
  (doc 'export #t)
  `(κ∀ ,vars ,body))

(doc 'section 'dependent-kinds)

(define (K-pi var domain codomain)
  (doc 'type (-> Symbol Kind Kind Kind))
  (doc 'description "Construct a dependent kind: Π(var : domain). codomain.")
  (doc 'export #t)
  `(Πκ ((,var : ,domain)) ,codomain))

(define (K-sort . args)
  (doc 'type (-> (Maybe Nat) Kind))
  (doc 'description "Construct the sort (kind of kinds). Without arg: □, with n: □n.")
  (doc 'export #t)
  (if (null? args)
      '□
      (let ([level (car args)])
           (if (and (integer? level) (>= level 0))
               `(□ ,level)
               (error 'K-sort "level must be non-negative integer" level)))))

(define (sort? k)
  (doc 'type (-> Any Boolean))
  (doc 'description "Returns true if k is a sort (□ or □n).")
  (doc 'export #t)
  (or (eq? k '□)
      (and (pair? k)
           (eq? (car k) '□)
           (= (length k) 2)
           (integer? (cadr k))
           (>= (cadr k) 0))))

(define (sort-level k)
  (doc 'type (-> Kind Nat))
  (doc 'description "Extract the level from a sort. □ has level 0.")
  (doc 'export #t)
  (cond
   [(eq? k '□) 0]
   [(and (pair? k) (eq? (car k) '□)) (cadr k)]
   [else (error 'sort-level "not a sort" k)]))

(define (dep-kind? k)
  (doc 'type (-> Any Boolean))
  (doc 'description "Returns true only for Πκ (dependent) kinds.")
  (doc 'export #t)
  (and (pair? k)
       (eq? (car k) 'Πκ)
       (= (length k) 3)
       (let ([binding (cadr k)])
            (and (list? binding)
                 (= (length binding) 1)
                 (let ([b (car binding)])
                      (and (list? b)
                           (= (length b) 3)
                           (symbol? (car b))
                           (eq? (cadr b) ':)))))))

(define (dep-kind-var k)
  (doc 'type (-> Kind Symbol))
  (doc 'description "Extract the bound variable from a dependent kind.")
  (doc 'export #t)
  (if (dep-kind? k)
      (caar (cadr k))
      (error 'dep-kind-var "not a dependent kind" k)))

(define (dep-kind-domain k)
  (doc 'type (-> Kind Kind))
  (doc 'description "Extract the domain from a dependent kind.")
  (doc 'export #t)
  (if (dep-kind? k)
      (caddar (cadr k))
      (error 'dep-kind-domain "not a dependent kind" k)))

(define (dep-kind-codomain k)
  (doc 'type (-> Kind Kind))
  (doc 'description "Extract the codomain from a dependent kind.")
  (doc 'export #t)
  (if (dep-kind? k)
      (caddr k)
      (error 'dep-kind-codomain "not a dependent kind" k)))

(doc 'section 'kind-predicates)

(define (kind? k)
  (doc 'type (-> Any Boolean))
  (doc 'description "Test if value is a well-formed kind.")
  (doc 'export #t)
  (cond
   [(eq? k '*) #t]
   [(eq? k 'Constraint) #t]
   [(eq? k 'Row) #t]
   ;; Sort (kind of kinds) - bare □
   [(eq? k '□) #t]
   ;; Kind variable (κ-prefixed symbols)
   [(kind-var? k) #t]
   [(not (pair? k)) #f]
   ;; Kind arrow
   [(eq? (car k) '⇒)
    (and (= (length k) 3)
         (kind? (cadr k))
         (kind? (caddr k)))]
   ;; Kind polymorphism
   [(eq? (car k) 'κ∀)
    (and (= (length k) 3)
         (list? (cadr k))
         (andmap symbol? (cadr k))
         (kind? (caddr k)))]
   ;; Dependent kind: (Πκ ((var : domain)) codomain)
   [(eq? (car k) 'Πκ)
    (and (= (length k) 3)
         (dep-kind? k)  ; Validates structure
         (kind? (dep-kind-domain k))
         (kind? (dep-kind-codomain k)))]
   ;; Leveled sort: (□ n)
   [(eq? (car k) '□)
    (and (= (length k) 2)
         (integer? (cadr k))
         (>= (cadr k) 0))]
   [else #f]))

(define (kind-var? k)
  (doc 'type (-> Any Boolean))
  (doc 'description "Kind variables start with κ prefix.")
  (doc 'export #t)
  (and (symbol? k)
       (let ([s (symbol->string k)])
            (and (> (string-length s) 1)
                 (char=? (string-ref s 0) #\κ)))))

(define (kind-arrow? k)
  (doc 'type (-> Kind Boolean))
  (doc 'description "Test if kind is a kind arrow (⇒ k1 k2).")
  (doc 'export #t)
  (and (pair? k) (eq? (car k) '⇒)))

(define (kind-param k)
  (doc 'type (-> Kind (Maybe Kind)))
  (doc 'description "Extract parameter kind from kind arrow.")
  (doc 'export #t)
  (if (kind-arrow? k) (cadr k) #f))

(define (kind-result k)
  (doc 'type (-> Kind (Maybe Kind)))
  (doc 'description "Extract result kind from kind arrow.")
  (doc 'export #t)
  (if (kind-arrow? k) (caddr k) #f))

(doc 'section 'kind-equality)

(define (kind=? k1 k2)
  (doc 'type (-> Kind Kind Boolean))
  (doc 'description "Structural equality for kinds.")
  (doc 'export #t)
  (cond
   [(and (symbol? k1) (symbol? k2)) (eq? k1 k2)]
   [(and (pair? k1) (pair? k2))
    (and (= (length k1) (length k2))
         (andmap (lambda (pair) (kind=? (car pair) (cdr pair)))
                 (map cons k1 k2)))]
   [else #f]))

(doc 'section 'type-constructor-representation)

(define (T@ f . args)
  (doc 'type (-> Type Type ... Type))
  (doc 'description "Construct a type-level application: (@ F Arg1 Arg2 ...).")
  (doc 'export #t)
  `(@ ,f ,@args))

(define (type-app? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Test if type is a type application.")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) '@)))

(define (type-app-head t)
  (doc 'type (-> Type Type))
  (doc 'description "Extract the head from a type application.")
  (doc 'export #t)
  (if (type-app? t) (cadr t) t))

(define (type-app-args t)
  (doc 'type (-> Type (List Type)))
  (doc 'description "Extract the arguments from a type application.")
  (doc 'export #t)
  (if (type-app? t) (cddr t) '()))

(doc 'section 'builtin-type-constructor-kinds)

(doc builtin-kinds 'type (Alist Symbol Kind))
(doc builtin-kinds 'description "The kind environment for built-in type constructors.")
(doc builtin-kinds 'export #t)
(define builtin-kinds
  `((List    . ,(K=> K* K*))
    (Vector  . ,(K=> K* K*))
    (Option  . ,(K=> K* K*))
    (Set     . ,(K=> K* K*))
    (Ref     . ,(K=> K* K*))
    (Stream  . ,(K=> K* K*))          ; Stream : * → *
    (Delayed . ,(K=> K* K*))          ; Delayed : * → *
    (->      . ,(K=>* K* K* K*))      ; Binary for now
    (×       . ,(K=>* K* K* K*))      ; Binary for now
    (+       . ,(K=>* K* K* K*))      ; Binary for now
    (Either  . ,(K=>* K* K* K*))
    (Pair    . ,(K=>* K* K* K*))
    (Result  . ,(K=>* K* K* K*))
    (Block   . ,(K=> 'Symbol (K=> K* K*)))  ; Block : Symbol → * → *
    (Cap     . ,(K=> 'Symbol (K=> K* K*)))  ; Cap : Symbol → * → *
    ;; Base types have kind *
    (Nat     . ,K*)
    (Int     . ,K*)
    (Bool    . ,K*)
    (Symbol  . ,K*)
    (String  . ,K*)
    (Bytes   . ,K*)
    (Unit    . ,K*)
    (Void    . ,K*)
    (Hash    . ,K*)
    ;; Type system constructs (for annotations in dep-types.ss, dep-infer.ss)
    (DataType . ,K*)
    (Signature . ,K*)
    (Decl    . ,K*)
    (DataDecl . ,K*)
    (Constructor-Declaration . ,K*)
    (GADTDecl . ,K*)
    (Clause  . ,K*)
    (Context . ,K*)
    (Binding . ,K*)
    (A       . ,K*)   ; Type variable placeholder in annotations
    (Error   . ,K*))) ; Error type in Result

(define (lookup-kind name)
  (doc 'type (-> Symbol (Maybe Kind)))
  (doc 'description "Look up kind of a built-in type constructor.")
  (doc 'export #t)
  (let ([entry (assq name builtin-kinds)])
       (if entry (cdr entry) #f)))

(doc 'section 'kind-inference)

(define (infer-kind type kenv)
  (doc 'type (-> Type KindEnv (Either Kind Error)))
  (doc 'description "Infer the kind of a type expression.")
  (doc 'export #t)
  (cond
   ;; Type variable — look up in environment
   [(symbol? type)
    (let ([builtin (lookup-kind type)])
         (if builtin
             builtin
             (let ([env-entry (assq type kenv)])
                  (if env-entry
                      (cdr env-entry)
                      `(error unknown-type ,type)))))]
   
   ;; Hole — kind is unknown, represented as kind hole
   [(eq? type '?) 'κ?]
   [(and (pair? type) (eq? (car type) '?)) 'κ?]
   
   [(not (pair? type))
    `(error invalid-type ,type)]
   
   ;; Type application: (@ F Args...)
   [(eq? (car type) '@)
    (infer-app-kind (cadr type) (cddr type) kenv)]
   
   ;; Universal quantification: (∀ (vars) body)
   ;; Vars can be simple (kind *) or kinded: (name : kind)
   ;; Quantified type has kind * if body has kind *
   [(eq? (car type) '∀)
    (let* ([vars (cadr type)]
           [body (caddr type)]
           ;; Extract variable names and their kinds
           [var-kinds (map (lambda (v)
                                   (if (and (pair? v)
                                            (= (length v) 3)
                                            (eq? (cadr v) ':))
                                       ;; Kinded var: (name : kind)
                                       (cons (car v) (caddr v))
                                       ;; Simple var: assume kind *
                                       (cons v K*)))
                           vars)]
           ;; Extend environment with variable kinds
           [new-env (append var-kinds kenv)]
           [body-kind (infer-kind body new-env)])
          (if (kind=? body-kind K*)
              K*
              `(error quantified-body-not-type ,body-kind)))]
   
   ;; Recursive type: (μ var body)
   [(eq? (car type) 'μ)
    (let* ([var (cadr type)]
           [body (caddr type)]
           [new-env (cons (cons var K*) kenv)]
           [body-kind (infer-kind body new-env)])
          (if (kind=? body-kind K*)
              K*
              `(error recursive-body-not-type ,body-kind)))]
   
   ;; Function type: (-> T1 ... Tn Tresult)
   [(eq? (car type) '->)
    (let ([arg-kinds (map (lambda (t) (infer-kind t kenv)) (cdr type))])
         (if (andmap (lambda (k) (kind=? k K*)) arg-kinds)
             K*
             `(error function-args-not-types ,arg-kinds)))]
   
   ;; Product type: (× T1 ... Tn)
   [(eq? (car type) '×)
    (let ([elem-kinds (map (lambda (t) (infer-kind t kenv)) (cdr type))])
         (if (andmap (lambda (k) (kind=? k K*)) elem-kinds)
             K*
             `(error product-elems-not-types ,elem-kinds)))]
   
   ;; Sum type: (+ (Tag T...) ...)
   [(eq? (car type) '+)
    (let ([variant-kinds
           (map (lambda (variant)
                        (map (lambda (t) (infer-kind t kenv)) (cdr variant)))
                (cdr type))])
         (if (andmap (lambda (ks) (andmap (lambda (k) (kind=? k K*)) ks)) variant-kinds)
             K*
             `(error sum-variants-not-types ,variant-kinds)))]
   
   ;; List, Vector, etc. — sugar for application
   [(eq? (car type) 'List)
    (infer-app-kind 'List (cdr type) kenv)]
   [(eq? (car type) 'Vector)
    (infer-app-kind 'Vector (cdr type) kenv)]
   [(eq? (car type) 'Ref)
    (infer-app-kind 'Ref (cdr type) kenv)]
   [(eq? (car type) 'Block)
    (let ([tag-kind (if (symbol? (cadr type)) 'Symbol `(error expected-symbol ,(cadr type)))]
          [payload-kind (infer-kind (caddr type) kenv)])
         (if (and (eq? tag-kind 'Symbol) (kind=? payload-kind K*))
             K*
             `(error block-kind-error ,tag-kind ,payload-kind)))]
   [(eq? (car type) 'Cap)
    (let ([cap-kind (if (symbol? (cadr type)) 'Symbol `(error expected-symbol ,(cadr type)))]
          [inner-kind (infer-kind (caddr type) kenv)])
         (if (and (eq? cap-kind 'Symbol) (kind=? inner-kind K*))
             K*
             `(error cap-kind-error ,cap-kind ,inner-kind)))]
   
   [else `(error unknown-type-form ,type)]))

(define (infer-app-kind head args kenv)
  (doc 'type (-> Type (List Type) KindEnv (Either Kind Error)))
  (doc 'description "Infer the kind of a type application.")
  (let ([head-kind (infer-kind head kenv)])
       (if (and (pair? head-kind) (eq? (car head-kind) 'error))
           head-kind
           (apply-kinds head-kind args kenv))))

(define (apply-kinds kind args kenv)
  (doc 'type (-> Kind (List Type) KindEnv (Either Kind Error)))
  (doc 'description "Apply a kind to type arguments.")
  (if (null? args)
      kind
      (if (kind-arrow? kind)
          (let* ([param-kind (kind-param kind)]
                 [result-kind (kind-result kind)]
                 [arg-kind (infer-kind (car args) kenv)])
                (if (kind=? param-kind arg-kind)
                    (apply-kinds result-kind (cdr args) kenv)
                    `(error kind-mismatch
                      (expected ,param-kind)
                      (got ,arg-kind)
                      (in ,(car args)))))
          `(error not-a-type-constructor ,kind ,args))))

(doc 'section 'type-classes-higher-kinded)

(doc typeclass? 'type (-> Any Boolean))
(doc typeclass? 'description "Predicate for typeclass record.")
(doc typeclass-name 'type (-> TypeClass Symbol))
(doc typeclass-name 'description "Extract name from typeclass.")
(doc typeclass-kind 'type (-> TypeClass Kind))
(doc typeclass-kind 'description "Extract kind from typeclass.")
(doc typeclass-supers 'type (-> TypeClass (List Symbol)))
(doc typeclass-supers 'description "Extract superclass constraints.")
(doc typeclass-methods 'type (-> TypeClass (Alist Symbol TypeScheme)))
(doc typeclass-methods 'description "Extract method signatures.")
(define-record-type typeclass
  (fields
   name        ; Symbol
   kind        ; Kind (e.g., (* → *) → Constraint)
   supers      ; (List Symbol) — superclass constraints
   methods))   ; (List (name . type-scheme))

(doc TC-Functor 'type TypeClass)
(doc TC-Functor 'description "Functor : (* → *) → Constraint. Provides fmap.")
(doc TC-Functor 'export #t)
(define TC-Functor
  (make-typeclass
   'Functor
   (K=> (K=> K* K*) K-constraint)
   '()  ; no superclasses
   `((fmap . (∀ (f a b)
                (=> (Functor f)
                    (-> (-> a b) (@ f a) (@ f b))))))))

(doc TC-Applicative 'type TypeClass)
(doc TC-Applicative 'description "Applicative : (* → *) → Constraint. Requires Functor. Provides pure, <*>.")
(doc TC-Applicative 'export #t)
(define TC-Applicative
  (make-typeclass
   'Applicative
   (K=> (K=> K* K*) K-constraint)
   '(Functor)
   `((pure . (∀ (f a)
                (=> (Applicative f)
                    (-> a (@ f a)))))
     (<*>  . (∀ (f a b)
                (=> (Applicative f)
                    (-> (@ f (-> a b)) (@ f a) (@ f b))))))))

(doc TC-Monad 'type TypeClass)
(doc TC-Monad 'description "Monad : (* → *) → Constraint. Requires Applicative. Provides >>=, return.")
(doc TC-Monad 'export #t)
(define TC-Monad
  (make-typeclass
   'Monad
   (K=> (K=> K* K*) K-constraint)
   '(Applicative)
   `((>>= . (∀ (m a b)
               (=> (Monad m)
                   (-> (@ m a) (-> a (@ m b)) (@ m b)))))
     (return . (∀ (m a)
                  (=> (Monad m)
                      (-> a (@ m a))))))))

(doc 'section 'value-level-type-classes)

(doc TC-Eq 'type TypeClass)
(doc TC-Eq 'description "Eq : * → Constraint. Provides ==, /=.")
(doc TC-Eq 'export #t)
(define TC-Eq
  (make-typeclass
   'Eq
   (K=> K* K-constraint)
   '()
   `((== . (∀ (a)
              (=> (Eq a)
                  (-> a a Bool))))
     (/= . (∀ (a)
              (=> (Eq a)
                  (-> a a Bool)))))))

(doc TC-Ord 'type TypeClass)
(doc TC-Ord 'description "Ord : * → Constraint. Requires Eq. Provides compare, <, <=, >, >=.")
(doc TC-Ord 'export #t)
(define TC-Ord
  (make-typeclass
   'Ord
   (K=> K* K-constraint)
   '(Eq)
   `((compare . (∀ (a)
                   (=> (Ord a)
                       (-> a a Symbol))))  ; Returns 'LT, 'EQ, or 'GT
     (<  . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (<= . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (>  . (∀ (a) (=> (Ord a) (-> a a Bool))))
     (>= . (∀ (a) (=> (Ord a) (-> a a Bool)))))))

(doc TC-Show 'type TypeClass)
(doc TC-Show 'description "Show : * → Constraint. Provides show : a → String.")
(doc TC-Show 'export #t)
(define TC-Show
  (make-typeclass
   'Show
   (K=> K* K-constraint)
   '()
   `((show . (∀ (a)
                (=> (Show a)
                    (-> a String)))))))

(doc TC-Pretty 'type TypeClass)
(doc TC-Pretty 'description "Pretty : * → Constraint. Width-aware layout. Provides pretty, pretty-prec.")
(doc TC-Pretty 'export #t)
(define TC-Pretty
  (make-typeclass
   'Pretty
   (K=> K* K-constraint)
   '()
   `((pretty . (∀ (a)
                  (=> (Pretty a)
                      (-> a Doc))))
     (pretty-prec . (∀ (a)
                       (=> (Pretty a)
                           (-> Int a Doc)))))))

(doc TC-Semigroup 'type TypeClass)
(doc TC-Semigroup 'description "Semigroup : * → Constraint. Provides <>.")
(doc TC-Semigroup 'export #t)
(define TC-Semigroup
  (make-typeclass
   'Semigroup
   (K=> K* K-constraint)
   '()
   `((<> . (∀ (a)
              (=> (Semigroup a)
                  (-> a a a)))))))

(doc TC-Monoid 'type TypeClass)
(doc TC-Monoid 'description "Monoid : * → Constraint. Requires Semigroup. Provides mempty.")
(doc TC-Monoid 'export #t)
(define TC-Monoid
  (make-typeclass
   'Monoid
   (K=> K* K-constraint)
   '(Semigroup)
   `((mempty . (∀ (a)
                  (=> (Monoid a) a))))))

(doc 'section 'numeric-type-classes)

(doc TC-Num 'type TypeClass)
(doc TC-Num 'description "Num : * → Constraint. Core arithmetic: +, -, *, negate, abs, signum, fromInteger.")
(doc TC-Num 'export #t)
(define TC-Num
  (make-typeclass
   'Num
   (K=> K* K-constraint)
   '()
   `((+       . (∀ (a) (=> (Num a) (-> a a a))))
     (-       . (∀ (a) (=> (Num a) (-> a a a))))
     (*       . (∀ (a) (=> (Num a) (-> a a a))))
     (negate  . (∀ (a) (=> (Num a) (-> a a))))
     (abs     . (∀ (a) (=> (Num a) (-> a a))))
     (signum  . (∀ (a) (=> (Num a) (-> a a))))
     (fromInteger . (∀ (a) (=> (Num a) (-> Int a)))))))

(doc TC-Fractional 'type TypeClass)
(doc TC-Fractional 'description "Fractional : * → Constraint. Requires Num. Provides /, recip, fromRational.")
(doc TC-Fractional 'export #t)
(define TC-Fractional
  (make-typeclass
   'Fractional
   (K=> K* K-constraint)
   '(Num)
   `((/      . (∀ (a) (=> (Fractional a) (-> a a a))))
     (recip  . (∀ (a) (=> (Fractional a) (-> a a))))
     (fromRational . (∀ (a) (=> (Fractional a) (-> Rational a)))))))

(doc TC-Floating 'type TypeClass)
(doc TC-Floating 'description "Floating : * → Constraint. Requires Fractional. Transcendental functions.")
(doc TC-Floating 'export #t)
(define TC-Floating
  (make-typeclass
   'Floating
   (K=> K* K-constraint)
   '(Fractional)
   `((pi     . (∀ (a) (=> (Floating a) a)))
     (exp    . (∀ (a) (=> (Floating a) (-> a a))))
     (log    . (∀ (a) (=> (Floating a) (-> a a))))
     (sqrt   . (∀ (a) (=> (Floating a) (-> a a))))
     (**     . (∀ (a) (=> (Floating a) (-> a a a))))
     (sin    . (∀ (a) (=> (Floating a) (-> a a))))
     (cos    . (∀ (a) (=> (Floating a) (-> a a))))
     (tan    . (∀ (a) (=> (Floating a) (-> a a))))
     (asin   . (∀ (a) (=> (Floating a) (-> a a))))
     (acos   . (∀ (a) (=> (Floating a) (-> a a))))
     (atan   . (∀ (a) (=> (Floating a) (-> a a))))
     (sinh   . (∀ (a) (=> (Floating a) (-> a a))))
     (cosh   . (∀ (a) (=> (Floating a) (-> a a))))
     (tanh   . (∀ (a) (=> (Floating a) (-> a a))))
     (asinh  . (∀ (a) (=> (Floating a) (-> a a))))
     (acosh  . (∀ (a) (=> (Floating a) (-> a a))))
     (atanh  . (∀ (a) (=> (Floating a) (-> a a)))))))

(doc TC-Real 'type TypeClass)
(doc TC-Real 'description "Real : * → Constraint. Requires Num, Ord. Provides toRational.")
(doc TC-Real 'export #t)
(define TC-Real
  (make-typeclass
   'Real
   (K=> K* K-constraint)
   '(Num Ord)
   `((toRational . (∀ (a) (=> (Real a) (-> a Rational)))))))

(doc TC-Integral 'type TypeClass)
(doc TC-Integral 'description "Integral : * → Constraint. Requires Real. Provides quot, rem, div, mod, toInteger.")
(doc TC-Integral 'export #t)
(define TC-Integral
  (make-typeclass
   'Integral
   (K=> K* K-constraint)
   '(Real)
   `((quot     . (∀ (a) (=> (Integral a) (-> a a a))))
     (rem      . (∀ (a) (=> (Integral a) (-> a a a))))
     (div      . (∀ (a) (=> (Integral a) (-> a a a))))
     (mod      . (∀ (a) (=> (Integral a) (-> a a a))))
     (toInteger . (∀ (a) (=> (Integral a) (-> a Int)))))))

(doc 'section 'contravariant-and-bifunctors)

(doc TC-Contravariant 'type TypeClass)
(doc TC-Contravariant 'description "Contravariant : (* → *) → Constraint. Dual of Functor. Provides contramap.")
(doc TC-Contravariant 'export #t)
(define TC-Contravariant
  (make-typeclass
   'Contravariant
   (K=> (K=> K* K*) K-constraint)
   '()
   `((contramap . (∀ (f a b)
                     (=> (Contravariant f)
                         (-> (-> a b) (@ f b) (@ f a))))))))

(doc TC-Bifunctor 'type TypeClass)
(doc TC-Bifunctor 'description "Bifunctor : (* → * → *) → Constraint. Provides bimap, first, second.")
(doc TC-Bifunctor 'export #t)
(define TC-Bifunctor
  (make-typeclass
   'Bifunctor
   (K=> (K=>* K* K* K*) K-constraint)  ; (* → * → *) → Constraint
   '()
   `((bimap  . (∀ (p a b c d)
                  (=> (Bifunctor p)
                      (-> (-> a b) (-> c d) (@ (@ p a) c) (@ (@ p b) d)))))
     (first  . (∀ (p a b c)
                  (=> (Bifunctor p)
                      (-> (-> a b) (@ (@ p a) c) (@ (@ p b) c)))))
     (second . (∀ (p a c d)
                  (=> (Bifunctor p)
                      (-> (-> c d) (@ (@ p a) c) (@ (@ p a) d))))))))

(doc 'section 'category-theory-type-classes)

(doc TC-Category 'type TypeClass)
(doc TC-Category 'description "Category : (* → * → *) → Constraint. Provides id, ∘ (composition).")
(doc TC-Category 'export #t)
(define TC-Category
  (make-typeclass
   'Category
   (K=> (K=>* K* K* K*) K-constraint)  ; (* → * → *) → Constraint
   '()
   `((id . (∀ (cat a)
              (=> (Category cat)
                  (@ (@ cat a) a))))
     (∘  . (∀ (cat a b c)
              (=> (Category cat)
                  (-> (@ (@ cat b) c) (@ (@ cat a) b) (@ (@ cat a) c))))))))

(doc TC-Profunctor 'type TypeClass)
(doc TC-Profunctor 'description "Profunctor : (* → * → *) → Constraint. Provides dimap, lmap, rmap.")
(doc TC-Profunctor 'export #t)
(define TC-Profunctor
  (make-typeclass
   'Profunctor
   (K=> (K=>* K* K* K*) K-constraint)
   '()
   `((dimap . (∀ (p a b c d)
                 (=> (Profunctor p)
                     (-> (-> a b) (-> c d) (@ (@ p b) c) (@ (@ p a) d)))))
     (lmap  . (∀ (p a b c)
                 (=> (Profunctor p)
                     (-> (-> a b) (@ (@ p b) c) (@ (@ p a) c)))))
     (rmap  . (∀ (p a c d)
                 (=> (Profunctor p)
                     (-> (-> c d) (@ (@ p a) c) (@ (@ p a) d))))))))

(doc TC-Arrow 'type TypeClass)
(doc TC-Arrow 'description "Arrow : (* → * → *) → Constraint. Requires Category. Provides arr, first*, second*, ***, &&&.")
(doc TC-Arrow 'export #t)
(define TC-Arrow
  (make-typeclass
   'Arrow
   (K=> (K=>* K* K* K*) K-constraint)
   '(Category)
   `((arr    . (∀ (arr a b)
                  (=> (Arrow arr)
                      (-> (-> a b) (@ (@ arr a) b)))))
     (first* . (∀ (arr a b c)
                  (=> (Arrow arr)
                      (-> (@ (@ arr a) b) (@ (@ arr (× a c)) (× b c))))))
     (second* . (∀ (arr a b c)
                   (=> (Arrow arr)
                       (-> (@ (@ arr a) b) (@ (@ arr (× c a)) (× c b))))))
     (***    . (∀ (arr a b c d)
                  (=> (Arrow arr)
                      (-> (@ (@ arr a) b) (@ (@ arr c) d)
                          (@ (@ arr (× a c)) (× b d))))))
     (&&&   . (∀ (arr a b c)
                 (=> (Arrow arr)
                     (-> (@ (@ arr a) b) (@ (@ arr a) c)
                         (@ (@ arr a) (× b c)))))))))

(doc 'section 'multi-parameter-type-classes)

(doc mparam-typeclass? 'type (-> Any Boolean))
(doc mparam-typeclass-name 'type (-> MParamTypeClass Symbol))
(doc mparam-typeclass-params 'type (-> MParamTypeClass (List Symbol)))
(doc mparam-typeclass-param-kinds 'type (-> MParamTypeClass (List Kind)))
(doc mparam-typeclass-fundeps 'type (-> MParamTypeClass (List FunDep)))
(doc mparam-typeclass-supers 'type (-> MParamTypeClass (List Constraint)))
(doc mparam-typeclass-methods 'type (-> MParamTypeClass (Alist Symbol TypeScheme)))
(define-record-type mparam-typeclass
  (fields
   name        ; Symbol
   params      ; (List Symbol) — type parameters (e.g., (r m))
   param-kinds ; (List Kind) — kind of each parameter
   fundeps     ; (List (List Symbol × List Symbol)) — functional deps
   supers      ; (List Constraint)
   methods))   ; (List (name . type-scheme))

(define (fundep lhs rhs)
  (doc 'type (-> (List Symbol) (List Symbol) FunDep))
  (doc 'description "Create a functional dependency: lhs -> rhs.")
  (doc 'export #t)
  (cons lhs rhs))

(define (fundep-lhs fd)
  (doc 'type (-> FunDep (List Symbol)))
  (doc 'description "Extract LHS of functional dependency.")
  (doc 'export #t)
  (car fd))

(define (fundep-rhs fd)
  (doc 'type (-> FunDep (List Symbol)))
  (doc 'description "Extract RHS of functional dependency.")
  (doc 'export #t)
  (cdr fd))

(doc 'section 'multi-parameter-type-class-examples)

(doc TC-Convertible 'type MParamTypeClass)
(doc TC-Convertible 'description "Convertible a b: convert between types. No fundeps.")
(doc TC-Convertible 'export #t)
(define TC-Convertible
  (make-mparam-typeclass
   'Convertible
   '(a b)
   (list K* K*)
   '()  ; no functional dependencies
   '()
   `((convert . (∀ (a b)
                   (=> (Convertible a b)
                       (-> a b)))))))

(doc TC-Collection 'type MParamTypeClass)
(doc TC-Collection 'description "Collection c e | c -> e: collection type determines element type.")
(doc TC-Collection 'export #t)
(define TC-Collection
  (make-mparam-typeclass
   'Collection
   '(c e)
   (list K* K*)
   (list (fundep '(c) '(e)))  ; c determines e
   '()
   `((empty   . (∀ (c e)
                   (=> (Collection c e) c)))
     (insert  . (∀ (c e)
                   (=> (Collection c e)
                       (-> e c c))))
     (member? . (∀ (c e)
                   (=> ((Collection c e) (Eq e))
                       (-> e c Bool)))))))

(doc TC-MonadReader 'type MParamTypeClass)
(doc TC-MonadReader 'description "MonadReader r m | m -> r: monad determines environment type.")
(doc TC-MonadReader 'export #t)
(define TC-MonadReader
  (make-mparam-typeclass
   'MonadReader
   '(r m)
   (list K* (K=> K* K*))  ; r : *, m : * → *
   (list (fundep '(m) '(r)))
   '(Monad)
   `((ask   . (∀ (r m)
                 (=> (MonadReader r m)
                     (@ m r))))
     (local . (∀ (r m a)
                 (=> (MonadReader r m)
                     (-> (-> r r) (@ m a) (@ m a))))))))

(doc TC-MonadState 'type MParamTypeClass)
(doc TC-MonadState 'description "MonadState s m | m -> s: monad determines state type.")
(doc TC-MonadState 'export #t)
(define TC-MonadState
  (make-mparam-typeclass
   'MonadState
   '(s m)
   (list K* (K=> K* K*))
   (list (fundep '(m) '(s)))
   '(Monad)
   `((get    . (∀ (s m)
                  (=> (MonadState s m)
                      (@ m s))))
     (put    . (∀ (s m)
                  (=> (MonadState s m)
                      (-> s (@ m Unit)))))
     (modify . (∀ (s m)
                  (=> (MonadState s m)
                      (-> (-> s s) (@ m Unit))))))))

(doc TC-MonadWriter 'type MParamTypeClass)
(doc TC-MonadWriter 'description "MonadWriter w m | m -> w: monad determines log type. Requires Monoid.")
(doc TC-MonadWriter 'export #t)
(define TC-MonadWriter
  (make-mparam-typeclass
   'MonadWriter
   '(w m)
   (list K* (K=> K* K*))
   (list (fundep '(m) '(w)))
   '(Monad Monoid)  ; w must be a Monoid
   `((tell   . (∀ (w m)
                  (=> (MonadWriter w m)
                      (-> w (@ m Unit)))))
     (listen . (∀ (w m a)
                  (=> (MonadWriter w m)
                      (-> (@ m a) (@ m (× a w))))))
     (pass   . (∀ (w m a)
                  (=> (MonadWriter w m)
                      (-> (@ m (× a (-> w w))) (@ m a))))))))

(doc 'section 'functional-dependency-utilities)

(define (apply-fundep fd subst)
  (doc 'type (-> FunDep TypeSubst TypeSubst))
  (doc 'description "Given known type bindings, derive new bindings from fundep.")
  (doc 'export #t)
  ;; If all LHS params are bound in subst, RHS params
  ;; can be determined (in actual resolution, looked up from instance)
  (let* ([lhs (fundep-lhs fd)]
         [rhs (fundep-rhs fd)]
         [lhs-bound? (andmap (lambda (p) (assq p subst)) lhs)])
        (if lhs-bound?
            ;; LHS determines RHS — mark RHS as determinable
            (map (lambda (p) (cons p 'determined)) rhs)
            '())))

(doc fundeps-closure 'type (-> (List FunDep) (Set Symbol) (Set Symbol)))
(doc fundeps-closure 'description "Compute the closure of known parameters under functional deps.")
(doc fundeps-closure 'export #t)
(define (fundeps-closure fundeps known)
  (define (add-new acc new-elems)
    (fold-left (lambda (a e)
                       (if (memq e a) a (cons e a)))
               acc
               new-elems))
  (let loop ([known known])
       (let ([new-known
              (fold-left
               (lambda (acc fd)
                       (if (andmap (lambda (p) (memq p acc)) (fundep-lhs fd))
                           (add-new acc (fundep-rhs fd))
                           acc))
               known
               fundeps)])
            (if (= (length new-known) (length known))
                known
                (loop new-known)))))

(define (check-fundep-consistency fundeps inst1 inst2)
  (doc 'type (-> (List FunDep) Instance Instance Boolean))
  (doc 'description "Check that two instances don't violate functional dependencies.")
  (doc 'export #t)
  #t)

(doc 'section 'type-class-instances)

(doc instance? 'type (-> Any Boolean))
(doc instance-class 'type (-> Instance Symbol))
(doc instance-type 'type (-> Instance Type))
(doc instance-context 'type (-> Instance (List Constraint)))
(doc instance-methods 'type (-> Instance (Alist Symbol Expr)))
(doc 'note "Instance implementations deferred until evaluator supports type class dictionaries.")
(define-record-type instance
  (fields
   class       ; Symbol — which type class
   type        ; Type — for which type (e.g., List, (@ Either e))
   context     ; (List Constraint) — required constraints
   methods))   ; (List (name . expr))

(doc 'section 'constrained-types)

(define (t=> constraints type)
  (doc 'type (-> (List Constraint) Type Type))
  (doc 'description "Construct constrained type: (=> (constraints) type).")
  (doc 'export #t)
  `(=> ,constraints ,type))

(define (constrained-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Test if type has constraints.")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) '=>)))

(define (get-constraints t)
  (doc 'type (-> Type (List Constraint)))
  (doc 'description "Extract constraints from constrained type.")
  (doc 'export #t)
  (if (constrained-type? t)
      (cadr t)
      '()))

(define (get-underlying-type t)
  (doc 'type (-> Type Type))
  (doc 'description "Extract underlying type from constrained type.")
  (doc 'export #t)
  (if (constrained-type? t)
      (caddr t)
      t))

(doc 'section 'kind-unification)

(define (kind-subst-lookup s var)
  (doc 'type (-> KindSubst Symbol (Maybe Kind)))
  (doc 'description "Look up kind variable in substitution.")
  (doc 'export #t)
  (let ([entry (assq var s)])
       (if entry (cdr entry) #f)))

(define (kind-subst-extend s var kind)
  (doc 'type (-> KindSubst Symbol Kind KindSubst))
  (doc 'description "Extend kind substitution with new binding.")
  (doc 'export #t)
  (cons (cons var kind) s))

(define (apply-kind-subst s kind)
  (doc 'type (-> KindSubst Kind Kind))
  (doc 'description "Apply kind substitution to a kind.")
  (doc 'export #t)
  (cond
   [(kind-var? kind)
    (let ([replacement (kind-subst-lookup s kind)])
         (if replacement
             (apply-kind-subst s replacement)
             kind))]
   [(not (pair? kind)) kind]
   [(eq? (car kind) '⇒)
    (K=> (apply-kind-subst s (cadr kind))
         (apply-kind-subst s (caddr kind)))]
   [(eq? (car kind) 'κ∀)
    ;; Remove bound vars from substitution
    (let* ([bound (cadr kind)]
           [body (caddr kind)]
           [s* (filter (lambda (p) (not (memq (car p) bound))) s)])
          `(κ∀ ,bound ,(apply-kind-subst s* body)))]
   [else kind]))

(define (kind-occurs? var kind)
  (doc 'type (-> Symbol Kind Boolean))
  (doc 'description "Check if kind variable occurs in kind (for occurs check).")
  (doc 'export #t)
  (cond
   [(kind-var? kind) (eq? var kind)]
   [(not (pair? kind)) #f]
   [(eq? (car kind) '⇒)
    (or (kind-occurs? var (cadr kind))
        (kind-occurs? var (caddr kind)))]
   [(eq? (car kind) 'κ∀)
    (if (memq var (cadr kind))
        #f
        (kind-occurs? var (caddr kind)))]
   [else #f]))

(define (unify-kinds k1 k2)
  (doc 'type (-> Kind Kind (Either KindSubst Error)))
  (doc 'description "Unify two kinds, returning a substitution.")
  (doc 'export #t)
  (unify-kinds-with '() k1 k2))

(define (unify-kinds-with s k1 k2)
  (doc 'type (-> KindSubst Kind Kind (Either KindSubst Error)))
  (doc 'description "Unify kinds with an existing substitution.")
  (let ([k1 (apply-kind-subst s k1)]
        [k2 (apply-kind-subst s k2)])
       (cond
        ;; Same kind
        [(kind=? k1 k2) `(ok ,s)]
        
        ;; Kind variable on left
        [(kind-var? k1)
         (if (kind-occurs? k1 k2)
             `(error kind-occurs-check ,k1 ,k2)
             `(ok ,(kind-subst-extend s k1 k2)))]
        
        ;; Kind variable on right
        [(kind-var? k2)
         (if (kind-occurs? k2 k1)
             `(error kind-occurs-check ,k2 ,k1)
             `(ok ,(kind-subst-extend s k2 k1)))]
        
        ;; Both are kind arrows
        [(and (kind-arrow? k1) (kind-arrow? k2))
         (let ([result (unify-kinds-with s (kind-param k1) (kind-param k2))])
              (if (eq? (car result) 'ok)
                  (unify-kinds-with (cadr result) (kind-result k1) (kind-result k2))
                  result))]
        
        [else `(error kind-mismatch ,k1 ,k2)])))

(doc 'section 'kind-display)

(define (kind->string k)
  (doc 'type (-> Kind String))
  (doc 'description "Convert kind to string for display.")
  (doc 'export #t)
  (cond
   [(eq? k '*) "*"]
   [(eq? k 'Constraint) "Constraint"]
   [(eq? k 'Row) "Row"]
   ;; Sort (kind of kinds)
   [(eq? k '□) "[]"]
   [(and (pair? k) (eq? (car k) '□))
    (string-append "[]" (number->string (cadr k)))]
   ;; Kind variable
   [(kind-var? k) (symbol->string k)]
   ;; Kind arrow
   [(kind-arrow? k)
    (let ([param (kind->string (kind-param k))]
          [result (kind->string (kind-result k))])
         (if (kind-arrow? (kind-param k))
             (string-append "(" param ") ⇒ " result)
             (string-append param " ⇒ " result)))]
   ;; Dependent kind: Pi(x : K). K'
   [(dep-kind? k)
    (let ([var (symbol->string (dep-kind-var k))]
          [domain (kind->string (dep-kind-domain k))]
          [codomain (kind->string (dep-kind-codomain k))])
         (string-append "Pi(" var " : " domain "). " codomain))]
   ;; Arbitrary symbol (used in dependent kind domains like 'Nat)
   [(symbol? k) (symbol->string k)]
   [(pair? k) (format "~s" k)]
   [else "?"]))

(doc 'note "Utilities (andmap, etc.) are provided by prelude.ss")
