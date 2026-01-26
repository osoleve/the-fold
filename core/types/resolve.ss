(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")

(doc 'module 'resolve)
(doc 'description "Type Class Instance Resolution - resolves constraints to evidence dictionaries.")
(doc 'layer 'core)

(doc 'section 'instance-database)

(doc empty-idb 'type 'IDB)
(doc empty-idb 'description "Empty instance database.")
(doc empty-idb 'export #t)
(define empty-idb '())

(define (idb-add db instance)
  (doc 'type (-> IDB Instance IDB))
  (doc 'description "Add an instance to the database.")
  (doc 'export #t)
  (cons instance db))

(define (idb-add* db instances)
  (doc 'type (-> IDB (List Instance) IDB))
  (doc 'description "Add multiple instances to the database.")
  (doc 'export #t)
  (append instances db))

(doc 'section 'constraint-representation)

(define (constraint-class c)
  (doc 'type (-> Constraint Symbol))
  (doc 'description "Extract the class name from a constraint.")
  (doc 'export #t)
  (car c))

(define (constraint-type c)
  (doc 'type (-> Constraint Type))
  (doc 'description "Extract the type from a constraint.")
  (doc 'export #t)
  (cadr c))

(define (constraint? c)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if a value is a well-formed constraint (ClassName Type).")
  (doc 'export #t)
  (and (pair? c)
       (= (length c) 2)
       (symbol? (car c))))

(doc 'section 'matching-instances)

(define (match-instance inst constraint)
  (doc 'type (-> Instance Constraint (Maybe (List Symbol Subst Instance))))
  (doc 'description "Try to match an instance against a constraint via unification.")
  (doc 'export #f)
  (if (eq? (instance-class inst) (constraint-class constraint))
      (let ([result (unify (instance-type inst) (constraint-type constraint))])
           (if (eq? (car result) 'ok)
               `(match ,(cadr result) ,inst)
               #f))
      #f))

(define (find-matching-instances constraint db)
  (doc 'type (-> Constraint IDB (List Match)))
  (doc 'description "Find all instances in the database that match a constraint.")
  (doc 'export #f)
  (filter-map (lambda (inst) (match-instance inst constraint)) db))

;;; filter-map provided by prelude

(doc 'section 'instance-specificity)

(define (type-specificity type)
  (doc 'type (-> Type Nat))
  (doc 'description "Compute specificity score: concrete types score higher than type variables.")
  (doc 'export #f)
  (cond
   [(symbol? type)
    (if (base-type? type) 2 0)]
   [(not (pair? type)) 0]
   [(eq? (car type) '@)
    (+ 1 (apply + (map type-specificity (cddr type))))]
   [(eq? (car type) '->)
    (apply + (map type-specificity (cdr type)))]
   [else 0]))

(define (compare-specificity m1 m2)
  (doc 'type (-> Match Match Int))
  (doc 'description "Compare two matches by specificity: -1 if m1 more specific, 1 if m2, 0 if equal.")
  (doc 'export #f)
  (let* ([inst1 (caddr m1)]
         [inst2 (caddr m2)]
         [s1 (type-specificity (instance-type inst1))]
         [s2 (type-specificity (instance-type inst2))])
        (cond
         [(> s1 s2) -1]
         [(< s1 s2) 1]
         [else 0])))

(define (select-most-specific matches)
  (doc 'type (-> (List Match) Match))
  (doc 'description "Select the most specific match from a list of overlapping instances.")
  (doc 'export #f)
  (if (null? (cdr matches))
      (car matches)
      (fold-left (lambda (best m)
                         (if (< (compare-specificity m best) 0)
                             m
                             best))
                 (car matches)
                 (cdr matches))))

(doc 'section 'resolution-algorithm)

(define (resolve constraint db)
  (doc 'type (-> Constraint IDB (Result Evidence Error)))
  (doc 'description "Resolve a constraint to evidence: find matching instance, check context, build dictionary.")
  (doc 'export #t)
  (let ([matches (find-matching-instances constraint db)])
       (cond
        [(null? matches)
         `(error no-instance-found
           (constraint ,constraint)
           (suggestion ,(suggest-instance constraint)))]
        [(> (length matches) 1)
         (resolve-match (select-most-specific matches) db)]
        [else
         (resolve-match (car matches) db)])))

(define (suggest-instance constraint)
  (doc 'type (-> Constraint String))
  (doc 'description "Generate a helpful suggestion for a missing instance.")
  (doc 'export #f)
  (let ([class (constraint-class constraint)]
        [type (constraint-type constraint)])
       (format "Add instance ~a for ~a" class type)))

(define (resolve-match match db)
  (doc 'type (-> Match IDB (Result Evidence Error)))
  (doc 'description "Resolve a matched instance: apply substitution and resolve context.")
  (doc 'export #f)
  (let ([subst (cadr match)]
        [inst (caddr match)])
       (let ([context-result (resolve-context (instance-context inst) subst db)])
            (if (eq? (car context-result) 'ok)
                `(ok (evidence
                      ,(instance-class inst)
                      ,(apply-subst subst (instance-type inst))
                      ,(instance-methods inst)
                      ,(cadr context-result)))
                context-result))))

(define (resolve-context constraints subst db)
  (doc 'type (-> (List Constraint) Subst IDB (Result (List Evidence) Error)))
  (doc 'description "Resolve all constraints in the instance context (superclass constraints).")
  (doc 'export #f)
  (if (null? constraints)
      `(ok ())
      (let* ([c (car constraints)]
             [c* (list (constraint-class c) (apply-subst subst (constraint-type c)))]
             [result (resolve c* db)])
            (if (eq? (car result) 'ok)
                (let ([rest (resolve-context (cdr constraints) subst db)])
                     (if (eq? (car rest) 'ok)
                         `(ok ,(cons (cadr result) (cadr rest)))
                         rest))
                result))))

(doc 'section 'superclass-resolution)

(define (resolve-superclasses evidence class-db)
  (doc 'type (-> Evidence ClassDB Evidence))
  (doc 'description "Derive evidence for superclasses from a given evidence dictionary.")
  (doc 'export #f)
  (let* ([class-name (cadr evidence)]
         [class-def (lookup-class class-name class-db)])
        (if class-def
            (let ([supers (typeclass-supers class-def)])
                 evidence)
            evidence)))

(doc 'section 'class-database)

(define (lookup-class name class-db)
  (doc 'type (-> Symbol ClassDB (Maybe TypeClass)))
  (doc 'description "Look up a type class definition by name.")
  (doc 'export #f)
  (let ([entry (assq name class-db)])
       (if entry (cdr entry) #f)))

(doc standard-classes 'type 'ClassDB)
(doc standard-classes 'description "Standard class database: Functor, Applicative, Monad, Eq, Ord, Show, Pretty, Semigroup, Monoid.")
(doc standard-classes 'export #t)
(define standard-classes
  `((Functor . ,TC-Functor)
    (Applicative . ,TC-Applicative)
    (Monad . ,TC-Monad)
    (Eq . ,TC-Eq)
    (Ord . ,TC-Ord)
    (Show . ,TC-Show)
    (Pretty . ,TC-Pretty)
    (Semigroup . ,TC-Semigroup)
    (Monoid . ,TC-Monoid)))

(doc 'section 'standard-instances)

(define inst-Functor-List
  (make-instance 'Functor 'List '()
                 `((fmap . list-fmap))))

(define inst-Applicative-List
  (make-instance 'Applicative 'List '()
                 `((pure . list-pure)
                   (<*> . list-ap))))

(define inst-Monad-List
  (make-instance 'Monad 'List '()
                 `((>>= . list-bind)
                   (return . list-return))))

(define inst-Functor-Option
  (make-instance 'Functor 'Option '()
                 `((fmap . option-fmap))))

(define inst-Applicative-Option
  (make-instance 'Applicative 'Option '()
                 `((pure . option-pure)
                   (<*> . option-ap))))

(define inst-Monad-Option
  (make-instance 'Monad 'Option '()
                 `((>>= . option-bind)
                   (return . option-return))))

(define inst-Functor-Either
  (make-instance 'Functor '(@ Either e) '()
                 `((fmap . either-fmap))))

(define inst-Monad-Either
  (make-instance 'Monad '(@ Either e) '()
                 `((>>= . either-bind)
                   (return . either-return))))

(doc 'section 'eq-instances)

(define inst-Eq-Nat
  (make-instance 'Eq 'Nat '()
                 `((== . nat-eq)
                   (/= . nat-neq))))

(define inst-Eq-Int
  (make-instance 'Eq 'Int '()
                 `((== . int-eq)
                   (/= . int-neq))))

(define inst-Eq-Bool
  (make-instance 'Eq 'Bool '()
                 `((== . bool-eq)
                   (/= . bool-neq))))

(define inst-Eq-Char
  (make-instance 'Eq 'Char '()
                 `((== . char-eq)
                   (/= . char-neq))))

(define inst-Eq-String
  (make-instance 'Eq 'String '()
                 `((== . string-eq)
                   (/= . string-neq))))

(define inst-Eq-Symbol
  (make-instance 'Eq 'Symbol '()
                 `((== . symbol-eq)
                   (/= . symbol-neq))))

(define inst-Eq-List
  (make-instance 'Eq '(@ List a) '((Eq a))
                 `((== . list-eq)
                   (/= . list-neq))))

(define inst-Eq-Option
  (make-instance 'Eq '(@ Option a) '((Eq a))
                 `((== . option-eq)
                   (/= . option-neq))))

(doc 'section 'ord-instances)

(define inst-Ord-Nat
  (make-instance 'Ord 'Nat '()
                 `((compare . nat-compare)
                   (<  . nat-lt)
                   (<= . nat-lte)
                   (>  . nat-gt)
                   (>= . nat-gte))))

(define inst-Ord-Int
  (make-instance 'Ord 'Int '()
                 `((compare . int-compare)
                   (<  . int-lt)
                   (<= . int-lte)
                   (>  . int-gt)
                   (>= . int-gte))))

(define inst-Ord-Char
  (make-instance 'Ord 'Char '()
                 `((compare . char-compare)
                   (<  . char-lt)
                   (<= . char-lte)
                   (>  . char-gt)
                   (>= . char-gte))))

(define inst-Ord-String
  (make-instance 'Ord 'String '()
                 `((compare . string-compare)
                   (<  . string-lt)
                   (<= . string-lte)
                   (>  . string-gt)
                   (>= . string-gte))))

(doc 'section 'show-instances)

(define inst-Show-Nat
  (make-instance 'Show 'Nat '()
                 `((show . nat-show))))

(define inst-Show-Int
  (make-instance 'Show 'Int '()
                 `((show . int-show))))

(define inst-Show-Bool
  (make-instance 'Show 'Bool '()
                 `((show . bool-show))))

(define inst-Show-Char
  (make-instance 'Show 'Char '()
                 `((show . char-show))))

(define inst-Show-String
  (make-instance 'Show 'String '()
                 `((show . string-show))))

(define inst-Show-Symbol
  (make-instance 'Show 'Symbol '()
                 `((show . symbol-show))))

(define inst-Show-List
  (make-instance 'Show '(@ List a) '((Show a))
                 `((show . list-show))))

(doc 'section 'pretty-instances)

(define inst-Pretty-Nat
  (make-instance 'Pretty 'Nat '()
                 `((pretty . nat-pretty)
                   (pretty-prec . nat-pretty-prec))))

(define inst-Pretty-Int
  (make-instance 'Pretty 'Int '()
                 `((pretty . int-pretty)
                   (pretty-prec . int-pretty-prec))))

(define inst-Pretty-Bool
  (make-instance 'Pretty 'Bool '()
                 `((pretty . bool-pretty)
                   (pretty-prec . bool-pretty-prec))))

(define inst-Pretty-Char
  (make-instance 'Pretty 'Char '()
                 `((pretty . char-pretty)
                   (pretty-prec . char-pretty-prec))))

(define inst-Pretty-String
  (make-instance 'Pretty 'String '()
                 `((pretty . string-pretty)
                   (pretty-prec . string-pretty-prec))))

(define inst-Pretty-Symbol
  (make-instance 'Pretty 'Symbol '()
                 `((pretty . symbol-pretty)
                   (pretty-prec . symbol-pretty-prec))))

(define inst-Pretty-List
  (make-instance 'Pretty '(@ List a) '((Pretty a))
                 `((pretty . list-pretty)
                   (pretty-prec . list-pretty-prec))))

(doc 'section 'semigroup-monoid-instances)

(define inst-Semigroup-String
  (make-instance 'Semigroup 'String '()
                 `((<> . string-append))))

(define inst-Monoid-String
  (make-instance 'Monoid 'String '()
                 `((mempty . ""))))

(define inst-Semigroup-List
  (make-instance 'Semigroup '(@ List a) '()
                 `((<> . list-append))))

(define inst-Monoid-List
  (make-instance 'Monoid '(@ List a) '()
                 `((mempty . list-empty))))

(doc 'section 'standard-instance-database)

(doc standard-instances 'type '(List Instance))
(doc standard-instances 'description "Complete standard instance database for all built-in types.")
(doc standard-instances 'export #t)
(define standard-instances
  (list
   inst-Functor-List
   inst-Applicative-List
   inst-Monad-List
   inst-Functor-Option
   inst-Applicative-Option
   inst-Monad-Option
   inst-Functor-Either
   inst-Monad-Either
   inst-Eq-Nat
   inst-Eq-Int
   inst-Eq-Bool
   inst-Eq-Char
   inst-Eq-String
   inst-Eq-Symbol
   inst-Eq-List
   inst-Eq-Option
   inst-Ord-Nat
   inst-Ord-Int
   inst-Ord-Char
   inst-Ord-String
   inst-Show-Nat
   inst-Show-Int
   inst-Show-Bool
   inst-Show-Char
   inst-Show-String
   inst-Show-Symbol
   inst-Show-List
   inst-Pretty-Nat
   inst-Pretty-Int
   inst-Pretty-Bool
   inst-Pretty-Char
   inst-Pretty-String
   inst-Pretty-Symbol
   inst-Pretty-List
   inst-Semigroup-String
   inst-Monoid-String
   inst-Semigroup-List
   inst-Monoid-List))

(doc 'section 'convenience-api)

(define (resolve-std constraint)
  (doc 'type (-> Constraint (Result Evidence Error)))
  (doc 'description "Resolve using the standard instance database.")
  (doc 'export #t)
  (resolve constraint standard-instances))

(define (has-instance? constraint db)
  (doc 'type (-> Constraint IDB Boolean))
  (doc 'description "Check if a constraint can be resolved in the database.")
  (doc 'export #t)
  (let ([result (resolve constraint db)])
       (eq? (car result) 'ok)))

(define (get-method evidence method-name)
  (doc 'type (-> Evidence Symbol (Maybe Expr)))
  (doc 'description "Extract a method implementation from an evidence dictionary.")
  (doc 'export #t)
  (let* ([methods (cadddr evidence)]
         [entry (assq method-name methods)])
        (if entry (cdr entry) #f)))
