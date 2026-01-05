;;; core/types/resolve.ss — Type Class Instance Resolution
;;; @module resolve
;;; @requires prelude types kinds
;;;
;;; When we see (fmap f xs) and xs : List Nat, we need to find
;;; the Functor instance for List and extract its fmap method.
;;;
;;; Resolution answers: Given a constraint, produce evidence.
;;;   resolve : Constraint × InstanceDB → Evidence | Error
;;;
;;; Evidence is a dictionary of method implementations, allowing
;;; the evaluator to dispatch to the correct implementation.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - types.ss
;;;   - kinds.ss

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")

;;; ============================================================
;;; Instance Database
;;; ============================================================

;;; An instance database maps (class, type) pairs to instance records.
;;; We use a simple list for now; production would use a trie.

(define empty-idb '())

(define (idb-add db instance)
  (cons instance db))

(define (idb-add* db instances)
  (append instances db))

;;; ============================================================
;;; Constraint Representation
;;; ============================================================

;;; A constraint is (ClassName Type)
;;; Examples:
;;;   (Functor List)
;;;   (Monad (@ Either String))
;;;   (Ord a)

(define (constraint-class c) (car c))
(define (constraint-type c) (cadr c))

(define (constraint? c)
  (and (pair? c)
       (= (length c) 2)
       (symbol? (car c))))

;;; ============================================================
;;; Matching Instances
;;; ============================================================

;;; An instance matches a constraint if:
;;; 1. The class names match
;;; 2. The instance type unifies with the constraint type

;;; match-instance : Instance × Constraint → (Option Subst)
(define (match-instance inst constraint)
  (if (eq? (instance-class inst) (constraint-class constraint))
      (let ([result (unify (instance-type inst) (constraint-type constraint))])
           (if (eq? (car result) 'ok)
               `(match ,(cadr result) ,inst)
               #f))
      #f))

;;; find-matching-instances : Constraint × IDB → (List (Subst × Instance))
(define (find-matching-instances constraint db)
  (filter-map (lambda (inst) (match-instance inst constraint)) db))

(define (filter-map f lst)
  (if (null? lst)
      '()
      (let ([result (f (car lst))])
           (if result
               (cons result (filter-map f (cdr lst)))
               (filter-map f (cdr lst))))))

;;; ============================================================
;;; Instance Specificity
;;; ============================================================

;;; A type T1 is more specific than T2 if T1 matches fewer types.
;;; Examples:
;;;   Int is more specific than a (type variable)
;;;   (List Int) is more specific than (List a)
;;;   (Either String a) is more specific than (Either e a)
;;;
;;; We use a scoring system:
;;;   - Concrete types (Int, Bool, etc.) add specificity
;;;   - Type constructors add partial specificity
;;;   - Type variables add nothing

(define (type-specificity type)
  (cond
   [(symbol? type)
    ;; Check if it's a base type or type variable
    (if (base-type? type) 2 0)]
   [(not (pair? type)) 0]
   [(eq? (car type) '@)
    ;; Type application: (@ F args...)
    ;; Score the constructor and arguments
    (+ 1 (apply + (map type-specificity (cddr type))))]
   [(eq? (car type) '->)
    ;; Function type
    (apply + (map type-specificity (cdr type)))]
   [else 0]))

;;; compare-specificity : Match × Match → -1 | 0 | 1
;;; Returns -1 if m1 is more specific, 1 if m2 is more specific, 0 if equal
(define (compare-specificity m1 m2)
  (let* ([inst1 (caddr m1)]
         [inst2 (caddr m2)]
         [s1 (type-specificity (instance-type inst1))]
         [s2 (type-specificity (instance-type inst2))])
        (cond
         [(> s1 s2) -1]
         [(< s1 s2) 1]
         [else 0])))

;;; select-most-specific : (List Match) → Match
;;; Select the most specific matching instance.
(define (select-most-specific matches)
  (if (null? (cdr matches))
      (car matches)
      (fold-left (lambda (best m)
                         (if (< (compare-specificity m best) 0)
                             m
                             best))
                 (car matches)
                 (cdr matches))))

;;; ============================================================
;;; Resolution Algorithm
;;; ============================================================

;;; resolve : Constraint × IDB → (Result Evidence Error)
;;;
;;; Evidence is:
;;;   (evidence class type methods context-evidence)
;;;
;;; Where methods is an alist of (name . implementation)
;;; and context-evidence is evidence for superclass constraints.

(define (resolve constraint db)
  (let ([matches (find-matching-instances constraint db)])
       (cond
        [(null? matches)
         `(error no-instance-found
           (constraint ,constraint)
           (suggestion ,(suggest-instance constraint)))]
        [(> (length matches) 1)
         ;; Overlapping instances — select most specific
         (resolve-match (select-most-specific matches) db)]
        [else
         (resolve-match (car matches) db)])))

;;; suggest-instance : Constraint → String
;;; Suggest how to add the missing instance.
(define (suggest-instance constraint)
  (let ([class (constraint-class constraint)]
        [type (constraint-type constraint)])
       (format "Add instance ~a for ~a" class type)))

;;; resolve-match : Match × IDB → (Result Evidence Error)
(define (resolve-match match db)
  (let ([subst (cadr match)]
        [inst (caddr match)])
       ;; Check instance context (superclass constraints)
       (let ([context-result (resolve-context (instance-context inst) subst db)])
            (if (eq? (car context-result) 'ok)
                `(ok (evidence
                      ,(instance-class inst)
                      ,(apply-subst subst (instance-type inst))
                      ,(instance-methods inst)
                      ,(cadr context-result)))
                context-result))))

;;; resolve-context : (List Constraint) × Subst × IDB → (Result (List Evidence) Error)
(define (resolve-context constraints subst db)
  (if (null? constraints)
      `(ok ())
      (let* ([c (car constraints)]
             ;; Apply substitution to constraint
             [c* (list (constraint-class c) (apply-subst subst (constraint-type c)))]
             [result (resolve c* db)])
            (if (eq? (car result) 'ok)
                (let ([rest (resolve-context (cdr constraints) subst db)])
                     (if (eq? (car rest) 'ok)
                         `(ok ,(cons (cadr result) (cadr rest)))
                         rest))
                result))))

;;; ============================================================
;;; Superclass Resolution
;;; ============================================================

;;; Given evidence for Monad f, we can derive evidence for
;;; Applicative f and Functor f.

(define (resolve-superclasses evidence class-db)
  (let* ([class-name (cadr evidence)]  ; evidence structure: (evidence class type methods ctx)
         [class-def (lookup-class class-name class-db)])
        (if class-def
            (let ([supers (typeclass-supers class-def)])
                 ;; Recursively resolve superclasses
                 ;; (For now, we assume context-evidence already has them)
                 evidence)
            evidence)))

;;; ============================================================
;;; Class Database
;;; ============================================================

;;; Lookup a type class definition by name.
(define (lookup-class name class-db)
  (let ([entry (assq name class-db)])
       (if entry (cdr entry) #f)))

;;; Standard class database
(define standard-classes
  `((Functor . ,TC-Functor)
    (Applicative . ,TC-Applicative)
    (Monad . ,TC-Monad)
    (Eq . ,TC-Eq)
    (Ord . ,TC-Ord)
    (Show . ,TC-Show)
    (Semigroup . ,TC-Semigroup)
    (Monoid . ,TC-Monoid)))

;;; ============================================================
;;; Standard Instances
;;; ============================================================

;;; List instances
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

;;; Option instances
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

;;; Either instances (parameterized)
;;; (Functor (@ Either e)) for any e
(define inst-Functor-Either
  (make-instance 'Functor '(@ Either e) '()
                 `((fmap . either-fmap))))

(define inst-Monad-Either
  (make-instance 'Monad '(@ Either e) '()
                 `((>>= . either-bind)
                   (return . either-return))))

;;; ============================================================
;;; Eq Instances
;;; ============================================================

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

;;; Eq (List a) requires Eq a
(define inst-Eq-List
  (make-instance 'Eq '(@ List a) '((Eq a))
                 `((== . list-eq)
                   (/= . list-neq))))

;;; Eq (Option a) requires Eq a
(define inst-Eq-Option
  (make-instance 'Eq '(@ Option a) '((Eq a))
                 `((== . option-eq)
                   (/= . option-neq))))

;;; ============================================================
;;; Ord Instances
;;; ============================================================

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

;;; ============================================================
;;; Show Instances
;;; ============================================================

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

;;; Show (List a) requires Show a
(define inst-Show-List
  (make-instance 'Show '(@ List a) '((Show a))
                 `((show . list-show))))

;;; ============================================================
;;; Semigroup and Monoid Instances
;;; ============================================================

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

;;; ============================================================
;;; Standard Instance Database
;;; ============================================================

(define standard-instances
  (list
   ;; Functor/Applicative/Monad
   inst-Functor-List
   inst-Applicative-List
   inst-Monad-List
   inst-Functor-Option
   inst-Applicative-Option
   inst-Monad-Option
   inst-Functor-Either
   inst-Monad-Either
   ;; Eq
   inst-Eq-Nat
   inst-Eq-Int
   inst-Eq-Bool
   inst-Eq-Char
   inst-Eq-String
   inst-Eq-Symbol
   inst-Eq-List
   inst-Eq-Option
   ;; Ord
   inst-Ord-Nat
   inst-Ord-Int
   inst-Ord-Char
   inst-Ord-String
   ;; Show
   inst-Show-Nat
   inst-Show-Int
   inst-Show-Bool
   inst-Show-Char
   inst-Show-String
   inst-Show-Symbol
   inst-Show-List
   ;; Semigroup/Monoid
   inst-Semigroup-String
   inst-Monoid-String
   inst-Semigroup-List
   inst-Monoid-List))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; resolve-std : Constraint → (Result Evidence Error)
;;; Resolve using standard instances.
(define (resolve-std constraint)
  (resolve constraint standard-instances))

;;; has-instance? : Constraint × IDB → Boolean
(define (has-instance? constraint db)
  (let ([result (resolve constraint db)])
       (eq? (car result) 'ok)))

;;; get-method : Evidence × Symbol → Expr | #f
;;; Extract a method from evidence.
(define (get-method evidence method-name)
  (let* ([methods (cadddr evidence)]  ; (evidence class type methods ctx)
         [entry (assq method-name methods)])
        (if entry (cdr entry) #f)))

;;; ============================================================
;;; Multi-Parameter Type Classes (Future)
;;; ============================================================

;;; For constraints like (Convertible a b), we need to match on
;;; multiple type parameters. The current design supports this
;;; since constraint-type can be any type, including tuples:
;;;   (Convertible (× a b))
;;; But the syntax is awkward. Future work: explicit MPTC support.

;;; ============================================================
;;; Functional Dependencies (Future)
;;; ============================================================

;;; For MonadState s m, the state type s is determined by m.
;;; Functional dependencies: m → s.
;;; This affects instance selection and type inference.
;;; Not yet implemented.

;;; ============================================================
;;; Deriving (Future)
;;; ============================================================

;;; Automatically derive instances for algebraic data types.
;;; deriving Functor for (data Maybe (a) (Nothing) (Just a))
;;; generates the obvious fmap that maps over the a positions.
;;; Not yet implemented.
