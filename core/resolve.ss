;;; core/resolve.ss — Type Class Instance Resolution
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

(load "prelude.ss")
(load "types.ss")
(load "kinds.ss")

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
;;; Resolution Algorithm
;;; ============================================================

;;; resolve : Constraint × IDB → (Result Evidence)
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
       `(error no-instance-found ,constraint)]
      [(> (length matches) 1)
       ;; Overlapping instances — need to pick most specific
       ;; For now, just take first
       (resolve-match (car matches) db)]
      [else
       (resolve-match (car matches) db)])))

;;; resolve-match : Match × IDB → (Result Evidence)
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

;;; resolve-context : (List Constraint) × Subst × IDB → (Result (List Evidence))
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
    (Monad . ,TC-Monad)))

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

;;; Standard instance database
(define standard-instances
  (list inst-Functor-List
        inst-Applicative-List
        inst-Monad-List
        inst-Functor-Option
        inst-Applicative-Option
        inst-Monad-Option
        inst-Functor-Either
        inst-Monad-Either))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; resolve-std : Constraint → (Result Evidence)
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
