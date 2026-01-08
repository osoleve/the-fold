;;; core/types/type-families.ss — Associated Type Families
;;;
;;; Type families are type-level functions associated with type classes.
;;; They enable types to vary with instances, generalizing functional dependencies.
;;;
;;; Key concepts:
;;;   1. Associated type declaration (in class definition)
;;;   2. Associated type instance (in instance definition)
;;;   3. Type family reduction (during type checking)
;;;
;;; Example:
;;;   (define-class (Collection c)
;;;     (type Elem c)              ; Associated type: element type depends on c
;;;     (empty : c)
;;;     (insert : (-> (Elem c) c c)))
;;;
;;;   (define-instance (Collection (List a))
;;;     (type Elem (List a) = a)   ; For List a, Elem is a
;;;     (empty '())
;;;     (insert cons))
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
;;; Type Family Declaration Grammar
;;; ============================================================
;;;
;;; Associated Type Declaration (in type class):
;;;   (type Name params...)
;;;   (type Name params... : Kind)  ; with explicit kind
;;;
;;; Associated Type Instance:
;;;   (type Name pattern = RHS)
;;;
;;; Examples:
;;;   (type Elem c)              ; Elem takes one param, kind inferred
;;;   (type Rep f : Type)        ; Rep takes one param, kind Type
;;;   (type Key m : Type)        ; Key is indexed by m, returns Type
;;;   (type Elem (List a) = a)   ; Instance: Elem of List a is a

;;; ============================================================
;;; Type Family Declaration
;;; ============================================================

;;; A type family declaration within a type class.
;;; Represented as: (type-family name params kind)
(define-record-type type-family
  (fields
   name      ; Symbol - the type family name (e.g., Elem, Rep, Key)
   params    ; (List Symbol) - type parameters (from the class)
   kind))    ; Kind - the result kind (defaults to *)

;;; type-family-decl? : SExpr → Boolean
;;; Check if this is a type family declaration: (type Name params...)
(define (type-family-decl? decl)
  (and (pair? decl)
       (eq? (car decl) 'type)
       (>= (length decl) 2)
       (symbol? (cadr decl))
       ;; Not an instance definition (no = sign)
       (not (memq '= decl))))

;;; parse-type-family-decl : SExpr → Any
;;; Parse a type family declaration from class definition.
;;; Forms:
;;;   (type Name param...)           ; kind inferred as *
;;;   (type Name param... : Kind)    ; explicit kind
(define (parse-type-family-decl decl)
  (if (not (type-family-decl? decl))
      (error 'parse-type-family-decl "not a type family declaration" decl)
      (let* ([name (cadr decl)]
             [rest (cddr decl)])
            ;; Check for explicit kind annotation
            (let-values ([(params kind) (split-at-colon rest)])
                        (make-type-family name params (or kind K*))))))

;;; split-at-colon : (List SExpr) → (Values (List Symbol) (Option Kind))
;;; Split a list at the colon, returning params before and kind after.
(define (split-at-colon lst)
  (let loop ([items lst] [params '()])
       (cond
        [(null? items) (values (reverse params) #f)]
        [(eq? (car items) ':)
         (if (null? (cdr items))
             (error 'split-at-colon "missing kind after colon" lst)
             (values (reverse params) (cadr items)))]
        [(symbol? (car items))
         (loop (cdr items) (cons (car items) params))]
        [else
         (error 'split-at-colon "expected symbol or colon" (car items))])))

;;; ============================================================
;;; Type Family Instance
;;; ============================================================

;;; A type family instance definition.
;;; Represented as: (type-family-instance family-name pattern rhs)
(define-record-type type-family-instance
  (fields
   family-name  ; Symbol - which type family
   pattern      ; Type - the LHS pattern (e.g., (List a))
   rhs))        ; Type - the result type (e.g., a)

;;; type-family-instance? : SExpr → Boolean
;;; Check if this is a type family instance: (type Name pattern = RHS)
(define (type-family-instance-decl? decl)
  (and (pair? decl)
       (eq? (car decl) 'type)
       (>= (length decl) 4)
       (symbol? (cadr decl))
       (if (memq '= decl) #t #f)))

;;; parse-type-family-instance : SExpr → Any
;;; Parse a type family instance definition.
;;; Form: (type Name pattern = RHS)
(define (parse-type-family-instance decl)
  (if (not (type-family-instance-decl? decl))
      (error 'parse-type-family-instance "not a type family instance" decl)
      (let* ([name (cadr decl)]
             [rest (cddr decl)])
            ;; Find the = sign
            (let loop ([items rest] [pattern-parts '()])
                 (cond
                  [(null? items)
                   (error 'parse-type-family-instance "missing = in type family instance" decl)]
                  [(eq? (car items) '=)
                   (if (null? (cdr items))
                       (error 'parse-type-family-instance "missing RHS after =" decl)
                       (let ([pattern (if (= (length pattern-parts) 1)
                                          (car pattern-parts)
                                          (reverse pattern-parts))]
                             [rhs (cadr items)])
                            (make-type-family-instance name pattern rhs)))]
                  [else
                   (loop (cdr items) (cons (car items) pattern-parts))])))))

;;; ============================================================
;;; Type Class with Associated Types
;;; ============================================================

;;; Extended type class record that includes associated types.
(define-record-type typeclass-with-families
  (fields
   name           ; Symbol
   kind           ; Kind
   supers         ; (List Symbol)
   methods        ; (List (name . type))
   type-families  ; (List Any)
   ))

;;; make-class-with-families : Symbol × Kind × (List Symbol) × (List Any) × (List Any) → Any
;;; Convenience constructor.
(define (make-class-with-families name kind supers methods families)
  (make-typeclass-with-families name kind supers methods families))

;;; upgrade-typeclass : TypeClass × (List Any) → Any
;;; Add type families to an existing type class.
(define (upgrade-typeclass tc families)
  (make-typeclass-with-families
   (typeclass-name tc)
   (typeclass-kind tc)
   (typeclass-supers tc)
   (typeclass-methods tc)
   families))

;;; ============================================================
;;; Type Family Any
;;; ============================================================

;;; The type family registry maps:
;;;   (family-name, class-name) → Any declaration
;;;   (family-name, pattern-head) → Any
;;;
;;; Represented as two alists for efficient lookup.

(define-record-type type-family-registry
  (fields
   declarations  ; ((family . class) . Any) alist
   instances))   ; ((family . type-head) . Any) alist

;;; empty-tf-registry : → TypeFamilyAny
(define (empty-tf-registry)
  (make-type-family-registry '() '()))

;;; tf-registry-add-decl : Any × Symbol × Symbol × Any → Any
;;; Add a type family declaration to the registry.
(define (tf-registry-add-decl registry family-name class-name tf)
  (make-type-family-registry
   (cons (cons (cons family-name class-name) tf)
         (type-family-registry-declarations registry))
   (type-family-registry-instances registry)))

;;; tf-registry-add-instance : Any × Symbol × Type × Any → Any
;;; Add a type family instance to the registry.
(define (tf-registry-add-instance registry family-name pattern tfi)
  (let ([pattern-head (type-head pattern)])
       (make-type-family-registry
        (type-family-registry-declarations registry)
        (cons (cons (cons family-name pattern-head) tfi)
              (type-family-registry-instances registry)))))

;;; tf-registry-lookup-decl : Any × Symbol → Any | #f
;;; Look up a type family declaration.
(define (tf-registry-lookup-decl registry family-name)
  (let ([entry (assoc-with (lambda (k) (eq? (car k) family-name))
                           (type-family-registry-declarations registry))])
       (if entry (cdr entry) #f)))

;;; tf-registry-lookup-instance : Any × Symbol × Type → Any | #f
;;; Look up a type family instance by family name and applied type.
(define (tf-registry-lookup-instance registry family-name applied-type)
  (let ([type-hd (type-head applied-type)])
       (let ([entry (assoc-with (lambda (k)
                                        (and (eq? (car k) family-name)
                                             (eq? (cdr k) type-hd)))
                                (type-family-registry-instances registry))])
            (if entry (cdr entry) #f))))

;;; assoc-with : (α → Boolean) × (List (α . β)) → (α . β) | #f
(define (assoc-with pred alist)
  (cond
   [(null? alist) #f]
   [(pred (caar alist)) (car alist)]
   [else (assoc-with pred (cdr alist))]))

;;; type-head : Type → Symbol
;;; Extract the head constructor of a type.
(define (type-head type)
  (cond
   [(symbol? type) type]
   [(pair? type) (if (symbol? (car type)) (car type) (type-head (car type)))]
   [else #f]))

;;; ============================================================
;;; Type Family Application
;;; ============================================================

;;; A type family application in a type expression.
;;; Syntax: (TF param) where TF is a type family name
;;; Example: (Elem (List Int)) → Int

;;; type-family-app? : Type × Any → Boolean
;;; Check if a type is a type family application.
(define (type-family-app? type registry)
  (and (pair? type)
       (symbol? (car type))
       (tf-registry-lookup-decl registry (car type))))

;;; ============================================================
;;; Type Family Reduction
;;; ============================================================

;;; Reduce type family applications to their instance definitions.
;;; This is the core of type family evaluation.

;;; reduce-type-families : Type × Any → Type
;;; Reduce all type family applications in a type.
(define (reduce-type-families type registry)
  (reduce-tf-fuel type registry 100))

;;; reduce-tf-fuel : Type × Any × Nat → Type
;;; Reduce with fuel to ensure termination.
;;; Uses "reduce arguments first" strategy for stuck type families.
(define (reduce-tf-fuel type registry fuel)
  (if (<= fuel 0)
      type  ; Out of fuel, return as-is
      (cond
       ;; Base cases
       [(symbol? type) type]
       [(not (pair? type)) type]
       
       ;; Type family application
       [(type-family-app? type registry)
        (let* ([family-name (car type)]
               [args (cdr type)]
               [applied-arg (if (= (length args) 1) (car args) args)])
              ;; First try to look up instance with current args
              (let ([instance (tf-registry-lookup-instance registry family-name applied-arg)])
                   (if instance
                       ;; Found instance, substitute and reduce again
                       (let* ([pattern (type-family-instance-pattern instance)]
                              [rhs (type-family-instance-rhs instance)]
                              [subst (match-type-pattern pattern applied-arg)])
                             (if subst
                                 (reduce-tf-fuel
                                  (apply-type-subst subst rhs)
                                  registry
                                  (- fuel 1))
                                 ;; Pattern didn't match, try reducing args
                                 (reduce-tf-args-and-retry type registry fuel)))
                       ;; No instance found - reduce arguments and retry
                       (reduce-tf-args-and-retry type registry fuel))))]
       
       ;; Recursively reduce in compound types
       [else
        (cons (car type)
              (map (lambda (t) (reduce-tf-fuel t registry (- fuel 1)))
                   (cdr type)))])))

;;; reduce-tf-args-and-retry : Type × Any × Nat → Type
;;; Reduce arguments of a stuck type family application and retry.
(define (reduce-tf-args-and-retry type registry fuel)
  (let* ([family-name (car type)]
         [args (cdr type)]
         [reduced-args (map (lambda (t) (reduce-tf-fuel t registry (- fuel 1))) args)]
         [reduced-type (cons family-name reduced-args)])
        ;; If arguments changed, try again with reduced type
        (if (equal? args reduced-args)
            reduced-type  ; No change, return as-is
            (let* ([applied-arg (if (= (length reduced-args) 1)
                                    (car reduced-args)
                                    reduced-args)]
                   [instance (tf-registry-lookup-instance registry family-name applied-arg)])
                  (if instance
                      ;; Found instance after reduction, apply it
                      (let* ([pattern (type-family-instance-pattern instance)]
                             [rhs (type-family-instance-rhs instance)]
                             [subst (match-type-pattern pattern applied-arg)])
                            (if subst
                                (reduce-tf-fuel
                                 (apply-type-subst subst rhs)
                                 registry
                                 (- fuel 2))
                                reduced-type))
                      ;; Still no instance, return with reduced args
                      reduced-type)))))

;;; match-type-pattern : Type × Type → Subst | #f
;;; Match a type against a pattern, returning substitution.
;;; Pattern variables are lowercase symbols.
(define (match-type-pattern pattern type)
  (match-tp-acc pattern type '()))

(define (match-tp-acc pattern type subst)
  (cond
   ;; Pattern variable: bind it
   [(and (symbol? pattern) (type-variable-pattern? pattern))
    (let ([existing (assq pattern subst)])
         (if existing
             ;; Already bound, check equality
             (if (type=? type (cdr existing))
                 subst
                 #f)
             ;; Not bound, add binding
             (cons (cons pattern type) subst)))]
   
   ;; Literal type: must match exactly
   [(symbol? pattern)
    (if (eq? pattern type) subst #f)]
   
   ;; Both compound: match structurally
   [(and (pair? pattern) (pair? type))
    (if (= (length pattern) (length type))
        (let loop ([ps pattern] [ts type] [s subst])
             (if (null? ps)
                 s
                 (let ([s* (match-tp-acc (car ps) (car ts) s)])
                      (if s*
                          (loop (cdr ps) (cdr ts) s*)
                          #f))))
        #f)]
   
   [else #f]))

;;; type-variable-pattern? : Symbol → Boolean
;;; Pattern variables are lowercase single letters or names starting with lowercase.
(define (type-variable-pattern? sym)
  (let ([s (symbol->string sym)])
       (and (> (string-length s) 0)
            (char-lower-case? (string-ref s 0)))))

;;; apply-type-subst : Subst × Type → Type
;;; Apply a type substitution.
(define (apply-type-subst subst type)
  (cond
   [(symbol? type)
    (let ([binding (assq type subst)])
         (if binding (cdr binding) type))]
   [(not (pair? type)) type]
   [else (map (lambda (t) (apply-type-subst subst t)) type)]))

;;; ============================================================
;;; Standard Type Families
;;; ============================================================

;;; Elem : Collection element type
(define TF-Elem
  (make-type-family 'Elem '(c) K*))

;;; Rep : Representable index type
(define TF-Rep
  (make-type-family 'Rep '(f) K*))

;;; Key : Map key type
(define TF-Key
  (make-type-family 'Key '(m) K*))

;;; Val : Map value type
(define TF-Val
  (make-type-family 'Val '(m) K*))

;;; Inner : Monad transformer inner monad
(define TF-Inner
  (make-type-family 'Inner '(t) (K=> K* K*)))

;;; ============================================================
;;; Standard Type Family Instances
;;; ============================================================

;;; Elem (List a) = a
(define TFI-Elem-List
  (make-type-family-instance 'Elem '(List a) 'a))

;;; Elem (Vector a) = a
(define TFI-Elem-Vector
  (make-type-family-instance 'Elem '(Vector a) 'a))

;;; Rep Identity = ()
(define TFI-Rep-Identity
  (make-type-family-instance 'Rep 'Identity 'Unit))

;;; Rep ((->) r) = r
(define TFI-Rep-Reader
  (make-type-family-instance 'Rep '(-> r) 'r))

;;; ============================================================
;;; Standard Any
;;; ============================================================

;;; Build the standard type family registry.
(define standard-tf-registry
  (let* ([r (empty-tf-registry)]
         ;; Add declarations
         [r (tf-registry-add-decl r 'Elem 'Collection TF-Elem)]
         [r (tf-registry-add-decl r 'Rep 'Representable TF-Rep)]
         [r (tf-registry-add-decl r 'Key 'Map TF-Key)]
         [r (tf-registry-add-decl r 'Val 'Map TF-Val)]
         [r (tf-registry-add-decl r 'Inner 'MonadTrans TF-Inner)]
         ;; Add instances
         [r (tf-registry-add-instance r 'Elem '(List a) TFI-Elem-List)]
         [r (tf-registry-add-instance r 'Elem '(Vector a) TFI-Elem-Vector)]
         [r (tf-registry-add-instance r 'Rep 'Identity TFI-Rep-Identity)]
         [r (tf-registry-add-instance r 'Rep '(-> r) TFI-Rep-Reader)])
        r))

;;; ============================================================
;;; Convenience API
;;; ============================================================

;;; reduce-std : Type → Type
;;; Reduce type families using the standard registry.
(define (reduce-std type)
  (reduce-type-families type standard-tf-registry))

;;; tf-apply : Symbol × Type → Type
;;; Construct a type family application.
(define (tf-apply family-name arg)
  (list family-name arg))

;;; ============================================================
;;; Example Type Classes with Associated Types
;;; ============================================================

;;; Collection class with Elem associated type
(define TC-Collection-with-Elem
  (make-class-with-families
   'Collection
   (K=> K* K-constraint)
   '()
   `((empty . (∀ (c) (=> (Collection c) c)))
     (insert . (∀ (c) (=> (Collection c) (-> (Elem c) c c))))
     (null? . (∀ (c) (=> (Collection c) (-> c Bool)))))
   (list TF-Elem)))

;;; Representable class with Rep associated type
(define TC-Representable-with-Rep
  (make-class-with-families
   'Representable
   (K=> (K=> K* K*) K-constraint)
   '(Functor)
   `((tabulate . (∀ (f a) (=> (Representable f) (-> (-> (Rep f) a) (@ f a)))))
     (index . (∀ (f a) (=> (Representable f) (-> (@ f a) (Rep f) a)))))
   (list TF-Rep)))

;;; Map class with Key and Val associated types
(define TC-Map-with-Types
  (make-class-with-families
   'Map
   (K=> K* K-constraint)
   '()
   `((lookup . (∀ (m) (=> (Map m) (-> (Key m) m (Option (Val m))))))
     (insert-kv . (∀ (m) (=> (Map m) (-> (Key m) (Val m) m m))))
     (delete . (∀ (m) (=> (Map m) (-> (Key m) m m)))))
   (list TF-Key TF-Val)))

;;; ============================================================
;;; Type Family Pretty Printing
;;; ============================================================

;;; type-family->string : Any → String
(define (type-family->string tf)
  (let ([name (type-family-name tf)]
        [params (type-family-params tf)]
        [kind (type-family-kind tf)])
       (string-append "type "
                      (symbol->string name)
                      " "
                      (join-strings " " (map symbol->string params))
                      (if (equal? kind K*)
                          ""
                          (string-append " : " (kind->string kind))))))

;;; type-family-instance->string : Any → String
(define (type-family-instance->string tfi)
  (let ([name (type-family-instance-family-name tfi)]
        [pattern (type-family-instance-pattern tfi)]
        [rhs (type-family-instance-rhs tfi)])
       (string-append "type "
                      (symbol->string name)
                      " "
                      (format "~s" pattern)
                      " = "
                      (format "~s" rhs))))
