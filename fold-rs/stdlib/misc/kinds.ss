; ============================================================
; Higher-Kinded Types for The Fold
; Types have types. We call them Kinds.
;
;   *           : The kind of ordinary types (Nat, Bool, List Nat)
;   * => *      : The kind of type constructors (List, Option, Vector)
;   * => * => * : Binary type constructors (Either, Pair, ->)
;   (* => *) => * : Higher-order kinds (Fix, Free)
;   Constraint  : The kind of type class constraints (Functor, Monad)
;
; With kinds, we can express:
;   - Functor : (* => *) => Constraint
;   - map : forall (f : * => *). Functor f => forall a b. (a -> b) -> f a -> f b
;
; Depends on: types.ss
; ============================================================

; Kind Grammar:
;   Kind ::= *                    ; Type
;          | (=> Kind Kind)       ; Kind arrow (type constructor)
;          | Constraint           ; Type class constraint
;          | Row                  ; Row kind (for extensible records)
;          | KVar                 ; Kind variable
;          | (k-forall (KVar ...) Kind) ; Kind polymorphism

; ============================================================
; Kind Representation
; ============================================================

; The base kind: ordinary types
(K* '*)

; The constraint kind: type class evidence
(K-constraint 'Constraint)

; The row kind: for extensible records/variants
(K-row 'Row)

; K=> : Kind x Kind -> Kind
; Construct a kind arrow
(K=> (fn (k1 k2)
         (list '=> k1 k2)))

; K=>* : (List Kind) -> Kind
; Multi-argument kind arrow (right-associative)
(K=>* (fix K=>*
           (fn (kinds)
               (if (null? (cdr kinds))
                   (car kinds)
                   (K=> (car kinds) (K=>* (cdr kinds)))))))

; K-forall : (List Symbol) x Kind -> Kind
(K-forall (fn (vars body)
              (list 'k-forall vars body)))

; ============================================================
; Kind Predicates
; ============================================================

; kind-var? : Any -> Boolean
; Kind variables start with k-
(kind-var? (fn (k)
               (if (symbol? k)
                   (let ((s (symbol->string k)))
                        (if (> (string-length s) 2)
                            (if (char=? (string-ref s 0) #\k)
                                (char=? (string-ref s 1) #\-)
                                #f)
                            #f))
                   #f)))

; kind? : Any -> Boolean
(kind? (fix kind?
            (fn (k)
                (if (eq? k '*) #t
                    (if (eq? k 'Constraint) #t
                        (if (eq? k 'Row) #t
                            (if (kind-var? k) #t
                                (if (not (pair? k)) #f
                                    ; Kind arrow: (=> K1 K2)
                                    (if (eq? (car k) '=>)
                                        (if (= (length k) 3)
                                            (if (kind? (cadr k))
                                                (kind? (caddr k))
                                                #f)
                                            #f)
                                        ; Kind polymorphism: (k-forall (vars) body)
                                        (if (eq? (car k) 'k-forall)
                                            (if (= (length k) 3)
                                                (if (list? (cadr k))
                                                    (if (all symbol? (cadr k))
                                                        (kind? (caddr k))
                                                        #f)
                                                    #f)
                                                #f)
                                            #f))))))))))

; kind-arrow? : Kind -> Boolean
(kind-arrow? (fn (k)
                 (if (pair? k) (eq? (car k) '=>) #f)))

; kind-param : Kind -> Kind
(kind-param (fn (k)
                (if (kind-arrow? k) (cadr k) #f)))

; kind-result : Kind -> Kind
(kind-result (fn (k)
                 (if (kind-arrow? k) (caddr k) #f)))

; ============================================================
; Kind Equality
; ============================================================

; kind=? : Kind x Kind -> Boolean
(kind=? (fix kind=?
             (fn (k1 k2)
                 (if (symbol? k1)
                     (if (symbol? k2)
                         (eq? k1 k2)
                         #f)
                     (if (pair? k1)
                         (if (pair? k2)
                             (if (= (length k1) (length k2))
                                 (all id (zip-with kind=? k1 k2))
                                 #f)
                             #f)
                         #f)))))

; ============================================================
; Type Application
; ============================================================

; type-app? : Type -> Boolean
(type-app? (fn (t)
               (if (pair? t) (eq? (car t) 'type-app) #f)))

; type-app-head : Type -> Type
(type-app-head (fn (t)
                   (if (type-app? t) (cadr t) t)))

; type-app-args : Type -> (List Type)
(type-app-args (fn (t)
                   (if (type-app? t) (cddr t) '())))

; T@ : Type x (List Type) -> Type
; Construct a type-level application
(T@ (fn (f args)
        (cons 'type-app (cons f args))))

; ============================================================
; Built-in Type Constructor Kinds
; ============================================================

; The kind environment for built-in type constructors
(builtin-kinds
 (list (cons 'List (K=> K* K*))
       (cons 'Vector (K=> K* K*))
       (cons 'Option (K=> K* K*))
       (cons 'Ref (K=> K* K*))
       (cons 'Either (K=> K* (K=> K* K*)))
       (cons 'Pair (K=> K* (K=> K* K*)))
       (cons 'Result (K=> K* (K=> K* K*)))
       ; Base types have kind *
       (cons 'Nat K*)
       (cons 'Int K*)
       (cons 'Bool K*)
       (cons 'Char K*)
       (cons 'Symbol K*)
       (cons 'String K*)
       (cons 'Bytes K*)
       (cons 'Unit K*)
       (cons 'Void K*)
       (cons 'Hash K*)))

; lookup-kind : Symbol -> Kind | #f
(lookup-kind (fn (name)
                 (let ((entry (assoc name builtin-kinds)))
                      (if entry (cdr entry) #f))))

; ============================================================
; Kind Inference
; ============================================================

; infer-kind : Type x KindEnv -> Kind | (error ...)
; Infer the kind of a type expression.
(infer-kind (fix infer-kind
                 (fn (type kenv)
                     ; Symbol — look up in environment
                     (if (symbol? type)
                         (let ((builtin (lookup-kind type)))
                              (if builtin
                                  builtin
                                  (let ((env-entry (assoc type kenv)))
                                       (if env-entry
                                           (cdr env-entry)
                                           (list 'error 'unknown-type type)))))
                         ; Hole
                         (if (eq? type '?)
                             'k-?
                             (if (not (pair? type))
                                 (list 'error 'invalid-type type)
                                 ; Type application: (type-app F Args...)
                                 (if (eq? (car type) 'type-app)
                                     (infer-app-kind (cadr type) (cddr type) kenv)
                                     ; Universal quantification: (forall (vars) body)
                                     (if (eq? (car type) 'forall)
                                         (let ((vars (cadr type))
                                               (body (caddr type)))
                                              (let ((var-kinds (map (fn (v) (cons v K*)) vars)))
                                                   (let ((new-env (append var-kinds kenv)))
                                                        (let ((body-kind (infer-kind body new-env)))
                                                             (if (kind=? body-kind K*)
                                                                 K*
                                                                 (list 'error 'quantified-body-not-type body-kind))))))
                                         ; Recursive type: (mu var body)
                                         (if (eq? (car type) 'mu)
                                             (let ((var (cadr type))
                                                   (body (caddr type)))
                                                  (let ((new-env (cons (cons var K*) kenv)))
                                                       (let ((body-kind (infer-kind body new-env)))
                                                            (if (kind=? body-kind K*)
                                                                K*
                                                                (list 'error 'recursive-body-not-type body-kind)))))
                                             ; Function type: (-> T1 ... Tn Tresult)
                                             (if (eq? (car type) '->)
                                                 (let ((arg-kinds (map (fn (t) (infer-kind t kenv)) (cdr type))))
                                                      (if (all (fn (k) (kind=? k K*)) arg-kinds)
                                                          K*
                                                          (list 'error 'function-args-not-types arg-kinds)))
                                                 ; Product type: (x T1 ... Tn)
                                                 (if (eq? (car type) 'x)
                                                     (let ((elem-kinds (map (fn (t) (infer-kind t kenv)) (cdr type))))
                                                          (if (all (fn (k) (kind=? k K*)) elem-kinds)
                                                              K*
                                                              (list 'error 'product-elems-not-types elem-kinds)))
                                                     ; Sum type: (+ (Tag T...) ...)
                                                     (if (eq? (car type) '+)
                                                         (let ((check-variant (fn (variant)
                                                                                  (let ((vkinds (map (fn (t) (infer-kind t kenv)) (cdr variant))))
                                                                                       (all (fn (k) (kind=? k K*)) vkinds)))))
                                                              (if (all check-variant (cdr type))
                                                                  K*
                                                                  (list 'error 'sum-variants-not-types)))
                                                         ; List type: (List T)
                                                         (if (eq? (car type) 'List)
                                                             (infer-app-kind 'List (cdr type) kenv)
                                                             ; Vector type: (Vector T)
                                                             (if (eq? (car type) 'Vector)
                                                                 (infer-app-kind 'Vector (cdr type) kenv)
                                                                 ; Ref type: (Ref T)
                                                                 (if (eq? (car type) 'Ref)
                                                                     (infer-app-kind 'Ref (cdr type) kenv)
                                                                     (list 'error 'unknown-type-form type))))))))))))))))

; infer-app-kind : Type x (List Type) x KindEnv -> Kind | (error ...)
; Infer the kind of a type application.
(infer-app-kind (fix infer-app-kind
                     (fn (head args kenv)
                         (let ((head-kind (infer-kind head kenv)))
                              (if (null? args)
                                  head-kind
                                  (if (kind-arrow? head-kind)
                                      (let ((param-kind (kind-param head-kind))
                                            (result-kind (kind-result head-kind))
                                            (arg-kind (infer-kind (car args) kenv)))
                                           (if (kind=? param-kind arg-kind)
                                               (infer-app-kind result-kind (cdr args) kenv)
                                               (list 'error 'kind-mismatch param-kind arg-kind)))
                                      (list 'error 'not-a-type-constructor head-kind)))))))

; ============================================================
; Well-Kinded Check
; ============================================================

; well-kinded? : Type -> Boolean
; Check if a type is well-kinded (kind = *)
(well-kinded? (fn (type)
                  (let ((k (infer-kind type '())))
                       (if (pair? k)
                           (not (eq? (car k) 'error))
                           #t))))

; has-kind? : Type x Kind -> Boolean
; Check if a type has a specific kind
(has-kind? (fn (type expected-kind)
               (kind=? (infer-kind type '()) expected-kind)))

; ============================================================
; Common Kinds
; ============================================================

; Type -> Type
(kind-unary (K=> K* K*))

; Type -> Type -> Type
(kind-binary (K=> K* (K=> K* K*)))

; (Type -> Type) -> Type
(kind-higher (K=> (K=> K* K*) K*))

; ============================================================
; Module Exports
; (see exports.ss for exported symbols)
; ============================================================
