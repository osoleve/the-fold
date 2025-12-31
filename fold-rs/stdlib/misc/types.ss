; ============================================================
; Type System for The Fold
; Types are Blocks. Types are S-expressions. Types are data.
;
; The type system is:
;   - Structural: types describe shape, not name
;   - Homoiconic: types are valid S-expressions, storable in CAS
;   - Bidirectional: inference flows both up and down
;   - Capability-aware: effects are visible in types
;   - Gradual: holes allow partial specification
;
; Type Grammar:
;   Type ::= BaseType
;          | (-> Type ... Type)           ; Function
;          | (x Type ...)                 ; Product (tuple)
;          | (+ (Tag Type ...) ...)       ; Sum (tagged union)
;          | (List Type)                  ; Homogeneous list
;          | (Vector Type)                ; Homogeneous vector
;          | (Block Tag Type)             ; Block with typed payload
;          | (Ref Type)                   ; Reference (hash) to typed block
;          | (forall (TVar ...) Type)     ; Universal quantification
;          | (mu TVar Type)               ; Recursive type
;          | (Cap Capability Type)        ; Capability-requiring type
;          | TVar                         ; Type variable
;          | ?                            ; Hole (unknown type)
;          | (? Name)                     ; Named hole
;
;   BaseType ::= Nat | Int | Bool | Symbol | String | Bytes | Unit | Void
;   Tag ::= symbol
;   TVar ::= symbol starting with lowercase
;   Capability ::= FS | Net | Time | ...
; ============================================================

; ============================================================
; Base Types
; ============================================================

; The primitive types of The Fold
(base-types '(Nat      ; Natural numbers (0, 1, 2, ...)
              Int      ; Integers (..., -1, 0, 1, ...)
              Bool     ; Boolean (#t, #f)
              Char     ; Unicode character
              Symbol   ; Interned symbols
              String   ; UTF-8 strings
              Bytes    ; Raw bytevectors
              Unit     ; Single value ()
              Void     ; No values (bottom for returns)
              Hash))   ; 33-byte versioned address (block reference)

; base-type? : Any -> Boolean
(base-type? (fn (t)
                (if (memq t base-types) #t #f)))

; ============================================================
; Type Predicates
; ============================================================

; type-var? : Any -> Boolean
; Type variables are lowercase symbols
(type-var? (fn (t)
               (if (symbol? t)
                   (char-lower-case? (string-ref (symbol->string t) 0))
                   #f)))

; variant? : Any -> Boolean
; Is this a valid sum type variant? (Tag Type ...)
(variant? (fn (v)
              (if (pair? v)
                  (if (symbol? (car v))
                      (all type? (cdr v))
                      #f)
                  #f)))

; type? : Any -> Boolean
; Is this a well-formed type expression?
(type? (fix type?
            (fn (t)
                (if (base-type? t) #t
                    (if (eq? t '?) #t  ; Hole
                        (if (type-var? t) #t  ; Type variable
                            (if (not (pair? t)) #f
                                ; Named hole: (? name)
                                (if (eq? (car t) '?)
                                    (if (= (length t) 2) (symbol? (cadr t)) #f)
                                    ; Function type: (-> T1 ... Tn Tresult)
                                    (if (eq? (car t) '->)
                                        (if (>= (length t) 3)
                                            (all type? (cdr t))
                                            #f)
                                        ; Product type: (x T1 ... Tn)
                                        (if (eq? (car t) 'x)
                                            (all type? (cdr t))
                                            ; Sum type: (+ (Tag T...) ...)
                                            (if (eq? (car t) '+)
                                                (all variant? (cdr t))
                                                ; List type: (List T)
                                                (if (eq? (car t) 'List)
                                                    (if (= (length t) 2)
                                                        (type? (cadr t))
                                                        #f)
                                                    ; Vector type: (Vector T)
                                                    (if (eq? (car t) 'Vector)
                                                        (if (= (length t) 2)
                                                            (type? (cadr t))
                                                            #f)
                                                        ; Block type: (Block Tag PayloadType)
                                                        (if (eq? (car t) 'Block)
                                                            (if (= (length t) 3)
                                                                (if (symbol? (cadr t))
                                                                    (type? (caddr t))
                                                                    #f)
                                                                #f)
                                                            ; Ref type: (Ref T)
                                                            (if (eq? (car t) 'Ref)
                                                                (if (= (length t) 2)
                                                                    (type? (cadr t))
                                                                    #f)
                                                                ; Type application: (type-app F Args...)
                                                                (if (eq? (car t) 'type-app)
                                                                    (if (>= (length t) 2)
                                                                        (all type? (cdr t))
                                                                        #f)
                                                                    ; Universal: (forall (vars...) T)
                                                                    (if (eq? (car t) 'forall)
                                                                        (if (= (length t) 3)
                                                                            (if (list? (cadr t))
                                                                                (type? (caddr t))
                                                                                #f)
                                                                            #f)
                                                                        ; Recursive: (mu var T)
                                                                        (if (eq? (car t) 'mu)
                                                                            (if (= (length t) 3)
                                                                                (if (symbol? (cadr t))
                                                                                    (type? (caddr t))
                                                                                    #f)
                                                                                #f)
                                                                            ; Capability: (Cap name T)
                                                                            (if (eq? (car t) 'Cap)
                                                                                (if (= (length t) 3)
                                                                                    (if (symbol? (cadr t))
                                                                                        (type? (caddr t))
                                                                                        #f)
                                                                                    #f)
                                                                                #f)))))))))))))))))))

; ============================================================
; Type Constructors
; ============================================================

; t-> : (List Type) -> Type
; Construct a function type from a list of types.
(t-> (fn (types) (cons '-> types)))

; t-product : (List Type) -> Type
; Construct a product type (tuple) from a list of types.
(t-product (fn (types) (cons 'x types)))

; t-sum : (List Variant) -> Type
; Construct a sum type from a list of variants.
(t-sum (fn (variants) (cons '+ variants)))

; t-list : Type -> Type
(t-list (fn (elem-type)
            (list 'List elem-type)))

; t-vector : Type -> Type
(t-vector (fn (elem-type)
              (list 'Vector elem-type)))

; t-block : Symbol x Type -> Type
(t-block (fn (tag payload-type)
             (list 'Block tag payload-type)))

; t-ref : Type -> Type
(t-ref (fn (target-type)
           (list 'Ref target-type)))

; t-forall : (List Symbol) x Type -> Type
(t-forall (fn (vars body)
              (list 'forall vars body)))

; t-rec : Symbol x Type -> Type
(t-rec (fn (var body)
           (list 'mu var body)))

; t-cap : Symbol x Type -> Type
(t-cap (fn (cap-name inner-type)
           (list 'Cap cap-name inner-type)))

; t-hole : -> Type
(t-hole (fn () '?))

; t-named-hole : Symbol -> Type
(t-named-hole (fn (name) (list '? name)))

; ============================================================
; Type Accessors
; ============================================================

; function-type? : Type -> Boolean
(function-type? (fn (t)
                    (if (pair? t) (eq? (car t) '->) #f)))

; function-param-types : Type -> (List Type)
; Get parameter types from function type (all but last)
(function-param-types (fn (t)
                          (if (function-type? t)
                              (take (cdr t) (- (length (cdr t)) 1))
                              '())))

; function-return-type : Type -> Type
; Get return type from function type (last element)
(function-return-type (fn (t)
                          (if (function-type? t)
                              (last (cdr t))
                              'Void)))

; function-arity : Type -> Nat
; Number of parameters
(function-arity (fn (t)
                    (if (function-type? t)
                        (- (length (cdr t)) 1)
                        0)))

; product-type? : Type -> Boolean
(product-type? (fn (t)
                   (if (pair? t) (eq? (car t) 'x) #f)))

; product-elements : Type -> (List Type)
(product-elements (fn (t)
                      (if (product-type? t) (cdr t) '())))

; sum-type? : Type -> Boolean
(sum-type? (fn (t)
               (if (pair? t) (eq? (car t) '+) #f)))

; sum-variants : Type -> (List (Tag x Type ...))
(sum-variants (fn (t)
                  (if (sum-type? t) (cdr t) '())))

; list-type? : Type -> Boolean
(list-type? (fn (t)
                (if (pair? t) (eq? (car t) 'List) #f)))

; list-element-type : Type -> Type
(list-element-type (fn (t)
                       (if (list-type? t) (cadr t) '?)))

; forall-type? : Type -> Boolean
(forall-type? (fn (t)
                  (if (pair? t) (eq? (car t) 'forall) #f)))

; forall-vars : Type -> (List Symbol)
(forall-vars (fn (t)
                 (if (forall-type? t) (cadr t) '())))

; forall-body : Type -> Type
(forall-body (fn (t)
                 (if (forall-type? t) (caddr t) '?)))

; recursive-type? : Type -> Boolean
(recursive-type? (fn (t)
                     (if (pair? t) (eq? (car t) 'mu) #f)))

; recursive-var : Type -> Symbol
(recursive-var (fn (t)
                   (if (recursive-type? t) (cadr t) 'x)))

; recursive-body : Type -> Type
(recursive-body (fn (t)
                    (if (recursive-type? t) (caddr t) '?)))

; hole? : Type -> Boolean
(hole? (fn (t)
           (if (eq? t '?) #t
               (if (pair? t)
                   (eq? (car t) '?)
                   #f))))

; named-hole? : Type -> Boolean
(named-hole? (fn (t)
                 (if (pair? t)
                     (if (eq? (car t) '?)
                         (= (length t) 2)
                         #f)
                     #f)))

; hole-name : Type -> Symbol | #f
(hole-name (fn (t)
               (if (named-hole? t) (cadr t) #f)))

; ============================================================
; Type Substitution
; ============================================================

; type-subst : Symbol x Type x Type -> Type
; Substitute all occurrences of var with replacement in target
(type-subst (fix type-subst
                 (fn (var replacement target)
                     (if (eq? target var)
                         replacement
                         (if (not (pair? target))
                             target
                             ; Don't substitute under binding that shadows var
                             (if (eq? (car target) 'forall)
                                 (if (memq var (cadr target))
                                     target  ; var is bound, don't substitute
                                     (list 'forall
                                           (cadr target)
                                           (type-subst var replacement (caddr target))))
                                 (if (eq? (car target) 'mu)
                                     (if (eq? var (cadr target))
                                         target  ; var is bound
                                         (list 'mu
                                               (cadr target)
                                               (type-subst var replacement (caddr target))))
                                     ; Recurse into compound types
                                     (map (fn (t) (type-subst var replacement t)) target))))))))

; type-subst-all : (List (Symbol x Type)) x Type -> Type
; Apply multiple substitutions
(type-subst-all (fn (substs target)
                    (foldl (fn (t pair)
                               (type-subst (car pair) (cdr pair) t))
                           target
                           substs)))

; ============================================================
; Free Type Variables
; ============================================================

; free-type-vars : Type -> (List Symbol)
; Collect free type variables in a type
(free-type-vars (fix free-type-vars
                     (fn (t)
                         (if (type-var? t)
                             (list t)
                             (if (not (pair? t))
                                 '()
                                 (if (eq? (car t) 'forall)
                                     (filter (fn (v) (not (memq v (cadr t))))
                                             (free-type-vars (caddr t)))
                                     (if (eq? (car t) 'mu)
                                         (filter (fn (v) (not (eq? v (cadr t))))
                                                 (free-type-vars (caddr t)))
                                         (flat-map free-type-vars (cdr t)))))))))

; closed-type? : Type -> Boolean
; A type is closed if it has no free type variables
(closed-type? (fn (t)
                  (null? (free-type-vars t))))

; ============================================================
; Type Equality (structural)
; ============================================================

; type-equal? : Type x Type -> Boolean
; Structural equality of types
(type-equal? (fix type-equal?
                  (fn (t1 t2)
                      (if (eq? t1 t2) #t
                          (if (not (pair? t1)) #f
                              (if (not (pair? t2)) #f
                                  (if (not (= (length t1) (length t2))) #f
                                      (all id (map type-equal? t1 t2)))))))))

; ============================================================
; Common Type Aliases
; ============================================================

; Numeric types
(type-nat 'Nat)
(type-int 'Int)
(type-bool 'Bool)
(type-char 'Char)
(type-symbol 'Symbol)
(type-string 'String)
(type-bytes 'Bytes)
(type-unit 'Unit)
(type-void 'Void)
(type-hash 'Hash)

; Common function types
(type-predicate (t-> (list '? 'Bool)))
(type-unary-op (t-> (list '? '?)))
(type-binary-op (t-> (list '? '? '?)))

; ============================================================
; Type Display
; ============================================================

; type->string : Type -> String
; Pretty-print a type
(type->string (fix type->string
                   (fn (t)
                       (if (symbol? t)
                           (symbol->string t)
                           (if (eq? t '?)
                               "?"
                               (if (not (pair? t))
                                   (string-append "#<invalid-type:" (symbol->string (type-of-value t)) ">")
                                   (string-append "("
                                                  (string-join (map type->string t) " ")
                                                  ")")))))))

; ============================================================
; Module Exports
; (see exports.ss for exported symbols)
; ============================================================
