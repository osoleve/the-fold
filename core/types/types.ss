;;; fabric/stitches/types.ss — The Type System of The Fold
;;;
;;; Types are Blocks. Types are S-expressions. Types are data.
;;;
;;; The type system is:
;;;   - Structural: types describe shape, not name
;;;   - Homoiconic: types are valid S-expressions, storable in CAS
;;;   - Bidirectional: inference flows both up and down
;;;   - Capability-aware: effects are visible in types
;;;   - Gradual: holes allow partial specification
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;
;;; See fabric/stitches/MODULES.md for full dependency graph.

(load "core/base/prelude.ss")
;;;
;;; Type Grammar:
;;;
;;;   Type ::= BaseType
;;;          | (-> Type ... Type)           ; Function
;;;          | (× Type ...)                 ; Product (tuple)
;;;          | (+ (Tag Type ...) ...)       ; Sum (tagged union)
;;;          | (List Type)                  ; Homogeneous list
;;;          | (Vector Type)                ; Homogeneous vector
;;;          | (Block Tag Type)             ; Block with typed payload
;;;          | (Ref Type)                   ; Reference (hash) to typed block
;;;          | (∀ (TVar ...) Type)          ; Universal quantification
;;;          | (μ TVar Type)                ; Recursive type
;;;          | (Cap Capability Type)        ; Capability-requiring type
;;;          | TVar                         ; Type variable
;;;          | ?                            ; Hole (unknown type)
;;;          | (? Name)                     ; Named hole
;;;
;;;   BaseType ::= Nat | Int | Bool | Symbol | String | Bytes | Unit | Void
;;;
;;;   Tag ::= symbol
;;;   TVar ::= symbol starting with lowercase
;;;   Capability ::= FS | Net | Time | ...

;;; ============================================================
;;; Base Types
;;; ============================================================

;;; The primitive types of The Fold
(define base-types
  '(Nat      ; Natural numbers (0, 1, 2, ...)
    Int      ; Integers (..., -1, 0, 1, ...)
    Bool     ; Boolean (#t, #f)
    Char     ; Unicode character
    Symbol   ; Interned symbols
    String   ; UTF-8 strings
    Bytes    ; Raw bytevectors
    Unit     ; Single value ()
    Void     ; No values (bottom for returns)
    Hash))   ; 33-byte versioned address (block reference)

;;; base-type? : Any → Boolean
(define (base-type? t)
  (if (memq t base-types) #t #f))

;;; ============================================================
;;; Type Predicates
;;; ============================================================

;;; type? : Any → Boolean
;;; Is this a well-formed type expression?
(define (type? t)
  (cond
   ;; Base types
   [(base-type? t) #t]
   ;; Hole
   [(eq? t '?) #t]
   ;; Named hole
   [(and (pair? t) (eq? (car t) '?) (symbol? (cadr t))) #t]
   ;; Type variable (lowercase symbol)
   [(and (symbol? t) (char-lower-case? (string-ref (symbol->string t) 0))) #t]
   ;; Compound types
   [(not (pair? t)) #f]
   ;; Function type: (-> T1 ... Tn Tresult)
   [(eq? (car t) '->) (and (>= (length t) 3) (andmap type? (cdr t)))]
   ;; Product type: (× T1 ... Tn)
   [(eq? (car t) '×) (andmap type? (cdr t))]
   ;; Sum type: (+ (Tag T...) ...)
   [(eq? (car t) '+) (andmap variant? (cdr t))]
   ;; List type: (List T)
   [(eq? (car t) 'List) (and (= (length t) 2) (type? (cadr t)))]
   ;; Vector type: (Vector T)
   [(eq? (car t) 'Vector) (and (= (length t) 2) (type? (cadr t)))]
   ;; Block type: (Block Tag PayloadType)
   [(eq? (car t) 'Block) (and (= (length t) 3) (symbol? (cadr t)) (type? (caddr t)))]
   ;; Ref type: (Ref T)
   [(eq? (car t) 'Ref) (and (= (length t) 2) (type? (cadr t)))]
   ;; Type application: (@ F Args...)
   ;; Used for HKTs: (@ List Int), (@ f a)
   [(eq? (car t) '@) (and (>= (length t) 2)
                          (andmap type? (cdr t)))]
   ;; Universal: (∀ (vars...) T)
   ;; Vars can be simple symbols (kind *) or kinded: (name : kind)
   [(eq? (car t) '∀) (and (= (length t) 3)
                          (list? (cadr t))
                          (andmap (lambda (v)
                                          (or (symbol? v)
                                              (and (pair? v)
                                                   (= (length v) 3)
                                                   (symbol? (car v))
                                                   (eq? (cadr v) ':))))
                                  (cadr t))
                          (type? (caddr t)))]
   ;; Recursive: (μ var T)
   [(eq? (car t) 'μ) (and (= (length t) 3) (symbol? (cadr t)) (type? (caddr t)))]
   ;; Capability: (Cap name T)
   [(eq? (car t) 'Cap) (and (= (length t) 3) (symbol? (cadr t)) (type? (caddr t)))]
   [else #f]))

;;; variant? : Any → Boolean
;;; Is this a valid sum type variant?
(define (variant? v)
  (and (pair? v)
       (symbol? (car v))
       (andmap type? (cdr v))))

;;; Note: andmap is provided by prelude.ss

;;; ============================================================
;;; Type Constructors
;;; ============================================================

;;; t-> : Type ... → Type
;;; Construct a function type.
(define (t-> . types)
  (cons '-> types))

;;; t× : Type ... → Type
;;; Construct a product type.
(define (t× . types)
  (cons '× types))

;;; t+ : (Tag × Type ...) ... → Type
;;; Construct a sum type.
(define (t+ . variants)
  (cons '+ variants))

;;; t-list : Type → Type
(define (t-list elem-type)
  `(List ,elem-type))

;;; t-vector : Type → Type
(define (t-vector elem-type)
  `(Vector ,elem-type))

;;; t-block : Symbol × Type → Type
(define (t-block tag payload-type)
  `(Block ,tag ,payload-type))

;;; t-ref : Type → Type
(define (t-ref target-type)
  `(Ref ,target-type))

;;; t-forall : (List Symbol) × Type → Type
;;; Simple form: vars are symbols, all assumed kind *
(define (t-forall vars body)
  `(∀ ,vars ,body))

;;; t-forall-kinded : (List (Symbol × Kind)) × Type → Type
;;; Kinded form: vars are (name : kind) pairs for HKT support
;;; Example: (t-forall-kinded '((f . (* → *)) (a . *)) '(@ f a))
;;;          → (∀ ((f : (⇒ * *)) (a : *)) (@ f a))
(define (t-forall-kinded kinded-vars body)
  `(∀ ,(map (lambda (pair) `(,(car pair) : ,(cdr pair))) kinded-vars) ,body))

;;; t-rec : Symbol × Type → Type
(define (t-rec var body)
  `(μ ,var ,body))

;;; t-cap : Symbol × Type → Type
(define (t-cap cap-name inner-type)
  `(Cap ,cap-name ,inner-type))

;;; t-hole : → Type
(define (t-hole)
  '?)

;;; t-named-hole : Symbol → Type
(define (t-named-hole name)
  `(? ,name))

;;; ============================================================
;;; Type Accessors
;;; ============================================================

;;; function-type? : Type → Boolean
(define (function-type? t)
  (and (pair? t) (eq? (car t) '->)))

;;; function-param-types : Type → (List Type)
(define (function-param-types t)
  (if (function-type? t)
      (reverse (cdr (reverse (cdr t))))  ; All but first and last
      '()))

;;; function-return-type : Type → Type
(define (function-return-type t)
  (if (function-type? t)
      (car (reverse (cdr t)))  ; Last element
      'Void))

;;; product-type? : Type → Boolean
(define (product-type? t)
  (and (pair? t) (eq? (car t) '×)))

;;; product-types : Type → (List Type)
(define (product-types t)
  (if (product-type? t) (cdr t) '()))

;;; sum-type? : Type → Boolean
(define (sum-type? t)
  (and (pair? t) (eq? (car t) '+)))

;;; sum-variants : Type → (List (Tag × Type ...))
(define (sum-variants t)
  (if (sum-type? t) (cdr t) '()))

;;; hole? : Type → Boolean
(define (hole? t)
  (or (eq? t '?)
      (and (pair? t) (eq? (car t) '?))))

;;; type-var? : Type → Boolean
(define (type-var? t)
  (and (symbol? t)
       (not (base-type? t))
       (not (eq? t '?))
       (char-lower-case? (string-ref (symbol->string t) 0))))

;;; ============================================================
;;; Kind-Annotated Type Variables (HKT Support)
;;; ============================================================

;;; A kinded type variable has the form (name : kind)
;;; Example: (f : (⇒ * *)) means f has kind * → *

;;; kinded-tvar? : Any → Boolean
;;; Is this a kind-annotated type variable?
(define (kinded-tvar? x)
  (and (pair? x)
       (= (length x) 3)
       (symbol? (car x))
       (eq? (cadr x) ':)))

;;; kinded-tvar-name : KindedTVar → Symbol
(define (kinded-tvar-name ktv)
  (car ktv))

;;; kinded-tvar-kind : KindedTVar → Kind
(define (kinded-tvar-kind ktv)
  (caddr ktv))

;;; forall-vars : Type → (List Symbol)
;;; Extract variable names from a forall, handling both simple and kinded forms.
(define (forall-vars t)
  (if (and (pair? t) (eq? (car t) '∀))
      (let ([vars (cadr t)])
           (map (lambda (v)
                        (if (kinded-tvar? v)
                            (kinded-tvar-name v)
                            v))
                vars))
      '()))

;;; forall-var-kinds : Type → (List (Symbol . Kind))
;;; Extract variable names with their kinds from a forall.
;;; Simple vars get kind *, kinded vars get their annotated kind.
(define (forall-var-kinds t)
  (if (and (pair? t) (eq? (car t) '∀))
      (let ([vars (cadr t)])
           (map (lambda (v)
                        (if (kinded-tvar? v)
                            (cons (kinded-tvar-name v) (kinded-tvar-kind v))
                            (cons v '*)))
                vars))
      '()))

;;; ============================================================
;;; Type Equality
;;; ============================================================

;;; type=? : Type × Type → Boolean
;;; Structural equality of types.
(define (type=? t1 t2)
  (cond
   [(and (symbol? t1) (symbol? t2)) (eq? t1 t2)]
   [(and (pair? t1) (pair? t2))
    (and (= (length t1) (length t2))
         (andmap (lambda (pair) (type=? (car pair) (cdr pair)))
                 (map cons t1 t2)))]
   [else #f]))

;;; ============================================================
;;; Free Type Variables
;;; ============================================================

;;; free-tvars : Type → (List Symbol)
;;; Collect free type variables in a type.
(define (free-tvars t)
  (free-tvars-with '() t))

(define (free-tvars-with bound t)
  (cond
   [(type-var? t)
    (if (memq t bound) '() (list t))]
   [(or (base-type? t) (hole? t)) '()]
   [(not (pair? t)) '()]
   [(eq? (car t) '∀)
    ;; Extract variable names from both simple (a) and kinded ((a : *)) forms
    (let* ([vars (cadr t)]
           [var-names (map (lambda (v)
                                   (if (kinded-tvar? v)
                                       (kinded-tvar-name v)
                                       v))
                           vars)]
           [new-bound (append var-names bound)])
          (free-tvars-with new-bound (caddr t)))]
   [(eq? (car t) 'μ)
    (let ([new-bound (cons (cadr t) bound)])
         (free-tvars-with new-bound (caddr t)))]
   [else
    (apply append (map (lambda (sub) (free-tvars-with bound sub)) (cdr t)))]))

;;; ============================================================
;;; Type Substitution
;;; ============================================================

;;; subst-type : Type × Symbol × Type → Type
;;; Substitute tvar with replacement in type.
(define (subst-type type tvar replacement)
  (cond
   [(eq? type tvar) replacement]
   [(or (base-type? type) (hole? type)) type]
   [(type-var? type) type]
   [(not (pair? type)) type]
   ;; Don't substitute under binders that shadow
   [(eq? (car type) '∀)
    ;; Extract var names from both simple (a) and kinded ((a : *)) forms
    (let ([var-names (map (lambda (v)
                                  (if (kinded-tvar? v)
                                      (kinded-tvar-name v)
                                      v))
                          (cadr type))])
         (if (memq tvar var-names)
             type  ; tvar is bound, don't substitute
             `(∀ ,(cadr type) ,(subst-type (caddr type) tvar replacement))))]
   [(eq? (car type) 'μ)
    (if (eq? tvar (cadr type))
        type  ; tvar is bound
        `(μ ,(cadr type) ,(subst-type (caddr type) tvar replacement)))]
   [else
    (cons (car type)
          (map (lambda (sub) (subst-type sub tvar replacement)) (cdr type)))]))

;;; ============================================================
;;; Common Type Patterns
;;; ============================================================

;;; Some frequently used types

(define T-unit 'Unit)
(define T-bool 'Bool)
(define T-nat 'Nat)
(define T-int 'Int)
(define T-char 'Char)
(define T-string 'String)
(define T-symbol 'Symbol)
(define T-bytes 'Bytes)
(define T-hash 'Hash)

;;; t-option : Type → Type
;;; Option type as a sum: (+ (None) (Some T))
(define (t-option t)
  (t+ '(None) `(Some ,t)))

;;; t-result : Type × Type → Type
;;; Result type: (+ (Ok T) (Err E))
(define (t-result ok-type err-type)
  (t+ `(Ok ,ok-type) `(Err ,err-type)))

;;; t-pair : Type × Type → Type
;;; Pair as product
(define (t-pair t1 t2)
  (t× t1 t2))

;;; ============================================================
;;; Type Display
;;; ============================================================

;;; type->string : Type → String
;;; Pretty-print a type.
(define (type->string t)
  (cond
   [(symbol? t) (symbol->string t)]
   [(eq? t '?) "?"]
   [(and (pair? t) (eq? (car t) '?))
    (string-append "?" (symbol->string (cadr t)))]
   [(and (pair? t) (eq? (car t) '->))
    (string-append "("
                   (join-strings " → " (map type->string (cdr t)))
                   ")")]
   [(and (pair? t) (eq? (car t) '×))
    (string-append "("
                   (join-strings " × " (map type->string (cdr t)))
                   ")")]
   [(pair? t)
    (string-append "("
                   (join-strings " " (map type->string t))
                   ")")]
   [else (format "~s" t)]))

;;; join-strings : String × (List String) → String
(define (join-strings sep strs)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Type as Block
;;; ============================================================

;;; Types can be serialized to blocks for storage in the CAS.
;;; This makes the type system homoiconic with the rest of The Fold.

;;; type->block : Type → Block
;;; Serialize a type to a block.
(define (type->block t)
  (make-block 'type
              (string->utf8 (format "~s" t))
              (vector)))

;;; block->type : Block → Type | #f
;;; Deserialize a type from a block.
(define (block->type blk)
  (if (eq? (block-tag blk) 'type)
      (let ([t (read (open-input-string (utf8->string (block-payload blk))))])
           (if (type? t) t #f))
      #f))
