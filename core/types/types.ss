(load "core/base/prelude.ss")

(doc 'module 'types)
(doc 'description "The Type System of The Fold. Types are Blocks, S-expressions, and data. Structural, homoiconic, bidirectional, capability-aware, and gradual.")
(doc 'layer 'core)

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

(doc 'section 'base-types)

(doc base-types 'type (List Symbol))
(doc base-types 'description "The primitive types of The Fold.")
(doc base-types 'export #t)
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

(define (base-type? t)
  (doc 'type (-> Any Boolean))
  (doc 'description "Check if t is a base type.")
  (doc 'export #t)
  (if (memq t base-types) #t #f))

(doc 'section 'type-predicates)

(define (type? t)
  (doc 'type (-> Any Boolean))
  (doc 'description "Is this a well-formed type expression?")
  (doc 'export #t)
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

(define (variant? v)
  (doc 'type (-> Any Boolean))
  (doc 'description "Is this a valid sum type variant?")
  (doc 'export #t)
  (and (pair? v)
       (symbol? (car v))
       (andmap type? (cdr v))))

;;; Note: andmap is provided by prelude.ss

(doc 'section 'type-constructors)

(define (t-> . types)
  (doc 'type (-> Type ... Type))
  (doc 'description "Construct a function type.")
  (doc 'export #t)
  (cons '-> types))

(define (t× . types)
  (doc 'type (-> Type ... Type))
  (doc 'description "Construct a product type.")
  (doc 'export #t)
  (cons '× types))

(define (t+ . variants)
  (doc 'type (-> (× Tag Type ...) ... Type))
  (doc 'description "Construct a sum type.")
  (doc 'export #t)
  (cons '+ variants))

(define (t-list elem-type)
  (doc 'type (-> Type Type))
  (doc 'description "Construct a List type.")
  (doc 'export #t)
  `(List ,elem-type))

(define (t-vector elem-type)
  (doc 'type (-> Type Type))
  (doc 'description "Construct a Vector type.")
  (doc 'export #t)
  `(Vector ,elem-type))

(define (t-block tag payload-type)
  (doc 'type (-> Symbol Type Type))
  (doc 'description "Construct a Block type.")
  (doc 'export #t)
  `(Block ,tag ,payload-type))

(define (t-ref target-type)
  (doc 'type (-> Type Type))
  (doc 'description "Construct a Ref type.")
  (doc 'export #t)
  `(Ref ,target-type))

(define (t-forall vars body)
  (doc 'type (-> (List Symbol) Type Type))
  (doc 'description "Construct a universally quantified type. Simple form: vars are symbols, all assumed kind *.")
  (doc 'export #t)
  `(∀ ,vars ,body))

(define (t-forall-kinded kinded-vars body)
  (doc 'type (-> (List (× Symbol Kind)) Type Type))
  (doc 'description "Construct a kinded forall. Vars are (name . kind) pairs for HKT support.")
  (doc 'export #t)
  `(∀ ,(map (lambda (pair) `(,(car pair) : ,(cdr pair))) kinded-vars) ,body))

(define (t-rec var body)
  (doc 'type (-> Symbol Type Type))
  (doc 'description "Construct a recursive type (μ var body).")
  (doc 'export #t)
  `(μ ,var ,body))

(define (t-cap cap-name inner-type)
  (doc 'type (-> Symbol Type Type))
  (doc 'description "Construct a capability-requiring type.")
  (doc 'export #t)
  `(Cap ,cap-name ,inner-type))

(define (t-hole)
  (doc 'type (-> Type))
  (doc 'description "Construct an anonymous type hole.")
  (doc 'export #t)
  '?)

(define (t-named-hole name)
  (doc 'type (-> Symbol Type))
  (doc 'description "Construct a named type hole.")
  (doc 'export #t)
  `(? ,name))

(doc 'section 'type-accessors)

(define (function-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a function type?")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) '->)))

(define (function-param-types t)
  (doc 'type (-> Type (List Type)))
  (doc 'description "Extract parameter types from a function type.")
  (doc 'export #t)
  (if (function-type? t)
      (reverse (cdr (reverse (cdr t))))  ; All but first and last
      '()))

(define (function-return-type t)
  (doc 'type (-> Type Type))
  (doc 'description "Extract return type from a function type.")
  (doc 'export #t)
  (if (function-type? t)
      (car (reverse (cdr t)))  ; Last element
      'Void))

(define (product-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a product type?")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) '×)))

(define (product-types t)
  (doc 'type (-> Type (List Type)))
  (doc 'description "Extract component types from a product type.")
  (doc 'export #t)
  (if (product-type? t) (cdr t) '()))

(define (sum-type? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a sum type?")
  (doc 'export #t)
  (and (pair? t) (eq? (car t) '+)))

(define (sum-variants t)
  (doc 'type (-> Type (List (× Tag Type ...))))
  (doc 'description "Extract variants from a sum type.")
  (doc 'export #t)
  (if (sum-type? t) (cdr t) '()))

(define (hole? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a type hole?")
  (doc 'export #t)
  (or (eq? t '?)
      (and (pair? t) (eq? (car t) '?))))

(define (type-var? t)
  (doc 'type (-> Type Boolean))
  (doc 'description "Is this a type variable?")
  (doc 'export #t)
  (and (symbol? t)
       (not (base-type? t))
       (not (eq? t '?))
       (char-lower-case? (string-ref (symbol->string t) 0))))

(doc 'section 'kinded-type-variables)

(define (kinded-tvar? x)
  (doc 'type (-> Any Boolean))
  (doc 'description "Is this a kind-annotated type variable? Form: (name : kind)")
  (doc 'export #t)
  (and (pair? x)
       (= (length x) 3)
       (symbol? (car x))
       (eq? (cadr x) ':)))

(define (kinded-tvar-name ktv)
  (doc 'type (-> KindedTVar Symbol))
  (doc 'description "Extract the name from a kinded type variable.")
  (doc 'export #t)
  (car ktv))

(define (kinded-tvar-kind ktv)
  (doc 'type (-> KindedTVar Kind))
  (doc 'description "Extract the kind from a kinded type variable.")
  (doc 'export #t)
  (caddr ktv))

(define (forall-vars t)
  (doc 'type (-> Type (List Symbol)))
  (doc 'description "Extract variable names from a forall, handling both simple and kinded forms.")
  (doc 'export #t)
  (if (and (pair? t) (eq? (car t) '∀))
      (let ([vars (cadr t)])
           (map (lambda (v)
                        (if (kinded-tvar? v)
                            (kinded-tvar-name v)
                            v))
                vars))
      '()))

(define (forall-var-kinds t)
  (doc 'type (-> Type (List (× Symbol Kind))))
  (doc 'description "Extract variable names with their kinds from a forall. Simple vars get kind *, kinded vars get their annotated kind.")
  (doc 'export #t)
  (if (and (pair? t) (eq? (car t) '∀))
      (let ([vars (cadr t)])
           (map (lambda (v)
                        (if (kinded-tvar? v)
                            (cons (kinded-tvar-name v) (kinded-tvar-kind v))
                            (cons v '*)))
                vars))
      '()))

(doc 'section 'type-equality)

(define (type=? t1 t2)
  (doc 'type (-> Type Type Boolean))
  (doc 'description "Structural equality of types.")
  (doc 'export #t)
  (cond
   [(and (symbol? t1) (symbol? t2)) (eq? t1 t2)]
   [(and (pair? t1) (pair? t2))
    (and (= (length t1) (length t2))
         (andmap (lambda (pair) (type=? (car pair) (cdr pair)))
                 (map cons t1 t2)))]
   [else #f]))

(doc 'section 'free-type-variables)

(define (free-tvars t)
  (doc 'type (-> Type (List Symbol)))
  (doc 'description "Collect free type variables in a type.")
  (doc 'export #t)
  (free-tvars-with '() t))

(define (free-tvars-with bound t)
  (doc 'type (-> (List Symbol) Type (List Symbol)))
  (doc 'description "Helper: collect free type variables with bound context.")
  (doc 'export #f)
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
    (append-map (lambda (sub) (free-tvars-with bound sub)) (cdr t))]))

(doc 'section 'type-substitution)

(define (subst-type type tvar replacement)
  (doc 'type (-> Type Symbol Type Type))
  (doc 'description "Substitute tvar with replacement in type.")
  (doc 'export #t)
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

(doc 'section 'common-type-patterns)

(doc T-unit 'type Type)
(doc T-unit 'description "The Unit type constant.")
(doc T-unit 'export #t)
(define T-unit 'Unit)

(doc T-bool 'type Type)
(doc T-bool 'description "The Bool type constant.")
(doc T-bool 'export #t)
(define T-bool 'Bool)

(doc T-nat 'type Type)
(doc T-nat 'description "The Nat type constant.")
(doc T-nat 'export #t)
(define T-nat 'Nat)

(doc T-int 'type Type)
(doc T-int 'description "The Int type constant.")
(doc T-int 'export #t)
(define T-int 'Int)

(doc T-char 'type Type)
(doc T-char 'description "The Char type constant.")
(doc T-char 'export #t)
(define T-char 'Char)

(doc T-string 'type Type)
(doc T-string 'description "The String type constant.")
(doc T-string 'export #t)
(define T-string 'String)

(doc T-symbol 'type Type)
(doc T-symbol 'description "The Symbol type constant.")
(doc T-symbol 'export #t)
(define T-symbol 'Symbol)

(doc T-bytes 'type Type)
(doc T-bytes 'description "The Bytes type constant.")
(doc T-bytes 'export #t)
(define T-bytes 'Bytes)

(doc T-hash 'type Type)
(doc T-hash 'description "The Hash type constant.")
(doc T-hash 'export #t)
(define T-hash 'Hash)

(define (t-option t)
  (doc 'type (-> Type Type))
  (doc 'description "Option type as a sum: (+ (None) (Some T)).")
  (doc 'export #t)
  (t+ '(None) `(Some ,t)))

(define (t-result ok-type err-type)
  (doc 'type (-> Type Type Type))
  (doc 'description "Result type: (+ (Ok T) (Err E)).")
  (doc 'export #t)
  (t+ `(Ok ,ok-type) `(Err ,err-type)))

(define (t-pair t1 t2)
  (doc 'type (-> Type Type Type))
  (doc 'description "Pair as product type.")
  (doc 'export #t)
  (t× t1 t2))

(doc 'section 'type-display)

(define (type->string t)
  (doc 'type (-> Type String))
  (doc 'description "Pretty-print a type.")
  (doc 'export #t)
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

(define (join-strings sep strs)
  (doc 'type (-> String (List String) String))
  (doc 'description "Join strings with separator.")
  (doc 'export #t)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

(doc 'section 'type-as-block)

(define (type->block t)
  (doc 'type (-> Type Block))
  (doc 'description "Serialize a type to a block for CAS storage.")
  (doc 'export #t)
  (make-block 'type
              (string->utf8 (format "~s" t))
              (vector)))

(define (block->type blk)
  (doc 'type (-> Block (Maybe Type)))
  (doc 'description "Deserialize a type from a block. Returns #f if invalid.")
  (doc 'export #t)
  (if (eq? (block-tag blk) 'type)
      (let ([t (read (open-input-string (utf8->string (block-payload blk))))])
           (if (type? t) t #f))
      #f))
