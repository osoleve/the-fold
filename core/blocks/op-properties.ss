(load "core/base/prelude.ss")

(doc 'module 'op-properties)
(doc 'description "Algebraic properties for normalization. Declares commutativity and associativity of operations.")
(doc 'layer 'core)

(doc 'section 'registry)

(doc *op-properties* 'type (Alist (Symbol . (Alist (Property . Boolean)))))
(doc *op-properties* 'description "Registry of algebraic properties for operations.")
(doc *op-properties* 'export #t)

(define *op-properties*
  '((+   . ((commutative . #t) (associative . #t)))
    (*   . ((commutative . #t) (associative . #t)))
    (min . ((commutative . #t) (associative . #t)))
    (max . ((commutative . #t) (associative . #t)))
    (gcd . ((commutative . #t) (associative . #t)))
    (lcm . ((commutative . #t) (associative . #t)))
    ;; Bitwise operations
    (bitwise-and . ((commutative . #t) (associative . #t)))
    (bitwise-ior . ((commutative . #t) (associative . #t)))
    (bitwise-xor . ((commutative . #t) (associative . #t)))
    ;; Set operations (unordered collections)
    (set-union        . ((commutative . #t) (associative . #t)))
    (set-intersection . ((commutative . #t) (associative . #t)))
    ;; Associative only (order matters)
    (append  . ((associative . #t)))
    (compose . ((associative . #t)))))

(doc 'section 'property-accessors)

(define (op-property op prop)
  (doc 'type (-> Symbol Symbol Boolean))
  (doc 'description "Look up a specific property for an operation.")
  (doc 'export #t)
  (let ([props (assq op *op-properties*)])
    (if props
        (let ([val (assq prop (cdr props))])
          (if val (cdr val) #f))
        #f)))

(define (op-commutative? op)
  (doc 'type (-> Symbol Boolean))
  (doc 'description "Is the operation commutative? (op a b) = (op b a)")
  (doc 'export #t)
  (op-property op 'commutative))

(define (op-associative? op)
  (doc 'type (-> Symbol Boolean))
  (doc 'description "Is the operation associative? (op (op a b) c) = (op a (op b c))")
  (doc 'export #t)
  (op-property op 'associative))

(doc 'section 'pure-operations)

(doc *pure-ops* 'type (List Symbol))
(doc *pure-ops* 'description "Operations known to be pure (no side effects). Conservative whitelist.")

(define *pure-ops*
  '(;; Arithmetic
    + - * / quotient remainder modulo abs
    floor ceiling truncate round
    expt sqrt exp log sin cos tan
    ;; Comparison
    < > <= >= = eq? eqv? equal?
    zero? positive? negative? odd? even?
    ;; List operations (non-mutating)
    car cdr caar cadr cdar cddr
    cons list list* append reverse
    length list-ref list-tail
    memq memv member assq assv assoc
    map filter fold-left fold-right
    ;; Vector operations (non-mutating)
    vector vector-ref vector-length
    vector->list list->vector
    ;; String operations (non-mutating)
    string string-ref string-length
    string-append substring
    string->list list->string
    string<? string>? string<=? string>=? string=?
    ;; Predicates
    null? pair? list? number? integer? real?
    string? symbol? boolean? char? vector? procedure?
    ;; Symbol/char
    symbol->string string->symbol
    char->integer integer->char
    char<? char>? char<=? char>=? char=?
    ;; Logic (note: these are functions, not short-circuit macros)
    not
    ;; Type constructors
    cons list vector make-vector))

(define (op-pure? op)
  (doc 'type (-> Symbol Boolean))
  (doc 'description "Is the operation known to be pure?")
  (doc 'export #t)
  (if (memq op *pure-ops*) #t #f))

(doc 'section 'identity-absorbing)

(doc *op-identities* 'type (Alist (Symbol . Any)))
(doc *op-identities* 'description "Maps operations to their identity elements.")
(define *op-identities*
  `((+ . 0)
    (* . 1)
    (- . 0)                 ; Right identity only: (- x 0) = x
    (min . +inf.0)
    (max . -inf.0)
    (gcd . 0)               ; (gcd x 0) = x
    (lcm . 1)               ; (lcm x 1) = x
    (bitwise-and . -1)      ; All bits set
    (bitwise-ior . 0)
    (bitwise-xor . 0)
    (append . ())
    (string-append . "")
    (set-union . ())        ; Empty set
    (set-intersection . #f) ; Universal set (no finite representation)
    (compose . identity)))  ; Identity function

(doc *op-absorbing* 'type (Alist (Symbol . Any)))
(doc *op-absorbing* 'description "Maps operations to their absorbing elements.")
(define *op-absorbing*
  `((* . 0)
    (bitwise-and . 0)
    (set-intersection . ())
    (gcd . 1)))

(define (op-identity op)
  (doc 'type (-> Symbol (Maybe Any)))
  (doc 'description "Return the identity element for an operation, or #f if none.")
  (doc 'export #t)
  (let ([entry (assq op *op-identities*)])
    (if entry (cdr entry) #f)))

(define (op-absorbing op)
  (doc 'type (-> Symbol (Maybe Any)))
  (doc 'description "Return the absorbing element for an operation, or #f if none.")
  (doc 'export #t)
  (let ([entry (assq op *op-absorbing*)])
    (if entry (cdr entry) #f)))

(define (identity-element? op val)
  (doc 'type (-> Symbol Any Boolean))
  (doc 'description "Is this value the identity element for this operation?")
  (doc 'export #t)
  (let ([id (op-identity op)])
    (and id (equal? id val))))

(define (absorbing-element? op val)
  (doc 'type (-> Symbol Any Boolean))
  (doc 'description "Is this value an absorbing element for this operation?")
  (doc 'export #t)
  (let ([abs (op-absorbing op)])
    (and abs (equal? abs val))))
