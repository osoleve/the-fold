;;; core/blocks/op-properties.ss — Algebraic properties for normalization
;;; @module op-properties
;;; @requires prelude
;;;
;;; Declares algebraic properties (commutativity, associativity) of operations.
;;; Used by normalize-algebraic to determine when argument reordering is
;;; semantically valid for content-addressed hashing.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Operation Properties Registry
;;; ====

;;; *op-properties* : Alist (Symbol -> Alist (Property -> Bool))
;;; Registry of algebraic properties for operations.
;;;
;;; Properties:
;;;   - commutative: (op a b) = (op b a)
;;;   - associative: (op (op a b) c) = (op a (op b c))
;;;
;;; IMPORTANT: Only list operations that are TRULY commutative/associative
;;; in the mathematical sense. Short-circuiting operators (and, or) are NOT
;;; commutative because (and (check-auth) (delete-db)) ≠ (and (delete-db) (check-auth))

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

;;; CAUTION: Floating-point + and * are NOT truly associative due to
;;; precision loss. For example:
;;;   (+ 1e20 1.0 -1e20) may give different results than (+ 1.0 1e20 -1e20)
;;; Consider restricting algebraic normalization to exact numbers only,
;;; or document and accept this semantic divergence for floats.

;;; ====
;;; Property Accessors
;;; ====

;;; op-property : Symbol × Symbol → Bool
;;; Look up a specific property for an operation.
(define (op-property op prop)
  (let ([props (assq op *op-properties*)])
    (if props
        (let ([val (assq prop (cdr props))])
          (if val (cdr val) #f))
        #f)))

;;; op-commutative? : Symbol → Bool
;;; Is the operation commutative? (op a b) = (op b a)
(define (op-commutative? op)
  (op-property op 'commutative))

;;; op-associative? : Symbol → Bool
;;; Is the operation associative? (op (op a b) c) = (op a (op b c))
(define (op-associative? op)
  (op-property op 'associative))

;;; ====
;;; Known Pure Operations
;;; ====

;;; Operations known to be pure (no side effects).
;;; Used for sequence canonicalization - only pure expressions can be reordered.
;;; CONSERVATIVE: Default to impure. Only whitelist verified pure operations.

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

;;; op-pure? : Symbol → Bool
;;; Is the operation known to be pure?
(define (op-pure? op)
  (if (memq op *pure-ops*) #t #f))

;;; ====
;;; Identity and Absorbing Elements
;;; ====
;;;
;;; Identity element: (op x identity) = x
;;; Absorbing element: (op x absorbing) = absorbing
;;;
;;; These enable simplifications during normalization:
;;;   (+ x 0)   → x
;;;   (* x 1)   → x
;;;   (* x 0)   → 0
;;;   (append x '()) → x

;;; *op-identities* : Alist (Symbol -> Value)
;;; Maps operations to their identity elements.
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

;;; *op-absorbing* : Alist (Symbol -> Value)
;;; Maps operations to their absorbing elements.
(define *op-absorbing*
  `((* . 0)
    (bitwise-and . 0)       ; All bits clear
    (set-intersection . ()) ; Empty set absorbs
    (gcd . 1)))             ; (gcd x 1) = 1 (when x > 0)

;;; op-identity : Symbol → Value | #f
;;; Return the identity element for an operation, or #f if none.
(define (op-identity op)
  (let ([entry (assq op *op-identities*)])
    (if entry (cdr entry) #f)))

;;; op-absorbing : Symbol → Value | #f
;;; Return the absorbing element for an operation, or #f if none.
(define (op-absorbing op)
  (let ([entry (assq op *op-absorbing*)])
    (if entry (cdr entry) #f)))

;;; identity-element? : Symbol × Value → Bool
;;; Is this value the identity element for this operation?
(define (identity-element? op val)
  (let ([id (op-identity op)])
    (and id (equal? id val))))

;;; absorbing-element? : Symbol × Value → Bool
;;; Is this value an absorbing element for this operation?
(define (absorbing-element? op val)
  (let ([abs (op-absorbing op)])
    (and abs (equal? abs val))))
