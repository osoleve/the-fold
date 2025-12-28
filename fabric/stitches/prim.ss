;;; fabric/stitches/prim.ss — Pure Primitive Dispatcher
;;;
;;; The `prim` form dispatches to pure operations:
;;;   (prim 'op arg ...)
;;;
;;; All primitives are total: they always return a value (or error value).
;;; All primitives are pure: same inputs = same outputs, no effects.
;;;
;;; This is Core code: pure, total, assumes perfect input from Shell.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "prelude.ss")

;;; Categories:
;;;   - Arithmetic: add, sub, mul, div, mod, neg, abs
;;;   - Comparison: eq?, lt?, le?, gt?, ge?, zero?, positive?, negative?
;;;   - Bitwise: bitand, bitor, bitxor, bitnot, shl, shr
;;;   - Boolean: not, and, or
;;;   - Block: make-block, block-tag, block-payload, block-refs, block-ref
;;;   - Bytevector: bv-length, bv-ref, bv-make, bv-concat
;;;   - String: string-length, string-ref, string-append, substring, ...
;;;   - Character: char->integer, integer->char, char-alphabetic?, ...
;;;   - List: cons, car, cdr, null?, pair?, list
;;;   - Vector: vec-make, vec-ref, vec-length, vec->list, list->vec
;;;   - Type predicates: number?, symbol?, string?, char?, bytevector?, block?, vector?

;;; ============================================================
;;; Primitive Dispatcher
;;; ============================================================

;;; prim : Symbol × Args... → Value
;;; Dispatch to a pure primitive operation.
(define (prim op . args)
  (case op
    ;; --------------------------------------------------------
    ;; Arithmetic
    ;; --------------------------------------------------------
    [(add) (apply + args)]
    [(sub) (apply - args)]
    [(mul) (apply * args)]
    [(div)
     (if (zero? (cadr args))
         '(error div-by-zero)
         (quotient (car args) (cadr args)))]
    [(mod)
     (if (zero? (cadr args))
         '(error mod-by-zero)
         (modulo (car args) (cadr args)))]
    [(neg) (- (car args))]
    [(abs) (abs (car args))]
    [(sqrt) (sqrt (car args))]
    [(expt) (expt (car args) (cadr args))]
    [(log) (log (car args))]
    [(sin) (sin (car args))]
    [(cos) (cos (car args))]
    [(tan) (tan (car args))]
    [(floor) (floor (car args))]
    [(ceiling) (ceiling (car args))]
    [(round) (round (car args))]

    ;; --------------------------------------------------------
    ;; Comparison
    ;; --------------------------------------------------------
    [(eq?) (equal? (car args) (cadr args))]
    [(lt?) (< (car args) (cadr args))]
    [(le?) (<= (car args) (cadr args))]
    [(gt?) (> (car args) (cadr args))]
    [(ge?) (>= (car args) (cadr args))]
    [(zero?) (zero? (car args))]
    [(positive?) (positive? (car args))]
    [(negative?) (negative? (car args))]

    ;; --------------------------------------------------------
    ;; Bitwise
    ;; --------------------------------------------------------
    [(bitand) (bitwise-and (car args) (cadr args))]
    [(bitor) (bitwise-ior (car args) (cadr args))]
    [(bitxor) (bitwise-xor (car args) (cadr args))]
    [(bitnot) (bitwise-not (car args))]
    [(shl) (bitwise-arithmetic-shift-left (car args) (cadr args))]
    [(shr) (bitwise-arithmetic-shift-right (car args) (cadr args))]

    ;; --------------------------------------------------------
    ;; Boolean
    ;; --------------------------------------------------------
    [(not) (not (car args))]
    [(and) (and (car args) (cadr args))]
    [(or) (or (car args) (cadr args))]

    ;; --------------------------------------------------------
    ;; Block operations
    ;; --------------------------------------------------------
    [(make-block)
     ;; (prim 'make-block tag payload refs-vector)
     (make-block (car args) (cadr args) (caddr args))]
    [(block-tag) (block-tag (car args))]
    [(block-payload) (block-payload (car args))]
    [(block-refs) (block-refs (car args))]
    [(block-ref)
     ;; (prim 'block-ref block index)
     (let ([refs (block-refs (car args))]
           [idx (cadr args)])
       (if (< idx (vector-length refs))
           (vector-ref refs idx)
           '(error index-out-of-bounds)))]
    [(block->bytes) (block->bytes (car args))]
    [(bytes->block) (bytes->block (car args))]

    ;; --------------------------------------------------------
    ;; Bytevector operations
    ;; --------------------------------------------------------
    [(bv-length) (bytevector-length (car args))]
    [(bv-ref) (bytevector-u8-ref (car args) (cadr args))]
    [(bv-make) (make-bytevector (car args) (if (null? (cdr args)) 0 (cadr args)))]
    [(bv-concat) (bytevector-concat args)]
    [(bv-copy)
     ;; (prim 'bv-copy src src-start dst dst-start count)
     (let ([src (car args)]
           [ss (cadr args)]
           [dst (caddr args)]
           [ds (cadddr args)]
           [n (car (cddddr args))])
       (bytevector-copy! src ss dst ds n)
       dst)]
    [(bv-slice)
     ;; (prim 'bv-slice bv start end)
     (let* ([bv (car args)]
            [start (cadr args)]
            [end (caddr args)]
            [len (- end start)]
            [result (make-bytevector len)])
       (bytevector-copy! bv start result 0 len)
       result)]

    ;; --------------------------------------------------------
    ;; String operations
    ;; --------------------------------------------------------
    [(string-length) (string-length (car args))]
    [(string-ref) (string-ref (car args) (cadr args))]
    [(string-append) (apply string-append args)]
    [(substring)
     ;; (prim 'substring str start end)
     (substring (car args) (cadr args) (caddr args))]
    [(string=?) (string=? (car args) (cadr args))]
    [(string<?) (string<? (car args) (cadr args))]
    [(string>?) (string>? (car args) (cadr args))]
    [(make-string) (make-string (car args) (if (null? (cdr args)) #\space (cadr args)))]
    [(string->list) (string->list (car args))]
    [(list->string) (list->string (car args))]

    ;; --------------------------------------------------------
    ;; Character operations
    ;; --------------------------------------------------------
    [(char->integer) (char->integer (car args))]
    [(integer->char) (integer->char (car args))]
    [(char=?) (char=? (car args) (cadr args))]
    [(char<?) (char<? (car args) (cadr args))]
    [(char-alphabetic?) (char-alphabetic? (car args))]
    [(char-numeric?) (char-numeric? (car args))]
    [(char-whitespace?) (char-whitespace? (car args))]
    [(char-upper-case?) (char-upper-case? (car args))]
    [(char-lower-case?) (char-lower-case? (car args))]
    [(char-upcase) (char-upcase (car args))]
    [(char-downcase) (char-downcase (car args))]

    ;; --------------------------------------------------------
    ;; String/Bytevector conversion
    ;; --------------------------------------------------------
    [(string->utf8) (string->utf8 (car args))]
    [(utf8->string) (utf8->string (car args))]
    [(symbol->string) (symbol->string (car args))]
    [(string->symbol) (string->symbol (car args))]
    [(number->string) (number->string (car args))]
    [(string->number) (string->number (car args))]

    ;; --------------------------------------------------------
    ;; List operations
    ;; --------------------------------------------------------
    [(cons) (cons (car args) (cadr args))]
    [(car) (car (car args))]
    [(cdr) (cdr (car args))]
    [(null?) (null? (car args))]
    [(pair?) (pair? (car args))]
    [(list) args]  ; Variadic: (prim 'list a b c) → (a b c)
    [(length) (length (car args))]
    [(append) (apply append args)]
    [(reverse) (reverse (car args))]
    [(list-ref) (list-ref (car args) (cadr args))]
    [(memq) (memq (car args) (cadr args))]
    [(assq) (assq (car args) (cadr args))]

    ;; --------------------------------------------------------
    ;; Vector operations
    ;; --------------------------------------------------------
    [(vec-make) (apply vector args)]
    [(vec-empty) (vector)]
    [(vec-ref) (vector-ref (car args) (cadr args))]
    [(vec-length) (vector-length (car args))]
    [(vec->list) (vector->list (car args))]
    [(list->vec) (list->vector (car args))]

    ;; --------------------------------------------------------
    ;; Type predicates
    ;; --------------------------------------------------------
    [(number?) (number? (car args))]
    [(integer?) (integer? (car args))]
    [(symbol?) (symbol? (car args))]
    [(string?) (string? (car args))]
    [(char?) (char? (car args))]
    [(bytevector?) (bytevector? (car args))]
    [(block?) (block? (car args))]
    [(vector?) (vector? (car args))]
    [(list?) (list? (car args))]
    [(boolean?) (boolean? (car args))]
    [(procedure?) (procedure? (car args))]

    ;; --------------------------------------------------------
    ;; Hash operations (pure computation)
    ;; --------------------------------------------------------
    [(sha256) (sha256 (car args))]
    [(hash-block) (hash-block (car args))]
    [(hash->hex) (hash->hex (car args))]
    [(hex->hash) (hex->hash (car args))]

    ;; --------------------------------------------------------
    ;; Error: unknown primitive
    ;; --------------------------------------------------------
    [else `(error unknown-primitive ,op)]))

;;; ============================================================
;;; Primitive Metadata
;;; ============================================================

;;; prim-fuel-cost : Symbol → Nat | #f
;;; Return the fuel cost of a primitive operation.
;;; Note: Core eval does not charge primitives for fuel; this table is
;;; for benchmarking and analysis only.
;;;
;;; Fuel costs are determined by computational complexity relative to
;;; the simplest primitives, which have a cost of 1 fuel unit.
;;;
;;; Benchmarked on Windows/Chez Scheme 10.3.0 (see bench-prim.ss):
;;;   - Tiers 1-5 are dominated by interpreter dispatch overhead (~50-200ns)
;;;   - SHA-256 is ~500x more expensive than simple ops (~43,000ns)
;;;   - Block serialization is ~5-10x baseline, not crypto-level
;;;   - number->string is notably expensive due to formatting
;;;
;;; Cost tiers:
;;;   1  - O(1) trivial: type predicates, simple accessors, boolean ops
;;;   2  - O(1) simple: basic arithmetic, cons, character ops
;;;   3  - O(1) moderate: division/modulo, bitwise ops, construction
;;;   5  - O(n) linear: length, reverse, list conversions
;;;  10  - O(n) with allocation: string-append, bv-concat, slicing
;;;  15  - Expensive conversions: number->string (formatting overhead)
;;; 100  - Cryptographic: sha256
;;; 110  - Crypto + overhead: hash-block (serialization + sha256)
;;;
(define (prim-fuel-cost op)
  (case op
    ;; --------------------------------------------------------
    ;; Tier 1 (Cost 1) - O(1) trivial operations
    ;; Benchmarked: ~40-100ns/op (interpreter overhead dominates)
    ;; --------------------------------------------------------
    ;; Type predicates - single tag check
    [(number? integer? symbol? string? char? bytevector? block?
      vector? list? boolean? procedure? null? pair?)
     1]
    ;; Simple accessors - direct field access
    [(car cdr block-tag block-payload block-refs)
     1]
    ;; Boolean operations - trivial logic
    [(not and or)
     1]
    ;; Simple comparisons (fixed-size types)
    [(eq? zero? positive? negative?)
     1]

    ;; --------------------------------------------------------
    ;; Tier 2 (Cost 2) - O(1) simple operations
    ;; Benchmarked: ~40-110ns/op
    ;; --------------------------------------------------------
    ;; Basic arithmetic
    [(add sub mul neg abs)
     2]
    ;; Mathematical functions
    [(floor ceiling round)
     2]
    ;; Numeric comparisons
    [(lt? le? gt? ge?)
     2]
    ;; Character operations (single char)
    [(char->integer integer->char char=? char<?
      char-alphabetic? char-numeric? char-whitespace?
      char-upper-case? char-lower-case? char-upcase char-downcase)
     2]
    ;; cons - single allocation
    [(cons)
     2]
    ;; Simple indexed access
    [(bv-ref vec-ref string-ref list-ref block-ref)
     2]
    ;; Length for fixed-size structures
    [(bv-length vec-length string-length)
     2]

    ;; --------------------------------------------------------
    ;; Tier 3 (Cost 3) - O(1) moderate operations
    ;; Benchmarked: ~40-160ns/op
    ;; --------------------------------------------------------
    ;; Division/modulo - more CPU cycles
    [(div mod)
     3]
    ;; Power and roots
    [(expt sqrt)
     4]
    ;; Transcendental functions - higher computational cost
    [(log sin cos tan)
     5]
    ;; Bitwise operations
    [(bitand bitor bitxor bitnot shl shr)
     3]
    ;; Block/vector construction - allocation + setup
    [(make-block vec-make vec-empty bv-make make-string)
     3]
    ;; Simple string comparisons (may scan)
    [(string=? string<? string>?)
     3]
    ;; memq/assq - O(n) but typically short lists
    [(memq assq)
     3]

    ;; --------------------------------------------------------
    ;; Tier 4 (Cost 5) - O(n) linear operations
    ;; Benchmarked: ~80-200ns/op for small inputs
    ;; --------------------------------------------------------
    ;; List traversal
    [(length reverse)
     5]
    ;; List/vector conversions
    [(vec->list list->vec)
     5]
    ;; String/list conversions
    [(string->list list->string)
     5]
    ;; List operations
    [(list append)
     5]
    ;; Symbol/string conversions
    [(symbol->string string->symbol)
     5]

    ;; --------------------------------------------------------
    ;; Tier 5 (Cost 10) - O(n) with allocation
    ;; Benchmarked: ~70-150ns/op for small inputs
    ;; --------------------------------------------------------
    ;; String operations with allocation
    [(string-append substring)
     10]
    ;; Bytevector operations with allocation
    [(bv-concat bv-copy bv-slice)
     10]
    ;; UTF-8 encoding/decoding
    [(string->utf8 utf8->string)
     10]
    ;; Block serialization/deserialization
    ;; Benchmarked: ~200-460ns/op (much cheaper than crypto)
    [(block->bytes bytes->block)
     10]
    ;; Hex conversion
    [(hash->hex hex->hash)
     10]
    ;; String/number parsing
    [(string->number)
     10]

    ;; --------------------------------------------------------
    ;; Tier 6 (Cost 15) - Expensive conversions
    ;; Benchmarked: ~1000ns/op (formatting overhead)
    ;; --------------------------------------------------------
    [(number->string)
     15]

    ;; --------------------------------------------------------
    ;; Tier 7 (Cost 100) - Cryptographic operations
    ;; Benchmarked: ~43,000ns/op (~500x baseline)
    ;; --------------------------------------------------------
    [(sha256)
     100]

    ;; --------------------------------------------------------
    ;; Tier 8 (Cost 110) - Serialization + cryptographic
    ;; Benchmarked: ~44,000ns/op (block->bytes + sha256)
    ;; --------------------------------------------------------
    [(hash-block)
     110]

    ;; --------------------------------------------------------
    ;; Unknown primitive
    ;; --------------------------------------------------------
    [else #f]))

;;; prim-arity : Symbol → Nat | 'variadic | #f
;;; Return the expected arity of a primitive.
(define (prim-arity op)
  (case op
    ;; Unary
    [(neg abs zero? positive? negative? not null? pair?
      block-tag block-payload block-refs block->bytes bytes->block
      bv-length car cdr length reverse list->vec vec->list vec-length
      number? integer? symbol? string? char? bytevector? block? vector?
      list? boolean? procedure?
      sha256 hash-block hash->hex hex->hash
      string->utf8 utf8->string symbol->string string->symbol
      number->string string->number
      ;; String (unary)
      string-length string->list list->string
      ;; Character (unary)
      char->integer integer->char
      char-alphabetic? char-numeric? char-whitespace?
      char-upper-case? char-lower-case? char-upcase char-downcase)
     1]
    ;; Binary
    [(add sub mul div mod eq? lt? le? gt? ge?
      bitand bitor bitxor shl shr and or
      cons bv-ref vec-ref list-ref memq assq block-ref
      ;; String (binary)
      string-ref string=? string<? string>?
      ;; Character (binary)
      char=? char<?)
     2]
    ;; Ternary
    [(make-block bv-slice substring) 3]
    ;; 5-ary
    [(bv-copy) 5]
    ;; Variadic
    [(list vec-make bv-concat append string-append) 'variadic]
    ;; Optionally 1 or 2
    [(bv-make bitnot make-string) 'variadic]
    ;; Unknown
    [else #f]))

;;; prim-pure? : Symbol → Boolean
;;; All primitives are pure (this is Core).
(define (prim-pure? op)
  (if (prim-arity op) #t #f))

;;; list-primitives : → (List Symbol)
;;; List all known primitives.
(define (list-primitives)
  '(;; Arithmetic
    add sub mul div mod neg abs sqrt expt log sin cos tan floor ceiling round
    ;; Comparison
    eq? lt? le? gt? ge? zero? positive? negative?
    ;; Bitwise
    bitand bitor bitxor bitnot shl shr
    ;; Boolean
    not and or
    ;; Block
    make-block block-tag block-payload block-refs block-ref
    block->bytes bytes->block
    ;; Bytevector
    bv-length bv-ref bv-make bv-concat bv-copy bv-slice
    ;; String
    string-length string-ref string-append substring
    string=? string<? string>? make-string string->list list->string
    ;; Character
    char->integer integer->char char=? char<?
    char-alphabetic? char-numeric? char-whitespace?
    char-upper-case? char-lower-case? char-upcase char-downcase
    ;; Conversion
    string->utf8 utf8->string symbol->string string->symbol
    number->string string->number
    ;; List
    cons car cdr null? pair? list length append reverse list-ref memq assq
    ;; Vector
    vec-make vec-ref vec-length vec->list list->vec
    ;; Type predicates
    number? integer? symbol? string? char? bytevector? block? vector?
    list? boolean? procedure?
    ;; Hash
    sha256 hash-block hash->hex hex->hash))
