;;; fabric/stitches/help.ss — Primitive Help System
;;;
;;; Provides discovery and documentation for primitives:
;;;   (help)                    - List all primitives by category
;;;   (help 'prim-name)         - Show detailed help for specific primitive
;;;   (apropos "pattern")       - Find primitives matching pattern
;;;   (help-categories)         - List available categories
;;;   (help-by-category 'cat)   - List primitives in specific category
;;;
;;; This is Core code: pure, total, assumes perfect input from Shell.
;;;
;;; Dependencies:
;;;   - prim.ss (for primitive metadata)

(load "core/prim.ss")

;;; ============================================================
;;; Primitive Documentation Database
;;; ============================================================

;;; Primitive documentation structure:
;;;   name        - Symbol
;;;   category    - Symbol (arithmetic, comparison, bitwise, etc.)
;;;   signature   - String showing parameter types
;;;   description - Brief description
;;;   examples    - List of example usage strings
;;;   fuel-cost   - Fuel cost (from prim-fuel-cost)
;;;   arity       - Arity info (from prim-arity)

(define *primitive-docs*
  `(
    ;; ============================================================
    ;; Arithmetic Operations
    ;; ============================================================
    ((name . add) (category . arithmetic) (signature . "(add a b ...)")
     (description . "Add numbers. Variadic.")
     (examples . ("(prim 'add 2 3) → 5" "(prim 'add 1 2 3 4) → 10")))
    
    ((name . sub) (category . arithmetic) (signature . "(sub a b)")
     (description . "Subtract b from a.")
     (examples . ("(prim 'sub 5 3) → 2" "(prim 'sub 10 7) → 3")))
    
    ((name . mul) (category . arithmetic) (signature . "(mul a b ...)")
     (description . "Multiply numbers. Variadic.")
     (examples . ("(prim 'mul 2 3) → 6" "(prim 'mul 2 3 4) → 24")))
    
    ((name . div) (category . arithmetic) (signature . "(div a b)")
     (description . "Integer division of a by b. Returns error on divide by zero.")
     (examples . ("(prim 'div 10 3) → 3" "(prim 'div 10 0) → (error div-by-zero)")))
    
    ((name . mod) (category . arithmetic) (signature . "(mod a b)")
     (description . "Modulo operation. Returns error on divide by zero.")
     (examples . ("(prim 'mod 10 3) → 1" "(prim 'mod 10 0) → (error mod-by-zero)")))
    
    ((name . neg) (category . arithmetic) (signature . "(neg a)")
     (description . "Negate a number.")
     (examples . ("(prim 'neg 5) → -5" "(prim 'neg -3) → 3")))
    
    ((name . abs) (category . arithmetic) (signature . "(abs a)")
     (description . "Absolute value of a number.")
     (examples . ("(prim 'abs -5) → 5" "(prim 'abs 3) → 3")))
    
    ((name . sqrt) (category . arithmetic) (signature . "(sqrt a)")
     (description . "Square root of a number.")
     (examples . ("(prim 'sqrt 16) → 4" "(prim 'sqrt 2) → 1.414...")))
    
    ((name . expt) (category . arithmetic) (signature . "(expt base exp)")
     (description . "Raise base to the power exp.")
     (examples . ("(prim 'expt 2 3) → 8" "(prim 'expt 10 0) → 1")))
    
    ((name . log) (category . arithmetic) (signature . "(log a)")
     (description . "Natural logarithm.")
     (examples . ("(prim 'log 1) → 0" "(prim 'log 2.718...) → 1")))
    
    ((name . sin) (category . arithmetic) (signature . "(sin a)")
     (description . "Sine function (radians).")
     (examples . ("(prim 'sin 0) → 0" "(prim 'sin 1.5708) → 1")))
    
    ((name . cos) (category . arithmetic) (signature . "(cos a)")
     (description . "Cosine function (radians).")
     (examples . ("(prim 'cos 0) → 1" "(prim 'cos 3.1416) → -1")))
    
    ((name . tan) (category . arithmetic) (signature . "(tan a)")
     (description . "Tangent function (radians).")
     (examples . ("(prim 'tan 0) → 0")))
    
    ((name . floor) (category . arithmetic) (signature . "(floor a)")
     (description . "Round down to nearest integer.")
     (examples . ("(prim 'floor 3.7) → 3" "(prim 'floor -3.2) → -4")))
    
    ((name . ceiling) (category . arithmetic) (signature . "(ceiling a)")
     (description . "Round up to nearest integer.")
     (examples . ("(prim 'ceiling 3.2) → 4" "(prim 'ceiling -3.7) → -3")))
    
    ((name . round) (category . arithmetic) (signature . "(round a)")
     (description . "Round to nearest integer.")
     (examples . ("(prim 'round 3.5) → 4" "(prim 'round 3.4) → 3")))
    
    ;; ============================================================
    ;; Comparison Operations
    ;; ============================================================
    ((name . eq?) (category . comparison) (signature . "(eq? a b)")
     (description . "Test if two values are equal.")
     (examples . ("(prim 'eq? 1 1) → #t" "(prim 'eq? 1 2) → #f" "(prim 'eq? 'a 'a) → #t")))
    
    ((name . lt?) (category . comparison) (signature . "(lt? a b)")
     (description . "Test if a is less than b.")
     (examples . ("(prim 'lt? 1 2) → #t" "(prim 'lt? 2 2) → #f" "(prim 'lt? 3 2) → #f")))
    
    ((name . le?) (category . comparison) (signature . "(le? a b)")
     (description . "Test if a is less than or equal to b.")
     (examples . ("(prim 'le? 1 2) → #t" "(prim 'le? 2 2) → #t")))
    
    ((name . gt?) (category . comparison) (signature . "(gt? a b)")
     (description . "Test if a is greater than b.")
     (examples . ("(prim 'gt? 3 2) → #t" "(prim 'gt? 2 2) → #f")))
    
    ((name . ge?) (category . comparison) (signature . "(ge? a b)")
     (description . "Test if a is greater than or equal to b.")
     (examples . ("(prim 'ge? 3 2) → #t" "(prim 'ge? 2 2) → #t")))
    
    ((name . zero?) (category . comparison) (signature . "(zero? a)")
     (description . "Test if a number is zero.")
     (examples . ("(prim 'zero? 0) → #t" "(prim 'zero? 1) → #f")))
    
    ((name . positive?) (category . comparison) (signature . "(positive? a)")
     (description . "Test if a number is positive.")
     (examples . ("(prim 'positive? 5) → #t" "(prim 'positive? -3) → #f")))
    
    ((name . negative?) (category . comparison) (signature . "(negative? a)")
     (description . "Test if a number is negative.")
     (examples . ("(prim 'negative? -5) → #t" "(prim 'negative? 3) → #f")))
    
    ;; ============================================================
    ;; Bitwise Operations
    ;; ============================================================
    ((name . bitand) (category . bitwise) (signature . "(bitand a b)")
     (description . "Bitwise AND of two integers.")
     (examples . ("(prim 'bitand 7 3) → 3" "(prim 'bitand 12 10) → 8")))
    
    ((name . bitor) (category . bitwise) (signature . "(bitor a b)")
     (description . "Bitwise OR of two integers.")
     (examples . ("(prim 'bitor 7 3) → 7" "(prim 'bitor 12 3) → 15")))
    
    ((name . bitxor) (category . bitwise) (signature . "(bitxor a b)")
     (description . "Bitwise XOR of two integers.")
     (examples . ("(prim 'bitxor 7 3) → 4" "(prim 'bitxor 5 5) → 0")))
    
    ((name . bitnot) (category . bitwise) (signature . "(bitnot a)")
     (description . "Bitwise NOT (complement) of an integer.")
     (examples . ("(prim 'bitnot 0) → -1")))
    
    ((name . shl) (category . bitwise) (signature . "(shl a n)")
     (description . "Shift left by n bits.")
     (examples . ("(prim 'shl 1 3) → 8" "(prim 'shl 5 2) → 20")))
    
    ((name . shr) (category . bitwise) (signature . "(shr a n)")
     (description . "Shift right by n bits.")
     (examples . ("(prim 'shr 8 3) → 1" "(prim 'shr 20 2) → 5")))
    
    ;; ============================================================
    ;; Boolean Operations
    ;; ============================================================
    ((name . not) (category . boolean) (signature . "(not a)")
     (description . "Logical negation.")
     (examples . ("(prim 'not #t) → #f" "(prim 'not #f) → #t")))
    
    ((name . and) (category . boolean) (signature . "(and a b)")
     (description . "Logical AND.")
     (examples . ("(prim 'and #t #t) → #t" "(prim 'and #t #f) → #f")))
    
    ((name . or) (category . boolean) (signature . "(or a b)")
     (description . "Logical OR.")
     (examples . ("(prim 'or #t #f) → #t" "(prim 'or #f #f) → #f")))
    
    ;; ============================================================
    ;; Block Operations
    ;; ============================================================
    ((name . make-block) (category . block) (signature . "(make-block tag payload refs)")
     (description . "Create a content-addressed block.")
     (examples . ("(prim 'make-block 'data #vu8(1 2 3) #())")))
    
    ((name . block-tag) (category . block) (signature . "(block-tag block)")
     (description . "Get the tag of a block.")
     (examples . ("(prim 'block-tag b) → 'data")))
    
    ((name . block-payload) (category . block) (signature . "(block-payload block)")
     (description . "Get the payload bytevector of a block.")
     (examples . ("(prim 'block-payload b) → #vu8(1 2 3)")))
    
    ((name . block-refs) (category . block) (signature . "(block-refs block)")
     (description . "Get the references vector of a block.")
     (examples . ("(prim 'block-refs b) → #()")))
    
    ((name . block-ref) (category . block) (signature . "(block-ref block i)")
     (description . "Get the i-th reference from a block.")
     (examples . ("(prim 'block-ref b 0) → <hash>")))
    
    ((name . block->bytes) (category . block) (signature . "(block->bytes block)")
     (description . "Serialize a block to bytes.")
     (examples . ("(prim 'block->bytes b) → #vu8(...)")))
    
    ((name . bytes->block) (category . block) (signature . "(bytes->block bv)")
     (description . "Deserialize bytes to a block.")
     (examples . ("(prim 'bytes->block #vu8(...)) → <block>")))
    
    ;; ============================================================
    ;; Bytevector Operations
    ;; ============================================================
    ((name . bv-make) (category . bytevector) (signature . "(bv-make len [fill])")
     (description . "Create a bytevector of given length, optionally filled.")
     (examples . ("(prim 'bv-make 5) → #vu8(0 0 0 0 0)")))
    
    ((name . bv-length) (category . bytevector) (signature . "(bv-length bv)")
     (description . "Return the length of a bytevector.")
     (examples . ("(prim 'bv-length #vu8(1 2 3)) → 3")))
    
    ((name . bv-ref) (category . bytevector) (signature . "(bv-ref bv i)")
     (description . "Get byte at index i in bytevector.")
     (examples . ("(prim 'bv-ref #vu8(10 20 30) 1) → 20")))
    
    ((name . bv-concat) (category . bytevector) (signature . "(bv-concat bv ...)")
     (description . "Concatenate bytevectors. Variadic.")
     (examples . ("(prim 'bv-concat #vu8(1 2) #vu8(3 4)) → #vu8(1 2 3 4)")))
    
    ((name . bv-copy) (category . bytevector) (signature . "(bv-copy src src-start dest dest-start len)")
     (description . "Copy bytes from src to dest.")
     (examples . ("(prim 'bv-copy src 0 dest 0 5)")))
    
    ((name . bv-slice) (category . bytevector) (signature . "(bv-slice bv start end)")
     (description . "Extract a slice from a bytevector.")
     (examples . ("(prim 'bv-slice #vu8(1 2 3 4 5) 1 4) → #vu8(2 3 4)")))
    
    ;; ============================================================
    ;; String Operations
    ;; ============================================================
    ((name . string-length) (category . string) (signature . "(string-length s)")
     (description . "Return the length of a string.")
     (examples . ("(prim 'string-length \"hello\") → 5")))
    
    ((name . string-ref) (category . string) (signature . "(string-ref s i)")
     (description . "Return character at index i in string s.")
     (examples . ("(prim 'string-ref \"hello\" 0) → #\\h")))
    
    ((name . string-append) (category . string) (signature . "(string-append s ...)")
     (description . "Concatenate strings. Variadic.")
     (examples . ("(prim 'string-append \"hello\" \" \" \"world\") → \"hello world\"")))
    
    ((name . substring) (category . string) (signature . "(substring s start end)")
     (description . "Extract substring from start to end.")
     (examples . ("(prim 'substring \"hello\" 1 4) → \"ell\"")))
    
    ((name . string=?) (category . string) (signature . "(string=? a b)")
     (description . "Test if two strings are equal.")
     (examples . ("(prim 'string=? \"a\" \"a\") → #t")))
    
    ((name . string<?) (category . string) (signature . "(string<? a b)")
     (description . "Test if string a is lexicographically less than b.")
     (examples . ("(prim 'string<? \"a\" \"b\") → #t")))
    
    ((name . string>?) (category . string) (signature . "(string>? a b)")
     (description . "Test if string a is lexicographically greater than b.")
     (examples . ("(prim 'string>? \"b\" \"a\") → #t")))
    
    ((name . make-string) (category . string) (signature . "(make-string len [char])")
     (description . "Create a string of given length, optionally with fill char.")
     (examples . ("(prim 'make-string 5 #\\x) → \"xxxxx\"")))
    
    ((name . string->list) (category . string) (signature . "(string->list s)")
     (description . "Convert string to list of characters.")
     (examples . ("(prim 'string->list \"abc\") → (#\\a #\\b #\\c)")))
    
    ((name . list->string) (category . string) (signature . "(list->string chars)")
     (description . "Convert list of characters to string.")
     (examples . ("(prim 'list->string '(#\\a #\\b #\\c)) → \"abc\"")))
    
    ;; ============================================================
    ;; Character Operations
    ;; ============================================================
    ((name . char->integer) (category . character) (signature . "(char->integer c)")
     (description . "Get Unicode code point of character.")
     (examples . ("(prim 'char->integer #\\A) → 65")))
    
    ((name . integer->char) (category . character) (signature . "(integer->char n)")
     (description . "Get character from Unicode code point.")
     (examples . ("(prim 'integer->char 65) → #\\A")))
    
    ((name . char=?) (category . character) (signature . "(char=? a b)")
     (description . "Test if two characters are equal.")
     (examples . ("(prim 'char=? #\\a #\\a) → #t")))
    
    ((name . char<?) (category . character) (signature . "(char<? a b)")
     (description . "Test if character a is less than b.")
     (examples . ("(prim 'char<? #\\a #\\b) → #t")))
    
    ((name . char-alphabetic?) (category . character) (signature . "(char-alphabetic? c)")
     (description . "Test if character is alphabetic.")
     (examples . ("(prim 'char-alphabetic? #\\a) → #t")))
    
    ((name . char-numeric?) (category . character) (signature . "(char-numeric? c)")
     (description . "Test if character is a digit.")
     (examples . ("(prim 'char-numeric? #\\5) → #t")))
    
    ((name . char-whitespace?) (category . character) (signature . "(char-whitespace? c)")
     (description . "Test if character is whitespace.")
     (examples . ("(prim 'char-whitespace? #\\space) → #t")))
    
    ((name . char-upper-case?) (category . character) (signature . "(char-upper-case? c)")
     (description . "Test if character is uppercase.")
     (examples . ("(prim 'char-upper-case? #\\A) → #t")))
    
    ((name . char-lower-case?) (category . character) (signature . "(char-lower-case? c)")
     (description . "Test if character is lowercase.")
     (examples . ("(prim 'char-lower-case? #\\a) → #t")))
    
    ((name . char-upcase) (category . character) (signature . "(char-upcase c)")
     (description . "Convert character to uppercase.")
     (examples . ("(prim 'char-upcase #\\a) → #\\A")))
    
    ((name . char-downcase) (category . character) (signature . "(char-downcase c)")
     (description . "Convert character to lowercase.")
     (examples . ("(prim 'char-downcase #\\A) → #\\a")))
    
    ;; ============================================================
    ;; Conversion Operations
    ;; ============================================================
    ((name . string->utf8) (category . conversion) (signature . "(string->utf8 s)")
     (description . "Convert string to UTF-8 bytevector.")
     (examples . ("(prim 'string->utf8 \"hello\") → #vu8(104 101 108 108 111)")))
    
    ((name . utf8->string) (category . conversion) (signature . "(utf8->string bv)")
     (description . "Convert UTF-8 bytevector to string.")
     (examples . ("(prim 'utf8->string #vu8(104 105)) → \"hi\"")))
    
    ((name . symbol->string) (category . conversion) (signature . "(symbol->string sym)")
     (description . "Convert symbol to string.")
     (examples . ("(prim 'symbol->string 'hello) → \"hello\"")))
    
    ((name . string->symbol) (category . conversion) (signature . "(string->symbol s)")
     (description . "Convert string to symbol.")
     (examples . ("(prim 'string->symbol \"hello\") → 'hello")))
    
    ((name . number->string) (category . conversion) (signature . "(number->string n)")
     (description . "Convert number to string.")
     (examples . ("(prim 'number->string 42) → \"42\"")))
    
    ((name . string->number) (category . conversion) (signature . "(string->number s)")
     (description . "Parse string as number.")
     (examples . ("(prim 'string->number \"42\") → 42")))
    
    ;; ============================================================
    ;; List Operations
    ;; ============================================================
    ((name . cons) (category . list) (signature . "(cons a b)")
     (description . "Construct a pair (cons cell).")
     (examples . ("(prim 'cons 1 2) → (1 . 2)" "(prim 'cons 1 '()) → (1)")))
    
    ((name . car) (category . list) (signature . "(car pair)")
     (description . "Return the first element of a pair.")
     (examples . ("(prim 'car '(1 . 2)) → 1" "(prim 'car '(1 2 3)) → 1")))
    
    ((name . cdr) (category . list) (signature . "(cdr pair)")
     (description . "Return the rest of a pair.")
     (examples . ("(prim 'cdr '(1 . 2)) → 2" "(prim 'cdr '(1 2 3)) → (2 3)")))
    
    ((name . null?) (category . list) (signature . "(null? obj)")
     (description . "Test if object is the empty list.")
     (examples . ("(prim 'null? '()) → #t" "(prim 'null? '(1)) → #f")))
    
    ((name . pair?) (category . list) (signature . "(pair? obj)")
     (description . "Test if object is a pair.")
     (examples . ("(prim 'pair? '(1 . 2)) → #t" "(prim 'pair? '()) → #f")))
    
    ((name . list) (category . list) (signature . "(list ...)")
     (description . "Create a list of the given elements. Variadic.")
     (examples . ("(prim 'list 1 2 3) → (1 2 3)" "(prim 'list) → ()")))
    
    ((name . length) (category . list) (signature . "(length list)")
     (description . "Return the length of a list.")
     (examples . ("(prim 'length '(1 2 3)) → 3" "(prim 'length '()) → 0")))
    
    ((name . append) (category . list) (signature . "(append lst ...)")
     (description . "Concatenate lists. Variadic.")
     (examples . ("(prim 'append '(1 2) '(3 4)) → (1 2 3 4)")))
    
    ((name . reverse) (category . list) (signature . "(reverse lst)")
     (description . "Reverse a list.")
     (examples . ("(prim 'reverse '(1 2 3)) → (3 2 1)")))
    
    ((name . list-ref) (category . list) (signature . "(list-ref lst i)")
     (description . "Get element at index i in list.")
     (examples . ("(prim 'list-ref '(a b c) 1) → b")))
    
    ((name . memq) (category . list) (signature . "(memq obj lst)")
     (description . "Find obj in list using eq?, return tail or #f.")
     (examples . ("(prim 'memq 'b '(a b c)) → (b c)")))
    
    ((name . assq) (category . list) (signature . "(assq key alist)")
     (description . "Find pair with key in association list using eq?.")
     (examples . ("(prim 'assq 'b '((a . 1) (b . 2))) → (b . 2)")))
    
    ;; ============================================================
    ;; Vector Operations
    ;; ============================================================
    ((name . vec-make) (category . vector) (signature . "(vec-make ...)")
     (description . "Create a vector from arguments. Variadic.")
     (examples . ("(prim 'vec-make 1 2 3) → #(1 2 3)")))
    
    ((name . vec-ref) (category . vector) (signature . "(vec-ref vec i)")
     (description . "Get element at index i in vector.")
     (examples . ("(prim 'vec-ref #(a b c) 1) → b")))
    
    ((name . vec-length) (category . vector) (signature . "(vec-length vec)")
     (description . "Return the length of a vector.")
     (examples . ("(prim 'vec-length #(1 2 3)) → 3")))
    
    ((name . vec->list) (category . vector) (signature . "(vec->list vec)")
     (description . "Convert vector to list.")
     (examples . ("(prim 'vec->list #(1 2 3)) → (1 2 3)")))
    
    ((name . list->vec) (category . vector) (signature . "(list->vec lst)")
     (description . "Convert list to vector.")
     (examples . ("(prim 'list->vec '(1 2 3)) → #(1 2 3)")))
    
    ;; ============================================================
    ;; Type Predicates
    ;; ============================================================
    ((name . number?) (category . type) (signature . "(number? obj)")
     (description . "Test if object is a number.")
     (examples . ("(prim 'number? 42) → #t")))
    
    ((name . integer?) (category . type) (signature . "(integer? obj)")
     (description . "Test if object is an integer.")
     (examples . ("(prim 'integer? 42) → #t" "(prim 'integer? 3.14) → #f")))
    
    ((name . symbol?) (category . type) (signature . "(symbol? obj)")
     (description . "Test if object is a symbol.")
     (examples . ("(prim 'symbol? 'hello) → #t")))
    
    ((name . string?) (category . type) (signature . "(string? obj)")
     (description . "Test if object is a string.")
     (examples . ("(prim 'string? \"hello\") → #t")))
    
    ((name . char?) (category . type) (signature . "(char? obj)")
     (description . "Test if object is a character.")
     (examples . ("(prim 'char? #\\a) → #t")))
    
    ((name . bytevector?) (category . type) (signature . "(bytevector? obj)")
     (description . "Test if object is a bytevector.")
     (examples . ("(prim 'bytevector? #vu8(1 2 3)) → #t")))
    
    ((name . block?) (category . type) (signature . "(block? obj)")
     (description . "Test if object is a block.")
     (examples . ("(prim 'block? b) → #t")))
    
    ((name . vector?) (category . type) (signature . "(vector? obj)")
     (description . "Test if object is a vector.")
     (examples . ("(prim 'vector? #(1 2 3)) → #t")))
    
    ((name . list?) (category . type) (signature . "(list? obj)")
     (description . "Test if object is a proper list.")
     (examples . ("(prim 'list? '(1 2 3)) → #t")))
    
    ((name . boolean?) (category . type) (signature . "(boolean? obj)")
     (description . "Test if object is a boolean.")
     (examples . ("(prim 'boolean? #t) → #t")))
    
    ((name . procedure?) (category . type) (signature . "(procedure? obj)")
     (description . "Test if object is a procedure.")
     (examples . ("(prim 'procedure? +) → #t")))
    
    ;; ============================================================
    ;; Hash Operations
    ;; ============================================================
    ((name . sha256) (category . hash) (signature . "(sha256 bv)")
     (description . "Compute SHA-256 hash of bytevector.")
     (examples . ("(prim 'sha256 #vu8(104 105)) → #vu8(...)")))
    
    ((name . hash-block) (category . hash) (signature . "(hash-block block)")
     (description . "Compute content address (hash) of a block.")
     (examples . ("(prim 'hash-block b) → <32-byte-hash>")))
    
    ((name . hash->hex) (category . hash) (signature . "(hash->hex hash)")
     (description . "Convert hash to hexadecimal string.")
     (examples . ("(prim 'hash->hex h) → \"abc123...\"")))
    
    ((name . hex->hash) (category . hash) (signature . "(hex->hash s)")
     (description . "Parse hexadecimal string to hash.")
     (examples . ("(prim 'hex->hash \"abc123...\") → <hash>")))
    ))

;;; ============================================================
;;; Category Management
;;; ============================================================

(define *categories*
  '((arithmetic . "Arithmetic Operations")
    (comparison . "Comparison Operations")
    (bitwise . "Bitwise Operations")
    (boolean . "Boolean Operations")
    (block . "Block Operations")
    (bytevector . "Bytevector Operations")
    (string . "String Operations")
    (character . "Character Operations")
    (conversion . "Conversion Operations")
    (list . "List Operations")
    (vector . "Vector Operations")
    (type . "Type Predicates")
    (hash . "Hash Operations")))

;;; get-category-name : Symbol → String
(define (get-category-name cat)
  (let ([found (assq cat *categories*)])
       (if found
           (cdr found)
           "Uncategorized")))

;;; get-primitives-by-category : Symbol → (List Alist)
(define (get-primitives-by-category cat)
  (filter (lambda (doc)
                  (eq? (cdr (assq 'category doc)) cat))
          *primitive-docs*))

;;; ============================================================
;;; Main Help Functions
;;; ============================================================

;;; help : [Symbol] → void
;;; Show help for all primitives or a specific primitive.
(define (help . args)
  (if (null? args)
      ;; Show all primitives grouped by category
      (begin
       (display "
")
       (display "  ┌────────────────────────────────────────────────────────────────────┐
")
       (display "  │                    PRIMITIVE OPERATIONS                            │
")
       (display "  └────────────────────────────────────────────────────────────────────┘
")
       (display "
")
       (display "  Use (help 'primitive-name) for detailed help on a specific primitive.
")
       (display "  Use (apropos \"pattern\") to search for primitives by name.
")
       (display "
")
       
       (for-each
        (lambda (cat-entry)
                (let ([cat (car cat-entry)]
                      [cat-name (cdr cat-entry)])
                     (let ([prims (get-primitives-by-category cat)])
                          (when (not (null? prims))
                                (display (format "  ~a:
" cat-name))
                                (for-each
                                 (lambda (doc)
                                         (let ([name (cdr (assq 'name doc))]
                                               [sig (cdr (assq 'signature doc))]
                                               [desc (cdr (assq 'description doc))])
                                              (display (format "    ~a~a~a
"
                                                               name
                                                               (make-string (max 1 (- 15 (string-length (symbol->string name)))) #\space)
                                                               desc))))
                                 prims)
                                (display "
")))))
        *categories*)
       
       (display "  Total primitives: ")
       (display (length *primitive-docs*))
       (display "
")
       (display "
"))
      
      ;; Show help for specific primitive
      (let* ([prim-name (car args)]
             [doc (find-primitive-doc prim-name)])
            (if doc
                (display-primitive-help doc)
                (begin
                 (display (format "Unknown primitive: ~a
" prim-name))
                 (let ([suggestion (suggest-primitive prim-name)])
                      (when suggestion
                            (display (format "Did you mean: ~a?
" suggestion)))))))))

;;; apropos : String → void
;;; Search for primitives whose names contain the given pattern.
(define (apropos pattern)
  (let* ([pattern-str (string-downcase pattern)]
         [matches (filter
                   (lambda (doc)
                           (let ([name-str (symbol->string (cdr (assq 'name doc)))])
                                (string-contains? (string-downcase name-str) pattern-str)))
                   *primitive-docs*)])
        (display "
")
        (display (format "  Primitives matching '~a':

" pattern))
        
        (if (null? matches)
            (display "  (no matches found)
")
            (for-each
             (lambda (doc)
                     (let ([name (cdr (assq 'name doc))]
                           [sig (cdr (assq 'signature doc))]
                           [desc (cdr (assq 'description doc))])
                          (display (format "    ~a~a~a
"
                                           name
                                           (make-string (max 1 (- 15 (string-length (symbol->string name)))) #\space)
                                           desc))))
             matches))
        
        (display "
")
        (display (format "  Found ~a matches.
" (length matches)))
        (display "
")))

;;; help-categories : → void
;;; List all available categories.
(define (help-categories)
  (display "
")
  (display "  Available help categories:
")
  (display "
")
  (for-each
   (lambda (cat-entry)
           (display (format "    ~a - ~a
"
                            (car cat-entry)
                            (cdr cat-entry))))
   *categories*)
  (display "
")
  (display "  Use (help-by-category 'category) to list primitives in a category.
")
  (display "
"))

;;; help-by-category : Symbol → void
;;; List all primitives in a specific category.
(define (help-by-category cat)
  (let ([prims (get-primitives-by-category cat)]
        [cat-name (get-category-name cat)])
       (display "
")
       (display (format "  ~a:

" cat-name))
       
       (if (null? prims)
           (display "  (no primitives in this category)
")
           (for-each
            (lambda (doc)
                    (display-primitive-summary doc))
            prims))
       
       (display "
")
       (display (format "  ~a primitives in category.
" (length prims)))
       (display "
")))

;;; ============================================================
;;; Helper Functions
;;; ============================================================

;;; find-primitive-doc : Symbol → Alist | #f
(define (find-primitive-doc name)
  (find (lambda (doc)
                (eq? (cdr (assq 'name doc)) name))
        *primitive-docs*))

;;; display-primitive-help : Alist → void
(define (display-primitive-help doc)
  (let ([name (cdr (assq 'name doc))]
        [sig (cdr (assq 'signature doc))]
        [desc (cdr (assq 'description doc))]
        [examples (cdr (assq 'examples doc))]
        [category (cdr (assq 'category doc))])
       
       (display "
")
       (display (format "  Primitive: ~a
" name))
       (display "  ────────────────────────────────────────────────────────────
")
       (display "
")
       (display (format "  Category: ~a
" (get-category-name category)))
       (display (format "  Signature: ~a
" sig))
       (display "
")
       (display (format "  ~a

" desc))
       
       (when (not (null? examples))
             (display "  Examples:
")
             (for-each
              (lambda (example)
                      (display (format "    ~a
" example)))
              examples)
             (display "
"))
       
       ;; Add fuel cost and arity info if available
       (let ([fuel-cost (prim-fuel-cost name)]
             [arity (prim-arity name)])
            (when fuel-cost
                  (display (format "  Fuel cost: ~a
" fuel-cost)))
            (when arity
                  (display (format "  Arity: ~a
"
                                   (if (eq? arity 'variadic) "variadic" arity)))))
       
       (display "
")))

;;; display-primitive-summary : Alist → void
(define (display-primitive-summary doc)
  (let ([name (cdr (assq 'name doc))]
        [sig (cdr (assq 'signature doc))]
        [desc (cdr (assq 'description doc))])
       (display (format "    ~a~a~a
"
                        name
                        (make-string (max 1 (- 15 (string-length (symbol->string name)))) #\space)
                        desc))))

;;; suggest-primitive : Symbol → Symbol | #f
;;; Suggest a similar primitive name for typos.
(define (suggest-primitive name)
  (let* ([name-str (symbol->string name)]
         [all-names (map (lambda (doc) (cdr (assq 'name doc))) *primitive-docs*)]
         [candidates
          (filter
           (lambda (candidate)
                   (<= (edit-distance name-str (symbol->string candidate)) 2))
           all-names)])
        (if (null? candidates)
            #f
            (car candidates))))

;;; NOTE: string-downcase and string-contains? are provided by prelude.ss
;;; (loaded transitively via prim.ss)

;;; edit-distance : String × String → Nat
;;; Simple Levenshtein distance for typo suggestions.
(define (edit-distance s1 s2)
  (let ([len1 (string-length s1)]
        [len2 (string-length s2)])
       (if (zero? len1)
           len2
           (if (zero? len2)
               len1
               (let ([matrix (make-vector (* (+ len1 1) (+ len2 1)) 0)])
                    ;; Initialize
                    (do ([i 0 (+ i 1)])
                        ((> i len1))
                        (vector-set! matrix (+ (* i (+ len2 1)) 0) i))
                    (do ([j 0 (+ j 1)])
                        ((> j len2))
                        (vector-set! matrix j j))
                    ;; Fill matrix
                    (do ([i 1 (+ i 1)])
                        ((> i len1))
                        (do ([j 1 (+ j 1)])
                            ((> j len2))
                            (let* ([cost (if (char=? (string-ref s1 (- i 1))
                                                     (string-ref s2 (- j 1)))
                                             0
                                             1)]
                                   [above (vector-ref matrix (+ (* (- i 1) (+ len2 1)) j))]
                                   [left (vector-ref matrix (+ (* i (+ len2 1)) (- j 1)))]
                                   [diag (vector-ref matrix (+ (* (- i 1) (+ len2 1)) (- j 1)))]
                                   [min-val (min (+ above 1)
                                                 (+ left 1)
                                                 (+ diag cost))])
                                  (vector-set! matrix (+ (* i (+ len2 1)) j) min-val))))
                    (vector-ref matrix (+ (* len1 (+ len2 1)) len2)))))))

;;; find : Procedure × (List Any) → Any | #f
(define (find pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) (car lst)]
   [else (find pred (cdr lst))]))

;;; ============================================================
;;; Initialization
;;; ============================================================

(display "
")
(display "Primitive help system loaded.
")
(display "Commands: (help), (help 'primitive), (apropos \"pattern\")
")
(display "          (help-categories), (help-by-category 'category)
")
(display "
")
