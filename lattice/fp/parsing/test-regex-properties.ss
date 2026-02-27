;;; lattice/fp/parsing/test-regex-properties.ss — QuickCheck properties for Regex
;;;
;;; Tests regex compilation and matching properties:
;;; - Parsing roundtrip (AST <-> string where applicable)
;;; - Compilation correctness (regex->nfa accepts expected strings)
;;; - Equivalence properties (alternative constructions match same language)
;;; - DFA determinism (regex->dfa produces valid DFA)

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'regex)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

;; every: check if predicate holds for all elements
(define (every pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-char-lower
  (gen-map (lambda (n) (integer->char (+ 97 n))) (gen-int-range 0 25)))

(define gen-char-digit
  (gen-map (lambda (n) (integer->char (+ 48 n))) (gen-int-range 0 9)))

(define gen-char-alphanum
  (gen-one-of (list gen-char-lower gen-char-digit)))

(define gen-literal-regex
  (gen-map (lambda (c) (string-append (string c) "")) gen-char-lower))

(define gen-simple-regex-pattern
  (gen-one-of
   (list gen-literal-regex
         (gen-pure ".")
         (gen-pure "a*")
         (gen-pure "a+")
         (gen-pure "a?")
         (gen-pure "ab")
         (gen-pure "a|b")
         (gen-pure "[abc]")
         (gen-pure "(ab)"))))

;;; ============================================================================
;;; AST Properties
;;; ============================================================================

(test-group regex-ast-properties

  ;; Literal AST roundtrip
  (define-property "regex-lit constructor preserves char"
    gen-char-lower
    (lambda (c)
      (let ([ast (regex-lit c)])
        (and (regex-lit? ast)
             (char=? c (regex-lit-char ast)))))
    'tests 50)

  ;; Dot AST is recognized
  (define-property "regex-dot creates dot node"
    (gen-pure #t)
    (lambda (_)
      (regex-dot? (regex-dot)))
    'tests 10)

  ;; Class AST preserves character list
  (define-property "regex-class preserves chars and negation flag"
    (gen-bind (gen-int-range 1 5)
      (lambda (n)
        (gen-map list->string (gen-list-of n gen-char-lower))))
    (lambda (chars)
      (let* ([char-list (string->list chars)]
             [ast-pos (regex-class char-list #f)]
             [ast-neg (regex-class char-list #t)])
        (and (regex-class? ast-pos)
             (not (regex-class-negated? ast-pos))
             (equal? char-list (regex-class-chars ast-pos))
             (regex-class-negated? ast-neg))))
    'tests 30)

  ;; Sequence AST preserves expressions
  (define-property "regex-seq preserves expression list"
    (gen-bind (gen-int-range 2 4)
      (lambda (n)
        (gen-list-of n gen-char-lower)))
    (lambda (chars)
      (let* ([exprs (map regex-lit chars)]
             [ast (regex-seq exprs)])
        (and (regex-seq? ast)
             (= (length exprs) (length (regex-seq-exprs ast))))))
    'tests 30)

  ;; Star AST preserves inner expression
  (define-property "regex-star preserves inner expression"
    gen-char-lower
    (lambda (c)
      (let* ([inner (regex-lit c)]
             [ast (regex-star inner)])
        (and (regex-star? ast)
             (regex-lit? (regex-star-expr ast))
             (char=? c (regex-lit-char (regex-star-expr ast))))))
    'tests 50)

  ;; Plus AST preserves inner expression  
  (define-property "regex-plus preserves inner expression"
    gen-char-lower
    (lambda (c)
      (let* ([inner (regex-lit c)]
             [ast (regex-plus inner)])
        (and (regex-plus? ast)
             (regex-lit? (regex-plus-expr ast)))))
    'tests 50)

  ;; Optional AST preserves inner expression
  (define-property "regex-opt preserves inner expression"
    gen-char-lower
    (lambda (c)
      (let* ([inner (regex-lit c)]
             [ast (regex-opt inner)])
        (and (regex-opt? ast)
             (regex-lit? (regex-opt-expr ast)))))
    'tests 50)

  ;; Empty AST is recognized
  (define-property "regex-empty creates empty node"
    (gen-pure #t)
    (lambda (_)
      (regex-empty? (regex-empty)))
    'tests 10)
)

;;; ============================================================================
;;; Parsing Properties
;;; ============================================================================

(test-group regex-parsing-properties

  ;; Literal parsing succeeds
  (define-property "parse single literal succeeds"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string c))])
        (right? result)))
    'tests 50)

  ;; Parse of literal gives lit node
  (define-property "parse literal gives lit node"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string c))])
        (and (right? result)
             (regex-lit? (from-right result)))))
    'tests 50)

  ;; Dot parsing
  (define-property "parse dot succeeds"
    (gen-pure #t)
    (lambda (_)
      (let ([result (regex-parse ".")])
        (and (right? result)
             (regex-dot? (from-right result)))))
    'tests 10)

  ;; Star parsing
  (define-property "parse star succeeds"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string-append (string c) "*"))])
        (and (right? result)
             (regex-star? (from-right result)))))
    'tests 50)

  ;; Plus parsing  
  (define-property "parse plus succeeds"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string-append (string c) "+"))])
        (and (right? result)
             (regex-plus? (from-right result)))))
    'tests 50)

  ;; Optional parsing
  (define-property "parse optional succeeds"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string-append (string c) "?"))])
        (and (right? result)
             (regex-opt? (from-right result)))))
    'tests 50)

  ;; Alternation parsing
  (define-property "parse alternation succeeds"
    (gen-bind gen-char-lower
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lower)))
    (lambda (cs)
      (let ([result (regex-parse (string-append (string (car cs)) "|" (string (cdr cs))))])
        (and (right? result)
             (regex-alt? (from-right result)))))
    'tests 50)

  ;; Concatenation parsing
  (define-property "parse concatenation succeeds"
    (gen-bind gen-char-lower
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lower)))
    (lambda (cs)
      (let ([result (regex-parse (string-append (string (car cs)) (string (cdr cs))))])
        (and (right? result)
             (regex-seq? (from-right result)))))
    'tests 50)

  ;; Character class parsing
  (define-property "parse character class succeeds"
    (gen-bind (gen-int-range 2 4)
      (lambda (n)
        (gen-map list->string (gen-list-of n gen-char-lower))))
    (lambda (chars)
      (let ([result (regex-parse (string-append "[" chars "]"))])
        (and (right? result)
             (regex-class? (from-right result))
             (not (regex-class-negated? (from-right result))))))
    'tests 30)

  ;; Negated character class parsing
  (define-property "parse negated character class succeeds"
    (gen-bind (gen-int-range 2 4)
      (lambda (n)
        (gen-map list->string (gen-list-of n gen-char-lower))))
    (lambda (chars)
      (let ([result (regex-parse (string-append "[^" chars "]"))])
        (and (right? result)
             (regex-class? (from-right result))
             (regex-class-negated? (from-right result)))))
    'tests 30)

  ;; Group parsing
  (define-property "parse group succeeds"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string-append "(" (string c) ")"))])
        (and (right? result)
             (regex-group? (from-right result)))))
    'tests 50)

  ;; Unclosed group fails
  (define-property "unclosed group parse fails"
    gen-char-lower
    (lambda (c)
      (let ([result (regex-parse (string-append "(" (string c)))])
        (left? result)))
    'tests 20)

  ;; Unclosed class fails
  (define-property "unclosed class parse fails"
    (gen-pure #t)
    (lambda (_)
      (let ([result (regex-parse "[abc")])
        (left? result)))
    'tests 10)
)

;;; ============================================================================
;;; Compilation and Matching Properties
;;; ============================================================================

(test-group regex-compilation-properties

  ;; Literal regex matches literal string
  (define-property "literal regex matches literal char"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string c))])
        (and (fsm? nfa)
             (fsm-accepts? nfa (string c))
             (not (fsm-accepts? nfa (string (integer->char (+ 1 (char->integer c))))))))) ;; char->string conversion
    'tests 50)

  ;; Star matches empty string
  (define-property "star regex matches empty string"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "*"))])
        (fsm-accepts? nfa "")))
    'tests 30)

  ;; Star matches one copy
  (define-property "star regex matches single char"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "*"))])
        (fsm-accepts? nfa (string c))))
    'tests 30)

  ;; Star matches multiple copies
  (define-property "star regex matches multiple copies"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "*"))])
        (and (fsm-accepts? nfa (make-string 5 c))
             (fsm-accepts? nfa (make-string 10 c)))))
    'tests 30)

  ;; Plus does not match empty
  (define-property "plus regex does not match empty"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "+"))])
        (not (fsm-accepts? nfa ""))))
    'tests 30)

  ;; Plus matches one or more
  (define-property "plus regex matches one or more"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "+"))])
        (and (fsm-accepts? nfa (string c))
             (fsm-accepts? nfa (make-string 5 c)))))
    'tests 30)

  ;; Optional matches empty
  (define-property "optional regex matches empty"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "?"))])
        (fsm-accepts? nfa "")))
    'tests 30)

  ;; Optional matches one
  (define-property "optional regex matches single char"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "?"))])
        (fsm-accepts? nfa (string c))))
    'tests 30)

  ;; Optional does not match two
  (define-property "optional regex does not match two"
    gen-char-lower
    (lambda (c)
      (let ([nfa (regex->nfa (string-append (string c) "?"))])
        (not (fsm-accepts? nfa (make-string 2 c)))))
    'tests 30)

  ;; Alternation matches either alternative
  (define-property "alternation matches either alternative"
    (gen-bind gen-char-lower
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lower)))
    (lambda (cs)
      (let ([nfa (regex->nfa (string-append (string (car cs)) "|" (string (cdr cs))))])
        (and (fsm-accepts? nfa (string (car cs)))
             (fsm-accepts? nfa (string (cdr cs))))))
    'tests 50)

  ;; Character class matches members
  (define-property "character class matches its members"
    (gen-bind (gen-int-range 2 4)
      (lambda (n)
        (gen-map list->string (gen-list-of n gen-char-lower))))
    (lambda (chars)
      (let ([nfa (regex->nfa (string-append "[" chars "]"))])
        (every (lambda (c) (fsm-accepts? nfa (string c)))
               (string->list chars))))
    'tests 30)

  ;; Concatenation matches concatenated strings
  (define-property "concatenation matches concatenated strings"
    (gen-bind gen-char-lower
      (lambda (c1)
        (gen-map (lambda (c2) (cons c1 c2)) gen-char-lower)))
    (lambda (cs)
      (let ([nfa (regex->nfa (string-append (string (car cs)) (string (cdr cs))))])
        (fsm-accepts? nfa (string-append (string (car cs)) (string (cdr cs))))))
    'tests 50)

  ;; Empty regex matches only empty string
  (define-property "empty regex matches only empty string"
    (gen-pure #t)
    (lambda (_)
      (let ([nfa (regex->nfa "")])
        (and (fsm-accepts? nfa "")
             (not (fsm-accepts? nfa "a")))))
    'tests 10)
)

;;; ============================================================================
;;; DFA Properties
;;; ============================================================================

(test-group regex-dfa-properties

  ;; DFA is deterministic
  (define-property "regex->dfa produces deterministic FSM"
    gen-simple-regex-pattern
    (lambda (pattern)
      (let ([dfa (regex->dfa pattern)])
        (fsm-deterministic? dfa)))
    'tests 30)

  ;; DFA and NFA accept same language for simple cases
  (define-property "dfa and nfa accept same strings"
    (gen-one-of
     (list (gen-pure "a")
           (gen-pure "ab")
           (gen-pure "a*")
           (gen-pure "a|b")))
    (lambda (pattern)
      (let* ([nfa (regex->nfa pattern)]
             [dfa (regex->dfa pattern)]
             [test-strings (list "" "a" "b" "aa" "ab")])
        (every (lambda (s)
                 (eq? (fsm-accepts? nfa s)
                      (fsm-accepts? dfa s)))
               test-strings)))
    'tests 20)
)

;;; ============================================================================
;;; Equivalence Properties
;;; ============================================================================

(test-group regex-equivalence-properties

  ;; a+ is equivalent to aa*
  (define-property "plus equals concat of literal and star"
    (gen-pure #t)
    (lambda (_)
      (let* ([nfa-plus (regex->nfa "a+")]
             [nfa-equiv (regex->nfa "aa*")]
             [test-strings (list "" "a" "aa" "aaa" "b" "ab")])
        (every (lambda (s)
                 (eq? (fsm-accepts? nfa-plus s)
                      (fsm-accepts? nfa-equiv s)))
               test-strings)))
    'tests 10)

  ;; a? is equivalent to (a|)
  (define-property "optional equals alternation with empty"
    (gen-pure #t)
    (lambda (_)
      (let* ([nfa-opt (regex->nfa "a?")]
             [nfa-equiv (regex->nfa "(a|)")]
             [test-strings (list "" "a" "aa" "b")])
        (every (lambda (s)
                 (eq? (fsm-accepts? nfa-opt s)
                      (fsm-accepts? nfa-equiv s)))
               test-strings)))
    'tests 10)

  ;; (a*)* is equivalent to a*
  (define-property "star of star equals star"
    (gen-pure #t)
    (lambda (_)
      (let* ([nfa-double (regex->nfa "(a*)*")]
             [nfa-single (regex->nfa "a*")]
             [test-strings (list "" "a" "aa" "aaa" "b")])
        (every (lambda (s)
                 (eq? (fsm-accepts? nfa-double s)
                      (fsm-accepts? nfa-single s)))
               test-strings)))
    'tests 10)

  ;; Character class [abc] is equivalent to a|b|c
  (define-property "char class equals alternation of literals"
    (gen-pure #t)
    (lambda (_)
      (let* ([nfa-class (regex->nfa "[abc]")]
             [nfa-alt (regex->nfa "a|b|c")]
             [test-strings (list "" "a" "b" "c" "d" "ab")])
        (every (lambda (s)
                 (eq? (fsm-accepts? nfa-class s)
                      (fsm-accepts? nfa-alt s)))
               test-strings)))
    'tests 10)
)

;;; ============================================================================
;;; Convenience Function Properties
;;; ============================================================================

(test-group regex-convenience-properties

  ;; regex-accepts? is consistent with regex->nfa + fsm-accepts?
  (define-property "regex-accepts? matches manual compile-and-test"
    gen-simple-regex-pattern
    (lambda (pattern)
      (let ([nfa (regex->nfa pattern)])
        ;; Test on a few strings
        (and (eq? (regex-accepts? pattern "a")
                  (fsm-accepts? nfa "a"))
             (eq? (regex-accepts? pattern "")
                  (fsm-accepts? nfa ""))
             (eq? (regex-accepts? pattern "ab")
                  (fsm-accepts? nfa "ab")))))
    'tests 20)

  ;; regex-accepts? returns boolean
  (define-property "regex-accepts? returns boolean"
    gen-simple-regex-pattern
    (lambda (pattern)
      (let ([result (regex-accepts? pattern "test")])
        (or (eq? result #t) (eq? result #f))))
    'tests 20)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
