(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module parser-compile
;;; @requires prelude parser fsm regex
(require 'prelude)
(require 'parser)
(require 'fsm)
(require 'regex)

(doc 'module 'parser-compile)
(doc 'description "Bridge between parser combinators and compiled DFA recognizers.

Enables O(n) recognition for regular patterns within parser combinator pipelines.
Three compilation paths:
  (1) dfa->parser:       DFA/NFA → Parser String (O(n) prefix matching)
  (2) regex-ast->parser:  RegexAST → Parser String (combinator translation)
  (3) compiled-regex:     Dual form carrying both DFA and parser

The DFA path runs the automaton directly on the input string, avoiding per-character
function call overhead. The combinator path preserves parser composability.
Use compiled-regex for patterns that need both fast matching and parser integration.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Position Advancement Helper
;;; ============================================================

(doc 'section 'helpers)

(doc 'type '(-> Pos String Nat Nat Pos))
(doc 'description "Advance parser position through characters in input[from..to).")
(define (advance-pos-range pos input from to)
  (let loop ([i from] [p pos])
    (if (>= i to) p
        (loop (+ i 1) (advance-pos p (string-ref input i))))))

;;; ============================================================
;;; DFA-Backed Parser
;;; ============================================================

(doc 'section 'dfa-parser)

(doc 'type '(-> FSM String (Parser String)))
(doc 'description "Wrap a DFA (or NFA) as a parser combinator that matches the longest
accepting prefix and returns the matched text. Runs in O(n) for DFAs.

If the FSM is non-deterministic, it is first converted to a DFA.
desc is used in error messages on parse failure.

The resulting parser is composable with all standard parser combinators:
  (parser-bind (dfa->parser email-dfa \"email\") (lambda (addr) ...))
  (parser-then (dfa->parser ident-dfa \"identifier\") (parser-char #\\=))")
(define (dfa->parser fsm desc)
  (doc 'export #t)
  (let ([automaton (if (fsm-deterministic? fsm) fsm (fsm-minimize (nfa->dfa fsm)))])
    (make-parser
     (lambda (state)
       (let* ([input (parser-state-input state)]
              [start (parser-state-index state)]
              [len (string-length input)]
              [init (fsm-start automaton)]
              [acc (fsm-accepting automaton)])
         ;; Run DFA from current position, track longest accepting prefix
         (let loop ([idx start]
                    [current init]
                    [last-accept-idx
                     (if (pair? (memq init acc)) start #f)])
           (if (>= idx len)
               ;; End of input
               (if last-accept-idx
                   (let* ([matched (substring input start last-accept-idx)]
                          [new-pos (advance-pos-range
                                    (parser-state-pos state) input start last-accept-idx)])
                     (right (cons matched (parser-make-state input last-accept-idx new-pos))))
                   (left (make-parse-error
                          (parser-state-pos state)
                          (string-append "expected " desc)
                          (list desc))))
               ;; Try next character
               (let ([next-states (fsm-delta automaton current (string-ref input idx))])
                 (if (null? next-states)
                     ;; Dead — report
                     (if last-accept-idx
                         (let* ([matched (substring input start last-accept-idx)]
                                [new-pos (advance-pos-range
                                          (parser-state-pos state) input start last-accept-idx)])
                           (right (cons matched (parser-make-state input last-accept-idx new-pos))))
                         (left (make-parse-error
                                (parser-state-pos state)
                                (string-append "expected " desc)
                                (list desc))))
                     ;; Advance
                     (let ([next (car next-states)])
                       (loop (+ idx 1)
                             next
                             (if (pair? (memq next acc))
                                 (+ idx 1)
                                 last-accept-idx))))))))))))

;;; ============================================================
;;; Regex AST → Parser Combinator Translation
;;; ============================================================

(doc 'section 'ast-to-parser)

(doc 'type '(-> RegexAST (Parser String)))
(doc 'description "Compile a regex AST directly to parser combinators.
Returns a parser that matches the pattern and returns the matched text.

Unlike the DFA path, this preserves parser combinator semantics:
- Alternation tries alternatives in order (not longest match)
- Uses parser-try for backtracking on alternation branches
- Supports the full regex AST including groups and repeats

Best for embedding regex patterns in larger parser pipelines where
you need combinator-level control over matching semantics.")
(define (regex-ast->parser ast)
  (doc 'export #t)
  (cond
   [(regex-empty? ast)
    (parser-pure "")]

   [(regex-lit? ast)
    (parser-map string (parser-char (regex-lit-char ast)))]

   [(regex-dot? ast)
    (parser-map string parser-any-char)]

   [(regex-class? ast)
    (let ([chars (regex-class-chars ast)]
          [negated? (regex-class-negated? ast)])
      (parser-map string
        (parser-satisfy
         (if negated?
             (lambda (c) (not (pair? (memv c chars))))
             (lambda (c) (pair? (memv c chars))))
         "character class")))]

   [(regex-seq? ast)
    (let ([parsers (map regex-ast->parser (regex-seq-exprs ast))])
      (fold-left (lambda (acc p)
                   (parser-bind acc
                     (lambda (s1)
                       (parser-map (lambda (s2) (string-append s1 s2)) p))))
                 (parser-pure "")
                 parsers))]

   [(regex-alt? ast)
    (let ([parsers (map (lambda (e) (parser-try (regex-ast->parser e)))
                        (regex-alt-exprs ast))])
      (parser-choice parsers))]

   [(regex-star? ast)
    (let ([inner (regex-ast->parser (regex-star-expr ast))])
      (parser-map (lambda (parts) (apply string-append parts))
                  (parser-many inner)))]

   [(regex-plus? ast)
    (let ([inner (regex-ast->parser (regex-plus-expr ast))])
      (parser-map (lambda (parts) (apply string-append parts))
                  (parser-some inner)))]

   [(regex-opt? ast)
    (let ([inner (regex-ast->parser (regex-opt-expr ast))])
      (parser-optional inner ""))]

   [(regex-group? ast)
    (regex-ast->parser (regex-group-expr ast))]

   [(regex-repeat? ast)
    (compile-repeat-to-parser
     (regex-ast->parser (regex-repeat-expr ast))
     (regex-repeat-min ast)
     (regex-repeat-max ast))]

   [else
    (parser-fail "unsupported regex AST node")]))

(doc 'type '(-> (Parser String) Nat (Option Nat) (Parser String)))
(doc 'description "Compile a repeat quantifier {min,max} to parser combinators.")
(define (compile-repeat-to-parser inner min max)
  (cond
   ;; {0,0} → empty
   [(and (= min 0) (eqv? max 0))
    (parser-pure "")]
   ;; {0,} → star
   [(and (= min 0) (not max))
    (parser-map (lambda (parts) (apply string-append parts))
                (parser-many inner))]
   ;; {n} or {n,n} → exactly n
   [(eqv? min max)
    (parser-map (lambda (parts) (apply string-append parts))
                (parser-count min inner))]
   ;; {n,} → at least n
   [(not max)
    (parser-map (lambda (parts) (apply string-append parts))
                (at-least min inner))]
   ;; {n,m} → between n and m
   [else
    (parser-map (lambda (parts) (apply string-append parts))
                (range-of min max inner))]))

;;; ============================================================
;;; Convenience: Regex String → Parser
;;; ============================================================

(doc 'section 'convenience)

(doc 'type '(-> String (Parser String)))
(doc 'description "Compile a regex pattern string to a DFA-backed parser.
Returns a parser that matches the longest prefix matching the pattern
and returns the matched text. O(n) matching.

  (parser-parse (regex->parser \"[0-9]+\") \"42abc\")
  → (right \"42\")")
(define (regex->parser pattern)
  (doc 'export #t)
  (dfa->parser (regex->dfa pattern) pattern))

(doc 'type '(-> String (Parser String)))
(doc 'description "Compile a regex pattern to a parser via combinator translation.
Unlike regex->parser (DFA path), this uses parser combinators internally,
enabling composition with parser-or, parser-bind, etc. without DFA limitations.

Alternation uses ordered-choice semantics (not longest match).")
(define (regex->combinator-parser pattern)
  (doc 'export #t)
  (let ([result (regex-parse pattern)])
    (if (left? result)
        (parser-fail (string-append "invalid regex: " (error-message (from-left result))))
        (regex-ast->parser (from-right result)))))

;;; ============================================================
;;; Compiled Regex (Dual Form)
;;; ============================================================

(doc 'section 'compiled-regex)

(doc 'type '(List 'compiled-regex String RegexAST FSM (Parser String)))
(doc 'description "A compiled regex carrying the original pattern, AST, minimized DFA,
and a DFA-backed parser. Supports both O(n) boolean matching (via DFA) and
value-producing parsing (via parser) from the same pattern.")
(define (make-compiled-regex pattern ast dfa parser)
  (list 'compiled-regex pattern ast dfa parser))

(define (compiled-regex? x)
  (doc 'export #t)
  (and (pair? x) (eq? (car x) 'compiled-regex)))

(define (compiled-regex-pattern crx)
  (doc 'export #t)
  (doc 'type '(-> CompiledRegex String))
  (list-ref crx 1))

(define (compiled-regex-ast crx)
  (doc 'export #t)
  (doc 'type '(-> CompiledRegex RegexAST))
  (list-ref crx 2))

(define (compiled-regex-dfa crx)
  (doc 'export #t)
  (doc 'type '(-> CompiledRegex FSM))
  (list-ref crx 3))

(define (compiled-regex-parser crx)
  (doc 'export #t)
  (doc 'type '(-> CompiledRegex (Parser String)))
  (list-ref crx 4))

(doc 'type '(-> String CompiledRegex))
(doc 'description "Compile a regex pattern into a dual-form compiled regex.
Pre-computes the minimized DFA and DFA-backed parser.

  (define email-rx (compile-regex \"[a-z]+@[a-z]+\\\\.[a-z]+\"))
  (compiled-regex-matches? email-rx \"foo@bar.com\")  → #t
  (parser-parse (compiled-regex-parser email-rx) \"foo@bar.com rest\")  → (right \"foo@bar.com\")")
(define (compile-regex pattern)
  (doc 'export #t)
  (let* ([parse-result (regex-parse pattern)]
         [ast (if (right? parse-result)
                  (from-right parse-result)
                  (error 'compile-regex
                    (string-append "invalid regex: "
                      (error-message (from-left parse-result)))))]
         [nfa (regex-compile ast *default-universe*)]
         [the-dfa (fsm-minimize nfa)]
         [the-parser (dfa->parser the-dfa pattern)])
    (make-compiled-regex pattern ast the-dfa the-parser)))

(doc 'type '(-> CompiledRegex String Boolean))
(doc 'description "O(n) boolean matching using the pre-compiled DFA.")
(define (compiled-regex-matches? crx input)
  (doc 'export #t)
  (fsm-accepts? (compiled-regex-dfa crx) input))

(doc 'type '(-> CompiledRegex String (Either Error String)))
(doc 'description "Parse input using the compiled regex's DFA-backed parser.
Returns matched prefix text.")
(define (compiled-regex-parse crx input)
  (doc 'export #t)
  (parser-parse (compiled-regex-parser crx) input))
