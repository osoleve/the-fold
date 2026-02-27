;;; lattice/fp/parsing/regex.ss — Regular Expression to NFA Compilation
;;;
;;; Thompson's construction for compiling regular expressions to NFAs.
;;; Integrates with the existing FSM library (fsm.ss).
;;;
;;; Supported syntax:
;;;   Literals:      a, b, c (including Unicode)
;;;   Concatenation: ab
;;;   Alternation:   a|b|c
;;;   Kleene star:   a*
;;;   Plus:          a+
;;;   Optional:      a?
;;;   Grouping:      (ab)
;;;   Char classes:  [abc], [a-z], [^abc]
;;;   Dot (any):     .
;;;   Escapes:       \*, \+, \?, \., \[, \], \(, \), \|, \\, \n, \t, \r, \{, \}
;;;   Quantifiers:   a{3}, a{2,4}, a{2,}, a{,4}
;;;   Anchors:       ^ (start), $ (end)
;;;   Lookahead:     (?=...) positive, (?!...) negative
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/fp/parsing/parser.ss
;;;   - lattice/fp/parsing/fsm.ss

;;; @module regex
;;; @requires prelude parser fsm

(require 'prelude)
(require 'parser)
(require 'fsm)

(doc 'module 'regex)
(doc 'description "Regular expression compilation via Thompson's construction")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; Local list predicate — avoids dependence on R6RS for-all which can be
;;; shadowed by quickcheck's for-all in the interaction environment.
(define (every pred lst)
  (or (null? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

;;; ============================================================
;;; Section 1: Character Universe
;;; ============================================================

(doc 'section 'character-universe)

;;; Default universe: printable ASCII + common whitespace
(define *default-universe*
  (append
   (list #\newline #\tab #\return #\space)
   ;; Printable ASCII: 33-126
   (map (lambda (i) (integer->char (+ i 33))) (iota 94))))

(doc *default-universe* 'type '(List Char))
(doc *default-universe* 'description "Default character universe for . and [^...]: printable ASCII plus whitespace")

;;; ============================================================
;;; Section 2: Regex AST
;;; ============================================================

(doc 'section 'regex-ast)

;;; AST node constructors

(define (regex-lit char)
  (doc 'type '(-> Char RegexAST))
  (list 'regex-lit char))

(define (regex-lit? x)
  (and (pair? x) (eq? (car x) 'regex-lit)))

(define (regex-lit-char x)
  (cadr x))

(define (regex-dot)
  (doc 'type '(-> RegexAST))
  '(regex-dot))

(define (regex-dot? x)
  (and (pair? x) (eq? (car x) 'regex-dot)))

(define (regex-class chars negated?)
  (doc 'type '(-> (List Char) Boolean RegexAST))
  (doc 'description "Character class. negated? = #t for [^...]")
  (list 'regex-class chars negated?))

(define (regex-class? x)
  (and (pair? x) (eq? (car x) 'regex-class)))

(define (regex-class-chars x)
  (cadr x))

(define (regex-class-negated? x)
  (caddr x))

(define (regex-seq exprs)
  (doc 'type '(-> (List RegexAST) RegexAST))
  (doc 'description "Concatenation of expressions")
  (list 'regex-seq exprs))

(define (regex-seq? x)
  (and (pair? x) (eq? (car x) 'regex-seq)))

(define (regex-seq-exprs x)
  (cadr x))

(define (regex-alt exprs)
  (doc 'type '(-> (List RegexAST) RegexAST))
  (doc 'description "Alternation (union) of expressions")
  (list 'regex-alt exprs))

(define (regex-alt? x)
  (and (pair? x) (eq? (car x) 'regex-alt)))

(define (regex-alt-exprs x)
  (cadr x))

(define (regex-star expr)
  (doc 'type '(-> RegexAST RegexAST))
  (list 'regex-star expr))

(define (regex-star? x)
  (and (pair? x) (eq? (car x) 'regex-star)))

(define (regex-star-expr x)
  (cadr x))

(define (regex-plus expr)
  (doc 'type '(-> RegexAST RegexAST))
  (list 'regex-plus expr))

(define (regex-plus? x)
  (and (pair? x) (eq? (car x) 'regex-plus)))

(define (regex-plus-expr x)
  (cadr x))

(define (regex-opt expr)
  (doc 'type '(-> RegexAST RegexAST))
  (list 'regex-opt expr))

(define (regex-opt? x)
  (and (pair? x) (eq? (car x) 'regex-opt)))

(define (regex-opt-expr x)
  (cadr x))

(define (regex-group expr)
  (doc 'type '(-> RegexAST RegexAST))
  (doc 'description "Grouping (non-capturing)")
  (list 'regex-group expr))

(define (regex-group? x)
  (and (pair? x) (eq? (car x) 'regex-group)))

(define (regex-group-expr x)
  (cadr x))

(define (regex-empty)
  (doc 'type '(-> RegexAST))
  (doc 'description "Empty regex - matches empty string")
  '(regex-empty))

(define (regex-empty? x)
  (and (pair? x) (eq? (car x) 'regex-empty)))

;;; Quantifier ranges: {n}, {n,m}, {n,}, {,m}
(define (regex-repeat expr min max)
  (doc 'type '(-> RegexAST Nat (Option Nat) RegexAST))
  (doc 'description "Bounded repetition. max = #f means unbounded.")
  (list 'regex-repeat expr min max))

(define (regex-repeat? x)
  (and (pair? x) (eq? (car x) 'regex-repeat)))

(define (regex-repeat-expr x)
  (cadr x))

(define (regex-repeat-min x)
  (caddr x))

(define (regex-repeat-max x)
  (cadddr x))

;;; Anchors: ^ (start of string), $ (end of string)
(define (regex-anchor type)
  (doc 'type '(-> Symbol RegexAST))
  (doc 'description "Zero-width anchor. type = 'start or 'end")
  (list 'regex-anchor type))

(define (regex-anchor? x)
  (and (pair? x) (eq? (car x) 'regex-anchor)))

(define (regex-anchor-type x)
  (cadr x))

;;; Lookahead assertions: (?=...) and (?!...)
(define (regex-lookahead expr positive?)
  (doc 'type '(-> RegexAST Boolean RegexAST))
  (doc 'description "Lookahead assertion. positive? = #t for (?=...), #f for (?!...)")
  (list 'regex-lookahead expr positive?))

(define (regex-lookahead? x)
  (and (pair? x) (eq? (car x) 'regex-lookahead)))

(define (regex-lookahead-expr x)
  (cadr x))

(define (regex-lookahead-positive? x)
  (caddr x))

;;; ============================================================
;;; Section 3: Regex Parser
;;; ============================================================

(doc 'section 'regex-parser)

;;; Grammar (precedence low to high):
;;;   regex     = alt
;;;   alt       = seq ('|' seq)*
;;;   seq       = postfix+
;;;   postfix   = atom ('*' | '+' | '?' | interval)*
;;;   interval  = '{' num '}' | '{' num? ',' num? '}'
;;;   atom      = lookahead | group | class | anchor | dot | literal
;;;   lookahead = '(?' ('=' | '!') regex ')'
;;;   anchor    = '^' | '$'
;;;   class     = '[' '^'? (char | range)+ ']'
;;;   literal   = char | escape

;;; Escape sequences
(define (parse-escape)
  (doc 'type '(Parser Char))
  (parser-bind
   (parser-char #\\)
   (lambda (_)
     (parser-or
      (parser-bind (parser-char #\n) (lambda (_) (parser-pure #\newline)))
      (parser-or
       (parser-bind (parser-char #\t) (lambda (_) (parser-pure #\tab)))
       (parser-or
        (parser-bind (parser-char #\r) (lambda (_) (parser-pure #\return)))
        (parser-or
         (parser-bind (parser-char #\\) (lambda (_) (parser-pure #\\)))
         ;; Meta characters that need escaping
         (parser-one-of "*+?.|[]()^${}"))))))))

;;; Literal character (not a metachar, or escaped)
(define (parse-literal)
  (doc 'type '(Parser RegexAST))
  (parser-map regex-lit
              (parser-or (parse-escape)
                         (parser-satisfy (lambda (c)
                                    (not (member c '(#\* #\+ #\? #\. #\| #\[ #\] #\( #\) #\\ #\^ #\$ #\{))))
                                  "literal character"))))

;;; Dot (any character)
(define (parse-dot)
  (doc 'type '(Parser RegexAST))
  (parser-bind (parser-char #\.)
               (lambda (_) (parser-pure (regex-dot)))))

;;; Character class: [abc] or [^abc] or [a-z]
(define (parse-class)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parser-char #\[)
   (lambda (_)
     (parser-bind
      (parser-option-maybe (parser-char #\^))
      (lambda (neg-maybe)
        (let ([negated? (just? neg-maybe)])
          (parser-bind
           (parser-some (parse-class-item))
           (lambda (items)
             (parser-bind
              (parser-char #\])
              (lambda (_)
                ;; Flatten items (ranges expand to lists)
                (let ([chars (flatten items)])
                  (parser-pure (regex-class chars negated?)))))))))))))

;;; Class item: single char or range (a-z)
(define (parse-class-item)
  (doc 'type '(Parser (List Char)))
  (parser-or
   (parser-try (parse-class-range))  ; Use try to backtrack if range fails after consuming start char
   (parser-map list (parse-class-char))))

;;; Character inside a class (handles escapes and literal ']' at start)
(define (parse-class-char)
  (doc 'type '(Parser Char))
  (parser-or
   (parse-escape)
   (parser-satisfy (lambda (c) (not (member c '(#\] #\\))))
            "class character")))

;;; Character range: a-z (but not if dash is at start/end)
(define (parse-class-range)
  (doc 'type '(Parser (List Char)))
  (parser-bind
   (parse-class-char)
   (lambda (start)
     (parser-bind
      (parser-char #\-)
      (lambda (_)
        (parser-bind
         (parse-class-char)
         (lambda (end)
           (if (char<=? start end)
               (let* ([start-int (char->integer start)]
                      [count (+ 1 (- (char->integer end) start-int))])
                 (parser-pure (map (lambda (i) (integer->char (+ i start-int)))
                                   (iota count))))
               (parser-fail "invalid range: start > end")))))))))

;;; Grouped expression: (regex)
(define (parse-group)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parser-char #\()
   (lambda (_)
     (parser-bind
      (parse-regex)
      (lambda (expr)
        (parser-bind
         (parser-char #\))
         (lambda (_)
           (parser-pure (regex-group expr)))))))))

;;; Lookahead assertion: (?=...) or (?!...)
;;; Wrapped in try for backtracking when used in choice with parse-group
(define (parse-lookahead)
  (doc 'type '(Parser RegexAST))
  (parser-try
   (parser-bind
    (parser-char #\()
    (lambda (_)
      (parser-bind
       (parser-char #\?)
       (lambda (_)
         (parser-bind
          (parser-one-of "=!")
          (lambda (type-char)
            (parser-bind
             (parse-regex)
             (lambda (inner)
               (parser-bind
                (parser-char #\))
                (lambda (_)
                  (parser-pure
                   (regex-lookahead inner (char=? type-char #\=)))))))))))))))

;;; Anchor: ^ or $
(define (parse-anchor)
  (doc 'type '(Parser RegexAST))
  (parser-or
   (parser-bind (parser-char #\^) (lambda (_) (parser-pure (regex-anchor 'start))))
   (parser-bind (parser-char #\$) (lambda (_) (parser-pure (regex-anchor 'end))))))

;;; Interval quantifier: {n}, {n,}, {,m}, {n,m}
(define (parse-interval)
  (doc 'type '(Parser (Pair Nat (Option Nat))))
  (doc 'description "Parse interval quantifier, returns (min . max) where max=#f means unbounded")
  (parser-bind
   (parser-char #\{)
   (lambda (_)
     (parser-bind
      (parser-option-maybe parser-natural)
      (lambda (min-maybe)
        (parser-or
         ;; Case: {n} - exact count (requires min to be present)
         (parser-bind
          (parser-char #\})
          (lambda (_)
            (if (nothing? min-maybe)
                (parser-fail "empty interval {} is invalid")
                (let ([n (from-just min-maybe)])
                  (parser-pure (cons n n))))))
         ;; Case: {n,}, {,m}, or {n,m}
         (parser-bind
          (parser-char #\,)
          (lambda (_)
            (parser-bind
             (parser-option-maybe parser-natural)
             (lambda (max-maybe)
               (parser-bind
                (parser-char #\})
                (lambda (_)
                  (let ([min (if (nothing? min-maybe) 0 (from-just min-maybe))]
                        [max (if (nothing? max-maybe) #f (from-just max-maybe))])
                    ;; Validate: if both present, min <= max
                    (if (and max (> min max))
                        (parser-fail "interval min > max")
                        (parser-pure (cons min max))))))))))))))))

;;; Atom: lookahead | group | class | anchor | dot | literal
(define (parse-atom)
  (doc 'type '(Parser RegexAST))
  (parser-choice (list (parse-lookahead)  ; Must come before parse-group
                (parse-group)
                (parse-class)
                (parse-anchor)
                (parse-dot)
                (parse-literal))))

;;; Single postfix operator: *, +, ?, or {n,m}
(define (parse-postfix-op)
  (doc 'type '(Parser (Union Char (Pair Nat (Option Nat)))))
  (parser-or
   (parser-one-of "*+?")
   (parse-interval)))

;;; Apply a postfix operator to an expression
(define (apply-postfix-op expr op)
  (cond
   [(char? op)
    (cond
     [(char=? op #\*) (regex-star expr)]
     [(char=? op #\+) (regex-plus expr)]
     [(char=? op #\?) (regex-opt expr)])]
   [(pair? op)
    ;; Interval: (min . max)
    (regex-repeat expr (car op) (cdr op))]))

;;; Postfix operators: atom followed by *, +, ?, {n,m}
(define (parse-postfix)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parse-atom)
   (lambda (atom)
     (parser-bind
      (parser-many (parse-postfix-op))
      (lambda (ops)
        (parser-pure (fold-left apply-postfix-op atom ops)))))))

;;; Sequence: one or more postfix expressions concatenated
(define (parse-seq)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parser-many (parse-postfix))
   (lambda (exprs)
     (parser-pure
      (cond
       [(null? exprs) (regex-empty)]
       [(null? (cdr exprs)) (car exprs)]
       [else (regex-seq exprs)])))))

;;; Alternation: seq | seq | ...
(define (parse-alt)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parse-seq)
   (lambda (first)
     (parser-bind
      (parser-many (parser-then (parser-char #\|) (parse-seq)))
      (lambda (rest)
        (parser-pure
         (if (null? rest)
             first
             (regex-alt (cons first rest)))))))))

;;; Top-level regex parser
(define (parse-regex)
  (doc 'type '(Parser RegexAST))
  (parse-alt))

;;; Parse a regex string into an AST
(define (regex-parse str)
  (doc 'type '(-> String (Either ParseError RegexAST)))
  (doc 'description "Parse a regex string into an AST")
  (parser-parse (parser-left (parse-regex) parser-eof) str))

;;; ============================================================
;;; Section 4: Thompson's Construction
;;; ============================================================

(doc 'section 'thompson-construction)

;;; Compile regex AST to NFA
;;; Uses the existing FSM library operations

(define (regex-compile ast universe)
  (doc 'type '(-> RegexAST (List Char) FSM))
  (doc 'description "Compile regex AST to NFA using Thompson's construction")
  (cond
   ;; Empty: accepts empty string
   [(regex-empty? ast)
    (fsm-epsilon-lang)]

   ;; Literal: single character
   [(regex-lit? ast)
    (fsm-char (regex-lit-char ast))]

   ;; Dot: any character in universe
   [(regex-dot? ast)
    (fsm-any-of universe)]

   ;; Character class
   [(regex-class? ast)
    (let ([chars (regex-class-chars ast)]
          [negated? (regex-class-negated? ast)])
      (if negated?
          ;; [^abc] = universe - chars
          (fsm-any-of (filter (lambda (c) (not (member c chars))) universe))
          ;; [abc]
          (fsm-any-of chars)))]

   ;; Sequence: concatenation
   [(regex-seq? ast)
    (let ([exprs (regex-seq-exprs ast)])
      (if (null? exprs)
          (fsm-epsilon-lang)
          (fold-left fsm-concat
                     (regex-compile (car exprs) universe)
                     (map (lambda (e) (regex-compile e universe))
                          (cdr exprs)))))]

   ;; Alternation: union
   [(regex-alt? ast)
    (let ([exprs (regex-alt-exprs ast)])
      (cond
       [(null? exprs)
        ;; Empty alternation: accepts nothing
        (make-fsm '(dead) '() '() 'dead '())]
       ;; Optimization: all literals -> fsm-any-of
       [(and (pair? exprs) (every regex-lit? exprs))
        (fsm-any-of (map regex-lit-char exprs))]
       [else
        (fold-left fsm-union
                   (regex-compile (car exprs) universe)
                   (map (lambda (e) (regex-compile e universe))
                        (cdr exprs)))]))]

   ;; Star: zero or more
   [(regex-star? ast)
    (fsm-star (regex-compile (regex-star-expr ast) universe))]

   ;; Plus: one or more
   [(regex-plus? ast)
    (fsm-plus (regex-compile (regex-plus-expr ast) universe))]

   ;; Optional: zero or one
   [(regex-opt? ast)
    (fsm-optional (regex-compile (regex-opt-expr ast) universe))]

   ;; Group: just compile the inner expression
   [(regex-group? ast)
    (regex-compile (regex-group-expr ast) universe)]

   ;; Repeat: {n}, {n,m}, {n,}, {,m}
   [(regex-repeat? ast)
    (compile-repeat (regex-repeat-expr ast)
                    (regex-repeat-min ast)
                    (regex-repeat-max ast)
                    universe)]

   ;; Anchor: ^ or $
   [(regex-anchor? ast)
    (compile-anchor (regex-anchor-type ast))]

   ;; Lookahead: (?=...) or (?!...)
   [(regex-lookahead? ast)
    (compile-lookahead (regex-lookahead-expr ast)
                       (regex-lookahead-positive? ast)
                       universe)]

   [else
    (error 'regex-compile (format "Unknown AST node: ~a" ast))]))

;;; Helper: compile repeat quantifier {n,m}
(define (compile-repeat expr min max universe)
  (doc 'type '(-> RegexAST Nat (Option Nat) (List Char) FSM))
  (let ([base (regex-compile expr universe)])
    (cond
     ;; {0,0} → empty string
     [(and (= min 0) (eqv? max 0))
      (fsm-epsilon-lang)]
     ;; {0,} → star (equivalent to a*)
     [(and (= min 0) (not max))
      (fsm-star base)]
     ;; {0,m} → m optionals chained
     [(= min 0)
      (fold-left (lambda (acc _)
                   (fsm-optional (fsm-concat base acc)))
                 (fsm-epsilon-lang)
                 (iota max))]
     ;; {n} or {n,n} → concatenate n copies
     [(eqv? min max)
      (fold-left fsm-concat base
                 (map (lambda (_) (regex-compile expr universe))
                      (iota (- min 1))))]
     ;; {n,} → n copies then star
     [(not max)
      (let ([required (fold-left fsm-concat base
                                 (map (lambda (_) (regex-compile expr universe))
                                      (iota (- min 1))))])
        (fsm-concat required (fsm-star base)))]
     ;; {n,m} → n copies then (m-n) optionals
     [else
      (let* ([required (fold-left fsm-concat base
                                  (map (lambda (_) (regex-compile expr universe))
                                       (iota (- min 1))))]
             [optional (fold-left (lambda (acc _)
                                    (fsm-concat (fsm-optional base) acc))
                                  (fsm-epsilon-lang)
                                  (iota (- max min)))])
        (fsm-concat required optional))])))

;;; Helper: compile anchor (^ or $)
(define (compile-anchor type)
  (doc 'type '(-> Symbol FSM))
  (let ([s0 (fsm-fresh-state "anc")]
        [s1 (fsm-fresh-state "anc")])
    (make-fsm-with-assertions
     (list s0 s1) '() '() s0 (list s1) '()
     (list (list s0 'anchor type s1)))))

;;; Helper: compile lookahead assertion
(define (compile-lookahead expr positive? universe)
  (doc 'type '(-> RegexAST Boolean (List Char) FSM))
  (let ([inner (regex-compile expr universe)]
        [s0 (fsm-fresh-state "la")]
        [s1 (fsm-fresh-state "la")])
    (make-fsm-with-assertions
     (list s0 s1) '() '() s0 (list s1) '()
     (list (list s0 'lookahead inner positive? s1)))))

;;; ============================================================
;;; Section 5: High-Level Interface
;;; ============================================================

(doc 'section 'high-level-interface)

(define regex->nfa
  (case-lambda
   [(pattern)
    (regex->nfa pattern *default-universe*)]
   [(pattern universe)
    (doc 'type '(-> String (Option (List Char)) FSM))
    (doc 'description "Compile regex pattern to NFA. Uses default ASCII universe if not specified.")
    (let ([parse-result (regex-parse pattern)])
      (if (left? parse-result)
          (error 'regex->nfa
                 (format "Parse error: ~a" (error-message (from-left parse-result))))
          (regex-compile (from-right parse-result) universe)))]))

(define regex->dfa
  (case-lambda
   [(pattern)
    (regex->dfa pattern *default-universe*)]
   [(pattern universe)
    (doc 'type '(-> String (Option (List Char)) FSM))
    (doc 'description "Compile regex pattern to minimized DFA")
    (fsm-minimize (regex->nfa pattern universe))]))

(define regex-accepts?
  (case-lambda
   [(pattern input)
    (regex-accepts? pattern input *default-universe*)]
   [(pattern input universe)
    (doc 'type '(-> String String (Option (List Char)) Boolean))
    (doc 'description "Check if regex pattern matches entire input string")
    (fsm-accepts? (regex->nfa pattern universe) input)]))

(define regex-match
  (case-lambda
   [(pattern input)
    (regex-match pattern input *default-universe*)]
   [(pattern input universe)
    (doc 'type '(-> String String (Option (List Char)) (Maybe (List State))))
    (doc 'description "Match regex pattern against input. Returns Just final-states on match, Nothing otherwise.")
    (let ([nfa (regex->nfa pattern universe)])
      (if (null? (fsm-assertions nfa))
          (fsm-run nfa input)
          (fsm-run-with-assertions nfa input)))]))

;;; ============================================================
;;; Section 6: Utilities
;;; ============================================================

(doc 'section 'utilities)

(define (regex-ast->string ast)
  (doc 'type '(-> RegexAST String))
  (doc 'description "Convert regex AST to string representation for debugging")
  (cond
   [(regex-empty? ast) "empty"]
   [(regex-lit? ast) (format "lit(~a)" (regex-lit-char ast))]
   [(regex-dot? ast) "dot"]
   [(regex-class? ast)
    (format "class(~a~a)"
            (if (regex-class-negated? ast) "^" "")
            (list->string (regex-class-chars ast)))]
   [(regex-seq? ast)
    (format "seq(~a)" (string-join (map regex-ast->string (regex-seq-exprs ast)) " "))]
   [(regex-alt? ast)
    (format "alt(~a)" (string-join (map regex-ast->string (regex-alt-exprs ast)) "|"))]
   [(regex-star? ast)
    (format "star(~a)" (regex-ast->string (regex-star-expr ast)))]
   [(regex-plus? ast)
    (format "plus(~a)" (regex-ast->string (regex-plus-expr ast)))]
   [(regex-opt? ast)
    (format "opt(~a)" (regex-ast->string (regex-opt-expr ast)))]
   [(regex-group? ast)
    (format "group(~a)" (regex-ast->string (regex-group-expr ast)))]
   [(regex-repeat? ast)
    (let ([min (regex-repeat-min ast)]
          [max (regex-repeat-max ast)])
      (format "repeat(~a,~a,~a)"
              (regex-ast->string (regex-repeat-expr ast))
              min
              (if max max "inf")))]
   [(regex-anchor? ast)
    (format "anchor(~a)" (regex-anchor-type ast))]
   [(regex-lookahead? ast)
    (format "~a(~a)"
            (if (regex-lookahead-positive? ast) "lookahead" "neglookahead")
            (regex-ast->string (regex-lookahead-expr ast)))]
   [else (format "unknown(~a)" ast)]))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; AST Constructors:
;;;   regex-lit, regex-dot, regex-class, regex-seq, regex-alt,
;;;   regex-star, regex-plus, regex-opt, regex-group, regex-empty,
;;;   regex-repeat, regex-anchor, regex-lookahead
;;;
;;; AST Predicates:
;;;   regex-lit?, regex-dot?, regex-class?, regex-seq?, regex-alt?,
;;;   regex-star?, regex-plus?, regex-opt?, regex-group?, regex-empty?,
;;;   regex-repeat?, regex-anchor?, regex-lookahead?
;;;
;;; AST Accessors:
;;;   regex-lit-char, regex-class-chars, regex-class-negated?,
;;;   regex-seq-exprs, regex-alt-exprs, regex-star-expr,
;;;   regex-plus-expr, regex-opt-expr, regex-group-expr,
;;;   regex-repeat-expr, regex-repeat-min, regex-repeat-max,
;;;   regex-anchor-type, regex-lookahead-expr, regex-lookahead-positive?
;;;
;;; Parsing:
;;;   regex-parse
;;;
;;; Compilation:
;;;   regex-compile, regex->nfa, regex->dfa
;;;
;;; Matching:
;;;   regex-accepts?, regex-match
;;;
;;; Utilities:
;;;   regex-ast->string, *default-universe*
