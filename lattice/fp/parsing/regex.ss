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
;;;   Escapes:       \*, \+, \?, \., \[, \], \(, \), \|, \\, \n, \t, \r
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - lattice/fp/parsing/parser.ss
;;;   - lattice/fp/parsing/fsm.ss

(load "core/base/prelude.ss")
(load "lattice/fp/parsing/parser.ss")
(load "lattice/fp/parsing/fsm.ss")

(doc 'module 'regex)
(doc 'description "Regular expression compilation via Thompson's construction")
(doc 'layer 'lattice)
(doc 'purity 'total)

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

;;; ============================================================
;;; Section 3: Regex Parser
;;; ============================================================

(doc 'section 'regex-parser)

;;; Grammar (precedence low to high):
;;;   regex     = alt
;;;   alt       = seq ('|' seq)*
;;;   seq       = postfix+
;;;   postfix   = atom ('*' | '+' | '?')*
;;;   atom      = literal | '.' | class | '(' regex ')'
;;;   class     = '[' '^'? (char | range)+ ']'
;;;   literal   = char | escape

;;; Escape sequences
(define (parse-escape)
  (doc 'type '(Parser Char))
  (parser-bind
   (char #\\)
   (lambda (_)
     (parser-or
      (parser-bind (char #\n) (lambda (_) (parser-pure #\newline)))
      (parser-or
       (parser-bind (char #\t) (lambda (_) (parser-pure #\tab)))
       (parser-or
        (parser-bind (char #\r) (lambda (_) (parser-pure #\return)))
        (parser-or
         (parser-bind (char #\\) (lambda (_) (parser-pure #\\)))
         ;; Meta characters that need escaping
         (one-of "*+?.|[]()^$"))))))))

;;; Literal character (not a metachar, or escaped)
(define (parse-literal)
  (doc 'type '(Parser RegexAST))
  (parser-map regex-lit
              (parser-or (parse-escape)
                         (satisfy (lambda (c)
                                    (not (member c '(#\* #\+ #\? #\. #\| #\[ #\] #\( #\) #\\))))
                                  "literal character"))))

;;; Dot (any character)
(define (parse-dot)
  (doc 'type '(Parser RegexAST))
  (parser-bind (char #\.)
               (lambda (_) (parser-pure (regex-dot)))))

;;; Character class: [abc] or [^abc] or [a-z]
(define (parse-class)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (char #\[)
   (lambda (_)
     (parser-bind
      (option-maybe (char #\^))
      (lambda (neg-maybe)
        (let ([negated? (just? neg-maybe)])
          (parser-bind
           (some (parse-class-item))
           (lambda (items)
             (parser-bind
              (char #\])
              (lambda (_)
                ;; Flatten items (ranges expand to lists)
                (let ([chars (apply append items)])
                  (parser-pure (regex-class chars negated?)))))))))))))

;;; Class item: single char or range (a-z)
(define (parse-class-item)
  (doc 'type '(Parser (List Char)))
  (parser-or
   (try (parse-class-range))  ; Use try to backtrack if range fails after consuming start char
   (parser-map list (parse-class-char))))

;;; Character inside a class (handles escapes and literal ']' at start)
(define (parse-class-char)
  (doc 'type '(Parser Char))
  (parser-or
   (parse-escape)
   (satisfy (lambda (c) (not (member c '(#\] #\\))))
            "class character")))

;;; Character range: a-z (but not if dash is at start/end)
(define (parse-class-range)
  (doc 'type '(Parser (List Char)))
  (parser-bind
   (parse-class-char)
   (lambda (start)
     (parser-bind
      (char #\-)
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
   (char #\()
   (lambda (_)
     (parser-bind
      (parse-regex)
      (lambda (expr)
        (parser-bind
         (char #\))
         (lambda (_)
           (parser-pure (regex-group expr)))))))))

;;; Atom: literal | dot | class | group
(define (parse-atom)
  (doc 'type '(Parser RegexAST))
  (choice (list (parse-group)
                (parse-class)
                (parse-dot)
                (parse-literal))))

;;; Postfix operators: atom followed by *, +, ?
(define (parse-postfix)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (parse-atom)
   (lambda (atom)
     (parser-bind
      (many (one-of "*+?"))
      (lambda (ops)
        (parser-pure
         (fold-left (lambda (expr op)
                      (cond
                       [(char=? op #\*) (regex-star expr)]
                       [(char=? op #\+) (regex-plus expr)]
                       [(char=? op #\?) (regex-opt expr)]))
                    atom
                    ops)))))))

;;; Sequence: one or more postfix expressions concatenated
(define (parse-seq)
  (doc 'type '(Parser RegexAST))
  (parser-bind
   (many (parse-postfix))
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
      (many (parser-then (char #\|) (parse-seq)))
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
  (parse (parser-left (parse-regex) eof) str))

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
       [(and (pair? exprs) (for-all regex-lit? exprs))
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

   [else
    (error 'regex-compile (format "Unknown AST node: ~a" ast))]))

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
    (fsm-run (regex->nfa pattern universe) input)]))

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
   [else (format "unknown(~a)" ast)]))

;;; Helper: string-join
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; AST Constructors:
;;;   regex-lit, regex-dot, regex-class, regex-seq, regex-alt,
;;;   regex-star, regex-plus, regex-opt, regex-group, regex-empty
;;;
;;; AST Predicates:
;;;   regex-lit?, regex-dot?, regex-class?, regex-seq?, regex-alt?,
;;;   regex-star?, regex-plus?, regex-opt?, regex-group?, regex-empty?
;;;
;;; AST Accessors:
;;;   regex-lit-char, regex-class-chars, regex-class-negated?,
;;;   regex-seq-exprs, regex-alt-exprs, regex-star-expr,
;;;   regex-plus-expr, regex-opt-expr, regex-group-expr
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
