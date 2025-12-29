;;; fabric/stitches/fold-parse.ss — Fold Syntax Parser
;;;
;;; Parses Fold's S-expression syntax using spanned combinators.
;;; Returns AST nodes with source spans for error reporting.
;;;
;;; Fold Syntax:
;;;   expr = atom | list | quoted
;;;   atom = number | string | symbol | boolean
;;;   list = "(" expr* ")"
;;;   quoted = "'" expr
;;;   number = int | float
;;;   string = '"' chars '"'
;;;   symbol = identifier
;;;   boolean = #t | #f
;;;   comment = ";" to-end-of-line
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - span.ss (spanned combinators)

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/span.ss")

;;; ============================================================
;;; Fold AST Types
;;; ============================================================

;;; AST nodes are spanned values:
;;;   (spanned value span)
;;;
;;; Values:
;;;   - Numbers: just the number
;;;   - Strings: just the string
;;;   - Symbols: just the symbol
;;;   - Booleans: #t or #f
;;;   - Lists: list of spanned values

;;; ============================================================
;;; Lexical Elements
;;; ============================================================

;;; fold-comment : SpannedParser ()
;;; Skip a line comment.
(define fold-comment
  (s-bind (s-char #\;) (lambda (_)
                               (s-bind (s-many (s-satisfy (lambda (c) (not (char=? c #\newline)))
                                                          "non-newline"))
                                       (lambda (_)
                                               (s-pure '()))))))

;;; fold-whitespace : SpannedParser ()
;;; Skip whitespace and comments.
(define fold-whitespace
  (s-bind (s-many (s-alt s-space fold-comment)) (lambda (_)
                                                        (s-pure '()))))

;;; fold-token : SpannedParser a → SpannedParser a
;;; Parse a token and skip trailing whitespace.
(define (fold-token parser)
  (s-seq-left parser fold-whitespace))

;;; ============================================================
;;; Atoms
;;; ============================================================

;;; fold-number : SpannedParser Number
;;; Parse an integer or floating point number.
(define fold-number
  (let* ([digits (s-many1 s-digit)]
         [sign (s-optional (s-alt (s-char #\-) (s-char #\+)))])
        (s-bind sign (lambda (s)
                             (s-bind digits (lambda (int-part)
                                                    (s-bind (s-optional (s-bind (s-char #\.) (lambda (_)
                                                                                                     (s-many1 s-digit))))
                                                            (lambda (frac)
                                                                    (let* ([neg? (and (not (null? s)) (char=? (car s) #\-))]
                                                                           [int-str (list->string int-part)]
                                                                           [num-str (if (null? frac)
                                                                                        int-str
                                                                                        (string-append int-str "."
                                                                                                       (list->string (car frac))))]
                                                                           [num (string->number num-str)])
                                                                          (s-pure (if neg? (- num) num)))))))))))
;;; fold-string : SpannedParser String
;;; Parse a double-quoted string with escape sequences.
(define fold-string
  (let* ([escape (s-bind (s-char #\\) (lambda (_)
                                              (s-bind s-item (lambda (c)
                                                                     (s-pure (case c
                                                                                   [(#\n) #\newline]
                                                                                   [(#\t) #\tab]
                                                                                   [(#\r) #\return]
                                                                                   [(#\\) #\\]
                                                                                   [(#\") #\"]
                                                                                   [else c]))))))]
         [string-char (s-alt escape
                             (s-satisfy (lambda (c)
                                                (not (or (char=? c #\")
                                                         (char=? c #\\))))
                                        "string character"))])
        (s-bind (s-char #\") (lambda (_)
                                     (s-bind (s-many string-char) (lambda (chars)
                                                                          (s-bind (s-char #\") (lambda (_)
                                                                                                       (s-pure (list->string chars))))))))))

;;; fold-boolean : SpannedParser Boolean
;;; Parse #t or #f.
(define fold-boolean
  (s-alt
   (s-bind (s-string "#t") (lambda (_) (s-pure #t)))
   (s-bind (s-string "#f") (lambda (_) (s-pure #f)))))

;;; symbol-initial? : Char → Boolean
;;; Characters that can start a symbol.
(define (symbol-initial? c)
  (or (char-alphabetic? c)
      (memv c '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\+ #\-))))

;;; symbol-subsequent? : Char → Boolean
;;; Characters that can follow in a symbol.
(define (symbol-subsequent? c)
  (or (symbol-initial? c)
      (char-numeric? c)
      (memv c '(#\. #\@))))

;;; fold-symbol : SpannedParser Symbol
;;; Parse a symbol/identifier.
(define fold-symbol
  (s-bind (s-satisfy symbol-initial? "symbol start") (lambda (first)
                                                             (s-bind (s-many (s-satisfy symbol-subsequent? "symbol char")) (lambda (rest)
                                                                                                                                   (s-pure (string->symbol (list->string (cons first rest)))))))))

;;; ============================================================
;;; Expressions
;;; ============================================================

;;; Forward declaration for mutual recursion
(define fold-expr #f)

;;; fold-atom : SpannedParser Atom
;;; Parse an atomic value.
(define fold-atom
  (fold-token
   (s-choice
    (list fold-boolean
          fold-number
          fold-string
          fold-symbol))))

;;; fold-list : SpannedParser (List Expr)
;;; Parse a parenthesized list.
(define fold-list
  (s-bind (fold-token (s-char #\()) (lambda (_)
                                            (s-bind (s-many (lambda (s) (fold-expr s))) (lambda (exprs)
                                                                                                (s-bind (fold-token (s-char #\))) (lambda (_)
                                                                                                                                          (s-pure exprs))))))))

;;; fold-quoted : SpannedParser Expr
;;; Parse a quoted expression: 'x -> (quote x)
(define fold-quoted
  (s-bind (s-char #\') (lambda (_)
                               (s-bind (lambda (s) (fold-expr s)) (lambda (expr)
                                                                          (s-pure (list 'quote expr)))))))

;;; fold-quasiquoted : SpannedParser Expr
;;; Parse a quasiquoted expression: `x -> (quasiquote x)
(define fold-quasiquoted
  (s-bind (s-char #\`) (lambda (_)
                               (s-bind (lambda (s) (fold-expr s)) (lambda (expr)
                                                                          (s-pure (list 'quasiquote expr)))))))

;;; fold-unquoted : SpannedParser Expr
;;; Parse an unquoted expression: ,x -> (unquote x)
(define fold-unquoted
  (s-bind (s-char #\,) (lambda (_)
                               (s-alt
                                ;; ,@ for unquote-splicing
                                (s-bind (s-char #\@) (lambda (_)
                                                             (s-bind (lambda (s) (fold-expr s)) (lambda (expr)
                                                                                                        (s-pure (list 'unquote-splicing expr))))))
                                ;; just , for unquote
                                (s-bind (lambda (s) (fold-expr s)) (lambda (expr)
                                                                           (s-pure (list 'unquote expr))))))))

;;; Initialize fold-expr
(set! fold-expr
      (lambda (state)
              ((s-bind fold-whitespace (lambda (_)
                                               (s-spanned
                                                (s-choice
                                                 (list fold-quoted
                                                       fold-quasiquoted
                                                       fold-unquoted
                                                       fold-list
                                                       fold-atom)))))
               state)))

;;; ============================================================
;;; Top-Level Parser
;;; ============================================================

;;; fold-program : SpannedParser (List Expr)
;;; Parse a sequence of expressions.
(define fold-program
  (s-bind fold-whitespace (lambda (_)
                                  (s-seq-left (s-many fold-expr) s-eof))))

;;; parse-fold : String [× String] → Result
;;; Parse Fold source code, returning spanned AST or error.
;;; Result = (ok ast) | (error parse expected span)
(define (parse-fold input . file-arg)
  (let* ([file (if (null? file-arg) "<input>" (car file-arg))]
         [result (run-spanned fold-program input file)])
        (if (spanned-ok? result)
            (list 'ok (spanned-value result))
            (let* ([state (spanned-error-state result)]
                   [expected (spanned-expected result)]
                   [span (state-span state)])
                  (list 'error 'parse expected span)))))

;;; parse-fold-expr : String [× String] → Result
;;; Parse a single Fold expression.
(define (parse-fold-expr input . file-arg)
  (let* ([file (if (null? file-arg) "<input>" (car file-arg))]
         [result (run-spanned fold-expr input file)])
        (if (spanned-ok? result)
            (list 'ok (spanned-value result))
            (let* ([state (spanned-error-state result)]
                   [expected (spanned-expected result)]
                   [span (state-span state)])
                  (list 'error 'parse expected span)))))

;;; ============================================================
;;; Span Extraction Utilities
;;; ============================================================

;;; strip-spans : SpannedExpr → Expr
;;; Remove span wrappers from an AST, leaving just the values.
(define (strip-spans expr)
  (cond
   [(spanned-value? expr)
    (strip-spans (get-spanned-value expr))]
   [(pair? expr)
    (map strip-spans expr)]
   [else expr]))

;;; collect-spans : SpannedExpr → (List (Expr × Span))
;;; Collect all spans from an AST.
(define (collect-spans expr)
  (cond
   [(spanned-value? expr)
    (cons (cons (strip-spans (get-spanned-value expr))
                (get-value-span expr))
          (collect-spans (get-spanned-value expr)))]
   [(pair? expr)
    (apply append (map collect-spans expr))]
   [else '()]))
