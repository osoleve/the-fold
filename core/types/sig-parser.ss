;;; core/types/sig-parser.ss — Type Signature Parser
;;; @module sig-parser
;;; @requires prelude
;;;
;;; Parses type signatures from documentation comments.
;;;
;;; Signature format:
;;;   ;;; name : Type1 × Type2 → ResultType
;;;   ;;; name : (List α) × α → (List α)
;;;   ;;; name : ∀ α. α → α
;;;
;;; Type grammar:
;;;   Type ::= BaseType | TypeVar | (TypeCon Type ...) | Type → Type | Type × Type
;;;   BaseType ::= Int | Nat | Bool | String | Symbol | Char | Unit | Void | Hash | ...
;;;   TypeVar ::= α | β | γ | ... | a | b | c | ... (lowercase single letters)
;;;   TypeCon ::= List | Vector | Option | Maybe | Pair | Result | ...
;;;
;;; This is Core code: pure, total, assumes perfect input.

(load "core/base/prelude.ss")

;;; ============================================================
;;; Special Characters
;;; ============================================================

(define %arrow "→")
(define %times "×")
(define %forall "∀")
(define %lparen "(")
(define %rparen ")")
(define %pipe "|")
(define %dot ".")

;;; ============================================================
;;; Tokenizer
;;; ============================================================

;;; Token types: symbol, arrow, times, lparen, rparen, pipe, forall, dot
(define (make-token type value)
  (cons type value))

(define (token-type tok) (car tok))
(define (token-value tok) (cdr tok))

;;; Known base types
(define base-type-names
  '(Int Nat Bool String Symbol Char Unit Void Hash Bytes
    Integer Number Real Rational Any))

;;; Known type constructors
(define type-constructor-names
  '(List Vector Option Maybe Pair Result Either Values
    Set Map Ref Block Cap))

;;; Type variable names (Greek letters and single lowercase)
(define greek-type-vars
  '(α β γ δ ε ζ η θ ι κ λ μ ν ξ ο π ρ σ τ υ φ χ ψ ω))

;;; type-var? : Symbol → Boolean
(define (type-var? sym)
  (or (memq sym greek-type-vars)
      ;; Single lowercase letter
      (let ([s (symbol->string sym)])
           (and (= (string-length s) 1)
                (char-lower-case? (string-ref s 0))))))

;;; tokenize-sig : String → (List Token) | (error ...)
;;; Tokenize a type signature string.
(define (tokenize-sig str)
  (let ([len (string-length str)])
       (tokenize-from str 0 len '())))

(define (tokenize-from str pos len tokens)
  (if (>= pos len)
      `(ok ,(reverse tokens))
      (let ([ch (string-ref str pos)])
           (cond
            ;; Skip whitespace
            [(char-whitespace? ch)
             (tokenize-from str (+ pos 1) len tokens)]
            
            ;; Multi-char operators: → ×
            [(and (< (+ pos 2) len)
                  (char=? ch #\→))
             ;; Check if it's the arrow character
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'arrow "→") tokens))]
            [(char=? ch (string-ref %arrow 0))
             ;; Arrow character
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'arrow "→") tokens))]
            [(char=? ch (string-ref %times 0))
             ;; Times character
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'times "×") tokens))]
            [(char=? ch (string-ref %forall 0))
             ;; Forall character
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'forall "∀") tokens))]
            
            ;; Single char tokens
            [(char=? ch #\()
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'lparen "(") tokens))]
            [(char=? ch #\))
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'rparen ")") tokens))]
            [(char=? ch #\|)
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'pipe "|") tokens))]
            [(char=? ch #\.)
             (tokenize-from str (+ pos 1) len
                            (cons (make-token 'dot ".") tokens))]
            [(char=? ch #\#)
             ;; Handle #f, #t
             (if (< (+ pos 1) len)
                 (let ([next (string-ref str (+ pos 1))])
                      (cond
                       [(char=? next #\f)
                        (tokenize-from str (+ pos 2) len
                                       (cons (make-token 'symbol '#f) tokens))]
                       [(char=? next #\t)
                        (tokenize-from str (+ pos 2) len
                                       (cons (make-token 'symbol '#t) tokens))]
                       [else `(error unexpected-char ,ch at ,pos)]))
                 `(error unexpected-char ,ch at ,pos))]
            
            ;; Symbol (identifier)
            [(or (char-alphabetic? ch) (char=? ch #\_))
             (let-values ([(sym end-pos) (scan-symbol str pos len)])
                         (tokenize-from str end-pos len
                                        (cons (make-token 'symbol sym) tokens)))]
            
            ;; Handle UTF-8 Greek letters directly
            [(> (char->integer ch) 127)
             ;; Try to parse as Greek letter
             (let-values ([(sym end-pos) (scan-greek str pos len)])
                         (if sym
                             (tokenize-from str end-pos len
                                            (cons (make-token 'symbol sym) tokens))
                             ;; Check for arrow/times/forall
                             (cond
                              [(string-prefix-at? str pos "→")
                               (tokenize-from str (+ pos (string-length "→")) len
                                              (cons (make-token 'arrow "→") tokens))]
                              [(string-prefix-at? str pos "×")
                               (tokenize-from str (+ pos (string-length "×")) len
                                              (cons (make-token 'times "×") tokens))]
                              [(string-prefix-at? str pos "∀")
                               (tokenize-from str (+ pos (string-length "∀")) len
                                              (cons (make-token 'forall "∀") tokens))]
                              [else `(error unexpected-char ,ch at ,pos)])))]
            
            [else `(error unexpected-char ,ch at ,pos)]))))

;;; string-prefix-at? : String × Nat × String → Boolean
(define (string-prefix-at? str pos prefix)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
       (and (<= (+ pos plen) slen)
            (string=? (substring str pos (+ pos plen)) prefix))))

;;; scan-symbol : String × Nat × Nat → (Values Symbol Nat)
;;; Scan an identifier starting at pos.
(define (scan-symbol str pos len)
  (let loop ([end pos])
       (if (>= end len)
           (values (string->symbol (substring str pos end)) end)
           (let ([ch (string-ref str end)])
                (if (or (char-alphabetic? ch)
                        (char-numeric? ch)
                        (char=? ch #\_)
                        (char=? ch #\-)
                        (char=? ch #\?)
                        (char=? ch #\!))
                    (loop (+ end 1))
                    (values (string->symbol (substring str pos end)) end))))))

;;; scan-greek : String × Nat × Nat → (Values Symbol|#f Nat)
;;; Try to scan a Greek letter.
(define (scan-greek str pos len)
  ;; Greek letters in UTF-8 are multi-byte
  ;; We check for common Greek letters used in type signatures
  (let ([remaining (- len pos)])
       (if (< remaining 2)
           (values #f pos)
           ;; Try matching known Greek letters
           (let loop ([greeks '((α . 2) (β . 2) (γ . 2) (δ . 2) (ε . 2)
                                (ζ . 2) (η . 2) (θ . 2) (ι . 2) (κ . 2)
                                (λ . 2) (μ . 2) (ν . 2) (ξ . 2) (ο . 2)
                                (π . 2) (ρ . 2) (σ . 2) (τ . 2) (υ . 2)
                                (φ . 2) (χ . 2) (ψ . 2) (ω . 2))])
                (if (null? greeks)
                    (values #f pos)
                    (let* ([entry (car greeks)]
                           [sym (car entry)]
                           [sym-str (symbol->string sym)]
                           [sym-len (string-length sym-str)])
                          (if (and (<= (+ pos sym-len) len)
                                   (string=? (substring str pos (+ pos sym-len)) sym-str))
                              (values sym (+ pos sym-len))
                              (loop (cdr greeks)))))))))

;;; ============================================================
;;; Parser
;;; ============================================================

;;; Parser state: list of remaining tokens
;;; Result: (ok ast remaining-tokens) | (error msg)

;;; parse-type : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse a complete type expression.
(define (parse-type tokens)
  (parse-arrow tokens))

;;; parse-arrow : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse arrow types (right-associative): A → B → C = A → (B → C)
(define (parse-arrow tokens)
  (let ([result (parse-product tokens)])
       (if (not (eq? (car result) 'ok))
           result
           (let ([left (cadr result)]
                 [rest (caddr result)])
                (if (and (pair? rest)
                         (eq? (token-type (car rest)) 'arrow))
                    ;; Parse right side of arrow
                    (let ([right-result (parse-arrow (cdr rest))])
                         (if (not (eq? (car right-result) 'ok))
                             right-result
                             (let ([right (cadr right-result)]
                                   [rest2 (caddr right-result)])
                                  `(ok (-> ,left ,right) ,rest2))))
                    ;; No arrow, return left
                    `(ok ,left ,rest))))))

;;; parse-product : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse product types: A × B × C
(define (parse-product tokens)
  (let ([result (parse-union tokens)])
       (if (not (eq? (car result) 'ok))
           result
           (let ([first (cadr result)]
                 [rest (caddr result)])
                (parse-product-rest (list first) rest)))))

(define (parse-product-rest acc tokens)
  (if (and (pair? tokens)
           (eq? (token-type (car tokens)) 'times))
      (let ([result (parse-union (cdr tokens))])
           (if (not (eq? (car result) 'ok))
               result
               (let ([next (cadr result)]
                     [rest (caddr result)])
                    (parse-product-rest (cons next acc) rest))))
      ;; No more products
      (if (= (length acc) 1)
          `(ok ,(car acc) ,tokens)
          `(ok (× ,@(reverse acc)) ,tokens))))

;;; parse-union : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse union types: A | B | C (for things like "α | #f")
(define (parse-union tokens)
  (let ([result (parse-atom tokens)])
       (if (not (eq? (car result) 'ok))
           result
           (let ([first (cadr result)]
                 [rest (caddr result)])
                (parse-union-rest (list first) rest)))))

(define (parse-union-rest acc tokens)
  (if (and (pair? tokens)
           (eq? (token-type (car tokens)) 'pipe))
      (let ([result (parse-atom (cdr tokens))])
           (if (not (eq? (car result) 'ok))
               result
               (let ([next (cadr result)]
                     [rest (caddr result)])
                    (parse-union-rest (cons next acc) rest))))
      ;; No more unions
      (if (= (length acc) 1)
          `(ok ,(car acc) ,tokens)
          ;; Convert A | B to (+ A B)
          `(ok (+ ,@(reverse acc)) ,tokens))))

;;; parse-atom : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse atomic types: symbols, parenthesized expressions, forall
(define (parse-atom tokens)
  (if (null? tokens)
      `(error unexpected-end-of-input)
      (let ([tok (car tokens)]
            [rest (cdr tokens)])
           (case (token-type tok)
                 [(symbol)
                  `(ok ,(token-value tok) ,rest)]
                 [(forall)
                  ;; Parse forall: ∀ vars . body
                  (parse-forall rest)]
                 [(lparen)
                  ;; Parse parenthesized type
                  (parse-paren rest)]
                 [else
                  `(error unexpected-token ,(token-type tok))]))))

;;; parse-forall : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse: ∀ α β ... . Type
(define (parse-forall tokens)
  (let-values ([(vars rest) (parse-forall-vars tokens '())])
              (if (and (pair? rest)
                       (eq? (token-type (car rest)) 'dot))
                  (let ([body-result (parse-type (cdr rest))])
                       (if (not (eq? (car body-result) 'ok))
                           body-result
                           (let ([body (cadr body-result)]
                                 [rest2 (caddr body-result)])
                                `(ok (∀ ,vars ,body) ,rest2))))
                  `(error expected-dot-in-forall))))

(define (parse-forall-vars tokens acc)
  (if (or (null? tokens)
          (eq? (token-type (car tokens)) 'dot))
      (values (reverse acc) tokens)
      (if (eq? (token-type (car tokens)) 'symbol)
          (parse-forall-vars (cdr tokens) (cons (token-value (car tokens)) acc))
          (values (reverse acc) tokens))))

;;; parse-paren : (List Token) → (ok Type Remaining) | (error ...)
;;; Parse parenthesized type: (List α) or (Pair α β) or grouping (α → β)
(define (parse-paren tokens)
  (if (null? tokens)
      `(error unmatched-lparen)
      ;; Check if it starts with a type constructor or is just grouping
      (if (and (eq? (token-type (car tokens)) 'symbol)
               (memq (token-value (car tokens)) type-constructor-names))
          ;; Type application: (List α), (Pair α β)
          (let ([con (token-value (car tokens))])
               (parse-type-app con (cdr tokens)))
          ;; Grouping or tuple
          (let ([inner-result (parse-type tokens)])
               (if (not (eq? (car inner-result) 'ok))
                   inner-result
                   (let ([inner (cadr inner-result)]
                         [rest (caddr inner-result)])
                        (if (and (pair? rest)
                                 (eq? (token-type (car rest)) 'rparen))
                            `(ok ,inner ,(cdr rest))
                            `(error expected-rparen))))))))

;;; parse-type-app : Symbol × (List Token) → (ok Type Remaining) | (error ...)
;;; Parse type application: List α β ... until )
(define (parse-type-app con tokens)
  (parse-type-args con '() tokens))

(define (parse-type-args con acc tokens)
  (if (null? tokens)
      `(error unmatched-lparen)
      (if (eq? (token-type (car tokens)) 'rparen)
          ;; End of application
          (if (null? acc)
              `(ok ,con ,(cdr tokens))
              `(ok (,con ,@(reverse acc)) ,(cdr tokens)))
          ;; Parse next argument
          (let ([arg-result (parse-atom tokens)])
               (if (not (eq? (car arg-result) 'ok))
                   arg-result
                   (let ([arg (cadr arg-result)]
                         [rest (caddr arg-result)])
                        (parse-type-args con (cons arg acc) rest)))))))

;;; ============================================================
;;; Signature Line Parser
;;; ============================================================

;;; parse-sig-line : String → (ok (name . type)) | (error ...) | (skip)
;;; Parse a signature comment line: ";;; name : Type"
(define (parse-sig-line line)
  (let ([trimmed (string-trim line)])
       ;; Must start with ;;;
       (if (not (string-starts-with? trimmed ";;;"))
           '(skip)
           (let* ([after-prefix (substring trimmed 3 (string-length trimmed))]
                  [content (string-trim after-prefix)])
                 ;; Look for name : type pattern
                 (let ([colon-pos (string-index-of content ":")])
                      (if (not colon-pos)
                          '(skip)
                          ;; Check if it looks like a type signature (not "Dependencies:", etc.)
                          (let* ([name-part (string-trim (substring content 0 colon-pos))]
                                 [type-part (string-trim (substring content (+ colon-pos 1)
                                                                    (string-length content)))])
                                ;; Name must be a valid identifier (single word, no spaces)
                                (if (or (string-empty? name-part)
                                        (string-empty? type-part)
                                        ;; Skip if name contains spaces (description, not sig)
                                        (string-contains? name-part " ")
                                        ;; Skip if type part contains characters we don't handle
                                        (string-contains? type-part ",")
                                        (string-contains? type-part "/")
                                        (string-contains? type-part "...")
                                        (string-contains? type-part "'")
                                        (string-contains? type-part "*")
                                        (string-contains? type-part "∘")
                                        ;; Skip if name looks like a keyword
                                        (memq (string->symbol name-part)
                                              '(Dependencies Note Returns Example Complexity
                                                where skip n mode initial params cutoff order
                                                window-fn observable coupling1 coupling2
                                                make-sys param-steps samples)))
                                    '(skip)
                                    ;; Try to parse the type
                                    (let ([tokens-result (tokenize-sig type-part)])
                                         (if (not (eq? (car tokens-result) 'ok))
                                             '(skip)  ;; Skip unparseable types silently
                                             (let* ([tokens (cadr tokens-result)]
                                                    [parse-result (parse-type tokens)])
                                                   (if (not (eq? (car parse-result) 'ok))
                                                       '(skip)  ;; Skip unparseable types silently
                                                       (let ([type (cadr parse-result)]
                                                             [remaining (caddr parse-result)])
                                                            (if (null? remaining)
                                                                `(ok (,(string->symbol name-part) . ,type))
                                                                '(skip)))))))))))))))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; type->internal : ParsedType → InternalType
;;; Convert parsed type to internal representation used by kind checker.
;;; Mainly handles notation differences.
(define (type->internal parsed)
  (cond
   [(symbol? parsed) parsed]
   [(not (pair? parsed)) parsed]
   ;; Function type: (-> A B)
   [(eq? (car parsed) '->)
    `(-> ,@(map type->internal (cdr parsed)))]
   ;; Product type: (× A B C)
   [(eq? (car parsed) '×)
    `(× ,@(map type->internal (cdr parsed)))]
   ;; Sum/union type: (+ A B)
   [(eq? (car parsed) '+)
    `(+ ,@(map (lambda (v)
                       (if (symbol? v)
                           `(,v)  ; Tag with no payload
                           (type->internal v)))
               (cdr parsed)))]
   ;; Forall: (∀ (vars) body)
   [(eq? (car parsed) '∀)
    `(∀ ,(cadr parsed) ,(type->internal (caddr parsed)))]
   ;; Type application: (List α) etc.
   [else
    (map type->internal parsed)]))

;;; format-type : Type → String
;;; Pretty print a type for error messages.
(define (format-type t)
  (cond
   [(symbol? t) (symbol->string t)]
   [(not (pair? t)) (format "~a" t)]
   [(eq? (car t) '->)
    (string-join (map format-type (cdr t)) " → ")]
   [(eq? (car t) '×)
    (string-join (map format-type (cdr t)) " × ")]
   [(eq? (car t) '∀)
    (string-append "∀ " (string-join (map symbol->string (cadr t)) " ")
                   ". " (format-type (caddr t)))]
   [else
    (string-append "(" (string-join (map format-type t) " ") ")")]))
