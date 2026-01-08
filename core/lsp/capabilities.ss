;;; core/lsp/capabilities.ss — LSP Language Features
;;; @module capabilities
;;; @requires prelude json protocol documents
;;;
;;; Implements LSP language features:
;;;   - Hover (type information)
;;;   - Go-to-definition
;;;   - Completion
;;;   - Document symbols
;;;
;;; Integrates with:
;;;   - core/lang/index.ss (symbol index)
;;;   - core/types/infer.ss (type inference)
;;;   - shell/lens/ (navigation)
;;;
;;; This is Core code: pure where possible.

(load "core/base/prelude.ss")
(load "core/lsp/json.ss")
(load "core/lsp/protocol.ss")
(load "core/lsp/documents.ss")

;;; ============================================================
;;; Symbol Index Integration
;;; ============================================================

;;; Try to load the symbol index
(define *index-available* #f)
(guard (e [else (set! *index-available* #f)])
       (load "core/lang/index.ss")
       (set! *index-available* #t))

;;; lookup-symbol-info : String → (Alist Symbol Any) | #f
;;; Look up symbol information from the index.
(define (lookup-symbol-info name)
  (if (and *index-available* (top-level-bound? 'index-lookup))
      (let ([sym (string->symbol name)])
           (guard (e [else #f])
                  (index-lookup sym)))
      #f))

;;; find-symbols-matching : String → (List (Alist Symbol Any))
;;; Find symbols whose names start with prefix.
(define (find-symbols-matching prefix)
  (if (and *index-available* (top-level-bound? 'index-find))
      (guard (e [else '()])
             (index-find prefix))
      '()))

;;; ============================================================
;;; Type Inference Integration
;;; ============================================================

;;; Try to load type inference
(define *infer-available* #f)
(guard (e [else (set! *infer-available* #f)])
       (load "core/types/infer.ss")
       (load "core/types/types.ss")
       (set! *infer-available* #t))

;;; get-type-string : String → String | #f
;;; Get the type of a symbol as a string.
(define (get-type-string name)
  ;; First check the symbol index for signature
  (let ([info (lookup-symbol-info name)])
       (if info
           (let ([sig (assq 'signature info)])
                (if (and sig (cdr sig))
                    (cdr sig)
                    #f))
           #f)))

;;; ============================================================
;;; Hover Implementation
;;; ============================================================

;;; compute-hover : Document × JsonObject → JsonObject | null
;;; Compute hover information for a position.
(define (compute-hover doc position)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           'null
           (let* ([info (lookup-symbol-info symbol)]
                  [type-str (get-type-string symbol)]
                  [hover-text (format-hover-text symbol info type-str)])
                 (if hover-text
                     (make-hover hover-text)
                     'null)))))

;;; format-hover-text : String × (Alist | #f) × (String | #f) → String | #f
;;; Format hover text from symbol info and type.
(define (format-hover-text symbol info type-str)
  (cond
   ;; We have type information
   [type-str
    (let ([doc (and info (assq 'docstring info))])
         (if (and doc (cdr doc))
             (format "```scheme\n~a : ~a\n```\n\n~a" symbol type-str (cdr doc))
             (format "```scheme\n~a : ~a\n```" symbol type-str)))]
   ;; We have index info but no type
   [info
    (let ([kind (assq 'kind info)]
          [doc (assq 'docstring info)])
         (if (and doc (cdr doc))
             (format "```scheme\n~a\n```\n*~a*\n\n~a"
                     symbol
                     (if kind (cdr kind) "symbol")
                     (cdr doc))
             (format "```scheme\n~a\n```" symbol)))]
   ;; Check if it's a primitive
   [(primitive-type symbol)
    => (lambda (ptype)
               (format "```scheme\n~a : ~a\n```\n\n*Primitive*" symbol ptype))]
   ;; Nothing found
   [else #f]))

;;; primitive-type : String → String | #f
;;; Get the type of a primitive operation.
(define (primitive-type name)
  (let ([primitives
         '(("+" . "(Int → Int → Int)")
           ("-" . "(Int → Int → Int)")
           ("*" . "(Int → Int → Int)")
           ("/" . "(Int → Int → Int)")
           ("=" . "(α → α → Bool)")
           ("<" . "(Int → Int → Bool)")
           (">" . "(Int → Int → Bool)")
           ("<=" . "(Int → Int → Bool)")
           (">=" . "(Int → Int → Bool)")
           ("car" . "((Pair α β) → α)")
           ("cdr" . "((Pair α β) → β)")
           ("cons" . "(α → β → (Pair α β))")
           ("null?" . "(α → Bool)")
           ("pair?" . "(α → Bool)")
           ("list" . "(α ... → (List α))")
           ("length" . "((List α) → Int)")
           ("append" . "((List α) → (List α) → (List α))")
           ("map" . "((α → β) → (List α) → (List β))")
           ("filter" . "((α → Bool) → (List α) → (List α))")
           ("fold" . "((β → α → β) → β → (List α) → β)")
           ("string-append" . "(String → String → String)")
           ("string-length" . "(String → Int)")
           ("display" . "(α → Unit)")
           ("newline" . "(Unit → Unit)"))])
       (let ([entry (assoc name primitives)])
            (and entry (cdr entry)))))

;;; ============================================================
;;; Go-to-Definition Implementation
;;; ============================================================

;;; compute-definition : Document × JsonObject → JsonObject | null
;;; Compute definition location for a position.
(define (compute-definition doc position)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           'null
           (let ([info (lookup-symbol-info symbol)])
                (if (not info)
                    'null
                    (let ([file (assq 'file info)]
                          [line (assq 'line info)])
                         (if (and file (cdr file) line (cdr line))
                             (make-location (path->uri (cdr file))
                                            (make-range (make-position (- (cdr line) 1) 0)
                                                        (make-position (- (cdr line) 1) 1)))
                             'null)))))))

;;; ============================================================
;;; Completion Implementation
;;; ============================================================

;;; compute-completions : Document × JsonObject → JsonObject
;;; Compute completions at a position.
(define (compute-completions doc position)
  (let* ([offset (lsp-position->offset doc position)]
         [prefix (completion-prefix-at-offset doc offset)]
         [items (gather-completion-items prefix)])
        (json-obj "isIncomplete" #f
                  "items" (apply json-arr items))))

;;; completion-prefix-at-offset : Document × Int → String
;;; Get the completion prefix at offset.
(define (completion-prefix-at-offset doc offset)
  (let* ([content (document-content doc)]
         [start (find-completion-start content offset)])
        (if (< start offset)
            (substring content start offset)
            "")))

;;; find-completion-start : String × Int → Int
;;; Find where the completion prefix starts.
(define (find-completion-start content offset)
  (let loop ([i (- offset 1)])
       (if (< i 0)
           0
           (let ([c (string-ref content i)])
                (if (completion-char? c)
                    (loop (- i 1))
                    (+ i 1))))))

;;; completion-char? : Char → Boolean
(define (completion-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; gather-completion-items : String → (List JsonObject)
;;; Gather completion items for a prefix.
(define (gather-completion-items prefix)
  (let* ([keyword-items (keyword-completions prefix)]
         [primitive-items (primitive-completions prefix)]
         [index-items (index-completions prefix)])
        (append keyword-items primitive-items index-items)))

;;; keyword-completions : String → (List JsonObject)
(define (keyword-completions prefix)
  (let ([keywords '("define" "define-syntax" "lambda" "let" "let*" "letrec"
                    "if" "cond" "case" "and" "or" "not" "begin" "when" "unless"
                    "quote" "quasiquote" "unquote" "unquote-splicing"
                    "set!" "do" "delay" "force" "parameterize"
                    "guard" "raise" "with-exception-handler"
                    "call/cc" "call-with-current-continuation"
                    "dynamic-wind" "values" "call-with-values"
                    "syntax-rules" "syntax-case" "identifier-syntax")])
       (filter-map (lambda (kw)
                           (if (or (string=? prefix "")
                                   (string-prefix? kw prefix))
                               (make-completion-item kw *completion-keyword* "keyword")
                               #f))
                   keywords)))

;;; primitive-completions : String → (List JsonObject)
(define (primitive-completions prefix)
  (let ([primitives '(("+" . "(Int → Int → Int)")
                      ("-" . "(Int → Int → Int)")
                      ("*" . "(Int → Int → Int)")
                      ("/" . "(Int → Int → Int)")
                      ("car" . "((Pair α β) → α)")
                      ("cdr" . "((Pair α β) → β)")
                      ("cons" . "(α → β → (Pair α β))")
                      ("list" . "(α ... → (List α))")
                      ("null?" . "(α → Bool)")
                      ("pair?" . "(α → Bool)")
                      ("map" . "((α → β) → (List α) → (List β))")
                      ("filter" . "((α → Bool) → (List α) → (List α))")
                      ("fold" . "((β → α → β) → β → (List α) → β)")
                      ("append" . "((List α) → (List α) → (List α))")
                      ("length" . "((List α) → Int)")
                      ("reverse" . "((List α) → (List α))")
                      ("string-append" . "(String ... → String)")
                      ("string-length" . "(String → Int)")
                      ("number->string" . "(Number → String)")
                      ("string->number" . "(String → Number | #f)")
                      ("display" . "(α → Unit)")
                      ("newline" . "(→ Unit)")
                      ("read" . "(→ α)")
                      ("write" . "(α → Unit)")
                      ("equal?" . "(α → α → Bool)")
                      ("eq?" . "(α → α → Bool)")
                      ("eqv?" . "(α → α → Bool)"))])
       (filter-map (lambda (prim)
                           (if (or (string=? prefix "")
                                   (string-prefix? (car prim) prefix))
                               (make-completion-item (car prim) *completion-function*
                                                     (cdr prim))
                               #f))
                   primitives)))

;;; index-completions : String → (List JsonObject)
(define (index-completions prefix)
  (if (< (string-length prefix) 2)
      '()  ; Don't search with very short prefixes
      (let ([matches (find-symbols-matching prefix)])
           (map (lambda (info)
                        (let* ([name (cdr (assq 'name info))]
                               [kind (assq 'kind info)]
                               [sig (assq 'signature info)]
                               [kind-val (if (and kind (eq? (cdr kind) 'syntax))
                                             *completion-keyword*
                                             *completion-function*)])
                              (make-completion-item (symbol->string name)
                                                    kind-val
                                                    (if (and sig (cdr sig))
                                                        (cdr sig)
                                                        #f))))
                (if (> (length matches) 50)
                    (take matches 50)  ; Limit results
                    matches)))))

;;; ============================================================
;;; Document Symbols Implementation
;;; ============================================================

;;; compute-document-symbols : Document → JsonArray
;;; Extract symbols from a document for outline view.
(define (compute-document-symbols doc)
  (let* ([content (document-content doc)]
         [symbols (extract-definitions content)])
        (apply json-arr
               (map (lambda (sym)
                            (definition->document-symbol doc sym))
                    symbols))))

;;; extract-definitions : String → (List (name kind line))
;;; Extract top-level definitions from source.
(define (extract-definitions content)
  (let ([lines (string-split content #\newline)])
       (let loop ([ls lines] [line-num 1] [acc '()])
            (if (null? ls)
                (reverse acc)
                (let ([line (car ls)])
                     (cond
                      [(definition-line? line)
                       (let ([name (extract-definition-name line)])
                            (if name
                                (loop (cdr ls) (+ line-num 1)
                                      (cons (list name 'define line-num) acc))
                                (loop (cdr ls) (+ line-num 1) acc)))]
                      [else
                       (loop (cdr ls) (+ line-num 1) acc)]))))))

;;; definition-line? : String → Boolean
(define (definition-line? line)
  (let ([trimmed (string-trim-left line)])
       (or (string-prefix? trimmed "(define ")
           (string-prefix? trimmed "(define-syntax "))))

;;; extract-definition-name : String → String | #f
(define (extract-definition-name line)
  (let* ([trimmed (string-trim-left line)]
         [prefix-len (cond
                      [(string-prefix? trimmed "(define-syntax ") 15]
                      [(string-prefix? trimmed "(define ") 8]
                      [else 0])])
        (if (= prefix-len 0)
            #f
            (let ([rest (substring trimmed prefix-len (string-length trimmed))])
                 (cond
                  ;; (define (name ...) ...)
                  [(and (> (string-length rest) 0)
                        (char=? (string-ref rest 0) #\())
                   (let ([end (find-name-end rest 1)])
                        (if (and end (> end 1))
                            (substring rest 1 end)
                            #f))]
                  ;; (define name ...)
                  [else
                   (let ([end (find-name-end rest 0)])
                        (if (and end (> end 0))
                            (substring rest 0 end)
                            #f))])))))

;;; find-name-end : String × Int → Int | #f
(define (find-name-end str start)
  (let ([len (string-length str)])
       (let loop ([i start])
            (cond
             [(>= i len) (if (> i start) i #f)]
             [(name-char? (string-ref str i)) (loop (+ i 1))]
             [(> i start) i]
             [else #f]))))

;;; name-char? : Char → Boolean
(define (name-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; definition->document-symbol : Document × (name kind line) → JsonObject
(define (definition->document-symbol doc def)
  (let* ([name (car def)]
         [kind (cadr def)]
         [line (caddr def)]
         [lsp-kind (if (eq? kind 'syntax) 14 12)]  ; 14=Constructor, 12=Function
         [range (make-range (make-position (- line 1) 0)
                            (make-position (- line 1) 1))])
        (json-obj "name" name
                  "kind" lsp-kind
                  "range" range
                  "selectionRange" range)))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; string-trim-left : String → String
(define (string-trim-left str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (if (or (>= i len)
                    (not (char-whitespace? (string-ref str i))))
                (substring str i len)
                (loop (+ i 1))))))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (let ([len (string-length str)])
       (let loop ([i 0] [start 0] [acc '()])
            (cond
             [(>= i len)
              (reverse (cons (substring str start len) acc))]
             [(char=? (string-ref str i) delim)
              (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
             [else
              (loop (+ i 1) start acc)]))))

;;; take : (List α) × Int → (List α)
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; filter-map : (α → β | #f) × (List α) → (List β)
(define (filter-map f lst)
  (let loop ([l lst] [acc '()])
       (if (null? l)
           (reverse acc)
           (let ([result (f (car l))])
                (if result
                    (loop (cdr l) (cons result acc))
                    (loop (cdr l) acc))))))
