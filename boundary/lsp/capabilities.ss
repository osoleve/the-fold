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
;;;   - boundary/tools/index.ss (symbol index)
;;;   - core/types/infer.ss (type inference)
;;;   - boundary/lens/ (navigation)
;;;
;;; This is Core code: pure where possible.

(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/documents.ss")

;;; Try to load pretty printer for formatting support
(define *pretty-available* #f)
(guard (e [else (set! *pretty-available* #f)])
       (load "core/util/pretty.ss")
       (set! *pretty-available* #t))

;;; ====
;;; Symbol Index Integration
;;; ====

;;; Try to load the symbol index
(define *index-available* #f)
(guard (e [else (set! *index-available* #f)])
       (load "boundary/tools/index.ss")
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

;;; ====
;;; Type Inference Integration
;;; ====

;;; Try to load type inference
(define *infer-available* #f)
(guard (e [else (set! *infer-available* #f)])
       (load "core/types/infer.ss")
       (load "core/types/types.ss")
       (set! *infer-available* #t))

;;; ====
;;; Real Type Inference for Hover
;;; ====

;;; try-parse-expr : String → Expr | #f
;;; Try to parse a string as a Scheme expression.
(define (try-parse-expr str)
  (guard (e [else #f])
         (read (open-input-string str))))

;;; parse-definitions : String → (List (Pair Symbol Expr))
;;; Parse top-level definitions from document content.
;;; Returns list of (name . init-expr) pairs for type inference.
(define (parse-definitions content)
  (guard (e [else '()])
         (let ([port (open-input-string content)])
              (let loop ([acc '()])
                   (let ([expr (read port)])
                        (if (eof-object? expr)
                            (reverse acc)
                            (loop (append (extract-def expr) acc))))))))

;;; extract-def : Expr → (List (Pair Symbol Expr))
;;; Extract definition bindings from a single form.
(define (extract-def expr)
  (cond
   ;; (define name value)
   [(and (pair? expr)
         (eq? (car expr) 'define)
         (symbol? (cadr expr)))
    (list (cons (cadr expr) (caddr expr)))]
   ;; (define (name args...) body)
   [(and (pair? expr)
         (eq? (car expr) 'define)
         (pair? (cadr expr)))
    (let ([name (caadr expr)]
          [args (cdadr expr)]
          [body (if (null? (cdddr expr)) (caddr expr) (cons 'begin (cddr expr)))])
         (list (cons name `(fn ,args ,body))))]
   [else '()]))

;;; build-tenv-from-defs : (List (Pair Symbol Expr)) → TEnv
;;; Build a type environment by inferring types for definitions.
;;; Falls back to fresh type variables for failed inferences.
(define (build-tenv-from-defs defs)
  (if (not *infer-available*)
      empty-tenv
      (let loop ([defs defs] [env empty-tenv])
           (if (null? defs)
               env
               (let* ([def (car defs)]
                      [name (car def)]
                      [init (cdr def)])
                     (guard (e [else (loop (cdr defs) env)])
                            (reset-fresh!)
                            (let ([result (infer init env)])
                                 (if (eq? (car result) 'ok)
                                     (let* ([type (cadr result)]
                                            [s (caddr result)]
                                            [gen-type (generalize env (apply-subst s type))])
                                           (loop (cdr defs) (tenv-extend env name gen-type)))
                                     (loop (cdr defs) env)))))))))

;;; try-infer-type : String × String → String | #f
;;; Try to infer the type of a symbol in the context of a document.
;;; Returns the type as a string, or #f if inference fails.
(define (try-infer-type name content)
  (if (not *infer-available*)
      #f
      (guard (e [else #f])
             (let* ([defs (parse-definitions content)]
                    [env (build-tenv-from-defs defs)]
                    [sym (string->symbol name)]
                    ;; First check if it's in the environment
                    [env-type (tenv-lookup env sym)])
                   (if env-type
                       (type->string env-type)
                       ;; Try to parse and infer as an expression
                       (let ([expr (try-parse-expr name)])
                            (if expr
                                (begin
                                 (reset-fresh!)
                                 (let ([result (infer expr env)])
                                      (if (eq? (car result) 'ok)
                                          (let* ([type (cadr result)]
                                                 [s (caddr result)]
                                                 [final-type (apply-subst s type)])
                                                (type->string (generalize '() final-type)))
                                          #f)))
                                #f)))))))

;;; get-type-string : String → String | #f
;;; Get the type of a symbol as a string.
;;; Tries multiple sources: real inference, symbol index, primitives.
(define (get-type-string name)
  (or
   ;; 1. Try the symbol index for pre-computed signature
   (let ([info (lookup-symbol-info name)])
        (and info
             (let ([sig (assq 'signature info)])
                  (and sig (cdr sig)))))
   ;; 2. Check primitive database
   (primitive-type name)))

;;; ====
;;; Hover Implementation
;;; ====

;;; compute-hover : Document × JsonObject → JsonObject | null
;;; Compute hover information for a position.
;;; Uses real type inference when available, falls back to index/primitives.
(define (compute-hover doc position)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           'null
           (let* ([info (lookup-symbol-info symbol)]
                  ;; Try real type inference first, then fall back to index/primitives
                  [type-str (or (and *infer-available*
                                     (try-infer-type symbol (document-content doc)))
                                (get-type-string symbol))]
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

;;; ====
;;; Primitive Type Database (shared between hover and completion)
;;; ====

;;; *primitives* : (Alist String String)
;;; Mapping from primitive names to their type signatures.
;;; This is the single source of truth for primitive types.
(define *primitives*
  '(;; Arithmetic
    ("+" . "(Int → Int → Int)")
    ("-" . "(Int → Int → Int)")
    ("*" . "(Int → Int → Int)")
    ("/" . "(Int → Int → Int)")
    ;; Comparison
    ("=" . "(α → α → Bool)")
    ("<" . "(Int → Int → Bool)")
    (">" . "(Int → Int → Bool)")
    ("<=" . "(Int → Int → Bool)")
    (">=" . "(Int → Int → Bool)")
    ("equal?" . "(α → α → Bool)")
    ("eq?" . "(α → α → Bool)")
    ("eqv?" . "(α → α → Bool)")
    ;; Pairs
    ("car" . "((Pair α β) → α)")
    ("cdr" . "((Pair α β) → β)")
    ("cons" . "(α → β → (Pair α β))")
    ("pair?" . "(α → Bool)")
    ;; Lists
    ("list" . "(α ... → (List α))")
    ("null?" . "(α → Bool)")
    ("length" . "((List α) → Int)")
    ("append" . "((List α) → (List α) → (List α))")
    ("reverse" . "((List α) → (List α))")
    ("map" . "((α → β) → (List α) → (List β))")
    ("filter" . "((α → Bool) → (List α) → (List α))")
    ("fold" . "((β → α → β) → β → (List α) → β)")
    ;; Strings
    ("string-append" . "(String ... → String)")
    ("string-length" . "(String → Int)")
    ("number->string" . "(Number → String)")
    ("string->number" . "(String → Number | #f)")
    ;; I/O
    ("display" . "(α → Unit)")
    ("newline" . "(→ Unit)")
    ("read" . "(→ α)")
    ("write" . "(α → Unit)")))

;;; primitive-type : String → String | #f
;;; Get the type of a primitive operation.
(define (primitive-type name)
  (let ([entry (assoc name *primitives*)])
       (and entry (cdr entry))))

;;; ====
;;; Signature Help Database
;;; ====

;;; *signatures* : (Alist String SignatureData)
;;; Mapping from function names to their signature information.
;;; Each entry is: (name label doc ((param-name . param-doc) ...))
(define *signatures*
  '(;; Higher-order functions
    ("map" "(map f lst)" "Apply function to each element of list."
     (("f" "Function to apply: (α → β)")
      ("lst" "List to map over: (List α)")))
    ("filter" "(filter pred lst)" "Keep elements satisfying predicate."
     (("pred" "Predicate function: (α → Bool)")
      ("lst" "List to filter: (List α)")))
    ("fold" "(fold f init lst)" "Left fold over list."
     (("f" "Combining function: (β → α → β)")
      ("init" "Initial accumulator value: β")
      ("lst" "List to fold: (List α)")))
    ("for-each" "(for-each f lst)" "Apply function to each element for side effects."
     (("f" "Function to apply: (α → Unit)")
      ("lst" "List to iterate: (List α)")))
    ;; List operations
    ("append" "(append lst1 lst2)" "Concatenate two lists."
     (("lst1" "First list: (List α)")
      ("lst2" "Second list: (List α)")))
    ("cons" "(cons head tail)" "Construct a pair."
     (("head" "First element: α")
      ("tail" "Second element (or list): β")))
    ("list-ref" "(list-ref lst idx)" "Get element at index."
     (("lst" "List to index: (List α)")
      ("idx" "Zero-based index: Int")))
    ;; Control flow
    ("if" "(if test then else)" "Conditional expression."
     (("test" "Condition: Bool")
      ("then" "Value if true: α")
      ("else" "Value if false: α")))
    ("cond" "(cond [test expr] ... [else expr])" "Multi-way conditional."
     (("clause" "Test-expression pair: [Bool α]")))
    ("let" "(let ([var val] ...) body)" "Local bindings."
     (("bindings" "Variable bindings: ([Symbol α] ...)")
      ("body" "Body expression: β")))
    ("lambda" "(lambda (params ...) body)" "Anonymous function."
     (("params" "Parameter list: (Symbol ...)")
      ("body" "Function body: α")))
    ("define" "(define name value)" "Define a binding."
     (("name" "Variable name: Symbol")
      ("value" "Value to bind: α")))
    ;; String operations
    ("string-append" "(string-append str ...)" "Concatenate strings."
     (("str" "Strings to concatenate: String ...")))
    ("substring" "(substring str start end)" "Extract substring."
     (("str" "Source string: String")
      ("start" "Start index: Int")
      ("end" "End index: Int")))
    ("string-ref" "(string-ref str idx)" "Get character at index."
     (("str" "Source string: String")
      ("idx" "Zero-based index: Int")))
    ;; I/O
    ("display" "(display obj)" "Write object to output."
     (("obj" "Object to display: α")))
    ("format" "(format fmt args ...)" "Format string with arguments."
     (("fmt" "Format string: String")
      ("args" "Format arguments: α ...")))))

;;; lookup-signature : String → SignatureData | #f
(define (lookup-signature name)
  (assoc name *signatures*))

;;; ====
;;; Signature Help Implementation
;;; ====

;;; compute-signature-help : Document × JsonObject → JsonObject | null
;;; Compute signature help at a position.
(define (compute-signature-help doc position)
  (let* ([offset (lsp-position->offset doc position)]
         [call-info (find-enclosing-call doc offset)])
        (if (not call-info)
            'null
            (let* ([func-name (car call-info)]
                   [param-idx (cdr call-info)]
                   [sig-data (lookup-signature func-name)])
                  (if (not sig-data)
                      'null
                      (let* ([label (cadr sig-data)]
                             [doc (caddr sig-data)]
                             [params (cadddr sig-data)]
                             [param-infos (map (lambda (p)
                                                       (make-parameter-info (car p) (cdr p)))
                                               params)]
                             [sig-info (make-signature-info label doc param-infos)])
                            (make-signature-help (list sig-info) 0 param-idx)))))))

;;; find-enclosing-call : Document × Int → (String . Int) | #f
;;; Find the enclosing function call and which parameter we're in.
;;; Returns (function-name . parameter-index) or #f.
(define (find-enclosing-call doc offset)
  (let* ([content (document-content doc)]
         [len (string-length content)])
        (if (>= offset len)
            #f
            (find-call-backwards content offset))))

;;; find-call-backwards : String × Int → (String . Int) | #f
;;; Scan backwards to find opening paren and extract function name.
(define (find-call-backwards content offset)
  (let loop ([i (- offset 1)]
             [depth 0]
             [param-count 0])
       (cond
        ;; Beginning of string
        [(< i 0) #f]
        ;; Found opening paren at depth 0
        [(and (char=? (string-ref content i) #\()
              (= depth 0))
         ;; Extract function name after the paren
         (let ([name (extract-symbol-at content (+ i 1))])
              (if name
                  (cons name param-count)
                  #f))]
        ;; Nested closing paren
        [(char=? (string-ref content i) #\))
         (loop (- i 1) (+ depth 1) param-count)]
        ;; Nested opening paren
        [(char=? (string-ref content i) #\()
         (loop (- i 1) (- depth 1) param-count)]
        ;; Space at depth 0 counts as parameter separator
        [(and (= depth 0)
              (char-whitespace? (string-ref content i))
              (> i 0)
              (not (char-whitespace? (string-ref content (- i 1)))))
         (loop (- i 1) depth (+ param-count 1))]
        ;; Keep scanning
        [else
         (loop (- i 1) depth param-count)])))

;;; extract-symbol-at : String × Int → String | #f
;;; Extract a symbol starting at the given position.
(define (extract-symbol-at content start)
  (let ([len (string-length content)])
       (if (>= start len)
           #f
           ;; Skip whitespace
           (let skip-ws ([i start])
                (cond
                 [(>= i len) #f]
                 [(char-whitespace? (string-ref content i))
                  (skip-ws (+ i 1))]
                 [(symbol-start-char? (string-ref content i))
                  ;; Found start of symbol
                  (let find-end ([j (+ i 1)])
                       (if (or (>= j len)
                               (not (symbol-char? (string-ref content j))))
                           (substring content i j)
                           (find-end (+ j 1))))]
                 [else #f])))))

;;; symbol-start-char? : Char → Boolean
(define (symbol-start-char? c)
  (or (char-alphabetic? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; ====
;;; Go-to-Definition Implementation
;;; ====

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

;;; ====
;;; Completion Implementation
;;; ====

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

;;; ====
;;; Snippet Templates
;;; ====

;;; *snippet-templates* : (Alist String (snippet-text . doc))
;;; Snippet templates for common Scheme forms.
;;; Snippets use VS Code snippet syntax: ${N:placeholder}
(define *snippet-templates*
  '(;; Definitions
    ("define" . ("(define ${1:name} ${2:value})$0"
                 "Define a variable"))
    ("define-fn" . ("(define (${1:name} ${2:args})\n  ${3:body})$0"
                    "Define a function"))
    ("define-syntax" . ("(define-syntax ${1:name}\n  (syntax-rules ()\n    [(${1:name} ${2:pattern}) ${3:template}]))$0"
                        "Define a macro"))
    ("lambda" . ("(lambda (${1:args}) ${2:body})$0"
                 "Anonymous function"))
    ;; Control flow
    ("if" . ("(if ${1:condition}\n    ${2:then}\n    ${3:else})$0"
             "Conditional expression"))
    ("cond" . ("(cond\n  [${1:test1} ${2:expr1}]\n  [${3:test2} ${4:expr2}]\n  [else ${5:default}])$0"
               "Multi-way conditional"))
    ("when" . ("(when ${1:condition}\n  ${2:body})$0"
               "Execute when true"))
    ("unless" . ("(unless ${1:condition}\n  ${2:body})$0"
                 "Execute when false"))
    ("case" . ("(case ${1:expr}\n  [(${2:val1}) ${3:result1}]\n  [(${4:val2}) ${5:result2}]\n  [else ${6:default}])$0"
               "Pattern matching"))
    ;; Binding forms
    ("let" . ("(let ([${1:var} ${2:val}])\n  ${3:body})$0"
              "Local bindings"))
    ("let*" . ("(let* ([${1:var1} ${2:val1}]\n       [${3:var2} ${4:val2}])\n  ${5:body})$0"
               "Sequential local bindings"))
    ("letrec" . ("(letrec ([${1:name} (lambda (${2:args}) ${3:body})])\n  ${4:expr})$0"
                 "Recursive local bindings"))
    ;; Iteration
    ("do" . ("(do ([${1:var} ${2:init} ${3:step}])\n    ((${4:test}) ${5:result})\n  ${6:body})$0"
             "Iteration construct"))
    ("loop" . ("(let ${1:loop} ([${2:var} ${3:init}])\n  (if ${4:done?}\n      ${5:result}\n      (${1:loop} ${6:next})))$0"
               "Named let loop"))
    ;; Exception handling
    ("guard" . ("(guard (e [else (display e)])\n  ${1:body})$0"
                "Exception handling"))
    ;; Higher-order
    ("map-fn" . ("(map (lambda (${1:x}) ${2:body}) ${3:lst})$0"
                 "Map with lambda"))
    ("filter-fn" . ("(filter (lambda (${1:x}) ${2:pred}) ${3:lst})$0"
                    "Filter with lambda"))
    ("fold-fn" . ("(fold (lambda (${1:acc} ${2:x}) ${3:body}) ${4:init} ${5:lst})$0"
                  "Fold with lambda"))))

;;; make-snippet-item : String × String × String × Int → JsonObject
;;; Create a completion item with snippet support.
;;; insertTextFormat: 1 = PlainText, 2 = Snippet
(define (make-snippet-item label snippet-text doc kind)
  (json-obj "label" label
            "kind" kind
            "detail" doc
            "insertText" snippet-text
            "insertTextFormat" 2  ; Snippet format
            "documentation" (json-obj "kind" "markdown"
                                      "value" (format "```scheme\n~a\n```" snippet-text))))

;;; gather-completion-items : String → (List JsonObject)
;;; Gather completion items for a prefix.
(define (gather-completion-items prefix)
  (let* ([snippet-items (snippet-completions prefix)]
         [keyword-items (keyword-completions prefix)]
         [primitive-items (primitive-completions prefix)]
         [index-items (index-completions prefix)])
        ;; Snippets first, then keywords, primitives, index
        (append snippet-items keyword-items primitive-items index-items)))

;;; snippet-completions : String → (List JsonObject)
;;; Get snippet completions matching prefix.
(define (snippet-completions prefix)
  (filter-map (lambda (template)
                      (let ([name (car template)]
                            [snippet (cadr template)]
                            [doc (cddr template)])
                           (if (or (string=? prefix "")
                                   (string-prefix? name prefix))
                               (make-snippet-item name snippet
                                                  (if (pair? doc) (car doc) "")
                                                  *completion-snippet*)
                               #f)))
              *snippet-templates*))

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
;;; Uses shared *primitives* list for type information.
(define (primitive-completions prefix)
  (filter-map (lambda (prim)
                      (if (or (string=? prefix "")
                              (string-prefix? (car prim) prefix))
                          (make-completion-item (car prim) *completion-function*
                                                (cdr prim))
                          #f))
              *primitives*))

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

;;; ====
;;; Document Symbols Implementation
;;; ====

;;; compute-document-symbols : Document → JsonArray
;;; Extract symbols from a document for outline view.
;;; Returns DocumentSymbol format with nested children.
(define (compute-document-symbols doc)
  (let* ([content (document-content doc)]
         [symbols (extract-definitions-deep content)])
        (apply json-arr
               (map (lambda (sym)
                            (definition->document-symbol-deep doc sym))
                    symbols))))

;;; extract-definitions-deep : String → (List SymbolNode)
;;; Extract all definitions including nested ones.
;;; Each SymbolNode is: (name kind line-start line-end children)
;;; where children is a list of nested SymbolNodes.
(define (extract-definitions-deep content)
  (guard (e [else (extract-definitions content)])  ; Fallback to simple extraction
         (let ([port (open-input-string content)])
              (let loop ([acc '()] [line-num 1])
                   (let ([expr (read-with-position port)])
                        (if (eof-object? (car expr))
                            (reverse acc)
                            (let* ([sexp (car expr)]
                                   [start-line (cdr expr)]
                                   [end-line (count-lines-through port line-num)]
                                   [sym-nodes (extract-symbols-from-sexp sexp start-line end-line)])
                                  (loop (append (reverse sym-nodes) acc)
                                        end-line))))))))

;;; read-with-position : Port → (Sexp . Int)
;;; Read an S-expression and return with its starting line.
(define (read-with-position port)
  (let* ([line (port-line-number port)]
         [expr (read port)])
        (cons expr (if (number? line) line 1))))

;;; count-lines-through : Port × Int → Int
;;; Get current line number in port.
(define (count-lines-through port prev-line)
  (let ([line (port-line-number port)])
       (if (number? line) line prev-line)))

;;; port-line-number : Port → Int | #f
;;; Get the current line number from a port (1-indexed).
(define (port-line-number port)
  (guard (e [else #f])
         (let ([pos (port-position port)])
              ;; This is implementation-dependent
              #f)))

;;; extract-symbols-from-sexp : Sexp × Int × Int → (List SymbolNode)
;;; Extract symbol definitions from an S-expression.
(define (extract-symbols-from-sexp sexp start-line end-line)
  (cond
   [(not (pair? sexp)) '()]
   [(eq? (car sexp) 'define)
    (extract-define-symbol sexp start-line end-line)]
   [(eq? (car sexp) 'define-syntax)
    (extract-define-syntax-symbol sexp start-line end-line)]
   [else '()]))

;;; extract-define-symbol : Sexp × Int × Int → (List SymbolNode)
;;; Extract symbol from a define form.
(define (extract-define-symbol sexp start-line end-line)
  (if (< (length sexp) 2)
      '()
      (let ([name-part (cadr sexp)])
           (cond
            ;; (define (name args...) body...)
            [(pair? name-part)
             (let* ([name (symbol->string (car name-part))]
                    [body (cddr sexp)]
                    [children (extract-nested-symbols body (+ start-line 1))])
                   (list (list name 'function start-line end-line children)))]
            ;; (define name value)
            [(symbol? name-part)
             (let* ([name (symbol->string name-part)]
                    [value (if (pair? (cddr sexp)) (caddr sexp) #f)]
                    [kind (if (and (pair? value) (eq? (car value) 'lambda))
                              'function
                              'variable)]
                    [children (if (pair? value)
                                  (extract-nested-symbols (list value) (+ start-line 1))
                                  '())])
                   (list (list name kind start-line end-line children)))]
            [else '()]))))

;;; extract-define-syntax-symbol : Sexp × Int × Int → (List SymbolNode)
(define (extract-define-syntax-symbol sexp start-line end-line)
  (if (< (length sexp) 2)
      '()
      (let ([name-part (cadr sexp)])
           (if (symbol? name-part)
               (list (list (symbol->string name-part) 'syntax start-line end-line '()))
               '()))))

;;; extract-nested-symbols : (List Sexp) × Int → (List SymbolNode)
;;; Extract nested definitions from body forms.
(define (extract-nested-symbols body start-line)
  (apply append
         (map (lambda (expr)
                      (extract-nested-from-expr expr start-line))
              body)))

;;; extract-nested-from-expr : Sexp × Int → (List SymbolNode)
(define (extract-nested-from-expr expr start-line)
  (cond
   [(not (pair? expr)) '()]
   ;; Nested define
   [(eq? (car expr) 'define)
    (extract-define-symbol expr start-line (+ start-line 1))]
   ;; let/let*/letrec bindings
   [(memq (car expr) '(let let* letrec))
    (extract-let-symbols expr start-line)]
   ;; Recurse into other forms
   [(pair? (car expr))
    (apply append (map (lambda (e) (extract-nested-from-expr e start-line)) expr))]
   [else '()]))

;;; extract-let-symbols : Sexp × Int → (List SymbolNode)
;;; Extract symbols from let bindings.
(define (extract-let-symbols expr start-line)
  (if (< (length expr) 3)
      '()
      (let ([bindings (cadr expr)])
           (if (and (pair? bindings) (pair? (car bindings)))
               ;; ((var val) ...) form
               (filter-map (lambda (binding)
                                   (if (and (pair? binding) (symbol? (car binding)))
                                       (list (symbol->string (car binding))
                                             'variable start-line (+ start-line 1) '())
                                       #f))
                           bindings)
               ;; Named let: (let name ((var val) ...) body)
               (if (symbol? bindings)
                   (cons (list (symbol->string bindings) 'function start-line (+ start-line 1) '())
                         (if (and (pair? (cddr expr)) (pair? (caddr expr)))
                             (filter-map (lambda (binding)
                                                 (if (and (pair? binding) (symbol? (car binding)))
                                                     (list (symbol->string (car binding))
                                                           'variable start-line (+ start-line 1) '())
                                                     #f))
                                         (caddr expr))
                             '()))
                   '())))))

;;; definition->document-symbol-deep : Document × SymbolNode → JsonObject
;;; Convert a SymbolNode to a DocumentSymbol JSON object.
(define (definition->document-symbol-deep doc sym)
  (let* ([name (car sym)]
         [kind (cadr sym)]
         [start-line (caddr sym)]
         [end-line (cadddr sym)]
         [children (if (pair? (cddddr sym)) (car (cddddr sym)) '())]
         [lsp-kind (symbol-kind->lsp-kind kind)]
         [range (make-range (make-position (- start-line 1) 0)
                            (make-position (- end-line 1) 100))]
         [sel-range (make-range (make-position (- start-line 1) 0)
                                (make-position (- start-line 1) (string-length name)))])
        (if (null? children)
            (json-obj "name" name
                      "kind" lsp-kind
                      "range" range
                      "selectionRange" sel-range)
            (json-obj "name" name
                      "kind" lsp-kind
                      "range" range
                      "selectionRange" sel-range
                      "children" (apply json-arr
                                        (map (lambda (c)
                                                     (definition->document-symbol-deep doc c))
                                             children))))))

;;; extract-definitions : String → (List (name kind line))
;;; Extract top-level definitions from source (fallback, simpler version).
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

;;; ====
;;; Utilities
;;; ====

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

;;; ====
;;; Find References Implementation
;;; ====

;;; compute-references : Document × JsonObject × Boolean → JsonArray
;;; Find all references to the symbol at position.
;;; include-declaration: if true, include the definition itself
(define (compute-references doc position include-declaration)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           (json-arr)
           (let ([refs (find-all-references symbol)])
                (apply json-arr refs)))))

;;; find-all-references : String → (List JsonObject)
;;; Find all occurrences of symbol across all open documents.
(define (find-all-references symbol)
  (let ([uris (doc-list)])
       (apply append
              (map (lambda (uri)
                           (find-references-in-document uri symbol))
                   uris))))

;;; find-references-in-document : String × String → (List JsonObject)
;;; Find all occurrences of symbol in a document.
(define (find-references-in-document uri symbol)
  (let ([doc (doc-get uri)])
       (if (not doc)
           '()
           (let* ([content (document-content doc)]
                  [positions (find-symbol-positions content symbol)])
                 (map (lambda (pos)
                              (let ([lsp-pos (offset->lsp-position doc pos)])
                                   (make-location uri
                                                  (make-range lsp-pos
                                                              (offset->lsp-position doc
                                                                                    (+ pos (string-length symbol)))))))
                      positions)))))

;;; find-symbol-positions : String × String → (List Int)
;;; Find all positions where symbol appears as a complete identifier.
(define (find-symbol-positions content symbol)
  (let ([sym-len (string-length symbol)]
        [content-len (string-length content)])
       (let loop ([i 0] [acc '()])
            (if (> (+ i sym-len) content-len)
                (reverse acc)
                (let* ([found (find-next-match content symbol i)]
                       [pos (if found found content-len)])
                      (if (>= pos content-len)
                          (reverse acc)
                          ;; Check if it's a complete symbol (not part of larger word)
                          (if (complete-symbol-match? content pos sym-len)
                              (loop (+ pos sym-len) (cons pos acc))
                              (loop (+ pos 1) acc))))))))

;;; find-next-match : String × String × Int → Int | #f
;;; Find next occurrence of pattern starting from index.
(define (find-next-match content pattern start)
  (let ([pattern-len (string-length pattern)]
        [content-len (string-length content)])
       (let loop ([i start])
            (cond
             [(> (+ i pattern-len) content-len) #f]
             [(string-match-at? content pattern i) i]
             [else (loop (+ i 1))]))))

;;; string-match-at? : String × String × Int → Boolean
;;; Check if pattern matches content at position.
(define (string-match-at? content pattern pos)
  (let ([pattern-len (string-length pattern)])
       (let loop ([i 0])
            (cond
             [(>= i pattern-len) #t]
             [(char=? (string-ref content (+ pos i))
                      (string-ref pattern i))
              (loop (+ i 1))]
             [else #f]))))

;;; complete-symbol-match? : String × Int × Int → Boolean
;;; Check if match at position is a complete symbol (not part of larger word).
(define (complete-symbol-match? content pos sym-len)
  (let ([content-len (string-length content)]
        [end-pos (+ pos sym-len)])
       (and ;; Check char before isn't a symbol char (or at start)
            (or (= pos 0)
                (not (symbol-char? (string-ref content (- pos 1)))))
            ;; Check char after isn't a symbol char (or at end)
            (or (>= end-pos content-len)
                (not (symbol-char? (string-ref content end-pos)))))))

;;; ====
;;; Workspace Symbol Implementation
;;; ====

;;; compute-workspace-symbols : String → JsonArray
;;; Search for symbols across the index (preferred) or open documents.
(define (compute-workspace-symbols query)
  ;; Try index first (covers entire codebase)
  (let ([index-results (find-symbols-matching query)])
    (if (pair? index-results)
        ;; Convert index results to LSP SymbolInformation
        (let ([symbols (filter-map
                        (lambda (entry)
                          (let ([name (cdr (assq 'name entry))]
                                [file (cdr (assq 'file entry))]
                                [line (cdr (assq 'line entry))]
                                [kind (cdr (assq 'kind entry))])
                            (and name file line
                                 (make-symbol-information
                                  (symbol->string name)
                                  (symbol-kind->lsp-kind kind)
                                  (path->uri file)
                                  line))))
                        index-results)])
          (apply json-arr (if (> (length symbols) 100)
                              (take symbols 100)
                              symbols)))
        ;; Fallback to open documents only
        (let* ([uris (doc-list)]
               [all-symbols (apply append
                                   (map (lambda (uri)
                                          (document-symbols-for-workspace uri query))
                                        uris))])
          (apply json-arr (if (> (length all-symbols) 100)
                              (take all-symbols 100)
                              all-symbols))))))

;;; document-symbols-for-workspace : String × String → (List JsonObject)
;;; Extract symbols from document that match query.
(define (document-symbols-for-workspace uri query)
  (let ([doc (doc-get uri)])
       (if (not doc)
           '()
           (let* ([content (document-content doc)]
                  [symbols (extract-definitions content)]
                  [query-lower (string-downcase query)])
                 (filter-map (lambda (sym)
                                     (let ([name (car sym)]
                                           [kind (cadr sym)]
                                           [line (caddr sym)])
                                          (if (or (string=? query "")
                                                  (string-contains-ci? name query-lower))
                                              (make-symbol-information name
                                                                       (symbol-kind->lsp-kind kind)
                                                                       uri
                                                                       line)
                                              #f)))
                             symbols)))))

;;; make-symbol-information : String × Int × String × Int → JsonObject
;;; Create a SymbolInformation for workspace/symbol response.
(define (make-symbol-information name kind uri line)
  (json-obj "name" name
            "kind" kind
            "location" (make-location uri
                                      (make-range (make-position (- line 1) 0)
                                                  (make-position (- line 1) 1)))))

;;; symbol-kind->lsp-kind : Symbol → Int
;;; Convert internal symbol kind to LSP SymbolKind.
(define (symbol-kind->lsp-kind kind)
  (case kind
        [(define function) 12]   ; Function
        [(syntax) 14]            ; Constructor (for macros)
        [(variable) 13]          ; Variable
        [else 12]))              ; Default to Function

;;; string-contains-ci? : String × String → Boolean
;;; Case-insensitive substring search.
(define (string-contains-ci? str pattern)
  (let* ([str-lower (string-downcase str)]
         [str-len (string-length str-lower)]
         [pat-len (string-length pattern)])
        (let loop ([i 0])
             (cond
              [(> (+ i pat-len) str-len) #f]
              [(string-match-at? str-lower pattern i) #t]
              [else (loop (+ i 1))]))))

;;; string-downcase : String → String
;;; Convert string to lowercase.
(define (string-downcase str)
  (let* ([len (string-length str)]
         [chars (let loop ([i 0] [acc '()])
                     (if (>= i len)
                         (reverse acc)
                         (loop (+ i 1)
                               (cons (char-downcase (string-ref str i)) acc))))])
        (list->string chars)))

;;; ====
;;; Document Formatting Implementation
;;; ====

;;; compute-formatting : Document × JsonObject → JsonArray
;;; Format the document according to options.
;;; Returns an array of TextEdit objects.
(define (compute-formatting doc options)
  (if (not *pretty-available*)
      (json-arr)  ; No formatting available
      (let* ([content (document-content doc)]
             [tab-size (or (and options (json-get options "tabSize")) 2)]
             [formatted (format-scheme-code content tab-size)])
            (if formatted
                (json-arr (make-text-edit
                           (make-range (make-position 0 0)
                                       (end-of-document doc))
                           formatted))
                (json-arr)))))

;;; end-of-document : Document → Position
;;; Get the position at the end of the document.
(define (end-of-document doc)
  (let* ([lines (line-count doc)]
         [last-line (- lines 1)]
         [last-content (get-line-content doc last-line)])
        (make-position last-line (string-length last-content))))

;;; make-text-edit : Range × String → JsonObject
;;; Create a TextEdit object.
(define (make-text-edit range new-text)
  (json-obj "range" range
            "newText" new-text))

;;; format-scheme-code : String × Int → String | #f
;;; Format Scheme code. Returns formatted string or #f on error.
(define (format-scheme-code content tab-size)
  (guard (e [else #f])
         (let* ([exprs (read-all-sexps content)]
                [docs (map (lambda (expr)
                                   (scheme-expr->doc expr tab-size))
                           exprs)]
                ;; Join with double newlines between top-level forms
                [combined (join-docs-with-blanks docs)])
               (pretty 80 combined))))

;;; read-all-sexps : String → (List Sexp)
;;; Read all S-expressions from a string.
(define (read-all-sexps str)
  (let ([port (open-input-string str)])
       (let loop ([acc '()])
            (let ([expr (read port)])
                 (if (eof-object? expr)
                     (reverse acc)
                     (loop (cons expr acc)))))))

;;; scheme-expr->doc : Sexp × Int → Doc
;;; Convert a Scheme expression to a pretty-printer document.
(define (scheme-expr->doc expr indent-size)
  (cond
   [(null? expr) (text "()")]
   [(pair? expr)
    (let ([head (car expr)])
         (cond
          ;; Special forms with body indentation
          [(and (symbol? head) (memq head '(define define-syntax lambda let let* letrec
                                            if cond case when unless guard)))
           (format-special-form expr indent-size)]
          ;; Regular application
          [else
           (parens (group (nest indent-size
                                (sep (map (lambda (e) (scheme-expr->doc e indent-size))
                                          expr)))))]))]
   [(symbol? expr) (text (symbol->string expr))]
   [(number? expr) (text (number->string expr))]
   [(string? expr) (text (format "~s" expr))]
   [(boolean? expr) (text (if expr "#t" "#f"))]
   [(char? expr) (text (format "~s" expr))]
   [(vector? expr)
    (<> (text "#")
        (parens (group (sep (map (lambda (e) (scheme-expr->doc e indent-size))
                                 (vector->list expr))))))]
   [else (text (format "~a" expr))]))

;;; format-special-form : Sexp × Int → Doc
;;; Format special forms like define, let, lambda.
(define (format-special-form expr indent-size)
  (let ([head (car expr)]
        [rest (cdr expr)])
       (cond
        ;; (define (name args...) body...)
        [(and (eq? head 'define)
              (pair? rest)
              (pair? (car rest)))
         (parens (<> (text "define ")
                     (<> (parens (sep (map (lambda (e) (scheme-expr->doc e indent-size))
                                           (car rest))))
                         (nest indent-size
                               (<> line
                                   (vsep (map (lambda (e) (scheme-expr->doc e indent-size))
                                              (cdr rest))))))))]
        ;; (define name value)
        [(eq? head 'define)
         (parens (group (<> (text "define ")
                            (sep (map (lambda (e) (scheme-expr->doc e indent-size))
                                      rest)))))]
        ;; (lambda (args...) body...)
        [(eq? head 'lambda)
         (if (and (pair? rest) (pair? (car rest)))
             (parens (<> (text "lambda ")
                         (<> (parens (sep (map (lambda (e) (scheme-expr->doc e indent-size))
                                               (car rest))))
                             (nest indent-size
                                   (<> line
                                       (vsep (map (lambda (e) (scheme-expr->doc e indent-size))
                                                  (cdr rest))))))))
             (parens (sep (cons (text "lambda")
                                (map (lambda (e) (scheme-expr->doc e indent-size))
                                     rest)))))]
        ;; (let ([bindings...]) body...)
        [(memq head '(let let* letrec))
         (if (and (pair? rest)
                  (pair? (car rest)))
             (parens (<> (text (symbol->string head))
                         (<> (text " ")
                             (<> (parens (group
                                          (nest 1 (vsep (map (lambda (b)
                                                                     (parens (sep (map (lambda (e)
                                                                                               (scheme-expr->doc e indent-size))
                                                                                       b))))
                                                             (car rest))))))
                                 (nest indent-size
                                       (<> line
                                           (vsep (map (lambda (e) (scheme-expr->doc e indent-size))
                                                      (cdr rest)))))))))
             (parens (sep (cons (text (symbol->string head))
                                (map (lambda (e) (scheme-expr->doc e indent-size))
                                     rest)))))]
        ;; Other special forms - standard nesting
        [else
         (parens (group (<> (text (symbol->string head))
                            (nest indent-size
                                  (<> line
                                      (vsep (map (lambda (e) (scheme-expr->doc e indent-size))
                                                 rest)))))))])))

;;; join-docs-with-blanks : (List Doc) → Doc
;;; Join documents with blank lines between them.
(define (join-docs-with-blanks docs)
  (if (null? docs)
      empty
      (let loop ([ds (cdr docs)] [acc (car docs)])
           (if (null? ds)
               acc
               (loop (cdr ds)
                     (<> acc (<> hardline (<> hardline (car ds)))))))))

;;; ====
;;; Rename Implementation
;;; ====

;;; compute-rename : Document × JsonObject × String → JsonObject | null
;;; Rename all occurrences of symbol at position to new-name.
;;; Returns a WorkspaceEdit with changes grouped by document.
(define (compute-rename doc position new-name)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           'null
           (let ([edits-by-uri (compute-rename-edits symbol new-name)])
                (if (null? edits-by-uri)
                    'null
                    (json-obj "changes" (make-changes-object edits-by-uri)))))))

;;; compute-rename-edits : String × String → (Alist String (List TextEdit))
;;; Compute all text edits needed for renaming, grouped by URI.
(define (compute-rename-edits old-name new-name)
  (let ([uris (doc-list)])
       (filter (lambda (pair) (pair? (cdr pair)))
               (map (lambda (uri)
                            (cons uri (compute-rename-edits-in-doc uri old-name new-name)))
                    uris))))

;;; compute-rename-edits-in-doc : String × String × String → (List TextEdit)
;;; Compute text edits for renaming in a single document.
(define (compute-rename-edits-in-doc uri old-name new-name)
  (let ([doc (doc-get uri)])
       (if (not doc)
           '()
           (let* ([content (document-content doc)]
                  [positions (find-symbol-positions content old-name)]
                  [old-len (string-length old-name)])
                 (map (lambda (pos)
                              (let* ([start-pos (offset->lsp-position doc pos)]
                                     [end-pos (offset->lsp-position doc (+ pos old-len))])
                                    (make-text-edit (make-range start-pos end-pos)
                                                    new-name)))
                      positions)))))

;;; make-changes-object : (Alist String (List TextEdit)) → JsonObject
;;; Convert edits-by-uri to a JSON object for WorkspaceEdit.changes.
(define (make-changes-object edits-by-uri)
  (apply json-obj
         (apply append
                (map (lambda (pair)
                             (list (car pair) (apply json-arr (cdr pair))))
                     edits-by-uri))))

;;; ====
;;; Code Actions Implementation
;;; ====

;;; compute-code-actions : Document × String × Range × Context → JsonArray
;;; Compute code actions for a range and context.
;;; Context contains diagnostics that triggered the request.
(define (compute-code-actions doc uri range context)
  (let* ([diagnostics (if context
                          (json-get context "diagnostics")
                          #f)]
         [diag-actions (if (and diagnostics (json-array? diagnostics))
                           (diagnostic-code-actions doc uri (cdr diagnostics))
                           '())]
         [refactor-actions (refactoring-code-actions doc uri range)])
        (apply json-arr (append diag-actions refactor-actions))))

;;; diagnostic-code-actions : Document × String × (List Diagnostic) → (List CodeAction)
;;; Generate quick fix actions for diagnostics.
(define (diagnostic-code-actions doc uri diagnostics)
  (apply append
         (map (lambda (diag)
                      (diagnostic-to-actions doc uri diag))
              diagnostics)))

;;; diagnostic-to-actions : Document × String × Diagnostic → (List CodeAction)
;;; Convert a diagnostic to code actions.
(define (diagnostic-to-actions doc uri diag)
  (let* ([message (json-get diag "message")]
         [range (json-get diag "range")]
         [code (json-get diag "code")])
        (cond
         ;; Unbalanced parentheses
         [(and message (string-contains? message "unbalanced"))
          (list (make-code-action
                 "Add missing closing parenthesis"
                 "quickfix"
                 uri
                 range
                 ")"
                 (list diag)))]
         ;; Undefined variable - offer to create definition
         [(and message (string-contains? message "undefined"))
          (let ([var-name (extract-undefined-name message)])
               (if var-name
                   (list (make-code-action
                          (format "Define '~a'" var-name)
                          "quickfix"
                          uri
                          (make-range (make-position 0 0) (make-position 0 0))
                          (format "(define ~a #f)\n\n" var-name)
                          (list diag)))
                   '()))]
         ;; Unknown diagnostic - no actions
         [else '()])))

;;; extract-undefined-name : String → String | #f
;;; Extract variable name from "undefined variable: foo" message.
(define (extract-undefined-name message)
  (let* ([prefix "undefined variable: "]
         [idx (string-contains-idx message prefix)])
        (if idx
            (let* ([start (+ idx (string-length prefix))]
                   [rest (substring message start (string-length message))])
                  (let loop ([i 0])
                       (if (or (>= i (string-length rest))
                               (char-whitespace? (string-ref rest i)))
                           (if (> i 0)
                               (substring rest 0 i)
                               #f)
                           (loop (+ i 1)))))
            #f)))

;;; string-contains-idx : String × String → Int | #f
;;; Find index of substring, or #f if not found.
(define (string-contains-idx str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sub-len) str-len) #f]
             [(string-match-at? str substr i) i]
             [else (loop (+ i 1))]))))

;;; string-contains? : String × String → Boolean
(define (string-contains? str substr)
  (if (string-contains-idx str substr) #t #f))

;;; refactoring-code-actions : Document × String × Range → (List CodeAction)
;;; Generate refactoring actions for selected code.
(define (refactoring-code-actions doc uri range)
  (let* ([start-pos (json-get range "start")]
         [end-pos (json-get range "end")]
         [start-offset (lsp-position->offset doc start-pos)]
         [end-offset (lsp-position->offset doc end-pos)]
         [content (document-content doc)]
         [selected (if (and start-offset end-offset (< start-offset end-offset))
                       (substring content start-offset end-offset)
                       "")])
        (cond
         ;; Non-empty selection - offer extract function
         [(and (> (string-length selected) 5)
               (not (string=? (string-trim-left selected) "")))
          (list (make-code-action
                 "Extract to function"
                 "refactor.extract"
                 uri
                 range
                 (format "(define (extracted-fn)\n  ~a)" selected)
                 '()))]
         [else '()])))

;;; make-code-action : String × String × String × Range × String × (List Diag) → JsonObject
;;; Create a CodeAction object.
;;; kind: "quickfix" | "refactor" | "refactor.extract" | etc.
(define (make-code-action title kind uri range new-text diagnostics)
  (let* ([edit (json-obj "changes"
                         (json-obj uri
                                   (json-arr (make-text-edit range new-text))))]
         [base (json-obj "title" title
                         "kind" kind
                         "edit" edit)])
        (if (null? diagnostics)
            base
            (cons 'json-object
                  (cons (cons "diagnostics" (apply json-arr diagnostics))
                        (cdr base))))))

;;; ====
;;; Semantic Tokens Implementation
;;; ====

;;; Token type indices (must match server capabilities)
(define *token-keyword* 0)
(define *token-function* 1)
(define *token-variable* 2)
(define *token-string* 3)
(define *token-number* 4)
(define *token-comment* 5)
(define *token-operator* 6)
(define *token-macro* 7)
(define *token-parameter* 8)
(define *token-type* 9)

;;; Token modifier bits
(define *mod-definition* 1)
(define *mod-declaration* 2)
(define *mod-readonly* 4)

;;; *scheme-keywords* : (List String)
;;; Keywords that get special token type.
(define *scheme-keywords*
  '("define" "define-syntax" "lambda" "let" "let*" "letrec" "letrec*"
    "if" "cond" "case" "when" "unless" "begin" "and" "or" "not"
    "set!" "do" "delay" "force" "quote" "quasiquote" "unquote"
    "syntax-rules" "syntax-case" "guard" "parameterize"
    "library" "import" "export" "module"))

;;; *scheme-operators* : (List String)
;;; Operators that get special token type.
(define *scheme-operators*
  '("+" "-" "*" "/" "=" "<" ">" "<=" ">=" "eq?" "eqv?" "equal?"
    "car" "cdr" "cons" "null?" "pair?" "list?" "length" "append"
    "map" "filter" "fold" "for-each" "apply" "call/cc"))

;;; compute-semantic-tokens : Document → JsonObject
;;; Compute semantic tokens for a document.
;;; Returns {data: [deltaLine, deltaStart, length, tokenType, modifiers, ...]}
(define (compute-semantic-tokens doc)
  (let* ([content (document-content doc)]
         [tokens (tokenize-scheme content)]
         [encoded (encode-tokens tokens)])
        (json-obj "data" (apply json-arr encoded))))

;;; tokenize-scheme : String → (List (line char length type modifiers))
;;; Tokenize Scheme source code.
(define (tokenize-scheme content)
  (let ([len (string-length content)])
       (let loop ([i 0] [line 0] [col 0] [acc '()])
            (if (>= i len)
                (reverse acc)
                (let ([c (string-ref content i)])
                     (cond
                      ;; Newline
                      [(char=? c #\newline)
                       (loop (+ i 1) (+ line 1) 0 acc)]
                      ;; Whitespace
                      [(char-whitespace? c)
                       (loop (+ i 1) line (+ col 1) acc)]
                      ;; Comment
                      [(char=? c #\;)
                       (let ([end (find-line-end content i)])
                            (loop end (+ line 1) 0
                                  (cons (list line col (- end i) *token-comment* 0) acc)))]
                      ;; String
                      [(char=? c #\")
                       (let ([end (find-string-end content (+ i 1))])
                            (if end
                                (let ([tok-len (- end i)])
                                     (loop end line (+ col tok-len)
                                           (cons (list line col tok-len *token-string* 0) acc)))
                                (loop (+ i 1) line (+ col 1) acc)))]
                      ;; Number
                      [(or (char-numeric? c)
                           (and (char=? c #\-)
                                (< (+ i 1) len)
                                (char-numeric? (string-ref content (+ i 1)))))
                       (let ([end (find-number-end content i)])
                            (let ([tok-len (- end i)])
                                 (loop end line (+ col tok-len)
                                       (cons (list line col tok-len *token-number* 0) acc))))]
                      ;; Symbol/keyword
                      [(symbol-start-char? c)
                       (let* ([end (find-symbol-end content i)]
                              [tok-len (- end i)]
                              [sym (substring content i end)]
                              [tok-info (classify-symbol sym)])
                             (loop end line (+ col tok-len)
                                   (cons (list line col tok-len (car tok-info) (cdr tok-info)) acc)))]
                      ;; Skip other characters (parens, etc.)
                      [else
                       (loop (+ i 1) line (+ col 1) acc)]))))))

;;; find-line-end : String × Int → Int
;;; Find end of line (or end of string).
(define (find-line-end content start)
  (let ([len (string-length content)])
       (let loop ([i start])
            (if (or (>= i len) (char=? (string-ref content i) #\newline))
                i
                (loop (+ i 1))))))

;;; find-string-end : String × Int → Int | #f
;;; Find end of string literal (closing quote).
(define (find-string-end content start)
  (let ([len (string-length content)])
       (let loop ([i start] [escaped #f])
            (cond
             [(>= i len) #f]
             [escaped (loop (+ i 1) #f)]
             [(char=? (string-ref content i) #\\) (loop (+ i 1) #t)]
             [(char=? (string-ref content i) #\") (+ i 1)]
             [else (loop (+ i 1) #f)]))))

;;; find-number-end : String × Int → Int
;;; Find end of numeric literal.
(define (find-number-end content start)
  (let ([len (string-length content)])
       (let loop ([i start])
            (if (or (>= i len)
                    (not (or (char-numeric? (string-ref content i))
                             (char=? (string-ref content i) #\.)
                             (char=? (string-ref content i) #\e)
                             (char=? (string-ref content i) #\E)
                             (char=? (string-ref content i) #\+)
                             (char=? (string-ref content i) #\-)
                             (char=? (string-ref content i) #\/))))
                i
                (loop (+ i 1))))))

;;; find-symbol-end : String × Int → Int
;;; Find end of symbol.
(define (find-symbol-end content start)
  (let ([len (string-length content)])
       (let loop ([i start])
            (if (or (>= i len)
                    (not (symbol-char? (string-ref content i))))
                i
                (loop (+ i 1))))))

;;; classify-symbol : String → (type . modifiers)
;;; Classify a symbol and return token type and modifiers.
(define (classify-symbol sym)
  (cond
   [(member sym *scheme-keywords*)
    (cons *token-keyword* 0)]
   [(member sym *scheme-operators*)
    (cons *token-operator* 0)]
   [(string-suffix? sym "?")
    (cons *token-function* 0)]  ; Predicates
   [(string-suffix? sym "!")
    (cons *token-function* 0)]  ; Mutators
   [(string-prefix? sym "*")
    (cons *token-variable* *mod-readonly*)]  ; Global constant
   [(char-upper-case? (string-ref sym 0))
    (cons *token-type* 0)]  ; Type-like
   [else
    (cons *token-variable* 0)]))

;;; string-suffix? : String × String → Boolean
(define (string-suffix? str suffix)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (>= str-len suf-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

;;; encode-tokens : (List Token) → (List Int)
;;; Encode tokens into delta format.
;;; Each token becomes 5 integers: deltaLine, deltaStartChar, length, tokenType, modifiers
;;; BUGFIX: Use cons instead of append to avoid O(N^2) complexity
(define (encode-tokens tokens)
  (let loop ([toks tokens] [prev-line 0] [prev-col 0] [acc '()])
       (if (null? toks)
           (reverse acc)
           (let* ([tok (car toks)]
                  [line (car tok)]
                  [col (cadr tok)]
                  [len (caddr tok)]
                  [type (cadddr tok)]
                  [mods (car (cddddr tok))]
                  [delta-line (- line prev-line)]
                  [delta-col (if (= delta-line 0) (- col prev-col) col)])
                 (loop (cdr toks)
                       line col
                       ;; Use multiple cons calls - O(1) instead of O(N) append
                       (cons mods (cons type (cons len (cons delta-col (cons delta-line acc))))))))))
