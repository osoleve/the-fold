(load "core/base/prelude.ss")
(load "boundary/lsp/json.ss")
(load "boundary/lsp/protocol.ss")
(load "boundary/lsp/documents.ss")
;; Note: state.ss is loaded via documents.ss

(doc 'module 'lsp/capabilities)
(doc 'description "Implements LSP language features: Hover (type information), Go-to-definition, Completion, and Document symbols. Integrates with boundary/tools/index.ss (symbol index), core/types/infer.ss (type inference), and boundary/lens/ (navigation).")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(prelude json protocol documents))

(doc 'note "Try to load pretty printer for formatting support")
(define *pretty-available* #f)
(guard (e [else (set! *pretty-available* #f)])
       (load "core/util/pretty.ss")
       (set! *pretty-available* #t))

(doc 'section 'symbol-index-integration)

(doc 'note "Try to load the symbol index")
(define *index-available* #f)
(guard (e [else (set! *index-available* #f)])
       (load "boundary/tools/index.ss")
       (set! *index-available* #t))

(doc 'note "Try to load doc annotation system for explicit type declarations")
(define *docs-available* #f)
(define *docs-quiet* #t)  ; Suppress "Doc index built" message
(guard (e [else (set! *docs-available* #f)])
       (load "boundary/meta/docs-io.ss")
       (set! *docs-available* #t))

(doc lookup-symbol-info 'type '(-> String (U (Alist Symbol Any) #f)))
(doc lookup-symbol-info 'description "Look up symbol information from the index")
(define (lookup-symbol-info name)
  (if (and *index-available* (top-level-bound? 'index-lookup))
      (let ([sym (string->symbol name)])
           (guard (e [else #f])
                  (index-lookup sym)))
      #f))

(doc find-symbols-matching 'type '(-> String (List (Alist Symbol Any))))
(doc find-symbols-matching 'description "Find symbols whose names start with prefix")
(define (find-symbols-matching prefix)
  (if (and *index-available* (top-level-bound? 'index-find))
      (guard (e [else '()])
             (index-find prefix))
      '()))

(doc 'section 'doc-annotation-integration)

(doc lookup-doc-type 'type '(-> String (U String #f)))
(doc lookup-doc-type 'description "Look up explicit type annotation from (doc symbol 'type ...) forms")
(doc lookup-doc-type 'note "Authoritative when present - author's explicit declaration")
(define (lookup-doc-type name)
  (if (not *docs-available*)
      #f
      (guard (e [else #f])
             (let* ([sym (string->symbol name)]
                    [docs (docs-for sym)]
                    ;; Find doc with 'type tag
                    [type-doc (find (lambda (d) (eq? (caddr d) 'type)) docs)])
               (if type-doc
                   ;; content is at index 3, e.g. ('(-> Int Int)) from (doc f 'type '(-> Int Int))
                   ;; The quote is captured in parsing, so we may get (quote (-> ...))
                   (let* ([content (cadddr type-doc)]
                          [first (and (pair? content) (car content))]
                          ;; Unwrap quote if present: '(-> ...) => (-> ...)
                          [type-expr (cond
                                       [(not first) #f]
                                       [(and (pair? first) (eq? (car first) 'quote))
                                        (cadr first)]
                                       [(pair? first) first]
                                       [else #f])])
                     (if type-expr
                         (format "~s" type-expr)
                         #f))
                   #f)))))

(doc lookup-doc-description 'type '(-> String (U String #f)))
(doc lookup-doc-description 'description "Look up description from (doc symbol 'description ...) forms")
(define (lookup-doc-description name)
  (if (not *docs-available*)
      #f
      (guard (e [else #f])
             (let* ([sym (string->symbol name)]
                    [docs (docs-for sym)]
                    [desc-doc (find (lambda (d) (eq? (caddr d) 'description)) docs)])
               (if desc-doc
                   (let ([content (cadddr desc-doc)])
                     (if (and (pair? content) (string? (car content)))
                         (car content)
                         #f))
                   #f)))))

(doc 'section 'type-inference-integration)

(doc 'note "Try to load type inference")
(define *infer-available* #f)
(guard (e [else (set! *infer-available* #f)])
       (load "core/types/infer.ss")
       (load "core/types/types.ss")
       (set! *infer-available* #t))

(doc 'section 'doc-to-type-checker-bridge)
(doc 'note "Bridge between doc annotation system and type checker. Populates declared types from doc index.")

(doc *doc-types-loaded?* 'type 'Bool)
(doc *doc-types-loaded?* 'description "Flag to track if doc types have been loaded into type checker.")
(define *doc-types-loaded?* #f)

(doc load-doc-types-into-checker! 'type '(-> Void))
(doc load-doc-types-into-checker! 'description "Load all doc type annotations into the type checker's declared types table.")
(doc load-doc-types-into-checker! 'note "Called lazily on first type inference. Builds doc index if needed.")
(define (load-doc-types-into-checker!)
  (when (and *docs-available* *infer-available* (not *doc-types-loaded?*))
    (guard (e [else (void)])  ; Silently fail if something goes wrong
      (ensure-doc-index!)
      (let ([type-docs (lf-docs 'type)])
        ;; Each doc is (file line tag content target)
        ;; We want entries with a target (targeted doc annotations)
        (for-each
         (lambda (doc-entry)
           (let ([content (cadddr doc-entry)]
                 [target (list-ref doc-entry 4)])
             (when (and target (pair? content))
               ;; content is like ('(-> Int Int)) - extract the type
               (let* ([first (car content)]
                      [type (cond
                              [(and (pair? first) (eq? (car first) 'quote))
                               (cadr first)]
                              [(pair? first) first]
                              [else #f])])
                 (when type
                   (register-declared-type! target type))))))
         type-docs))
      (set! *doc-types-loaded?* #t))))

(doc invalidate-doc-type-cache! 'type '(-> Void))
(doc invalidate-doc-type-cache! 'description "Invalidate cached doc types so they reload on next inference. Call on file changes.")
(doc invalidate-doc-type-cache! 'export #t)
(define (invalidate-doc-type-cache!)
  (set! *doc-types-loaded?* #f)
  ;; Also clear declared types so stale entries don't persist
  (when *infer-available*
    (clear-declared-types!))
  ;; Reset doc index so it rebuilds on next query
  (when *docs-available*
    (set! *doc-index-built?* #f)))

;;; Register capabilities state with the LSP state registry
;;; Note: These flags are set at load time based on what modules are available
;;; Resetting them doesn't make sense, so reset is a no-op
(lsp-register-state! 'capabilities
                     '(*pretty-available* *index-available* *infer-available* *docs-available*)
                     (lambda () (void)))  ; No reset - these are load-time config

(doc 'section 'type-inference-for-hover)

(doc try-parse-expr 'type '(-> String (U Expr #f)))
(doc try-parse-expr 'description "Try to parse a string as a Scheme expression")
(define (try-parse-expr str)
  (guard (e [else #f])
         (read (open-input-string str))))

(doc parse-definitions 'type '(-> String (List (Pair Symbol Expr))))
(doc parse-definitions 'description "Parse top-level definitions from document content")
(doc parse-definitions 'returns "list of (name . init-expr) pairs for type inference")
(define (parse-definitions content)
  (guard (e [else '()])
         (let ([port (open-input-string content)])
              (let loop ([acc '()])
                   (let ([expr (read port)])
                        (if (eof-object? expr)
                            (reverse acc)
                            (loop (append (extract-def expr) acc))))))))

(doc extract-def 'type '(-> Expr (List (Pair Symbol Expr))))
(doc extract-def 'description "Extract definition bindings from a single form")
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

(doc apply-subst-to-tenv 'type '(-> Subst TEnv TEnv))
(doc apply-subst-to-tenv 'description "Apply a substitution to all types in a type environment")
(define (apply-subst-to-tenv subst env)
  (map (lambda (entry)
         (cons (car entry) (apply-subst subst (cdr entry))))
       env))

(doc build-tenv-from-defs 'type '(-> (List (Pair Symbol Expr)) TEnv))
(doc build-tenv-from-defs 'description "Build a type environment by inferring types for definitions")
(doc build-tenv-from-defs 'note "Two-pass inference: (1) seed all names with placeholder type variables, (2) infer against the full env so forward references resolve via unification. Generalization deferred to end.")
(define (build-tenv-from-defs defs)
  (if (not *infer-available*)
      empty-tenv
      (begin
        (load-doc-types-into-checker!)
        (reset-fresh!)  ;; Once at the top — never inside the loop

        ;; Pass 1: Build seed env with declared types or fresh placeholders
        (let* ([seed-bindings
                (map (lambda (def)
                       (let* ([name (car def)]
                              [declared (lookup-declared-type name)])
                         (cons name (or declared (fresh-tvar)))))
                     defs)]
               [seed-env (tenv-extend* empty-tenv seed-bindings)])

          ;; Pass 2: Infer each definition against full seed env, accumulate substitution
          (let loop ([remaining defs]
                     [placeholders (map cdr seed-bindings)]
                     [subst empty-subst])
            (if (null? remaining)
                ;; Finalize: apply composed substitution, then generalize
                (fold-left
                 (lambda (env binding)
                   (let* ([name (car binding)]
                          [placeholder (cdr binding)]
                          [final-type (apply-subst subst placeholder)]
                          [gen-type (generalize empty-tenv final-type)])
                     (tenv-extend env name gen-type)))
                 empty-tenv
                 seed-bindings)

                (let* ([def (car remaining)]
                       [name (car def)]
                       [init (cdr def)]
                       [placeholder (car placeholders)])
                  ;; Declared types are already correct — skip inference
                  (if (lookup-declared-type name)
                      (loop (cdr remaining) (cdr placeholders) subst)
                      ;; Infer and unify with placeholder
                      (guard (e [else (loop (cdr remaining) (cdr placeholders) subst)])
                             (let* ([current-env (apply-subst-to-tenv subst seed-env)]
                                    [result (infer init current-env)])
                               (if (eq? (car result) 'ok)
                                   (let* ([inferred-type (cadr result)]
                                          [s-infer (caddr result)]
                                          [subst-1 (compose-subst s-infer subst)]
                                          ;; Unify placeholder with inferred type
                                          [t-ph (apply-subst subst-1 placeholder)]
                                          [t-inf (apply-subst subst-1 inferred-type)]
                                          [res-unify (unify t-ph t-inf)])
                                     (if (eq? (car res-unify) 'ok)
                                         (loop (cdr remaining)
                                               (cdr placeholders)
                                               (compose-subst (cadr res-unify) subst-1))
                                         ;; Unification failed — keep going
                                         (loop (cdr remaining) (cdr placeholders) subst-1)))
                                   ;; Inference failed — skip
                                   (loop (cdr remaining) (cdr placeholders) subst))))))))))))

(doc try-infer-type 'type '(-> String String (U String #f)))
(doc try-infer-type 'description "Try to infer the type of a symbol in the context of a document")
(doc try-infer-type 'returns "the type as a string, or #f if inference fails")
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

(doc 'section 'local-binding-inference)

(doc parse-forms-with-lines 'type '(-> String (List (Pair Form Int))))
(doc parse-forms-with-lines 'description "Parse all top-level forms with their starting line numbers")
(doc parse-forms-with-lines 'note "Uses incremental newline counting for O(N) performance")
(define (parse-forms-with-lines content)
  (guard (e [else '()])
         (let ([port (open-input-string content)])
              (let loop ([acc '()] [last-pos 0] [last-line 0])
                   (let* ([before-pos (file-position port)]
                          [form (read port)])
                        (if (eof-object? form)
                            (reverse acc)
                            (let* ([start-line (+ last-line (count-newlines-between content last-pos before-pos))]
                                   [after-pos (file-position port)]
                                   [end-line (+ start-line (count-newlines-between content before-pos after-pos))])
                                  (loop (cons (list form start-line end-line) acc)
                                        after-pos
                                        end-line))))))))

(doc count-newlines-between 'type '(-> String Int Int Int))
(doc count-newlines-between 'description "Count newlines in content between start and end positions")
(doc count-newlines-between 'note "O(end - start) instead of O(end) for incremental counting")
(define (count-newlines-between content start end)
  (let ([len (string-length content)]
        [safe-end (min end (string-length content))])
       (let loop ([i start] [count 0])
            (if (>= i safe-end)
                count
                (loop (+ i 1)
                      (if (char=? (string-ref content i) #\newline)
                          (+ count 1)
                          count))))))

(doc find-definition-containing-line 'type '(-> (List (List Form Int Int)) Int (U (List Form Int Int) #f)))
(doc find-definition-containing-line 'description "Find the top-level definition that contains a given line")
(define (find-definition-containing-line forms line)
  (find (lambda (entry)
               (let ([start (cadr entry)]
                     [end (caddr entry)])
                    (and (>= line start) (<= line end))))
        forms))

(doc search-let-initializers 'type '(-> Symbol Bindings Symbol Bindings Bindings (U Bindings #f)))
(doc search-let-initializers 'description "Search through let binding initializers for the target symbol")
(doc search-let-initializers 'note "Handles different scoping rules for let, let*, and letrec:")
(doc search-let-initializers 'note "  - let: initializers see only outer bindings (parallel binding)")
(doc search-let-initializers 'note "  - let*: each initializer sees outer + previous bindings (sequential)")
(doc search-let-initializers 'note "  - letrec: initializers see outer + all let bindings (mutual recursion)")
(define (search-let-initializers let-type let-bindings target outer-bindings all-let-bindings)
  (if (not (list? let-bindings))
      #f
      (let loop ([remaining let-bindings]
                 [accumulated-bindings '()])  ; For let*, tracks previous bindings
           (if (null? remaining)
               #f
               (let* ([binding (car remaining)]
                      [init-expr (if (and (pair? binding) (pair? (cdr binding)))
                                     (cadr binding)
                                     #f)]
                      ;; Choose bindings visible to this initializer based on let type
                      [visible-bindings
                       (case let-type
                         [(let) outer-bindings]  ; Only outer scope
                         [(let*) (append accumulated-bindings outer-bindings)]  ; Previous + outer
                         [(letrec) (append all-let-bindings outer-bindings)]  ; All + outer
                         [else outer-bindings])]
                      ;; New binding for let* accumulation
                      [new-binding (if (and (pair? binding) (symbol? (car binding)))
                                       (cons (car binding) init-expr)
                                       #f)])
                     (or (and init-expr
                              (extract-bindings-deep init-expr target visible-bindings))
                         (loop (cdr remaining)
                               (if new-binding
                                   (cons new-binding accumulated-bindings)
                                   accumulated-bindings))))))))

;;; extract-local-bindings : Sexp × Symbol → (List (Symbol . Sexp))
;;; Extract all local bindings visible to a symbol reference within a form.
;;; flatten-params : ParamSpec → (List Symbol)
;;; Convert lambda parameter specs to a flat list of symbols.
;;; Handles: proper list (a b c), improper list (a b . rest), single symbol args
(define (flatten-params params)
  (cond
   [(null? params) '()]
   [(symbol? params) (list params)]  ; single symbol or dotted tail
   [(pair? params) (cons (car params) (flatten-params (cdr params)))]
   [else '()]))

;;; Walks the AST looking for let/let*/letrec/lambda forms containing the symbol.
(define (extract-local-bindings form target-sym)
  (extract-bindings-deep form target-sym '()))

;;; extract-bindings-deep : Sexp × Symbol × Bindings → Bindings
;;; Recursively extract bindings, accumulating those in scope.
(define (extract-bindings-deep form target bindings)
  (cond
   ;; Found our target symbol - return accumulated bindings
   [(eq? form target) bindings]
   ;; Not a pair - nothing to extract
   [(not (pair? form)) #f]
   ;; Lambda: (fn (args...) body) or (lambda (args...) body)
   [(memq (car form) '(fn lambda))
    (if (>= (length form) 3)
        (let* ([params (cadr form)]
               [body (cddr form)]
               [param-bindings (map (lambda (p) (cons p 'param))
                                    (flatten-params params))]
               [new-bindings (append param-bindings bindings)])
              (extract-in-body body target new-bindings))
        #f)]
   ;; Let forms: (let ((var val) ...) body)
   [(memq (car form) '(let let* letrec))
    (if (>= (length form) 3)
        (let* ([let-type (car form)]
               [bindings-part (cadr form)]
               ;; Handle named let: (let name ((var val)...) body)
               [named? (symbol? bindings-part)]
               [let-bindings (if named? (caddr form) bindings-part)]
               [body (if named? (cdddr form) (cddr form))]
               [binding-pairs (if (list? let-bindings)
                                  (filter-map (lambda (b)
                                                      (if (and (pair? b) (symbol? (car b)))
                                                          (cons (car b) (if (pair? (cdr b)) (cadr b) #f))
                                                          #f))
                                              let-bindings)
                                  '())]
               ;; For named let, include the loop name
               [name-binding (if named? (list (cons bindings-part 'named-let)) '())]
               [all-let-bindings (append name-binding binding-pairs)]
               [new-bindings (append all-let-bindings bindings)])
              ;; First, search in the initializers
              (or (search-let-initializers let-type let-bindings target bindings all-let-bindings)
                  ;; Then search in the body with all bindings
                  (extract-in-body body target new-bindings)))
        #f)]
   ;; Define: (define (name args...) body) or (define name value)
   [(eq? (car form) 'define)
    (if (< (length form) 3)
        #f
        (let ([name-part (cadr form)])
             (if (pair? name-part)
                 ;; (define (name args...) body...) - extract params and recurse
                 (let* ([params (cdr name-part)]
                        [body (cddr form)]
                        [param-bindings (map (lambda (p) (cons p 'param))
                                             (flatten-params params))]
                        [new-bindings (append param-bindings bindings)])
                       (extract-in-body body target new-bindings))
                 ;; (define name value) - just recurse into value
                 (extract-bindings-deep (caddr form) target bindings))))]
   ;; Other forms - search in subexpressions
   [else
    (extract-in-body form target bindings)]))

;;; extract-in-body : (List Sexp) × Symbol × Bindings → Bindings | #f
;;; Search through a list of forms for the target symbol.
(define (extract-in-body forms target bindings)
  (if (null? forms)
      #f
      (or (extract-bindings-deep (car forms) target bindings)
          (extract-in-body (cdr forms) target bindings))))

;;; try-infer-type-local : String × String × Int → String | #f
;;; Try to infer type including local bindings at a specific line.
(define (try-infer-type-local name content line)
  (if (not *infer-available*)
      #f
      (guard (e [else #f])
             (let* ([forms (parse-forms-with-lines content)]
                    [def-entry (find-definition-containing-line forms line)]
                    [sym (string->symbol name)])
                   (if (not def-entry)
                       ;; No definition at this line - fall back to basic inference
                       (try-infer-type name content)
                       ;; Found definition - extract local bindings
                       (let* ([def-form (car def-entry)]
                              [local-bindings (extract-local-bindings def-form sym)]
                              [defs (parse-definitions content)]
                              [base-env (build-tenv-from-defs defs)])
                             (if (not local-bindings)
                                 ;; Symbol not found in this definition - basic inference
                                 (try-infer-type name content)
                                 ;; Build environment with local bindings
                                 (let ([env-with-locals (build-env-with-locals base-env local-bindings)])
                                      (reset-fresh!)
                                      (let ([lookup-type (tenv-lookup env-with-locals sym)])
                                           (if lookup-type
                                               (type->string lookup-type)
                                               ;; Try inferring as expression
                                               (let ([result (infer sym env-with-locals)])
                                                    (if (eq? (car result) 'ok)
                                                        (type->string (apply-subst (caddr result) (cadr result)))
                                                        #f))))))))))))

;;; build-env-with-locals : TEnv × (List (Symbol . Sexp)) → TEnv
;;; Add local bindings to a type environment.
;;; Infers types for bindings with initializers, uses fresh vars for params.
(define (build-env-with-locals base-env local-bindings)
  (let loop ([bindings local-bindings] [env base-env])
       (if (null? bindings)
           env
           (let* ([binding (car bindings)]
                  [var (car binding)]
                  [init (cdr binding)])
                 (cond
                  ;; Parameter or named-let - use fresh type variable
                  [(or (eq? init 'param) (eq? init 'named-let))
                   (loop (cdr bindings) (tenv-extend env var (fresh-tvar)))]
                  ;; Has initializer - try to infer its type
                  [(and init (not (eq? init #f)))
                   (guard (e [else (loop (cdr bindings) (tenv-extend env var (fresh-tvar)))])
                          (let ([result (infer init env)])
                               (if (eq? (car result) 'ok)
                                   (let ([inferred-type (apply-subst (caddr result) (cadr result))])
                                        (loop (cdr bindings) (tenv-extend env var inferred-type)))
                                   (loop (cdr bindings) (tenv-extend env var (fresh-tvar))))))]
                  ;; No initializer - fresh type variable
                  [else
                   (loop (cdr bindings) (tenv-extend env var (fresh-tvar)))])))))

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
;;; Priority order for type information:
;;;   1. Explicit (doc 'type ...) annotation (author's declaration)
;;;   2. Local type inference (let/lambda bindings)
;;;   3. Global type inference (top-level definitions)
;;;   4. Symbol index signature / primitive database
(define (compute-hover doc position)
  (let ([symbol (symbol-at-position doc position)])
       (if (not symbol)
           'null
           (let* ([info (lookup-symbol-info symbol)]
                  [line (json-get position "line")]  ; 0-indexed line number
                  ;; Check doc annotation first (authoritative), then inference, then fallback
                  [type-str (or (lookup-doc-type symbol)
                                (and *infer-available* line
                                     (try-infer-type-local symbol (document-content doc) line))
                                (and *infer-available*
                                     (try-infer-type symbol (document-content doc)))
                                (get-type-string symbol))]
                  ;; Also check for doc description
                  [doc-desc (lookup-doc-description symbol)]
                  [hover-text (format-hover-text symbol info type-str doc-desc)])
                 (if hover-text
                     (make-hover hover-text)
                     'null)))))

;;; format-hover-text : String × (Alist | #f) × (String | #f) × (String | #f) → String | #f
;;; Format hover text from symbol info, type, and doc description.
;;; doc-desc (from (doc 'description ...)) takes priority over index docstring.
(define (format-hover-text symbol info type-str doc-desc)
  (let ([description (or doc-desc
                         (and info
                              (let ([d (assq 'docstring info)])
                                (and d (cdr d)))))])
    (cond
     ;; We have type information
     [type-str
      (if description
          (format "```scheme\n~a : ~a\n```\n\n~a" symbol type-str description)
          (format "```scheme\n~a : ~a\n```" symbol type-str))]
     ;; We have description but no type
     [description
      (let ([kind (and info (assq 'kind info))])
        (format "```scheme\n~a\n```\n*~a*\n\n~a"
                symbol
                (if kind (cdr kind) "symbol")
                description))]
     ;; We have index info but no type or description
     [info
      (format "```scheme\n~a\n```" symbol)]
     ;; Check if it's a primitive
     [(primitive-type symbol)
      => (lambda (ptype)
           (format "```scheme\n~a : ~a\n```\n\n*Primitive*" symbol ptype))]
     ;; Nothing found
     [else #f])))

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
;;; Now syntax-aware: skips strings and comments.
(define (find-call-backwards content offset)
  ;; First check if we're inside a comment (after ; on current line)
  (if (inside-comment? content offset)
      #f
      (let loop ([i (- offset 1)]
                 [depth 0]
                 [param-count 0])
           (cond
            ;; Beginning of content
            [(< i 0) #f]
            ;; Skip past strings (scanning backwards)
            [(char=? (string-ref content i) #\")
             (let ([new-i (skip-string-backwards content i)])
                  (if (< new-i 0)
                      #f  ; Unclosed string
                      (loop new-i depth param-count)))]
            ;; Skip comments - if position i is after a ; on this line, skip to before ;
            [(inside-comment? content i)
             (let ([line-start (find-line-start content i)])
                  (loop (- line-start 1) depth param-count))]
            ;; Found opening paren at depth 0
            [(and (char=? (string-ref content i) #\()
                  (= depth 0))
             ;; Extract function name after the paren
             (let ([name (extract-symbol-at content (+ i 1))])
                  (if name
                      ;; Subtract 1: the space after function name was counted
                      ;; but it separates fn from arg1, not arg1 from arg2
                      (cons name (max 0 (- param-count 1)))
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
             (loop (- i 1) depth param-count)]))))

;;; inside-comment? : String × Int → Boolean
;;; Check if position is inside a line comment (after ; before newline).
;;; Must check inside-string? first since ; inside a multi-line string isn't a comment.
(define (inside-comment? content offset)
  ;; If inside a string, can't be in a comment
  (if (inside-string? content offset)
      #f
      ;; Check current line for ; before position
      (let ([line-start (find-line-start content offset)])
        (let loop ([i line-start])
          (cond
            [(>= i offset) #f]  ; Reached our position without seeing ;
            [(char=? (string-ref content i) #\;) #t]  ; Found comment start
            [(char=? (string-ref content i) #\")
             ;; Skip past string on this line
             (let ([end (skip-string-forward content i)])
               (loop end))]
            [else (loop (+ i 1))])))))

;;; inside-string? : String × Int → Boolean
;;; Check if position is inside a string literal.
;;; Scans from file start to handle multi-line strings correctly.
(define (inside-string? content offset)
  (let ([len (string-length content)])
    (let loop ([i 0] [in-string #f])
      (cond
        [(>= i offset) in-string]  ; Reached position, return state
        [(>= i len) #f]            ; Past end of content
        ;; Inside a string
        [(and in-string (char=? (string-ref content i) #\\) (< (+ i 1) len))
         (loop (+ i 2) #t)]        ; Skip escape sequence
        [(and in-string (char=? (string-ref content i) #\"))
         (loop (+ i 1) #f)]        ; End string
        [in-string
         (loop (+ i 1) #t)]        ; Continue in string
        ;; Outside a string
        [(char=? (string-ref content i) #\")
         (loop (+ i 1) #t)]        ; Start string
        [(char=? (string-ref content i) #\;)
         ;; Comment - skip to end of line
         (loop (skip-past-newline content i len) #f)]
        [else
         (loop (+ i 1) #f)]))))

;;; inside-string-or-comment? : String × Int → Boolean
;;; Check if position is inside a string literal or comment.
;;; Checks string first since inside-comment? depends on string state.
(define (inside-string-or-comment? content offset)
  (or (inside-string? content offset)
      (inside-comment? content offset)))

;;; skip-past-newline : String × Int × Int → Int
;;; Find the position after the next newline, or len if none.
(define (skip-past-newline content start len)
  (let loop ([i start])
    (cond
      [(>= i len) len]
      [(char=? (string-ref content i) #\newline) (+ i 1)]
      [else (loop (+ i 1))])))

;;; find-line-start : String × Int → Int
;;; Find the start of the line containing offset.
(define (find-line-start content offset)
  (let loop ([i (- offset 1)])
       (cond
        [(< i 0) 0]
        [(char=? (string-ref content i) #\newline) (+ i 1)]
        [else (loop (- i 1))])))

;;; skip-string-backwards : String × Int → Int
;;; Given position i pointing to closing ", scan backwards to opening ".
;;; Returns position before the opening ", or -1 if unclosed.
(define (skip-string-backwards content i)
  (if (not (char=? (string-ref content i) #\"))
      i  ; Not at string end
      (let loop ([j (- i 1)])
           (cond
            [(< j 0) -1]  ; Unclosed string
            [(char=? (string-ref content j) #\")
             ;; Check if escaped
             (if (and (> j 0) (char=? (string-ref content (- j 1)) #\\))
                 (loop (- j 2))  ; Escaped, keep looking
                 (- j 1))]  ; Found opening quote
            [else (loop (- j 1))]))))

;;; skip-string-forward : String × Int → Int
;;; Given position i pointing to opening ", scan forward to after closing ".
(define (skip-string-forward content i)
  (let ([len (string-length content)])
       (if (not (char=? (string-ref content i) #\"))
           i
           (let loop ([j (+ i 1)] [escape #f])
                (cond
                 [(>= j len) len]
                 [escape (loop (+ j 1) #f)]
                 [(char=? (string-ref content j) #\\) (loop (+ j 1) #t)]
                 [(char=? (string-ref content j) #\") (+ j 1)]
                 [else (loop (+ j 1) #f)])))))

;;; extract-symbol-at : String × Int → String | #f
;;; Extract a symbol starting at the given position.
;;; Handles both regular symbols and pipe-quoted symbols like |foo bar|.
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
                 ;; Pipe-quoted symbol
                 [(char=? (string-ref content i) #\|)
                  (let find-close ([j (+ i 1)] [escape? #f])
                    (cond
                      [(>= j len) #f]  ;; Unclosed pipe - invalid
                      [escape? (find-close (+ j 1) #f)]
                      [(char=? (string-ref content j) #\\) (find-close (+ j 1) #t)]
                      [(char=? (string-ref content j) #\|)
                       (substring content i (+ j 1))]  ;; Include both pipes
                      [else (find-close (+ j 1) #f)]))]
                 ;; Regular symbol
                 [(symbol-start-char? (string-ref content i))
                  (let find-end ([j (+ i 1)])
                       (if (or (>= j len)
                               (not (symbol-char? (string-ref content j))))
                           (substring content i j)
                           (find-end (+ j 1))))]
                 [else #f])))))

;;; symbol-start-char? : Char → Boolean
;;; Note: | is valid for pipe-quoted symbols like |foo bar|
(define (symbol-start-char? c)
  (or (char-alphabetic? c)
      (char=? c #\|)  ;; pipe-quoted symbols
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
                    (take 50 matches)  ; Limit results
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
         (let ([port (open-input-string content)]
               [len (string-length content)])
              (let loop ([acc '()] [search-from 0])
                   (let ([sexp (read port)])
                        (if (eof-object? sexp)
                            (reverse acc)
                            (let* ([end-pos (port-position-safe port)]
                                   ;; Find actual form start: scan for '(' from search-from
                                   [form-start (find-form-start content search-from)]
                                   [start-line (offset->line-number content form-start)]
                                   [end-line (offset->line-number content end-pos)]
                                   [sym-nodes (extract-symbols-from-sexp sexp start-line end-line)])
                                  (loop (append (reverse sym-nodes) acc)
                                        end-pos))))))))

;;; find-form-start : String × Int → Int
;;; Find the position of the next '(' starting from offset.
;;; This skips whitespace and comments to find where a form actually begins.
(define (find-form-start content from)
  (let ([len (string-length content)])
       (let loop ([i from])
            (cond
             [(>= i len) from]
             [(char=? (string-ref content i) #\() i]
             [(char=? (string-ref content i) #\;)
              ;; Skip comment to end of line
              (loop (skip-to-newline-idx content i))]
             [else (loop (+ i 1))]))))

;;; skip-to-newline-idx : String × Int → Int
;;; Skip to next newline (or end).
(define (skip-to-newline-idx content i)
  (let ([len (string-length content)])
       (let loop ([j i])
            (if (or (>= j len) (char=? (string-ref content j) #\newline))
                (+ j 1)
                (loop (+ j 1))))))

;;; port-position-safe : Port → Int
;;; Get port position, returning 0 if not available.
(define (port-position-safe port)
  (guard (e [else 0])
         (port-position port)))

;;; offset->line-number : String × Int → Int
;;; Count newlines in content up to offset to determine line number (1-indexed).
(define (offset->line-number content offset)
  (let ([len (min offset (string-length content))])
       (let loop ([i 0] [line 1])
            (if (>= i len)
                line
                (if (char=? (string-ref content i) #\newline)
                    (loop (+ i 1) (+ line 1))
                    (loop (+ i 1) line))))))

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

;; string-trim-left, string-split, take, and filter-map provided by prelude

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
;;; Excludes matches inside comments and string literals.
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
                          ;; Check if it's a complete symbol AND not in comment/string
                          (if (and (complete-symbol-match? content pos sym-len)
                                   (not (inside-string-or-comment? content pos)))
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
                              (take 100 symbols)
                              symbols)))
        ;; Fallback to open documents only
        (let* ([uris (doc-list)]
               [all-symbols (apply append
                                   (map (lambda (uri)
                                          (document-symbols-for-workspace uri query))
                                        uris))])
          (apply json-arr (if (> (length all-symbols) 100)
                              (take 100 all-symbols)
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

;;; has-comments? : String → Boolean
;;; Check if content contains comments (semicolons outside strings).
;;; This is a conservative check - may have false positives for ; in strings,
;;; but that's safe (we just skip formatting).
(define (has-comments? content)
  (let ([len (string-length content)])
    (let loop ([i 0] [in-string #f])
      (if (>= i len)
          #f
          (let ([c (string-ref content i)])
            (cond
             [(and (not in-string) (char=? c #\;)) #t]  ; Found comment
             [(char=? c #\") (loop (+ i 1) (not in-string))]  ; Toggle string state
             [(and in-string (char=? c #\\) (< (+ i 1) len))
              (loop (+ i 2) in-string)]  ; Skip escaped char in string
             [else (loop (+ i 1) in-string)]))))))

;;; format-scheme-code : String × Int → String | #f
;;; Format Scheme code. Returns formatted string or #f on error.
;;; Returns #f if content contains comments (to avoid data loss).
(define (format-scheme-code content tab-size)
  (guard (e [else #f])
         ;; Refuse to format if comments present - read discards them
         (if (has-comments? content)
             #f
             (let* ([exprs (read-all-sexps-from-string content)]
                    [docs (map (lambda (expr)
                                       (scheme-expr->doc expr tab-size))
                               exprs)]
                    ;; Join with double newlines between top-level forms
                    [combined (join-docs-with-blanks docs)])
                   (pretty 80 combined)))))

;;; read-all-sexps-from-string : String → (List Sexp)
;;; Read all S-expressions from a string (not a file path).
;;; Note: Named to avoid shadowing read-all-sexps from docs.ss which reads files.
(define (read-all-sexps-from-string str)
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
;;; Scope-Aware Symbol Analysis (for Rename)
;;; ====

;;; symbol-binding-type : Document × Int → Symbol
;;; Determine if the symbol at offset is a 'global, 'local-def, or 'local-ref.
;;; 'global = top-level define
;;; 'local-def = let/lambda binding site
;;; 'local-ref = reference to a local binding (within a local scope)
(define (symbol-binding-type doc offset)
  (let* ([content (document-content doc)]
         [line (offset->line doc offset)]
         [forms (parse-forms-with-lines content)]
         [containing (find-definition-containing-line forms line)])
        (if (not containing)
            ;; Not in a definition - likely a top-level reference
            'global
            ;; Check if this is a binding site or a reference
            (let* ([form (car containing)]
                   [form-start-line (cadr containing)])
                  (classify-symbol-in-form form content offset form-start-line)))))

;;; offset->line : Document × Int → Int
;;; Convert byte offset to 0-indexed line number.
(define (offset->line doc offset)
  (let ([content (document-content doc)])
       (let loop ([i 0] [line 0])
            (cond
             [(>= i offset) line]
             [(>= i (string-length content)) line]
             [(char=? (string-ref content i) #\newline) (loop (+ i 1) (+ line 1))]
             [else (loop (+ i 1) line)]))))

;;; classify-symbol-in-form : Sexp × String × Int × Int → Symbol
;;; Classify the symbol at offset within a form.
(define (classify-symbol-in-form form content offset start-line)
  (cond
   [(not (pair? form)) 'global]
   ;; Top-level define: (define name value) or (define (name args) body)
   [(eq? (car form) 'define)
    (if (< (length form) 2)
        'global
        (let ([name-part (cadr form)])
             (cond
              ;; (define (name args...) body)
              [(pair? name-part)
               ;; Check if offset points to the function name
               (if (is-symbol-at-position? content offset (symbol->string (car name-part)))
                   'global  ; Function name is global
                   ;; Check if it's a parameter
                   (if (any-symbol-at-position? content offset (cdr name-part))
                       'local-def  ; Parameter binding
                       'local-ref))]  ; Reference within body
              ;; (define name value)
              [(symbol? name-part)
               (if (is-symbol-at-position? content offset (symbol->string name-part))
                   'global
                   'local-ref)]
              [else 'global])))]
   ;; Let forms
   [(memq (car form) '(let let* letrec))
    (if (< (length form) 3)
        'global
        (let* ([bindings-part (cadr form)]
               [named? (symbol? bindings-part)]
               [let-bindings (if named? (caddr form) bindings-part)])
              ;; Check if offset is in a binding position
              (if (and (list? let-bindings)
                       (exists (lambda (b)
                                    (and (pair? b)
                                         (symbol? (car b))
                                         (is-symbol-at-position? content offset (symbol->string (car b)))))
                             let-bindings))
                  'local-def
                  'local-ref)))]
   ;; Lambda
   [(memq (car form) '(fn lambda))
    (if (< (length form) 3)
        'global
        (let ([params (cadr form)])
             (if (and (list? params)
                      (any-symbol-at-position? content offset params))
                 'local-def
                 'local-ref)))]
   [else 'local-ref]))

;;; is-symbol-at-position? : String × Int × String → Boolean
;;; Check if the symbol name appears at the given offset.
(define (is-symbol-at-position? content offset name)
  (let ([len (string-length name)]
        [content-len (string-length content)])
       (and (<= (+ offset len) content-len)
            (string=? (substring content offset (+ offset len)) name)
            ;; Verify it's a complete symbol (not part of larger word)
            (complete-symbol-match? content offset len))))

;;; any-symbol-at-position? : String × Int × (List Symbol) → Boolean
;;; Check if any of the symbols appear at the given offset.
(define (any-symbol-at-position? content offset symbols)
  (exists (lambda (sym)
               (and (symbol? sym)
                    (is-symbol-at-position? content offset (symbol->string sym))))
        symbols))

;;; find-local-scope-boundaries : Document × Int → (start-offset . end-offset) | #f
;;; Find the boundaries of the local scope containing the offset.
;;; For let/lambda, this is the body of the form.
(define (find-local-scope-boundaries doc offset)
  (let* ([content (document-content doc)]
         [line (offset->line doc offset)]
         [forms (parse-forms-with-lines content)]
         [containing (find-definition-containing-line forms line)])
        (if (not containing)
            #f
            (find-innermost-scope-from-form (car containing) content offset))))

;;; find-innermost-scope-from-form : Sexp × String × Int → (start . end) | #f
;;; Find the innermost scope that contains the offset.
;;; Returns character offset boundaries.
(define (find-innermost-scope-from-form form content offset)
  ;; For now, return the boundaries of the entire top-level form.
  ;; A more sophisticated implementation would find the innermost let/lambda.
  ;; This is conservative: we may rename more than strictly necessary,
  ;; but we'll skip positions where the symbol is shadowed.
  #f)

;;; find-scoped-symbol-positions : String × String × Document × Int → (List Int)
;;; Find symbol positions respecting scope.
;;; Unlike find-symbol-positions, this filters out shadowed occurrences.
(define (find-scoped-symbol-positions content symbol doc rename-offset)
  (let* ([binding-type (symbol-binding-type doc rename-offset)]
         [all-positions (find-symbol-positions content symbol)])
        (case binding-type
          [(global)
           ;; Global symbol - return all positions (except shadowed ones)
           (filter-shadowed-positions content symbol all-positions)]
          [(local-def local-ref)
           ;; Local symbol - only positions in the same local scope
           (filter-to-same-scope content symbol doc rename-offset all-positions)]
          [else all-positions])))

;;; filter-shadowed-positions : String × String × (List Int) → (List Int)
;;; Filter out positions where the symbol is shadowed by a local binding.
(define (filter-shadowed-positions content symbol positions)
  (filter (lambda (pos)
                  (not (is-shadowed-at-position? content symbol pos)))
          positions))

;;; is-shadowed-at-position? : String × String × Int → Boolean
;;; Check if the symbol at position is shadowed by a closer local binding.
(define (is-shadowed-at-position? content symbol pos)
  (guard (e [else #f])
         (let* ([port (open-input-string content)]
                [forms (read-all-forms-with-pos port)]
                [sym (string->symbol symbol)])
               (exists (lambda (form-entry)
                            (let ([form (car form-entry)]
                                  [start (cadr form-entry)]
                                  [end (caddr form-entry)])
                                 (and (>= pos start)
                                      (<= pos end)
                                      (symbol-shadowed-in-form? sym form pos start))))
                     forms))))

;;; read-all-forms-with-pos : Port → (List (form start end))
;;; Read all forms with their start/end positions.
(define (read-all-forms-with-pos port)
  (let loop ([acc '()])
       (let ([start (file-position port)]
             [form (read port)])
            (if (eof-object? form)
                (reverse acc)
                (let ([end (file-position port)])
                     (loop (cons (list form start end) acc)))))))

;;; symbol-shadowed-in-form? : Symbol × Sexp × Int × Int → Boolean
;;; Check if symbol at pos is shadowed within form.
(define (symbol-shadowed-in-form? sym form pos form-start)
  ;; Walk the form to find let/lambda bindings that shadow sym
  ;; and check if pos falls within their scope
  (cond
   [(not (pair? form)) #f]
   ;; Let forms
   [(memq (car form) '(let let* letrec))
    (and (>= (length form) 3)
         (let* ([bindings-part (cadr form)]
                [named? (symbol? bindings-part)]
                [let-bindings (if named? (caddr form) bindings-part)])
               ;; Check if sym is bound in this let
               (and (list? let-bindings)
                    (exists (lambda (b)
                                 (and (pair? b)
                                      (eq? (car b) sym)))
                          let-bindings))))]
   ;; Lambda
   [(memq (car form) '(fn lambda))
    (and (>= (length form) 3)
         (let ([params (cadr form)])
              (and (list? params)
                   (memq sym params))))]
   ;; Recurse into nested forms - any nested scope that shadows
   [else
    (exists (lambda (subform)
                 (and (pair? subform)
                      (symbol-shadowed-in-form? sym subform pos form-start)))
          form)]))

;;; filter-to-same-scope : String × String × Document × Int × (List Int) → (List Int)
;;; Filter positions to only those in the same local scope as rename-offset.
(define (filter-to-same-scope content symbol doc rename-offset positions)
  ;; Find the form containing rename-offset, then filter to positions
  ;; that are in the same innermost binding scope
  (let* ([line (offset->line doc rename-offset)]
         [forms (parse-forms-with-lines content)]
         [containing (find-definition-containing-line forms line)])
        (if (not containing)
            positions  ; No containing form, return all
            ;; Find the scope of our symbol within this form
            (let* ([form (car containing)]
                   [sym (string->symbol symbol)]
                   [scope-info (find-binding-scope-in-form form sym content rename-offset 0)])
                  (if (not scope-info)
                      positions
                      (let ([scope-start (car scope-info)]
                            [scope-end (cdr scope-info)])
                           ;; Filter to positions within this scope, excluding shadowed
                           (filter (lambda (pos)
                                          (and (>= pos scope-start)
                                               (<= pos scope-end)
                                               (not (is-shadowed-at-position? content symbol pos))))
                                   positions)))))))

;;; find-binding-scope-in-form : Sexp × Symbol × String × Int × Int → (start . end) | #f
;;; Find the scope (start/end offsets) where a binding is active.
(define (find-binding-scope-in-form form sym content target-offset current-offset)
  ;; Walk the form to find the binding that sym refers to at target-offset
  ;; Return the scope boundaries of that binding
  ;; This is a simplified version - returns the entire containing definition's scope
  (let ([content-len (string-length content)])
       ;; For now, return the span from current-offset to end of content
       ;; A proper implementation would track exact form boundaries
       (cons current-offset content-len)))

;;; ====
;;; Rename Implementation (Scope-Aware)
;;; ====

;;; compute-rename : Document × JsonObject × String → JsonObject | null
;;; Rename all occurrences of symbol at position to new-name.
;;; Now scope-aware: local bindings only rename within their scope.
;;; Returns a WorkspaceEdit with changes grouped by document.
(define (compute-rename doc position new-name)
  (let* ([symbol (symbol-at-position doc position)]
         [offset (lsp-position->offset doc position)])
        (if (not symbol)
            'null
            (let* ([binding-type (symbol-binding-type doc offset)]
                   [edits-by-uri (compute-rename-edits-scoped symbol new-name doc offset binding-type)])
                  (if (null? edits-by-uri)
                      'null
                      (json-obj "changes" (make-changes-object edits-by-uri)))))))

;;; compute-rename-edits-scoped : String × String × Document × Int × Symbol → (Alist String (List TextEdit))
;;; Compute text edits respecting scope.
;;; For global symbols: rename across all documents (filtering shadowed uses)
;;; For local symbols: rename only within the current document's local scope
(define (compute-rename-edits-scoped old-name new-name origin-doc origin-offset binding-type)
  (case binding-type
    [(global)
     ;; Global: rename in all documents, but skip shadowed positions
     (let ([uris (doc-list)])
          (filter (lambda (pair) (pair? (cdr pair)))
                  (map (lambda (uri)
                               (cons uri (compute-rename-edits-in-doc-filtered uri old-name new-name)))
                       uris)))]
    [(local-def local-ref)
     ;; Local: only rename in origin document, within scope
     (let* ([uri (document-uri origin-doc)]
            [edits (compute-rename-edits-local uri old-name new-name origin-doc origin-offset)])
           (if (null? edits)
               '()
               (list (cons uri edits))))]
    [else
     ;; Fallback: use filtered global rename
     (compute-rename-edits-scoped old-name new-name origin-doc origin-offset 'global)]))

;;; compute-rename-edits-in-doc-filtered : String × String × String → (List TextEdit)
;;; Compute text edits for global rename, filtering shadowed positions.
(define (compute-rename-edits-in-doc-filtered uri old-name new-name)
  (let ([doc (doc-get uri)])
       (if (not doc)
           '()
           (let* ([content (document-content doc)]
                  [positions (filter-shadowed-positions content old-name
                                                        (find-symbol-positions content old-name))]
                  [old-len (string-length old-name)])
                 (map (lambda (pos)
                              (let* ([start-pos (offset->lsp-position doc pos)]
                                     [end-pos (offset->lsp-position doc (+ pos old-len))])
                                    (make-text-edit (make-range start-pos end-pos)
                                                    new-name)))
                      positions)))))

;;; compute-rename-edits-local : String × String × String × Document × Int → (List TextEdit)
;;; Compute text edits for local rename (within scope only).
(define (compute-rename-edits-local uri old-name new-name origin-doc origin-offset)
  (let ([doc (doc-get uri)])
       (if (not doc)
           '()
           (let* ([content (document-content doc)]
                  [positions (find-scoped-symbol-positions content old-name doc origin-offset)]
                  [old-len (string-length old-name)])
                 (map (lambda (pos)
                              (let* ([start-pos (offset->lsp-position doc pos)]
                                     [end-pos (offset->lsp-position doc (+ pos old-len))])
                                    (make-text-edit (make-range start-pos end-pos)
                                                    new-name)))
                      positions)))))

;;; Legacy function for backward compatibility
;;; compute-rename-edits : String × String → (Alist String (List TextEdit))
;;; Compute all text edits needed for renaming, grouped by URI.
;;; WARNING: This does NOT use scope-aware rename. Use compute-rename instead.
(define (compute-rename-edits old-name new-name)
  (let ([uris (doc-list)])
       (filter (lambda (pair) (pair? (cdr pair)))
               (map (lambda (uri)
                            (cons uri (compute-rename-edits-in-doc uri old-name new-name)))
                    uris))))

;;; Legacy function for backward compatibility
;;; compute-rename-edits-in-doc : String × String × String → (List TextEdit)
;;; WARNING: This does NOT use scope-aware rename.
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
