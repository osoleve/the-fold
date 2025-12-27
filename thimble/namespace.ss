;;; thimble/namespace.ss — Session Namespace Isolation
;;;
;;; Provides transparent symbol prefixing for session isolation.
;;; User-defined variables are prefixed with session ID, preventing
;;; cross-session pollution while maintaining shared access to globals.
;;;
;;; Usage:
;;;   (namespace-transform 'my-session '(define x 42))
;;;   → (define |my-session:x| 42)
;;;
;;;   (namespace-transform 'my-session '(+ x y))
;;;   → (+ |my-session:x| |my-session:y|)
;;;
;;; Escape hatches:
;;;   (global x)           — Reference global x without prefix
;;;   (global-define x 42) — Define global x without prefix
;;;
;;; This is Shell code: expression transformation for isolation.

;;; ============================================================
;;; Global Symbol Whitelist
;;; ============================================================

;;; Symbols that are NEVER prefixed (built-ins, REPL functions, etc.)
(define *global-whitelist*
  (make-hashtable symbol-hash eq?))

;;; Initialize whitelist with known globals
(define (init-global-whitelist!)
  ;; Scheme primitives and syntax
  (for-each
    (lambda (sym)
      (hashtable-set! *global-whitelist* sym #t))
    '(;; Core syntax
      quote quasiquote unquote unquote-splicing
      lambda define define-syntax let let* letrec letrec*
      if cond case and or not when unless
      begin set! do

      ;; Binding forms
      let-values let*-values define-values
      parameterize fluid-let

      ;; Pattern matching
      match guard

      ;; Module system
      library import export

      ;; Primitives - arithmetic
      + - * / = < > <= >= zero? positive? negative?
      add1 sub1 abs min max gcd lcm
      floor ceiling truncate round
      exp log sin cos tan sqrt expt
      quotient remainder modulo
      number? integer? real? rational? complex?
      exact? inexact? exact->inexact inexact->exact

      ;; Primitives - comparison
      eq? eqv? equal?

      ;; Primitives - lists
      cons car cdr caar cadr cdar cddr
      caaar caadr cadar caddr cdaar cdadr cddar cdddr
      caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
      cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
      list list? null? pair? length append reverse
      list-ref list-tail list-head
      memq memv member assq assv assoc
      map for-each filter fold-left fold-right
      car+cdr

      ;; Primitives - vectors
      vector vector? make-vector vector-length
      vector-ref vector-set! vector->list list->vector
      vector-for-each vector-map

      ;; Primitives - strings
      string string? make-string string-length
      string-ref string-set! substring string-append
      string=? string<? string>? string<=? string>=?
      string->list list->string string->symbol symbol->string
      string->number number->string
      string-upcase string-downcase string-titlecase
      string-contains string-prefix? string-suffix?

      ;; Primitives - characters
      char? char=? char<? char>? char<=? char>=?
      char->integer integer->char
      char-upcase char-downcase char-alphabetic?
      char-numeric? char-whitespace?

      ;; Primitives - symbols
      symbol? symbol=? gensym

      ;; Primitives - booleans
      boolean? boolean=?

      ;; Primitives - procedures
      procedure? apply call/cc call-with-current-continuation
      values call-with-values dynamic-wind

      ;; Primitives - I/O
      display write newline read
      read-char write-char peek-char
      open-input-file open-output-file
      close-input-port close-output-port
      call-with-input-file call-with-output-file
      with-input-from-file with-output-to-file
      current-input-port current-output-port
      port? input-port? output-port?
      eof-object? eof-object
      get-line get-string-all put-string
      format printf fprintf
      pretty-print

      ;; Primitives - control
      error raise raise-continuable
      condition? condition-message
      assert

      ;; Primitives - evaluation
      eval load compile
      interaction-environment environment

      ;; Primitives - hashtables
      make-hashtable make-eq-hashtable make-eqv-hashtable
      hashtable? hashtable-ref hashtable-set!
      hashtable-delete! hashtable-contains?
      hashtable-keys hashtable-values hashtable-size
      symbol-hash string-hash equal-hash

      ;; Primitives - records
      define-record-type make-record-type
      record? record-type-descriptor

      ;; Primitives - bytevectors
      bytevector? make-bytevector bytevector-length
      bytevector-u8-ref bytevector-u8-set!

      ;; Primitives - time
      current-time time? time-second time-nanosecond
      make-time sleep

      ;; Primitives - files
      file-exists? delete-file directory-list
      mkdir

      ;; Chez-specific
      void void? top-level-bound? top-level-value
      parameterize make-parameter
      sort list-sort vector-sort
      iota
      trace untrace
      box unbox set-box!
      fx+ fx- fx* fx< fx> fx<= fx>=
      fxand fxior fxxor fxnot fxsll fxsrl

      ;; REPL commands
      hi bye who help commands
      digest chat msg post!
      commit! push! pull!

      ;; Typed evaluation
      fold-eval fold-type fold-parse fold-compile fold-eval-help
      load-core t :type :ann

      ;; Block explorer
      blocks explore-block tree popular orphans search
      bx bx-popular bx-orphans bx-search bx-view bx-back bx-home bx-help

      ;; Other REPL utilities
      fs version clear resume-session

      ;; Forum/CAS
      store! fetch pin! unpin!
      hash-block block? make-block
      block-tag block-payload block-refs

      ;; Session management
      global global-define

      ;; Turtle graphics
      make-turtle fd bk rt lt pu pd
      home cs setxy setx sety setheading
      setpc setbg setpw
      arc circle filled-circle dot
      polygon filled-polygon
      square triangle star spiral
      turtle->drawing drawing->svg save-svg

      ;; Other utilities
      repeat

      ;; Fold language keywords (used inside fold-eval/fold-type expressions)
      fn fix prim ann case make-block
      )))

;; Initialize on load
(init-global-whitelist!)

;;; global? : Symbol → Bool
;;; Check if a symbol is in the global whitelist.
(define (global? sym)
  (hashtable-ref *global-whitelist* sym #f))

;;; add-global! : Symbol → void
;;; Add a symbol to the global whitelist.
(define (add-global! sym)
  (hashtable-set! *global-whitelist* sym #t))

;;; ============================================================
;;; Symbol Prefixing
;;; ============================================================

;;; prefix-symbol : Symbol String → Symbol
;;; Create a namespaced symbol: foo → |session-id:foo|
(define (prefix-symbol sym session-id)
  (string->symbol
    (string-append session-id ":" (symbol->string sym))))

;;; prefixed? : Symbol → Bool
;;; Check if a symbol is already prefixed (contains colon).
(define (prefixed? sym)
  (let ([s (symbol->string sym)])
    (let loop ([i 0])
      (if (>= i (string-length s))
          #f
          (if (char=? (string-ref s i) #\:)
              #t
              (loop (+ i 1)))))))

;;; ============================================================
;;; Expression Transformation
;;; ============================================================

;;; namespace-transform : String Expr → Expr
;;; Transform an expression to use namespaced symbols.
;;; - Bound variables (from lambda, let, etc.) are tracked and not prefixed
;;; - Free variables are prefixed unless in the global whitelist
;;; - Special forms are handled appropriately
(define (namespace-transform session-id expr)
  (transform expr session-id '()))

;;; transform : Expr String (List Symbol) → Expr
;;; Transform with bound variable tracking.
;;; bound-vars: list of symbols that are locally bound (don't prefix)
(define (transform expr session-id bound-vars)
  (cond
    ;; Atoms
    [(symbol? expr)
     (cond
       [(memq expr bound-vars) expr]           ; Locally bound
       [(global? expr) expr]                   ; Global whitelist
       [(prefixed? expr) expr]                 ; Already prefixed
       [else (prefix-symbol expr session-id)])] ; Prefix it

    [(or (number? expr) (string? expr) (boolean? expr)
         (char? expr) (null? expr) (bytevector? expr))
     expr]

    ;; Special forms
    [(pair? expr)
     (let ([head (car expr)])
       (cond
         ;; Quote - don't transform contents
         [(eq? head 'quote) expr]
         [(eq? head 'quasiquote)
          (list 'quasiquote (transform-quasiquote (cadr expr) session-id bound-vars))]

         ;; Global escape hatches
         [(eq? head 'global)
          ;; (global x) → x (no prefix)
          (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
              (cadr expr)
              (error 'namespace-transform "global requires a symbol" expr))]

         [(eq? head 'global-define)
          ;; (global-define x val) → (begin (add-global! 'x) (define x val))
          ;; This registers the symbol so future references/set! work without escape
          (if (and (pair? (cdr expr)) (symbol? (cadr expr)))
              (let ([sym (cadr expr)])
                (list 'begin
                      (list 'add-global! (list 'quote sym))
                      (cons 'define
                            (cons sym
                                  (map (lambda (e) (transform e session-id bound-vars))
                                       (cddr expr))))))
              (error 'namespace-transform "global-define requires a symbol" expr))]

         ;; Lambda - extend bound vars with parameters
         [(eq? head 'lambda)
          (let* ([params (cadr expr)]
                 [body (cddr expr)]
                 [param-syms (extract-params params)]
                 [new-bound (append param-syms bound-vars)])
            (cons 'lambda
                  (cons params
                        (map (lambda (e) (transform e session-id new-bound))
                             body))))]

         ;; Define - prefix the defined name, transform the body
         [(eq? head 'define)
          (let ([pattern (cadr expr)]
                [body (cddr expr)])
            (cond
              ;; (define x val)
              [(symbol? pattern)
               (cons 'define
                     (cons (prefix-symbol pattern session-id)
                           (map (lambda (e) (transform e session-id bound-vars))
                                body)))]
              ;; (define (f args...) body)
              [(pair? pattern)
               (let* ([name (car pattern)]
                      [params (cdr pattern)]
                      [param-syms (extract-params params)]
                      [new-bound (append param-syms bound-vars)])
                 (cons 'define
                       (cons (cons (prefix-symbol name session-id) params)
                             (map (lambda (e) (transform e session-id new-bound))
                                  body))))]
              [else expr]))]

         ;; Let forms - extend bound vars with bindings
         [(memq head '(let let* letrec letrec*))
          (transform-let head (cdr expr) session-id bound-vars)]

         ;; Let-values
         [(eq? head 'let-values)
          (transform-let-values (cdr expr) session-id bound-vars)]

         ;; Set! - prefix if not bound
         [(eq? head 'set!)
          (let ([var (cadr expr)]
                [val (caddr expr)])
            (list 'set!
                  (if (or (memq var bound-vars) (global? var))
                      var
                      (prefix-symbol var session-id))
                  (transform val session-id bound-vars)))]

         ;; Other special forms - transform subexpressions
         [else
          (map (lambda (e) (transform e session-id bound-vars)) expr)]))]

    ;; Vectors
    [(vector? expr)
     (list->vector
       (map (lambda (e) (transform e session-id bound-vars))
            (vector->list expr)))]

    ;; Unknown
    [else expr]))

;;; extract-params : Formals → (List Symbol)
;;; Extract parameter symbols from lambda formals.
(define (extract-params formals)
  (cond
    [(null? formals) '()]
    [(symbol? formals) (list formals)]  ; Rest parameter
    [(pair? formals)
     (cons (if (pair? (car formals)) (caar formals) (car formals))
           (extract-params (cdr formals)))]
    [else '()]))

;;; transform-let : Symbol Expr String (List Symbol) → Expr
;;; Transform let/let*/letrec/letrec* forms.
(define (transform-let let-type rest session-id bound-vars)
  (cond
    ;; Named let: (let name ((var val) ...) body)
    [(and (eq? let-type 'let) (symbol? (car rest)))
     (let* ([name (car rest)]
            [bindings (cadr rest)]
            [body (cddr rest)]
            [binding-vars (map car bindings)]
            [new-bound (append binding-vars (list name) bound-vars)])
       (cons 'let
             (cons (prefix-symbol name session-id)
                   (cons (map (lambda (b)
                                (list (car b)
                                      (transform (cadr b) session-id bound-vars)))
                              bindings)
                         (map (lambda (e) (transform e session-id new-bound))
                              body)))))]

    ;; Regular let forms
    [else
     (let* ([bindings (car rest)]
            [body (cdr rest)]
            [binding-vars (map car bindings)]
            [new-bound (append binding-vars bound-vars)]
            ;; For let*, transform incrementally
            [transformed-bindings
             (if (memq let-type '(let* letrec*))
                 (let loop ([bs bindings] [bv bound-vars] [acc '()])
                   (if (null? bs)
                       (reverse acc)
                       (let ([b (car bs)])
                         (loop (cdr bs)
                               (cons (car b) bv)
                               (cons (list (car b)
                                           (transform (cadr b) session-id
                                                     (if (eq? let-type 'letrec*)
                                                         (append binding-vars bound-vars)
                                                         bv)))
                                     acc)))))
                 ;; For let/letrec, transform all in same scope
                 (map (lambda (b)
                        (list (car b)
                              (transform (cadr b) session-id
                                        (if (eq? let-type 'letrec)
                                            new-bound
                                            bound-vars))))
                      bindings))])
       (cons let-type
             (cons transformed-bindings
                   (map (lambda (e) (transform e session-id new-bound))
                        body))))]))

;;; transform-let-values : Expr String (List Symbol) → Expr
(define (transform-let-values rest session-id bound-vars)
  (let* ([bindings (car rest)]
         [body (cdr rest)]
         [binding-vars (apply append (map (lambda (b) (extract-params (car b))) bindings))]
         [new-bound (append binding-vars bound-vars)])
    (cons 'let-values
          (cons (map (lambda (b)
                       (list (car b)
                             (transform (cadr b) session-id bound-vars)))
                     bindings)
                (map (lambda (e) (transform e session-id new-bound))
                     body)))))

;;; transform-quasiquote : Expr String (List Symbol) → Expr
;;; Transform inside quasiquote, only transforming unquoted parts.
(define (transform-quasiquote expr session-id bound-vars)
  (cond
    [(pair? expr)
     (cond
       [(eq? (car expr) 'unquote)
        (list 'unquote (transform (cadr expr) session-id bound-vars))]
       [(eq? (car expr) 'unquote-splicing)
        (list 'unquote-splicing (transform (cadr expr) session-id bound-vars))]
       [else
        (cons (transform-quasiquote (car expr) session-id bound-vars)
              (transform-quasiquote (cdr expr) session-id bound-vars))])]
    [else expr]))

;;; ============================================================
;;; Integration Helpers
;;; ============================================================

;;; eval-namespaced : String String → Any
;;; Parse and evaluate an expression with namespace transformation.
(define (eval-namespaced session-id expr-string)
  (let* ([port (open-input-string expr-string)]
         [results '()])
    (let loop ()
      (let ([expr (read port)])
        (if (eof-object? expr)
            (if (null? results)
                (void)
                (car results))  ; Return last result
            (let ([transformed (namespace-transform session-id expr)])
              (set! results (list (eval transformed)))
              (loop)))))))
