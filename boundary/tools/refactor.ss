;;; boundary/refactor.ss — Refactoring Toolkit
;;;
;;; Safe automated refactorings for Scheme code:
;;;   - Rename symbol (across files)
;;;   - Extract function
;;;   - Inline function
;;;   - Extract let binding
;;;   - Introduce parameter
;;;
;;; This is Shell code: impure, does file I/O, defensive.
;;;
;;; Design:
;;;   - Parse files into S-expressions with source locations
;;;   - Transform S-expressions
;;;   - Generate diffs/previews before applying
;;;   - Support batch operations across files
;;;
;;; Dependencies:
;;;   - prelude.ss

;; Load prelude - path depends on working directory
;; When run from core: (load "prelude.ss")
;; This file is typically loaded after prelude is already loaded

;;; ====
;;; Source Location Tracking
;;; ====

;;; A located value pairs a value with its source location
(define (make-loc value file line col)
  (vector 'loc value file line col))

(define (loc? x)
  (and (vector? x) (>= (vector-length x) 5) (eq? (vector-ref x 0) 'loc)))

(define (loc-value l) (vector-ref l 1))
(define (loc-file l) (vector-ref l 2))
(define (loc-line l) (vector-ref l 3))
(define (loc-col l) (vector-ref l 4))

;;; ====
;;; Change Representation
;;; ====

;;; A change represents a single edit to apply
(define (make-change type file old-text new-text line col)
  (vector 'change type file old-text new-text line col))

(define (change? x)
  (and (vector? x) (>= (vector-length x) 7) (eq? (vector-ref x 0) 'change)))

(define (change-type c) (vector-ref c 1))
(define (change-file c) (vector-ref c 2))
(define (change-old c) (vector-ref c 3))
(define (change-new c) (vector-ref c 4))
(define (change-line c) (vector-ref c 5))
(define (change-col c) (vector-ref c 6))

;;; Format a change for preview
(define (change->string c)
  (format "~a:~a: ~a
  - ~a
  + ~a"
          (change-file c)
          (change-line c)
          (change-type c)
          (change-old c)
          (change-new c)))

;;; ====
;;; S-Expression Traversal
;;; ====

;;; Walk an S-expression, applying a function to each node
;;; walker : (sexp context) -> sexp
;;; Returns transformed S-expression
(define (sexp-walk walker sexp context)
  (let ([transformed (walker sexp context)])
       (cond
        [(pair? transformed)
         (cons (sexp-walk walker (car transformed) context)
               (sexp-walk walker (cdr transformed) context))]
        [else transformed])))

;;; Collect all symbols in an S-expression
(define (sexp-symbols sexp)
  (cond
   [(symbol? sexp) (list sexp)]
   [(pair? sexp)
    (append (sexp-symbols (car sexp))
            (sexp-symbols (cdr sexp)))]
   [else '()]))

;;; Find all occurrences of a symbol in an S-expression
;;; Returns list of paths (list of indices) to each occurrence
(define (sexp-find-symbol sexp sym)
  (let loop ([sexp sexp] [path '()])
       (cond
        [(and (symbol? sexp) (eq? sexp sym))
         (list (reverse path))]
        [(pair? sexp)
         (append (loop (car sexp) (cons 'car path))
                 (loop (cdr sexp) (cons 'cdr path)))]
        [else '()])))

;;; ====
;;; Rename Symbol
;;; ====

;;; rename-symbol : sexp symbol symbol -> sexp
;;; Rename all occurrences of old-name to new-name in sexp
(define (rename-symbol sexp old-name new-name)
  (sexp-walk
   (lambda (s ctx)
           (if (and (symbol? s) (eq? s old-name))
               new-name
               s))
   sexp
   #f))

;;; rename-symbol-in-string : string symbol symbol -> string
;;; Rename symbol in a string containing Scheme code
(define (rename-symbol-in-string code old-name new-name)
  (let* ([port (open-input-string code)]
         [sexps (read-all-sexps port)]
         [renamed (map (lambda (s) (rename-symbol s old-name new-name)) sexps)])
        (sexps->string renamed)))

;;; Helper: read all S-expressions from port
(define (read-all-sexps port)
  (let loop ([acc '()])
       (let ([sexp (read port)])
            (if (eof-object? sexp)
                (reverse acc)
                (loop (cons sexp acc))))))

;;; Helper: convert S-expressions back to string
(define (sexps->string sexps)
  (apply string-append
         (map (lambda (s)
                      (string-append (sexp->string s) "

"))
              sexps)))

;;; Pretty-print an S-expression to string
(define (sexp->string sexp)
  (let ([port (open-output-string)])
       (pretty-print sexp port)
       (get-output-string port)))

;;; ====
;;; Extract Function
;;; ====

;;; extract-function : sexp symbol list -> (values sexp sexp)
;;; Extract an expression into a new function definition
;;; Returns: (new-definition . modified-sexp)
;;;
;;; Example:
;;;   (extract-function '(+ (* x x) (* y y)) 'sum-squares '(x y))
;;;   => (define (sum-squares x y) (+ (* x x) (* y y)))
;;;      (sum-squares x y)
(define (extract-function expr name params)
  (let ([definition `(define (,name ,@params) ,expr)]
        [call `(,name ,@params)])
       (cons definition call)))

;;; extract-function-from-sexp : sexp path symbol list -> sexp
;;; Extract expression at path into a new function
;;; Inserts definition before the containing top-level form
(define (extract-at-path sexp path name params)
  (let* ([expr (sexp-at-path sexp path)]
         [extracted (extract-function expr name params)]
         [definition (car extracted)]
         [call (cdr extracted)]
         [modified (sexp-replace-at-path sexp path call)])
        (list definition modified)))

;;; Get S-expression at a path
(define (sexp-at-path sexp path)
  (if (null? path)
      sexp
      (let ([step (car path)])
           (sexp-at-path
            (if (eq? step 'car) (car sexp) (cdr sexp))
            (cdr path)))))

;;; Replace S-expression at a path
(define (sexp-replace-at-path sexp path new-value)
  (if (null? path)
      new-value
      (let ([step (car path)])
           (if (eq? step 'car)
               (cons (sexp-replace-at-path (car sexp) (cdr path) new-value)
                     (cdr sexp))
               (cons (car sexp)
                     (sexp-replace-at-path (cdr sexp) (cdr path) new-value))))))

;;; ====
;;; Inline Function
;;; ====

;;; Find a function definition in a list of top-level forms
(define (find-definition forms name)
  (let loop ([forms forms])
       (cond
        [(null? forms) #f]
        [(and (pair? (car forms))
              (eq? (caar forms) 'define)
              (pair? (cadar forms))
              (eq? (caadar forms) name))
         (car forms)]
        [else (loop (cdr forms))])))

;;; Get parameters from a function definition
(define (definition-params def)
  (cdadr def))

;;; Get body from a function definition
(define (definition-body def)
  (if (null? (cddr def))
      (void)
      (if (null? (cdddr def))
          (caddr def)
          `(begin ,@(cddr def)))))

;;; Substitute parameters with arguments in body
(define (substitute-params body params args)
  (if (null? params)
      body
      (substitute-params
       (rename-symbol body (car params) (car args))
       (cdr params)
       (cdr args))))

;;; inline-call : sexp definition -> sexp
;;; Replace a function call with its inlined body
(define (inline-call call def)
  (let* ([params (definition-params def)]
         [body (definition-body def)]
         [args (cdr call)])
        (if (= (length params) (length args))
            (substitute-params body params args)
            (error 'inline-call
                   (format "Arity mismatch: expected ~a args, got ~a"
                           (length params) (length args))))))

;;; inline-function : sexp symbol -> sexp
;;; Inline all calls to a function in an S-expression
;;; Assumes the definition is available in the same scope
(define (inline-function-calls sexp name def)
  (sexp-walk
   (lambda (s ctx)
           (if (and (pair? s)
                    (eq? (car s) name))
               (inline-call s def)
               s))
   sexp
   #f))

;;; ====
;;; Extract Let Binding
;;; ====

;;; extract-let : sexp path symbol -> sexp
;;; Extract expression at path into a let binding
;;;
;;; Example:
;;;   (extract-let '(+ (* x x) (* y y)) '(car cdr car) 'x-sq)
;;;   => (let ([x-sq (* x x)]) (+ x-sq (* y y)))
(define (extract-let sexp path name)
  (let* ([expr (sexp-at-path sexp path)]
         [modified (sexp-replace-at-path sexp path name)])
        `(let ([,name ,expr]) ,modified)))

;;; ====
;;; Introduce Parameter
;;; ====

;;; introduce-parameter : sexp expr symbol -> sexp
;;; Replace all occurrences of expr with a new parameter
;;; Returns modified body (caller must add parameter to definition)
(define (introduce-parameter body expr param-name)
  (sexp-walk
   (lambda (s ctx)
           (if (equal? s expr)
               param-name
               s))
   body
   #f))

;;; ====
;;; Free Variables Analysis
;;; ====

;;; Compute free variables in an expression
;;; (variables used but not bound within the expression)
(define (free-variables expr)
  (free-vars-helper expr '()))

(define (free-vars-helper expr bound)
  (cond
   [(symbol? expr)
    (if (member expr bound) '() (list expr))]
   [(not (pair? expr)) '()]
   ;; Lambda binds its parameters
   [(eq? (car expr) 'lambda)
    (let ([params (cadr expr)]
          [body (cddr expr)])
         (free-vars-helper body (append (if (list? params) params (list params))
                                        bound)))]
   ;; Let binds its variables
   [(eq? (car expr) 'let)
    (let* ([bindings (cadr expr)]
           [body (cddr expr)]
           [vars (map car bindings)]
           [init-frees (apply append
                              (map (lambda (b) (free-vars-helper (cadr b) bound))
                                   bindings))]
           [body-frees (free-vars-helper body (append vars bound))])
          (append init-frees body-frees))]
   ;; Let* binds sequentially
   [(eq? (car expr) 'let*)
    (let loop ([bindings (cadr expr)]
               [bound bound]
               [frees '()])
         (if (null? bindings)
             (append frees (free-vars-helper (cddr expr) bound))
             (let ([var (caar bindings)]
                   [init (cadar bindings)])
                  (loop (cdr bindings)
                        (cons var bound)
                        (append frees (free-vars-helper init bound))))))]
   ;; Define at top level
   [(eq? (car expr) 'define)
    (if (pair? (cadr expr))
        ;; Function definition
        (let ([params (cdadr expr)]
              [body (cddr expr)])
             (free-vars-helper body (append params bound)))
        ;; Variable definition
        (free-vars-helper (caddr expr) bound))]
   ;; General case: recurse
   [else
    (apply append (map (lambda (sub) (free-vars-helper sub bound)) expr))]))

;;; NOTE: unique is provided by core/base/prelude.ss

;;; ====
;;; File Operations
;;; ====

;;; Read a Scheme file and return list of S-expressions
(define (read-scheme-file path)
  (call-with-input-file path
                        (lambda (port)
                                (read-all-sexps port))))

;;; Write S-expressions to a file
(define (write-scheme-file path sexps)
  (call-with-output-file path
                         (lambda (port)
                                 (for-each (lambda (sexp)
                                                   (pretty-print sexp port)
                                                   (newline port))
                                           sexps))))

;;; ====
;;; Batch Rename Across Files
;;; ====

;;; rename-in-files : list symbol symbol -> list
;;; Rename a symbol across multiple files
;;; Returns list of changes made
(define (rename-in-files file-paths old-name new-name)
  (let loop ([paths file-paths] [changes '()])
       (if (null? paths)
           (reverse changes)
           (let* ([path (car paths)]
                  [sexps (read-scheme-file path)]
                  [renamed (map (lambda (s) (rename-symbol s old-name new-name))
                                sexps)]
                  [changed? (not (equal? sexps renamed))])
                 (when changed?
                       (write-scheme-file path renamed))
                 (loop (cdr paths)
                       (if changed?
                           (cons (make-change 'rename path
                                              (symbol->string old-name)
                                              (symbol->string new-name)
                                              0 0)
                                 changes)
                           changes))))))

;;; ====
;;; Preview Mode
;;; ====

;;; Generate a preview of rename without applying
(define (preview-rename sexps old-name new-name)
  (let ([occurrences (apply append
                            (map (lambda (s) (sexp-find-symbol s old-name))
                                 sexps))])
       (map (lambda (path)
                    (format "  Found ~a at path ~a" old-name path))
            occurrences)))

;;; ====
;;; Refactoring Session
;;; ====

;;; A session tracks changes for undo
(define (make-refactor-session)
  (vector 'refactor-session '() '()))  ; (changes undo-stack)

(define (session? x)
  (and (vector? x) (>= (vector-length x) 3)
       (eq? (vector-ref x 0) 'refactor-session)))

(define (session-changes s) (vector-ref s 1))
(define (session-undo-stack s) (vector-ref s 2))

(define (session-add-change! session change)
  (vector-set! session 1 (cons change (session-changes session)))
  (vector-set! session 2 (cons change (session-undo-stack session))))

;;; ====
;;; High-Level API
;;; ====

;;; refactor-rename : string symbol symbol -> change
;;; Rename a symbol in a file
(define (refactor-rename file-path old-name new-name)
  (let* ([sexps (read-scheme-file file-path)]
         [renamed (map (lambda (s) (rename-symbol s old-name new-name)) sexps)])
        (write-scheme-file file-path renamed)
        (make-change 'rename file-path
                     (symbol->string old-name)
                     (symbol->string new-name)
                     0 0)))

;;; refactor-extract : string path symbol list -> change
;;; Extract expression at path into new function
(define (refactor-extract file-path sexp-index expr-path name params)
  (let* ([sexps (read-scheme-file file-path)]
         [target-sexp (list-ref sexps sexp-index)]
         [extracted (extract-at-path target-sexp expr-path name params)]
         [definition (car extracted)]
         [modified (cadr extracted)]
         [new-sexps (append (take sexps sexp-index)
                            (list definition modified)
                            (drop sexps (+ sexp-index 1)))])
        (write-scheme-file file-path new-sexps)
        (make-change 'extract file-path
                     (sexp->string target-sexp)
                     (sexp->string modified)
                     0 0)))

;;; Helper: take first n elements
(define (take lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (cdr lst) (- n 1)))))

;;; Helper: drop first n elements
(define (drop lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop (cdr lst) (- n 1))))

;;; ====
;;; Semantic Search
;;; ====

;;; Find all definitions of a given type
(define (find-definitions sexps type)
  (filter (lambda (sexp)
                  (and (pair? sexp)
                       (eq? (car sexp) type)))
          sexps))

;;; Find all function definitions
(define (find-functions sexps)
  (filter (lambda (sexp)
                  (and (pair? sexp)
                       (eq? (car sexp) 'define)
                       (pair? (cadr sexp))))
          sexps))

;;; Find all variable definitions
(define (find-variables sexps)
  (filter (lambda (sexp)
                  (and (pair? sexp)
                       (eq? (car sexp) 'define)
                       (symbol? (cadr sexp))))
          sexps))

;;; Find all usages of a symbol
(define (find-usages sexps sym)
  (apply append
         (map (lambda (sexp)
                      (sexp-find-symbol sexp sym))
              sexps)))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Source Location:
;;;   make-loc, loc?, loc-value, loc-file, loc-line, loc-col
;;;
;;; Change Tracking:
;;;   make-change, change?, change-type, change-file,
;;;   change-old, change-new, change->string
;;;
;;; S-Expression Traversal:
;;;   sexp-walk, sexp-symbols, sexp-find-symbol,
;;;   sexp-at-path, sexp-replace-at-path
;;;
;;; Core Refactorings:
;;;   rename-symbol, rename-symbol-in-string
;;;   extract-function, extract-at-path, extract-let
;;;   inline-call, inline-function-calls
;;;   introduce-parameter
;;;
;;; Analysis:
;;;   free-variables, unique
;;;   find-definition, definition-params, definition-body
;;;
;;; File Operations:
;;;   read-scheme-file, write-scheme-file
;;;   rename-in-files, preview-rename
;;;
;;; Session Management:
;;;   make-refactor-session, session-add-change!
;;;
;;; High-Level API:
;;;   refactor-rename, refactor-extract
;;;
;;; Semantic Search:
;;;   find-definitions, find-functions, find-variables, find-usages
