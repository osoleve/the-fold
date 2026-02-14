(source-directories (cons "core" (source-directories)))
(source-directories (cons "shell" (source-directories)))

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/infer.ss")
(load "boundary/tools/edit.ss")

(doc 'module 'type-inspect)
(doc 'description "Examine and display type information for expressions")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'features
     "Infer and display types for expressions"
     "Show type derivations step-by-step"
     "Explain type errors in detail"
     "Compare expected vs actual types"
     "Visualize type structure")
(doc 'usage "(type-inspect expr)")
(doc 'usage "(type-inspect-with-env expr env)")
(doc 'usage "(type-explain expr expected-type)")
(doc 'usage "(type-visualize type)")
(doc 'usage "(type-check-file fs \"path/to/file.ss\")")
(doc 'note "String utilities provided by core/prelude.ss")

(doc 'section 'type-display)

(define (type->string type)
  (doc 'type (-> Type String))
  (doc 'description "Convert a type to a human-readable string")
  (cond
   [(symbol? type) (symbol->string type)]
   [(not (pair? type)) (format "~s" type)]
   [(eq? (car type) '→)
    (let ([arg-types (cadr type)]
          [ret-type (caddr type)])
         (string-append
          (if (pair? arg-types)
              (string-append "("
                             (string-join (map type->string arg-types) " × ")
                             ")")
              (type->string arg-types))
          " → "
          (type->string ret-type)))]
   [(eq? (car type) '∀)
    (let ([vars (cadr type)]
          [body (caddr type)])
         (string-append "∀ "
                        (string-join (map symbol->string vars) " ")
                        ". "
                        (type->string body)))]
   [(eq? (car type) 'μ)
    (let ([var (cadr type)]
          [body (caddr type)])
         (string-append "μ " (symbol->string var) ". " (type->string body)))]
   [(eq? (car type) 'Ref)
    (string-append "Ref(" (type->string (cadr type)) ")")]
   [(eq? (car type) 'Capability)
    (string-append "Capability(" (symbol->string (cadr type)) ")")]
   [(eq? (car type) 'Block)
    (string-append "Block(" (symbol->string (cadr type)) ")")]
   [(eq? (car type) 'List)
    (string-append "List(" (type->string (cadr type)) ")")]
   [(eq? (car type) 'Vector)
    (string-append "Vector(" (type->string (cadr type)) ")")]
   [else (format "~s" type)]))


(define (display-type type)
  (doc 'type (-> Type Void))
  (doc 'description "Pretty-print a type with indentation for nested structures")
  (display (type->string type)))

(doc 'section 'type-inspection)

(define (type-inspect expr)
  (doc 'type (-> Expr Void))
  (doc 'description "Infer and display the type of an expression")
  (type-inspect-with-env expr empty-tenv))

(define (type-inspect-with-env expr env)
  (doc 'type (-> Expr TEnv Void))
  (doc 'description "Infer and display type with a given environment")
  (display "\n==================== TYPE INSPECTION =====================\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  
  (reset-fresh!)
  (let ([result (infer env expr)])
       (cond
        [(error? result)
         (display "Type Error:\n")
         (display (format "  ~a\n" (error-message result)))]
        [else
         (let ([type (result-value result)])
              (display "Inferred Type:\n")
              (display "  ")
              (display-type type)
              (display "\n\n")
              
              ;; Show type structure
              (display "Type Structure:\n")
              (display-type-structure type 1)
              (display "\n"))])))

(define (display-type-structure type depth)
  (doc 'type (-> Type Nat Void))
  (doc 'description "Display type structure with indentation")
  (let ([indent (make-string (* depth 2) #\space)])
       (cond
        [(symbol? type)
         (display (format "~a• ~a (type variable)\n" indent type))]
        [(not (pair? type))
         (display (format "~a• ~a (base type)\n" indent type))]
        [(eq? (car type) '→)
         (display (format "~a• Function type\n" indent))
         (display (format "~a  Arguments:\n" indent))
         (display-type-structure (cadr type) (+ depth 2))
         (display (format "~a  Return:\n" indent))
         (display-type-structure (caddr type) (+ depth 2))]
        [(eq? (car type) '∀)
         (display (format "~a• Universal quantification\n" indent))
         (display (format "~a  Bound variables: ~a\n" indent (cadr type)))
         (display (format "~a  Body:\n" indent))
         (display-type-structure (caddr type) (+ depth 2))]
        [(eq? (car type) 'μ)
         (display (format "~a• Recursive type\n" indent))
         (display (format "~a  Fixed point variable: ~a\n" indent (cadr type)))
         (display (format "~a  Body:\n" indent))
         (display-type-structure (caddr type) (+ depth 2))]
        [else
         (display (format "~a• ~a\n" indent (type->string type)))])))

(doc 'section 'type-explanation)

(define (type-explain expr expected-type)
  (doc 'type (-> Expr Type Void))
  (doc 'description "Check if expression has expected type and explain why/why not")
  (display "\n==================== TYPE EXPLANATION ====================\n\n")
  
  (display "Expression:\n")
  (display (format "  ~s\n\n" expr))
  
  (display "Expected Type:\n")
  (display "  ")
  (display-type expected-type)
  (display "\n\n")
  
  (reset-fresh!)
  (let ([result (check empty-tenv expr expected-type)])
       (cond
        [(error? result)
         (display "❌ Type Check FAILED\n\n")
         (display "Reason:\n")
         (display (format "  ~a\n\n" (error-message result)))
         
         ;; Also show inferred type
         (let ([inferred (infer empty-tenv expr)])
              (when (not (error? inferred))
                    (display "Inferred Type:\n")
                    (display "  ")
                    (display-type (result-value inferred))
                    (display "\n\n")
                    (display "Comparison:\n")
                    (compare-types expected-type (result-value inferred))))]
        [else
         (display "✓ Type Check PASSED\n\n")
         (display "The expression has the expected type.\n")])))

(define (compare-types expected actual)
  (doc 'type (-> Type Type Void))
  (doc 'description "Display a comparison of two types")
  (display "  Expected: ")
  (display-type expected)
  (display "\n  Actual:   ")
  (display-type actual)
  (display "\n\n")
  
  (cond
   [(equal? expected actual)
    (display "  → Types are identical.\n")]
   [(and (pair? expected) (pair? actual) (eq? (car expected) (car actual)))
    (display (format "  → Same type constructor (~a) but different arguments.\n" (car expected)))]
   [(and (symbol? expected) (symbol? actual))
    (display "  → Both are type variables but different.\n")]
   [else
    (display "  → Types have different structure.\n")]))

(doc 'section 'type-visualization)

(define (type-visualize type)
  (doc 'type (-> Type Void))
  (doc 'description "Display a tree visualization of type structure")
  (display "\n=================== TYPE VISUALIZATION ====================\n\n")
  
  (display-type-tree type "" ""))

(define (display-type-tree type prefix last-prefix)
  (doc 'type (-> Type String String Void))
  (doc 'description "Display type as a tree with box-drawing characters")
  (cond
   [(symbol? type)
    (display (format "~a~a\n" prefix type))]
   [(not (pair? type))
    (display (format "~a~a\n" prefix type))]
   [(eq? (car type) '→)
    (display (format "~a→ (function)\n" prefix))
    (let ([new-prefix (string-append last-prefix "  ├─ ")]
          [last-new-prefix (string-append last-prefix "  │  ")])
         (display (string-append last-prefix "  ├─ args: "))
         (display-type (cadr type))
         (display "\n")
         (display (string-append last-prefix "  └─ ret:  "))
         (display-type (caddr type))
         (display "\n"))]
   [(eq? (car type) '∀)
    (display (format "~a∀ (forall ~a)\n" prefix (cadr type)))
    (display (string-append last-prefix "  └─ "))
    (display-type (caddr type))
    (display "\n")]
   [(eq? (car type) 'μ)
    (display (format "~aμ (mu ~a)\n" prefix (cadr type)))
    (display (string-append last-prefix "  └─ "))
    (display-type (caddr type))
    (display "\n")]
   [else
    (display (format "~a~a\n" prefix type))]))

(doc 'section 'file-type-checking)

(define (type-check-file fs file-path)
  (doc 'type (-> FS String Void))
  (doc 'description "Read a file, parse expressions, and check their types")
  (guard (e [else
             (display (format "Error reading file: ~a\n" (if (condition? e)
                                                             (condition-message e)
                                                             e)))])
         (let* ([content (read-text-file fs file-path)]
                [port (open-input-string content)])
               
               (display "\n==================== FILE TYPE CHECK =====================\n\n")
               (display (format "File: ~a\n\n" file-path))
               
               (let loop ([expr-num 1]
                          [errors '()]
                          [successes 0])
                    (let ([expr (guard (e [else #f])
                                       (read port))])
                         (cond
                          [(eof-object? expr)
                           (display "\n")
                           (display (format "Summary:\n"))
                           (display (format "  Expressions checked: ~a\n" (+ successes (length errors))))
                           (display (format "  ✓ Type-correct: ~a\n" successes))
                           (display (format "  ✗ Type errors: ~a\n" (length errors)))
                           (when (not (null? errors))
                                 (display "\nErrors:\n")
                                 (for-each
                                  (lambda (err)
                                          (display (format "  Expression ~a: ~a\n" (car err) (cdr err))))
                                  (reverse errors)))]
                          [(not expr)
                           (display (format "Parse error at expression ~a\n" expr-num))]
                          [else
                           (reset-fresh!)
                           (let ([result (infer empty-tenv expr)])
                                (cond
                                 [(error? result)
                                  (display (format "Expression ~a: ✗ ~a\n" expr-num (error-message result)))
                                  (loop (+ expr-num 1) (cons (cons expr-num (error-message result)) errors) successes)]
                                 [else
                                  (display (format "Expression ~a: ✓ " expr-num))
                                  (display-type (result-value result))
                                  (display "\n")
                                  (loop (+ expr-num 1) errors (+ successes 1))]))]))))))

(doc 'section 'environment-inspection)

(define (display-tenv env)
  (doc 'type (-> TEnv Void))
  (doc 'description "Display a type environment")
  (display "\nType Environment:\n")
  (if (null? env)
      (display "  (empty)\n")
      (for-each
       (lambda (binding)
               (display (format "  ~a : " (car binding)))
               (display-type (cdr binding))
               (display "\n"))
       env))
  (display "\n"))

(define (tenv-inspect env)
  (doc 'type (-> TEnv Void))
  (doc 'description "Show detailed information about a type environment")
  (display "\n============== TYPE ENVIRONMENT INSPECTION ===============\n\n")
  
  (display (format "Total bindings: ~a\n\n" (length env)))
  
  (if (null? env)
      (display "  (empty environment)\n")
      (for-each
       (lambda (binding)
               (display (format "Variable: ~a\n" (car binding)))
               (display "Type: ")
               (display-type (cdr binding))
               (display "\n")
               (display "Structure:\n")
               (display-type-structure (cdr binding) 1)
               (display "\n"))
       env)))

(doc 'section 'batch-type-checking)

(define (type-check-expressions exprs)
  (doc 'type (-> (List Expr) Void))
  (doc 'description "Type-check a list of expressions and show results")
  (display "\n================= BATCH TYPE CHECKING ====================\n\n")
  
  (let loop ([remaining exprs]
             [expr-num 1]
             [env empty-tenv]
             [errors '()]
             [successes 0])
       (if (null? remaining)
           (begin
            (display "\n")
            (display "Summary:\n")
            (display (format "  Total expressions: ~a\n" (- expr-num 1)))
            (display (format "  ✓ Type-correct: ~a\n" successes))
            (display (format "  ✗ Type errors: ~a\n" (length errors)))
            (when (not (null? errors))
                  (display "\nErrors:\n")
                  (for-each
                   (lambda (err)
                           (display (format "  Expression ~a: ~a\n" (car err) (cdr err))))
                   (reverse errors))))
           (let ([expr (car remaining)])
                (reset-fresh!)
                (let ([result (infer env expr)])
                     (cond
                      [(error? result)
                       (display (format "~a. ✗ " expr-num))
                       (display (format "~s\n" expr))
                       (display (format "   Error: ~a\n\n" (error-message result)))
                       (loop (cdr remaining) (+ expr-num 1) env
                             (cons (cons expr-num (error-message result)) errors)
                             successes)]
                      [else
                       (let ([inferred-type (result-value result)])
                            (display (format "~a. ✓ " expr-num))
                            (display (format "~s\n" expr))
                            (display "   Type: ")
                            (display-type inferred-type)
                            (display "\n\n")
                            ;; Add definitions to environment for subsequent expressions
                            (let ([new-env (if (and (pair? expr) (eq? (car expr) 'define))
                                               (tenv-extend env (cadr expr) inferred-type)
                                               env)])
                                 (loop (cdr remaining) (+ expr-num 1) new-env errors (+ successes 1))))]))))))

(display "Type inspector loaded.\n")
(display "Usage:\n")
(display "  (type-inspect expr)\n")
(display "  (type-inspect-with-env expr env)\n")
(display "  (type-explain expr expected-type)\n")
(display "  (type-visualize type)\n")
(display "  (type-check-file fs \"file.ss\")\n")
(display "  (type-check-expressions (list expr1 expr2 ...))\n")
(display "  (tenv-inspect env)\n")
