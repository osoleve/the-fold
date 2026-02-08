(define *sql-validate-loaded* #t)

(doc 'module 'sql-validate)
(doc 'description "SQL Semantic Validation")
(doc 'note "Validates SQL AST for semantic correctness. Uses applicative validation to accumulate ALL errors.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(unless (top-level-bound? '*sql-types-loaded*)
        (load "lattice/query/sql/types.ss"))

;;; Validation type (applicative validation = Either with different names)
(define (validation-success val) (list 'validation-success val))
(define (validation-failure err) (list 'validation-failure err))
(define (validation-success? v) (and (pair? v) (eq? (car v) 'validation-success)))
(define (from-success v) (list-ref v 1))
(define (from-failure v) (list-ref v 1))

;;; Applicative combinators — accumulate ALL errors
(define (validation-ap2 f va vb)
  (cond
   [(and (validation-success? va) (validation-success? vb))
    (validation-success ((f (from-success va)) (from-success vb)))]
   [(and (not (validation-success? va)) (not (validation-success? vb)))
    (validation-failure (append (list (from-failure va)) (list (from-failure vb))))]
   [(not (validation-success? va)) (validation-failure (list (from-failure va)))]
   [else (validation-failure (list (from-failure vb)))]))

(define (validation-ap3 f va vb vc)
  (validation-ap2 (lambda (ab) (lambda (c) (ab c)))
                  (validation-ap2 f va vb) vc))

(define (validation-ap4 f va vb vc vd)
  (validation-ap2 (lambda (abc) (lambda (d) (abc d)))
                  (validation-ap3 f va vb vc) vd))

(define (validation-sequence vs)
  (if (null? vs)
      (validation-success '())
      (validation-ap2 (lambda (x) (lambda (xs) (cons x xs)))
                      (car vs)
                      (validation-sequence (cdr vs)))))

;;; ====
;;; Validation Error Types
;;; ====

;;; sql-error : Symbol × Span × String → Error
(define (sql-error code span message)
  (list 'sql-error code span message))

(define (sql-error? x)
  (and (pair? x) (eq? (car x) 'sql-error)))

(define (sql-error-code e) (list-ref e 1))
(define (sql-error-span e) (list-ref e 2))
(define (sql-error-message e) (list-ref e 3))

;;; format-sql-error : Error → String
(define (format-sql-error e)
  (if (sql-error? e)
      (let ([span (sql-error-span e)]
            [msg (sql-error-message e)])
           (if (and span (source-span? span))
               (let ([start (span-start span)])
                    (string-append "Line " (number->string (pos-line start))
                                   ", Column " (number->string (pos-col start))
                                   ": " msg))
               msg))
      (format "~a" e)))

;;; ====
;;; Validation Context
;;; ====
;;;
;;; Context tracks available tables and columns during validation.

;;; make-context : (List String) × (Alist String (List String)) → Context
;;; tables: list of table names in scope
;;; columns: alist mapping table names to column lists
(define (make-context tables columns)
  (list 'context tables columns))

(define (context? x)
  (and (pair? x) (eq? (car x) 'context)))

(define (context-tables ctx) (list-ref ctx 1))
(define (context-columns ctx) (list-ref ctx 2))

;;; empty-context : Context
(define empty-context (make-context '() '()))

;;; context-add-table : Context × String × (List String) → Context
(define (context-add-table ctx table-name columns)
  (make-context
   (cons table-name (context-tables ctx))
   (cons (cons table-name columns) (context-columns ctx))))

;;; context-has-table? : Context × String → Boolean
(define (context-has-table? ctx table-name)
  (member table-name (context-tables ctx)))

;;; context-get-columns : Context × String → (List String)
(define (context-get-columns ctx table-name)
  (let ([pair (assoc table-name (context-columns ctx))])
       (if pair (cdr pair) '())))

;;; ====
;;; Statement Validation
;;; ====

;;; validate-statement : AST → Validation (List Error) AST
(define (validate-statement ast)
  (cond
   [(select? ast) (validate-select ast)]
   [(insert? ast) (validate-insert ast)]
   [(update? ast) (validate-update ast)]
   [(delete? ast) (validate-delete ast)]
   [else (validation-failure (sql-error 'unknown-statement (sql-span ast)
                                        "Unknown statement type"))]))

;;; ====
;;; SELECT Validation
;;; ====

;;; validate-select : AST → Validation (List Error) AST
(define (validate-select ast)
  (let* ([columns (select-columns ast)]
         [from (select-from ast)]
         [where (select-where ast)]
         [group-by (select-group-by ast)]
         [having (select-having ast)]
         [order-by (select-order-by ast)])
        (validation-ap4
         (lambda (cols) (lambda (whr) (lambda (grp) (lambda (hvng) ast))))
         ;; Validate columns aren't empty
         (if (null? columns)
             (validation-failure (sql-error 'empty-select (sql-span ast)
                                            "SELECT must specify at least one column"))
             (validation-sequence (map validate-select-item columns)))
         ;; Validate WHERE expression
         (if where
             (validate-expression where)
             (validation-success #f))
         ;; Validate GROUP BY expressions
         (validation-sequence (map validate-expression group-by))
         ;; Validate HAVING (must have GROUP BY)
         (if having
             (if (null? group-by)
                 (validation-failure (sql-error 'having-without-group (sql-span ast)
                                                "HAVING requires GROUP BY clause"))
                 (validate-expression having))
             (validation-success #f)))))

;;; validate-select-item : AST → Validation (List Error) AST
(define (validate-select-item item)
  (cond
   [(star? item) (validation-success item)]
   [(alias? item)
    (validation-ap2
     (lambda (expr) (lambda (name) item))
     (validate-expression (alias-expr item))
     (validate-identifier (alias-name item)))]
   [else (validate-expression item)]))

;;; ====
;;; INSERT Validation
;;; ====

;;; validate-insert : AST → Validation (List Error) AST
(define (validate-insert ast)
  (let* ([table (insert-table ast)]
         [columns (insert-columns ast)]
         [values (insert-values ast)])
        (validation-ap3
         (lambda (t) (lambda (c) (lambda (v) ast)))
         ;; Validate table name
         (validate-identifier table)
         ;; Validate column names
         (validation-sequence (map validate-identifier columns))
         ;; Validate values
         (validate-insert-values values columns))))

;;; validate-insert-values : AST × (List String) → Validation (List Error) AST
(define (validate-insert-values values columns)
  (cond
   [(values-clause? values)
    (let ([rows (values-clause-rows values)])
         (validation-sequence
          (map (lambda (row)
                       (if (and (not (null? columns))
                                (not (= (length row) (length columns))))
                           (validation-failure
                            (sql-error 'column-count-mismatch no-span
                                       (string-append "Expected " (number->string (length columns))
                                                      " values but got " (number->string (length row)))))
                           (validation-sequence (map validate-value-expr row))))
               rows)))]
   [(select? values)
    (validate-select values)]
   [else (validation-failure (sql-error 'invalid-values no-span
                                        "Invalid INSERT values"))]))

;;; validate-value-expr : AST → Validation (List Error) AST
(define (validate-value-expr expr)
  (if (default-value? expr)
      (validation-success expr)
      (validate-expression expr)))

;;; ====
;;; UPDATE Validation
;;; ====

;;; validate-update : AST → Validation (List Error) AST
(define (validate-update ast)
  (let* ([table (update-table ast)]
         [set-clauses (update-set-clauses ast)]
         [where (update-where ast)])
        (validation-ap3
         (lambda (t) (lambda (s) (lambda (w) ast)))
         ;; Validate table name
         (validate-identifier table)
         ;; Validate SET clauses
         (if (null? set-clauses)
             (validation-failure (sql-error 'empty-set (sql-span ast)
                                            "UPDATE must have at least one SET clause"))
             (validation-sequence (map validate-set-clause set-clauses)))
         ;; Validate WHERE (optional)
         (if where (validate-expression where) (validation-success #f)))))

;;; validate-set-clause : AST → Validation (List Error) AST
(define (validate-set-clause clause)
  (validation-ap2
   (lambda (col) (lambda (val) clause))
   (validate-identifier (set-clause-column clause))
   (validate-expression (set-clause-value clause))))

;;; ====
;;; DELETE Validation
;;; ====

;;; validate-delete : AST → Validation (List Error) AST
(define (validate-delete ast)
  (let* ([table (delete-table ast)]
         [where (delete-where ast)])
        (validation-ap2
         (lambda (t) (lambda (w) ast))
         ;; Validate table name
         (validate-identifier table)
         ;; Validate WHERE (optional)
         (if where (validate-expression where) (validation-success #f)))))

;;; ====
;;; Expression Validation
;;; ====

;;; validate-expression : AST → Validation (List Error) AST
(define (validate-expression expr)
  (cond
   [(literal? expr) (validation-success expr)]
   [(column-ref? expr) (validate-column-ref expr)]
   [(binary-op? expr) (validate-binary-op expr)]
   [(unary-op? expr) (validate-unary-op expr)]
   [(function-call? expr) (validate-function-call expr)]
   [(case-expr? expr) (validate-case-expr expr)]
   [(in-expr? expr) (validate-in-expr expr)]
   [(between-expr? expr) (validate-between-expr expr)]
   [(like-expr? expr) (validate-like-expr expr)]
   [(is-null-expr? expr) (validate-is-null-expr expr)]
   [(subquery? expr) (validate-subquery expr)]
   [(exists-expr? expr) (validate-exists-expr expr)]
   [else (validation-failure (sql-error 'unknown-expr (if (sql-ast? expr) (sql-span expr) no-span)
                                        "Unknown expression type"))]))

;;; validate-column-ref : AST → Validation (List Error) AST
(define (validate-column-ref ref)
  (let ([table (column-ref-table ref)]
        [column (column-ref-name ref)])
       (if table
           (validation-ap2
            (lambda (t) (lambda (c) ref))
            (validate-identifier table)
            (validate-identifier column))
           (validate-identifier column))))

;;; validate-binary-op : AST → Validation (List Error) AST
(define (validate-binary-op expr)
  (let ([op (binary-op-op expr)]
        [left (binary-op-left expr)]
        [right (binary-op-right expr)])
       (validation-ap3
        (lambda (o) (lambda (l) (lambda (r) expr)))
        (validate-operator op)
        (validate-expression left)
        (validate-expression right))))

;;; validate-unary-op : AST → Validation (List Error) AST
(define (validate-unary-op expr)
  (let ([op (unary-op-op expr)]
        [operand (unary-op-operand expr)])
       (validation-ap2
        (lambda (o) (lambda (a) expr))
        (validate-operator op)
        (validate-expression operand))))

;;; validate-function-call : AST → Validation (List Error) AST
(define (validate-function-call expr)
  (let ([name (function-call-name expr)]
        [args (function-call-args expr)])
       (validation-ap2
        (lambda (n) (lambda (a) expr))
        (validate-identifier name)
        (if (and (pair? args) (eq? (car args) '*))
            (validation-success args)
            (validation-sequence (map validate-expression args))))))

;;; validate-case-expr : AST → Validation (List Error) AST
(define (validate-case-expr expr)
  (let ([operand (case-expr-operand expr)]
        [when-clauses (case-expr-when-clauses expr)]
        [else-clause (case-expr-else expr)])
       (validation-ap3
        (lambda (o) (lambda (w) (lambda (e) expr)))
        (if operand (validate-expression operand) (validation-success #f))
        (if (null? when-clauses)
            (validation-failure (sql-error 'empty-case (sql-span expr)
                                           "CASE must have at least one WHEN clause"))
            (validation-sequence (map validate-when-clause when-clauses)))
        (if else-clause (validate-expression else-clause) (validation-success #f)))))

;;; validate-when-clause : AST → Validation (List Error) AST
(define (validate-when-clause clause)
  (validation-ap2
   (lambda (c) (lambda (r) clause))
   (validate-expression (when-condition clause))
   (validate-expression (when-result clause))))

;;; validate-in-expr : AST → Validation (List Error) AST
(define (validate-in-expr expr)
  (let ([left (in-expr-expr expr)]
        [values (in-expr-values expr)])
       (validation-ap2
        (lambda (l) (lambda (v) expr))
        (validate-expression left)
        (if (null? values)
            (validation-failure (sql-error 'empty-in (sql-span expr)
                                           "IN clause cannot be empty"))
            (validation-sequence (map validate-expression values))))))

;;; validate-between-expr : AST → Validation (List Error) AST
(define (validate-between-expr expr)
  (validation-ap3
   (lambda (e) (lambda (l) (lambda (h) expr)))
   (validate-expression (between-expr-expr expr))
   (validate-expression (between-expr-low expr))
   (validate-expression (between-expr-high expr))))

;;; validate-like-expr : AST → Validation (List Error) AST
(define (validate-like-expr expr)
  (validation-ap2
   (lambda (e) (lambda (p) expr))
   (validate-expression (like-expr-expr expr))
   (validate-expression (like-expr-pattern expr))))

;;; validate-is-null-expr : AST → Validation (List Error) AST
(define (validate-is-null-expr expr)
  (validate-expression (is-null-expr-expr expr)))

;;; validate-subquery : AST → Validation (List Error) AST
(define (validate-subquery expr)
  (validate-select (subquery-select expr)))

;;; validate-exists-expr : AST → Validation (List Error) AST
(define (validate-exists-expr expr)
  (validate-subquery (exists-expr-subquery expr)))

;;; ====
;;; Primitive Validators
;;; ====

;;; validate-identifier : String → Validation (List Error) String
(define (validate-identifier name)
  (if (and (string? name) (> (string-length name) 0))
      (validation-success name)
      (validation-failure (sql-error 'invalid-identifier no-span
                                     (string-append "Invalid identifier: " (format "~a" name))))))

;;; validate-operator : Symbol → Validation (List Error) Symbol
(define (validate-operator op)
  (let ([valid-ops '(+ - * / % || = <> < > <= >= and or not)])
       (if (memq op valid-ops)
           (validation-success op)
           (validation-failure (sql-error 'invalid-operator no-span
                                          (string-append "Unknown operator: " (symbol->string op)))))))

;;; ====
;;; Aggregate Function Validation
;;; ====

;;; sql-aggregate-functions : (List String)
(define sql-aggregate-functions
  '("COUNT" "SUM" "AVG" "MIN" "MAX" "GROUP_CONCAT" "STRING_AGG"))

;;; aggregate-function? : String → Boolean
(define (aggregate-function? name)
  (any? (lambda (f) (string-ci-equal? name f)) sql-aggregate-functions))

;;; string-ci-equal? defined in lexer.ss

;;; collect-aggregates : AST → (List AST)
;;; Collect all aggregate function calls in an expression
(define (collect-aggregates expr)
  (collect-nodes (lambda (n)
                         (and (function-call? n)
                              (aggregate-function? (function-call-name n))))
                 expr))

;;; has-aggregates? : AST → Boolean
(define (has-aggregates? expr)
  (not (null? (collect-aggregates expr))))

;;; ====
;;; Advanced Validation (requires schema)
;;; ====

;;; validate-with-schema : AST × Schema → Validation (List Error) AST
;;; Schema is an alist: ((table-name . (col1 col2 ...)) ...)
(define (validate-with-schema ast schema)
  (let ([ctx (fold-left
              (lambda (ctx entry)
                      (context-add-table ctx (car entry) (cdr entry)))
              empty-context schema)])
       (validate-statement-with-context ast ctx)))

;;; validate-statement-with-context : AST × Context → Validation (List Error) AST
(define (validate-statement-with-context ast ctx)
  ;; For now, just run basic validation
  ;; Schema-aware validation would check column references
  (validate-statement ast))
