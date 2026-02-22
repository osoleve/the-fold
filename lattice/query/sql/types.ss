(define *sql-types-loaded* #t)

(doc 'module 'sql-types)
(doc 'description "SQL AST Types")
(doc 'note "Abstract Syntax Tree definitions for ANSI SQL DML statements. All nodes include source spans for error reporting.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; Source span primitives (formerly in fp/parser-dsl.ss)
(define (make-source-span start end) (list 'source-span start end))
(define (source-span? x) (and (pair? x) (eq? (car x) 'source-span)))
(define (span-start s) (list-ref s 1))
(define (span-end s) (list-ref s 2))
(define (merge-spans s1 s2)
  (if (and (source-span? s1) (source-span? s2))
      (make-source-span (span-start s1) (span-end s2))
      (or s1 s2)))
(define no-span #f)

;;; ====
;;; SQL AST Node Constructors
;;; ====
;;;
;;; Pattern: (sql-ast tag span . data)
;;; All constructors return AST nodes with source location tracking.

;;; sql-ast : Symbol × Span × Any... → AST
(define (sql-ast tag span . data)
  (list 'sql-ast tag span data))

;;; sql-ast? : Any → Boolean
(define (sql-ast? x)
  (and (pair? x) (eq? (car x) 'sql-ast)))

;;; sql-tag : AST → Symbol
(define (sql-tag node) (list-ref node 1))

;;; sql-span : AST → Span
(define (sql-span node) (list-ref node 2))

;;; sql-data : AST → List
(define (sql-data node) (list-ref node 3))

;;; sql-ref : AST × Nat → Any
(define (sql-ref node n)
  (list-ref (sql-data node) n))

;;; ====
;;; Statement Types
;;; ====

;;; SELECT statement
;;; Data: (distinct? columns from-clause where-clause group-by having order-by limit)
(define (make-select span distinct? columns from where group-by having order-by limit)
  (sql-ast 'select span distinct? columns from where group-by having order-by limit))

(define (select? x) (and (sql-ast? x) (eq? (sql-tag x) 'select)))
(define (select-distinct? s) (sql-ref s 0))
(define (select-columns s) (sql-ref s 1))
(define (select-from s) (sql-ref s 2))
(define (select-where s) (sql-ref s 3))
(define (select-group-by s) (sql-ref s 4))
(define (select-having s) (sql-ref s 5))
(define (select-order-by s) (sql-ref s 6))
(define (select-limit s) (sql-ref s 7))

;;; INSERT statement
;;; Data: (table columns values-or-select)
(define (make-insert span table columns values)
  (sql-ast 'insert span table columns values))

(define (insert? x) (and (sql-ast? x) (eq? (sql-tag x) 'insert)))
(define (insert-table i) (sql-ref i 0))
(define (insert-columns i) (sql-ref i 1))
(define (insert-values i) (sql-ref i 2))

;;; UPDATE statement
;;; Data: (table set-clauses where-clause)
(define (make-update span table set-clauses where)
  (sql-ast 'update span table set-clauses where))

(define (update? x) (and (sql-ast? x) (eq? (sql-tag x) 'update)))
(define (update-table u) (sql-ref u 0))
(define (update-set-clauses u) (sql-ref u 1))
(define (update-where u) (sql-ref u 2))

;;; DELETE statement
;;; Data: (table where-clause)
(define (make-delete span table where)
  (sql-ast 'delete span table where))

(define (delete? x) (and (sql-ast? x) (eq? (sql-tag x) 'delete)))
(define (delete-table d) (sql-ref d 0))
(define (delete-where d) (sql-ref d 1))

;;; ====
;;; SELECT Components
;;; ====

;;; Column reference
;;; Data: (table-alias column-name) or (column-name) for unqualified
(define (make-column-ref span table-alias column-name)
  (sql-ast 'column-ref span table-alias column-name))

(define (column-ref? x) (and (sql-ast? x) (eq? (sql-tag x) 'column-ref)))
(define (column-ref-table c) (sql-ref c 0))
(define (column-ref-name c) (sql-ref c 1))

;;; Star (SELECT *)
(define (make-star span table-alias)
  (sql-ast 'star span table-alias))

(define (star? x) (and (sql-ast? x) (eq? (sql-tag x) 'star)))
(define (star-table s) (sql-ref s 0))

;;; Aliased expression (SELECT expr AS alias)
(define (make-alias span expr alias-name)
  (sql-ast 'alias span expr alias-name))

(define (alias? x) (and (sql-ast? x) (eq? (sql-tag x) 'alias)))
(define (alias-expr a) (sql-ref a 0))
(define (alias-name a) (sql-ref a 1))

;;; Table reference with optional alias
(define (make-table-ref span table-name alias-name)
  (sql-ast 'table-ref span table-name alias-name))

(define (table-ref? x) (and (sql-ast? x) (eq? (sql-tag x) 'table-ref)))
(define (table-ref-name t) (sql-ref t 0))
(define (table-ref-alias t) (sql-ref t 1))

;;; JOIN clause
(define (make-join span type left right condition)
  (sql-ast 'join span type left right condition))

(define (join? x) (and (sql-ast? x) (eq? (sql-tag x) 'join)))
(define (join-type j) (sql-ref j 0))
(define (join-left j) (sql-ref j 1))
(define (join-right j) (sql-ref j 2))
(define (join-condition j) (sql-ref j 3))

;;; ORDER BY item
(define (make-order-item span expr direction nulls)
  (sql-ast 'order-item span expr direction nulls))

(define (order-item? x) (and (sql-ast? x) (eq? (sql-tag x) 'order-item)))
(define (order-item-expr o) (sql-ref o 0))
(define (order-item-direction o) (sql-ref o 1))  ; 'asc or 'desc
(define (order-item-nulls o) (sql-ref o 2))      ; 'first, 'last, or #f

;;; LIMIT/OFFSET
(define (make-limit-clause span limit offset)
  (sql-ast 'limit-clause span limit offset))

(define (limit-clause? x) (and (sql-ast? x) (eq? (sql-tag x) 'limit-clause)))
(define (limit-value l) (sql-ref l 0))
(define (limit-offset l) (sql-ref l 1))

;;; ====
;;; Expression Types
;;; ====

;;; Literal values
(define (make-literal span value type)
  (sql-ast 'literal span value type))

(define (literal? x) (and (sql-ast? x) (eq? (sql-tag x) 'literal)))
(define (literal-value l) (sql-ref l 0))
(define (literal-type l) (sql-ref l 1))  ; 'number, 'string, 'boolean, 'null

;;; Binary operator
(define (make-binary-op span op left right)
  (sql-ast 'binary-op span op left right))

(define (binary-op? x) (and (sql-ast? x) (eq? (sql-tag x) 'binary-op)))
(define (binary-op-op b) (sql-ref b 0))
(define (binary-op-left b) (sql-ref b 1))
(define (binary-op-right b) (sql-ref b 2))

;;; Unary operator
(define (make-unary-op span op operand)
  (sql-ast 'unary-op span op operand))

(define (unary-op? x) (and (sql-ast? x) (eq? (sql-tag x) 'unary-op)))
(define (unary-op-op u) (sql-ref u 0))
(define (unary-op-operand u) (sql-ref u 1))

;;; Function call
(define (make-function-call span name args distinct?)
  (sql-ast 'function-call span name args distinct?))

(define (function-call? x) (and (sql-ast? x) (eq? (sql-tag x) 'function-call)))
(define (function-call-name f) (sql-ref f 0))
(define (function-call-args f) (sql-ref f 1))
(define (function-call-distinct? f) (sql-ref f 2))

;;; CASE expression
(define (make-case-expr span operand when-clauses else-clause)
  (sql-ast 'case-expr span operand when-clauses else-clause))

(define (case-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'case-expr)))
(define (case-expr-operand c) (sql-ref c 0))  ; #f for searched CASE
(define (case-expr-when-clauses c) (sql-ref c 1))
(define (case-expr-else c) (sql-ref c 2))

;;; WHEN clause for CASE
(define (make-when-clause span condition result)
  (sql-ast 'when-clause span condition result))

(define (when-clause? x) (and (sql-ast? x) (eq? (sql-tag x) 'when-clause)))
(define (when-condition w) (sql-ref w 0))
(define (when-result w) (sql-ref w 1))

;;; IN expression
(define (make-in-expr span expr values not?)
  (sql-ast 'in-expr span expr values not?))

(define (in-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'in-expr)))
(define (in-expr-expr i) (sql-ref i 0))
(define (in-expr-values i) (sql-ref i 1))
(define (in-expr-not? i) (sql-ref i 2))

;;; BETWEEN expression
(define (make-between-expr span expr low high not?)
  (sql-ast 'between-expr span expr low high not?))

(define (between-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'between-expr)))
(define (between-expr-expr b) (sql-ref b 0))
(define (between-expr-low b) (sql-ref b 1))
(define (between-expr-high b) (sql-ref b 2))
(define (between-expr-not? b) (sql-ref b 3))

;;; LIKE expression
(define (make-like-expr span expr pattern escape not?)
  (sql-ast 'like-expr span expr pattern escape not?))

(define (like-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'like-expr)))
(define (like-expr-expr l) (sql-ref l 0))
(define (like-expr-pattern l) (sql-ref l 1))
(define (like-expr-escape l) (sql-ref l 2))
(define (like-expr-not? l) (sql-ref l 3))

;;; IS NULL / IS NOT NULL
(define (make-is-null-expr span expr not?)
  (sql-ast 'is-null-expr span expr not?))

(define (is-null-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'is-null-expr)))
(define (is-null-expr-expr i) (sql-ref i 0))
(define (is-null-expr-not? i) (sql-ref i 1))

;;; Subquery (SELECT inside another expression)
(define (make-subquery span select)
  (sql-ast 'subquery span select))

(define (subquery? x) (and (sql-ast? x) (eq? (sql-tag x) 'subquery)))
(define (subquery-select s) (sql-ref s 0))

;;; EXISTS expression
(define (make-exists-expr span subquery not?)
  (sql-ast 'exists-expr span subquery not?))

(define (exists-expr? x) (and (sql-ast? x) (eq? (sql-tag x) 'exists-expr)))
(define (exists-expr-subquery e) (sql-ref e 0))
(define (exists-expr-not? e) (sql-ref e 1))

;;; ====
;;; UPDATE Components
;;; ====

;;; SET clause (column = value)
(define (make-set-clause span column value)
  (sql-ast 'set-clause span column value))

(define (set-clause? x) (and (sql-ast? x) (eq? (sql-tag x) 'set-clause)))
(define (set-clause-column s) (sql-ref s 0))
(define (set-clause-value s) (sql-ref s 1))

;;; ====
;;; INSERT Components
;;; ====

;;; VALUES clause (list of value lists)
(define (make-values-clause span rows)
  (sql-ast 'values-clause span rows))

(define (values-clause? x) (and (sql-ast? x) (eq? (sql-tag x) 'values-clause)))
(define (values-clause-rows v) (sql-ref v 0))

;;; DEFAULT keyword (for INSERT)
(define (make-default-value span)
  (sql-ast 'default span))

(define (default-value? x) (and (sql-ast? x) (eq? (sql-tag x) 'default)))

;;; ====
;;; Type Inference Support
;;; ====

;;; SQL type tags (for validation)
(define sql-type-number 'number)
(define sql-type-string 'string)
(define sql-type-boolean 'boolean)
(define sql-type-null 'null)
(define sql-type-unknown 'unknown)
(define sql-type-any 'any)

;;; ====
;;; AST Utilities
;;; ====

;;; sql-statement? : Any → Boolean
(define (sql-statement? x)
  (and (sql-ast? x)
       (memq (sql-tag x) '(select insert update delete))))

;;; sql-expression? : Any → Boolean
(define (sql-expression? x)
  (and (sql-ast? x)
       (memq (sql-tag x) '(literal column-ref binary-op unary-op function-call
                           case-expr in-expr between-expr like-expr is-null-expr
                           subquery exists-expr star alias))))

;;; walk-ast : (AST → Any) × AST → Void
;;; Walk AST depth-first, calling visitor on each node.
(define (walk-ast visitor node)
  (when (sql-ast? node)
        (visitor node)
        (for-each (lambda (child)
                          (when (sql-ast? child)
                                (walk-ast visitor child))
                          (when (list? child)
                                (for-each (lambda (c)
                                                  (when (sql-ast? c)
                                                        (walk-ast visitor c)))
                                          child)))
                  (sql-data node))))

;;; collect-nodes : (AST → Boolean) × AST → (List AST)
;;; Collect all nodes matching predicate.
(define (collect-nodes pred node)
  (let ([result '()])
       (walk-ast (lambda (n)
                         (when (pred n)
                               (set! result (cons n result))))
                 node)
       (reverse result)))
