;;; playpen/sql/parser.ss — SQL Grammar Parser
;;;
;;; Parser combinators for ANSI SQL DML statements.
;;; Uses operator precedence parsing for expressions.
;;;
;;; Dependencies:
;;;   - playpen/sql/types.ss
;;;   - playpen/sql/lexer.ss

;;; Load dependencies - assumes running from project root
(define *sql-parser-loaded* #t)

(unless (top-level-bound? '*sql-types-loaded*)
        (load "lattice/query/sql/types.ss"))
(unless (top-level-bound? '*sql-lexer-loaded*)
        (load "lattice/query/sql/lexer.ss"))

;;; ====
;;; Forward Declarations
;;; ====
;;;
;;; We need mutual recursion between expressions and SELECT.

;;; Placeholder parsers
(define sql-expr #f)
(define sql-select-stmt #f)

;;; Thunk to defer parsing
(define (sql-expr-thunk state)
  (run-parser sql-expr state))

(define (sql-select-thunk state)
  (run-parser sql-select-stmt state))

;;; Lazy expression parser
(define lazy-sql-expr
  (make-parser sql-expr-thunk))

;;; Lazy select parser
(define lazy-sql-select
  (make-parser sql-select-thunk))

;;; ====
;;; Expression Atoms
;;; ====

;;; sql-literal-expr : Parser AST
(define sql-literal-expr
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind
                        (choice (list
                                 (parser-map (lambda (n) (cons n 'number)) sql-number)
                                 (parser-map (lambda (s) (cons s 'string)) sql-string-literal)
                                 (parser-map (lambda (b) (cons b 'boolean)) sql-boolean)
                                 (parser-map (lambda (_) (cons 'null 'null)) sql-null)))
                        (lambda (val-type)
                                (parser-bind get-pos
                                             (lambda (end)
                                                     (parser-pure (make-literal
                                                                   (make-source-span start end)
                                                                   (car val-type)
                                                                   (cdr val-type))))))))))

;;; sql-column-ref : Parser AST
(define sql-column-ref
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind sql-identifier
                                    (lambda (first)
                                            (parser-bind (optional (parser-then sql-dot sql-identifier) #f)
                                                         (lambda (second)
                                                                 (parser-bind get-pos
                                                                              (lambda (end)
                                                                                      (parser-pure
                                                                                       (if second
                                                                                           (make-column-ref (make-source-span start end) first second)
                                                                                           (make-column-ref (make-source-span start end) #f first))))))))))))

;;; sql-star-expr : Parser AST
(define sql-star-expr
  (parser-bind get-pos
               (lambda (start)
                       (parser-or
                        ;; table.*
                        (try (parser-bind sql-identifier
                                          (lambda (table)
                                                  (parser-bind sql-dot
                                                               (lambda (_)
                                                                       (parser-bind sql-star
                                                                                    (lambda (_)
                                                                                            (parser-bind get-pos
                                                                                                         (lambda (end)
                                                                                                                 (parser-pure (make-star (make-source-span start end) table)))))))))))
                        ;; plain *
                        (parser-bind sql-star
                                     (lambda (_)
                                             (parser-bind get-pos
                                                          (lambda (end)
                                                                  (parser-pure (make-star (make-source-span start end) #f))))))))))

;;; sql-function-name : Parser String
;;; Function names can be identifiers OR reserved words like COUNT, SUM, etc.
(define sql-function-name
  (sql-lexeme
   (parser-or
    ;; Try aggregate functions first (they are reserved words)
    (choice (map (lambda (kw) (try (string-ci-parser kw)))
                 '("COUNT" "SUM" "AVG" "MIN" "MAX" "UPPER" "LOWER" "LENGTH"
                   "COALESCE" "NULLIF" "CAST" "SUBSTRING" "TRIM" "ABS" "ROUND")))
    ;; Then regular identifiers
    sql-bare-identifier)))

;;; sql-function-call : Parser AST
(define sql-function-call
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind sql-function-name
                                    (lambda (name)
                                            (parser-bind sql-lparen
                                                         (lambda (_)
                                                                 (parser-bind (optional (parser-then (sql-keyword "DISTINCT") (parser-pure #t)) #f)
                                                                              (lambda (distinct?)
                                                                                      (parser-bind
                                                                                       (choice (list
                                                                                                (parser-then sql-star (parser-pure '(*)))
                                                                                                (sql-comma-sep lazy-sql-expr)))
                                                                                       (lambda (args)
                                                                                               (parser-bind sql-rparen
                                                                                                            (lambda (_)
                                                                                                                    (parser-bind get-pos
                                                                                                                                 (lambda (end)
                                                                                                                                         (parser-pure (make-function-call
                                                                                                                                                       (make-source-span start end)
                                                                                                                                                       name args distinct?)))))))))))))))))

;;; sql-case-expr : Parser AST
(define sql-case-expr
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "CASE")
                                    (lambda (_)
                                            (parser-bind (option-maybe (try lazy-sql-expr))
                                                         (lambda (operand-maybe)
                                                                 (parser-bind (some sql-when-clause)
                                                                              (lambda (when-clauses)
                                                                                      (parser-bind (optional (parser-then (sql-keyword "ELSE") lazy-sql-expr) #f)
                                                                                                   (lambda (else-clause)
                                                                                                           (parser-bind (sql-keyword "END")
                                                                                                                        (lambda (_)
                                                                                                                                (parser-bind get-pos
                                                                                                                                             (lambda (end)
                                                                                                                                                     (parser-pure (make-case-expr
                                                                                                                                                                   (make-source-span start end)
                                                                                                                                                                   (if (nothing? operand-maybe) #f (from-just operand-maybe))
                                                                                                                                                                   when-clauses
                                                                                                                                                                   else-clause)))))))))))))))))

;;; sql-when-clause : Parser AST
(define sql-when-clause
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "WHEN")
                                    (lambda (_)
                                            (parser-bind lazy-sql-expr
                                                         (lambda (condition)
                                                                 (parser-bind (sql-keyword "THEN")
                                                                              (lambda (_)
                                                                                      (parser-bind lazy-sql-expr
                                                                                                   (lambda (result)
                                                                                                           (parser-bind get-pos
                                                                                                                        (lambda (end)
                                                                                                                                (parser-pure (make-when-clause
                                                                                                                                              (make-source-span start end)
                                                                                                                                              condition result)))))))))))))))

;;; sql-subquery : Parser AST
(define sql-subquery
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind sql-lparen
                                    (lambda (_)
                                            (parser-bind lazy-sql-select
                                                         (lambda (select)
                                                                 (parser-bind sql-rparen
                                                                              (lambda (_)
                                                                                      (parser-bind get-pos
                                                                                                   (lambda (end)
                                                                                                           (parser-pure (make-subquery
                                                                                                                         (make-source-span start end) select)))))))))))))

;;; sql-exists-expr : Parser AST
(define sql-exists-expr
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (optional (parser-then (sql-keyword "NOT") (parser-pure #t)) #f)
                                    (lambda (not?)
                                            (parser-bind (sql-keyword "EXISTS")
                                                         (lambda (_)
                                                                 (parser-bind sql-subquery
                                                                              (lambda (subq)
                                                                                      (parser-bind get-pos
                                                                                                   (lambda (end)
                                                                                                           (parser-pure (make-exists-expr
                                                                                                                         (make-source-span start end) subq not?)))))))))))))

;;; sql-paren-expr : Parser AST
(define sql-paren-expr
  (parser-or
   (try sql-subquery)
   (sql-parens lazy-sql-expr)))

;;; sql-atom : Parser AST
(define sql-atom
  (choice (list
           (try sql-case-expr)
           (try sql-exists-expr)
           (try sql-function-call)
           sql-literal-expr
           (try sql-paren-expr)
           sql-column-ref)))

;;; ====
;;; Expression Parser with Precedence
;;; ====

;;; sql-mul-expr : Parser AST
(define sql-mul-expr
  (chainl1 sql-atom
           (choice (list
                    (parser-then sql-star
                                 (parser-pure (lambda (l r) (make-binary-op no-span '* l r))))
                    (parser-then sql-slash
                                 (parser-pure (lambda (l r) (make-binary-op no-span '/ l r))))
                    (parser-then sql-percent
                                 (parser-pure (lambda (l r) (make-binary-op no-span '% l r))))))))

;;; sql-add-expr : Parser AST
(define sql-add-expr
  (chainl1 sql-mul-expr
           (choice (list
                    (parser-then sql-plus
                                 (parser-pure (lambda (l r) (make-binary-op no-span '+ l r))))
                    (parser-then sql-minus
                                 (parser-pure (lambda (l r) (make-binary-op no-span '- l r))))
                    (parser-then sql-concat
                                 (parser-pure (lambda (l r) (make-binary-op no-span 'concat l r))))))))

;;; sql-comparison-op : Parser Symbol
(define sql-comparison-op
  (choice (list
           (parser-then sql-eq (parser-pure '=))
           (parser-then sql-neq (parser-pure '<>))
           (parser-then sql-lte (parser-pure '<=))
           (parser-then sql-gte (parser-pure '>=))
           (parser-then sql-lt (parser-pure '<))
           (parser-then sql-gt (parser-pure '>)))))

;;; sql-compare-expr : Parser AST
(define sql-compare-expr
  (parser-bind sql-add-expr
               (lambda (left)
                       (parser-or
                        (parser-bind sql-comparison-op
                                     (lambda (op)
                                             (parser-bind sql-add-expr
                                                          (lambda (right)
                                                                  (parser-pure (make-binary-op no-span op left right))))))
                        (parser-pure left)))))

;;; sql-is-expr : Parser AST
(define sql-is-expr
  (parser-bind sql-compare-expr
               (lambda (left)
                       (parser-or
                        (parser-bind get-pos
                                     (lambda (start)
                                             (parser-bind (sql-keyword "IS")
                                                          (lambda (_)
                                                                  (parser-bind (optional (parser-then (sql-keyword "NOT") (parser-pure #t)) #f)
                                                                               (lambda (not?)
                                                                                       (parser-bind (sql-keyword "NULL")
                                                                                                    (lambda (_)
                                                                                                            (parser-bind get-pos
                                                                                                                         (lambda (end)
                                                                                                                                 (parser-pure (make-is-null-expr
                                                                                                                                               (make-source-span start end) left not?))))))))))))
                        (parser-pure left)))))

;;; sql-between-expr : Parser AST
(define sql-between-expr
  (parser-bind sql-is-expr
               (lambda (left)
                       (parser-or
                        ;; NOT BETWEEN
                        (try (parser-bind get-pos
                                          (lambda (start)
                                                  (parser-bind (sql-keyword "NOT")
                                                               (lambda (_)
                                                                       (parser-bind (sql-keyword "BETWEEN")
                                                                                    (lambda (_)
                                                                                            (parser-bind sql-is-expr
                                                                                                         (lambda (low)
                                                                                                                 (parser-bind (sql-keyword "AND")
                                                                                                                              (lambda (_)
                                                                                                                                      (parser-bind sql-is-expr
                                                                                                                                                   (lambda (high)
                                                                                                                                                           (parser-bind get-pos
                                                                                                                                                                        (lambda (end)
                                                                                                                                                                                (parser-pure (make-between-expr
                                                                                                                                                                                              (make-source-span start end)
                                                                                                                                                                                              left low high #t)))))))))))))))))
                        ;; BETWEEN (without NOT)
                        (parser-bind get-pos
                                     (lambda (start)
                                             (parser-or
                                              (parser-bind (sql-keyword "BETWEEN")
                                                           (lambda (_)
                                                                   (parser-bind sql-is-expr
                                                                                (lambda (low)
                                                                                        (parser-bind (sql-keyword "AND")
                                                                                                     (lambda (_)
                                                                                                             (parser-bind sql-is-expr
                                                                                                                          (lambda (high)
                                                                                                                                  (parser-bind get-pos
                                                                                                                                               (lambda (end)
                                                                                                                                                       (parser-pure (make-between-expr
                                                                                                                                                                     (make-source-span start end)
                                                                                                                                                                     left low high #f))))))))))))
                                              (parser-pure left))))))))

;;; sql-in-values : Parser (Either (List AST) AST)
;;; Parse either a list of values or a subquery for IN clause
(define sql-in-values
  (sql-parens
   (parser-or
    ;; Try subquery first
    (try lazy-sql-select)
    ;; Otherwise a list of expressions
    (sql-comma-sep1 sql-between-expr))))

;;; sql-in-expr : Parser AST
(define sql-in-expr
  (parser-bind sql-between-expr
               (lambda (left)
                       (parser-or
                        ;; NOT IN
                        (try (parser-bind get-pos
                                          (lambda (start)
                                                  (parser-bind (sql-keyword "NOT")
                                                               (lambda (_)
                                                                       (parser-bind (sql-keyword "IN")
                                                                                    (lambda (_)
                                                                                            (parser-bind sql-in-values
                                                                                                         (lambda (values)
                                                                                                                 (parser-bind get-pos
                                                                                                                              (lambda (end)
                                                                                                                                      (let ([vals (if (select? values)
                                                                                                                                                      (list (make-subquery (make-source-span start end) values))
                                                                                                                                                      values)])
                                                                                                                                           (parser-pure (make-in-expr
                                                                                                                                                         (make-source-span start end)
                                                                                                                                                         left vals #t))))))))))))))
                        ;; IN (without NOT)
                        (parser-bind get-pos
                                     (lambda (start)
                                             (parser-or
                                              (parser-bind (sql-keyword "IN")
                                                           (lambda (_)
                                                                   (parser-bind sql-in-values
                                                                                (lambda (values)
                                                                                        (parser-bind get-pos
                                                                                                     (lambda (end)
                                                                                                             (let ([vals (if (select? values)
                                                                                                                             (list (make-subquery (make-source-span start end) values))
                                                                                                                             values)])
                                                                                                                  (parser-pure (make-in-expr
                                                                                                                                (make-source-span start end)
                                                                                                                                left vals #f)))))))))
                                              (parser-pure left))))))))

;;; sql-like-expr : Parser AST
(define sql-like-expr
  (parser-bind sql-in-expr
               (lambda (left)
                       (parser-or
                        ;; NOT LIKE
                        (try (parser-bind get-pos
                                          (lambda (start)
                                                  (parser-bind (sql-keyword "NOT")
                                                               (lambda (_)
                                                                       (parser-bind (sql-keyword "LIKE")
                                                                                    (lambda (_)
                                                                                            (parser-bind sql-in-expr
                                                                                                         (lambda (pattern)
                                                                                                                 (parser-bind (optional (parser-then (sql-keyword "ESCAPE") sql-string-literal) #f)
                                                                                                                              (lambda (escape)
                                                                                                                                      (parser-bind get-pos
                                                                                                                                                   (lambda (end)
                                                                                                                                                           (parser-pure (make-like-expr
                                                                                                                                                                         (make-source-span start end)
                                                                                                                                                                         left pattern escape #t)))))))))))))))
                        ;; LIKE (without NOT)
                        (parser-bind get-pos
                                     (lambda (start)
                                             (parser-or
                                              (parser-bind (sql-keyword "LIKE")
                                                           (lambda (_)
                                                                   (parser-bind sql-in-expr
                                                                                (lambda (pattern)
                                                                                        (parser-bind (optional (parser-then (sql-keyword "ESCAPE") sql-string-literal) #f)
                                                                                                     (lambda (escape)
                                                                                                             (parser-bind get-pos
                                                                                                                          (lambda (end)
                                                                                                                                  (parser-pure (make-like-expr
                                                                                                                                                (make-source-span start end)
                                                                                                                                                left pattern escape #f))))))))))
                                              (parser-pure left))))))))

;;; sql-not-expr : Parser AST
;;; Note: NOT LIKE, NOT IN, NOT BETWEEN are handled in their respective parsers.
;;; This only handles standalone NOT prefix for boolean negation.
(define sql-not-expr
  (parser-or
   (parser-bind get-pos
                (lambda (start)
                        ;; Only match NOT if it's NOT followed by LIKE, IN, BETWEEN, EXISTS
                        ;; Those are handled by their own expression parsers
                        (parser-bind (try (parser-left (sql-keyword "NOT")
                                                       (not-followed-by (choice (list
                                                                                 (sql-keyword "LIKE")
                                                                                 (sql-keyword "IN")
                                                                                 (sql-keyword "BETWEEN")
                                                                                 (sql-keyword "EXISTS"))))))
                                     (lambda (_)
                                             (parser-bind sql-not-expr
                                                          (lambda (operand)
                                                                  (parser-bind get-pos
                                                                               (lambda (end)
                                                                                       (parser-pure (make-unary-op
                                                                                                     (make-source-span start end) 'not operand))))))))))
   sql-like-expr))

;;; sql-and-expr : Parser AST
(define sql-and-expr
  (chainl1 sql-not-expr
           (parser-then (sql-keyword "AND")
                        (parser-pure (lambda (l r) (make-binary-op no-span 'and l r))))))

;;; sql-or-expr : Parser AST
(define sql-or-expr
  (chainl1 sql-and-expr
           (parser-then (sql-keyword "OR")
                        (parser-pure (lambda (l r) (make-binary-op no-span 'or l r))))))

;;; Initialize sql-expr
(set! sql-expr sql-or-expr)

;;; ====
;;; SELECT Statement
;;; ====

;;; sql-select-item : Parser AST
(define sql-select-item
  (parser-or
   (try sql-star-expr)
   (parser-bind lazy-sql-expr
                (lambda (expr)
                        (parser-or
                         (parser-bind (parser-then (optional (sql-keyword "AS") #f) sql-identifier)
                                      (lambda (alias)
                                              (parser-pure (make-alias no-span expr alias))))
                         (parser-pure expr))))))

;;; sql-table-ref : Parser AST
(define sql-table-ref
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind sql-identifier
                                    (lambda (table-name)
                                            (parser-bind (optional (parser-then (optional (sql-keyword "AS") #f) sql-identifier) #f)
                                                         (lambda (alias)
                                                                 (parser-bind get-pos
                                                                              (lambda (end)
                                                                                      (parser-pure (make-table-ref
                                                                                                    (make-source-span start end) table-name alias)))))))))))

;;; sql-join-type : Parser Symbol
;;; Parses optional join type keyword before JOIN
(define sql-join-type
  (optional
   (choice (list
            (parser-then (sql-keyword "INNER") (parser-pure 'inner))
            (parser-then (sql-keyword "LEFT")
                         (parser-then (optional (sql-keyword "OUTER") #f) (parser-pure 'left)))
            (parser-then (sql-keyword "RIGHT")
                         (parser-then (optional (sql-keyword "OUTER") #f) (parser-pure 'right)))
            (parser-then (sql-keyword "FULL")
                         (parser-then (optional (sql-keyword "OUTER") #f) (parser-pure 'full)))
            (parser-then (sql-keyword "CROSS") (parser-pure 'cross))
            (parser-then (sql-keyword "NATURAL") (parser-pure 'natural))))
   'inner))

;;; sql-join-clause : Parser (List Symbol AST AST)
(define sql-join-clause
  (parser-bind sql-join-type
               (lambda (jtype)
                       (parser-bind (sql-keyword "JOIN")
                                    (lambda (_)
                                            (parser-bind sql-table-ref
                                                         (lambda (right)
                                                                 ;; FIX: NATURAL JOINs also don't use ON clause
                                                                 (parser-bind (if (memq jtype '(cross natural))
                                                                                  (parser-pure #f)
                                                                                  (parser-then (sql-keyword "ON") lazy-sql-expr))
                                                                              (lambda (condition)
                                                                                      (parser-pure (list jtype right condition)))))))))))

;;; sql-from-item : Parser AST
(define sql-from-item
  (parser-bind sql-table-ref
               (lambda (first-table)
                       (parser-bind (many sql-join-clause)
                                    (lambda (joins)
                                            (parser-pure
                                             (fold-left
                                              (lambda (left join-info)
                                                      (let ([jtype (car join-info)]
                                                            [right (cadr join-info)]
                                                            [cond (caddr join-info)])
                                                           (make-join no-span jtype left right cond)))
                                              first-table joins)))))))

;;; sql-order-item : Parser AST
(define sql-order-item
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind lazy-sql-expr
                                    (lambda (expr)
                                            (parser-bind (optional
                                                          (choice (list
                                                                   (parser-then (sql-keyword "ASC") (parser-pure 'asc))
                                                                   (parser-then (sql-keyword "DESC") (parser-pure 'desc))))
                                                          'asc)
                                                         (lambda (direction)
                                                                 (parser-bind (optional
                                                                               (parser-then (sql-keyword "NULLS")
                                                                                            (choice (list
                                                                                                     (parser-then (sql-keyword "FIRST") (parser-pure 'first))
                                                                                                     (parser-then (sql-keyword "LAST") (parser-pure 'last)))))
                                                                               #f)
                                                                              (lambda (nulls)
                                                                                      (parser-bind get-pos
                                                                                                   (lambda (end)
                                                                                                           (parser-pure (make-order-item
                                                                                                                         (make-source-span start end)
                                                                                                                         expr direction nulls)))))))))))))

;;; sql-limit-clause : Parser AST
(define sql-limit-clause
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "LIMIT")
                                    (lambda (_)
                                            (parser-bind sql-integer
                                                         (lambda (limit)
                                                                 (parser-bind (optional (parser-then (sql-keyword "OFFSET") sql-integer) #f)
                                                                              (lambda (offset)
                                                                                      (parser-bind get-pos
                                                                                                   (lambda (end)
                                                                                                           (parser-pure (make-limit-clause
                                                                                                                         (make-source-span start end) limit offset)))))))))))))

;;; sql-select : Parser AST
(define sql-select
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "SELECT")
                                    (lambda (_)
                                            (parser-bind (optional (parser-then (sql-keyword "DISTINCT") (parser-pure #t)) #f)
                                                         (lambda (distinct?)
                                                                 (parser-bind (sql-comma-sep1 sql-select-item)
                                                                              (lambda (columns)
                                                                                      (parser-bind (parser-then (sql-keyword "FROM")
                                                                                                                (sql-comma-sep1 sql-from-item))
                                                                                                   (lambda (from)
                                                                                                           (parser-bind (optional (parser-then (sql-keyword "WHERE") lazy-sql-expr) #f)
                                                                                                                        (lambda (where)
                                                                                                                                (parser-bind (optional
                                                                                                                                              (parser-then (sql-keyword "GROUP")
                                                                                                                                                           (parser-then (sql-keyword "BY")
                                                                                                                                                                        (sql-comma-sep1 lazy-sql-expr))) '())
                                                                                                                                             (lambda (group-by)
                                                                                                                                                     (parser-bind (optional
                                                                                                                                                                   (parser-then (sql-keyword "HAVING") lazy-sql-expr) #f)
                                                                                                                                                                  (lambda (having)
                                                                                                                                                                          (parser-bind (optional
                                                                                                                                                                                        (parser-then (sql-keyword "ORDER")
                                                                                                                                                                                                     (parser-then (sql-keyword "BY")
                                                                                                                                                                                                                  (sql-comma-sep1 sql-order-item))) '())
                                                                                                                                                                                       (lambda (order-by)
                                                                                                                                                                                               (parser-bind (option-maybe sql-limit-clause)
                                                                                                                                                                                                            (lambda (limit)
                                                                                                                                                                                                                    (parser-bind get-pos
                                                                                                                                                                                                                                 (lambda (end)
                                                                                                                                                                                                                                         (parser-pure (make-select
                                                                                                                                                                                                                                                       (make-source-span start end)
                                                                                                                                                                                                                                                       distinct? columns from where group-by having order-by
                                                                                                                                                                                                                                                       (if (nothing? limit) #f (from-just limit)))))))))))))))))))))))))))

;;; Initialize sql-select-stmt
(set! sql-select-stmt sql-select)

;;; ====
;;; INSERT Statement
;;; ====

;;; sql-values-row : Parser (List AST)
(define sql-values-row
  (sql-parens (sql-comma-sep1
               (choice (list
                        (parser-bind get-pos
                                     (lambda (start)
                                             (parser-bind (sql-keyword "DEFAULT")
                                                          (lambda (_)
                                                                  (parser-bind get-pos
                                                                               (lambda (end)
                                                                                       (parser-pure (make-default-value (make-source-span start end)))))))))
                        lazy-sql-expr)))))

;;; sql-insert : Parser AST
(define sql-insert
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "INSERT")
                                    (lambda (_)
                                            (parser-bind (sql-keyword "INTO")
                                                         (lambda (_)
                                                                 (parser-bind sql-identifier
                                                                              (lambda (table)
                                                                                      (parser-bind (optional (sql-parens (sql-comma-sep1 sql-identifier)) '())
                                                                                                   (lambda (columns)
                                                                                                           (parser-bind
                                                                                                            (choice (list
                                                                                                                     (parser-bind (sql-keyword "VALUES")
                                                                                                                                  (lambda (_)
                                                                                                                                          (parser-bind (sql-comma-sep1 sql-values-row)
                                                                                                                                                       (lambda (rows)
                                                                                                                                                               (parser-pure (make-values-clause no-span rows))))))
                                                                                                                     lazy-sql-select))
                                                                                                            (lambda (values)
                                                                                                                    (parser-bind get-pos
                                                                                                                                 (lambda (end)
                                                                                                                                         (parser-pure (make-insert
                                                                                                                                                       (make-source-span start end)
                                                                                                                                                       table columns values)))))))))))))))))

;;; ====
;;; UPDATE Statement
;;; ====

;;; sql-set-clause : Parser AST
(define sql-set-clause
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind sql-identifier
                                    (lambda (column)
                                            (parser-bind sql-eq
                                                         (lambda (_)
                                                                 (parser-bind lazy-sql-expr
                                                                              (lambda (value)
                                                                                      (parser-bind get-pos
                                                                                                   (lambda (end)
                                                                                                           (parser-pure (make-set-clause
                                                                                                                         (make-source-span start end) column value)))))))))))))

;;; sql-update : Parser AST
(define sql-update
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "UPDATE")
                                    (lambda (_)
                                            (parser-bind sql-identifier
                                                         (lambda (table)
                                                                 (parser-bind (sql-keyword "SET")
                                                                              (lambda (_)
                                                                                      (parser-bind (sql-comma-sep1 sql-set-clause)
                                                                                                   (lambda (set-clauses)
                                                                                                           (parser-bind (optional (parser-then (sql-keyword "WHERE") lazy-sql-expr) #f)
                                                                                                                        (lambda (where)
                                                                                                                                (parser-bind get-pos
                                                                                                                                             (lambda (end)
                                                                                                                                                     (parser-pure (make-update
                                                                                                                                                                   (make-source-span start end)
                                                                                                                                                                   table set-clauses where)))))))))))))))))

;;; ====
;;; DELETE Statement
;;; ====

;;; sql-delete : Parser AST
(define sql-delete
  (parser-bind get-pos
               (lambda (start)
                       (parser-bind (sql-keyword "DELETE")
                                    (lambda (_)
                                            (parser-bind (sql-keyword "FROM")
                                                         (lambda (_)
                                                                 (parser-bind sql-identifier
                                                                              (lambda (table)
                                                                                      (parser-bind (optional (parser-then (sql-keyword "WHERE") lazy-sql-expr) #f)
                                                                                                   (lambda (where)
                                                                                                           (parser-bind get-pos
                                                                                                                        (lambda (end)
                                                                                                                                (parser-pure (make-delete
                                                                                                                                              (make-source-span start end) table where)))))))))))))))

;;; ====
;;; Top-Level Statement Parser
;;; ====

;;; sql-statement : Parser AST
(define sql-statement
  (parser-then sql-whitespace
               (choice (list
                        sql-select
                        sql-insert
                        sql-update
                        sql-delete))))

;;; parse-sql-stmt : String → Either Error AST
(define (parse-sql-stmt input)
  (parse-all sql-statement input))
