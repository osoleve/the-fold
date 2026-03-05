;;; @module rule
;;; @requires prelude

(require 'prelude)

(doc 'module 'rule)
(doc 'description "Rewrite Rule Data Structures

Core data structures for representing rewrite rules.
A rule captures LHS → RHS transformation with pattern variables.

Pattern Language:
  (?x)           - Metavariable, matches any expression, binds to x
  (?x number)    - Constrained metavar, must satisfy predicate
  literal        - Must match exactly
  (f (?x) (?y))  - Structural match with nested metavars

Rule Structure:
  ((name . Symbol)           - Rule identifier
   (lhs . Pattern)           - Left-hand side pattern
   (rhs . Template)          - Right-hand side template
   (category . Symbol)       - e.g., 'monoid, 'functor, 'monad
   (direction . Symbol)      - 'forward, 'backward, 'bidirectional
   (conditions . List))      - Optional side conditions")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'helper-option-extraction)

(define (get-opt opts key default)
  (doc 'export #t)
  (doc 'type '(-> List Symbol Any Any))
  (doc 'description "Extract an option from a plist-style options list.")
  (let loop ([opts opts])
       (cond
        [(null? opts) default]
        [(null? (cdr opts)) default]
        [(eq? (car opts) key) (cadr opts)]
        [else (loop (cddr opts))])))

(doc 'section 'rule-construction)

(define (make-rule name lhs rhs . opts)
  (doc 'export #t)
  (doc 'type '(-> Symbol Pattern Template Options Rule))
  (doc 'description "Create a rewrite rule.
Options:
  'category sym  - Rule category (default: 'custom)
  'direction sym - Rule direction (default: 'forward)
  'conditions ls - Side conditions (default: '())")
  `((name . ,name)
    (lhs . ,lhs)
    (rhs . ,rhs)
    (category . ,(get-opt opts 'category 'custom))
    (direction . ,(get-opt opts 'direction 'forward))
    (conditions . ,(get-opt opts 'conditions '()))))

(doc 'section 'rule-predicates)

(define (rule? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if x is a valid rule structure.")
  (and (pair? x)
       (assq 'name x)
       (assq 'lhs x)
       (assq 'rhs x)))

(doc 'section 'rule-accessors)

(define (rule-name r)
  (doc 'export #t)
  (doc 'type '(-> Rule Symbol))
  (cdr (assq 'name r)))

(define (rule-lhs r)
  (doc 'export #t)
  (doc 'type '(-> Rule Pattern))
  (cdr (assq 'lhs r)))

(define (rule-rhs r)
  (doc 'export #t)
  (doc 'type '(-> Rule Template))
  (cdr (assq 'rhs r)))

(define (rule-category r)
  (doc 'export #t)
  (doc 'type '(-> Rule Symbol))
  (let ([cat (assq 'category r)])
       (if cat (cdr cat) 'custom)))

(define (rule-direction r)
  (doc 'export #t)
  (doc 'type '(-> Rule Symbol))
  (let ([dir (assq 'direction r)])
       (if dir (cdr dir) 'forward)))

(define (rule-conditions r)
  (doc 'export #t)
  (doc 'type '(-> Rule List))
  (let ([cond (assq 'conditions r)])
       (if cond (cdr cond) '())))

(doc 'section 'metavariable-detection)

(define (metavar? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (doc 'description "Check if pattern element is a metavariable.
Metavariables are lists starting with a ?-prefixed symbol.")
  (and (pair? x)
       (symbol? (car x))
       (let ([s (symbol->string (car x))])
            (and (> (string-length s) 0)
                 (char=? (string-ref s 0) #\?)))))

(define (metavar-name mv)
  (doc 'export #t)
  (doc 'type '(-> Pattern Symbol))
  (doc 'description "Extract the variable name from a metavariable.
(?x) → x, (?foo constraint) → foo")
  (let* ([sym (car mv)]
         [s (symbol->string sym)])
        (string->symbol (substring s 1 (string-length s)))))

(define (metavar-constraint mv)
  (doc 'export #t)
  (doc 'type '(-> Pattern (Union Symbol Boolean)))
  (doc 'description "Extract the constraint from a metavariable, if any.
(?x) → #f, (?x number) → number")
  (if (and (pair? mv) (pair? (cdr mv)))
      (cadr mv)
      #f))

(doc 'section 'pattern-utilities)

(define (pattern-vars pattern)
  (doc 'export #t)
  (doc 'type '(-> Pattern (List Symbol)))
  (doc 'description "Collect all metavariable names from a pattern.")
  (cond
   [(metavar? pattern)
    (list (metavar-name pattern))]
   [(pair? pattern)
    (append (pattern-vars (car pattern))
            (pattern-vars (cdr pattern)))]
   [else '()]))

(define (pattern-vars-unique pattern)
  (doc 'export #t)
  (doc 'type '(-> Pattern (List Symbol)))
  (doc 'description "Collect unique metavariable names from a pattern.")
  (let ([vars (pattern-vars pattern)])
       (let loop ([vs vars] [seen '()] [acc '()])
            (cond
             [(null? vs) (reverse acc)]
             [(memq (car vs) seen) (loop (cdr vs) seen acc)]
             [else (loop (cdr vs) (cons (car vs) seen) (cons (car vs) acc))]))))

(doc 'section 'rule-display)

(define (rule->string r)
  (doc 'export #t)
  (doc 'type '(-> Rule String))
  (doc 'description "Format a rule for display.")
  (format "~a: ~a → ~a [~a]"
          (rule-name r)
          (rule-lhs r)
          (rule-rhs r)
          (rule-category r)))

(doc 'section 'rule-validation)

(define (valid-rule? r)
  (doc 'export #t)
  (doc 'type '(-> Rule Boolean))
  (doc 'description "Check if a rule is well-formed:
- All RHS variables must appear in LHS")
  (let ([lhs-vars (pattern-vars-unique (rule-lhs r))]
        [rhs-vars (pattern-vars-unique (rule-rhs r))])
       (for-all (lambda (v) (memq v lhs-vars)) rhs-vars)))
