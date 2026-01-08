;;; core/fp/rewrite/rule.ss — Rewrite Rule Data Structures
;;;
;;; Core data structures for representing rewrite rules.
;;; A rule captures LHS → RHS transformation with pattern variables.
;;;
;;; Pattern Language:
;;;   (?x)           - Metavariable, matches any expression, binds to x
;;;   (?x number)    - Constrained metavar, must satisfy predicate
;;;   literal        - Must match exactly
;;;   (f (?x) (?y))  - Structural match with nested metavars
;;;
;;; Rule Structure:
;;;   ((name . Symbol)           - Rule identifier
;;;    (lhs . Pattern)           - Left-hand side pattern
;;;    (rhs . Template)          - Right-hand side template
;;;    (category . Symbol)       - e.g., 'monoid, 'functor, 'monad
;;;    (direction . Symbol)      - 'forward, 'backward, 'bidirectional
;;;    (conditions . List))      - Optional side conditions
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Helper: Option Extraction
;;; ============================================================

;;; get-opt : List × Symbol × Any → Any
;;; Extract an option from a plist-style options list.
(define (get-opt opts key default)
  (let loop ([opts opts])
       (cond
        [(null? opts) default]
        [(null? (cdr opts)) default]
        [(eq? (car opts) key) (cadr opts)]
        [else (loop (cddr opts))])))

;;; ============================================================
;;; Rule Construction
;;; ============================================================

;;; make-rule : Symbol × Pattern × Template × Opts... → Rule
;;; Create a rewrite rule.
;;; Options:
;;;   'category sym  - Rule category (default: 'custom)
;;;   'direction sym - Rule direction (default: 'forward)
;;;   'conditions ls - Side conditions (default: '())
(define (make-rule name lhs rhs . opts)
  `((name . ,name)
    (lhs . ,lhs)
    (rhs . ,rhs)
    (category . ,(get-opt opts 'category 'custom))
    (direction . ,(get-opt opts 'direction 'forward))
    (conditions . ,(get-opt opts 'conditions '()))))

;;; ============================================================
;;; Rule Predicates
;;; ============================================================

;;; rule? : Any → Boolean
;;; Check if x is a valid rule structure.
(define (rule? x)
  (and (pair? x)
       (assq 'name x)
       (assq 'lhs x)
       (assq 'rhs x)))

;;; ============================================================
;;; Rule Accessors
;;; ============================================================

;;; rule-name : Rule → Symbol
(define (rule-name r)
  (cdr (assq 'name r)))

;;; rule-lhs : Rule → Pattern
(define (rule-lhs r)
  (cdr (assq 'lhs r)))

;;; rule-rhs : Rule → Template
(define (rule-rhs r)
  (cdr (assq 'rhs r)))

;;; rule-category : Rule → Symbol
(define (rule-category r)
  (let ([cat (assq 'category r)])
       (if cat (cdr cat) 'custom)))

;;; rule-direction : Rule → Symbol
(define (rule-direction r)
  (let ([dir (assq 'direction r)])
       (if dir (cdr dir) 'forward)))

;;; rule-conditions : Rule → List
(define (rule-conditions r)
  (let ([cond (assq 'conditions r)])
       (if cond (cdr cond) '())))

;;; ============================================================
;;; Metavariable Detection
;;; ============================================================

;;; metavar? : Any → Boolean
;;; Check if pattern element is a metavariable.
;;; Metavariables are lists starting with a ?-prefixed symbol.
(define (metavar? x)
  (and (pair? x)
       (symbol? (car x))
       (let ([s (symbol->string (car x))])
            (and (> (string-length s) 0)
                 (char=? (string-ref s 0) #\?)))))

;;; metavar-name : Pattern → Symbol
;;; Extract the variable name from a metavariable.
;;; (?x) → x, (?foo constraint) → foo
(define (metavar-name mv)
  (let* ([sym (car mv)]
         [s (symbol->string sym)])
        (string->symbol (substring s 1 (string-length s)))))

;;; metavar-constraint : Pattern → Symbol | #f
;;; Extract the constraint from a metavariable, if any.
;;; (?x) → #f, (?x number) → number
(define (metavar-constraint mv)
  (if (and (pair? mv) (pair? (cdr mv)))
      (cadr mv)
      #f))

;;; ============================================================
;;; Pattern Utilities
;;; ============================================================

;;; pattern-vars : Pattern → (List Symbol)
;;; Collect all metavariable names from a pattern.
(define (pattern-vars pattern)
  (cond
   [(metavar? pattern)
    (list (metavar-name pattern))]
   [(pair? pattern)
    (append (pattern-vars (car pattern))
            (pattern-vars (cdr pattern)))]
   [else '()]))

;;; pattern-vars-unique : Pattern → (List Symbol)
;;; Collect unique metavariable names from a pattern.
(define (pattern-vars-unique pattern)
  (let ([vars (pattern-vars pattern)])
       (let loop ([vs vars] [seen '()] [acc '()])
            (cond
             [(null? vs) (reverse acc)]
             [(memq (car vs) seen) (loop (cdr vs) seen acc)]
             [else (loop (cdr vs) (cons (car vs) seen) (cons (car vs) acc))]))))

;;; ============================================================
;;; Rule Display
;;; ============================================================

;;; rule->string : Rule → String
;;; Format a rule for display.
(define (rule->string r)
  (format "~a: ~a → ~a [~a]"
          (rule-name r)
          (rule-lhs r)
          (rule-rhs r)
          (rule-category r)))

;;; ============================================================
;;; Rule Validation
;;; ============================================================

;;; valid-rule? : Rule → Boolean
;;; Check if a rule is well-formed:
;;; - All RHS variables must appear in LHS
(define (valid-rule? r)
  (let ([lhs-vars (pattern-vars-unique (rule-lhs r))]
        [rhs-vars (pattern-vars-unique (rule-rhs r))])
       (for-all (lambda (v) (memq v lhs-vars)) rhs-vars)))
