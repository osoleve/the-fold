;;; @module trace
;;; @requires prelude
;;; @description Rewrite trace visualization
;;; @purity total
;;; @stability stable

(require 'prelude)

(doc 'module 'trace)
(doc 'description "Transformation Trace Structures

Data structures for recording step-by-step rewrite traces.
Traces capture the history of rule applications for:
  - Educational display of transformation steps
  - Verification that equivalence is preserved
  - Debugging and inspection

Trace Structure:
  ((initial . Expr)        - Starting expression
   (steps . List)          - List of trace steps
   (final . Expr)          - Result expression
   (verified? . Boolean))  - Whether equivalence was verified

Step Structure:
  ((from . Expr)           - Expression before rewrite
   (to . Expr)             - Expression after rewrite
   (rule . Symbol)         - Name of rule applied
   (position . Path)       - Path into expression tree
   (bindings . Alist))     - Variable bindings used")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'trace-step-construction)

(define (make-trace-step from to rule position bindings)
  (doc 'export #t)
  (doc 'type '(-> Expr Expr Symbol Path Alist Step))
  (doc 'description "Create a single trace step recording a rewrite.")
  `((from . ,from)
    (to . ,to)
    (rule . ,rule)
    (position . ,position)
    (bindings . ,bindings)))

(doc 'section 'trace-step-accessors)

(define (step? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (assq 'from x)
       (assq 'to x)
       (assq 'rule x)))

(define (step-from s)
  (doc 'export #t)
  (doc 'type '(-> Step Expr))
  (cdr (assq 'from s)))

(define (step-to s)
  (doc 'export #t)
  (doc 'type '(-> Step Expr))
  (cdr (assq 'to s)))

(define (step-rule s)
  (doc 'export #t)
  (doc 'type '(-> Step Symbol))
  (cdr (assq 'rule s)))

(define (step-position s)
  (doc 'export #t)
  (doc 'type '(-> Step Path))
  (let ([pos (assq 'position s)])
       (if pos (cdr pos) '())))

(define (step-bindings s)
  (doc 'export #t)
  (doc 'type '(-> Step Alist))
  (let ([binds (assq 'bindings s)])
       (if binds (cdr binds) '())))

(doc 'section 'trace-construction)

(define (make-trace initial)
  (doc 'export #t)
  (doc 'type '(-> Expr Trace))
  (doc 'description "Create a new trace starting with an initial expression.")
  `((initial . ,initial)
    (steps . ())
    (final . ,initial)
    (verified? . #f)))

(doc 'section 'trace-predicates)

(define (trace? x)
  (doc 'export #t)
  (doc 'type '(-> Any Boolean))
  (and (pair? x)
       (assq 'initial x)
       (assq 'steps x)
       (assq 'final x)))

(doc 'section 'trace-accessors)

(define (trace-initial t)
  (doc 'export #t)
  (doc 'type '(-> Trace Expr))
  (cdr (assq 'initial t)))

(define (trace-steps t)
  (doc 'export #t)
  (doc 'type '(-> Trace (List Step)))
  (cdr (assq 'steps t)))

(define (trace-final t)
  (doc 'export #t)
  (doc 'type '(-> Trace Expr))
  (cdr (assq 'final t)))

(define (trace-verified? t)
  (doc 'export #t)
  (doc 'type '(-> Trace Boolean))
  (let ([v (assq 'verified? t)])
       (if v (cdr v) #f)))

(define (trace-step-count t)
  (doc 'export #t)
  (doc 'type '(-> Trace Nat))
  (length (trace-steps t)))

(doc 'section 'trace-modification)

(define (trace-add-step trace step)
  (doc 'export #t)
  (doc 'type '(-> Trace Step Trace))
  (doc 'description "Add a step to the trace, updating the final expression.")
  `((initial . ,(trace-initial trace))
    (steps . ,(append (trace-steps trace) (list step)))
    (final . ,(step-to step))
    (verified? . #f)))

(define (trace-set-verified trace verified?)
  (doc 'export #t)
  (doc 'type '(-> Trace Boolean Trace))
  (doc 'description "Mark the trace as verified (or not).")
  `((initial . ,(trace-initial trace))
    (steps . ,(trace-steps trace))
    (final . ,(trace-final trace))
    (verified? . ,verified?)))

(define (trace-append t1 t2)
  (doc 'export #t)
  (doc 'type '(-> Trace Trace Trace))
  (doc 'description "Append two traces (second must start where first ends).")
  `((initial . ,(trace-initial t1))
    (steps . ,(append (trace-steps t1) (trace-steps t2)))
    (final . ,(trace-final t2))
    (verified? . #f)))

(doc 'section 'trace-display-helpers)

(define (trace-rules-used trace)
  (doc 'export #t)
  (doc 'type '(-> Trace (List Symbol)))
  (doc 'description "Get list of all rules used in a trace.")
  (map step-rule (trace-steps trace)))

(define (trace-rules-unique trace)
  (doc 'export #t)
  (doc 'type '(-> Trace (List Symbol)))
  (doc 'description "Get unique rules used in a trace.")
  (let loop ([rules (trace-rules-used trace)] [seen '()] [acc '()])
       (cond
        [(null? rules) (reverse acc)]
        [(memq (car rules) seen) (loop (cdr rules) seen acc)]
        [else (loop (cdr rules) (cons (car rules) seen) (cons (car rules) acc))])))

(define (step->string s)
  (doc 'export #t)
  (doc 'type '(-> Step String))
  (doc 'description "Format a step for display.")
  (format "~a: ~a → ~a"
          (step-rule s)
          (step-from s)
          (step-to s)))

(define (trace->summary t)
  (doc 'export #t)
  (doc 'type '(-> Trace String))
  (doc 'description "Brief summary of a trace.")
  (format "~a step~a: ~a → ~a"
          (trace-step-count t)
          (if (= (trace-step-count t) 1) "" "s")
          (trace-initial t)
          (trace-final t)))
