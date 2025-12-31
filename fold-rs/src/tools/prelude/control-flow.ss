; control-flow.ss - Control flow utilities and threading macros
; Part of The Fold prelude

; ============================================
; Threading macros
; ============================================

; thread-first: Thread value through functions (value as first arg)
; (thread-first 5 (list inc double)) => 12  ; (double (inc 5))
(define (thread-first val fns)
  (foldl (fn (acc f) (f acc)) val fns))

; thread-last: Thread value as last arg through functions
; For single-arg functions, same as thread-first
(define (thread-last val fns)
  (foldl (fn (acc f) (f acc)) val fns))

; pipeline: Create a pipeline of functions
; ((pipeline (list inc double)) 5) => 12
(define (pipeline fns)
  (fn (x)
    (foldl (fn (acc f) (f acc)) x fns)))

; ============================================
; Conditional combinators
; ============================================

; cond-fn: Create function that tests predicates in order
; ((cond-fn (list (cons even? "even") (cons odd? "odd"))) 3) => "odd"
(define (cond-fn clauses)
  (fn (x)
    (if (null? clauses)
        #f
        (if ((car (car clauses)) x)
            (cdr (car clauses))
            ((cond-fn (cdr clauses)) x)))))

; when-let-fn: Execute with value if truthy
; (when-let-fn (get-val) (fn (v) (process v))) => result or '()
(define (when-let-fn val then-fn)
  (if val (then-fn val) '()))

; if-let-fn: Bind value and branch on it being truthy
; (if-let-fn (get-val) (fn (v) (process v)) (fn () (default))) => result
(define (if-let-fn val then-fn else-fn)
  (if val (then-fn val) (else-fn)))

; ============================================
; Iteration utilities
; ============================================

; do-times: Execute function n times (for side effects)
; (do-times 5 (fn (i) (display i))) => void
(define (do-times n f)
  (if (<= n 0)
      '()
      (begin
        (f (- n 1))
        (do-times (- n 1) f))))

; until: Repeat function until predicate holds
; (until zero? dec 5) => 0
(define (until pred f x)
  (if (pred x)
      x
      (until pred f (f x))))

; ============================================
; Module exports
; ============================================

(module-exports
  thread-first
  thread-last
  pipeline
  cond-fn
  when-let-fn
  if-let-fn
  do-times
  until)
