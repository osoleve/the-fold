; ============================================================
; Control Flow Utilities and Threading
; Threading macros, conditional combinators, iteration
; ============================================================

; thread-first: Thread value through functions
; (thread-first 5 (list inc double)) => 12
(thread-first (fn (val fns)
                 (foldl (fn (acc f) (f acc)) val fns)))

; thread-last: Thread value as last arg through functions
(thread-last (fn (val fns)
                (foldl (fn (acc f) (f acc)) val fns)))

; pipeline: Create a pipeline of functions
; ((pipeline (list inc double)) 5) => 12
(pipeline (fn (fns)
             (fn (x)
                 (foldl (fn (acc f) (f acc)) x fns))))

; cond-fn: Create function that tests predicates in order
(cond-fn (fix cond-fn
             (fn (clauses)
                 (fn (x)
                     (if (null? clauses)
                         #f
                         (if ((car (car clauses)) x)
                             (cdr (car clauses))
                             ((cond-fn (cdr clauses)) x)))))))

; when-let-fn: Execute with value if truthy
(when-let-fn (fn (val then-fn)
                (if val (then-fn val) '())))

; if-let-fn: Bind value and branch on it being truthy
(if-let-fn (fn (val then-fn else-fn)
              (if val (then-fn val) (else-fn))))

; do-times: Execute function n times
(do-times (fix do-times
              (fn (n f)
                  (if (<= n 0)
                      '()
                      (begin
                        (f (- n 1))
                        (do-times (- n 1) f))))))

; until-fn: Repeat function until predicate holds
(until-fn (fix until-fn
              (fn (pred f x)
                  (if (pred x)
                      x
                      (until-fn pred f (f x))))))
