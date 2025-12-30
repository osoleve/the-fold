; ============================================================
; Control Flow Utilities
; Iteration, guards, and control structures
; ============================================================

; -- Iteration utilities --

; for-each: Apply function to each element for side effects
(for-each (fix for-each
               (fn (f lst)
                   (if (null? lst)
                       #t
                       (begin (f (car lst)) (for-each f (cdr lst)))))))

; for-each-indexed: Apply function with index to each element
(for-each-indexed (fn (f lst)
                      (let ((go (fix go
                                     (fn (i xs)
                                         (if (null? xs)
                                             #t
                                             (begin (f i (car xs)) (go (+ i 1) (cdr xs))))))))
                           (go 0 lst))))

; do-times: Execute function n times
(do-times (fix do-times
               (fn (n f)
                   (if (<= n 0)
                       #t
                       (begin (f) (do-times (- n 1) f))))))

; do-range: Execute function for each number in range
(do-range (fn (start end f)
              (for-each f (range start end))))

; loop-collect: Loop n times collecting results
(loop-collect (fn (n f)
                  (map f (range 0 n))))

; repeat-until: Repeat function until predicate is true
(repeat-until (fn (pred f init)
                  (let ((go (fix go
                                 (fn (val)
                                     (if (pred val)
                                         val
                                         (go (f val)))))))
                       (go init))))

; while-true: Loop while predicate holds (returns last value)
(while-true (fn (pred f init)
                (let ((go (fix go
                               (fn (val)
                                   (if (pred val)
                                       (go (f val))
                                       val)))))
                     (go init))))

; -- Guard utilities --

; guard: Return value if predicate true, else default
(guard (fn (pred value default)
           (if pred value default)))

; ensure: Assert condition, return value or call error handler
(ensure (fn (pred value on-fail)
            (if pred value (on-fail))))

; chain-guards: Chain multiple guards, return first success
(chain-guards (fix chain-guards
                   (fn (guards)
                       (if (null? guards)
                           #f
                           (let ((g (car guards)))
                                (if (car g)
                                    (cadr g)
                                    (chain-guards (cdr guards))))))))

; -- Conditional utilities --

; cond-value: Evaluate conditions and return corresponding value
(cond-value (fix cond-value
                 (fn (clauses)
                     (if (null? clauses)
                         #f
                         (let ((clause (car clauses)))
                              (if (car clause)
                                  (cadr clause)
                                  (cond-value (cdr clauses))))))))

; case-of: Match value against cases
(case-of (fn (val cases default)
             (let ((entry (assoc val cases)))
                  (if entry (cdr entry) default))))

; -- Boolean utilities --

; implies: Logical implication (p => q)
(implies (fn (p q) (or (not p) q)))

; iff: Logical equivalence (p <=> q)
(iff (fn (p q) (eq? p q)))

; nand: Not and
(nand (fn (a b) (not (and a b))))

; nor: Not or
(nor (fn (a b) (not (or a b))))

; xor: Exclusive or
(xor (fn (a b) (and (or a b) (not (and a b)))))

; both?: Check if both arguments are truthy
(both? (fn (a b) (and a b)))

; either?: Check if at least one argument is truthy
(either? (fn (a b) (or a b)))

; -- Default value utilities --

; default: Return value if truthy, else default
(default (fn (val default-val)
             (if val val default-val)))

; coalesce: Return first truthy value
(coalesce (fix coalesce
               (fn (vals)
                   (if (null? vals)
                       #f
                       (if (car vals)
                           (car vals)
                           (coalesce (cdr vals)))))))

; first-truthy: Alias for coalesce
(first-truthy coalesce)

; default-value: Alias for default
(default-value default)

; -- Error handling (functional) --

; try-catch: Execute function, return default on error
; Note: This is a pure approximation - real try/catch needs runtime support
(try-catch (fn (f default)
               (f)))  ; In pure Scheme, just execute (no actual exception handling)

; -- Short-circuit evaluation --

; short-and: And with short-circuit (list of thunks)
(short-and (fix short-and
                (fn (thunks)
                    (if (null? thunks)
                        #t
                        (if ((car thunks))
                            (short-and (cdr thunks))
                            #f)))))

; short-or: Or with short-circuit (list of thunks)
(short-or (fix short-or
               (fn (thunks)
                   (if (null? thunks)
                       #f
                       (if ((car thunks))
                           #t
                           (short-or (cdr thunks)))))))

; -- Predicate combinators --

; conjoin: Combine predicates with and (takes list of predicates)
(conjoin (fn (predicates)
             (fn (x) (all (fn (p) (p x)) predicates))))

; disjoin: Combine predicates with or (takes list of predicates)
(disjoin (fn (predicates)
             (fn (x) (any (fn (p) (p x)) predicates))))

; predicate-and: Binary predicate conjunction
(predicate-and (fn (p1 p2)
                   (fn (x) (and (p1 x) (p2 x)))))

; predicate-or: Binary predicate disjunction
(predicate-or (fn (p1 p2)
                  (fn (x) (or (p1 x) (p2 x)))))

; predicate-not: Negate predicate (alias for complement)
(predicate-not complement)

; every-pred: Create predicate that checks all predicates (takes list)
(every-pred (fn (predicates)
                (fn (x) (all (fn (p) (p x)) predicates))))

; some-pred: Create predicate that checks if any predicate holds (takes list)
(some-pred (fn (predicates)
               (fn (x) (any (fn (p) (p x)) predicates))))
