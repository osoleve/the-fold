; ============================================================
; Function Combinators and Utilities
; Composition, currying, and higher-order function tools
; ============================================================

; -- car/cdr compositions (needed early for trampoline and other modules) --
; Two-level compositions
(caar (fn (x) (car (car x))))
(cadr (fn (x) (car (cdr x))))
(cdar (fn (x) (cdr (car x))))
(cddr (fn (x) (cdr (cdr x))))
; Three-level compositions
(caaar (fn (x) (car (car (car x)))))
(caadr (fn (x) (car (car (cdr x)))))
(cadar (fn (x) (car (cdr (car x)))))
(caddr (fn (x) (car (cdr (cdr x)))))
(cdaar (fn (x) (cdr (car (car x)))))
(cdadr (fn (x) (cdr (car (cdr x)))))
(cddar (fn (x) (cdr (cdr (car x)))))
(cdddr (fn (x) (cdr (cdr (cdr x)))))
; Four-level compositions
(caaaar (fn (x) (car (car (car (car x))))))
(caaadr (fn (x) (car (car (car (cdr x))))))
(caadar (fn (x) (car (car (cdr (car x))))))
(caaddr (fn (x) (car (car (cdr (cdr x))))))
(cadaar (fn (x) (car (cdr (car (car x))))))
(cadadr (fn (x) (car (cdr (car (cdr x))))))
(caddar (fn (x) (car (cdr (cdr (car x))))))
(cadddr (fn (x) (car (cdr (cdr (cdr x))))))
(cdaaar (fn (x) (cdr (car (car (car x))))))
(cdaadr (fn (x) (cdr (car (car (cdr x))))))
(cdadar (fn (x) (cdr (car (cdr (car x))))))
(cdaddr (fn (x) (cdr (car (cdr (cdr x))))))
(cddaar (fn (x) (cdr (cdr (car (car x))))))
(cddadr (fn (x) (cdr (cdr (car (cdr x))))))
(cdddar (fn (x) (cdr (cdr (cdr (car x))))))
(cddddr (fn (x) (cdr (cdr (cdr (cdr x))))))

; id: Identity function
(id (fn (x) x))

; const: Return a function that always returns the same value
(const (fn (x) (fn (y) x)))

; compose: Compose two functions (f . g)
(compose (fn (f g) (fn (x) (f (g x)))))

; pipe: Compose functions left-to-right (opposite of compose)
(pipe (fn (f g) (fn (x) (g (f x)))))

; flip: Swap arguments of a binary function
(flip (fn (f) (fn (x y) (f y x))))

; complement: Negate a predicate
(complement (fn (p) (fn (x) (not (p x)))))

; curry2: Curry a 2-argument function
(curry2 (fn (f) (fn (x) (fn (y) (f x y)))))

; uncurry2: Uncurry to a 2-argument function
(uncurry2 (fn (f) (fn (x y) ((f x) y))))

; on: Apply binary function to results of unary function
; Example: ((on < length) "ab" "abc") => #t
(on (fn (f g) (fn (x y) (f (g x) (g y)))))

; juxt: Apply list of functions to same argument, return list of results
; ((juxt (list inc dec)) 5) => (6 4)
(juxt (fn (funcs)
          (fn (x) (map (fn (f) (f x)) funcs))))

; compose-n: Compose a list of functions right-to-left
(compose-n (fn (funcs)
               (foldr compose id funcs)))

; pipe-n: Compose a list of functions left-to-right
(pipe-n (fn (funcs)
            (foldl (fn (acc f) (pipe acc f)) id funcs)))

; apply-both: Apply two functions to same argument, return pair
(apply-both (fn (f g)
                (fn (x) (cons (f x) (g x)))))

; converge: Apply two functions to arg, combine with third
; (converge + (list inc dec)) 5 => 10 (6 + 4)
(converge (fn (f gs)
              (fn (x) (apply f (map (fn (g) (g x)) gs)))))

; memoize-assoc: Simple memoization using alist (impure, needs mutation)
; Note: This is a pure approximation that returns a memo-capable structure
(memo-table (fn () '()))
(memo-lookup (fn (table key)
                 (let ((entry (assoc key table)))
                      (if entry (cdr entry) #f))))
(memo-insert (fn (table key value)
                 (cons (cons key value) table)))

; with-memo: Execute with memoization context
(with-memo (fn (f memo)
               (fn (x)
                   (let ((cached (memo-lookup memo x)))
                        (if cached
                            cached
                            (f x))))))

; Y combinator (fixed-point combinator)
(Y (fn (f)
       ((fn (x) (f (fn (y) ((x x) y))))
        (fn (x) (f (fn (y) ((x x) y)))))))

; constantly: Alternative to const (takes value, returns thunk-like fn)
(constantly const)

; negate-pred: Same as complement
(negate-pred complement)

; juxtapose: Same as juxt
(juxtapose juxt)

; partial2: Partially apply first argument
(partial2 (fn (f x) (fn (y) (f x y))))

; partial2-right: Partially apply second argument
(partial2-right (fn (f y) (fn (x) (f x y))))

; arity-1: Wrap function to take only first argument
(arity-1 (fn (f) (fn (x) (f x))))

; apply-n: Apply function n times, returning all intermediate values
; (apply-n inc 3 0) => (0 1 2 3)
(apply-n (fix apply-n
              (fn (f n x)
                  (if (<= n 0)
                      (list x)
                      (cons x (apply-n f (- n 1) (f x)))))))

; trampoline support: Bounce for tail-call optimization
; bounce: Create a bounce continuation (thunk wrapper)
(bounce (fn (thunk) (list 'bounce thunk)))
(bounce? (fn (x) (if (pair? x) (eq? (car x) 'bounce) #f)))
; done: Mark value as final (identity for non-bounce values)
(done (fn (x) x))
; trampoline: Run a thunk, handling bounces until final value
(trampoline (fix trampoline
                 (fn (result)
                     (if (procedure? result)
                         ; If it's a thunk, call it and recurse
                         (trampoline (result))
                         (if (bounce? result)
                             ; If it's a bounce, unwrap and recurse
                             (trampoline (cadr result))
                             ; Otherwise return the value
                             result)))))
