; ============================================================
; Scheme Compatibility Aliases
; Standard Scheme function names and car/cdr compositions
; ============================================================

; -- Standard Scheme aliases --

; fold-left: Alias for foldl (SRFI-1 style)
(fold-left foldl)

; fold-right: Alias for foldr (SRFI-1 style)
(fold-right foldr)

; reduce: Alias for foldl with first element as initial
(reduce (fn (f lst)
            (if (null? lst)
                '()
                (foldl f (car lst) (cdr lst)))))

; reduce-right: Reduce from right with last element as initial
(reduce-right (fn (f lst)
                  (if (null? lst)
                      '()
                      (foldr f (last lst) (init lst)))))

; for-all: Alias for all
(for-all all)

; exists: Alias for any
(exists any)

; find: Alias for find-if
(find find-if)

; filter-not: Alias for remove-if
(filter-not remove-if)

; remove is defined in list.ss (uses eq?)
; remove-equal: Remove using equal? for deep comparison
(remove-equal (fix remove-equal
                   (fn (x lst)
                       (if (null? lst)
                           '()
                           (if (equal? x (car lst))
                               (cdr lst)
                               (cons (car lst) (remove-equal x (cdr lst))))))))

; delete: Alias for remove
(delete remove)

; remq: Remove using eq?
(remq (fn (x lst)
          (filter (fn (y) (not (eq? x y))) lst)))

; assv: Association list lookup using eqv?
(assv (fn (key alist)
          (find-if (fn (pair) (eqv? key (car pair))) alist)))

; memv: Member using eqv?
(memv (fn (x lst)
          (if (null? lst)
              #f
              (if (eqv? x (car lst))
                  lst
                  (memv x (cdr lst))))))

; -- car/cdr compositions (Scheme standard) --

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

; -- Additional convenience aliases --

; first through fifth (Common Lisp style)
(first car)
(second cadr)
(third caddr)
(fourth cadddr)
(fifth (fn (x) (car (cddddr x))))

; rest: Alias for cdr
(rest cdr)

; empty?: Alias for null?
(empty? null?)

; not-empty?: Alias for non-null check
(not-empty? (fn (x) (not (null? x))))

; identity: Alias for id
(identity id)

; always: Alias for const
(always const)

; -- List construction aliases --

; iota: Generate sequence (simplified - single arity)
; (iota count start) => (start start+1 ... start+count-1)
; Use iota-from for step control
(iota (fn (count start)
          (iota-from count start 1)))

; make-list: Create list of n elements
; (make-list n) => (n copies of #f)
; (make-list n fill) => (n copies of fill)
(make-list-fill (fn (n fill)
                    (replicate n fill)))

; list-tabulate: Create list by applying function to indices
(list-tabulate (fn (n f)
                   (map f (range 0 n))))

; -- Numeric aliases --

; add1: Alias for inc
(add1 inc)

; sub1: Alias for dec
(sub1 dec)

; modulo: Alias for mod
(modulo mod)

; quotient-remainder: Return both quotient and remainder
(quotient-remainder (fn (a b)
                        (cons (/ a b) (mod a b))))

; exact-integer-sqrt: Integer square root
(exact-integer-sqrt (fn (n)
                        (floor (sqrt n))))

; -- String aliases --

; string-null?: Alias for string-empty?
(string-null? string-empty?)

; string-head: First character
(string-head (fn (s) (string-ref s 0)))

; string-tail: All but first character
(string-tail (fn (s) (substring s 1 (string-length s))))

; -- Boolean aliases --

; true: Constant #t
(true #t)

; false: Constant #f
(false #f)

; -- Pair/cons aliases --

; make-pair: Alias for cons
(make-pair cons)

; pair-first: Alias for car
(pair-first car)

; pair-second: Alias for cdr
(pair-second cdr)

; -- Vector operations (list-based for now) --

; vector->list: Convert vector to list (identity for now)
(vector->list id)

; list->vector: Convert list to vector (identity for now)
(list->vector id)

; vector-length: Length of vector
(vector-length length)

; vector-ref: Reference vector element
(vector-ref nth)

; vector-map: Map over vector
(vector-map map)

; vector-for-each: For-each over vector
(vector-for-each for-each)

; -- Additional utilities --

; values: Return multiple values (as list for now)
(values list)

; call-with-values: Call producer, apply consumer to results
(call-with-values (fn (producer consumer)
                      (apply consumer (producer))))

; receive: Destructure multiple values (simplified)
(receive (fn (formals producer body)
             (apply body (producer))))

; dedup-consecutive: Remove consecutive duplicates (alias)
(dedup-consecutive dedupe)

; take-n: Alias for take
(take-n take)

; drop-n: Alias for drop
(drop-n drop)
