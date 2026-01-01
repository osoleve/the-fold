; ============================================================
; Predicate Combinators and Utilities
; Combining and manipulating predicate functions
; ============================================================

; both: Combine two predicates with and
; ((both positive? even?) 4) => #t
(both (fn (p1 p2)
         (fn (x) (and (p1 x) (p2 x)))))

; either-pred: Combine two predicates with or
; ((either-pred positive? even?) -2) => #t
(either-pred (fn (p1 p2)
                (fn (x) (or (p1 x) (p2 x)))))

; neither: Combine two predicates with nor
; ((neither positive? even?) -3) => #f
(neither (fn (p1 p2)
            (fn (x) (not (or (p1 x) (p2 x))))))

; negate-fn: Negate a predicate
; ((negate-fn even?) 3) => #t
(negate-fn (fn (p)
              (fn (x) (not (p x)))))

; all-of: Check if all predicates hold (binary version)
; ((all-of positive? even?) 4) => #t
(all-of (fn (p1 p2)
           (fn (x) (and (p1 x) (p2 x)))))

; any-of: Check if any predicate holds (binary version)
; ((any-of negative? even?) 4) => #t
(any-of (fn (p1 p2)
           (fn (x) (or (p1 x) (p2 x)))))

; none-of: Check if no predicates hold (binary version)
; ((none-of negative? even?) 3) => #t
(none-of (fn (p1 p2)
            (fn (x) (not (or (p1 x) (p2 x))))))

; all-of-list: Check if all predicates in list hold
; ((all-of-list (list positive? even?)) 4) => #t
(all-of-list (fn (preds)
                (fn (x)
                    (all (fn (p) (p x)) preds))))

; any-of-list: Check if any predicate in list holds
; ((any-of-list (list negative? even?)) 4) => #t
(any-of-list (fn (preds)
                (fn (x)
                    (any (fn (p) (p x)) preds))))

; none-of-list: Check if no predicates in list hold
; ((none-of-list (list negative? odd?)) 4) => #t
(none-of-list (fn (preds)
                 (fn (x)
                     (not (any (fn (p) (p x)) preds)))))

; conjoin: Alias for both
(conjoin both)

; disjoin: Alias for either-pred
(disjoin either-pred)
