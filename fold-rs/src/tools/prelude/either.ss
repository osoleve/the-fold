; ============================================================
; Either Monad
; Represents values with two possibilities: Left (error) or Right (success)
; Uses tagged list representation: (left x) or (right x)
; ============================================================

; -- Either constructors --

; left: Create a Left value (typically error)
(left (fn (x) (list 'left x)))

; right: Create a Right value (typically success)
(right (fn (x) (list 'right x)))

; -- Either predicates --

; left?: Check if value is Left
(left? (fn (e) (and (pair? e) (eq? (car e) 'left))))

; right?: Check if value is Right
(right? (fn (e) (and (pair? e) (eq? (car e) 'right))))

; -- Either extractors --

; from-left: Extract value from Left (unsafe)
(from-left (fn (e) (cadr e)))

; from-right: Extract value from Right (unsafe)
(from-right (fn (e) (cadr e)))

; -- Result aliases (ok/err convention) --

; ok: Alias for right (success)
(ok right)

; err: Alias for left (error)
(err left)

; ok?: Alias for right?
(ok? right?)

; err?: Alias for left?
(err? left?)

; ok-value: Extract value from ok result
(ok-value from-right)

; err-value: Extract value from err result
(err-value from-left)

; -- Either combinators --

; either: Case analysis on Either
; (either left-fn right-fn e)
(either (fn (f g e)
            (if (left? e)
                (f (from-left e))
                (g (from-right e)))))

; either-map: Apply function to Right value, pass through Left
(either-map (fn (f e)
                (if (right? e)
                    (right (f (from-right e)))
                    e)))

; either-map-left: Apply function to Left value, pass through Right
(either-map-left (fn (f e)
                     (if (left? e)
                         (left (f (from-left e)))
                         e)))

; either-bind: Monadic bind for Either (>>= in Haskell)
(either-bind (fn (e f)
                 (if (right? e)
                     (f (from-right e))
                     e)))

; either-apply: Apply Either function to Either value
(either-apply (fn (ef ex)
                  (either-bind ef (fn (f)
                                      (either-bind ex (fn (x)
                                                          (right (f x))))))))

; from-either: Extract value with default for Left
(from-either (fn (default e)
                 (if (right? e) (from-right e) default)))

; either-to-maybe: Convert Either to Maybe (Left becomes Nothing)
(either-to-maybe (fn (e)
                     (if (right? e) (just (from-right e)) (nothing))))

; maybe-to-either: Convert Maybe to Either (Nothing becomes Left with given error)
(maybe-to-either (fn (error-val m)
                     (if (just? m) (right (from-just m)) (left error-val))))

; lefts: Extract all Left values from list
(lefts (fn (lst)
           (map from-left (filter left? lst))))

; rights: Extract all Right values from list
(rights (fn (lst)
            (map from-right (filter right? lst))))

; partition-eithers: Split list of Eithers into (lefts rights)
(partition-eithers (fn (lst)
                       (list (lefts lst) (rights lst))))

; sequence-either: Convert list of Eithers to Either of list
; Returns first Left if any, otherwise Right of all values
(sequence-either (fix sequence-either
                      (fn (lst)
                          (if (null? lst)
                              (right '())
                              (either-bind (car lst) (fn (x)
                                                         (either-bind (sequence-either (cdr lst)) (fn (xs)
                                                                                                      (right (cons x xs))))))))))

; traverse-either: Map and sequence in one pass
(traverse-either (fn (f lst)
                     (sequence-either (map f lst))))
