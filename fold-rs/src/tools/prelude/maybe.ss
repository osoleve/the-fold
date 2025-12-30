; ============================================================
; Maybe Monad
; Option type for optional values
; Uses tagged list representation: (just x) or (nothing)
; ============================================================

; -- Tagged Maybe type --

; just: Create a Just value
(just (fn (x) (list 'just x)))

; nothing: Create a Nothing value
(nothing (fn () (list 'nothing)))

; just?: Check if value is Just
(just? (fn (m) (and (pair? m) (eq? (car m) 'just))))

; nothing?: Check if value is Nothing
(nothing? (fn (m) (and (pair? m) (eq? (car m) 'nothing))))

; from-just: Extract value from Just
(from-just (fn (m) (cadr m)))

; -- Maybe combinators --

; maybe-map: Apply function to Just value, pass through Nothing
(maybe-map (fn (f m)
               (if (just? m)
                   (just (f (from-just m)))
                   m)))

; maybe-bind: Monadic bind for Maybe
(maybe-bind (fn (m f)
                (if (just? m)
                    (f (from-just m))
                    m)))

; maybe-apply: Apply Maybe function to Maybe value
(maybe-apply (fn (mf mx)
                 (maybe-bind mf (fn (f)
                                    (maybe-bind mx (fn (x)
                                                       (just (f x))))))))

; maybe-or: Return first Just, or last Nothing
(maybe-or (fn (m1 m2)
              (if (just? m1) m1 m2)))

; maybe-and: Return second if both Just, else Nothing
(maybe-and (fn (m1 m2)
               (if (just? m1) m2 (nothing))))

; from-maybe: Get value or default if Nothing
(from-maybe (fn (default m)
                (if (just? m) (from-just m) default)))

; maybe-to-list: Convert Maybe to list (empty or singleton)
(maybe-to-list (fn (m)
                   (if (just? m) (list (from-just m)) '())))

; list-to-maybe: First element as Just, or Nothing if empty
(list-to-maybe (fn (lst)
                   (if (null? lst) (nothing) (just (car lst)))))

; cat-maybes: Filter out Nothings, extract Just values
(cat-maybes (fn (lst)
                (map from-just (filter just? lst))))

; map-maybe: Map and filter in one pass
(map-maybe (fn (f lst)
               (cat-maybes (map f lst))))

; -- Simple #f-based Maybe utilities (alternative representation) --

; maybe: Apply function if value is not #f, else return default
(maybe (fn (default f x)
           (if x (f x) default)))

; from-maybe-simple: Get value or default if #f
(from-maybe-simple (fn (default x)
                       (if x x default)))

; map-maybe-simple: Map over list, keeping only non-#f results
(map-maybe-simple (fn (f lst)
                      (filter id (map f lst))))

; cat-maybes-simple: Filter out #f values from list
(cat-maybes-simple (fn (lst) (filter id lst)))
