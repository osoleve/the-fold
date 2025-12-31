; ============================================================
; Vector Higher-Order Functions
; Map, filter, fold operations on vectors
; ============================================================

; vec-map-fn: Map function over vector
(vec-map-fn (fn (f v)
               (list->vec (map f (vec->list v)))))

; vec-filter-fn: Filter vector by predicate
(vec-filter-fn (fn (f v)
                  (list->vec (filter f (vec->list v)))))

; vec-foldl-fn: Left fold over vector
(vec-foldl-fn (fn (f init v)
                 (foldl f init (vec->list v))))

; vec-foldr-fn: Right fold over vector
(vec-foldr-fn (fn (f init v)
                 (foldr f init (vec->list v))))

; vec-any-fn: Check if any element satisfies predicate
(vec-any-fn (fn (f v)
               (any f (vec->list v))))

; vec-all-fn: Check if all elements satisfy predicate
(vec-all-fn (fn (f v)
               (all f (vec->list v))))

; vec-find-fn: Find first element matching predicate
(vec-find-fn (fn (f v)
                (find-if f (vec->list v))))

; vec-sum-fn: Sum of vector elements
(vec-sum-fn (fn (v)
               (foldl + 0 (vec->list v))))

; vec-product-fn: Product of vector elements
(vec-product-fn (fn (v)
                   (foldl * 1 (vec->list v))))
