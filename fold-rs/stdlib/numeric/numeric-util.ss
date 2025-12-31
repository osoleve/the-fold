; ============================================================
; Numeric Utility Functions
; List-based numeric operations and vector math
; ============================================================

; clamp-list: Clamp all values in list to range
(clamp-list (fn (lo hi lst)
               (map (fn (x) (max lo (min hi x))) lst)))

; normalize-range: Scale list values to 0-1 range
(normalize-range (fn (lst)
                    (if (null? lst)
                        '()
                        (let ((lo (foldl min (car lst) lst))
                              (hi (foldl max (car lst) lst)))
                             (if (= lo hi)
                                 (map (fn (_) 0) lst)
                                 (map (fn (x) (/ (- x lo) (- hi lo))) lst))))))

; running-sum: Cumulative sum
(running-sum (fn (lst)
                (cdr (scanl + 0 lst))))

; running-product: Cumulative product
(running-product (fn (lst)
                    (cdr (scanl * 1 lst))))

; differences: Consecutive differences
(differences (fn (lst)
                (if (null? lst)
                    '()
                    (if (null? (cdr lst))
                        '()
                        (zip-with - (cdr lst) lst)))))

; magnitude: Vector magnitude (Euclidean norm)
(magnitude (fn (v)
              (sqrt (foldl + 0 (map (fn (x) (* x x)) v)))))

; normalize-vec: Normalize vector to unit length
(normalize-vec (fn (v)
                  (let ((mag (magnitude v)))
                       (if (= mag 0)
                           v
                           (map (fn (x) (/ x mag)) v)))))

; dot-product: Dot product of vectors
(dot-product (fn (u v)
                (foldl + 0 (zip-with * u v))))

; cross-product-3d: Cross product of 3D vectors
(cross-product-3d (fn (u v)
                     (list (- (* (cadr u) (caddr v)) (* (caddr u) (cadr v)))
                           (- (* (caddr u) (car v)) (* (car u) (caddr v)))
                           (- (* (car u) (cadr v)) (* (cadr u) (car v))))))
