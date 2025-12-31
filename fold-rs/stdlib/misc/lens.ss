; ============================================================
; Functional Lens Utilities
; Nested data access and update operations
; ============================================================

; lens-get: Get value at path in nested alist
(lens-get (fn (path data)
             (foldl (fn (acc key)
                       (if acc
                           (let ((entry (assoc key acc)))
                                (if entry (cdr entry) #f))
                           #f))
                    data
                    path)))

; lens-set: Set value at path in nested alist
(lens-set (fix lens-set
              (fn (path value data)
                  (if (null? path)
                      value
                      (if (null? (cdr path))
                          (assoc-set (car path) value data)
                          (let ((key (car path)))
                               (let ((nested (assoc-ref key data)))
                                    (assoc-set key
                                               (lens-set (cdr path) value nested)
                                               data))))))))

; lens-update: Update value at path using function
(lens-update (fn (path f data)
                (lens-set path (f (lens-get path data)) data)))
