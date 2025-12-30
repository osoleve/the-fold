; ============================================================
; Extended Alist Functions
; Canonical versions - no duplicates
; ============================================================

; alist-map: Apply function to all values in alist
; (alist-map inc '((a . 1) (b . 2))) => ((a . 2) (b . 3))
(alist-map (fn (f alist)
               (map (fn (pair) (cons (car pair) (f (cdr pair)))) alist)))

; alist-filter: Keep pairs where predicate holds on value
; (alist-filter even? '((a . 1) (b . 2) (c . 3))) => ((b . 2))
(alist-filter (fn (p alist)
                  (filter (fn (pair) (p (cdr pair))) alist)))

; alist-find: Find first pair where predicate holds on value
; (alist-find even? '((a . 1) (b . 2) (c . 3))) => (b . 2)
(alist-find (fn (p alist)
                (find-if (fn (pair) (p (cdr pair))) alist)))

; alist-update: Update value for key using function
; (alist-update 'a inc '((a . 1) (b . 2))) => ((a . 2) (b . 2))
(alist-update (fn (key f alist)
                  (map (fn (pair)
                           (if (eq? (car pair) key)
                               (cons key (f (cdr pair)))
                               pair))
                       alist)))

; alist-merge: Merge two alists (second takes precedence)
; (alist-merge '((a . 1) (b . 2)) '((b . 3) (c . 4))) => ((a . 1) (b . 3) (c . 4))
(alist-merge (fn (a1 a2)
                 (let ((keys1 (map car a1))
                       (keys2 (map car a2)))
                      (append
                       (filter (fn (pair) (not (elem? (car pair) keys2))) a1)
                       a2))))

; group-by: Group list elements by key function
; (group-by car '((a 1) (b 2) (a 3))) => (((a 1) (a 3)) ((b 2)))
(group-by (fn (key-fn lst)
              (let ((keys (nub (map key-fn lst))))
                   (map (fn (k) (filter (fn (x) (eq? (key-fn x) k)) lst)) keys))))
