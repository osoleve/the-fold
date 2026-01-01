; ============================================================
; Collection - Alist Operations
; Association list manipulation and property lists
; Part of collection.ss module
; ============================================================

; ============================================
; Association List Utilities
; ============================================

; alist-get: Get value from alist by key (returns #f if not found)
(alist-get (fn (key alist)
               (let ((entry (assoc key alist)))
                    (if entry (cdr entry) #f))))

; alist-get-default: Get value or default if not found
(alist-get-default (fn (key default alist)
                       (let ((entry (assoc key alist)))
                            (if entry (cdr entry) default))))

; alist-set: Set key-value (returns new alist)
(alist-set (fn (key value alist)
               (cons (cons key value) (alist-remove key alist))))

; alist-remove: Remove key from alist
(alist-remove (fn (key alist)
                  (filter (fn (pair) (not (eq? (car pair) key))) alist)))

; alist-update: Update value with function (three-arg version for compatibility)
(alist-update (fn (key f default alist)
                  (alist-set key (f (alist-get-default key default alist)) alist)))

; alist-update-in-place: Update value for key using function (map-based)
; (alist-update-in-place 'a inc '((a . 1) (b . 2))) => ((a . 2) (b . 2))
(alist-update-in-place (fn (key f alist)
                           (map (fn (pair)
                                    (if (eq? (car pair) key)
                                        (cons key (f (cdr pair)))
                                        pair))
                                alist)))

; alist-keys: Get all keys
(alist-keys (fn (alist) (map car alist)))

; alist-values: Get all values
(alist-values (fn (alist) (map cdr alist)))

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

; alist-merge: Merge two alists (second takes precedence)
; (alist-merge '((a . 1) (b . 2)) '((b . 3) (c . 4))) => ((a . 1) (b . 3) (c . 4))
(alist-merge (fn (a1 a2)
                 (let ((keys1 (map car a1))
                       (keys2 (map car a2)))
                      (append
                       (filter (fn (pair) (not (elem? (car pair) keys2))) a1)
                       a2))))

; alist-invert: Swap keys and values
(alist-invert (fn (alist)
                  (map (fn (pair) (cons (cdr pair) (car pair))) alist)))

; alist->hash: Convert to hashtable-like structure (still alist, but cleaned)
(alist->hash (fn (alist)
                 (foldl (fn (acc pair)
                            (if (assoc (car pair) acc)
                                acc
                                (cons pair acc)))
                        '()
                        alist)))

; ============================================
; Legacy assoc-* names (for compatibility)
; ============================================

; assoc-ref: Get value for key (or #f)
(assoc-ref (fn (key alist)
               (let ((pair (assoc key alist)))
                    (if pair (cdr pair) #f))))

; assoc-set: Set value for key (returns new alist)
(assoc-set (fn (key val alist)
               (cons (cons key val)
                     (filter (fn (p) (not (eq? key (car p)))) alist))))

; assoc-remove: Remove key from alist
(assoc-remove (fn (key alist)
                  (filter (fn (p) (not (eq? key (car p)))) alist)))

; assoc-keys: Get all keys from alist
(assoc-keys (fn (alist)
                (map car alist)))

; assoc-values: Get all values from alist
(assoc-values (fn (alist)
                  (map cdr alist)))

; ============================================
; Grouping and Indexing Utilities
; ============================================

; group-by: Group list elements by key function
; (group-by car '((a 1) (b 2) (a 3))) => (((a 1) (a 3)) ((b 2)))
(group-by (fn (key-fn lst)
              (let ((keys (nub (map key-fn lst))))
                   (map (fn (k) (filter (fn (x) (eq? (key-fn x) k)) lst)) keys))))

; group-by-alist: Group list elements by key function, return alist
; (group-by-alist car '((a 1) (b 2) (a 3))) => ((a . ((a 1) (a 3))) (b . ((b 2))))
(group-by-alist (fn (key-fn lst)
                    (foldl (fn (acc x)
                               (let ((k (key-fn x)))
                                    (alist-update k
                                                  (fn (vs) (cons x vs))
                                                  '()
                                                  acc)))
                           '()
                           lst)))

; index-by: Create lookup table indexed by key function
(index-by (fn (key-fn lst)
              (map (fn (x) (cons (key-fn x) x)) lst)))

; ============================================
; Property List Utilities (key value key value ...)
; ============================================

; plist-get: Get value from property list
(plist-get (fix plist-get
                (fn (key plist)
                    (if (null? plist)
                        #f
                        (if (null? (cdr plist))
                            #f
                            (if (eq? key (car plist))
                                (cadr plist)
                                (plist-get key (cddr plist))))))))

; plist-set: Set value in property list
(plist-set (fn (key value plist)
               (cons key (cons value (plist-remove key plist)))))

; plist-remove: Remove key-value pair from property list
(plist-remove (fix plist-remove
                   (fn (key plist)
                       (if (null? plist)
                           '()
                           (if (null? (cdr plist))
                               plist
                               (if (eq? key (car plist))
                                   (plist-remove key (cddr plist))
                                   (cons (car plist)
                                         (cons (cadr plist)
                                               (plist-remove key (cddr plist))))))))))

; plist-keys: Get all keys from property list
(plist-keys (fix plist-keys
                 (fn (plist)
                     (if (null? plist)
                         '()
                         (if (null? (cdr plist))
                             '()
                             (cons (car plist) (plist-keys (cddr plist))))))))

; plist-values: Get all values from property list
(plist-values (fix plist-values
                   (fn (plist)
                       (if (null? plist)
                           '()
                           (if (null? (cdr plist))
                               '()
                               (cons (cadr plist) (plist-values (cddr plist))))))))

; plist->alist: Convert property list to alist
(plist->alist (fix plist->alist
                   (fn (plist)
                       (if (null? plist)
                           '()
                           (if (null? (cdr plist))
                               '()
                               (cons (cons (car plist) (cadr plist))
                                     (plist->alist (cddr plist))))))))

; alist->plist: Convert alist to property list
(alist->plist (fn (alist)
                  (concat (map (fn (pair) (list (car pair) (cdr pair))) alist))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
