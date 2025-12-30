; ============================================================
; Collection Utilities
; Dictionary (alist-based) and Set (list-based) operations
; ============================================================

; ============================================
; Dictionary Operations (alist-based)
; ============================================

; dict-new: Create empty dictionary
(dict-new (fn () '()))

; dict-set: Set key-value pair (returns new dict)
(dict-set (fn (d k v)
              (cons (cons k v) (dict-remove d k))))

; dict-get: Get value for key, or default
(dict-get (fn (d k default)
              (let ((entry (assoc k d)))
                   (if entry (cdr entry) default))))

; dict-get-in: Get nested value using path of keys
(dict-get-in (fix dict-get-in
                  (fn (d keys default)
                      (if (null? keys)
                          d
                          (let ((entry (assoc (car keys) d)))
                               (if entry
                                   (dict-get-in (cdr entry) (cdr keys) default)
                                   default))))))

; dict-remove: Remove key from dictionary
(dict-remove (fn (d k)
                 (filter (fn (pair) (not (eq? (car pair) k))) d)))

; dict-has?: Check if key exists
(dict-has? (fn (d k)
               (if (assoc k d) #t #f)))

; dict-keys: Get all keys
(dict-keys (fn (d) (map car d)))

; dict-values: Get all values
(dict-values (fn (d) (map cdr d)))

; dict-size: Get number of entries
(dict-size length)

; dict-empty?: Check if dictionary is empty
(dict-empty? null?)

; dict-update: Update value with function
(dict-update (fn (d k f default)
                 (dict-set d k (f (dict-get d k default)))))

; dict-merge: Merge two dictionaries (second wins on conflict)
(dict-merge (fn (d1 d2)
                (foldl (fn (acc pair) (dict-set acc (car pair) (cdr pair))) d1 d2)))

; dict-filter: Filter dictionary by predicate on (key . value)
(dict-filter filter)

; dict-map-values: Map function over values
(dict-map-values (fn (f d)
                     (map (fn (pair) (cons (car pair) (f (cdr pair)))) d)))

; dict-map-keys: Map function over keys
(dict-map-keys (fn (f d)
                   (map (fn (pair) (cons (f (car pair)) (cdr pair))) d)))

; dict-from-lists: Create dict from key list and value list
(dict-from-lists (fn (keys vals)
                     (zip keys vals)))

; dict-to-list: Convert dict to list of (key value) pairs
(dict-to-list (fn (d)
                  (map (fn (pair) (list (car pair) (cdr pair))) d)))

; dict-invert: Swap keys and values
(dict-invert (fn (d)
                 (map (fn (pair) (cons (cdr pair) (car pair))) d)))

; dict-select-keys: Keep only specified keys
(dict-select-keys (fn (d keys)
                      (filter (fn (pair) (member? (car pair) keys)) d)))

; dict-dissoc: Remove multiple keys
(dict-dissoc (fn (d keys)
                 (filter (fn (pair) (not (member? (car pair) keys))) d)))

; ============================================
; Set Operations (list-based)
; ============================================

; set-new: Create empty set
(set-new (fn () '()))

; set-add: Add element to set
(set-add (fn (s x)
             (if (member? x s) s (cons x s))))

; set-remove: Remove element from set
(set-remove remove)

; set-member?: Check if element is in set
(set-member? member?)

; set-size: Get number of elements
(set-size length)

; set-empty?: Check if set is empty
(set-empty? null?)

; set-from-list: Create set from list (removes duplicates)
(set-from-list nub)

; set-to-list: Convert set to list
(set-to-list id)

; union: Union of two lists/sets
(union (fn (xs ys)
           (foldl (fn (acc x) (if (member? x acc) acc (cons x acc))) ys xs)))

; intersection: Intersection of two lists/sets
(intersection (fn (xs ys)
                  (filter (fn (x) (member? x ys)) xs)))

; difference: Difference of two lists/sets (xs - ys)
(difference (fn (xs ys)
                (filter (fn (x) (not (member? x ys))) xs)))

; symmetric-difference: Elements in exactly one of the sets
(symmetric-difference (fn (xs ys)
                          (append (difference xs ys) (difference ys xs))))

; subset?: Check if xs is subset of ys
(subset? (fn (xs ys)
             (all (fn (x) (member? x ys)) xs)))

; set-union: Alias for union
(set-union union)

; set-intersection: Alias for intersection
(set-intersection intersection)

; set-difference: Alias for difference
(set-difference difference)

; set-symmetric-difference: Alias for symmetric-difference
(set-symmetric-difference symmetric-difference)

; set-subset?: Alias for subset?
(set-subset? subset?)

; set-equal?: Check if two sets have same elements
(set-equal? (fn (s1 s2)
                (and (set-subset? s1 s2) (set-subset? s2 s1))))

; set-disjoint?: Check if two sets have no common elements
(set-disjoint? (fn (s1 s2)
                   (null? (set-intersection s1 s2))))

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

; alist-update: Update value with function
(alist-update (fn (key f default alist)
                  (alist-set key (f (alist-get-default key default alist)) alist)))

; alist-keys: Get all keys
(alist-keys (fn (alist) (map car alist)))

; alist-values: Get all values
(alist-values (fn (alist) (map cdr alist)))

; alist->hash: Convert to hashtable-like structure (still alist, but cleaned)
(alist->hash (fn (alist)
                 (foldl (fn (acc pair)
                            (if (assoc (car pair) acc)
                                acc
                                (cons pair acc)))
                        '()
                        alist)))

; group-by: Group list elements by key function
(group-by (fn (key-fn lst)
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
