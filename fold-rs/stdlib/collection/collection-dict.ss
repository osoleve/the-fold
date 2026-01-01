; ============================================================
; Collection - Dictionary Operations
; Higher-level dictionary abstraction (alist-based)
; Part of collection.ss module
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

; --- Module Exports ---
; (see exports.ss for exported symbols)
