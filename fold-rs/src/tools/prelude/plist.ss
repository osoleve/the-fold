; ============================================================
; Property List Utilities
; Operations on alternating key-value lists
; ============================================================

; plist-get: Get value for key in property list
(plist-get (fix plist-get
               (fn (key plist)
                   (if (null? plist)
                       #f
                       (if (null? (cdr plist))
                           #f
                           (if (eq? key (car plist))
                               (cadr plist)
                               (plist-get key (cddr plist))))))))

; plist-set: Set value for key in property list
(plist-set (fix plist-set
               (fn (key value plist)
                   (if (null? plist)
                       (list key value)
                       (if (null? (cdr plist))
                           (list key value)
                           (if (eq? key (car plist))
                               (cons key (cons value (cddr plist)))
                               (cons (car plist)
                                     (cons (cadr plist)
                                           (plist-set key value (cddr plist))))))))))

; plist-remove: Remove key from property list
(plist-remove (fix plist-remove
                  (fn (key plist)
                      (if (null? plist)
                          '()
                          (if (null? (cdr plist))
                              '()
                              (if (eq? key (car plist))
                                  (cddr plist)
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

; plist-has-key?: Check if key exists in property list
(plist-has-key? (fn (key plist)
                   (if (plist-get key plist) #t #f)))
