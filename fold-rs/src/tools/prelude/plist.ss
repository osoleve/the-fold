; plist.ss - Property list utilities
; Part of The Fold prelude
;
; Property lists (plists) are flat lists of alternating keys and values:
; '(name "Alice" age 30 active #t) represents {name: "Alice", age: 30, active: true}

; ============================================
; Property list operations
; ============================================

; plist-get: Get value for key in property list
; (plist-get 'age '(name "Alice" age 30)) => 30
; (plist-get 'city '(name "Alice" age 30)) => #f
(define (plist-get key plist)
  (if (null? plist)
      #f
      (if (null? (cdr plist))
          #f
          (if (eq? key (car plist))
              (cadr plist)
              (plist-get key (cddr plist))))))

; plist-set: Set value for key in property list (maintains position)
; (plist-set 'age 31 '(name "Alice" age 30)) => (name "Alice" age 31)
; (plist-set 'city "NYC" '(name "Alice" age 30)) => (city "NYC")
(define (plist-set key value plist)
  (if (null? plist)
      (list key value)
      (if (null? (cdr plist))
          (list key value)
          (if (eq? key (car plist))
              (cons key (cons value (cddr plist)))
              (cons (car plist)
                    (cons (cadr plist)
                          (plist-set key value (cddr plist))))))))

; plist-remove: Remove key from property list
; (plist-remove 'age '(name "Alice" age 30)) => (name "Alice")
(define (plist-remove key plist)
  (if (null? plist)
      '()
      (if (null? (cdr plist))
          '()
          (if (eq? key (car plist))
              (cddr plist)
              (cons (car plist)
                    (cons (cadr plist)
                          (plist-remove key (cddr plist))))))))

; plist-keys: Get all keys from property list
; (plist-keys '(name "Alice" age 30)) => (name age)
(define (plist-keys plist)
  (if (null? plist)
      '()
      (if (null? (cdr plist))
          '()
          (cons (car plist) (plist-keys (cddr plist))))))

; plist-values: Get all values from property list
; (plist-values '(name "Alice" age 30)) => ("Alice" 30)
(define (plist-values plist)
  (if (null? plist)
      '()
      (if (null? (cdr plist))
          '()
          (cons (cadr plist) (plist-values (cddr plist))))))

; plist-has-key?: Check if key exists in property list
; (plist-has-key? 'age '(name "Alice" age 30)) => #t
(define (plist-has-key? key plist)
  (if (plist-get key plist) #t #f))

; ============================================
; Module exports
; ============================================

(module-exports
  plist-get
  plist-set
  plist-remove
  plist-keys
  plist-values
  plist-has-key?)
