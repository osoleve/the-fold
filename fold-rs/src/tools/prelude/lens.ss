; lens.ss - Functional lens utilities for nested data access
; Part of The Fold prelude

; ============================================
; Lens operations for nested alists
; ============================================

; lens-get: Get value at path in nested alist
; (lens-get '(a b) '((a . ((b . 5))))) => 5
; (lens-get '(a c) '((a . ((b . 5))))) => #f
(define (lens-get path data)
  (foldl (fn (acc key)
           (if acc
               (let ((entry (assoc key acc)))
                 (if entry (cdr entry) #f))
               #f))
         data
         path))

; lens-set: Set value at path in nested alist (returns new alist)
; (lens-set '(a b) 10 '((a . ((b . 5))))) => ((a . ((b . 10))))
(define (lens-set path value data)
  (if (null? path)
      value
      (if (null? (cdr path))
          (assoc-set (car path) value data)
          (let ((key (car path)))
            (let ((nested (assoc-ref key data)))
              (assoc-set key
                         (lens-set (cdr path) value nested)
                         data))))))

; lens-update: Update value at path using function
; (lens-update '(a b) inc '((a . ((b . 5))))) => ((a . ((b . 6))))
(define (lens-update path f data)
  (lens-set path (f (lens-get path data)) data))

; ============================================
; Module exports
; ============================================

(module-exports
  lens-get
  lens-set
  lens-update)
