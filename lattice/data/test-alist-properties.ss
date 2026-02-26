;;; lattice/data/test-alist-properties.ss — QuickCheck properties for alist helpers

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'data/alist)

;;; ============================================================================
;;; Generators
;;; ============================================================================

(define gen-key
  (gen-map (lambda (n)
             (string->symbol (string-append "k" (number->string n))))
           (gen-int-range 0 20)))

(define gen-kv
  (gen-bind gen-key
    (lambda (k)
      (gen-map (lambda (v) (cons k v)) gen-int))))

(define gen-alist
  (gen-list gen-kv))

(define (normalize-unique-alist a)
  ;; Keep one value per key (last write wins), matching map-like use.
  (fold-left (lambda (acc kv)
               (alist-set acc (car kv) (cdr kv)))
             '()
             a))

(define gen-unique-alist
  (gen-map normalize-unique-alist gen-alist))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group alist-properties

  (define-property "alist-set then alist-ref returns the stored value"
    (gen-bind gen-alist
      (lambda (a)
        (gen-bind gen-key
          (lambda (k)
            (gen-map (lambda (v) (list a k v)) gen-int)))))
    (lambda (args)
      (let* ([a (car args)]
             [k (cadr args)]
             [v (caddr args)]
             [a2 (alist-set a k v)])
        (equal? v (alist-ref a2 k))))
    'tests 240)

  (define-property "alist-remove removes key lookups"
    (gen-bind gen-unique-alist
      (lambda (a)
        (gen-map (lambda (k) (cons a k)) gen-key)))
    (lambda (args)
      (let* ([a (car args)]
             [k (cdr args)]
             [a2 (alist-remove a k)])
        (not (alist-ref a2 k))))
    'tests 220)

  (define-property "alist-merge is right-biased for conflicts"
    (gen-bind gen-key
      (lambda (k)
        (gen-bind gen-int
          (lambda (v1)
            (gen-map (lambda (v2) (list k v1 v2)) gen-int)))))
    (lambda (args)
      (let* ([k (car args)]
             [v1 (cadr args)]
             [v2 (caddr args)]
             [merged (alist-merge (list (cons k v1)) (list (cons k v2)))])
        (equal? v2 (alist-ref merged k))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
