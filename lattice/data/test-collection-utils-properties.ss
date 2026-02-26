;;; lattice/data/test-collection-utils-properties.ss — QuickCheck properties for collection utils

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'collection-utils)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-int-small
  (gen-int-range -120 120))

(define gen-int-list
  (gen-list gen-int-small))

(define (int->entity-block tag-prefix n)
  (make-block 'entity
              (string->utf8 (format "~a:~a" tag-prefix n))
              (vector)))

(define (collection-from-ints xs)
  (let* ([blocks (map (lambda (x) (int->entity-block "base" x)) xs)]
         [hashes (map hash-block blocks)])
    (make-block 'collection
                (string->utf8 "qc-collection")
                (list->vector hashes))))

(define (hash-member? h hs)
  (let loop ([rest hs])
    (and (pair? rest)
         (or (equal? h (car rest))
             (loop (cdr rest))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group collection-utils-properties

  (define-property "foldr cons identity"
    gen-int-list
    (lambda (xs)
      (equal? (foldr cons '() xs) xs))
    'tests 240)

  (define-property "collection-empty? iff collection-size is zero"
    gen-int-list
    (lambda (xs)
      (let ([c (collection-from-ints xs)])
        (equal? (collection-empty? c)
                (= (collection-size c) 0))))
    'tests 220
    'max-size 40)

  (define-property "collection-add grows size and includes new hash"
    (gen-bind gen-int-small
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-int-small)))
    (lambda (ab)
      (let* ([a (car ab)]
             [b (cdr ab)]
             [base (int->entity-block "base" a)]
             [c0 (make-block 'collection
                             (string->utf8 "single")
                             (vector (hash-block base)))]
             [new-member (int->entity-block "new" b)]
             [new-hash (hash-block new-member)]
             [c1 (collection-add c0 new-member)])
        (and (= (collection-size c1) (+ (collection-size c0) 1))
             (hash-member? new-hash (collection-hashes c1)))))
    'tests 220)

  (define-property "collection-remove undoes remove of newly-added member"
    (gen-bind gen-int-small
      (lambda (a)
        (gen-map (lambda (b) (cons a b)) gen-int-small)))
    (lambda (ab)
      (let* ([a (car ab)]
             [b (cdr ab)]
             [base (int->entity-block "base" a)]
             [c0 (make-block 'collection
                             (string->utf8 "single")
                             (vector (hash-block base)))]
             [new-member (int->entity-block "new" b)]
             [new-hash (hash-block new-member)]
             [c1 (collection-add c0 new-member)]
             [c2 (collection-remove c1 new-hash)])
        (and (= (collection-size c2) (collection-size c0))
             (not (hash-member? new-hash (collection-hashes c2))))))
    'tests 220)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
