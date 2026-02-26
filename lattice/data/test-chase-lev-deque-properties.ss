;;; lattice/data/test-chase-lev-deque-properties.ss — QuickCheck properties for chase-lev deque

(load "core/testing/test-framework.ss")
(load "core/lang/module.ss")
(require 'quickcheck)
(require 'chase-lev-deque)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define gen-task
  (gen-int-range -200 200))

(define gen-task-list
  (gen-list gen-task))

(define (push-all! d xs)
  (for-each (lambda (x) (deque-push! d x)) xs))

(define (pop-all! d)
  (let loop ([acc '()])
    (let ([x (deque-pop! d)])
      (if (eq? x 'empty)
          (reverse acc)
          (loop (cons x acc))))))

(define (steal-all! d)
  (let loop ([acc '()])
    (let ([x (deque-steal! d)])
      (if (eq? x 'empty)
          (reverse acc)
          (loop (cons x acc))))))

;;; ============================================================================
;;; Properties
;;; ============================================================================

(test-group chase-lev-deque-properties

  (define-property "push then pop yields reverse order"
    gen-task-list
    (lambda (xs)
      (let ([d (make-chase-lev-deque 256)])
        (push-all! d xs)
        (equal? (pop-all! d) (reverse xs))))
    'tests 220
    'max-size 40)

  (define-property "push then steal yields insertion order"
    gen-task-list
    (lambda (xs)
      (let ([d (make-chase-lev-deque 256)])
        (push-all! d xs)
        (equal? (steal-all! d) xs)))
    'tests 200
    'max-size 40)

  (define-property "size tracks number of pushes and pops"
    gen-task-list
    (lambda (xs)
      (let ([d (make-chase-lev-deque 256)])
        (push-all! d xs)
        (and (= (deque-size d) (length xs))
             (begin (pop-all! d)
                    (and (= (deque-size d) 0)
                         (deque-empty? d))))))
    'tests 200
    'max-size 40)
)

;;; ============================================================================
;;; Run
;;; ============================================================================

(run-all-tests-and-exit)
