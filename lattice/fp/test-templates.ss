;;; lattice/fp/test-templates.ss — Tests for FP Templates
;;;
;;; Comprehensive tests for monoids, foldables, functors,
;;; applicatives, lenses, and prisms.

(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(load "lattice/fp/templates.ss")

;;; ====
;;; Monoid Tests
;;; ====

(test-group monoid-basics
            
            (define-test monoid-construction
              (let ([m (make-monoid 0 +)])
                   (assert-true (monoid? m))
                   (assert-equal 0 (monoid-mempty m))))
            
            (define-test monoid-sum-operations
              (assert-equal 0 (monoid-mempty monoid-sum))
              (assert-equal 15 ((monoid-mappend monoid-sum) 10 5)))
            
            (define-test monoid-product-operations
              (assert-equal 1 (monoid-mempty monoid-product))
              (assert-equal 50 ((monoid-mappend monoid-product) 10 5)))
            
            (define-test monoid-list-operations
              (assert-equal '() (monoid-mempty monoid-list))
              (assert-equal '(1 2 3 4) ((monoid-mappend monoid-list) '(1 2) '(3 4))))
            
            (define-test monoid-string-operations
              (assert-equal "" (monoid-mempty monoid-string))
              (assert-equal "hello world" ((monoid-mappend monoid-string) "hello " "world")))
            
            (define-test monoid-all-operations
              (assert-equal #t (monoid-mempty monoid-all))
              (assert-equal #t ((monoid-mappend monoid-all) #t #t))
              (assert-equal #f ((monoid-mappend monoid-all) #t #f)))
            
            (define-test monoid-any-operations
              (assert-equal #f (monoid-mempty monoid-any))
              (assert-equal #t ((monoid-mappend monoid-any) #f #t))
              (assert-equal #f ((monoid-mappend monoid-any) #f #f))))

(test-group monoid-operations
            
            (define-test mconcat-sum
              (assert-equal 15 (mconcat monoid-sum '(1 2 3 4 5))))
            
            (define-test mconcat-product
              (assert-equal 120 (mconcat monoid-product '(1 2 3 4 5))))
            
            (define-test mconcat-list
              (assert-equal '(1 2 3 4 5 6) (mconcat monoid-list '((1 2) (3 4) (5 6)))))
            
            (define-test mconcat-empty
              (assert-equal 0 (mconcat monoid-sum '()))
              (assert-equal '() (mconcat monoid-list '())))
            
            (define-test mtimes-sum
              (assert-equal 15 (mtimes monoid-sum 3 5)))
            
            (define-test mtimes-list
              (assert-equal '(a a a) (mtimes monoid-list 3 '(a)))))

(test-group monoid-laws
            
            (define-test sum-laws
              (assert-true (verify-monoid-laws monoid-sum '(1 2 3 4 5))))
            
            (define-test product-laws
              (assert-true (verify-monoid-laws monoid-product '(1 2 3 4 5))))
            
            (define-test list-laws
              (assert-true (verify-monoid-laws monoid-list '((a) (b) (c)))))
            
            (define-test all-laws
              (assert-true (verify-monoid-laws monoid-all '(#t #t #t)))))

;;; ====
;;; Functor Tests
;;; ====

(test-group functor-basics
            
            (define-test functor-construction
              (let ([f (make-functor map)])
                   (assert-true (functor? f))))
            
            (define-test functor-list-fmap
              (assert-equal '(2 4 6 8)
                            (fmap-with functor-list (lambda (x) (* x 2)) '(1 2 3 4))))
            
            (define-test functor-maybe-fmap-just
              (assert-equal (just 10)
                            (fmap-with functor-maybe (lambda (x) (* x 2)) (just 5))))
            
            (define-test functor-maybe-fmap-nothing
              (assert-equal nothing
                            (fmap-with functor-maybe (lambda (x) (* x 2)) nothing)))
            
            (define-test functor-either-fmap-right
              (assert-equal (right 10)
                            (fmap-with functor-either (lambda (x) (* x 2)) (right 5))))
            
            (define-test functor-either-fmap-left
              (assert-equal (left 'error)
                            (fmap-with functor-either (lambda (x) (* x 2)) (left 'error)))))

(test-group functor-operations
            
            (define-test replace-with-list
              (assert-equal '(x x x x)
                            (replace-with functor-list 'x '(1 2 3 4))))
            
            (define-test void-with-list
              (assert-equal '(() () ())
                            (void-with functor-list '(a b c)))))

;;; ====
;;; Foldable Tests
;;; ====

(test-group foldable-basics
            
            (define-test foldable-construction
              (assert-true (foldable? foldable-list)))
            
            (define-test foldable-list-foldr
              (assert-equal '(1 2 3 4 5)
                            ((foldable-foldr foldable-list) cons '() '(1 2 3 4 5))))
            
            (define-test foldable-list-foldl
              (assert-equal 15
                            ((foldable-foldl foldable-list) + 0 '(1 2 3 4 5))))
            
            (define-test foldable-maybe-foldr-just
              (assert-equal '(5)
                            ((foldable-foldr foldable-maybe) cons '() (just 5))))
            
            (define-test foldable-maybe-foldr-nothing
              (assert-equal '()
                            ((foldable-foldr foldable-maybe) cons '() nothing))))

(test-group foldable-operations
            
            (define-test fold-with-sum
              (assert-equal 15
                            (fold-with foldable-list monoid-sum '(1 2 3 4 5))))
            
            (define-test to-list-from-list
              (assert-equal '(1 2 3)
                            (to-list foldable-list '(1 2 3))))
            
            (define-test null-foldable-empty
              (assert-true (null-foldable? foldable-list '())))
            
            (define-test null-foldable-non-empty
              (assert-false (null-foldable? foldable-list '(1))))
            
            (define-test length-foldable-list
              (assert-equal 5 (length-foldable foldable-list '(a b c d e))))
            
            (define-test elem-foldable-found
              (assert-true (elem-foldable foldable-list 3 '(1 2 3 4 5))))
            
            (define-test elem-foldable-not-found
              (assert-false (elem-foldable foldable-list 6 '(1 2 3 4 5)))))

;;; ====
;;; Applicative Tests
;;; ====

(test-group applicative-basics
            
            (define-test applicative-construction
              (assert-true (applicative? applicative-list)))
            
            (define-test applicative-list-pure
              (assert-equal '(5)
                            ((applicative-pure applicative-list) 5)))
            
            (define-test applicative-list-ap
              (assert-equal '(2 4 3 6)
                            (ap-with applicative-list
                                     (list (lambda (x) (* x 2)) (lambda (x) (* x 3)))
                                     '(1 2))))
            
            (define-test applicative-maybe-pure
              (assert-equal (just 5)
                            ((applicative-pure applicative-maybe) 5)))
            
            (define-test applicative-maybe-ap-both-just
              (assert-equal (just 10)
                            (ap-with applicative-maybe
                                     (just (lambda (x) (* x 2)))
                                     (just 5))))
            
            (define-test applicative-maybe-ap-nothing-fn
              (assert-equal nothing
                            (ap-with applicative-maybe
                                     nothing
                                     (just 5))))
            
            (define-test applicative-maybe-ap-nothing-val
              (assert-equal nothing
                            (ap-with applicative-maybe
                                     (just (lambda (x) (* x 2)))
                                     nothing))))

(test-group applicative-operations
            
            (define-test sequence-a-list
              (assert-equal '((1 a) (1 b) (2 a) (2 b))
                            (sequence-a applicative-list '((1 2) (a b)))))
            
            (define-test sequence-a-maybe-all-just
              (assert-equal (just '(1 2 3))
                            (sequence-a applicative-maybe (list (just 1) (just 2) (just 3)))))
            
            (define-test sequence-a-maybe-with-nothing
              (assert-equal nothing
                            (sequence-a applicative-maybe (list (just 1) nothing (just 3)))))
            
            (define-test traverse-a-list
              ;; traverse list with `list` (singleton) over list applicative
              ;; Each element becomes (e), then sequence gives cartesian product
              ;; Since each is singleton, result is single list with all elements
              (assert-equal '((1 2 3))
                            (traverse-a applicative-list list '(1 2 3)))))

;;; ====
;;; Lens Tests
;;; ====

(test-group lens-basics
            
            (define-test lens-construction
              (let ([l (make-lens car (lambda (a p) (cons a (cdr p))))])
                   (assert-true (lens? l))))
            
            (define-test lens-fst-view
              (assert-equal 1 (view lens-fst '(1 . 2))))
            
            (define-test lens-fst-set
              (assert-equal '(10 . 2) (set-lens lens-fst 10 '(1 . 2))))
            
            (define-test lens-fst-over
              (assert-equal '(2 . 2) (over lens-fst (lambda (x) (* x 2)) '(1 . 2))))
            
            (define-test lens-snd-view
              (assert-equal 2 (view lens-snd '(1 . 2))))
            
            (define-test lens-snd-set
              (assert-equal '(1 . 20) (set-lens lens-snd 20 '(1 . 2))))
            
            (define-test lens-head-view
              (assert-equal 1 (view lens-head '(1 2 3))))
            
            (define-test lens-head-set
              (assert-equal '(10 2 3) (set-lens lens-head 10 '(1 2 3))))
            
            (define-test lens-tail-view
              (assert-equal '(2 3) (view lens-tail '(1 2 3))))
            
            (define-test lens-tail-set
              (assert-equal '(1 x y) (set-lens lens-tail '(x y) '(1 2 3)))))

(test-group lens-nth
            
            (define-test lens-nth-0-view
              (assert-equal 'a (view (lens-nth 0) '(a b c))))
            
            (define-test lens-nth-1-view
              (assert-equal 'b (view (lens-nth 1) '(a b c))))
            
            (define-test lens-nth-2-view
              (assert-equal 'c (view (lens-nth 2) '(a b c))))
            
            (define-test lens-nth-set
              (assert-equal '(a X c) (set-lens (lens-nth 1) 'X '(a b c))))
            
            (define-test lens-nth-over
              (assert-equal '(1 20 3)
                            (over (lens-nth 1) (lambda (x) (* x 10)) '(1 2 3)))))

(test-group lens-key
            
            (define-test lens-key-view
              (assert-equal 10 (view (lens-key 'x) '((x . 10) (y . 20)))))
            
            (define-test lens-key-set
              (let ([result (set-lens (lens-key 'x) 100 '((x . 10) (y . 20)))])
                   ;; Key should be updated (new pair at front)
                   (assert-equal 100 (cdr (assq 'x result)))))
            
            (define-test lens-key-missing
              (assert-false (view (lens-key 'z) '((x . 10) (y . 20))))))

(test-group lens-composition
            
            (define-test compose-fst-fst
              (let ([l (lens-compose lens-fst lens-fst)])
                   (assert-equal 1 (view l '((1 . 2) . 3)))))
            
            (define-test compose-fst-snd
              (let ([l (lens-compose lens-fst lens-snd)])
                   (assert-equal 2 (view l '((1 . 2) . 3)))))
            
            (define-test compose-set
              (let ([l (lens-compose lens-fst lens-fst)])
                   (assert-equal '((100 . 2) . 3) (set-lens l 100 '((1 . 2) . 3))))))

;;; ====
;;; Prism Tests
;;; ====

(test-group prism-basics
            
            (define-test prism-construction
              (let ([p (make-prism
                        (lambda (m) (if (just? m) m nothing))
                        just)])
                   (assert-true (prism? p))))
            
            (define-test prism-just-preview-just
              (assert-equal (just 5) (preview prism-just (just 5))))
            
            (define-test prism-just-preview-nothing
              (assert-equal nothing (preview prism-just nothing)))
            
            (define-test prism-just-review
              (assert-equal (just 5) (review prism-just 5)))
            
            (define-test prism-left-preview-left
              (assert-equal (just 'error) (preview prism-left (left 'error))))
            
            (define-test prism-left-preview-right
              (assert-equal nothing (preview prism-left (right 5))))
            
            (define-test prism-left-review
              (assert-equal (left 'error) (review prism-left 'error)))
            
            (define-test prism-right-preview-right
              (assert-equal (just 5) (preview prism-right (right 5))))
            
            (define-test prism-right-preview-left
              (assert-equal nothing (preview prism-right (left 'error))))
            
            (define-test prism-right-review
              (assert-equal (right 5) (review prism-right 5))))

;;; ====
;;; Template Generation Tests
;;; ====

(test-group template-generation
            
            (define-test gen-monoid-template-structure
              (let ([tmpl (gen-monoid-template 'pair '(cons '() '()) 'append-pairs)])
                   (assert-equal 'define (car tmpl))
                   (assert-equal 'monoid-pair (cadr tmpl))))
            
            (define-test gen-functor-template-structure
              (let ([tmpl (gen-functor-template 'tree 'tree-map)])
                   (assert-equal 'define (car tmpl))
                   (assert-equal 'functor-tree (cadr tmpl))))
            
            (define-test gen-lens-template-structure
              (let ([tmpl (gen-lens-template 'person 'name)])
                   (assert-equal 'define (car tmpl))
                   (assert-equal 'lens-person-name (cadr tmpl)))))

;;; ====
;;; Summary
;;; ====

(display "\n=== FP Templates Tests Complete ===\n")
(display "Passed: ")
(display *tests-passed*)
(display " | Failed: ")
(display *tests-failed*)
(display " | Total: ")
(display *tests-run*)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
