;;; fabric/stitches/fp/test-optics.ss — Tests for Optics Library

;;; NOTE: Run from fabric/stitches directory

(load "core/test-framework.ss")
(load "core/fp/optics/optics.ss")

(display "
")
(display "==============================================================
")
(display "         OPTICS TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Identity Functor Tests
;;; ============================================================

(test-group identity-functor
            (define-test make-identity-test
              (let ([id (make-identity 42)])
                   (assert-true (identity? id))
                   (assert-equal 42 (run-identity id))))
            
            (define-test identity-fmap-test
              (let* ([id (make-identity 21)]
                     [id2 (identity-fmap (lambda (x) (* x 2)) id)])
                    (assert-equal 42 (run-identity id2)))))

;;; ============================================================
;;; Const Functor Tests
;;; ============================================================

(test-group const-functor
            (define-test make-const-test
              (let ([c (make-const "hello")])
                   (assert-true (const? c))
                   (assert-equal "hello" (get-const c))))
            
            (define-test const-fmap-ignores-test
              (let* ([c (make-const "kept")]
                     [c2 (const-fmap (lambda (x) "changed") c)])
                    (assert-equal "kept" (get-const c2)))))

;;; ============================================================
;;; Lens Tests
;;; ============================================================

(test-group lens-basic
            (define-test lens-view-test
              (let ([pair (cons "hello" 42)])
                   (assert-equal "hello" (lens-view _1 pair))
                   (assert-equal 42 (lens-view _2 pair))))
            
            (define-test lens-set-test
              (let ([pair (cons "hello" 42)])
                   (assert-equal (cons "world" 42) (lens-set _1 "world" pair))
                   (assert-equal (cons "hello" 100) (lens-set _2 100 pair))))
            
            (define-test lens-over-test
              (let ([pair (cons 10 20)])
                   (assert-equal (cons 20 20) (lens-over _1 (lambda (x) (* x 2)) pair))
                   (assert-equal (cons 10 21) (lens-over _2 add1 pair))))
            
            (define-test custom-lens-test
              (let* ([person (cons "Alice" 30)]
                     [name-lens (make-lens car (lambda (p n) (cons n (cdr p))))]
                     [age-lens (make-lens cdr (lambda (p a) (cons (car p) a)))])
                    (assert-equal "Alice" (lens-view name-lens person))
                    (assert-equal 30 (lens-view age-lens person))
                    (assert-equal (cons "Bob" 30) (lens-set name-lens "Bob" person))
                    (assert-equal (cons "Alice" 31) (lens-over age-lens add1 person)))))

;;; ============================================================
;;; Lens Laws Tests
;;; ============================================================

(test-group lens-laws
            ;; Law 1: view l (set l v s) = v
            (define-test set-get-law-test
              (let ([s (cons "old" 1)]
                    [v "new"])
                   (assert-equal v (lens-view _1 (lens-set _1 v s)))))
            
            ;; Law 2: set l (view l s) s = s
            (define-test get-set-law-test
              (let ([s (cons "hello" 42)])
                   (assert-equal s (lens-set _1 (lens-view _1 s) s))))
            
            ;; Law 3: set l v (set l w s) = set l v s
            (define-test set-set-law-test
              (let ([s (cons 1 2)]
                    [v 100]
                    [w 200])
                   (assert-equal (lens-set _1 v s)
                                 (lens-set _1 v (lens-set _1 w s))))))

;;; ============================================================
;;; Lens Composition Tests
;;; ============================================================

(test-group lens-composition
            (define-test nested-pair-view-test
              (let* ([nested (cons (cons 1 2) 3)]
                     [inner-first (lens-compose _1 _1)])
                    (assert-equal 1 (lens-view inner-first nested))))
            
            (define-test nested-pair-set-test
              (let* ([nested (cons (cons 1 2) 3)]
                     [inner-first (lens-compose _1 _1)])
                    (assert-equal (cons (cons 100 2) 3)
                                  (lens-set inner-first 100 nested))))
            
            (define-test nested-pair-over-test
              (let* ([nested (cons (cons 5 10) 15)]
                     [inner-second (lens-compose _1 _2)])
                    (assert-equal (cons (cons 5 20) 15)
                                  (lens-over inner-second (lambda (x) (* x 2)) nested))))
            
            (define-test deep-nesting-test
              (let* ([deep (cons (cons (cons 1 2) 3) 4)]
                     [deepest (lens-compose _1 (lens-compose _1 _1))])
                    (assert-equal 1 (lens-view deepest deep))
                    (assert-equal (cons (cons (cons 42 2) 3) 4)
                                  (lens-set deepest 42 deep)))))

;;; ============================================================
;;; Iso Tests
;;; ============================================================

(test-group iso-basic
            (define-test iso-view-test
              (assert-equal (string->list "hello")
                            (iso-view _chars "hello")))
            
            (define-test iso-review-test
              (assert-equal "hi" (iso-review _chars (string->list "hi"))))
            
            (define-test iso-flip-test
              (let ([flipped (iso-flip _chars)])
                   (assert-equal "abc" (iso-view flipped (string->list "abc")))))
            
            (define-test iso-compose-test
              (let ([double-flip (iso-compose _chars (iso-flip _chars))])
                   (assert-equal "test" (iso-view double-flip "test")))))

;;; ============================================================
;;; Iso Laws Tests
;;; ============================================================

(test-group iso-laws
            ;; Law 1: view i (review i a) = a
            (define-test from-to-law-test
              (let ([chars (string->list "hello")])
                   (assert-equal chars (iso-view _chars (iso-review _chars chars)))))
            
            ;; Law 2: review i (view i s) = s
            (define-test to-from-law-test
              (assert-equal "hello" (iso-review _chars (iso-view _chars "hello")))))

;;; ============================================================
;;; Prism Tests
;;; ============================================================

(test-group prism-basic
            (define-test prism-match-left-test
              (assert-equal (just 42) (prism-match _Left (left 42))))
            
            (define-test prism-match-right-test
              (assert-equal nothing (prism-match _Left (right "x"))))
            
            (define-test prism-review-test
              (assert-equal (left 42) (prism-review _Left 42)))
            
            (define-test prism-over-match-test
              (assert-equal (left 43) (prism-over _Left add1 (left 42))))
            
            (define-test prism-over-no-match-test
              (assert-equal (right "x") (prism-over _Left add1 (right "x")))))

;;; ============================================================
;;; Maybe Prism Tests
;;; ============================================================

(test-group prism-maybe
            (define-test just-prism-match-test
              (assert-equal (just 1) (prism-match _Just (just 1))))
            
            (define-test just-prism-nothing-test
              (assert-equal nothing (prism-match _Just nothing)))
            
            (define-test just-prism-over-test
              (assert-equal (just 6) (prism-over _Just add1 (just 5))))
            
            (define-test nothing-prism-match-test
              (assert-true (just? (prism-match _Nothing nothing))))
            
            (define-test nothing-prism-no-match-test
              (assert-equal nothing (prism-match _Nothing (just 1)))))

;;; ============================================================
;;; List Prism Tests
;;; ============================================================

(test-group prism-list
            (define-test cons-prism-match-test
              (let ([result (prism-match _Cons '(1 2 3))])
                   (assert-true (just? result))
                   (assert-equal 1 (car (from-just result)))
                   (assert-equal '(2 3) (cdr (from-just result)))))
            
            (define-test cons-prism-empty-test
              (assert-equal nothing (prism-match _Cons '())))
            
            (define-test nil-prism-match-test
              (assert-true (just? (prism-match _Nil '()))))
            
            (define-test nil-prism-no-match-test
              (assert-equal nothing (prism-match _Nil '(1 2 3)))))

;;; ============================================================
;;; Affine Tests
;;; ============================================================

(test-group affine-basic
            (define-test ix-preview-found-test
              (assert-equal (just 'b) (affine-preview (ix 1) '(a b c))))
            
            (define-test ix-preview-not-found-test
              (assert-equal nothing (affine-preview (ix 5) '(a b c))))
            
            (define-test ix-set-test
              (assert-equal '(a x c) (affine-set (ix 1) 'x '(a b c))))
            
            (define-test ix-set-out-of-bounds-test
              (assert-equal '(a b c) (affine-set (ix 5) 'x '(a b c))))
            
            (define-test ix-over-test
              (assert-equal '(a B c)
                            (affine-over (ix 1)
                                         (lambda (x) (string->symbol (string-upcase (symbol->string x))))
                                         '(a b c)))))

;;; ============================================================
;;; At Lens Tests (Alists)
;;; ============================================================

(test-group at-lens
            (define-test at-view-found-test
              (let ([alist '((name . "Alice") (age . 30))])
                   (assert-equal (just "Alice") (lens-view (at 'name) alist))))
            
            (define-test at-view-not-found-test
              (let ([alist '((name . "Alice"))])
                   (assert-equal nothing (lens-view (at 'age) alist))))
            
            (define-test at-set-existing-test
              (let* ([alist '((name . "Alice") (age . 30))]
                     [updated (lens-set (at 'name) (just "Bob") alist)])
                    (assert-equal (just "Bob") (lens-view (at 'name) updated))))
            
            (define-test at-set-new-test
              (let* ([alist '((name . "Alice"))]
                     [updated (lens-set (at 'age) (just 25) alist)])
                    (assert-equal (just 25) (lens-view (at 'age) updated))))
            
            (define-test at-delete-test
              (let* ([alist '((name . "Alice") (age . 30))]
                     [updated (lens-set (at 'age) nothing alist)])
                    (assert-equal nothing (lens-view (at 'age) updated)))))

;;; ============================================================
;;; Stock Isos Tests
;;; ============================================================

(test-group stock-isos
            (define-test id-iso-test
              (assert-equal 42 (iso-view _id 42))
              (assert-equal 42 (iso-review _id 42)))
            
            (define-test swap-iso-test
              (assert-equal (cons 2 1) (iso-view _swap (cons 1 2)))
              (assert-equal (cons 1 2) (iso-review _swap (cons 2 1)))))

;;; ============================================================
;;; Getter/Setter Tests
;;; ============================================================

(test-group getter-setter
            (define-test getter-view-test
              (let ([length-getter (to length)])
                   (assert-equal 5 (getter-view length-getter '(1 2 3 4 5)))))
            
            (define-test setter-over-test
              (assert-equal '(2 4 6) (setter-over mapped (lambda (x) (* x 2)) '(1 2 3))))
            
            (define-test setter-set-test
              (assert-equal '(0 0 0) (setter-set mapped 0 '(1 2 3)))))

;;; ============================================================
;;; Optic Combinators Tests
;;; ============================================================

(test-group optic-combinators
            (define-test filtered-match-test
              (let ([positive (filtered (lambda (x) (> x 0)))])
                   (assert-equal (just 5) (affine-preview positive 5))))
            
            (define-test filtered-no-match-test
              (let ([positive (filtered (lambda (x) (> x 0)))])
                   (assert-equal nothing (affine-preview positive -3))))
            
            (define-test failing-first-succeeds-test
              (let ([aff (failing (ix 0) (ix 1))])
                   (assert-equal (just 'a) (affine-preview aff '(a b c)))))
            
            (define-test failing-fallback-test
              (let ([aff (failing (ix 5) (ix 0))])
                   (assert-equal (just 'a) (affine-preview aff '(a b c))))))

;;; ============================================================
;;; Has/Isnt Tests
;;; ============================================================

(test-group has-isnt
            (define-test has-left-test
              (assert-true (has _Left (left 1))))
            
            (define-test has-right-test
              (assert-false (has _Left (right 1))))
            
            (define-test isnt-left-test
              (assert-false (isnt _Left (left 1))))
            
            (define-test isnt-right-test
              (assert-true (isnt _Left (right 1)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(newline)
(exit-with-summary)
