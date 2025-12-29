;;; fabric/stitches/fp/test-optics.ss — Tests for Optics Library

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/optics.ss")

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
              (assert-equal '(#\h # #\l #\l #\o)
                            (iso-view _chars "hello")))
            
            (define-test iso-review-test
              (assert-equal "hi" (iso-review _chars '(#\h #\i))))
            
            (define-test iso-flip-test
              (let ([flipped (iso-flip _chars)])
                   (assert-equal "abc" (iso-view flipped '(# # #