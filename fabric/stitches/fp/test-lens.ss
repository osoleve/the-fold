;;; fabric/stitches/fp/test-lens.ss — Tests for Lens Library

;;; NOTE: Run from fabric/stitches directory

(load "fabric/stitches/test-framework.ss")
(load "fabric/stitches/fp/lens.ss")

(display "
")
(display "==============================================================
")
(display "         LENS LIBRARY TESTS
")
(display "==============================================================
")

;;; ============================================================
;;; Core Lens Tests
;;; ============================================================

(test-group lens-basics
            (define-test make-lens-test
              (let ([l (make-lens car (lambda (a p) (cons a (cdr p))))])
                   (assert-true (lens? l))))
            
            (define-test view-test
              (let ([pair (cons 1 2)])
                   (assert-equal 1 (view fst-lens pair))
                   (assert-equal 2 (view snd-lens pair))))
            
            (define-test set-test
              (let ([pair (cons 1 2)])
                   (assert-equal (cons 10 2) (set fst-lens 10 pair))
                   (assert-equal (cons 1 20) (set snd-lens 20 pair))))
            
            (define-test over-test
              (let ([pair (cons 1 2)])
                   (assert-equal (cons 2 2) (over fst-lens add1 pair))
                   (assert-equal (cons 1 3) (over snd-lens add1 pair)))))

;;; ============================================================
;;; Lens Laws Tests
;;; ============================================================

(test-group lens-laws
            ;; Get-Put: view l (set l a s) = a
            (define-test get-put-law-test
              (let ([s (cons 1 2)]
                    [a 42])
                   (assert-equal a (view fst-lens (set fst-lens a s)))
                   (assert-equal a (view snd-lens (set snd-lens a s)))))
            
            ;; Put-Get: set l (view l s) s = s
            (define-test put-get-law-test
              (let ([s (cons 1 2)])
                   (assert-equal s (set fst-lens (view fst-lens s) s))
                   (assert-equal s (set snd-lens (view snd-lens s) s))))
            
            ;; Put-Put: set l a2 (set l a1 s) = set l a2 s
            (define-test put-put-law-test
              (let ([s (cons 1 2)])
                   (assert-equal (set fst-lens 30 s)
                                 (set fst-lens 30 (set fst-lens 20 s)))
                   (assert-equal (set snd-lens 30 s)
                                 (set snd-lens 30 (set snd-lens 20 s))))))

;;; ============================================================
;;; Identity Lens Tests
;;; ============================================================

(test-group identity-lens
            (define-test id-lens-view-test
              (assert-equal 42 (view id-lens 42))
              (assert-equal '(a b c) (view id-lens '(a b c))))
            
            (define-test id-lens-set-test
              (assert-equal 100 (set id-lens 100 42))
              (assert-equal 'new (set id-lens 'new 'old)))
            
            (define-test id-lens-over-test
              (assert-equal 43 (over id-lens add1 42))))

;;; ============================================================
;;; Lens Composition Tests
;;; ============================================================

(test-group lens-composition
            (define-test compose-two-lenses-test
              (let ([nested (cons (cons 1 2) 3)]
                    [inner-fst (lens-compose fst-lens fst-lens)])
                   (assert-equal 1 (view inner-fst nested))
                   (assert-equal (cons (cons 100 2) 3)
                                 (set inner-fst 100 nested))))
            
            (define-test compose-three-lenses-test
              (let ([deep (cons (cons (cons 1 2) 3) 4)]
                    [deep-lens (.. fst-lens fst-lens fst-lens)])
                   (assert-equal 1 (view deep-lens deep))
                   (assert-equal (cons (cons (cons 99 2) 3) 4)
                                 (set deep-lens 99 deep))))
            
            (define-test lens-then-test
              (let ([nested (cons (cons 1 2) 3)])
                   ;; lens-then goes left-to-right
                   (let ([l (lens-then fst-lens fst-lens)])
                        (assert-equal 1 (view l nested)))))
            
            (define-test compose-with-id-test
              (let ([l (lens-compose fst-lens id-lens)]
                    [pair (cons 1 2)])
                   (assert-equal 1 (view l pair))
                   (assert-equal (cons 10 2) (set l 10 pair)))))

;;; ============================================================
;;; List Lens Tests
;;; ============================================================

(test-group list-lenses
            (define-test car-lens-test
              (let ([lst '(1 2 3)])
                   (assert-equal 1 (view car-lens lst))
                   (assert-equal '(10 2 3) (set car-lens 10 lst))))
            
            (define-test cdr-lens-test
              (let ([lst '(1 2 3)])
                   (assert-equal '(2 3) (view cdr-lens lst))
                   (assert-equal '(1 a b) (set cdr-lens '(a b) lst))))
            
            (define-test nth-lens-0-test
              (let ([lst '(a b c d)])
                   (assert-equal 'a (view (nth-lens 0) lst))
                   (assert-equal '(x b c d) (set (nth-lens 0) 'x lst))))
            
            (define-test nth-lens-2-test
              (let ([lst '(a b c d)])
                   (assert-equal 'c (view (nth-lens 2) lst))
                   (assert-equal '(a b x d) (set (nth-lens 2) 'x lst))))
            
            (define-test nth-lens-over-test
              (let ([lst '(1 2 3 4 5)])
                   (assert-equal '(1 2 30 4 5) (over (nth-lens 2) (lambda (x) (* x 10)) lst)))))

;;; ============================================================
;;; Alist Lens Tests
;;; ============================================================

(test-group alist-lenses
            (define-test key-lens-view-test
              (let ([data '((name . "Alice") (age . 30))])
                   (assert-equal "Alice" (view (key-lens 'name) data))
                   (assert-equal 30 (view (key-lens 'age) data))))
            
            (define-test key-lens-missing-test
              (let ([data '((name . "Alice"))])
                   (assert-equal #f (view (key-lens 'missing) data))))
            
            (define-test key-lens-set-existing-test
              (let ([data '((name . "Alice") (age . 30))])
                   (let ([result (set (key-lens 'age) 31 data)])
                        (assert-equal 31 (view (key-lens 'age) result))
                        (assert-equal "Alice" (view (key-lens 'name) result)))))
            
            (define-test key-lens-set-new-test
              (let ([data '((name . "Alice"))])
                   (let ([result (set (key-lens 'age) 30 data)])
                        (assert-equal 30 (view (key-lens 'age) result))
                        (assert-equal "Alice" (view (key-lens 'name) result)))))
            
            (define-test key-lens-nested-test
              ;; Compose key lenses for nested alists
              (let ([data '((user . ((name . "Bob") (email . "bob@example.com"))))]
                    [user-name-lens (lens-compose (key-lens 'name) (key-lens 'user))])
                   (assert-equal "Bob" (view user-name-lens data)))))

;;; ============================================================
;;; Prism Tests
;;; ============================================================

(test-group prism-basics
            (define-test just-prism-match-test
              (let ([m (just 42)])
                   (let ([result (preview just-prism m)])
                        (assert-true (just? result))
                        (assert-equal 42 (from-just result)))))
            
            (define-test just-prism-no-match-test
              (let ([result (preview just-prism nothing)])
                   (assert-true (nothing? result))))
            
            (define-test just-prism-review-test
              (let ([m (review just-prism 42)])
                   (assert-true (just? m))
                   (assert-equal 42 (from-just m))))
            
            (define-test left-prism-match-test
              (let ([e (left "error")])
                   (let ([result (preview left-prism e)])
                        (assert-true (just? result))
                        (assert-equal "error" (from-just result)))))
            
            (define-test left-prism-no-match-test
              (let ([e (right 42)])
                   (let ([result (preview left-prism e)])
                        (assert-true (nothing? result)))))
            
            (define-test right-prism-match-test
              (let ([e (right 42)])
                   (let ([result (preview right-prism e)])
                        (assert-true (just? result))
                        (assert-equal 42 (from-just result)))))
            
            (define-test over-prism-match-test
              (let ([m (just 42)])
                   (let ([result (over-prism just-prism add1 m)])
                        (assert-true (just? result))
                        (assert-equal 43 (from-just result)))))
            
            (define-test over-prism-no-match-test
              (let ([result (over-prism just-prism add1 nothing)])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; List Prism Tests
;;; ============================================================

(test-group list-prisms
            (define-test cons-prism-match-test
              (let ([lst '(1 2 3)])
                   (let ([result (preview cons-prism lst)])
                        (assert-true (just? result))
                        (assert-equal (cons 1 '(2 3)) (from-just result)))))
            
            (define-test cons-prism-no-match-test
              (let ([result (preview cons-prism '())])
                   (assert-true (nothing? result))))
            
            (define-test nil-prism-match-test
              (let ([result (preview nil-prism '())])
                   (assert-true (just? result))))
            
            (define-test nil-prism-no-match-test
              (let ([result (preview nil-prism '(1 2 3))])
                   (assert-true (nothing? result)))))

;;; ============================================================
;;; Traversal Tests
;;; ============================================================

(test-group traversals
            (define-test each-to-list-test
              (assert-equal '(1 2 3) (to-list-of each '(1 2 3)))
              (assert-equal '() (to-list-of each '())))
            
            (define-test each-over-test
              (assert-equal '(2 3 4) (over-traversal each add1 '(1 2 3)))
              (assert-equal '() (over-traversal each add1 '())))
            
            (define-test filtered-to-list-test
              (assert-equal '(2 4) (to-list-of (filtered even?) '(1 2 3 4 5))))
            
            (define-test filtered-over-test
              (assert-equal '(1 20 3 40 5) (over-traversal (filtered even?)
                                                           (lambda (x) (* x 10))
                                                           '(1 2 3 4 5))))
            
            (define-test lens-to-traversal-test
              (let ([trav (lens->traversal fst-lens)]
                    [pair (cons 1 2)])
                   (assert-equal '(1) (to-list-of trav pair))
                   (assert-equal (cons 10 2) (over-traversal trav (lambda (x) (* x 10)) pair)))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
            ;; Updating nested structure
            (define-test nested-update-test
              (let* ([person '((name . "Alice")
                               (address . ((city . "Boston")
                                           (zip . "02101"))))]
                     [city-lens (lens-compose (key-lens 'city) (key-lens 'address))]
                     [updated (set city-lens "Cambridge" person)])
                    (assert-equal "Cambridge" (view city-lens updated))
                    (assert-equal "Alice" (view (key-lens 'name) updated))))
            
            ;; Mapping over list elements
            (define-test map-with-traversal-test
              (let* ([numbers '(1 2 3 4 5)]
                     [double (lambda (x) (+ x x))]
                     [doubled (over-traversal each double numbers)])
                    (assert-equal '(2 4 6 8 10) doubled)))
            
            ;; Filtering and transforming
            (define-test filter-transform-test
              (let* ([data '(1 2 3 4 5 6)]
                     [result (over-traversal (filtered (lambda (x) (> x 3)))
                                             (lambda (x) (- x))
                                             data)])
                    (assert-equal '(1 2 3 -4 -5 -6) result))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "
")
(display "==============================================================
")
(printf "Tests passed: ~a
" *tests-passed*)
(printf "Tests failed: ~a
" *tests-failed*)
(printf "Total tests:  ~a
" *tests-run*)

(if (= *tests-failed* 0)
    (display "
[SUCCESS] All lens tests passed.
")
    (display "
[FAILURE] Some lens tests failed.
"))
