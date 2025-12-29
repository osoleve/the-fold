;;; fabric/stitches/fp/test-optics.ss — Tests for Optics Library

;;; NOTE: Run from fabric/stitches directory

(load "test-framework.ss")
(load "fp/optics.ss")

(display "\n")
(display "==============================================================\n")
(display "         OPTICS TESTS\n")
(display "==============================================================\n")

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
    (assert-equal '(#\h #\e #\l #\l #\o)
                  (iso-view _chars "hello")))

  (define-test iso-review-test
    (assert-equal "hi" (iso-review _chars '(#\h #\i))))

  (define-test iso-flip-test
    (let ([flipped (iso-flip _chars)])
      (assert-equal "abc" (iso-view flipped '(#\a #\b #\c)))
      (assert-equal '(#\x #\y) (iso-review flipped "xy"))))

  (define-test iso-swap-test
    (assert-equal (cons 2 1) (iso-view _swap (cons 1 2)))
    (assert-equal (cons 'a 'b) (iso-review _swap (cons 'b 'a))))

  (define-test iso-id-test
    (assert-equal 42 (iso-view _id 42))
    (assert-equal "hello" (iso-review _id "hello"))))

;;; ============================================================
;;; Iso Laws Tests
;;; ============================================================

(test-group iso-laws
  ;; Law 1: view i (review i a) = a
  (define-test from-to-law-test
    (let ([chars '(#\t #\e #\s #\t)])
      (assert-equal chars (iso-view _chars (iso-review _chars chars)))))

  ;; Law 2: review i (view i s) = s
  (define-test to-from-law-test
    (let ([s "test"])
      (assert-equal s (iso-review _chars (iso-view _chars s))))))

;;; ============================================================
;;; Iso Composition Tests
;;; ============================================================

(test-group iso-composition
  (define-test iso-compose-test
    ;; Compose _id with _chars should be same as _chars
    (let ([composed (iso-compose _id _chars)])
      (assert-equal '(#\a #\b) (iso-view composed "ab"))
      (assert-equal "xy" (iso-review composed '(#\x #\y))))))

;;; ============================================================
;;; Prism Tests
;;; ============================================================

(test-group prism-basic
  (define-test prism-left-match-test
    (let ([result (prism-match _Left (left 42))])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test prism-left-no-match-test
    (let ([result (prism-match _Left (right "hello"))])
      (assert-true (nothing? result))))

  (define-test prism-right-match-test
    (let ([result (prism-match _Right (right "hello"))])
      (assert-true (just? result))
      (assert-equal "hello" (from-just result))))

  (define-test prism-review-test
    (let ([result (prism-review _Left 42)])
      (assert-true (left? result))
      (assert-equal 42 (from-left result))))

  (define-test prism-over-match-test
    (let ([result (prism-over _Left add1 (left 5))])
      (assert-true (left? result))
      (assert-equal 6 (from-left result))))

  (define-test prism-over-no-match-test
    (let ([result (prism-over _Left add1 (right "unchanged"))])
      (assert-true (right? result))
      (assert-equal "unchanged" (from-right result)))))

;;; ============================================================
;;; Maybe Prism Tests
;;; ============================================================

(test-group prism-maybe
  (define-test prism-just-match-test
    (let ([result (prism-match _Just (just 42))])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test prism-just-no-match-test
    (let ([result (prism-match _Just nothing)])
      (assert-true (nothing? result))))

  (define-test prism-nothing-match-test
    (let ([result (prism-match _Nothing nothing)])
      (assert-true (just? result))))

  (define-test prism-nothing-no-match-test
    (let ([result (prism-match _Nothing (just 1))])
      (assert-true (nothing? result))))

  (define-test prism-just-over-test
    (let ([result (prism-over _Just (lambda (x) (* x 2)) (just 21))])
      (assert-true (just? result))
      (assert-equal 42 (from-just result)))))

;;; ============================================================
;;; List Prism Tests
;;; ============================================================

(test-group prism-list
  (define-test prism-cons-match-test
    (let ([result (prism-match _Cons '(1 2 3))])
      (assert-true (just? result))
      (let ([pair (from-just result)])
        (assert-equal 1 (car pair))
        (assert-equal '(2 3) (cdr pair)))))

  (define-test prism-cons-no-match-test
    (let ([result (prism-match _Cons '())])
      (assert-true (nothing? result))))

  (define-test prism-nil-match-test
    (let ([result (prism-match _Nil '())])
      (assert-true (just? result))))

  (define-test prism-nil-no-match-test
    (let ([result (prism-match _Nil '(1 2))])
      (assert-true (nothing? result))))

  (define-test prism-cons-review-test
    (let ([result (prism-review _Cons (cons 'a '(b c)))])
      (assert-equal '(a b c) result))))

;;; ============================================================
;;; Prism Laws Tests
;;; ============================================================

(test-group prism-laws
  ;; Law 1: preview p (review p v) = Just v
  (define-test review-preview-law-test
    (let ([v 42])
      (assert-equal (just v) (prism-match _Left (prism-review _Left v)))))

  ;; Law 2: maybe s (review p) (preview p s) = s (for matching case)
  (define-test preview-review-law-match-test
    (let ([s (left 42)])
      (let ([previewed (prism-match _Left s)])
        (assert-true (just? previewed))
        (assert-equal s (prism-review _Left (from-just previewed)))))))

;;; ============================================================
;;; Affine Tests
;;; ============================================================

(test-group affine-basic
  (define-test ix-preview-valid-test
    (let ([result (affine-preview (ix 1) '(a b c))])
      (assert-true (just? result))
      (assert-equal 'b (from-just result))))

  (define-test ix-preview-invalid-test
    (let ([result (affine-preview (ix 5) '(a b c))])
      (assert-true (nothing? result))))

  (define-test ix-preview-negative-test
    (let ([result (affine-preview (ix -1) '(a b c))])
      (assert-true (nothing? result))))

  (define-test ix-set-valid-test
    (let ([result (affine-set (ix 1) 'x '(a b c))])
      (assert-equal '(a x c) result)))

  (define-test ix-set-invalid-test
    (let ([result (affine-set (ix 5) 'x '(a b c))])
      (assert-equal '(a b c) result)))

  (define-test ix-over-test
    (let ([result (affine-over (ix 0) add1 '(10 20 30))])
      (assert-equal '(11 20 30) result))))

;;; ============================================================
;;; At (Alist) Lens Tests
;;; ============================================================

(test-group at-lens
  (define alist '((name . "Alice") (age . 30) (city . "NYC")))

  (define-test at-view-found-test
    (let ([result (lens-view (at 'name) alist)])
      (assert-true (just? result))
      (assert-equal "Alice" (from-just result))))

  (define-test at-view-not-found-test
    (let ([result (lens-view (at 'email) alist)])
      (assert-true (nothing? result))))

  (define-test at-set-existing-test
    (let ([result (lens-set (at 'name) (just "Bob") alist)])
      (assert-equal "Bob" (from-just (lens-view (at 'name) result)))))

  (define-test at-set-new-test
    (let ([result (lens-set (at 'email) (just "a@b.c") alist)])
      (assert-equal "a@b.c" (from-just (lens-view (at 'email) result)))))

  (define-test at-remove-test
    (let ([result (lens-set (at 'name) nothing alist)])
      (assert-true (nothing? (lens-view (at 'name) result))))))

;;; ============================================================
;;; Filtered Affine Tests
;;; ============================================================

(test-group filtered-affine
  (define-test filtered-match-test
    (let ([positive (filtered (lambda (x) (> x 0)))])
      (assert-equal (just 5) (affine-preview positive 5))
      (assert-true (nothing? (affine-preview positive -5)))))

  (define-test filtered-set-match-test
    (let ([positive (filtered (lambda (x) (> x 0)))])
      (assert-equal 100 (affine-set positive 100 5))))

  (define-test filtered-set-no-match-test
    (let ([positive (filtered (lambda (x) (> x 0)))])
      (assert-equal -5 (affine-set positive 100 -5)))))

;;; ============================================================
;;; Taking/Dropping Tests
;;; ============================================================

(test-group taking-dropping
  (define-test taking-preview-test
    (let ([result (affine-preview (taking 3) '(1 2 3 4 5))])
      (assert-true (just? result))
      (assert-equal '(1 2 3) (from-just result))))

  (define-test taking-preview-too-short-test
    (let ([result (affine-preview (taking 5) '(1 2))])
      (assert-true (nothing? result))))

  (define-test taking-set-test
    (let ([result (affine-set (taking 2) '(a b) '(1 2 3 4))])
      (assert-equal '(a b 3 4) result)))

  (define-test dropping-preview-test
    (let ([result (affine-preview (dropping 2) '(1 2 3 4 5))])
      (assert-true (just? result))
      (assert-equal '(3 4 5) (from-just result))))

  (define-test dropping-set-test
    (let ([result (affine-set (dropping 2) '(x y z) '(1 2 3 4 5))])
      (assert-equal '(1 2 x y z) result))))

;;; ============================================================
;;; Getter Tests
;;; ============================================================

(test-group getter
  (define-test getter-view-test
    (let ([len-getter (to length)])
      (assert-equal 5 (getter-view len-getter '(1 2 3 4 5)))))

  (define-test getter-string-length-test
    (let ([str-len (to string-length)])
      (assert-equal 5 (getter-view str-len "hello")))))

;;; ============================================================
;;; Setter Tests
;;; ============================================================

(test-group setter
  (define-test mapped-over-test
    (let ([result (setter-over mapped add1 '(1 2 3))])
      (assert-equal '(2 3 4) result)))

  (define-test mapped-set-test
    (let ([result (setter-set mapped 0 '(1 2 3))])
      (assert-equal '(0 0 0) result))))

;;; ============================================================
;;; Has/Isn't Tests
;;; ============================================================

(test-group has-isnt
  (define-test has-left-test
    (assert-true (has _Left (left 1)))
    (assert-false (has _Left (right 1))))

  (define-test isnt-left-test
    (assert-false (isnt _Left (left 1)))
    (assert-true (isnt _Left (right 1))))

  (define-test has-just-test
    (assert-true (has _Just (just 1)))
    (assert-false (has _Just nothing)))

  (define-test has-cons-test
    (assert-true (has _Cons '(1 2 3)))
    (assert-false (has _Cons '()))))

;;; ============================================================
;;; Failing Combinator Tests
;;; ============================================================

(test-group failing-combinator
  (define-test failing-first-succeeds-test
    (let* ([aff1 (ix 0)]
           [aff2 (ix 1)]
           [combined (failing aff1 aff2)]
           [result (affine-preview combined '(a b c))])
      (assert-equal (just 'a) result)))

  (define-test failing-fallback-test
    (let* ([aff1 (filtered (lambda (x) (> x 100)))]
           [aff2 (filtered (lambda (x) (> x 0)))]
           [combined (failing aff1 aff2)]
           [result (affine-preview combined 50)])
      (assert-equal (just 50) result)))

  (define-test failing-both-fail-test
    (let* ([aff1 (filtered (lambda (x) (> x 100)))]
           [aff2 (filtered (lambda (x) (> x 50)))]
           [combined (failing aff1 aff2)]
           [result (affine-preview combined 25)])
      (assert-true (nothing? result)))))

;;; ============================================================
;;; Lens Default Tests
;;; ============================================================

(test-group lens-default
  (define-test default-when-present-test
    (let ([result (lens-default (ix 1) 'default '(a b c))])
      (assert-equal 'b result)))

  (define-test default-when-absent-test
    (let ([result (lens-default (ix 10) 'default '(a b c))])
      (assert-equal 'default result))))

;;; ============================================================
;;; Prism Composition Tests
;;; ============================================================

(test-group prism-composition
  ;; Compose _Just with _Left to access left inside just
  (define-test prism-compose-match-test
    (let* ([nested (just (left 42))]
           [composed (prism-compose _Just _Left)]
           [result (prism-match composed nested)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test prism-compose-no-outer-match-test
    (let* ([nested nothing]
           [composed (prism-compose _Just _Left)]
           [result (prism-match composed nested)])
      (assert-true (nothing? result))))

  (define-test prism-compose-no-inner-match-test
    (let* ([nested (just (right "not left"))]
           [composed (prism-compose _Just _Left)]
           [result (prism-match composed nested)])
      (assert-true (nothing? result))))

  (define-test prism-compose-review-test
    (let* ([composed (prism-compose _Just _Left)]
           [result (prism-review composed 42)])
      (assert-true (just? result))
      (let ([inner (from-just result)])
        (assert-true (left? inner))
        (assert-equal 42 (from-left inner))))))

;;; ============================================================
;;; Practical Examples Tests
;;; ============================================================

(test-group practical-examples
  ;; Modeling a person record as nested pairs
  (define-test person-record-test
    (let* ([person (cons "Alice" (cons 30 "NYC"))]
           [name-lens _1]
           [age-lens (lens-compose _2 _1)]
           [city-lens (lens-compose _2 _2)])
      (assert-equal "Alice" (lens-view name-lens person))
      (assert-equal 30 (lens-view age-lens person))
      (assert-equal "NYC" (lens-view city-lens person))
      (let ([older (lens-over age-lens add1 person)])
        (assert-equal 31 (lens-view age-lens older)))))

  ;; Safe access with prisms and affines
  (define-test safe-head-test
    (let ([safe-head (lambda (lst) (affine-preview (ix 0) lst))])
      (assert-equal (just 1) (safe-head '(1 2 3)))
      (assert-true (nothing? (safe-head '())))))

  ;; Mapping over optional values
  (define-test map-over-maybe-test
    (let ([double-maybe (lambda (m)
                          (prism-over _Just (lambda (x) (* x 2)) m))])
      (assert-equal (just 42) (double-maybe (just 21)))
      (assert-true (nothing? (double-maybe nothing)))))

  ;; Updating deeply nested structure
  (define-test deep-update-test
    (let* ([data (cons (cons (just 5) "meta") "other")]
           [deep-val (lens-compose _1 (lens-compose _1 (iso->lens (make-iso
                                                                    from-just
                                                                    just))))])
      ;; This works when we know the just is present
      (assert-equal 5 (lens-view deep-val data))
      (assert-equal (cons (cons (just 10) "meta") "other")
                    (lens-over deep-val (lambda (x) (* x 2)) data)))))

;;; ============================================================
;;; Summary
;;; ============================================================

(display "\n")
(display "==============================================================\n")
(printf "Tests passed: ~a\n" *tests-passed*)
(printf "Tests failed: ~a\n" *tests-failed*)
(printf "Total tests:  ~a\n" *tests-run*)

(if (= *tests-failed* 0)
    (display "\n[SUCCESS] All optics tests passed.\n")
    (display "\n[FAILURE] Some optics tests failed.\n"))
