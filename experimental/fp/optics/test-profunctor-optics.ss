;;; fabric/stitches/fp/test-profunctor-optics.ss — Tests for Profunctor Optics
;;;
;;; Run from project root:
;;;   scheme --script fabric/stitches/fp/test-profunctor-optics.ss

(load "core/test-framework.ss")
(load "core/fp/optics/profunctor-optics.ss")

(display "\n")
(display "==============================================================\n")
(display "         PROFUNCTOR OPTICS TESTS\n")
(display "==============================================================\n")

;;; ============================================================
;;; Dictionary Tests
;;; ============================================================

(test-group profunctor-dicts
            (define-test fn-profunctor-dict-test
              (assert-true (profunctor-dict? fn-profunctor-dict))
              (assert-equal 'function (profunctor-dict-name fn-profunctor-dict)))
            
            (define-test fn-strong-dict-test
              (assert-true (strong-dict? fn-strong-dict))
              (assert-true (profunctor-dict? (strong-dict-profunctor fn-strong-dict))))
            
            (define-test fn-choice-dict-test
              (assert-true (choice-dict? fn-choice-dict))
              (assert-true (profunctor-dict? (choice-dict-profunctor fn-choice-dict)))))

;;; ============================================================
;;; Forget Profunctor Tests
;;; ============================================================

(test-group forget-profunctor
            (define-test forget-basic-test
              (let ([f (make-forget add1)])
                   (assert-true (forget? f))
                   (assert-equal 6 (run-forget f 5))))
            
            (define-test forget-dimap-test
              (let* ([f (make-forget identity)]
                     [f2 (forget-dimap (lambda (x) (* x 2)) add1 f)])
                    (assert-equal 10 (run-forget f2 5))))
            
            (define-test forget-first-test
              (let* ([f (make-forget identity)]
                     [f2 (forget-first f)])
                    (assert-equal 'a (run-forget f2 (cons 'a 'b))))))

;;; ============================================================
;;; Lens Basic Tests
;;; ============================================================

(test-group pro-lens-basic
            (define-test pro-view-test
              (let ([pair (cons "hello" 42)])
                   (assert-equal "hello" (pro-view pro-_1 pair))
                   (assert-equal 42 (pro-view pro-_2 pair))))
            
            (define-test pro-set-test
              (let ([pair (cons "hello" 42)])
                   (assert-equal (cons "world" 42) (pro-set pro-_1 "world" pair))
                   (assert-equal (cons "hello" 100) (pro-set pro-_2 100 pair))))
            
            (define-test pro-over-test
              (let ([pair (cons 10 20)])
                   (assert-equal (cons 20 20) (pro-over pro-_1 (lambda (x) (* x 2)) pair))
                   (assert-equal (cons 10 21) (pro-over pro-_2 add1 pair))))
            
            (define-test custom-pro-lens-test
              (let* ([person (cons "Alice" 30)]
                     [name-lens (pro-lens car (lambda (p n) (cons n (cdr p))))]
                     [age-lens (pro-lens cdr (lambda (p a) (cons (car p) a)))])
                    (assert-equal "Alice" (pro-view name-lens person))
                    (assert-equal 30 (pro-view age-lens person))
                    (assert-equal (cons "Bob" 30) (pro-set name-lens "Bob" person))
                    (assert-equal (cons "Alice" 31) (pro-over age-lens add1 person)))))

;;; ============================================================
;;; Lens Laws Tests
;;; ============================================================

(test-group pro-lens-laws
            ;; Law 1: view l (set l v s) = v
            (define-test set-get-law
              (let ([s (cons "old" 1)]
                    [v "new"])
                   (assert-equal v (pro-view pro-_1 (pro-set pro-_1 v s)))))
            
            ;; Law 2: set l (view l s) s = s
            (define-test get-set-law
              (let ([s (cons "hello" 42)])
                   (assert-equal s (pro-set pro-_1 (pro-view pro-_1 s) s))))
            
            ;; Law 3: set l v (set l w s) = set l v s
            (define-test set-set-law
              (let ([s (cons 1 2)]
                    [v 100]
                    [w 200])
                   (assert-equal (pro-set pro-_1 v s)
                                 (pro-set pro-_1 v (pro-set pro-_1 w s))))))

;;; ============================================================
;;; Prism Tests
;;; ============================================================

(test-group pro-prism-basic
            (define-test left-prism-preview-test
              (assert-equal (just 42) (pro-preview pro-_Left (left 42)))
              (assert-equal nothing (pro-preview pro-_Left (right "hello"))))
            
            (define-test right-prism-preview-test
              (assert-equal (just "hello") (pro-preview pro-_Right (right "hello")))
              (assert-equal nothing (pro-preview pro-_Right (left 42))))
            
            (define-test just-prism-preview-test
              (assert-equal (just 5) (pro-preview pro-_Just (just 5)))
              (assert-equal nothing (pro-preview pro-_Just nothing)))
            
            (define-test prism-review-test
              (assert-equal (left 42) (pro-review pro-_Left 42))
              (assert-equal (right "hi") (pro-review pro-_Right "hi"))
              (assert-equal (just 5) (pro-review pro-_Just 5)))
            
            (define-test prism-has-isnt-test
              (assert-true (pro-has pro-_Left (left 1)))
              (assert-false (pro-has pro-_Left (right 1)))
              (assert-true (pro-isnt pro-_Left (right 1)))
              (assert-false (pro-isnt pro-_Left (left 1)))))

;;; ============================================================
;;; Prism Laws Tests
;;; ============================================================

(test-group pro-prism-laws
            ;; Law 1: preview p (review p v) = Just v
            (define-test review-preview-law
              (let ([v 42])
                   (assert-equal (just v) (pro-preview pro-_Left (pro-review pro-_Left v)))
                   (assert-equal (just v) (pro-preview pro-_Right (pro-review pro-_Right v)))
                   (assert-equal (just v) (pro-preview pro-_Just (pro-review pro-_Just v))))))

;;; ============================================================
;;; Iso Tests
;;; ============================================================

(test-group pro-iso-basic
            (define swap-iso (pro-iso (lambda (p) (cons (cdr p) (car p)))
                                      (lambda (p) (cons (cdr p) (car p)))))
            
            (define-test iso-view-test
              (assert-equal (cons 2 1) (pro-view swap-iso (cons 1 2))))
            
            (define-test iso-set-test
              (assert-equal (cons 20 10) (pro-set swap-iso (cons 10 20) (cons 1 2)))))

;;; ============================================================
;;; Affine Tests
;;; ============================================================

(test-group pro-affine-basic
            (define-test ix-in-bounds-test
              (assert-equal (just 'b) (pro-preview (pro-ix 1) '(a b c)))
              (assert-equal (just 'a) (pro-preview (pro-ix 0) '(a b c)))
              (assert-equal (just 'c) (pro-preview (pro-ix 2) '(a b c))))
            
            (define-test ix-out-of-bounds-test
              (assert-equal nothing (pro-preview (pro-ix 10) '(a b c)))
              (assert-equal nothing (pro-preview (pro-ix -1) '(a b c))))
            
            (define-test ix-set-test
              (assert-equal '(X b c) (pro-set (pro-ix 0) 'X '(a b c)))
              (assert-equal '(a X c) (pro-set (pro-ix 1) 'X '(a b c)))
              (assert-equal '(a b X) (pro-set (pro-ix 2) 'X '(a b c)))))

;;; ============================================================
;;; Sequence Prism Tests
;;; ============================================================

(test-group sequence-prisms
            (define-test empty-prism-test
              (assert-equal (just '()) (pro-preview pro-_Empty '()))
              (assert-equal nothing (pro-preview pro-_Empty '(1 2 3)))
              (assert-equal '() (pro-review pro-_Empty '())))
            
            (define-test cons-prism-test
              (assert-equal (just (cons 1 '(2 3))) (pro-preview pro-_Cons '(1 2 3)))
              (assert-equal nothing (pro-preview pro-_Cons '()))
              (assert-equal '(a b c) (pro-review pro-_Cons (cons 'a '(b c)))))
            
            (define-test snoc-prism-test
              (assert-equal (just (cons '(1 2) 3)) (pro-preview pro-_Snoc '(1 2 3)))
              (assert-equal (just (cons '() 1)) (pro-preview pro-_Snoc '(1)))
              (assert-equal nothing (pro-preview pro-_Snoc '()))
              (assert-equal '(a b c) (pro-review pro-_Snoc (cons '(a b) 'c)))))

;;; ============================================================
;;; At/Ixed Tests
;;; ============================================================

(test-group at-ixed
            (define test-alist '((name . "Alice") (age . 30) (city . "NYC")))
            
            (define-test at-found-test
              (assert-equal (just "Alice") (pro-view (pro-at 'name) test-alist))
              (assert-equal (just 30) (pro-view (pro-at 'age) test-alist)))
            
            (define-test at-not-found-test
              (assert-equal nothing (pro-view (pro-at 'missing) test-alist)))
            
            (define-test at-set-existing-test
              (let ([result (pro-set (pro-at 'name) (just "Bob") test-alist)])
                   (assert-equal (just "Bob") (pro-view (pro-at 'name) result))))
            
            (define-test at-set-new-test
              (let ([result (pro-set (pro-at 'country) (just "USA") test-alist)])
                   (assert-equal (just "USA") (pro-view (pro-at 'country) result))))
            
            (define-test at-remove-test
              (let ([result (pro-set (pro-at 'age) nothing test-alist)])
                   (assert-equal nothing (pro-view (pro-at 'age) result))))
            
            (define-test key-affine-test
              (assert-equal (just "Alice") (pro-preview (pro-key 'name) test-alist))
              (assert-equal nothing (pro-preview (pro-key 'missing) test-alist))))

;;; ============================================================
;;; Composition Tests
;;; ============================================================

(test-group pro-composition
            (define-test lens-lens-compose-test
              (let* ([nested (cons (cons 1 2) 3)]
                     [inner-first (pro-compose pro-_1 pro-_1)])
                    (assert-equal 1 (pro-view inner-first nested))
                    (assert-equal (cons (cons 42 2) 3) (pro-set inner-first 42 nested))))
            
            (define-test lens-lens-deep-test
              (let* ([deep (cons (cons (cons 'a 'b) 'c) 'd)]
                     [deep-lens (pro-compose pro-_1 (pro-compose pro-_1 pro-_1))])
                    (assert-equal 'a (pro-view deep-lens deep))))
            
            (define-test lens-with-affine-test
              (let* ([data (cons '(x y z) 10)]
                     [first-elem (pro-compose pro-_1 (pro-ix 0))])
                    (assert-equal (just 'x) (pro-preview first-elem data))
                    (assert-equal (cons '(X y z) 10) (pro-set first-elem 'X data))))
            
            (define-test prism-prism-compose-test
              (let* ([v (left (just 42))]
                     [deep-focus (pro-compose pro-_Left pro-_Just)])
                    (assert-equal (just 42) (pro-preview deep-focus v))
                    (assert-equal nothing (pro-preview deep-focus (left nothing)))
                    (assert-equal nothing (pro-preview deep-focus (right "hi")))))
            
            (define-test composition-preserves-laws-test
              (let* ([nested (cons (cons 10 20) 30)]
                     [inner (pro-compose pro-_1 pro-_1)]
                     [v 99])
                    ;; set-get law
                    (assert-equal v (pro-view inner (pro-set inner v nested)))
                    ;; get-set law
                    (assert-equal nested (pro-set inner (pro-view inner nested) nested)))))

;;; ============================================================
;;; Filtered Tests
;;; ============================================================

(test-group pro-filtered
            (define positive (pro-filtered (lambda (x) (> x 0))))
            
            (define-test filtered-match-test
              (assert-equal (just 5) (pro-preview positive 5))
              (assert-equal (just 1) (pro-preview positive 1)))
            
            (define-test filtered-no-match-test
              (assert-equal nothing (pro-preview positive 0))
              (assert-equal nothing (pro-preview positive -5)))
            
            (define-test filtered-set-test
              (assert-equal 99 (pro-set positive 99 5))
              (assert-equal -5 (pro-set positive 99 -5))))  ; No change when doesn't match

;;; ============================================================
;;; Migration Utilities Tests
;;; ============================================================

(test-group migration
            (define-test vl-to-pro-conversion-test
              (let* ([pro-l (vl-lens->pro-lens car (lambda (b s) (cons b (cdr s))))]
                     [pair (cons 1 2)])
                    (assert-equal 1 (pro-view pro-l pair))
                    (assert-equal (cons 99 2) (pro-set pro-l 99 pair))))
            
            (define-test pro-to-vl-conversion-test
              (let* ([converted (pro-lens->vl-lens pro-_1)]
                     [view-fn (car converted)]
                     [set-fn (cdr converted)]
                     [pair (cons 'a 'b)])
                    (assert-equal 'a (view-fn pair))
                    (assert-equal (cons 'X 'b) (set-fn 'X pair)))))

;;; ============================================================
;;; Run All Tests
;;; ============================================================

(display "\n")
(run-all-tests)
