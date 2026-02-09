;;; lattice/optics/test-optics.ss — Tests for Optics Tower
;;;
;;; Comprehensive tests for all optic types and their composition.

(load "core/testing/test-framework.ss")
(load "lattice/fp/templates.ss")
(load "lattice/optics/optics.ss")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define test-pair '(1 . 2))
(define test-list '(1 2 3 4 5))
(define test-nested '((1 2) (3 4) (5 6)))
(define test-maybe-just (just 42))
(define test-maybe-nothing nothing)
(define test-either-left (left "error"))
(define test-either-right (right 99))

;;; ============================================================
;;; Isomorphism Tests
;;; ============================================================

(test-group "Iso"
  (define-test "iso-id is identity"
    (assert-equal 42 (iso-view iso-id 42))
    (assert-equal 42 (iso-review iso-id 42)))

  (define-test "iso-swapped swaps pairs"
    (assert-equal '(2 . 1) (iso-view iso-swapped '(1 . 2)))
    (assert-equal '(1 . 2) (iso-review iso-swapped '(2 . 1))))

  (define-test "iso-reversed reverses lists"
    (assert-equal '(3 2 1) (iso-view iso-reversed '(1 2 3)))
    (assert-equal '(1 2 3) (iso-review iso-reversed '(3 2 1))))

  (define-test "iso-flip reverses direction"
    (let ([flipped (iso-flip iso-swapped)])
      (assert-equal '(1 . 2) (iso-view flipped '(2 . 1)))
      (assert-equal '(2 . 1) (iso-review flipped '(1 . 2)))))

  (define-test "iso-compose chains isos"
    (let ([double-swap (iso-compose iso-swapped iso-swapped)])
      (assert-equal '(1 . 2) (iso-view double-swap '(1 . 2)))))

  (define-test "iso-over modifies through iso"
    (assert-equal '(3 2 1)
      (iso-over iso-reversed (lambda (xs) (map (lambda (x) (+ x 1)) xs)) '(2 1 0))))

  (define-test "verify-iso-laws for iso-swapped"
    (assert-true
      (verify-iso-laws iso-swapped
                       '((1 . 2) (a . b) ((x) . (y)))
                       '((2 . 1) (b . a) ((y) . (x)))))))

;;; ============================================================
;;; Lens Tests
;;; ============================================================

(test-group "Lens"
  (define-test "lens-fst focuses on car"
    (assert-equal 1 (view lens-fst test-pair))
    (assert-equal '(99 . 2) (set-lens lens-fst 99 test-pair)))

  (define-test "lens-snd focuses on cdr"
    (assert-equal 2 (view lens-snd test-pair))
    (assert-equal '(1 . 99) (set-lens lens-snd 99 test-pair)))

  (define-test "lens-nth focuses on list element"
    (assert-equal 3 (view (lens-nth 2) test-list))
    (assert-equal '(1 2 99 4 5) (set-lens (lens-nth 2) 99 test-list)))

  (define-test "lens-compose chains lenses"
    (let* ([data '((1 . 2) . (3 . 4))]
           [lens-fst-fst (lens-compose lens-fst lens-fst)])
      (assert-equal 1 (view lens-fst-fst data))
      (assert-equal '((99 . 2) . (3 . 4)) (set-lens lens-fst-fst 99 data))))

  (define-test "over modifies focused value"
    (assert-equal '(2 . 2) (over lens-fst (lambda (x) (+ x 1)) test-pair)))

  (define-test "lens-id is identity"
    (assert-equal 42 (view lens-id 42))
    (assert-equal 99 (set-lens lens-id 99 42)))

  (define-test "verify-lens-laws for lens-fst"
    (assert-true
      (verify-lens-laws lens-fst
                        '(((1 . 2) . 99) ((a . b) . c))))))

;;; ============================================================
;;; Prism Tests
;;; ============================================================

(test-group "Prism"
  (define-test "prism-just matches Just values"
    (assert-true (just? (preview prism-just test-maybe-just)))
    (assert-equal 42 (from-just (preview prism-just test-maybe-just))))

  (define-test "prism-just returns nothing for Nothing"
    (assert-true (nothing? (preview prism-just test-maybe-nothing))))

  (define-test "review constructs value"
    (assert-equal (just 42) (review prism-just 42)))

  (define-test "prism-left matches Left values"
    (assert-true (just? (preview prism-left test-either-left)))
    (assert-equal "error" (from-just (preview prism-left test-either-left))))

  (define-test "prism-left returns nothing for Right"
    (assert-true (nothing? (preview prism-left test-either-right))))

  (define-test "prism-right matches Right values"
    (assert-true (just? (preview prism-right test-either-right)))
    (assert-equal 99 (from-just (preview prism-right test-either-right))))

  (define-test "prism-cons matches non-empty lists"
    (let ([result (preview prism-cons test-list)])
      (assert-true (just? result))
      (assert-equal '(1 . (2 3 4 5)) (from-just result))))

  (define-test "prism-cons returns nothing for empty list"
    (assert-true (nothing? (preview prism-cons '()))))

  (define-test "prism-nil matches empty list"
    (assert-true (just? (preview prism-nil '())))
    (assert-true (nothing? (preview prism-nil test-list))))

  (define-test "prism-over modifies when matched"
    (assert-equal (just 43)
      (prism-over prism-just (lambda (x) (+ x 1)) test-maybe-just)))

  (define-test "prism-over returns unchanged when not matched"
    (assert-equal nothing
      (prism-over prism-just (lambda (x) (+ x 1)) test-maybe-nothing)))

  (define-test "prism-compose chains prisms"
    (let* ([nested-just (just (just 42))]
           [prism-just-just (prism-compose prism-just prism-just)])
      (assert-true (just? (preview prism-just-just nested-just)))
      (assert-equal 42 (from-just (preview prism-just-just nested-just)))))

  (define-test "verify-prism-laws for prism-just"
    (assert-true
      (verify-prism-laws prism-just
                         '(1 "hello" (a b c))
                         (list (just 1) (just "hello") nothing)))))

;;; ============================================================
;;; Affine Tests
;;; ============================================================

(test-group "Affine"
  (define-test "affine-id previews and sets"
    (assert-equal (just 42) (affine-preview affine-id 42))
    (assert-equal 99 (affine-set affine-id 99 42)))

  (define-test "affine-nth focuses on list element"
    (assert-equal (just 3) (affine-preview (affine-nth 2) test-list))
    (assert-true (nothing? (affine-preview (affine-nth 10) test-list))))

  (define-test "affine-nth sets element in list"
    (assert-equal '(1 2 99 4 5) (affine-set (affine-nth 2) 99 test-list))
    (assert-equal test-list (affine-set (affine-nth 10) 99 test-list)))  ; unchanged if out of bounds

  (define-test "lens->affine converts lens"
    (let ([affine-fst (lens->affine lens-fst)])
      (assert-equal (just 1) (affine-preview affine-fst test-pair))
      (assert-equal '(99 . 2) (affine-set affine-fst 99 test-pair))))

  (define-test "prism->affine converts prism"
    (let ([affine-just (prism->affine prism-just)])
      (assert-equal (just 42) (affine-preview affine-just test-maybe-just))
      (assert-true (nothing? (affine-preview affine-just test-maybe-nothing)))))

  (define-test "affine-compose chains affines"
    (let* ([affine-fst (lens->affine lens-fst)]
           [affine-just (prism->affine prism-just)]
           [composed (affine-compose affine-just affine-fst)]
           [data (just '(1 . 2))])
      (assert-equal (just 1) (affine-preview composed data))
      (assert-equal (just '(99 . 2)) (affine-set composed 99 data))))

  (define-test "affine-over modifies when target exists"
    (let ([affine-just (prism->affine prism-just)])
      (assert-equal (just 43)
        (affine-over affine-just (lambda (x) (+ x 1)) test-maybe-just)))))

;;; ============================================================
;;; Traversal Tests
;;; ============================================================

(test-group "Traversal"
  (define-test "traversal-each gets all elements"
    (assert-equal test-list (traversal-to-list traversal-each test-list)))

  (define-test "traversal-each modifies all elements"
    (assert-equal '(2 3 4 5 6)
      (traversal-over traversal-each (lambda (x) (+ x 1)) test-list)))

  (define-test "traversal-set sets all to same value"
    (assert-equal '(0 0 0 0 0)
      (traversal-set traversal-each 0 test-list)))

  (define-test "traversal-filtered only affects matching elements"
    (let ([trav-even (traversal-filtered even?)])
      (assert-equal '(2 4) (traversal-to-list trav-even test-list))
      (assert-equal '(1 0 3 0 5)
        (traversal-over trav-even (const 0) test-list))))

  (define-test "traversal-both traverses pair"
    (assert-equal '(1 2) (traversal-to-list traversal-both '(1 . 2)))
    (assert-equal '(2 . 3)
      (traversal-over traversal-both (lambda (x) (+ x 1)) '(1 . 2))))

  (define-test "traversal-just traverses Just"
    (assert-equal '(42) (traversal-to-list traversal-just test-maybe-just))
    (assert-equal '() (traversal-to-list traversal-just test-maybe-nothing)))

  (define-test "lens->traversal converts lens"
    (let ([trav-fst (lens->traversal lens-fst)])
      (assert-equal '(1) (traversal-to-list trav-fst test-pair))))

  (define-test "traversal-compose chains traversals"
    (let ([trav-nested (traversal-compose traversal-each traversal-each)])
      (assert-equal '(1 2 3 4 5 6)
        (traversal-to-list trav-nested test-nested))))

  (define-test "verify-traversal-laws for traversal-each"
    (assert-true
      (verify-traversal-laws traversal-each
                             '((1 2 3) () (a b))))))

;;; ============================================================
;;; Fold Tests
;;; ============================================================

(test-group "Fold"
  (define-test "fold-each gets all elements"
    (assert-equal test-list (fold-to-list fold-each test-list)))

  (define-test "fold-preview gets first element"
    (assert-equal (just 1) (fold-preview fold-each test-list))
    (assert-true (nothing? (fold-preview fold-each '()))))

  (define-test "fold-has checks for targets"
    (assert-true (fold-has fold-each test-list))
    (assert-false (fold-has fold-each '())))

  (define-test "fold-length counts targets"
    (assert-equal 5 (fold-length fold-each test-list))
    (assert-equal 0 (fold-length fold-each '())))

  (define-test "fold-all checks all"
    (assert-true (fold-all fold-each number? test-list))
    (assert-false (fold-all fold-each even? test-list)))

  (define-test "fold-any checks any"
    (assert-true (fold-any fold-each even? test-list))
    (assert-false (fold-any fold-each string? test-list)))

  (define-test "fold-sum sums numbers"
    (assert-equal 15 (fold-sum fold-each test-list)))

  (define-test "fold-filtered filters elements"
    (assert-equal '(2 4) (fold-to-list (fold-filtered even?) test-list)))

  (define-test "fold-taking limits results"
    (assert-equal '(1 2 3) (fold-to-list (fold-taking 3) test-list)))

  (define-test "fold-dropping skips elements"
    (assert-equal '(4 5) (fold-to-list (fold-dropping 3) test-list)))

  (define-test "fold-compose chains folds"
    (let ([fold-nested (fold-compose fold-each fold-each)])
      (assert-equal '(1 2 3 4 5 6)
        (fold-to-list fold-nested test-nested)))))

;;; ============================================================
;;; Getter Tests
;;; ============================================================

(test-group "Getter"
  (define-test "getter-id returns value"
    (assert-equal 42 (getter-view getter-id 42)))

  (define-test "getter-fst gets car"
    (assert-equal 1 (getter-view getter-fst test-pair)))

  (define-test "getter-snd gets cdr"
    (assert-equal 2 (getter-view getter-snd test-pair)))

  (define-test "getter-to lifts function"
    (let ([getter-len (getter-to length)])
      (assert-equal 5 (getter-view getter-len test-list))))

  (define-test "getter-compose chains getters"
    (let* ([data '((1 . 2) . 3)]
           [getter-fst-fst (getter-compose getter-fst getter-fst)])
      (assert-equal 1 (getter-view getter-fst-fst data))))

  (define-test "lens->getter converts lens"
    (let ([getter (lens->getter lens-fst)])
      (assert-equal 1 (getter-view getter test-pair)))))

;;; ============================================================
;;; Setter Tests
;;; ============================================================

(test-group "Setter"
  (define-test "setter-mapped modifies list elements"
    (assert-equal '(2 3 4 5 6)
      (setter-over setter-mapped (lambda (x) (+ x 1)) test-list)))

  (define-test "setter-set sets all elements"
    (assert-equal '(0 0 0 0 0)
      (setter-set setter-mapped 0 test-list)))

  (define-test "setter-compose chains setters"
    (let ([setter-nested (setter-compose setter-mapped setter-mapped)])
      (assert-equal '((2 3) (4 5) (6 7))
        (setter-over setter-nested (lambda (x) (+ x 1)) test-nested))))

  (define-test "lens->setter converts lens"
    (let ([setter (lens->setter lens-fst)])
      (assert-equal '(2 . 2)
        (setter-over setter (lambda (x) (+ x 1)) test-pair)))))

;;; ============================================================
;;; Unified Composition Tests
;;; ============================================================

(test-group "Unified Composition"
  (define-test "optic-compose iso+iso = iso"
    (let ([composed (optic-compose iso-swapped iso-swapped)])
      (assert-true (iso? composed))))

  (define-test "optic-compose lens+lens = lens"
    (let ([composed (optic-compose lens-fst lens-snd)])
      (assert-true (lens? composed))))

  (define-test "optic-compose prism+prism = prism"
    (let ([composed (optic-compose prism-just prism-just)])
      (assert-true (prism? composed))))

  (define-test "optic-compose lens+prism = affine"
    (let ([composed (optic-compose lens-fst prism-just)])
      (assert-true (affine? composed))))

  (define-test "optic-compose prism+lens = affine"
    (let ([composed (optic-compose prism-just lens-fst)])
      (assert-true (affine? composed))))

  (define-test "optic-compose with traversal = traversal"
    (let ([composed (optic-compose traversal-each lens-fst)])
      (assert-true (traversal? composed))))

  (define-test "optic-compose with fold = fold"
    (let ([composed (optic-compose fold-each (lens->fold lens-fst))])
      (assert-true (fold-optic? composed)))))

;;; ============================================================
;;; Operator Tests
;;; ============================================================

(test-group "Operators"
  (define-test "^. views through lens"
    (assert-equal 1 (^. test-pair lens-fst)))

  (define-test "^. views through getter"
    (assert-equal 1 (^. test-pair getter-fst)))

  (define-test "^? previews through lens"
    (assert-equal (just 1) (^? test-pair lens-fst)))

  (define-test "^? previews through prism"
    (assert-equal (just 42) (^? test-maybe-just prism-just))
    (assert-true (nothing? (^? test-maybe-nothing prism-just))))

  (define-test "^.. gets all through traversal"
    (assert-equal test-list (^.. test-list traversal-each)))

  (define-test ".~ sets value"
    (assert-equal '(99 . 2) (& test-pair (.~ lens-fst 99))))

  (define-test "%~ modifies value"
    (assert-equal '(2 . 2) (& test-pair (%~ lens-fst (lambda (x) (+ x 1))))))

  (define-test "& chains operations"
    (assert-equal '(10 . 2)
      (& test-pair
         (%~ lens-fst (lambda (x) (* x 10)))))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group "Integration"
  (define-test "compose lens through nested structure"
    (let* ([data '((name . "Alice") (age . 30))]
           ;; Lens to get value of 'name key (using lens-key from templates)
           [name-lens (lens-key 'name)])
      (assert-equal "Alice" (view name-lens data))
      (assert-equal '((name . "Bob") (age . 30))
        (set-lens name-lens "Bob" data))))

  (define-test "traverse filtered elements in nested lists"
    (let* ([data '((1 2 3) (4 5 6) (7 8 9))]
           [trav (traversal-compose traversal-each (traversal-filtered even?))])
      (assert-equal '(2 4 6 8) (traversal-to-list trav data))))

  (define-test "affine through maybe in pair"
    (let* ([data (cons (just 42) "hello")]
           [affine (affine-compose (lens->affine lens-fst) (prism->affine prism-just))])
      (assert-equal (just 42) (affine-preview affine data))
      (assert-equal (cons (just 99) "hello") (affine-set affine 99 data))))

  (define-test "iso preserves structure through transformations"
    (let* ([original '(1 2 3)]
           [transformed (iso-view iso-reversed original)]
           [back (iso-review iso-reversed transformed)])
      (assert-equal original back))))

;;; ============================================================
;;; Grate Tests
;;; ============================================================

(test-group "Grate"
  (define-test "grate-id is identity"
    (assert-equal 42 (grate-over grate-id identity 42))
    (assert-equal 42 (grate-review grate-id 42)))

  (define-test "grate-fn produces function result"
    ;; grate-fn : Grate (x -> a) (x -> b) a b
    ;; grate-review produces a constant function
    (let ([const-fn (grate-review grate-fn 42)])
      (assert-equal 42 (const-fn 'anything))
      (assert-equal 42 (const-fn 999))))

  (define-test "grate-fn with grate-over"
    ;; Modify a function by mapping over its results
    (let* ([original-fn (lambda (x) (* x 2))]
           [modified-fn (grate-over grate-fn add1 original-fn)])
      (assert-equal 5 (modified-fn 2))   ; (* 2 2) + 1 = 5
      (assert-equal 11 (modified-fn 5)))) ; (* 5 2) + 1 = 11

  (define-test "grate-pair-same works on homogeneous pairs"
    ;; grate-pair-same : Grate (a . a) (b . b) a b
    (assert-equal '(11 . 21)
      (grate-over grate-pair-same add1 '(10 . 20))))

  (define-test "grate-pair-same review creates pair of same value"
    (assert-equal '(42 . 42)
      (grate-review grate-pair-same 42)))

  (define-test "grate-zipWith zips two structures"
    ;; Zip two pairs together using +
    (assert-equal '(4 . 6)
      (grate-zipWith grate-pair-same + '(1 . 2) '(3 . 4))))

  (define-test "grate-zipWith3 zips three structures"
    ;; Zip three pairs
    (assert-equal '(6 . 9)
      (grate-zipWith3 grate-pair-same
                      (lambda (a b c) (+ a b c))
                      '(1 . 2) '(2 . 3) '(3 . 4))))

  (define-test "grate-zip pairs elements"
    (assert-equal '((1 . 3) . (2 . 4))
      (grate-zip grate-pair-same '(1 . 2) '(3 . 4))))

  (define-test "grate-list-rep works on fixed-length lists"
    ;; grate-list-rep 3 : Grate (List a) (List b) a b for lists of length 3
    (let ([g3 (grate-list-rep 3)])
      (assert-equal '(2 3 4)
        (grate-over g3 add1 '(1 2 3)))
      (assert-equal '(42 42 42)
        (grate-review g3 42))))

  (define-test "grate-list-rep zipWith sums corresponding elements"
    (let ([g3 (grate-list-rep 3)])
      (assert-equal '(5 7 9)
        (grate-zipWith g3 + '(1 2 3) '(4 5 6)))))

  (define-test "grate-list-rep 0 handles empty lists"
    ;; Edge case: zero-length lists (n=0)
    (let ([g0 (grate-list-rep 0)])
      (assert-equal '()
        (grate-over g0 add1 '()))
      (assert-equal '()
        (grate-review g0 42))
      (assert-equal '()
        (grate-zipWith g0 + '() '()))))

  (define-test "grate-compose chains grates"
    ;; Compose grate-fn with grate-pair-same
    ;; Result: Grate ((x -> a) . (x -> a)) ((x -> b) . (x -> b)) a b
    (let* ([composed (grate-compose grate-pair-same grate-fn)]
           ;; A pair of functions
           [fns (cons (lambda (x) (+ x 1))
                      (lambda (x) (* x 2)))]
           ;; Modify both functions
           [modified (grate-over composed (lambda (a) (* a 10)) fns)])
      ;; modified should be a pair of modified functions
      (assert-equal 20 ((car modified) 1))   ; (1 + 1) * 10 = 20
      (assert-equal 40 ((cdr modified) 2)))) ; (2 * 2) * 10 = 40

  (define-test "iso->grate converts iso to grate"
    ;; iso->grate creates a grate where grate-over does: backward(f(forward(s)))
    (let ([g (iso->grate iso-swapped)])
      ;; With identity, we get back the original (forward then backward)
      (assert-equal '(1 . 2)
        (grate-over g identity '(1 . 2)))
      ;; With a pair-transforming function:
      ;; swap('(1 . 2)) = '(2 . 1), f('(2 . 1)) = '(3 . 1), swap('(3 . 1)) = '(1 . 3)
      (assert-equal '(1 . 3)
        (grate-over g (lambda (p) (cons (add1 (car p)) (cdr p))) '(1 . 2)))))

  (define-test "grate->setter allows use as setter"
    (let ([s (grate->setter grate-pair-same)])
      (assert-equal '(11 . 21)
        (setter-over s add1 '(10 . 20)))))

  (define-test "verify-grate-laws for grate-pair-same"
    (assert-true
      (verify-grate-laws grate-pair-same '((1 . 2) (10 . 20) (a . b)))))

  (define-test "verify-grate-laws for grate-list-rep"
    (assert-true
      (verify-grate-laws (grate-list-rep 3) '((1 2 3) (10 20 30)))))

  (define-test "grate-fn zipWith applies same argument to two functions"
    (let* ([f (lambda (x) (+ x 1))]
           [g (lambda (x) (* x 2))])
      ;; zipWith on grate-fn: apply both functions to same input, combine results
      ;; grate-zipWith on grate-fn produces a new function
      (let ([h (grate-zipWith grate-fn + f g)])
        ;; h(x) = f(x) + g(x) = (x+1) + (x*2)
        (assert-equal 10 (h 3)))))  ; (3+1) + (3*2) = 4 + 6 = 10

  (define-test "grate with operators"
    ;; %~ works with grates
    (assert-equal '(11 . 21)
      (& '(10 . 20) (%~ grate-pair-same add1))))

  (define-test ".~ sets through grate (review)"
    (assert-equal '(99 . 99)
      (& '(1 . 2) (.~ grate-pair-same 99)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
