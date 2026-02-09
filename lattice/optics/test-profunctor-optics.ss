;;; lattice/optics/test-profunctor-optics.ss — Tests for Profunctor Optics
;;;
;;; Comprehensive tests for profunctor-based optics including:
;;;   - Profunctor type class instances
;;;   - Strong and Choice profunctors
;;;   - Profunctor Iso, Lens, Prism, Affine
;;;   - Conversions to/from concrete optics
;;;   - Composition and law verification

(load "core/testing/test-framework.ss")
(load "lattice/optics/profunctor-optics.ss")

;;; ============================================================
;;; Test Data
;;; ============================================================

(define test-pair '(1 . 2))
(define test-nested '((10 . 20) . (30 . 40)))
(define test-list '(1 2 3 4 5))
(define test-maybe-just (just 42))
(define test-maybe-nothing nothing)
(define test-either-left (left "error"))
(define test-either-right (right 99))

;;; ============================================================
;;; Profunctor Type Class Tests
;;; ============================================================

(test-group "Profunctor"
  (define-test "profunctor-fn dimap composes correctly"
    (let* ([f (lambda (x) (- x 1))]    ; a' -> a
           [g (lambda (x) (* x 2))]    ; b -> b'
           [h (lambda (x) (+ x 10))]   ; a -> b
           [result (dimap profunctor-fn f g h)])
      ;; (dimap f g h) x = g (h (f x))
      ;; result 5 = g (h (f 5)) = g (h 4) = g 14 = 28
      (assert-equal 28 (result 5))))

  (define-test "lmap applies to first argument"
    (let* ([f (lambda (x) (+ x 100))]
           [h (lambda (x) (* x 2))]
           [result (lmap profunctor-fn f h)])
      ;; (lmap f h) x = h (f x)
      ;; result 5 = h (f 5) = h 105 = 210
      (assert-equal 210 (result 5))))

  (define-test "rmap applies to second argument"
    (let* ([g (lambda (x) (+ x 1))]
           [h (lambda (x) (* x 3))]
           [result (rmap profunctor-fn g h)])
      ;; (rmap g h) x = g (h x)
      ;; result 5 = g (h 5) = g 15 = 16
      (assert-equal 16 (result 5))))

  (define-test "dimap id id = id"
    (let* ([h (lambda (x) (+ x 10))]
           [result (dimap profunctor-fn identity identity h)])
      (assert-equal 15 (result 5))
      (assert-equal 15 (h 5)))))

;;; ============================================================
;;; Strong Profunctor Tests
;;; ============================================================

(test-group "Strong"
  (define-test "strong-fn pfirst applies to car of pair"
    (let* ([f (lambda (x) (* x 2))]
           [result (pfirst strong-fn f)])
      (assert-equal '(10 . "hello") (result '(5 . "hello")))))

  (define-test "strong-fn psecond applies to cdr of pair"
    (let* ([f (lambda (x) (string-append x "!"))]
           [result (psecond strong-fn f)])
      (assert-equal '(5 . "hello!") (result '(5 . "hello")))))

  (define-test "pfirst preserves second element"
    (let* ([f (lambda (x) (* x 10))]
           [result (pfirst strong-fn f)])
      (assert-equal '(100 . keep-me) (result '(10 . keep-me)))))

  (define-test "psecond preserves first element"
    (let* ([f (lambda (x) (+ x 1))]
           [result (psecond strong-fn f)])
      (assert-equal '(keep-me . 6) (result '(keep-me . 5))))))

;;; ============================================================
;;; Choice Profunctor Tests
;;; ============================================================

(test-group "Choice"
  (define-test "choice-fn pleft applies to Left values"
    (let* ([f (lambda (x) (* x 2))]
           [result (pleft choice-fn f)])
      (assert-true (left? (result (left 5))))
      (assert-equal 10 (from-left (result (left 5))))))

  (define-test "choice-fn pleft passes through Right values"
    (let* ([f (lambda (x) (* x 2))]
           [result (pleft choice-fn f)])
      (assert-true (right? (result (right "unchanged"))))
      (assert-equal "unchanged" (from-right (result (right "unchanged"))))))

  (define-test "choice-fn pright applies to Right values"
    (let* ([f (lambda (x) (+ x 100))]
           [result (pright choice-fn f)])
      (assert-true (right? (result (right 5))))
      (assert-equal 105 (from-right (result (right 5))))))

  (define-test "choice-fn pright passes through Left values"
    (let* ([f (lambda (x) (+ x 100))]
           [result (pright choice-fn f)])
      (assert-true (left? (result (left "error"))))
      (assert-equal "error" (from-left (result (left "error"))))))

  (define-test "swap-either swaps Left and Right"
    (assert-true (right? (swap-either (left 42))))
    (assert-equal 42 (from-right (swap-either (left 42))))
    (assert-true (left? (swap-either (right "hello"))))
    (assert-equal "hello" (from-left (swap-either (right "hello"))))))

;;; ============================================================
;;; Forget and Tagged Tests
;;; ============================================================

(test-group "Forget"
  (define-test "make-forget wraps function"
    (let ([fg (make-forget (lambda (x) (+ x 1)))])
      (assert-true (forget? fg))
      (assert-equal 6 ((run-forget fg) 5))))

  (define-test "profunctor-forget dimap ignores g"
    (let* ([fg (make-forget (lambda (x) (* x 2)))]
           [result (dimap profunctor-forget
                          (lambda (x) (+ x 10))  ; f
                          (lambda (x) "ignored") ; g
                          fg)])
      ;; Should compose with f, ignore g
      (assert-equal 30 ((run-forget result) 5))))

  (define-test "strong-forget pfirst extracts car"
    (let* ([fg (make-forget (lambda (x) (* x 3)))]
           [result (pfirst strong-forget fg)])
      (assert-equal 15 ((run-forget result) '(5 . "ignored"))))))

(test-group "Tagged"
  (define-test "make-tagged wraps value"
    (let ([tg (make-tagged 42)])
      (assert-true (tagged? tg))
      (assert-equal 42 (run-tagged tg))))

  (define-test "profunctor-tagged dimap ignores f"
    (let* ([tg (make-tagged 10)]
           [result (dimap profunctor-tagged
                          (lambda (x) "ignored")  ; f
                          (lambda (x) (* x 2))    ; g
                          tg)])
      (assert-equal 20 (run-tagged result))))

  (define-test "choice-tagged pleft wraps in Left"
    (let* ([tg (make-tagged 42)]
           [result (pleft choice-tagged tg)])
      (assert-true (left? (run-tagged result)))
      (assert-equal 42 (from-left (run-tagged result))))))

;;; ============================================================
;;; Profunctor Iso Tests
;;; ============================================================

(test-group "PIso"
  (define-test "p-iso-id is identity"
    (assert-equal 42 ((p-iso-forward p-iso-id) 42))
    (assert-equal 42 ((p-iso-backward p-iso-id) 42)))

  (define-test "p-iso-swapped swaps pairs"
    (assert-equal '(2 . 1) ((p-iso-forward p-iso-swapped) '(1 . 2)))
    (assert-equal '(1 . 2) ((p-iso-backward p-iso-swapped) '(2 . 1))))

  (define-test "p-iso-reversed reverses lists"
    (assert-equal '(3 2 1) ((p-iso-forward p-iso-reversed) '(1 2 3)))
    (assert-equal '(1 2 3) ((p-iso-backward p-iso-reversed) '(3 2 1))))

  (define-test "p-iso-compose chains isos"
    (let ([double-swap (p-iso-compose p-iso-swapped p-iso-swapped)])
      (assert-equal '(1 . 2) ((p-iso-forward double-swap) '(1 . 2)))))

  (define-test "run-p-iso applies via profunctor"
    (let* ([result (run-p-iso profunctor-fn p-iso-swapped (lambda (x) (cons (+ (car x) 1) (cdr x))))])
      ;; dimap swap swap f
      ;; (1 . 2) -> swap -> (2 . 1) -> f -> (3 . 1) -> swap -> (1 . 3)
      (assert-equal '(1 . 3) (result '(1 . 2)))))

  (define-test "verify-p-iso-laws for p-iso-swapped"
    (assert-true
     (verify-p-iso-laws p-iso-swapped
                        '((1 . 2) (a . b))
                        '((2 . 1) (b . a))))))

;;; ============================================================
;;; Profunctor Lens Tests
;;; ============================================================

(test-group "PLens"
  (define-test "p-lens-fst focuses on car"
    (assert-equal 1 (p-view p-lens-fst test-pair))
    (assert-equal '(99 . 2) (p-set p-lens-fst 99 test-pair)))

  (define-test "p-lens-snd focuses on cdr"
    (assert-equal 2 (p-view p-lens-snd test-pair))
    (assert-equal '(1 . 99) (p-set p-lens-snd 99 test-pair)))

  (define-test "p-lens-head focuses on list head"
    (assert-equal 1 (p-view p-lens-head test-list))
    (assert-equal '(99 2 3 4 5) (p-set p-lens-head 99 test-list)))

  (define-test "p-lens-nth focuses on nth element"
    (assert-equal 3 (p-view (p-lens-nth 2) test-list))
    (assert-equal '(1 2 99 4 5) (p-set (p-lens-nth 2) 99 test-list)))

  (define-test "p-over modifies value"
    (assert-equal '(2 . 2) (p-over p-lens-fst (lambda (x) (+ x 1)) test-pair)))

  (define-test "p-lens-compose chains lenses"
    (let* ([nested (p-lens-compose p-lens-fst p-lens-fst)])
      (assert-equal 10 (p-view nested test-nested))
      (assert-equal '((99 . 20) . (30 . 40)) (p-set nested 99 test-nested))))

  (define-test "run-p-lens uses strong profunctor"
    (let* ([f (lambda (x) (* x 2))]
           [result (run-p-lens strong-fn p-lens-fst f)])
      (assert-equal '(20 . 2) (result '(10 . 2)))))

  (define-test "verify-p-lens-laws for p-lens-fst"
    (assert-true
     (verify-p-lens-laws p-lens-fst
                         '(((1 . 2) . 99) ((a . b) . c))))))

;;; ============================================================
;;; Profunctor Prism Tests
;;; ============================================================

(test-group "PPrism"
  (define-test "p-prism-just matches Just values"
    (let ([result (p-preview p-prism-just test-maybe-just)])
      (assert-true (just? result))
      (assert-equal 42 (from-just result))))

  (define-test "p-prism-just returns nothing for Nothing"
    (assert-true (nothing? (p-preview p-prism-just test-maybe-nothing))))

  (define-test "p-review constructs value"
    (assert-equal (just 42) (p-review p-prism-just 42)))

  (define-test "p-prism-left matches Left values"
    (let ([result (p-preview p-prism-left test-either-left)])
      (assert-true (just? result))
      (assert-equal "error" (from-just result))))

  (define-test "p-prism-right matches Right values"
    (let ([result (p-preview p-prism-right test-either-right)])
      (assert-true (just? result))
      (assert-equal 99 (from-just result))))

  (define-test "p-prism-cons matches non-empty lists"
    (let ([result (p-preview p-prism-cons test-list)])
      (assert-true (just? result))
      (assert-equal '(1 . (2 3 4 5)) (from-just result))))

  (define-test "p-prism-cons returns nothing for empty list"
    (assert-true (nothing? (p-preview p-prism-cons '()))))

  (define-test "p-prism-nil matches empty list"
    (assert-true (just? (p-preview p-prism-nil '())))
    (assert-true (nothing? (p-preview p-prism-nil test-list))))

  (define-test "p-prism-over modifies when matched"
    (assert-equal (just 43)
                  (p-prism-over p-prism-just (lambda (x) (+ x 1)) test-maybe-just)))

  (define-test "p-prism-over returns unchanged when not matched"
    (assert-equal nothing
                  (p-prism-over p-prism-just (lambda (x) (+ x 1)) test-maybe-nothing)))

  (define-test "p-prism-compose chains prisms"
    (let* ([nested-just (just (just 42))]
           [composed (p-prism-compose p-prism-just p-prism-just)])
      (let ([result (p-preview composed nested-just)])
        (assert-true (just? result))
        (assert-equal 42 (from-just result)))))

  (define-test "run-p-prism uses choice profunctor"
    (let* ([f (lambda (x) (* x 2))]
           [result (run-p-prism choice-fn p-prism-just f)])
      (assert-equal (just 84) (result (just 42)))
      (assert-equal nothing (result nothing))))

  (define-test "verify-p-prism-laws for p-prism-just"
    (assert-true
     (verify-p-prism-laws p-prism-just
                          '(1 "hello" (a b c))
                          (list (just 1) (just "hello") nothing)))))

;;; ============================================================
;;; Profunctor Affine Tests
;;; ============================================================

(test-group "PAffine"
  (define-test "p-affine-nth focuses on element"
    (let ([result (p-affine-preview-fn (p-affine-nth 2) test-list)])
      (assert-true (just? result))
      (assert-equal 3 (from-just result))))

  (define-test "p-affine-nth returns nothing for out of bounds"
    (assert-true (nothing? (p-affine-preview-fn (p-affine-nth 10) test-list))))

  (define-test "p-affine-set-fn modifies element"
    (assert-equal '(1 2 99 4 5)
                  (p-affine-set-fn (p-affine-nth 2) 99 test-list)))

  (define-test "p-affine-set-fn returns unchanged for out of bounds"
    (assert-equal test-list
                  (p-affine-set-fn (p-affine-nth 10) 99 test-list)))

  (define-test "p-affine-compose chains affines"
    (let* ([composed (p-affine-compose (p-affine-nth 0) (p-affine-nth 0))]
           [data '((1 2 3) (4 5 6))])
      (let ([result (p-affine-preview-fn composed data)])
        (assert-true (just? result))
        (assert-equal 1 (from-just result))))))

;;; ============================================================
;;; Conversion Tests
;;; ============================================================

(test-group "Conversions"
  (define-test "iso->p-iso preserves behavior"
    (let* ([concrete iso-swapped]
           [profunctor (iso->p-iso concrete)])
      (assert-equal '(2 . 1) ((p-iso-forward profunctor) '(1 . 2)))
      (assert-equal '(1 . 2) ((p-iso-backward profunctor) '(2 . 1)))))

  (define-test "p-iso->iso round-trips"
    (let* ([original p-iso-swapped]
           [concrete (p-iso->iso original)]
           [back (iso->p-iso concrete)])
      (assert-equal ((p-iso-forward original) '(1 . 2))
                    ((p-iso-forward back) '(1 . 2)))))

  (define-test "lens->p-lens preserves behavior"
    (let* ([concrete lens-fst]
           [profunctor (lens->p-lens concrete)])
      (assert-equal 1 (p-view profunctor test-pair))
      (assert-equal '(99 . 2) (p-set profunctor 99 test-pair))))

  (define-test "p-lens->lens round-trips"
    (let* ([original p-lens-fst]
           [concrete (p-lens->lens original)]
           [back (lens->p-lens concrete)])
      (assert-equal (p-view original test-pair)
                    (p-view back test-pair))))

  (define-test "prism->p-prism preserves behavior"
    (let* ([concrete prism-just]
           [profunctor (prism->p-prism concrete)])
      (assert-true (just? (p-preview profunctor test-maybe-just)))
      (assert-true (nothing? (p-preview profunctor test-maybe-nothing)))))

  (define-test "p-prism->prism round-trips"
    (let* ([original p-prism-just]
           [concrete (p-prism->prism original)]
           [back (prism->p-prism concrete)])
      (let ([r1 (p-preview original test-maybe-just)]
            [r2 (p-preview back test-maybe-just)])
        (assert-true (just? r1))
        (assert-true (just? r2))
        (assert-equal (from-just r1) (from-just r2)))))

  (define-test "affine->p-affine preserves behavior"
    (let* ([concrete (affine-nth 2)]
           [profunctor (affine->p-affine concrete)])
      (let ([result (p-affine-preview-fn profunctor test-list)])
        (assert-true (just? result))
        (assert-equal 3 (from-just result)))))

  (define-test "p-affine->affine round-trips"
    (let* ([original (p-affine-nth 2)]
           [concrete (p-affine->affine original)]
           [back (affine->p-affine concrete)])
      (let ([r1 (p-affine-preview-fn original test-list)]
            [r2 (p-affine-preview-fn back test-list)])
        (assert-true (just? r1))
        (assert-true (just? r2))
        (assert-equal (from-just r1) (from-just r2))))))

;;; ============================================================
;;; Unified Composition Tests
;;; ============================================================

(test-group "Unified Composition"
  (define-test "p-optic-compose iso+iso = iso"
    (let ([composed (p-optic-compose p-iso-swapped p-iso-swapped)])
      (assert-true (p-iso? composed))))

  (define-test "p-optic-compose lens+lens = lens"
    (let ([composed (p-optic-compose p-lens-fst p-lens-snd)])
      (assert-true (p-lens? composed))
      ;; ((a . b) . c) -> fst -> (a . b) -> snd -> b
      (assert-equal 20 (p-view composed test-nested))))

  (define-test "p-optic-compose prism+prism = prism"
    (let ([composed (p-optic-compose p-prism-just p-prism-just)])
      (assert-true (p-prism? composed))))

  (define-test "p-optic-compose lens+prism = affine"
    (let ([composed (p-optic-compose p-lens-fst p-prism-just)])
      (assert-true (p-affine? composed))))

  (define-test "p-optic-compose prism+lens = affine"
    (let ([composed (p-optic-compose p-prism-just p-lens-fst)])
      (assert-true (p-affine? composed))))

  (define-test "p-optic-compose iso+lens = lens"
    (let ([composed (p-optic-compose p-iso-swapped p-lens-fst)])
      (assert-true (p-lens? composed))
      ;; (1 . 2) -> swap -> (2 . 1) -> fst -> 2
      (assert-equal 2 (p-view composed '(1 . 2)))))

  (define-test "p-optic-compose lens+iso = lens"
    (let ([composed (p-optic-compose p-lens-fst p-iso-swapped)])
      (assert-true (p-lens? composed))
      ;; ((1 . 2) . 3) -> fst -> (1 . 2) -> swap -> (2 . 1)
      (assert-equal '(2 . 1) (p-view composed '((1 . 2) . 3)))))

  (define-test "p-optic-compose iso+prism = prism"
    (let* ([composed (p-optic-compose p-iso-id p-prism-just)])
      (assert-true (p-prism? composed))))

  (define-test "composed lens+prism works correctly"
    (let* ([composed (p-optic-compose p-lens-fst p-prism-just)]
           [data (cons (just 42) "hello")])
      (let ([result (p-affine-preview-fn composed data)])
        (assert-true (just? result))
        (assert-equal 42 (from-just result)))
      (assert-equal (cons (just 99) "hello")
                    (p-affine-set-fn composed 99 data)))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(test-group "Integration"
  (define-test "profunctor lens matches concrete lens behavior"
    (let* ([concrete lens-fst]
           [profunctor p-lens-fst])
      (assert-equal (view concrete test-pair)
                    (p-view profunctor test-pair))
      (assert-equal (set-lens concrete 99 test-pair)
                    (p-set profunctor 99 test-pair))))

  (define-test "profunctor prism matches concrete prism behavior"
    (let* ([concrete prism-just]
           [profunctor p-prism-just])
      ;; Preview
      (let ([c-result (preview concrete test-maybe-just)]
            [p-result (p-preview profunctor test-maybe-just)])
        (assert-true (just? c-result))
        (assert-true (just? p-result))
        (assert-equal (from-just c-result) (from-just p-result)))
      ;; Review
      (assert-equal (review concrete 99)
                    (p-review profunctor 99))))

  (define-test "deep lens composition"
    (let* ([l1 p-lens-fst]
           [l2 p-lens-fst]
           [l3 p-lens-snd]
           [composed (p-optic-compose (p-optic-compose l1 l2) l3)]
           [data '(((a . b) . c) . d)])
      ;; ((a . b) . c) . d) -> fst -> ((a . b) . c) -> fst -> (a . b) -> snd -> b
      (assert-equal 'b (p-view composed data))))

  (define-test "mixed optic composition"
    (let* ([lens p-lens-fst]
           [prism p-prism-just]
           [affine (p-optic-compose lens prism)]
           [data (cons (just 42) "rest")])
      (let ([result (p-affine-preview-fn affine data)])
        (assert-true (just? result))
        (assert-equal 42 (from-just result)))
      ;; Modify through the affine
      (let ([modified (p-affine-set-fn affine 100 data)])
        (assert-equal (cons (just 100) "rest") modified))))

  (define-test "conversion preserves composition"
    (let* ([concrete-l1 lens-fst]
           [concrete-l2 lens-snd]
           [concrete-composed (lens-compose concrete-l1 concrete-l2)]
           [prof-l1 (lens->p-lens concrete-l1)]
           [prof-l2 (lens->p-lens concrete-l2)]
           [prof-composed (p-lens-compose prof-l1 prof-l2)]
           [data '((1 . 2) . 3)])
      (assert-equal (view concrete-composed data)
                    (p-view prof-composed data))
      (assert-equal (set-lens concrete-composed 99 data)
                    (p-set prof-composed 99 data)))))

;;; ============================================================
;;; Law Verification Tests
;;; ============================================================

(test-group "Laws"
  (define-test "profunctor-fn satisfies profunctor laws"
    ;; For function profunctors, we verify laws by checking behavior, not identity.
    ;; dimap id id f should behave the same as f.
    (let* ([f (lambda (x) (+ x 1))]
           [result (dimap profunctor-fn identity identity f)])
      (assert-equal (f 5) (result 5))
      (assert-equal (f 10) (result 10))
      (assert-equal (f 0) (result 0))))

  (define-test "p-iso-swapped satisfies iso laws"
    (assert-true
     (verify-p-iso-laws p-iso-swapped
                        '((1 . 2) (a . b) ((x) . (y)))
                        '((2 . 1) (b . a) ((y) . (x))))))

  (define-test "p-iso-reversed satisfies iso laws"
    (assert-true
     (verify-p-iso-laws p-iso-reversed
                        '((1 2 3) () (a b))
                        '((3 2 1) () (b a)))))

  (define-test "p-lens-fst satisfies lens laws"
    (assert-true
     (verify-p-lens-laws p-lens-fst
                         '(((1 . 2) . 99) ((a . b) . c) (("x" . "y") . "z")))))

  (define-test "p-lens-snd satisfies lens laws"
    (assert-true
     (verify-p-lens-laws p-lens-snd
                         '(((1 . 2) . 99) ((a . b) . c)))))

  (define-test "p-prism-just satisfies prism laws"
    (assert-true
     (verify-p-prism-laws p-prism-just
                          '(1 "hello" (a b c))
                          (list (just 1) (just "hello") nothing))))

  (define-test "p-prism-left satisfies prism laws"
    (assert-true
     (verify-p-prism-laws p-prism-left
                          '(1 "error" 42)
                          (list (left 1) (left "error") (right "ignored")))))

  (define-test "p-prism-right satisfies prism laws"
    (assert-true
     (verify-p-prism-laws p-prism-right
                          '(1 "value" 42)
                          (list (right 1) (right "value") (left "ignored"))))))

;;; ============================================================
;;; Wander Profunctor Tests
;;; ============================================================

(test-group "Wander"
  (define-test "wander-fn applies traversal to list"
    (let* ([f (lambda (x) (* x 2))]
           [traverse-fn (lambda (g xs) (map g xs))]
           [result (pwander wander-fn traverse-fn f)])
      (assert-equal '(2 4 6) (result '(1 2 3)))))

  (define-test "wander-fn with identity traversal is identity"
    (let* ([f (lambda (x) (+ x 1))]
           [id-traverse (lambda (g x) (g x))]
           [result (pwander wander-fn id-traverse f)])
      (assert-equal 6 (result 5))))

  (define-test "wander-fn with pair traversal processes both"
    (let* ([f (lambda (x) (* x 3))]
           [pair-traverse (lambda (g p) (cons (g (car p)) (g (cdr p))))]
           [result (pwander wander-fn pair-traverse f)])
      (assert-equal '(3 . 6) (result '(1 . 2))))))

;;; ============================================================
;;; Profunctor Traversal Tests
;;; ============================================================

(test-group "PTraversal"
  (define-test "p-traversal-each traverses list elements"
    (assert-equal '(1 2 3 4 5)
                  (p-traversal-to-list p-traversal-each test-list)))

  (define-test "p-traversal-over modifies all elements"
    (assert-equal '(2 4 6 8 10)
                  (p-traversal-over p-traversal-each (lambda (x) (* x 2)) test-list)))

  (define-test "p-traversal-set sets all elements"
    (assert-equal '(0 0 0 0 0)
                  (p-traversal-set p-traversal-each 0 test-list)))

  (define-test "p-traversal-both traverses pair elements"
    (assert-equal '(1 2)
                  (p-traversal-to-list p-traversal-both test-pair)))

  (define-test "p-traversal-both modifies both elements"
    (assert-equal '(10 . 20)
                  (p-traversal-over p-traversal-both
                                    (lambda (x) (* x 10))
                                    test-pair)))

  (define-test "p-traversal-filtered only traverses matching"
    (let ([odd? (lambda (x) (odd? x))])
      (assert-equal '(1 3 5)
                    (p-traversal-to-list (p-traversal-filtered odd?) test-list))))

  (define-test "p-traversal-compose chains traversals"
    (let* ([nested '((1 2) (3 4))]
           [composed (p-traversal-compose p-traversal-each p-traversal-each)])
      (assert-equal '(1 2 3 4)
                    (p-traversal-to-list composed nested))
      (assert-equal '((10 20) (30 40))
                    (p-traversal-over composed (lambda (x) (* x 10)) nested))))

  (define-test "run-p-traversal uses wander profunctor"
    (let* ([f (lambda (x) (* x 2))]
           [result (run-p-traversal wander-fn p-traversal-each f)])
      (assert-equal '(2 4 6) (result '(1 2 3)))))

  (define-test "p-lens->p-traversal preserves lens behavior"
    (let* ([ptrav (p-lens->p-traversal p-lens-fst)])
      (assert-equal '(1)
                    (p-traversal-to-list ptrav test-pair))
      (assert-equal '(99 . 2)
                    (p-traversal-over ptrav (lambda (_) 99) test-pair))))

  (define-test "p-prism->p-traversal works with matching"
    (let* ([ptrav (p-prism->p-traversal p-prism-just)])
      (assert-equal '(42)
                    (p-traversal-to-list ptrav test-maybe-just))
      (assert-equal '()
                    (p-traversal-to-list ptrav test-maybe-nothing))))

  (define-test "traversal conversion round-trips"
    (let* ([original traversal-each]
           [profunctor (traversal->p-traversal original)]
           [back (p-traversal->traversal profunctor)])
      (assert-equal (traversal-to-list original test-list)
                    (traversal-to-list back test-list)))))

;;; ============================================================
;;; Profunctor Fold Tests
;;; ============================================================

(test-group "PFold"
  (define-test "p-fold-each folds list elements"
    (assert-equal '(1 2 3 4 5)
                  (p-fold-to-list p-fold-each test-list)))

  (define-test "p-fold-preview gets first element"
    (let ([result (p-fold-preview p-fold-each test-list)])
      (assert-true (just? result))
      (assert-equal 1 (from-just result))))

  (define-test "p-fold-preview returns nothing for empty"
    (assert-true (nothing? (p-fold-preview p-fold-each '()))))

  (define-test "p-fold-has checks existence"
    (assert-true (p-fold-has p-fold-each test-list))
    (assert-false (p-fold-has p-fold-each '())))

  (define-test "p-fold-length counts elements"
    (assert-equal 5 (p-fold-length p-fold-each test-list))
    (assert-equal 0 (p-fold-length p-fold-each '())))

  (define-test "p-fold-all checks predicate on all"
    (assert-true (p-fold-all p-fold-each number? test-list))
    (assert-false (p-fold-all p-fold-each (lambda (x) (> x 3)) test-list)))

  (define-test "p-fold-any checks predicate on any"
    (assert-true (p-fold-any p-fold-each (lambda (x) (> x 3)) test-list))
    (assert-false (p-fold-any p-fold-each (lambda (x) (> x 10)) test-list)))

  (define-test "p-fold-filtered only includes matching"
    (assert-equal '(2 4)
                  (p-fold-to-list (p-fold-filtered even?) test-list)))

  (define-test "p-fold-taking limits results"
    (assert-equal '(1 2 3)
                  (p-fold-to-list (p-fold-taking 3) test-list)))

  (define-test "p-fold-compose chains folds"
    (let* ([nested '((1 2) (3 4))]
           [composed (p-fold-compose p-fold-each p-fold-each)])
      (assert-equal '(1 2 3 4)
                    (p-fold-to-list composed nested))))

  (define-test "p-traversal->p-fold converts correctly"
    (let* ([pfold (p-traversal->p-fold p-traversal-each)])
      (assert-equal '(1 2 3 4 5)
                    (p-fold-to-list pfold test-list))))

  (define-test "p-lens->p-fold works correctly"
    (let* ([pfold (p-lens->p-fold p-lens-fst)])
      (assert-equal '(1)
                    (p-fold-to-list pfold test-pair))))

  (define-test "p-prism->p-fold works with matching"
    (let* ([pfold (p-prism->p-fold p-prism-just)])
      (assert-equal '(42)
                    (p-fold-to-list pfold test-maybe-just))
      (assert-equal '()
                    (p-fold-to-list pfold test-maybe-nothing)))))

;;; ============================================================
;;; Extended Composition Tests
;;; ============================================================

(test-group "Extended Composition"
  (define-test "p-optic-compose traversal+traversal = traversal"
    (let ([composed (p-optic-compose p-traversal-each p-traversal-each)])
      (assert-true (p-traversal? composed))))

  (define-test "p-optic-compose lens+traversal = traversal"
    (let* ([composed (p-optic-compose p-lens-fst p-traversal-each)]
           [data '((1 2 3) . rest)])
      (assert-true (p-traversal? composed))
      (assert-equal '(1 2 3)
                    (p-traversal-to-list composed data))))

  (define-test "p-optic-compose traversal+lens = traversal"
    (let* ([composed (p-optic-compose p-traversal-each p-lens-fst)]
           [data '((1 . a) (2 . b) (3 . c))])
      (assert-true (p-traversal? composed))
      (assert-equal '(1 2 3)
                    (p-traversal-to-list composed data))))

  (define-test "p-optic-compose prism+traversal = traversal"
    (let* ([composed (p-optic-compose p-prism-just p-traversal-each)]
           [data (just '(1 2 3))])
      (assert-true (p-traversal? composed))
      (assert-equal '(1 2 3)
                    (p-traversal-to-list composed data))))

  (define-test "p-optic-compose fold+fold = fold"
    (let ([composed (p-optic-compose p-fold-each p-fold-each)])
      (assert-true (p-fold? composed))))

  (define-test "p-optic-compose lens+fold = fold"
    (let* ([composed (p-optic-compose p-lens-fst p-fold-each)]
           [data '((1 2 3) . rest)])
      (assert-true (p-fold? composed))
      (assert-equal '(1 2 3)
                    (p-fold-to-list composed data))))

  (define-test "p-optic-compose traversal+fold = fold"
    (let* ([composed (p-optic-compose p-traversal-each p-fold-each)]
           [data '((1 2) (3 4))])
      (assert-true (p-fold? composed))
      (assert-equal '(1 2 3 4)
                    (p-fold-to-list composed data))))

  (define-test "->p-fold converts various optic types"
    (assert-true (p-fold? (->p-fold p-lens-fst)))
    (assert-true (p-fold? (->p-fold p-prism-just)))
    (assert-true (p-fold? (->p-fold p-traversal-each)))
    (assert-true (p-fold? (->p-fold (p-affine-nth 0)))))

  (define-test "composed traversal modification works"
    (let* ([data '((1 2 3) (4 5 6))]
           [composed (p-optic-compose p-traversal-each p-traversal-each)])
      (assert-equal '((10 20 30) (40 50 60))
                    (p-traversal-over composed (lambda (x) (* x 10)) data))))

  (define-test "complex nested traversal"
    (let* ([data '(((1 2) . a) ((3 4) . b))]
           [composed (p-optic-compose
                      (p-optic-compose p-traversal-each p-lens-fst)
                      p-traversal-each)])
      (assert-equal '(1 2 3 4)
                    (p-traversal-to-list composed data))
      (assert-equal '(((100 200) . a) ((300 400) . b))
                    (p-traversal-over composed (lambda (x) (* x 100)) data)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
