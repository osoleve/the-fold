(load "core/testing/test-framework.ss")
(load "lattice/fp/category/limits.ss")

(doc 'module 'test-limits)
(doc 'description "Tests for limits and colimits module")

;;; ============================================================
;;; Section 1: Shape Tests
;;; ============================================================

(test-group "diagram-shapes"
  (define-test "shape-empty has no objects"
    (assert-equal '() (shape-objects shape-empty)))

  (define-test "shape-discrete-2 has two objects"
    (assert-equal '(a b) (shape-objects shape-discrete-2)))

  (define-test "shape-parallel-pair has two parallel morphisms"
    (assert-equal 2 (length (shape-morphisms shape-parallel-pair))))

  (define-test "shape-cospan has correct structure"
    (assert-equal '(a b c) (shape-objects shape-cospan))
    (let ([morphs (shape-morphisms shape-cospan)])
      (assert-equal 2 (length morphs))
      (assert-true (pair? (member '(f a c) morphs)))
      (assert-true (pair? (member '(g b c) morphs)))))

  (define-test "shape-span has correct structure"
    (assert-equal '(a b c) (shape-objects shape-span))
    (let ([morphs (shape-morphisms shape-span)])
      (assert-equal 2 (length morphs))
      (assert-true (pair? (member '(f c a) morphs)))
      (assert-true (pair? (member '(g c b) morphs)))))

  (define-test "custom shape creation"
    (let ([s (make-shape 'triangle '(x y z) '((f x y) (g y z) (h x z)))])
      (assert-true (shape? s))
      (assert-equal 'triangle (shape-name s))
      (assert-equal 3 (length (shape-objects s)))
      (assert-equal 3 (length (shape-morphisms s))))))

;;; ============================================================
;;; Section 2: Diagram Tests
;;; ============================================================

(test-group "diagrams"
  (define-test "product diagram creation"
    (let ([d (make-product-diagram 'int 'string)])
      (assert-true (diagram? d))
      (assert-equal 'int (diagram-at d 'a))
      (assert-equal 'string (diagram-at d 'b))))

  (define-test "coproduct diagram creation"
    (let ([d (make-coproduct-diagram '(1 2 3) '(a b))])
      (assert-equal '(1 2 3) (diagram-at d 'a))
      (assert-equal '(a b) (diagram-at d 'b))))

  (define-test "equalizer diagram creation"
    (let ([d (make-equalizer-diagram '(1 2 3 4) '() add1 (lambda (x) (* 2 x)))])
      (assert-equal '(1 2 3 4) (diagram-at d 'a))
      (let ([f (diagram-mor d 'f)]
            [g (diagram-mor d 'g)])
        (assert-equal 3 (f 2))
        (assert-equal 4 (g 2)))))

  (define-test "pullback diagram creation"
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) (+ x 3))]
           [d (make-pullback-diagram '(1 2 3) '(4 5 6) 'nat f g)])
      (assert-equal '(1 2 3) (diagram-at d 'a))
      (assert-equal '(4 5 6) (diagram-at d 'b))
      (assert-equal 'nat (diagram-at d 'c)))))

;;; ============================================================
;;; Section 3: Cone and Cocone Tests
;;; ============================================================

(test-group "cones"
  (define-test "cone creation"
    (let* ([d (make-product-diagram 1 2)]
           [apex (cons 1 2)]
           [projs (lambda (obj)
                    (case obj
                      [(a) car]
                      [(b) cdr]))]
           [c (make-cone apex d projs)])
      (assert-true (cone? c))
      (assert-equal (cons 1 2) (cone-apex c))
      (assert-equal 1 ((cone-project c 'a) apex))
      (assert-equal 2 ((cone-project c 'b) apex))))

  (define-test "cocone creation"
    (let* ([d (make-coproduct-diagram 'int 'string)]
           [apex '(coproduct int string)]
           [injs (lambda (obj)
                   (case obj
                     [(a) make-left]
                     [(b) make-right]))]
           [c (make-cocone apex d injs)])
      (assert-true (cocone? c))
      (assert-true (left? ((cocone-inject c 'a) 42)))
      (assert-true (right? ((cocone-inject c 'b) "hello"))))))

;;; ============================================================
;;; Section 4: Product Tests
;;; ============================================================

(test-group "products"
  (define-test "binary product construction"
    (let ([p (binary-product 'int 'bool)])
      (assert-true (limit? p))
      (assert-equal (cons 'int 'bool) (limit-apex p))))

  (define-test "product projections"
    (let* ([p (binary-product 1 2)]
           [apex (limit-apex p)]
           [cone (limit-cone p)])
      (assert-equal 1 ((cone-project cone 'a) apex))
      (assert-equal 2 ((cone-project cone 'b) apex))))

  (define-test "product pairing morphism"
    (let ([pair (product-pair add1 (lambda (x) (* x 2)))])
      (assert-equal (cons 6 10) (pair 5))))

  (define-test "product universal property"
    (let* ([p (binary-product 'a 'b)]
           [other-apex 'x]
           [f (lambda (x) 'a)]
           [g (lambda (x) 'b)]
           [other-projs (lambda (obj)
                          (case obj
                            [(a) f]
                            [(b) g]))]
           [other-cone (make-cone other-apex (cone-diagram (limit-cone p)) other-projs)]
           [factor (limit-factor p other-cone)])
      (let ([result (factor 'x)])
        (assert-equal 'a (car result))
        (assert-equal 'b (cdr result)))))

  (define-test "n-ary product"
    (let ([p (n-ary-product '(1 2 3 4 5))])
      (assert-true (limit? p))
      (assert-equal '(1 2 3 4 5) (limit-apex p)))))

;;; ============================================================
;;; Section 5: Coproduct Tests
;;; ============================================================

(test-group "coproducts"
  (define-test "binary coproduct construction"
    (let ([c (binary-coproduct 'int 'bool)])
      (assert-true (colimit? c))))

  (define-test "coproduct injections"
    (assert-true (left? (coproduct-inl 42)))
    (assert-true (right? (coproduct-inr "hello")))
    (assert-equal 42 (from-left (coproduct-inl 42)))
    (assert-equal "hello" (from-right (coproduct-inr "hello"))))

  (define-test "coproduct copairing"
    (let ([copair (coproduct-copair add1 string-length)])
      (assert-equal 6 (copair (make-left 5)))
      (assert-equal 5 (copair (make-right "hello")))))

  (define-test "coproduct universal property"
    (let* ([c (binary-coproduct 'int 'string)]
           [other-apex 'target]
           [f (lambda (x) (list 'from-int x))]
           [g (lambda (x) (list 'from-string x))]
           [other-injs (lambda (obj)
                         (case obj
                           [(a) f]
                           [(b) g]))]
           [other-cocone (make-cocone other-apex
                                      (cocone-diagram (colimit-cocone c))
                                      other-injs)]
           [factor (colimit-factor c other-cocone)])
      (assert-equal '(from-int 42) (factor (make-left 42)))
      (assert-equal '(from-string "hi") (factor (make-right "hi"))))))

;;; ============================================================
;;; Section 6: Equalizer Tests
;;; ============================================================

(test-group "equalizers"
  (define-test "equalizer finds equal elements"
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) (+ x x))]
           [eq (equalizer '(1 2 3 4 5) f g)]
           [apex (limit-apex eq)])
      (assert-equal '(1 2 3 4 5) apex)))

  (define-test "equalizer filters non-equal elements"
    (let* ([f add1]
           [g (lambda (x) (* x 2))]
           [eq (equalizer '(0 1 2 3 4 5) f g)]
           [apex (limit-apex eq)])
      (assert-equal '(1) apex)))

  (define-test "equalizer with no solutions"
    (let* ([f add1]
           [g (lambda (x) (+ x 2))]
           [eq (equalizer '(1 2 3) f g)]
           [apex (limit-apex eq)])
      (assert-equal '() apex)))

  (define-test "equalizer inclusion morphism"
    (let* ([eq (equalizer '(1 2 3 4) add1 add1)]
           [incl (equalizer-inclusion eq)])
      (assert-equal '(1 2 3 4) (incl '(1 2 3 4))))))

;;; ============================================================
;;; Section 7: Coequalizer Tests
;;; ============================================================

(test-group "coequalizers"
  (define-test "coequalizer creates equivalence classes"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) (+ x 2))]
           [coeq (coequalizer '(0) '(0 1 2 3 4) f g)]
           [apex (colimit-apex coeq)])
      (assert-true (list? apex))))

  (define-test "coequalizer quotient map respects equivalence"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) (+ x 1))]
           [coeq (coequalizer '(1 2) '(1 2 3) f g)]
           [q (coequalizer-quotient coeq)])
      (assert-equal (q 1) (q 2))
      (assert-equal (q 2) (q 3))))

  (define-test "coequalizer with identical morphisms"
    (let* ([f (lambda (x) (* x 2))]
           [coeq (coequalizer '(1 2) '(1 2 3 4) f f)]
           [apex (colimit-apex coeq)])
      (assert-equal 4 (length apex)))))

;;; ============================================================
;;; Section 8: Pullback Tests
;;; ============================================================

(test-group "pullbacks"
  (define-test "pullback finds matching pairs"
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) x)]
           [pb (pullback '(1 2 3 4 5) '(1 2 3 4 5 6 7 8 9 10) 'nat f g)]
           [apex (limit-apex pb)])
      (assert-equal 5 (length apex))
      (assert-true (pair? (member '(1 . 2) apex)))
      (assert-true (pair? (member '(2 . 4) apex)))
      (assert-true (pair? (member '(3 . 6) apex)))
      (assert-true (pair? (member '(4 . 8) apex)))
      (assert-true (pair? (member '(5 . 10) apex)))))

  (define-test "pullback with no matches"
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) (+ x 100))]
           [pb (pullback '(1 2 3) '(1 2 3) 'nat f g)]
           [apex (limit-apex pb)])
      (assert-equal '() apex)))

  (define-test "pullback projections"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) x)]
           [pb (pullback '(1 2 3) '(1 2 3) 'nat f g)]
           [p1 (pullback-p1 pb)]
           [p2 (pullback-p2 pb)]
           [apex (limit-apex pb)])
      (assert-equal 3 (length (p1 apex)))
      (assert-equal 3 (length (p2 apex)))))

  (define-test "pullback of identical morphisms is diagonal"
    (let* ([f (lambda (x) x)]
           [pb (pullback '(a b c) '(a b c) 'set f f)]
           [apex (limit-apex pb)])
      (assert-equal 3 (length apex))
      (for-each (lambda (p) (assert-equal (car p) (cdr p))) apex))))

;;; ============================================================
;;; Section 9: Pushout Tests
;;; ============================================================

(test-group "pushouts"
  (define-test "pushout creates quotient of coproduct"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) x)]
           [po (pushout '(a b) '(a b) '(a b) f g)]
           [apex (colimit-apex po)])
      (assert-true (list? apex))))

  (define-test "pushout injections"
    (let* ([f (lambda (x) (string-append "a-" x))]
           [g (lambda (x) (string-append "b-" x))]
           [po (pushout '("1" "2") '("1" "2") '("x") f g)]
           [i1 (pushout-i1 po)]
           [i2 (pushout-i2 po)])
      (assert-equal (i1 "a-x") (i2 "b-x"))))

  (define-test "pushout universal property"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) (+ x 10))]
           [po (pushout '(1 2 3) '(11 12 13) '(1) f g)]
           [apex (colimit-apex po)])
      (let ([class-of-1 ((pushout-i1 po) 1)]
            [class-of-11 ((pushout-i2 po) 11)])
        (assert-equal class-of-1 class-of-11)))))

;;; ============================================================
;;; Section 10: Terminal and Initial Object Tests
;;; ============================================================

(test-group "terminal-initial"
  (define-test "terminal object construction"
    (let ([term (terminal-object 'unit)])
      (assert-true (limit? term))
      (assert-equal 'unit (limit-apex term))))

  (define-test "terminal morphism always returns unit"
    (let* ([term (terminal-object '*)]
           [bang (terminal-morphism term)])
      (assert-equal '* ((bang (make-cone 'anything
                                         (make-diagram shape-empty
                                                       (lambda (x) x)
                                                       (lambda (x) x))
                                         (lambda (x) x)))
                        'any-value))))

  (define-test "initial object construction"
    (let ([init (initial-object)])
      (assert-true (colimit? init))
      (assert-equal '() (colimit-apex init)))))

;;; ============================================================
;;; Section 11: Display Tests
;;; ============================================================

(test-group "display"
  (define-test "shape->string"
    (let ([s (shape->string shape-discrete-2)])
      (assert-true (string? s))
      (assert-true (string-contains? s "discrete-2"))))

  (define-test "diagram->string"
    (let* ([d (make-product-diagram 'a 'b)]
           [s (diagram->string d)])
      (assert-true (string? s))
      (assert-true (string-contains? s "discrete-2"))))

  (define-test "limit->string"
    (let* ([p (binary-product 1 2)]
           [s (limit->string p)])
      (assert-true (string? s))
      (assert-true (string-contains? s "Limit")))))

;;; Helper for string-contains?
(define (string-contains? str substr)
  (let ([str-len (string-length str)]
        [sub-len (string-length substr)])
    (let loop ([i 0])
      (cond
       [(> (+ i sub-len) str-len) #f]
       [(string=? (substring str i (+ i sub-len)) substr) #t]
       [else (loop (+ i 1))]))))

;;; ============================================================
;;; Section 12: Verification Tests
;;; ============================================================

(test-group "verification"
  (define-test "verify-cone for product"
    (let* ([p (binary-product 1 2)]
           [cone (limit-cone p)])
      (assert-true (verify-cone cone))))

  (define-test "verify-cocone for coproduct"
    (let* ([c (binary-coproduct 1 2)]
           [cocone (colimit-cocone c)])
      (assert-true (verify-cocone cocone)))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
