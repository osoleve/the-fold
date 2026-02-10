(load "core/lang/module.ss")
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
    ;; Set semantics: product of {1,2} × {a,b} = {(1,a),(1,b),(2,a),(2,b)}
    (let* ([p (binary-product '(1 2) '(a b))]
           [apex (limit-apex p)])
      (assert-true (limit? p))
      (assert-equal 4 (length apex))
      (assert-true (pair? (member '(1 . a) apex)))
      (assert-true (pair? (member '(2 . b) apex)))))

  (define-test "product projections"
    ;; Element-level projections: π_a = car, π_b = cdr
    (let* ([p (binary-product '(1 2) '(x y))]
           [apex (limit-apex p)]
           [cone (limit-cone p)]
           [π-a (cone-project cone 'a)]
           [π-b (cone-project cone 'b)])
      ;; Test on specific element from apex
      (assert-equal 1 (π-a '(1 . x)))
      (assert-equal 'x (π-b '(1 . x)))
      ;; Map over all elements
      (assert-equal '(1 1 2 2) (map π-a apex))
      (assert-equal '(x y x y) (map π-b apex))))

  (define-test "product pairing morphism"
    (let ([pair (product-pair add1 (lambda (x) (* x 2)))])
      (assert-equal (cons 6 10) (pair 5))))

  (define-test "product universal property"
    ;; For any cone over the discrete diagram, factor through the product
    (let* ([p (binary-product '(1 2) '(a b))]
           [other-apex 'z]
           [f (lambda (x) 1)]   ; f : Z → A
           [g (lambda (x) 'a)]  ; g : Z → B
           [other-projs (lambda (obj)
                          (case obj
                            [(a) f]
                            [(b) g]))]
           [other-cone (make-cone other-apex (cone-diagram (limit-cone p)) other-projs)]
           [factor (limit-factor p other-cone)])
      (let ([result (factor 'z)])
        (assert-equal 1 (car result))
        (assert-equal 'a (cdr result)))))

  (define-test "n-ary product"
    ;; n-ary product computes full cartesian product
    (let* ([p (n-ary-product '((1 2) (a b)))]
           [apex (limit-apex p)])
      (assert-true (limit? p))
      (assert-equal 4 (length apex))
      (assert-true (pair? (member '(1 a) apex)))
      (assert-true (pair? (member '(2 b) apex)))))

  (define-test "n-ary product projections"
    (let* ([p (n-ary-product '((1 2) (a b) (x y)))]
           [apex (limit-apex p)]
           [cone (limit-cone p)]
           [π-0 (cone-project cone 'x0)]
           [π-1 (cone-project cone 'x1)]
           [π-2 (cone-project cone 'x2)])
      (assert-equal 8 (length apex))
      (assert-equal 1 (π-0 '(1 a x)))
      (assert-equal 'a (π-1 '(1 a x)))
      (assert-equal 'x (π-2 '(1 a x)))))

  (define-test "composition: equalizer of product morphisms"
    ;; This test verifies fix for fold-zxs5: products and equalizers now compose
    ;; Product of {1,2} × {1,2}, then find equalizer of f(a,b)=(a+b) and g(a,b)=3
    (let* ([prod (binary-product '(1 2) '(1 2))]
           [prod-apex (limit-apex prod)]  ; {(1,1),(1,2),(2,1),(2,2)}
           [f (lambda (p) (+ (car p) (cdr p)))]  ; f(a,b) = a+b
           [g (lambda (p) 3)]                     ; g(a,b) = 3
           ;; Codomain of f and g is integers - use range that includes 3
           [eq (equalizer prod-apex '(2 3 4) f g)]
           [eq-apex (limit-apex eq)])
      ;; Equalizer should be pairs where a+b=3, i.e., {(1,2),(2,1)}
      (assert-equal 2 (length eq-apex))
      (assert-true (pair? (member '(1 . 2) eq-apex)))
      (assert-true (pair? (member '(2 . 1) eq-apex))))))

;;; ============================================================
;;; Section 5: Coproduct Tests
;;; ============================================================

(test-group "coproducts"
  (define-test "binary coproduct construction"
    (let ([c (binary-coproduct '(1 2 3) '(a b))])
      (assert-true (colimit? c))
      ;; Apex should be the disjoint union
      (let ([apex (colimit-apex c)])
        (assert-equal 5 (length apex))
        ;; Check all tagged elements are present
        (assert-true (pair? (member (make-left 1) apex)))
        (assert-true (pair? (member (make-left 2) apex)))
        (assert-true (pair? (member (make-left 3) apex)))
        (assert-true (pair? (member (make-right 'a) apex)))
        (assert-true (pair? (member (make-right 'b) apex))))))

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
    (let* ([c (binary-coproduct '(1 2) '("hi" "bye"))]
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
    ;; f(x) = 2*x, g(x) = x+x, so f=g everywhere
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) (+ x x))]
           [eq (equalizer '(1 2 3 4 5) '(2 4 6 8 10) f g)]
           [apex (limit-apex eq)])
      (assert-equal '(1 2 3 4 5) apex)))

  (define-test "equalizer filters non-equal elements"
    ;; f(x) = x+1, g(x) = 2*x; equal only when x+1=2x, i.e., x=1
    (let* ([f add1]
           [g (lambda (x) (* x 2))]
           [eq (equalizer '(0 1 2 3 4 5) '(0 1 2 3 4 5 6 7 8 9 10) f g)]
           [apex (limit-apex eq)])
      (assert-equal '(1) apex)))

  (define-test "equalizer with no solutions"
    ;; f(x) = x+1, g(x) = x+2, never equal
    (let* ([f add1]
           [g (lambda (x) (+ x 2))]
           [eq (equalizer '(1 2 3) '(2 3 4 5) f g)]
           [apex (limit-apex eq)])
      (assert-equal '() apex)))

  (define-test "equalizer inclusion morphism"
    ;; f = g = add1, so equalizer is all of A
    (let* ([eq (equalizer '(1 2 3 4) '(2 3 4 5) add1 add1)]
           [incl (equalizer-inclusion eq)]
           [apex (limit-apex eq)])
      ;; Inclusion is element-level: maps each e in E to itself in A
      (assert-equal '(1 2 3 4) (map incl apex))
      (assert-equal 1 (incl 1))
      (assert-equal 2 (incl 2)))))

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
      ;; Projections are element-level: apply to each pair in apex
      (assert-equal 3 (length (map p1 apex)))
      (assert-equal 3 (length (map p2 apex)))
      ;; Test specific projections
      (assert-equal 1 (p1 '(1 . 1)))
      (assert-equal 1 (p2 '(1 . 1)))))

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
    (let* ([d (make-product-diagram '(a b) '(1 2))]
           [s (diagram->string d)])
      (assert-true (string? s))
      (assert-true (string-contains? s "discrete-2"))))

  (define-test "limit->string"
    (let* ([p (binary-product '(1) '(2))]
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
    (let* ([p (binary-product '(1 2) '(a b))]
           [cone (limit-cone p)])
      (assert-true (verify-cone cone))))

  (define-test "verify-cone for equalizer"
    (let* ([f add1]
           [g (lambda (x) (* x 2))]
           [eq (equalizer '(0 1 2 3 4 5) '(0 1 2 3 4 5 6 7 8 9 10) f g)]
           [cone (limit-cone eq)])
      ;; Apex is (1) since add1(1)=2=2*1
      (assert-equal '(1) (cone-apex cone))
      (assert-true (verify-cone cone))))

  (define-test "verify-cone for pullback"
    (let* ([f (lambda (x) (* x 2))]
           [g (lambda (x) x)]
           [pb (pullback '(1 2 3) '(2 4 6) 'nat f g)]
           [cone (limit-cone pb)])
      ;; Apex contains pairs where f(a)=g(b), i.e., 2a=b
      (assert-true (verify-cone cone))))

  (define-test "verify-cocone for coproduct"
    (let* ([c (binary-coproduct '(1 2) '(a b))]
           [cocone (colimit-cocone c)])
      (assert-true (verify-cocone cocone))))

  (define-test "verify-cocone for coequalizer"
    (let* ([f (lambda (x) x)]
           [g (lambda (x) (+ x 1))]
           [coeq (coequalizer '(1 2) '(1 2 3) f g)]
           [cocone (colimit-cocone coeq)])
      (assert-true (verify-cocone cocone)))))

;;; ============================================================
;;; Section 13: General Limit Construction Tests
;;; ============================================================

(test-group "general-construction"
  (define-test "limit-from-diagram on empty shape gives terminal"
    (let* ([diagram (make-diagram shape-empty
                                  (lambda (x) (error 'test "no objects"))
                                  (lambda (x) (error 'test "no morphisms")))]
           [lim (limit-from-diagram diagram)])
      (assert-true (limit? lim))
      (assert-equal '() (limit-apex lim))))

  (define-test "limit-from-diagram on discrete-2 gives product"
    (let* ([diagram (make-diagram shape-discrete-2
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(1 2)]
                                      [(b) '(x y)]))
                                  (lambda (m) (error 'test "no morphisms")))]
           [lim (limit-from-diagram diagram)]
           [apex (limit-apex lim)])
      (assert-true (limit? lim))
      (assert-equal 4 (length apex))
      ;; Tuples should be (a-val b-val)
      (assert-true (pair? (member '(1 x) apex)))
      (assert-true (pair? (member '(2 y) apex)))))

  (define-test "limit-from-diagram on parallel-pair gives equalizer"
    ;; f(x) = x+1, g(x) = 2x; equal when x=1
    (let* ([diagram (make-diagram shape-parallel-pair
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(0 1 2 3)]
                                      [(b) '(0 1 2 3 4 5 6)]))
                                  (lambda (m)
                                    (case m
                                      [(f) add1]
                                      [(g) (lambda (x) (* 2 x))])))]
           [lim (limit-from-diagram diagram)]
           [apex (limit-apex lim)])
      (assert-true (limit? lim))
      ;; Equalizer elements are tuples (a b) where f(a)=g(a) and b=f(a)
      ;; Only a=1 satisfies f(1)=2=g(1), so apex has element (1 2)
      (assert-equal 1 (length apex))
      (assert-equal 1 (car (car apex)))))

  (define-test "limit-from-diagram on cospan gives pullback"
    ;; A = {1,2,3}, B = {2,4,6}, C = naturals
    ;; f(a) = 2a, g(b) = b
    ;; Pullback: pairs (a,b) where 2a = b
    (let* ([diagram (make-diagram shape-cospan
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(1 2 3)]
                                      [(b) '(2 4 6)]
                                      [(c) '(2 4 6)]))
                                  (lambda (m)
                                    (case m
                                      [(f) (lambda (x) (* 2 x))]
                                      [(g) (lambda (x) x)])))]
           [lim (limit-from-diagram diagram)]
           [apex (limit-apex lim)])
      (assert-true (limit? lim))
      ;; Should have (1,2,2), (2,4,4), (3,6,6)
      (assert-equal 3 (length apex))))

  (define-test "limit-from-diagram projections work correctly"
    (let* ([diagram (make-diagram shape-discrete-2
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(1 2)]
                                      [(b) '(x y)]))
                                  (lambda (m) (error 'test "no morphisms")))]
           [lim (limit-from-diagram diagram)]
           [cone (limit-cone lim)]
           [π-a (cone-project cone 'a)]
           [π-b (cone-project cone 'b)])
      (assert-equal 1 (π-a '(1 x)))
      (assert-equal 'x (π-b '(1 x)))
      (assert-equal 2 (π-a '(2 y)))
      (assert-equal 'y (π-b '(2 y)))))

  (define-test "limit-from-diagram verifies cone commutativity"
    (let* ([diagram (make-diagram shape-parallel-pair
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(1 2 3)]
                                      [(b) '(1 2 3 4 5 6)]))
                                  (lambda (m)
                                    (case m
                                      [(f) (lambda (x) (* x 2))]
                                      [(g) (lambda (x) (* x 2))])))]
           [lim (limit-from-diagram diagram)]
           [cone (limit-cone lim)])
      ;; When f=g, equalizer is all of A (as tuples)
      (assert-true (verify-cone cone))))

  (define-test "colimit-from-diagram on empty shape gives initial"
    (let* ([diagram (make-diagram shape-empty
                                  (lambda (x) (error 'test "no objects"))
                                  (lambda (x) (error 'test "no morphisms")))]
           [colim (colimit-from-diagram diagram)])
      (assert-true (colimit? colim))
      (assert-equal '() (colimit-apex colim))))

  (define-test "colimit-from-diagram on discrete-2 gives coproduct"
    (let* ([diagram (make-diagram shape-discrete-2
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(1 2)]
                                      [(b) '(x y)]))
                                  (lambda (m) (error 'test "no morphisms")))]
           [colim (colimit-from-diagram diagram)]
           [apex (colimit-apex colim)])
      (assert-true (colimit? colim))
      ;; Coproduct has 4 elements, tagged
      (assert-equal 4 (length apex))))

  (define-test "colimit-from-diagram on span gives pushout"
    ;; A = {a1,a2}, B = {b1,b2}, C = {c}
    ;; f: c → a1, g: c → b1
    ;; Pushout: (A + B) / (a1 ~ b1)
    (let* ([diagram (make-diagram shape-span
                                  (lambda (obj)
                                    (case obj
                                      [(a) '(a1 a2)]
                                      [(b) '(b1 b2)]
                                      [(c) '(c)]))
                                  (lambda (m)
                                    (case m
                                      [(f) (lambda (x) 'a1)]
                                      [(g) (lambda (x) 'b1)])))]
           [colim (colimit-from-diagram diagram)]
           [apex (colimit-apex colim)]
           [ι-a (cocone-inject (colimit-cocone colim) 'a)]
           [ι-b (cocone-inject (colimit-cocone colim) 'b)])
      (assert-true (colimit? colim))
      ;; a1 and b1 should be in the same equivalence class
      (assert-equal (ι-a 'a1) (ι-b 'b1))
      ;; a2 and b2 should be in different classes
      (assert-false (equal? (ι-a 'a2) (ι-b 'b2)))))

  (define-test "n-ary-coproduct construction"
    (let* ([colim (n-ary-coproduct '((1 2) (a b) (x)) '(num alpha sym))]
           [apex (colimit-apex colim)])
      (assert-true (colimit? colim))
      (assert-equal 5 (length apex))
      (assert-true (pair? (member '(num . 1) apex)))
      (assert-true (pair? (member '(alpha . a) apex)))
      (assert-true (pair? (member '(sym . x) apex))))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(run-all-tests)
