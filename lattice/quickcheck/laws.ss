;;; lattice/quickcheck/laws.ss — Algebraic law checking via property-based testing
;;; @module qc-laws
;;; @requires prelude qc-generators qc-shrink quickcheck

(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)
(require 'qc-generators)
(require 'qc-shrink)
(require 'quickcheck)

(doc 'module 'qc-laws)
(doc 'description "Law-checking combinators for algebraic structures. Each takes generators + operations + equality predicate, returns QC results.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================================
;;; Helpers
;;; ============================================================================

(define *default-law-tests* 200)

(define (law-opts . extra)
  (doc 'description "Merge extra opts with default test count")
  (append extra (list 'tests *default-law-tests*)))

;;; ============================================================================
;;; Semigroup laws: associativity
;;; ============================================================================

(doc 'section 'semigroup)

(define (check-semigroup-laws gen op =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) (-> a a Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: (op (op x y) z) =? (op x (op y z))")
  (let ([gen3 (gen-bind gen (lambda (x)
                (gen-bind gen (lambda (y)
                  (gen-map (lambda (z) (list x y z)) gen)))))])
    (list
      (cons 'associativity
            (check-property gen3
              (lambda (xyz)
                (let ([x (car xyz)] [y (cadr xyz)] [z (caddr xyz)])
                  (=? (op (op x y) z)
                      (op x (op y z)))))
              'tests *default-law-tests*)))))

;;; ============================================================================
;;; Monoid laws: identity + associativity
;;; ============================================================================

(doc 'section 'monoid)

(define (check-monoid-laws gen op identity =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) a (-> a a Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: left identity, right identity, associativity")
  (append
    (list
      (cons 'left-identity
            (check-property gen
              (lambda (x) (=? (op identity x) x))
              'tests *default-law-tests*))
      (cons 'right-identity
            (check-property gen
              (lambda (x) (=? (op x identity) x))
              'tests *default-law-tests*)))
    (check-semigroup-laws gen op =?)))

;;; ============================================================================
;;; Group laws: monoid + inverse
;;; ============================================================================

(doc 'section 'group)

(define (check-group-laws gen op identity inv =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) a (-> a a) (-> a a Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: monoid laws + left/right inverse")
  (append
    (check-monoid-laws gen op identity =?)
    (list
      (cons 'left-inverse
            (check-property gen
              (lambda (x) (=? (op (inv x) x) identity))
              'tests *default-law-tests*))
      (cons 'right-inverse
            (check-property gen
              (lambda (x) (=? (op x (inv x)) identity))
              'tests *default-law-tests*)))))

;;; ============================================================================
;;; Abelian group: group + commutativity
;;; ============================================================================

(doc 'section 'abelian-group)

(define (check-abelian-group-laws gen op identity inv =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) a (-> a a) (-> a a Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: group laws + commutativity")
  (let ([gen2 (gen-bind gen (lambda (x) (gen-map (lambda (y) (cons x y)) gen)))])
    (append
      (check-group-laws gen op identity inv =?)
      (list
        (cons 'commutativity
              (check-property gen2
                (lambda (xy)
                  (=? (op (car xy) (cdr xy))
                      (op (cdr xy) (car xy))))
                'tests *default-law-tests*))))))

;;; ============================================================================
;;; Ring laws: abelian group (add) + monoid (mul) + distributivity
;;; ============================================================================

(doc 'section 'ring)

(define (check-ring-laws gen add zero neg mul one =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) a (-> a a) (-> a a a) a (-> a a Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: additive abelian group, multiplicative monoid, distributivity")
  (let ([gen3 (gen-bind gen (lambda (x)
                (gen-bind gen (lambda (y)
                  (gen-map (lambda (z) (list x y z)) gen)))))])
    (append
      ;; Tag additive laws
      (map (lambda (p) (cons (string->symbol
                               (string-append "add:" (symbol->string (car p))))
                             (cdr p)))
           (check-abelian-group-laws gen add zero neg =?))
      ;; Tag multiplicative laws
      (map (lambda (p) (cons (string->symbol
                               (string-append "mul:" (symbol->string (car p))))
                             (cdr p)))
           (check-monoid-laws gen mul one =?))
      ;; Distributivity
      (list
        (cons 'left-distributivity
              (check-property gen3
                (lambda (xyz)
                  (let ([x (car xyz)] [y (cadr xyz)] [z (caddr xyz)])
                    (=? (mul x (add y z))
                        (add (mul x y) (mul x z)))))
                'tests *default-law-tests*))
        (cons 'right-distributivity
              (check-property gen3
                (lambda (xyz)
                  (let ([x (car xyz)] [y (cadr xyz)] [z (caddr xyz)])
                    (=? (mul (add x y) z)
                        (add (mul x z) (mul y z)))))
                'tests *default-law-tests*))))))

;;; ============================================================================
;;; Vector space laws: abelian group + scalar operations
;;; ============================================================================

(doc 'section 'vector-space)

(define (check-vector-space-laws gen-vec gen-scalar add zero neg scale =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen v) (Gen s) (-> v v v) v (-> v v) (-> v s v) (-> v v Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: additive abelian group + scalar compatibility, distributivity, identity")
  (let ([gen-vs (gen-bind gen-vec (lambda (v) (gen-map (lambda (s) (cons v s)) gen-scalar)))]
        [gen-vvs (gen-bind gen-vec (lambda (u)
                   (gen-bind gen-vec (lambda (v)
                     (gen-map (lambda (s) (list u v s)) gen-scalar)))))]
        [gen-vss (gen-bind gen-vec (lambda (v)
                   (gen-bind gen-scalar (lambda (a)
                     (gen-map (lambda (b) (list v a b)) gen-scalar)))))])
    (append
      ;; Additive abelian group
      (check-abelian-group-laws gen-vec add zero neg =?)
      (list
        ;; Scalar identity: 1*v = v
        (cons 'scalar-identity
              (check-property gen-vec
                (lambda (v) (=? (scale v 1) v))
                'tests *default-law-tests*))
        ;; Scalar compatibility: a(bv) = (ab)v
        (cons 'scalar-compatibility
              (check-property gen-vss
                (lambda (vas)
                  (let ([v (car vas)] [a (cadr vas)] [b (caddr vas)])
                    (=? (scale (scale v b) a)
                        (scale v (* a b)))))
                'tests *default-law-tests*))
        ;; Scalar distributivity over vector addition: a(u+v) = au + av
        (cons 'scalar-distributes-over-vec-add
              (check-property gen-vvs
                (lambda (uvs)
                  (let ([u (car uvs)] [v (cadr uvs)] [s (caddr uvs)])
                    (=? (scale (add u v) s)
                        (add (scale u s) (scale v s)))))
                'tests *default-law-tests*))
        ;; Scalar sum distributivity: (a+b)v = av + bv
        (cons 'scalar-sum-distributes
              (check-property gen-vss
                (lambda (vas)
                  (let ([v (car vas)] [a (cadr vas)] [b (caddr vas)])
                    (=? (scale v (+ a b))
                        (add (scale v a) (scale v b)))))
                'tests *default-law-tests*))))))

;;; ============================================================================
;;; Inner product laws
;;; ============================================================================

(doc 'section 'inner-product)

(define (check-inner-product-laws gen-vec gen-scalar dot norm add scale zero =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen v) (Gen s) (-> v v s) (-> v s) (-> v v v) (-> v s v) v (-> s s Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: symmetry, linearity, positive-definiteness, norm consistency")
  (let ([gen2 (gen-bind gen-vec (lambda (u) (gen-map (lambda (v) (cons u v)) gen-vec)))]
        [gen-uvs (gen-bind gen-vec (lambda (u)
                   (gen-bind gen-vec (lambda (v)
                     (gen-map (lambda (s) (list u v s)) gen-scalar)))))])
    (list
      ;; Symmetry: <u,v> = <v,u>
      (cons 'symmetry
            (check-property gen2
              (lambda (uv) (=? (dot (car uv) (cdr uv))
                               (dot (cdr uv) (car uv))))
              'tests *default-law-tests*))
      ;; Linearity in first arg: <au + bw, v> ≈ a<u,v> + b<w,v>
      ;; (simplified: <sv, w> = s<v,w>)
      (cons 'scalar-linearity
            (check-property gen-uvs
              (lambda (uvs)
                (let ([u (car uvs)] [v (cadr uvs)] [s (caddr uvs)])
                  (=? (dot (scale u s) v)
                      (* s (dot u v)))))
              'tests *default-law-tests*))
      ;; Additivity: <u+v, w> = <u,w> + <v,w>
      (cons 'additivity
            (check-property (gen-bind gen-vec (lambda (u)
                              (gen-bind gen-vec (lambda (v)
                                (gen-map (lambda (w) (list u v w)) gen-vec)))))
              (lambda (uvw)
                (let ([u (car uvw)] [v (cadr uvw)] [w (caddr uvw)])
                  (=? (dot (add u v) w)
                      (+ (dot u w) (dot v w)))))
              'tests *default-law-tests*))
      ;; Positive definiteness: <v,v> >= 0
      (cons 'positive-definiteness
            (check-property gen-vec
              (lambda (v) (>= (dot v v) 0))
              'tests *default-law-tests*))
      ;; Norm consistency: ||v|| = sqrt(<v,v>)
      (cons 'norm-consistency
            (check-property gen-vec
              (lambda (v) (=? (norm v) (sqrt (dot v v))))
              'tests *default-law-tests*)))))

;;; ============================================================================
;;; Involution laws (e.g., transpose, reverse, negate-negate)
;;; ============================================================================

(doc 'section 'involution)

(define (check-involution-law gen f =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a) (-> a a Boolean) QCResult))
  (doc 'description "Check: f(f(x)) =? x")
  (check-property gen
    (lambda (x) (=? (f (f x)) x))
    'tests *default-law-tests*))

;;; ============================================================================
;;; Anti-commutativity (e.g., cross product)
;;; ============================================================================

(doc 'section 'anti-commutativity)

(define (check-anti-commutativity-law gen op neg =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen a) (-> a a a) (-> a a) (-> a a Boolean) QCResult))
  (doc 'description "Check: op(x,y) =? neg(op(y,x))")
  (let ([gen2 (gen-bind gen (lambda (x) (gen-map (lambda (y) (cons x y)) gen)))])
    (check-property gen2
      (lambda (xy)
        (=? (op (car xy) (cdr xy))
            (neg (op (cdr xy) (car xy)))))
      'tests *default-law-tests*)))

;;; ============================================================================
;;; Transpose-of-product law
;;; ============================================================================

(doc 'section 'transpose)

(define (check-transpose-product-law gen-mat mul transpose =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen Matrix) (-> Matrix Matrix Matrix) (-> Matrix Matrix) (-> Matrix Matrix Boolean) QCResult))
  (doc 'description "Check: transpose(A*B) =? transpose(B) * transpose(A)")
  (let ([gen2 (gen-bind gen-mat (lambda (a) (gen-map (lambda (b) (cons a b)) gen-mat)))])
    (check-property gen2
      (lambda (ab)
        (=? (transpose (mul (car ab) (cdr ab)))
            (mul (transpose (cdr ab)) (transpose (car ab)))))
      'tests *default-law-tests*)))

;;; ============================================================================
;;; Functor laws
;;; ============================================================================

(doc 'section 'functor)

(define (check-functor-laws gen fmap =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen (f a)) (-> (-> a b) (f a) (f b)) (-> (f a) (f a) Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: fmap id = id, fmap (f . g) = fmap f . fmap g")
  (list
    ;; Identity: fmap id = id
    (cons 'functor-identity
          (check-property gen
            (lambda (fa) (=? (fmap (lambda (x) x) fa) fa))
            'tests *default-law-tests*))
    ;; Composition: fmap (f . g) = fmap f . fmap g
    ;; Use concrete functions: f = add1, g = double
    (cons 'functor-composition
          (check-property gen
            (lambda (fa)
              (let ([f (lambda (x) (+ x 1))]
                    [g (lambda (x) (* x 2))])
                (=? (fmap (lambda (x) (f (g x))) fa)
                    (fmap f (fmap g fa)))))
            'tests *default-law-tests*))))

;;; ============================================================================
;;; Monad laws
;;; ============================================================================

(doc 'section 'monad)

(define (check-monad-laws gen-a bind pure gen-val =?)
  (doc 'export #t)
  (doc 'type '(-> (Gen (m a)) (-> (m a) (-> a (m b)) (m b)) (-> a (m a)) (Gen a) (-> (m a) (m a) Boolean) (List (Pair Symbol QCResult))))
  (doc 'description "Check: left identity, right identity, associativity")
  (let ([f (lambda (x) (pure (+ x 1)))]
        [g (lambda (x) (pure (* x 2)))])
    (list
      ;; Left identity: pure a >>= f = f a
      (cons 'left-identity
            (check-property gen-val
              (lambda (a) (=? (bind (pure a) f) (f a)))
              'tests *default-law-tests*))
      ;; Right identity: m >>= pure = m
      (cons 'right-identity
            (check-property gen-a
              (lambda (m) (=? (bind m pure) m))
              'tests *default-law-tests*))
      ;; Associativity: (m >>= f) >>= g = m >>= (\x -> f x >>= g)
      (cons 'associativity
            (check-property gen-a
              (lambda (m)
                (=? (bind (bind m f) g)
                    (bind m (lambda (x) (bind (f x) g)))))
              'tests *default-law-tests*)))))

;;; ============================================================================
;;; Result reporting
;;; ============================================================================

(doc 'section 'reporting)

(define (all-laws-hold? results)
  (doc 'export #t)
  (doc 'type '(-> (List (Pair Symbol QCResult)) Boolean))
  (doc 'description "Check if all law results are successes")
  (andmap (lambda (r) (qc-success? (cdr r))) results))

(define (report-law-results name results)
  (doc 'export #t)
  (doc 'type '(-> String (List (Pair Symbol QCResult)) Unit))
  (doc 'description "Print law check results with pass/fail markers")
  (for-each
    (lambda (r)
      (let ([law-name (car r)]
            [result (cdr r)])
        (if (qc-success? result)
            (begin (display "      ") (display law-name) (display " ✓") (newline))
            (begin (display "      ") (display law-name) (display " ✗")
                   (when (qc-failure? result)
                     (display " — counterexample: ")
                     (write (qc-failure-shrunk result)))
                   (newline)))))
    results))

(define (assert-laws name results)
  (doc 'export #t)
  (doc 'type '(-> String (List (Pair Symbol QCResult)) Unit))
  (doc 'description "Assert all laws hold, with detailed failure reporting. For use inside define-test.")
  (report-law-results name results)
  (assert-true (all-laws-hold? results)))
