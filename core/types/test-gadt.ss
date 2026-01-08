;;; core/types/test-gadt.ss — Tests for GADT Support
;;;
;;; Tests for GADT predicates, accessors, and type refinement.

(load "core/base/prelude.ss")
(load "core/types/types.ss")
(load "core/types/dep-types.ss")
(load "core/types/gadt.ss")

;;; ============================================================
;;; Test Framework
;;; ============================================================

(define *test-count* 0)
(define *pass-count* 0)
(define *fail-count* 0)

(define (test name expected actual)
  (set! *test-count* (+ *test-count* 1))
  (if (equal? expected actual)
      (begin
       (set! *pass-count* (+ *pass-count* 1))
       (display "  ") (display *pass-count*) (display ". ")
       (display name)
       (newline))
      (begin
       (set! *fail-count* (+ *fail-count* 1))
       (display "FAIL ")
       (display name)
       (newline)
       (display "  Expected: ")
       (write expected)
       (newline)
       (display "  Actual:   ")
       (write actual)
       (newline))))

(define (test-true name actual)
  (test name #t actual))

(define (test-false name actual)
  (test name #f actual))

;;; ============================================================
;;; Example GADTs for Testing
;;; ============================================================

;; Simple expression GADT
(define expr-gadt
  '(gadt (Expr a)
    [Lit  : (-> Int (Expr Int))]
    [Add  : (-> (Expr Int) (Expr Int) (Expr Int))]
    [Eq   : (-> (Expr Int) (Expr Int) (Expr Bool))]
    [If   : (∀ (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))]))

;; Length-indexed vector GADT
(define vec-gadt
  '(gadt (Vec (n : Nat) a)
    [VNil  : (Vec 0 a)]
    [VCons : (∀ (m) (-> a (Vec m a) (Vec (succ m) a)))]))

(define (run-tests)
  (display "
=== GADT Test Suite ===

")
  
  ;; --------------------------------------------------------
  ;; GADT Predicates
  ;; --------------------------------------------------------
  (display "--- GADT Predicates ---
")
  
  (test-true "gadt-type? on GADT"
             (gadt-type? expr-gadt))
  
  (test-false "gadt-type? on data"
              (gadt-type? '(data (Maybe a) [Nothing : (Maybe a)] [Just : (-> a (Maybe a))])))
  
  (test-true "gadt-well-formed? on Expr"
             (gadt-well-formed? expr-gadt))
  
  (test-true "gadt-well-formed? on Vec"
             (gadt-well-formed? vec-gadt))
  
  (test-false "gadt-well-formed? on malformed"
              (gadt-well-formed? '(gadt)))
  
  (test-true "gadt-ctor-decl? on valid"
             (gadt-ctor-decl? '[Lit : (-> Int (Expr Int))]))
  
  (test-false "gadt-ctor-decl? on invalid"
              (gadt-ctor-decl? '(Lit (-> Int (Expr Int)))))
  
  (test-true "gadt-index-decl? on symbol"
             (gadt-index-decl? 'a))
  
  (test-true "gadt-index-decl? on kinded"
             (gadt-index-decl? '(n : Nat)))
  
  (test-false "gadt-index-decl? on malformed"
              (gadt-index-decl? '(n Nat)))
  
  ;; --------------------------------------------------------
  ;; GADT Accessors
  ;; --------------------------------------------------------
  (display "
--- GADT Accessors ---
")
  
  (test "gadt-name on Expr"
        'Expr
        (gadt-name expr-gadt))
  
  (test "gadt-name on Vec"
        'Vec
        (gadt-name vec-gadt))
  
  (test "gadt-index-vars on Expr"
        '(a)
        (gadt-index-vars expr-gadt))
  
  (test "gadt-index-vars on Vec"
        '(n a)
        (gadt-index-vars vec-gadt))
  
  (test "gadt-index-kinds on Expr"
        '((a . Type))
        (gadt-index-kinds expr-gadt))
  
  (test "gadt-index-kinds on Vec"
        '((n . Nat) (a . Type))
        (gadt-index-kinds vec-gadt))
  
  (test "gadt-ctor-names on Expr"
        '(Lit Add Eq If)
        (gadt-ctor-names expr-gadt))
  
  (test "gadt-ctor-names on Vec"
        '(VNil VCons)
        (gadt-ctor-names vec-gadt))
  
  (test "gadt-ctor-type Lit"
        '(-> Int (Expr Int))
        (gadt-ctor-type expr-gadt 'Lit))
  
  (test "gadt-ctor-type VNil"
        '(Vec 0 a)
        (gadt-ctor-type vec-gadt 'VNil))
  
  (test "gadt-ctor-type unknown"
        #f
        (gadt-ctor-type expr-gadt 'Unknown))
  
  ;; --------------------------------------------------------
  ;; Constructor Type Analysis
  ;; --------------------------------------------------------
  (display "
--- Constructor Type Analysis ---
")
  
  (test "gadt-ctor-return-type on simple arrow"
        '(Expr Int)
        (gadt-ctor-return-type '(-> Int (Expr Int))))
  
  (test "gadt-ctor-return-type on nested arrow"
        '(Expr Int)
        (gadt-ctor-return-type '(-> (Expr Int) (Expr Int) (Expr Int))))
  
  (test "gadt-ctor-return-type on forall"
        '(Expr b)
        (gadt-ctor-return-type '(∀ (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))))
  
  (test "gadt-ctor-return-type on nullary"
        '(Vec 0 a)
        (gadt-ctor-return-type '(Vec 0 a)))
  
  (test "gadt-ctor-param-types on arrow"
        '(Int)
        (gadt-ctor-param-types '(-> Int (Expr Int))))
  
  (test "gadt-ctor-param-types on multi-arg"
        '((Expr Int) (Expr Int))
        (gadt-ctor-param-types '(-> (Expr Int) (Expr Int) (Expr Int))))
  
  (test "gadt-ctor-param-types on nullary"
        '()
        (gadt-ctor-param-types '(Vec 0 a)))
  
  (test "gadt-ctor-return-indices Lit"
        '(Int)
        (gadt-ctor-return-indices '(-> Int (Expr Int)) 'Expr))
  
  (test "gadt-ctor-return-indices VNil"
        '(0 a)
        (gadt-ctor-return-indices '(Vec 0 a) 'Vec))
  
  (test "gadt-ctor-forall-vars If"
        '(b)
        (gadt-ctor-forall-vars '(∀ (b) (-> (Expr Bool) (Expr b) (Expr b) (Expr b)))))
  
  (test "gadt-ctor-forall-vars Lit"
        '()
        (gadt-ctor-forall-vars '(-> Int (Expr Int))))
  
  ;; --------------------------------------------------------
  ;; Type Refinement
  ;; --------------------------------------------------------
  (display "
--- Type Refinement ---
")
  
  (test "extract-type-equations simple"
        '((a . Int))
        (extract-type-equations '(a) '(Int) '(a)))
  
  (test "extract-type-equations multi-index"
        '((n . 0) (a . Int))
        (extract-type-equations '(n a) '(0 Int) '(n a)))
  
  (test "extract-type-equations partial match"
        '((a . Bool))
        (extract-type-equations '(a) '(Bool) '(a b)))
  
  (test "extract-type-equations no vars"
        '()
        (extract-type-equations '(Int) '(Int) '()))
  
  (test "extract-type-equations concrete match"
        '()
        (extract-type-equations '(Int) '(Int) '(a)))
  
  (test "apply-refinements simple"
        '(-> Int Int)
        (apply-refinements '(-> a a) '((a . Int))))
  
  (test "apply-refinements multiple"
        '(-> Int Bool)
        (apply-refinements '(-> a b) '((a . Int) (b . Bool))))
  
  (test "apply-refinements nested"
        '(List Int)
        (apply-refinements '(List a) '((a . Int))))
  
  ;; --------------------------------------------------------
  ;; GADT Case Expressions
  ;; --------------------------------------------------------
  (display "
--- GADT Case Expressions ---
")
  
  (let ([example-case '(gadt-case e
                        ((Lit n) n)
                        ((Add x y) (+ (eval x) (eval y)))
                        ((Eq x y) (= (eval x) (eval y))))])
       (test-true "gadt-case-expr? on gadt-case"
                  (gadt-case-expr? example-case))
       (test-false "gadt-case-expr? on case"
                   (gadt-case-expr? '(case e ((Lit n) n))))
       (test "gadt-case-scrutinee"
             'e
             (gadt-case-scrutinee example-case))
       (test "gadt-case-clauses count"
             3
             (length (gadt-case-clauses example-case)))
       (test "gadt-clause-pattern"
             '(Lit n)
             (gadt-clause-pattern (car (gadt-case-clauses example-case))))
       (test "gadt-clause-body"
             'n
             (gadt-clause-body (car (gadt-case-clauses example-case)))))
  
  (test "gadt-pattern-ctor"
        'Lit
        (gadt-pattern-ctor '(Lit n)))
  
  (test "gadt-pattern-vars"
        '(n)
        (gadt-pattern-vars '(Lit n)))
  
  (test "gadt-pattern-vars multi"
        '(x y)
        (gadt-pattern-vars '(Add x y)))
  
  ;; --------------------------------------------------------
  ;; GADT Registry
  ;; --------------------------------------------------------
  (display "
--- GADT Registry ---
")
  
  (let ([registry (make-gadt-registry (list expr-gadt vec-gadt))])
       (test "registry lookup Expr"
             expr-gadt
             (gadt-registry-lookup registry 'Expr))
       (test "registry lookup Vec"
             vec-gadt
             (gadt-registry-lookup registry 'Vec))
       (test "registry lookup unknown"
             #f
             (gadt-registry-lookup registry 'Unknown))
       (let ([new-gadt '(gadt (Maybe a) [Nothing : (Maybe a)] [Just : (-> a (Maybe a))])])
            (let ([new-registry (gadt-registry-add registry new-gadt)])
                 (test "registry add"
                       new-gadt
                       (gadt-registry-lookup new-registry 'Maybe)))))
  
  ;; --------------------------------------------------------
  ;; GADT Wellformedness
  ;; --------------------------------------------------------
  (display "
--- GADT Wellformedness ---
")
  
  (test-true "gadt-ctor-returns-gadt? Lit"
             (gadt-ctor-returns-gadt? '(-> Int (Expr Int)) 'Expr))
  
  (test-true "gadt-ctor-returns-gadt? VNil"
             (gadt-ctor-returns-gadt? '(Vec 0 a) 'Vec))
  
  (test-false "gadt-ctor-returns-gadt? wrong type"
              (gadt-ctor-returns-gadt? '(-> Int Int) 'Expr))
  
  (test-true "gadt-all-ctors-return-gadt? Expr"
             (gadt-all-ctors-return-gadt? expr-gadt))
  
  (test-true "gadt-all-ctors-return-gadt? Vec"
             (gadt-all-ctors-return-gadt? vec-gadt))
  
  ;; --------------------------------------------------------
  ;; GADT Type Construction
  ;; --------------------------------------------------------
  (display "
--- GADT Type Construction ---
")
  
  (test "t-gadt simple"
        '(gadt (Maybe a) [Nothing : (Maybe a)] [Just : (-> a (Maybe a))])
        (t-gadt 'Maybe '((a . Type)) '((Nothing . (Maybe a)) (Just . (-> a (Maybe a))))))
  
  (test "t-gadt with explicit kind"
        '(gadt (Vec (n : Nat) a) [VNil : (Vec 0 a)])
        (t-gadt 'Vec '((n . Nat) (a . Type)) '((VNil . (Vec 0 a)))))
  
  (test "gadt-make-applied with indices"
        '(Expr Int)
        (gadt-make-applied 'Expr '(Int)))
  
  (test "gadt-make-applied no indices"
        'Nat
        (gadt-make-applied 'Nat '()))
  
  (test "gadt-make-applied multi-index"
        '(Vec 3 Int)
        (gadt-make-applied 'Vec '(3 Int)))
  
  ;; --------------------------------------------------------
  ;; Summary
  ;; --------------------------------------------------------
  (display "
=== Summary ===
")
  (display "Total: ") (display *test-count*) (newline)
  (display "Passed: ") (display *pass-count*) (newline)
  (display "Failed: ") (display *fail-count*) (newline)
  
  (if (= *fail-count* 0)
      (display "
All tests passed!
")
      (begin
       (display "
SOME TESTS FAILED!
")
       (exit 1))))

;; Run tests
(run-tests)
