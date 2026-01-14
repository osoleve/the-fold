;;; core/types/test-type-families.ss — Tests for Associated Type Families
;;;
;;; Run from project root: scheme --script core/types/test-type-families.ss

(load "core/types/type-families.ss")

(define *tests-passed* 0)
(define *tests-failed* 0)

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (begin
       (set! *tests-passed* (+ *tests-passed* 1))
       (display "✓"))
      (begin
       (set! *tests-failed* (+ *tests-failed* 1))
       (display "✗")
       (newline)
       (display "    expected: ")
       (write expected)
       (newline)
       (display "    got: ")
       (write actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

;;; ====
;;; Type Family Declaration Parsing
;;; ====
(test-section "Type Family Declaration Parsing")

(test "type-family-decl? basic" #t (type-family-decl? '(type Elem c)))
(test "type-family-decl? with kind" #t (type-family-decl? '(type Rep f : Type)))
(test "type-family-decl? instance is not decl" #f (type-family-decl? '(type Elem (List a) = a)))

(let ([tf (parse-type-family-decl '(type Elem c))])
     (test "parse basic: name" 'Elem (type-family-name tf))
     (test "parse basic: params" '(c) (type-family-params tf))
     (test "parse basic: kind defaults to *" K* (type-family-kind tf)))

(let ([tf (parse-type-family-decl '(type Rep f : Type))])
     (test "parse with kind: name" 'Rep (type-family-name tf))
     (test "parse with kind: params" '(f) (type-family-params tf))
     (test "parse with kind: kind" 'Type (type-family-kind tf)))

(let ([tf (parse-type-family-decl '(type Key m v : Type))])
     (test "parse multi-param: name" 'Key (type-family-name tf))
     (test "parse multi-param: params" '(m v) (type-family-params tf)))

;;; ====
;;; Type Family Instance Parsing
;;; ====
(test-section "Type Family Instance Parsing")

(test "type-family-instance-decl? basic" #t (type-family-instance-decl? '(type Elem (List a) = a)))
(test "type-family-instance-decl? decl is not instance" #f (type-family-instance-decl? '(type Elem c)))

(let ([tfi (parse-type-family-instance '(type Elem (List a) = a))])
     (test "parse instance: family-name" 'Elem (type-family-instance-family-name tfi))
     (test "parse instance: pattern" '(List a) (type-family-instance-pattern tfi))
     (test "parse instance: rhs" 'a (type-family-instance-rhs tfi)))

(let ([tfi (parse-type-family-instance '(type Rep Identity = Unit))])
     (test "parse simple instance: pattern" 'Identity (type-family-instance-pattern tfi))
     (test "parse simple instance: rhs" 'Unit (type-family-instance-rhs tfi)))

;;; ====
;;; Type Family Registry
;;; ====
(test-section "Type Family Registry")

(let* ([r (empty-tf-registry)]
       [tf (make-type-family 'Test '(a) K*)]
       [r (tf-registry-add-decl r 'Test 'TestClass tf)])
      (test "lookup decl: found" #t (type-family? (tf-registry-lookup-decl r 'Test)))
      (test "lookup decl: not found" #f (tf-registry-lookup-decl r 'Unknown)))

(let* ([r (empty-tf-registry)]
       [tfi (make-type-family-instance 'Elem '(List a) 'a)]
       [r (tf-registry-add-instance r 'Elem '(List a) tfi)])
      (test "lookup instance: found" #t
            (type-family-instance? (tf-registry-lookup-instance r 'Elem '(List Int))))
      (test "lookup instance: not found" #f
            (tf-registry-lookup-instance r 'Elem '(Vector Int))))

;;; ====
;;; Type Pattern Matching
;;; ====
(test-section "Type Pattern Matching")

;; Match (List a) against (List Int)
(let ([subst (match-type-pattern '(List a) '(List Int))])
     (test "match (List a) vs (List Int): success" #t (pair? subst))
     (test "match (List a) vs (List Int): binding" 'Int (cdr (assq 'a subst))))

;; Match Identity against Identity
(let ([subst (match-type-pattern 'Identity 'Identity)])
     (test "match Identity vs Identity: success" '() subst))

;; Match (List a) against (Vector Int) - should fail
(test "match (List a) vs (Vector Int): failure" #f
      (match-type-pattern '(List a) '(Vector Int)))

;; Match (-> a b) against (-> Int Bool)
(let ([subst (match-type-pattern '(-> a b) '(-> Int Bool))])
     (test "match (-> a b) vs (-> Int Bool): success" #t (pair? subst))
     (test "match: a = Int" 'Int (cdr (assq 'a subst)))
     (test "match: b = Bool" 'Bool (cdr (assq 'b subst))))

;; Match with same variable used twice
(let ([subst (match-type-pattern '(-> a a) '(-> Int Int))])
     (test "match (-> a a) vs (-> Int Int): success" #t (pair? subst)))

(test "match (-> a a) vs (-> Int Bool): failure" #f
      (match-type-pattern '(-> a a) '(-> Int Bool)))

;;; ====
;;; Type Family Reduction
;;; ====
(test-section "Type Family Reduction")

;; Reduce (Elem (List Int)) → Int
(test "reduce Elem (List Int)" 'Int
      (reduce-std '(Elem (List Int))))

;; Reduce (Elem (List (List Bool))) → (List Bool)
(test "reduce Elem (List (List Bool))" '(List Bool)
      (reduce-std '(Elem (List (List Bool)))))

;; Reduce (Elem (Vector String)) → String
(test "reduce Elem (Vector String)" 'String
      (reduce-std '(Elem (Vector String))))

;; Reduce (Rep Identity) → Unit
(test "reduce Rep Identity" 'Unit
      (reduce-std '(Rep Identity)))

;; Non-reducible type family application
(test "unknown instance stays unreduced" '(Elem (Set Int))
      (reduce-std '(Elem (Set Int))))

;; Reduce inside compound type
(test "reduce inside function type"
      '(-> Int Int)
      (reduce-std '(-> (Elem (List Int)) (Elem (List Int)))))

;; Reduce nested type family (if instances exist)
(test "nested type family"
      '(List Int)
      (reduce-std '(List (Elem (List Int)))))

;;; ====
;;; Type Application Construction
;;; ====
(test-section "Type Family Application")

(test "tf-apply Elem" '(Elem (List Int)) (tf-apply 'Elem '(List Int)))
(test "tf-apply Rep" '(Rep Identity) (tf-apply 'Rep 'Identity))

;;; ====
;;; Standard Type Families
;;; ====
(test-section "Standard Type Families")

(test "TF-Elem exists" #t (type-family? TF-Elem))
(test "TF-Elem name" 'Elem (type-family-name TF-Elem))

(test "TF-Rep exists" #t (type-family? TF-Rep))
(test "TF-Rep name" 'Rep (type-family-name TF-Rep))

(test "TF-Key exists" #t (type-family? TF-Key))
(test "TF-Val exists" #t (type-family? TF-Val))
(test "TF-Inner exists" #t (type-family? TF-Inner))

;;; ====
;;; Standard Instances
;;; ====
(test-section "Standard Instances")

(test "TFI-Elem-List exists" #t (type-family-instance? TFI-Elem-List))
(test "TFI-Elem-List family" 'Elem (type-family-instance-family-name TFI-Elem-List))
(test "TFI-Elem-List pattern" '(List a) (type-family-instance-pattern TFI-Elem-List))
(test "TFI-Elem-List rhs" 'a (type-family-instance-rhs TFI-Elem-List))

(test "TFI-Rep-Identity rhs" 'Unit (type-family-instance-rhs TFI-Rep-Identity))

;;; ====
;;; Type Classes with Associated Types
;;; ====
(test-section "Type Classes with Associated Types")

(test "TC-Collection-with-Elem exists" #t
      (typeclass-with-families? TC-Collection-with-Elem))
(test "TC-Collection-with-Elem name" 'Collection
      (typeclass-with-families-name TC-Collection-with-Elem))
(test "TC-Collection-with-Elem has families" #t
      (> (length (typeclass-with-families-type-families TC-Collection-with-Elem)) 0))
(test "TC-Collection-with-Elem has insert" #t
      (pair? (assq 'insert (typeclass-with-families-methods TC-Collection-with-Elem))))

(test "TC-Representable-with-Rep exists" #t
      (typeclass-with-families? TC-Representable-with-Rep))
(test "TC-Representable-with-Rep has tabulate" #t
      (pair? (assq 'tabulate (typeclass-with-families-methods TC-Representable-with-Rep))))

(test "TC-Map-with-Types has two families" 2
      (length (typeclass-with-families-type-families TC-Map-with-Types)))

;;; ====
;;; Pretty Printing
;;; ====
(test-section "Pretty Printing")

(test "type-family->string basic"
      "type Elem c"
      (type-family->string (make-type-family 'Elem '(c) K*)))

(test "type-family->string with kind"
      "type Rep f : Type"
      (type-family->string (make-type-family 'Rep '(f) 'Type)))

;;; ====
;;; Integration: Method Types with Type Families
;;; ====
(test-section "Integration Tests")

;; Verify that insert method type uses Elem
(let* ([methods (typeclass-with-families-methods TC-Collection-with-Elem)]
       [insert-type (cdr (assq 'insert methods))])
      ;; insert : ∀c. Collection c => (Elem c) → c → c
      (test "insert method exists" #t (pair? insert-type)))

;; Verify that tabulate uses Rep
(let* ([methods (typeclass-with-families-methods TC-Representable-with-Rep)]
       [tabulate-type (cdr (assq 'tabulate methods))])
      (test "tabulate method exists" #t (pair? tabulate-type)))

;;; ====
;;; Summary
;;; ====
(newline)
(display "====")
(newline)
(display "Tests passed: ")
(display *tests-passed*)
(newline)
(display "Tests failed: ")
(display *tests-failed*)
(newline)

(when (> *tests-failed* 0)
      (exit 1))
