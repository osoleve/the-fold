(load "core/testing/test-framework.ss")
(load "lattice/validation/contract.ss")

;;; ====
;;; Primitive Specs
;;; ====

(test-group "primitive-specs"

  (define-test "symbol? checks symbols"
    (assert-true (contract-check spec/symbol 'hello))
    (assert-false (contract-check spec/symbol "hello"))
    (assert-false (contract-check spec/symbol 42)))

  (define-test "string? checks strings"
    (assert-true (contract-check spec/string "hello"))
    (assert-false (contract-check spec/string 'hello)))

  (define-test "number? checks numbers"
    (assert-true (contract-check spec/number 42))
    (assert-true (contract-check spec/number 3.14))
    (assert-false (contract-check spec/number "42")))

  (define-test "integer? checks integers"
    (assert-true (contract-check spec/integer 42))
    (assert-false (contract-check spec/integer 3.14)))

  (define-test "bytevector? checks bytevectors"
    (assert-true (contract-check spec/bytevector #vu8(1 2 3)))
    (assert-false (contract-check spec/bytevector "bytes")))

  (define-test "boolean? checks booleans"
    (assert-true (contract-check spec/boolean #t))
    (assert-true (contract-check spec/boolean #f))
    (assert-false (contract-check spec/boolean 0)))

  (define-test "null? checks null"
    (assert-true (contract-check spec/null '()))
    (assert-false (contract-check spec/null '(1))))

  (define-test "vector? checks vectors"
    (assert-true (contract-check spec/vector (vector 1 2 3)))
    (assert-false (contract-check spec/vector '(1 2 3)))))

;;; ====
;;; Combinators
;;; ====

(test-group "combinators"

  (define-test "and requires all"
    (let ([spec (spec/and spec/number (spec/range 0 100))])
      (assert-true (contract-check spec 50))
      (assert-false (contract-check spec 150))
      (assert-false (contract-check spec "50"))))

  (define-test "or requires one"
    (let ([spec (spec/or spec/string spec/symbol)])
      (assert-true (contract-check spec "hello"))
      (assert-true (contract-check spec 'hello))
      (assert-false (contract-check spec 42))))

  (define-test "not negates"
    (let ([spec (spec/not spec/null)])
      (assert-true (contract-check spec 42))
      (assert-false (contract-check spec '())))))

;;; ====
;;; Collections
;;; ====

(test-group "collections"

  (define-test "list-of checks all elements"
    (let ([spec (spec/list-of spec/number)])
      (assert-true (contract-check spec '(1 2 3)))
      (assert-true (contract-check spec '()))
      (assert-false (contract-check spec '(1 "two" 3)))
      (assert-false (contract-check spec 42))))

  (define-test "vector-of checks all elements"
    (let ([spec (spec/vector-of spec/symbol)])
      (assert-true (contract-check spec (vector 'a 'b 'c)))
      (assert-true (contract-check spec (vector)))
      (assert-false (contract-check spec (vector 'a "b")))))

  (define-test "pair-of checks car and cdr"
    (let ([spec (spec/pair-of spec/symbol spec/number)])
      (assert-true (contract-check spec '(x . 42)))
      (assert-false (contract-check spec '(42 . x)))
      (assert-false (contract-check spec 42)))))

;;; ====
;;; Constraints
;;; ====

(test-group "constraints"

  (define-test "range checks bounds"
    (let ([spec (spec/range 0 100)])
      (assert-true (contract-check spec 0))
      (assert-true (contract-check spec 50))
      (assert-true (contract-check spec 100))
      (assert-false (contract-check spec -1))
      (assert-false (contract-check spec 101))
      (assert-false (contract-check spec "50"))))

  (define-test "length= checks exact length"
    (let ([spec (spec/length= 3)])
      (assert-true (contract-check spec '(1 2 3)))
      (assert-true (contract-check spec (vector 'a 'b 'c)))
      (assert-true (contract-check spec "abc"))
      (assert-true (contract-check spec #vu8(1 2 3)))
      (assert-false (contract-check spec '(1 2)))
      (assert-false (contract-check spec '(1 2 3 4)))))

  (define-test "length>= checks minimum length"
    (let ([spec (spec/length>= 2)])
      (assert-true (contract-check spec '(1 2)))
      (assert-true (contract-check spec '(1 2 3)))
      (assert-false (contract-check spec '(1)))
      (assert-false (contract-check spec '())))))

;;; ====
;;; Fields (Structural)
;;; ====

(test-group "fields"

  (define-test "required fields must be present"
    (let ([spec (spec/fields '(name . (prim symbol?)) '(age . (prim number?)))])
      (assert-true (contract-check spec '((name . alice) (age . 30))))
      (assert-false (contract-check spec '((name . alice))))
      (assert-false (contract-check spec '()))))

  (define-test "field values must match spec"
    (let ([spec (spec/fields '(tag . (prim symbol?)))])
      (assert-true (contract-check spec '((tag . foo))))
      (assert-false (contract-check spec '((tag . "foo"))))))

  (define-test "optional fields may be absent"
    (let ([spec (spec/fields
                  '(name . (prim symbol?))
                  (spec/optional 'age spec/number))])
      (assert-true (contract-check spec '((name . alice))))
      (assert-true (contract-check spec '((name . alice) (age . 30))))
      (assert-false (contract-check spec '((name . alice) (age . "thirty"))))))

  (define-test "extra fields are allowed"
    ;; Fields spec checks required fields exist, doesn't reject extras
    (let ([spec (spec/fields '(name . (prim symbol?)))])
      (assert-true (contract-check spec '((name . alice) (extra . data)))))))

;;; ====
;;; Contract Record
;;; ====

(test-group "contract-record"

  (define-test "make-contract creates valid contract"
    (let ([c (make-contract 'my-rule spec/symbol)])
      (assert-true (contract? c))
      (assert-equal 'my-rule (contract-name c))
      (assert-equal spec/symbol (contract-spec c))
      (assert-equal 64 (string-length (contract-hash c)))))

  (define-test "same spec produces same hash"
    (let ([c1 (make-contract 'a spec/symbol)]
          [c2 (make-contract 'b spec/symbol)])
      ;; Same spec, different name — same hash (hash is of spec, not name)
      (assert-equal (contract-hash c1) (contract-hash c2))))

  (define-test "different specs produce different hashes"
    (let ([c1 (make-contract 'a spec/symbol)]
          [c2 (make-contract 'a spec/string)])
      (assert-false (string=? (contract-hash c1) (contract-hash c2)))))

  (define-test "hash is deterministic"
    (let ([c (make-contract 'test (spec/and spec/number (spec/range 0 100)))])
      (assert-equal (contract-hash c) (contract-hash c)))))

;;; ====
;;; Validation (Error Mode)
;;; ====

(test-group "validation"

  (define-test "valid data returns ok"
    (let ([result (contract-validate spec/symbol 'hello)])
      (assert-equal 'ok (car result))))

  (define-test "invalid data returns error with path"
    (let ([result (contract-validate spec/symbol 42)])
      (assert-equal 'error (car result))
      ;; Should have path and message
      (assert-true (string? (cadr result)))
      (assert-true (string? (caddr result)))))

  (define-test "nested errors track path"
    (let ([result (contract-validate (spec/list-of spec/number) '(1 2 "three" 4))])
      (assert-equal 'error (car result))
      ;; Path should indicate element [2]
      (assert-true (pair? (cdr result)))))

  (define-test "field errors include field name"
    (let ([spec (spec/fields '(name . (prim symbol?)) '(age . (prim number?)))])
      (let ([result (contract-validate spec '((name . alice) (age . "thirty")))])
        (assert-equal 'error (car result)))))

  (define-test "missing field error"
    (let ([spec (spec/fields '(name . (prim symbol?)) '(age . (prim number?)))])
      (let ([result (contract-validate spec '((name . alice)))])
        (assert-equal 'error (car result))
        ;; Error message should mention the missing field
        (assert-true (string? (caddr result)))))))

;;; ====
;;; Registry
;;; ====

(test-group "registry"

  (define-test "empty registry"
    (let ([reg (make-contract-registry)])
      (assert-true (contract-registry? reg))
      (assert-equal '() (registry-contracts reg))))

  (define-test "add and lookup"
    (let* ([c (make-contract 'hash-check
                (spec/and spec/bytevector (spec/length= 33)))]
           [reg (registry-add! (make-contract-registry) c)])
      (let ([found (registry-lookup reg 'hash-check)])
        (assert-true (contract? found))
        (assert-equal 'hash-check (contract-name found)))))

  (define-test "lookup missing returns #f"
    (assert-false (registry-lookup (make-contract-registry) 'nonexistent)))

  (define-test "registry hash is deterministic"
    (let* ([c1 (make-contract 'a spec/symbol)]
           [c2 (make-contract 'b spec/string)]
           [reg1 (registry-add! (registry-add! (make-contract-registry) c1) c2)]
           [reg2 (registry-add! (registry-add! (make-contract-registry) c2) c1)])
      ;; Order of insertion shouldn't matter (sorted by hash)
      (assert-equal (registry-hash reg1) (registry-hash reg2))))

  (define-test "different registries have different hashes"
    (let* ([reg1 (registry-add! (make-contract-registry)
                   (make-contract 'a spec/symbol))]
           [reg2 (registry-add! (make-contract-registry)
                   (make-contract 'a spec/string))])
      (assert-false (string=? (registry-hash reg1) (registry-hash reg2))))))

;;; ====
;;; Registry References
;;; ====

(test-group "registry-refs"

  (define-test "ref resolves via registry"
    (let* ([hash-contract (make-contract 'valid-hash
                            (spec/and spec/bytevector (spec/length= 33)))]
           [reg (registry-add! (make-contract-registry) hash-contract)])
      (assert-true (contract-check-in reg (spec/ref 'valid-hash)
                     (make-bytevector 33 0)))
      (assert-false (contract-check-in reg (spec/ref 'valid-hash)
                      (make-bytevector 32 0)))))

  (define-test "ref without registry fails"
    (assert-false (contract-check (spec/ref 'anything) 42)))

  (define-test "validate with ref produces structured error"
    (let* ([c (make-contract 'positive (spec/range 1 999999))]
           [reg (registry-add! (make-contract-registry) c)])
      (let ([result (contract-validate-in reg (spec/ref 'positive) -5)])
        (assert-equal 'error (car result))))))

;;; ====
;;; Serialization
;;; ====

(test-group "serialization"

  (define-test "serialize primitive spec"
    (assert-equal "(prim symbol?)" (contract-serialize-spec spec/symbol)))

  (define-test "serialize compound spec"
    (let ([spec (spec/and spec/number (spec/range 0 100))])
      (let ([s (contract-serialize-spec spec)])
        (assert-true (string? s))
        (assert-true (> (string-length s) 10)))))

  (define-test "serialize contract includes name"
    (let* ([c (make-contract 'test spec/symbol)]
           [s (contract-serialize c)])
      (assert-true (> (string-length s) 0)))))

;;; ====
;;; Real-World Contracts (Smoke Tests)
;;; ====

(test-group "real-world"

  (define-test "block contract"
    ;; Mirrors boundary/tools/validate.ss block validation
    (let* ([hash-spec (spec/and spec/bytevector (spec/length= 33))]
           [block-spec (spec/fields
                         '(tag . (prim symbol?))
                         '(payload . (prim bytevector?))
                         `(refs . ,(spec/vector-of hash-spec)))]
           [c (make-contract 'block block-spec)])
      ;; Valid block-like alist
      (assert-true (contract-check (contract-spec c)
        '((tag . test-block)
          (payload . #vu8(1 2 3))
          (refs . #()))))
      ;; Missing payload
      (assert-false (contract-check (contract-spec c)
        '((tag . test-block)
          (refs . #()))))))

  (define-test "hex-string contract"
    (let* ([hex-spec (spec/and spec/string (spec/length= 66))]
           [c (make-contract 'hex-hash hex-spec)])
      (assert-true (contract-check (contract-spec c)
        (make-string 66 #\a)))
      (assert-false (contract-check (contract-spec c)
        (make-string 64 #\a))))))

(run-all-tests)
