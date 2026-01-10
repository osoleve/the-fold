;;; Test for Scheme-to-Rust Codegen Serializer
;;; RED PHASE: core/lang/rust-codegen.ss is not implemented yet.

(load "core/base/prelude.ss")
;;; We expect this to fail.
(load "core/lang/rust-codegen.ss")

(define (test name expected actual)
  (display "  ")
  (display name)
  (display ": ")
  (if (equal? expected actual)
      (display "✓")
      (begin
       (display "✗
    expected: ")
       (display expected)
       (display "
    got: ")
       (display actual)))
  (newline))

(define (test-section name)
  (newline)
  (display name)
  (newline))

(test-section "Rust IR Serialization - Literals")
(test "Int literal" "42" (rust-serialize '(R-Literal 42)))
(test "Bool literal true" "true" (rust-serialize '(R-Literal #t)))
(test "Bool literal false" "false" (rust-serialize '(R-Literal #f)))

(test-section "Rust IR Serialization - Expressions")
(test "Simple addition" "(1 + 2)" (rust-serialize '(R-Call + (R-Literal 1) (R-Literal 2))))
(test "Variable reference" "x" (rust-serialize '(R-Var x)))

(test-section "Rust IR Serialization - Statements")
(test "Let binding" "let x = 42;" (rust-serialize '(R-Let x (R-Literal 42))))
(test "If expression" "if true { 1 } else { 0 }"
      (rust-serialize '(R-If (R-Literal #t) (R-Literal 1) (R-Literal 0))))

(test-section "Rust IR Serialization - Functions")
(test "Simple function"
      "#[no_mangle]\npub extern \"C\" fn add_one(x: i64, fuel_in: u64, out: *mut TestResult) {\n    if out.is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + 1);\n    result.status = 1;\n    result.value = val as f64;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn add_one ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 1))

(newline)
