;;; Test for Scheme-to-Rust Type Mapping

(load "core/blocks/block.ss")
(load "core/types/types.ss")
(load "core/lang/rust-mapping.ss")

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

(test-section "Base Type Mapping")
(test "Nat -> u64" "u64" (scheme-type->rust 'Nat))
(test "Int -> i64" "i64" (scheme-type->rust 'Int))
(test "Bool -> bool" "bool" (scheme-type->rust 'Bool))
(test "Char -> char" "char" (scheme-type->rust 'Char))
(test "Unit -> ()" "()" (scheme-type->rust 'Unit))
(test "Void -> !" "!" (scheme-type->rust 'Void))
(test "String -> String" "String" (scheme-type->rust 'String))
(test "Bytes -> Vec<u8>" "Vec<u8>" (scheme-type->rust 'Bytes))
(test "Hash -> [u8; 33]" "[u8; 33]" (scheme-type->rust 'Hash))

(test-section "Function Type Mapping")
(test "Simple function" "extern \"C\" fn(i64, u64) -> i64"
      (scheme-type->rust '(-> Int Int)))
(test "Multi-arg function" "extern \"C\" fn(i64, bool, u64) -> String"
      (scheme-type->rust '(-> Int Bool String)))

(newline)
