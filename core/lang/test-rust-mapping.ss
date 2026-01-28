;;; core/lang/test-rust-mapping.ss — Tests for Scheme-to-Rust Type Mapping
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/blocks/block.ss")
(load "core/types/types.ss")
(load "core/lang/rust-mapping.ss")

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group base-type-mapping
  (define-test "Nat -> u64"
    (assert-equal "u64" (scheme-type->rust 'Nat)))

  (define-test "Int -> i64"
    (assert-equal "i64" (scheme-type->rust 'Int)))

  (define-test "Bool -> bool"
    (assert-equal "bool" (scheme-type->rust 'Bool)))

  (define-test "Char -> char"
    (assert-equal "char" (scheme-type->rust 'Char)))

  (define-test "Unit -> ()"
    (assert-equal "()" (scheme-type->rust 'Unit)))

  (define-test "Void -> !"
    (assert-equal "!" (scheme-type->rust 'Void)))

  (define-test "String -> String"
    (assert-equal "String" (scheme-type->rust 'String)))

  (define-test "Bytes -> Vec<u8>"
    (assert-equal "Vec<u8>" (scheme-type->rust 'Bytes)))

  (define-test "Hash -> [u8; 33]"
    (assert-equal "[u8; 33]" (scheme-type->rust 'Hash))))

(test-group function-type-mapping
  (define-test "Simple function"
    (assert-equal "extern \"C\" fn(i64, u64) -> i64"
                  (scheme-type->rust '(-> Int Int))))

  (define-test "Multi-arg function"
    (assert-equal "extern \"C\" fn(i64, bool, u64) -> String"
                  (scheme-type->rust '(-> Int Bool String)))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
