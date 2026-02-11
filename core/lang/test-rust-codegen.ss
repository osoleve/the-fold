;;; core/lang/test-rust-codegen.ss — Tests for Scheme-to-Rust Codegen Serializer
;;; Standardized to use test-framework.ss

(load "core/testing/test-framework.ss")
(load "core/base/prelude.ss")
(load "core/lang/rust-codegen.ss")

;;; ============================================================================
;;; Domain-specific helpers
;;; ============================================================================

(define (string-contains haystack needle)
  (let* ([h-len (string-length haystack)]
         [n-len (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i n-len) h-len) #f]
        [(string=? (substring haystack i (+ i n-len)) needle) #t]
        [else (loop (+ i 1))]))))

;;; ============================================================================
;;; Tests
;;; ============================================================================

(test-group rust-ir-serialization-literals
  (define-test "Int literal"
    (assert-equal "42" (rust-serialize '(R-Literal 42))))

  (define-test "Bool literal true"
    (assert-equal "true" (rust-serialize '(R-Literal #t))))

  (define-test "Bool literal false"
    (assert-equal "false" (rust-serialize '(R-Literal #f)))))

(test-group rust-ir-serialization-expressions
  (define-test "Simple addition"
    (assert-equal "(1 + 2)" (rust-serialize '(R-Call + (R-Literal 1) (R-Literal 2)))))

  (define-test "Variable reference"
    (assert-equal "x" (rust-serialize '(R-Var x)))))

(test-group rust-ir-serialization-statements
  (define-test "Let binding"
    (assert-equal "let x = 42;" (rust-serialize '(R-Let x (R-Literal 42)))))

  (define-test "If expression"
    (assert-equal "if true { 1 } else { 0 }"
                  (rust-serialize '(R-If (R-Literal #t) (R-Literal 1) (R-Literal 0))))))

(test-group rust-ir-serialization-functions
  (define-test "Simple function (i64)"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn add_one(x: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x + 1;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn add_one ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 1))))

(test-group rust-ir-serialization-comparison
  (define-test "Less than"
    (assert-equal "(x < y)" (rust-serialize '(R-Call lt? (R-Var x) (R-Var y)))))

  (define-test "Less or equal"
    (assert-equal "(x <= y)" (rust-serialize '(R-Call le? (R-Var x) (R-Var y)))))

  (define-test "Greater than"
    (assert-equal "(x > y)" (rust-serialize '(R-Call gt? (R-Var x) (R-Var y)))))

  (define-test "Greater or equal"
    (assert-equal "(x >= y)" (rust-serialize '(R-Call ge? (R-Var x) (R-Var y)))))

  (define-test "Equal"
    (assert-equal "(x == y)" (rust-serialize '(R-Call eq? (R-Var x) (R-Var y))))))

(test-group rust-ir-serialization-logical
  (define-test "And"
    (assert-equal "(x && y)" (rust-serialize '(R-Call and (R-Var x) (R-Var y)))))

  (define-test "Or"
    (assert-equal "(x || y)" (rust-serialize '(R-Call or (R-Var x) (R-Var y)))))

  (define-test "Not"
    (assert-equal "(!x)" (rust-serialize '(R-Call not (R-Var x))))))

(test-group rust-ir-serialization-bitwise
  (define-test "Bitand"
    (assert-equal "(x & y)" (rust-serialize '(R-Call bitand (R-Var x) (R-Var y)))))

  (define-test "Bitor"
    (assert-equal "(x | y)" (rust-serialize '(R-Call bitor (R-Var x) (R-Var y)))))

  (define-test "Bitxor"
    (assert-equal "(x ^ y)" (rust-serialize '(R-Call bitxor (R-Var x) (R-Var y)))))

  (define-test "Shift left"
    (assert-equal "(x << y)" (rust-serialize '(R-Call shl (R-Var x) (R-Var y)))))

  (define-test "Shift right"
    (assert-equal "(x >> y)" (rust-serialize '(R-Call shr (R-Var x) (R-Var y))))))

(test-group rust-ir-serialization-math
  (define-test "Abs"
    (assert-equal "(x.abs())" (rust-serialize '(R-Call abs (R-Var x)))))

  (define-test "Sqrt"
    (assert-equal "(x.sqrt())" (rust-serialize '(R-Call sqrt (R-Var x)))))

  (define-test "Sin"
    (assert-equal "(x.sin())" (rust-serialize '(R-Call sin (R-Var x)))))

  (define-test "Cos"
    (assert-equal "(x.cos())" (rust-serialize '(R-Call cos (R-Var x)))))

  (define-test "Tan"
    (assert-equal "(x.tan())" (rust-serialize '(R-Call tan (R-Var x)))))

  (define-test "Log (ln)"
    (assert-equal "(x.ln())" (rust-serialize '(R-Call log (R-Var x)))))

  (define-test "Floor"
    (assert-equal "(x.floor())" (rust-serialize '(R-Call floor (R-Var x)))))

  (define-test "Ceiling"
    (assert-equal "(x.ceil())" (rust-serialize '(R-Call ceiling (R-Var x)))))

  (define-test "Neg"
    (assert-equal "(-x)" (rust-serialize '(R-Call neg (R-Var x)))))

  (define-test "Sq (powi)"
    (assert-equal "(x.powi(2))" (rust-serialize '(R-Call sq (R-Var x)))))

  (define-test "Exp"
    (assert-equal "(x.exp())" (rust-serialize '(R-Call exp (R-Var x)))))

  (define-test "Asin"
    (assert-equal "(x.asin())" (rust-serialize '(R-Call asin (R-Var x)))))

  (define-test "Acos"
    (assert-equal "(x.acos())" (rust-serialize '(R-Call acos (R-Var x)))))

  (define-test "Atan"
    (assert-equal "(x.atan())" (rust-serialize '(R-Call atan (R-Var x)))))

  (define-test "Sinh"
    (assert-equal "(x.sinh())" (rust-serialize '(R-Call sinh (R-Var x)))))

  (define-test "Cosh"
    (assert-equal "(x.cosh())" (rust-serialize '(R-Call cosh (R-Var x)))))

  (define-test "Tanh"
    (assert-equal "(x.tanh())" (rust-serialize '(R-Call tanh (R-Var x)))))

  (define-test "Expt (powf)"
    (assert-equal "(x.powf(y))" (rust-serialize '(R-Call expt (R-Var x) (R-Var y)))))

  (define-test "Pow (powf)"
    (assert-equal "(x.powf(y))" (rust-serialize '(R-Call pow (R-Var x) (R-Var y)))))

  (define-test "Atan2"
    (assert-equal "(y.atan2(x))" (rust-serialize '(R-Call atan2 (R-Var y) (R-Var x)))))

  (define-test "Hypot"
    (assert-equal "(x.hypot(y))" (rust-serialize '(R-Call hypot (R-Var x) (R-Var y)))))

  (define-test "Min"
    (assert-equal "(x.min(y))" (rust-serialize '(R-Call min (R-Var x) (R-Var y)))))

  (define-test "Max"
    (assert-equal "(x.max(y))" (rust-serialize '(R-Call max (R-Var x) (R-Var y)))))

  (define-test "Truncate"
    (assert-equal "(x.trunc())" (rust-serialize '(R-Call truncate (R-Var x))))))

(test-group rust-ir-serialization-block
  (define-test "Empty block"
    (assert-equal "{}" (rust-serialize '(R-Block))))

  (define-test "Single expr block"
    (assert-equal "{ x }" (rust-serialize '(R-Block (R-Var x)))))

  (define-test "Let + expr block"
    (assert-equal "{ let x = 42; x }" (rust-serialize '(R-Block (R-Let x (R-Literal 42)) (R-Var x))))))

(test-group scheme-to-rust-ir-translation
  (define-test "Literal number"
    (assert-equal '(R-Literal 42) (scheme->rust-ir 42)))

  (define-test "Literal bool"
    (assert-equal '(R-Literal #t) (scheme->rust-ir #t)))

  (define-test "Variable"
    (assert-equal '(R-Var x) (scheme->rust-ir 'x)))

  (define-test "Prim add"
    (assert-equal '(R-Call add (R-Literal 1) (R-Literal 2)) (scheme->rust-ir '(prim 'add 1 2))))

  (define-test "Prim lt?"
    (assert-equal '(R-Call lt? (R-Var x) (R-Var y)) (scheme->rust-ir '(prim 'lt? x y))))

  (define-test "Direct +"
    (assert-equal '(R-Call + (R-Literal 1) (R-Literal 2)) (scheme->rust-ir '(+ 1 2))))

  (define-test "If expr"
    (assert-equal '(R-If (R-Var p) (R-Literal 1) (R-Literal 0)) (scheme->rust-ir '(if p 1 0))))

  (define-test "Let binding"
    (assert-equal '(R-Block (R-Let x (R-Literal 42)) (R-Var x)) (scheme->rust-ir '(let ((x 42)) x))))

  (define-test "Nested"
    (assert-equal '(R-Call lt? (R-Call abs (R-Var x)) (R-Call abs (R-Var y)))
                  (scheme->rust-ir '(prim 'lt? (abs x) (abs y))))))

(test-group end-to-end-compilation
  (load "boundary/ffi/rust-compile.ss")

  (define-test "Compiling basic add"
    (let ([res (compile-rust-lib "test_codegen"
                                 '(R-Fn add_one ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))
                                 1)])
      (cleanup-rust-lib "test_codegen")
      (assert-equal 'ok (car res))))

  (define-test "Compiling with math ops"
    (let ([res (compile-rust-lib "test_math"
                                 '(R-Fn sum_abs ((x f64) (y f64)) f64
                                   (R-Call + (R-Call abs (R-Var x)) (R-Call abs (R-Var y))))
                                 2)])
      (cleanup-rust-lib "test_math")
      (assert-equal 'ok (car res))))

  (define-test "Compiling with bitwise"
    (let ([res (compile-rust-lib "test_bitwise"
                                 '(R-Fn mask_and_shift ((x i64) (mask i64) (shift i64)) i64
                                   (R-Call shl (R-Call bitand (R-Var x) (R-Var mask)) (R-Var shift)))
                                 3)])
      (cleanup-rust-lib "test_bitwise")
      (assert-equal 'ok (car res)))))

(test-group fuel-cost-computation
  (define-test "Literal cost"
    (assert-equal 0 (ir-fuel-cost '(R-Literal 42))))

  (define-test "Variable cost"
    (assert-equal 0 (ir-fuel-cost '(R-Var x))))

  (define-test "Simple add cost"
    (assert-equal 1 (ir-fuel-cost '(R-Call + (R-Literal 1) (R-Literal 2)))))

  (define-test "Add with vars cost"
    (assert-equal 1 (ir-fuel-cost '(R-Call + (R-Var x) (R-Var y)))))

  (define-test "Division cost"
    (assert-equal 2 (ir-fuel-cost '(R-Call / (R-Var x) (R-Var y)))))

  (define-test "Nested ops cost"
    (assert-equal 2 (ir-fuel-cost '(R-Call + (R-Call * (R-Var x) (R-Var y)) (R-Var z)))))

  (define-test "If expression cost"
    (assert-equal 1 (ir-fuel-cost '(R-If (R-Var p) (R-Literal 1) (R-Literal 0)))))

  (define-test "If with branches cost (max)"
    (assert-equal 4 (ir-fuel-cost '(R-If (R-Call lt? (R-Var x) (R-Var y))
                                         (R-Call / (R-Var a) (R-Var b))
                                         (R-Literal 0)))))

  (define-test "Block cost"
    (assert-equal 1 (ir-fuel-cost '(R-Block (R-Let x (R-Call * (R-Var a) (R-Var b))) (R-Var x)))))

  (define-test "Trig function cost"
    (assert-equal 3 (ir-fuel-cost '(R-Call sin (R-Var x)))))

  (define-test "Sqrt cost"
    (assert-equal 2 (ir-fuel-cost '(R-Call sqrt (R-Var x)))))

  (define-test "Op-fuel-cost: add"
    (assert-equal 1 (op-fuel-cost 'add)))

  (define-test "Op-fuel-cost: div"
    (assert-equal 2 (op-fuel-cost 'div)))

  (define-test "Op-fuel-cost: sin"
    (assert-equal 3 (op-fuel-cost 'sin)))

  (define-test "Op-fuel-cost: sq"
    (assert-equal 1 (op-fuel-cost 'sq)))

  (define-test "Op-fuel-cost: asin"
    (assert-equal 3 (op-fuel-cost 'asin)))

  (define-test "Op-fuel-cost: sinh"
    (assert-equal 3 (op-fuel-cost 'sinh)))

  (define-test "Op-fuel-cost: exp"
    (assert-equal 3 (op-fuel-cost 'exp)))

  (define-test "Op-fuel-cost: atan2"
    (assert-equal 3 (op-fuel-cost 'atan2)))

  (define-test "Op-fuel-cost: hypot"
    (assert-equal 3 (op-fuel-cost 'hypot)))

  (define-test "Op-fuel-cost: min"
    (assert-equal 1 (op-fuel-cost 'min)))

  (define-test "Op-fuel-cost: max"
    (assert-equal 1 (op-fuel-cost 'max)))

  (define-test "Op-fuel-cost: truncate"
    (assert-equal 1 (op-fuel-cost 'truncate)))

  (define-test "Op-fuel-cost: remainder"
    (assert-equal 2 (op-fuel-cost 'remainder)))

  (define-test "Op-fuel-cost: unknown"
    (assert-equal 1 (op-fuel-cost 'unknown-op))))

(test-group autodiff-gradient-formulas
  (define-test "Gradient: add"
    (assert-equal '(1 1) (op-local-gradient 'add)))

  (define-test "Gradient: sub"
    (assert-equal '(1 -1) (op-local-gradient 'sub)))

  (define-test "Gradient: mul"
    (assert-equal '(b a) (op-local-gradient 'mul)))

  (define-test "Gradient: div"
    (assert-equal '((/ 1 b) (/ (- a) (* b b))) (op-local-gradient 'div)))

  (define-test "Gradient: neg"
    (assert-equal '(-1) (op-local-gradient 'neg)))

  (define-test "Gradient: sqrt"
    (assert-equal '((/ 1 (* 2 (sqrt a)))) (op-local-gradient 'sqrt)))

  (define-test "Gradient: sin"
    (assert-equal '((cos a)) (op-local-gradient 'sin)))

  (define-test "Gradient: cos"
    (assert-equal '((- (sin a))) (op-local-gradient 'cos)))

  (define-test "Gradient: exp"
    (assert-equal '((exp a)) (op-local-gradient 'exp)))

  (define-test "Gradient: log"
    (assert-equal '((/ 1 a)) (op-local-gradient 'log)))

  (define-test "Gradient: sq"
    (assert-equal '((* 2 a)) (op-local-gradient 'sq)))

  (define-test "Gradient: sinh"
    (assert-equal '((cosh a)) (op-local-gradient 'sinh)))

  (define-test "Gradient: cosh"
    (assert-equal '((sinh a)) (op-local-gradient 'cosh)))

  (define-test "Gradient: tanh"
    (assert-equal '((/ 1 (* (cosh a) (cosh a)))) (op-local-gradient 'tanh)))

  (define-test "Gradient: atan2"
    (assert-equal '((/ b (+ (* a a) (* b b))) (/ (- a) (+ (* a a) (* b b)))) (op-local-gradient 'atan2)))

  (define-test "Gradient: hypot"
    (assert-equal '((/ a (hypot a b)) (/ b (hypot a b))) (op-local-gradient 'hypot)))

  (define-test "Gradient: min"
    (assert-equal '((if (<= a b) 1 0) (if (<= a b) 0 1)) (op-local-gradient 'min)))

  (define-test "Gradient: max"
    (assert-equal '((if (>= a b) 1 0) (if (>= a b) 0 1)) (op-local-gradient 'max)))

  (define-test "Gradient: truncate"
    (assert-equal '(0) (op-local-gradient 'truncate)))

  (define-test "Gradient: non-diff lt?"
    (assert-equal #f (op-local-gradient 'lt?)))

  (define-test "Gradient: non-diff bitand"
    (assert-equal #f (op-local-gradient 'bitand)))

  (define-test "Differentiable: add"
    (assert-true (if (op-differentiable? 'add) #t #f)))

  (define-test "Differentiable: lt?"
    (assert-false (op-differentiable? 'lt?))))

(test-group rust-emit-with-auto-cost
  (define-test "Auto-computed cost (i64 add)"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn auto_add(x: i64, y: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x + y;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn auto_add ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y))))))

  (define-test "Auto-computed cost (f64 nested ops)"
    (assert-equal "#[repr(C)] pub struct F64Result { pub status: u8, pub _pad: [u8; 7], pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn nested_ops(x: f64, y: f64, z: f64, fuel_in: u64, out: *mut F64Result) {\n    if (out as *const F64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x * y) + z;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn nested_ops ((x f64) (y f64) (z f64)) f64 (R-Call + (R-Call * (R-Var x) (R-Var y)) (R-Var z))))))

  (define-test "Explicit cost override still works"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn override_cost(x: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 999;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x + 1;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn override_cost ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 999))))

(test-group type-safe-code-generation
  (define-test "Bool function uses BoolResult"
    (assert-equal "#[repr(C)] pub struct BoolResult { pub status: u8, pub value: u8, pub _pad: [u8; 6], pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn is_positive(x: i64, fuel_in: u64, out: *mut BoolResult) {\n    if (out as *const BoolResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x > 0;\n    result.status = 1;\n    result.value = if val { 1 } else { 0 };\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn is_positive ((x i64)) bool (R-Call gt? (R-Var x) (R-Literal 0))))))

  (define-test "U64 function uses U64Result"
    (assert-equal "#[repr(C)] pub struct U64Result { pub status: u8, pub _pad: [u8; 7], pub value: u64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn double_unsigned(x: u64, fuel_in: u64, out: *mut U64Result) {\n    if (out as *const U64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x * 2;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn double_unsigned ((x u64)) u64 (R-Call * (R-Var x) (R-Literal 2)))))))

(test-group typed-literals
  (define-test "Typed i64 literal"
    (assert-equal "42i64" (rust-serialize '(R-Literal 42 i64))))

  (define-test "Typed f64 literal (int)"
    (assert-equal "42.0_f64" (rust-serialize '(R-Literal 42 f64))))

  (define-test "Typed f64 literal (float)"
    (assert-equal "3.14_f64" (rust-serialize '(R-Literal 3.14 f64))))

  (define-test "Typed u64 literal"
    (assert-equal "100u64" (rust-serialize '(R-Literal 100 u64))))

  (define-test "Typed i32 literal"
    (assert-equal "10i32" (rust-serialize '(R-Literal 10 i32))))

  (define-test "Typed f32 literal"
    (assert-equal "2.5_f32" (rust-serialize '(R-Literal 2.5 f32))))

  (define-test "Untyped int literal"
    (assert-equal "42" (rust-serialize '(R-Literal 42))))

  (define-test "Untyped float literal"
    (assert-equal "3.14_f64" (rust-serialize '(R-Literal 3.14))))

  (define-test "Untyped bool true"
    (assert-equal "true" (rust-serialize '(R-Literal #t))))

  (define-test "Untyped bool false"
    (assert-equal "false" (rust-serialize '(R-Literal #f)))))

(test-group type-mapping-helpers
  (define-test "ret-type->result-struct i64"
    (assert-equal "I64Result" (ret-type->result-struct 'i64)))

  (define-test "ret-type->result-struct f64"
    (assert-equal "F64Result" (ret-type->result-struct 'f64)))

  (define-test "ret-type->result-struct bool"
    (assert-equal "BoolResult" (ret-type->result-struct 'bool)))

  (define-test "ret-type->result-struct u64"
    (assert-equal "U64Result" (ret-type->result-struct 'u64)))

  (define-test "ret-type->result-struct i32"
    (assert-equal "I32Result" (ret-type->result-struct 'i32)))

  (define-test "ret-type->result-struct f32"
    (assert-equal "F32Result" (ret-type->result-struct 'f32)))

  (define-test "ret-type->result-struct unknown"
    (assert-equal "TestResult" (ret-type->result-struct 'unknown)))

  (define-test "ret-type->value-assignment i64"
    (assert-equal "    result.value = val;\n" (ret-type->value-assignment 'i64)))

  (define-test "ret-type->value-assignment f64"
    (assert-equal "    result.value = val;\n" (ret-type->value-assignment 'f64)))

  (define-test "ret-type->value-assignment bool"
    (assert-equal "    result.value = if val { 1 } else { 0 };\n" (ret-type->value-assignment 'bool)))

  (define-test "ret-type->value-assignment i32"
    (assert-equal "    result.value = val;\n" (ret-type->value-assignment 'i32)))

  (define-test "ret-type->value-assignment f32"
    (assert-equal "    result.value = val;\n" (ret-type->value-assignment 'f32)))

  (define-test "ret-type->value-assignment unknown"
    (assert-equal "    result.value = val as f64;\n" (ret-type->value-assignment 'unknown))))

(test-group i32-f32-function-emit
  (define-test "I32 function uses I32Result"
    (assert-equal "#[repr(C)] pub struct I32Result { pub status: u8, pub _pad: [u8; 3], pub value: i32, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn add_small(x: i32, y: i32, fuel_in: u64, out: *mut I32Result) {\n    if (out as *const I32Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x + y;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn add_small ((x i32) (y i32)) i32 (R-Call + (R-Var x) (R-Var y))))))

  (define-test "F32 function uses F32Result"
    (assert-equal "#[repr(C)] pub struct F32Result { pub status: u8, pub _pad: [u8; 3], pub value: f32, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn mul_float(x: f32, y: f32, fuel_in: u64, out: *mut F32Result) {\n    if (out as *const F32Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x * y;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn mul_float ((x f32) (y f32)) f32 (R-Call * (R-Var x) (R-Var y)))))))

(test-group division-by-zero-protection
  (define-test "Collect divisors: simple div"
    (assert-equal '((R-Var y))
                  (ir-collect-divisors '(R-Call / (R-Var x) (R-Var y)))))

  (define-test "Collect divisors: simple mod"
    (assert-equal '((R-Var y))
                  (ir-collect-divisors '(R-Call mod (R-Var x) (R-Var y)))))

  (define-test "Collect divisors: nested"
    (assert-equal '((R-Var b) (R-Var d))
                  (ir-collect-divisors '(R-Call + (R-Call / (R-Var a) (R-Var b))
                                                  (R-Call % (R-Var c) (R-Var d))))))

  (define-test "Collect divisors: no division"
    (assert-equal '()
                  (ir-collect-divisors '(R-Call + (R-Var x) (R-Var y)))))

  (define-test "Collect divisors: literal divisor"
    (assert-equal '((R-Literal 2))
                  (ir-collect-divisors '(R-Call / (R-Var x) (R-Literal 2)))))

  (define-test "integer-type? i64"
    (assert-true (if (integer-type? 'i64) #t #f)))

  (define-test "integer-type? i32"
    (assert-true (if (integer-type? 'i32) #t #f)))

  (define-test "integer-type? u64"
    (assert-true (if (integer-type? 'u64) #t #f)))

  (define-test "integer-type? f64"
    (assert-false (integer-type? 'f64)))

  (define-test "integer-type? f32"
    (assert-false (integer-type? 'f32)))

  (define-test "integer-type? bool"
    (assert-false (integer-type? 'bool)))

  (define-test "Guard for i64 divisor var"
    (assert-equal "y == 0"
                  (ir-divisor->guard '(R-Var y) '((x i64) (y i64)))))

  (define-test "No guard for f64 divisor var"
    (assert-equal #f
                  (ir-divisor->guard '(R-Var y) '((x f64) (y f64)))))

  (define-test "Guard for zero literal"
    (assert-equal "true /* constant zero divisor */"
                  (ir-divisor->guard '(R-Literal 0) '())))

  (define-test "No guard for non-zero literal"
    (assert-equal #f
                  (ir-divisor->guard '(R-Literal 5) '())))

  (define-test "Guards for i64 division"
    (assert-equal "    // Division-by-zero protection\n    if y == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n"
                  (emit-divisor-guards '((R-Var y)) '((x i64) (y i64)))))

  (define-test "No guards for f64 division"
    (assert-equal ""
                  (emit-divisor-guards '((R-Var y)) '((x f64) (y f64)))))

  (define-test "I64 division emits guard"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn safe_div(x: i64, y: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    // Division-by-zero protection\n    if y == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n    let val = x / y;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn safe_div ((x i64) (y i64)) i64 (R-Call / (R-Var x) (R-Var y))))))

  (define-test "F64 division no guard (Inf/NaN)"
    (assert-equal "#[repr(C)] pub struct F64Result { pub status: u8, pub _pad: [u8; 7], pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn float_div(x: f64, y: f64, fuel_in: u64, out: *mut F64Result) {\n    if (out as *const F64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = x / y;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn float_div ((x f64) (y f64)) f64 (R-Call / (R-Var x) (R-Var y)))))))

(test-group ir-contains-integer-var-tests
  (define-test "Contains int var: simple"
    (assert-true (ir-contains-integer-var? '(R-Var x) '((x i64)))))

  (define-test "Contains int var: float"
    (assert-false (ir-contains-integer-var? '(R-Var x) '((x f64)))))

  (define-test "Contains int var: in call"
    (assert-true (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x i64) (y i64)))))

  (define-test "Contains int var: mixed"
    (assert-true (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x i64) (y f64)))))

  (define-test "Contains int var: all float"
    (assert-false (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x f64) (y f64)))))

  (define-test "Contains int var: literal only"
    (assert-false (ir-contains-integer-var? '(R-Literal 5) '()))))

(test-group complex-divisor-guards
  (define-test "Guard for complex int expression"
    (assert-equal "(y + z) == 0"
                  (ir-divisor->guard '(R-Call + (R-Var y) (R-Var z)) '((x i64) (y i64) (z i64)))))

  (define-test "No guard for complex float expression"
    (assert-equal #f
                  (ir-divisor->guard '(R-Call + (R-Var y) (R-Var z)) '((x f64) (y f64) (z f64)))))

  (define-test "Complex i64 divisor emits guard"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn div_by_sum(x: i64, y: i64, z: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 3;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    // Division-by-zero protection\n    if (y + z) == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n    let val = x / (y + z);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn div_by_sum ((x i64) (y i64) (z i64)) i64
                               (R-Call / (R-Var x) (R-Call + (R-Var y) (R-Var z))))))))

(test-group variadic-primitives
  (define-test "Variadic add (3 args)"
    (assert-equal "((a + b) + c)"
                  (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic add (4 args)"
    (assert-equal "(((a + b) + c) + d)"
                  (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d)))))

  (define-test "Variadic add (5 args)"
    (assert-equal "((((a + b) + c) + d) + e)"
                  (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e)))))

  (define-test "Variadic mul (3 args)"
    (assert-equal "((x * y) * z)"
                  (rust-serialize '(R-Call * (R-Var x) (R-Var y) (R-Var z)))))

  (define-test "Variadic and (3 args)"
    (assert-equal "((a && b) && c)"
                  (rust-serialize '(R-Call and (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic or (4 args)"
    (assert-equal "(((a || b) || c) || d)"
                  (rust-serialize '(R-Call or (R-Var a) (R-Var b) (R-Var c) (R-Var d)))))

  (define-test "Variadic bitand (3 args)"
    (assert-equal "((a & b) & c)"
                  (rust-serialize '(R-Call bitand (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic bitor (3 args)"
    (assert-equal "((a | b) | c)"
                  (rust-serialize '(R-Call bitor (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic bitxor (3 args)"
    (assert-equal "((a ^ b) ^ c)"
                  (rust-serialize '(R-Call bitxor (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic min (3 args)"
    (assert-equal "(a.min(b).min(c))"
                  (rust-serialize '(R-Call min (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic min (4 args)"
    (assert-equal "(a.min(b).min(c).min(d))"
                  (rust-serialize '(R-Call min (R-Var a) (R-Var b) (R-Var c) (R-Var d)))))

  (define-test "Variadic max (3 args)"
    (assert-equal "(a.max(b).max(c))"
                  (rust-serialize '(R-Call max (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Variadic max (4 args)"
    (assert-equal "(a.max(b).max(c).max(d))"
                  (rust-serialize '(R-Call max (R-Var a) (R-Var b) (R-Var c) (R-Var d)))))

  (define-test "Variadic add with literals"
    (assert-equal "((1 + 2) + 3)"
                  (rust-serialize '(R-Call + (R-Literal 1) (R-Literal 2) (R-Literal 3)))))

  (define-test "Variadic mul with typed literals"
    (assert-equal "((1i64 * 2i64) * 3i64)"
                  (rust-serialize '(R-Call * (R-Literal 1 i64) (R-Literal 2 i64) (R-Literal 3 i64))))))

(test-group variadic-fuel-cost
  (define-test "Fuel cost: binary add (2 args)"
    (assert-equal 1
                  (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b)))))

  (define-test "Fuel cost: variadic add (3 args)"
    (assert-equal 2
                  (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Fuel cost: variadic add (5 args)"
    (assert-equal 4
                  (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e)))))

  (define-test "Fuel cost: variadic min (4 args)"
    (assert-equal 3
                  (ir-fuel-cost '(R-Call min (R-Var a) (R-Var b) (R-Var c) (R-Var d))))))

(test-group scheme-ir-variadic
  (define-test "Scheme->IR variadic add"
    (assert-equal '(R-Call + (R-Literal 1) (R-Literal 2) (R-Literal 3) (R-Literal 4) (R-Literal 5))
                  (scheme->rust-ir '(+ 1 2 3 4 5))))

  (define-test "Scheme->IR variadic mul"
    (assert-equal '(R-Call * (R-Var a) (R-Var b) (R-Var c))
                  (scheme->rust-ir '(* a b c))))

  (define-test "Full variadic add function"
    (assert-equal "#[repr(C)] pub struct I64Result { pub status: u8, pub _pad: [u8; 7], pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn sum5(a: i64, b: i64, c: i64, d: i64, e: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 4;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (((a + b) + c) + d) + e;\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
                  (rust-emit '(R-Fn sum5 ((a i64) (b i64) (c i64) (d i64) (e i64)) i64
                               (R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e)))))))

(test-group variadic-edge-cases
  (define-test "0-arg add identity"
    (assert-equal "0"
                  (rust-serialize '(R-Call +))))

  (define-test "0-arg mul identity"
    (assert-equal "1"
                  (rust-serialize '(R-Call *))))

  (define-test "0-arg and identity"
    (assert-equal "true"
                  (rust-serialize '(R-Call and))))

  (define-test "0-arg or identity"
    (assert-equal "false"
                  (rust-serialize '(R-Call or))))

  (define-test "1-arg add pass-through"
    (assert-equal "x"
                  (rust-serialize '(R-Call + (R-Var x)))))

  (define-test "1-arg mul pass-through"
    (assert-equal "42"
                  (rust-serialize '(R-Call * (R-Literal 42)))))

  (define-test "1-arg and pass-through"
    (assert-equal "flag"
                  (rust-serialize '(R-Call and (R-Var flag)))))

  (define-test "1-arg or pass-through"
    (assert-equal "cond"
                  (rust-serialize '(R-Call or (R-Var cond)))))

  (define-test "1-arg bitand pass-through"
    (assert-equal "bits"
                  (rust-serialize '(R-Call bitand (R-Var bits)))))

  (define-test "Comparison < stays binary"
    (assert-equal "(a < b)"
                  (rust-serialize '(R-Call < (R-Var a) (R-Var b)))))

  (define-test "Comparison < 3-arg fallback"
    (assert-equal "<(a, b, c)"
                  (rust-serialize '(R-Call < (R-Var a) (R-Var b) (R-Var c)))))

  (define-test "Shift << stays binary"
    (assert-equal "(a << b)"
                  (rust-serialize '(R-Call shl (R-Var a) (R-Var b)))))

  (define-test "Sub - stays binary"
    (assert-equal "(a - b)"
                  (rust-serialize '(R-Call - (R-Var a) (R-Var b)))))

  (define-test "Div / stays binary"
    (assert-equal "(a / b)"
                  (rust-serialize '(R-Call / (R-Var a) (R-Var b))))))

(test-group variadic-fuel-edge-cases
  (define-test "Fuel cost: 0-arg add"
    (assert-equal 0
                  (ir-fuel-cost '(R-Call +))))

  (define-test "Fuel cost: 1-arg mul"
    (assert-equal 0
                  (ir-fuel-cost '(R-Call * (R-Var x)))))

  (define-test "Fuel cost: 1-arg sqrt (not variadic-safe)"
    (assert-equal 2
                  (ir-fuel-cost '(R-Call sqrt (R-Var x)))))

  (define-test "Fuel cost: 1-arg sin (not variadic-safe)"
    (assert-equal 3
                  (ir-fuel-cost '(R-Call sin (R-Var x))))))

(test-group variadic-safe-op-tests
  (define-test "variadic-safe: add"
    (assert-true (if (variadic-safe-op? '+) #t #f)))

  (define-test "variadic-safe: mul"
    (assert-true (if (variadic-safe-op? '*) #t #f)))

  (define-test "variadic-safe: and"
    (assert-true (if (variadic-safe-op? 'and) #t #f)))

  (define-test "variadic-safe: or"
    (assert-true (if (variadic-safe-op? 'or) #t #f)))

  (define-test "variadic-safe: sub (not)"
    (assert-false (variadic-safe-op? '-)))

  (define-test "variadic-safe: div (not)"
    (assert-false (variadic-safe-op? '/)))

  (define-test "variadic-safe: lt (not)"
    (assert-false (variadic-safe-op? '<)))

  (define-test "variadic-safe: eq (not)"
    (assert-false (variadic-safe-op? 'eq?)))

  (define-test "variadic-safe: shl (not)"
    (assert-false (variadic-safe-op? 'shl))))

(test-group crate-integration
  (define-test "rust-emit-module uses crate import"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn test_fn ((x i64)) i64 (R-Var x)))
                      "use crate::{I64Result}") #t #f)))

  (define-test "rust-emit-module no inline struct"
    (assert-false (string-contains
                   (rust-emit-module '(R-Fn test_fn ((x i64)) i64 (R-Var x)))
                   "#[repr(C)]")))

  (define-test "rust-emit-module prefixes fn with fold_"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn my_func ((x i64)) i64 (R-Var x)))
                      "pub extern \"C\" fn fold_my_func") #t #f)))

  (define-test "rust-emit-module includes header comment"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn test ((x i64)) i64 (R-Var x)))
                      "//! Auto-generated by The Fold codegen") #t #f)))

  (define-test "rust-emit-module F64Result import"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn float_fn ((x f64)) f64 (R-Var x)))
                      "use crate::{F64Result}") #t #f)))

  (define-test "rust-emit-module BoolResult import"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn bool_fn ((x i64) (y i64)) bool (R-Call < (R-Var x) (R-Var y))))
                      "use crate::{BoolResult}") #t #f))))

(test-group identifier-sanitization
  (define-test "sanitize: normal name"
    (assert-equal "my_func"
                  (sanitize-rust-ident "my_func")))

  (define-test "sanitize: hyphenated name"
    (assert-equal "foo_bar"
                  (sanitize-rust-ident "foo-bar")))

  (define-test "sanitize: special chars"
    (assert-equal "set_"
                  (sanitize-rust-ident "set!")))

  (define-test "sanitize: uppercase to lower"
    (assert-equal "myclass"
                  (sanitize-rust-ident "MyClass")))

  (define-test "sanitize: leading number"
    (assert-equal "m_123test"
                  (sanitize-rust-ident "123test")))

  (define-test "sanitize: all numbers"
    (assert-equal "m_42"
                  (sanitize-rust-ident "42")))

  (define-test "sanitize: empty string"
    (assert-equal "unnamed"
                  (sanitize-rust-ident "")))

  (define-test "sanitize: keyword fn"
    (assert-equal "m_fn"
                  (sanitize-rust-ident "fn")))

  (define-test "sanitize: keyword let"
    (assert-equal "m_let"
                  (sanitize-rust-ident "let")))

  (define-test "sanitize: keyword if"
    (assert-equal "m_if"
                  (sanitize-rust-ident "if")))

  (define-test "sanitize: keyword struct"
    (assert-equal "m_struct"
                  (sanitize-rust-ident "struct")))

  (define-test "sanitize: keyword impl"
    (assert-equal "m_impl"
                  (sanitize-rust-ident "impl")))

  (define-test "rust-emit-module sanitizes hyphenated name"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn foo-bar ((x i64)) i64 (R-Var x)))
                      "fn fold_foo_bar") #t #f)))

  (define-test "rust-emit-module sanitizes keyword name"
    (assert-true (if (string-contains
                      (rust-emit-module '(R-Fn fn ((x i64)) i64 (R-Var x)))
                      "fn fold_m_fn") #t #f))))

(test-group r-lambda-serialization
  (define-test "Simple lambda"
    (assert-equal "|x: i64| -> i64 { x }"
                  (rust-serialize '(R-Lambda ((x i64)) i64 (R-Var x)))))

  (define-test "Lambda with two params"
    (assert-equal "|x: f64, y: f64| -> f64 { (x + y) }"
                  (rust-serialize '(R-Lambda ((x f64) (y f64)) f64 (R-Call + (R-Var x) (R-Var y))))))

  (define-test "Lambda with complex body"
    (assert-equal "|a: f64, b: f64| -> f64 { if (a > b) { a } else { b } }"
                  (rust-serialize '(R-Lambda ((a f64) (b f64)) f64
                                    (R-If (R-Call > (R-Var a) (R-Var b))
                                      (R-Var a)
                                      (R-Var b)))))))

(test-group r-letrec-serialization
  (define-test "Letrec simple recursion"
    (let ([code (rust-serialize '(R-Letrec fact ((n i64)) i64
                                  (R-If (R-Call <= (R-Var n) (R-Literal 1))
                                    (R-Literal 1)
                                    (R-Call * (R-Var n) (R-Call fact (R-Call - (R-Var n) (R-Literal 1)))))
                                  (R-Var fact)))])
      (assert-true (if (and (string-contains code "fn fact(n: i64, __fuel: &mut u64)")
                            (string-contains code "if *__fuel == 0 { return None; }")
                            (string-contains code "*__fuel -= 1")
                            (string-contains code "fact((n - 1), __fuel)?")) #t #f))))

  (define-test "Letrec with two params"
    (let ([code (rust-serialize '(R-Letrec fib ((n i64) (acc i64)) i64
                                  (R-If (R-Call <= (R-Var n) (R-Literal 0))
                                    (R-Var acc)
                                    (R-Call fib (R-Call - (R-Var n) (R-Literal 1))
                                                (R-Call + (R-Var acc) (R-Var n))))
                                  (R-Var fib)))])
      (assert-true (if (and (string-contains code "fn fib(n: i64, acc: i64, __fuel: &mut u64)")
                            (string-contains code "fib((n - 1), (acc + n), __fuel)?")) #t #f)))))

(test-group scheme-ir-fn-fix
  (define-test "Scheme->IR typed lambda"
    (assert-equal '(R-Lambda ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y)))
                  (scheme->rust-ir '(fn ((x i64) (y i64)) i64 (+ x y)))))

  (define-test "Scheme->IR untyped lambda"
    (let ([result (scheme->rust-ir '(fn (x) (+ x 1)))])
      (assert-true (if (and (eq? (car result) 'R-Literal)
                            (string? (cadr result))
                            (string-contains (cadr result) "untyped lambda")) #t #f))))

  (define-test "Scheme->IR typed fix"
    (assert-equal '(R-Letrec fact ((n i64)) i64
                    (R-If (R-Call <= (R-Var n) (R-Literal 1))
                      (R-Literal 1)
                      (R-Call * (R-Var n) (R-Call fact (R-Call - (R-Var n) (R-Literal 1)))))
                    (R-Var fact))
                  (scheme->rust-ir '(fix fact (fn ((n i64)) i64
                                      (if (<= n 1) 1 (* n (fact (- n 1))))))))))

(test-group fuel-cost-closures-recursion
  (define-test "Fuel cost: simple lambda"
    (assert-equal 2  ; 1 (closure) + 1 (add op)
                  (ir-fuel-cost '(R-Lambda ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y))))))

  (define-test "Fuel cost: letrec"
    (assert-equal 5  ; 1 (def) + 3 (body) + 1 (call)
                  (ir-fuel-cost '(R-Letrec fact ((n i64)) i64
                                  (R-If (R-Call <= (R-Var n) (R-Literal 1))
                                    (R-Literal 1)
                                    (R-Call * (R-Var n) (R-Var acc)))
                                  (R-Call fact (R-Literal 5)))))))

(test-group division-in-closures
  (define-test "Lambda body NOT collected (scoping fix)"
    (assert-equal '()
                  (ir-collect-divisors '(R-Lambda ((x i64) (y i64)) i64 (R-Call / (R-Var x) (R-Var y))))))

  (define-test "Letrec body NOT collected (scoping fix)"
    (assert-equal '()
                  (ir-collect-divisors '(R-Letrec div_acc ((n i64)) i64
                                         (R-Call / (R-Literal 100) (R-Var n))
                                         (R-Call div_acc (R-Literal 5))))))

  (define-test "Letrec in-expr IS collected"
    (assert-equal '((R-Var x))
                  (ir-collect-divisors '(R-Letrec f ((n i64)) i64
                                         (R-Var n)
                                         (R-Call / (R-Literal 10) (R-Var x))))))

  (define-test "Contains int var in lambda body"
    (assert-true (ir-contains-integer-var?
                  '(R-Lambda ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))
                  '())))

  (define-test "No int vars in float lambda"
    (assert-false (ir-contains-integer-var?
                   '(R-Lambda ((x f64)) f64 (R-Call + (R-Var x) (R-Literal 1)))
                   '()))))

(test-group qa-edge-cases
  (define-test "R-Call with R-Var operator"
    (assert-equal "f(x, y)"
                  (rust-serialize '(R-Call (R-Var f) (R-Var x) (R-Var y)))))

  (define-test "R-Call with nested R-Var"
    (assert-equal "callback(1, 2)"
                  (rust-serialize '(R-Call (R-Var callback) (R-Literal 1) (R-Literal 2)))))

  (define-test "Lambda with empty params"
    (assert-equal "|| -> i64 { 42 }"
                  (rust-serialize '(R-Lambda () i64 (R-Literal 42)))))

  (define-test "Letrec with empty params (thunk)"
    (let ([code (rust-serialize '(R-Letrec get_value () i64 (R-Literal 42) (R-Var get_value)))])
      (assert-true (if (and (string-contains code "fn get_value(__fuel: &mut u64)")
                            (string-contains code "if *__fuel == 0")) #t #f))))

  (define-test "Scheme->IR empty param lambda"
    (assert-equal '(R-Lambda () i64 (R-Literal 42))
                  (scheme->rust-ir '(fn () i64 42))))

  (define-test "Scheme->IR empty param fix"
    (assert-equal '(R-Letrec counter () i64 (R-Literal 0) (R-Var counter))
                  (scheme->rust-ir '(fix counter (fn () i64 0)))))

  (define-test "Lambda with single param"
    (assert-equal "|x: i64| -> i64 { (x + 1) }"
                  (rust-serialize '(R-Lambda ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))))))

(test-group end-to-end-closure-recursion
  (define-test "Lambda serialization syntax valid"
    (let ([code (rust-serialize '(R-Lambda ((x f64)) f64 (R-Call * (R-Var x) (R-Var x))))])
      (assert-true (if (and (string-contains code "|x: f64|")
                            (string-contains code "-> f64")) #t #f))))

  (define-test "Letrec serialization syntax valid"
    (let ([code (rust-serialize '(R-Letrec sum ((n i64)) i64
                                  (R-If (R-Call <= (R-Var n) (R-Literal 0))
                                    (R-Literal 0)
                                    (R-Call + (R-Var n) (R-Call sum (R-Call - (R-Var n) (R-Literal 1)))))
                                  (R-Call sum (R-Literal 10))))])
      (assert-true (if (and (string-contains code "fn sum(n: i64, __fuel: &mut u64)")
                            (string-contains code "sum(10, &mut __remaining)")) #t #f))))

  (define-test "Compiling factorial with letrec"
    (let ([res (compile-rust-lib "test_factorial"
                 '(R-Fn factorial ((n i64)) i64
                   (R-Letrec fact ((x i64)) i64
                     (R-If (R-Call <= (R-Var x) (R-Literal 1))
                       (R-Literal 1)
                       (R-Call * (R-Var x) (R-Call fact (R-Call - (R-Var x) (R-Literal 1)))))
                     (R-Call fact (R-Var n)))))])
      (cleanup-rust-lib "test_factorial")
      (assert-equal 'ok (car res)))))

(test-group fuel-threading
  (define-test "ir-contains-letrec? positive"
    (assert-true (ir-contains-letrec?
                  '(R-Block
                    (R-Let x (R-Literal 1))
                    (R-Letrec f ((n i64)) i64 (R-Var n) (R-Call f (R-Literal 5)))))))

  (define-test "ir-contains-letrec? negative"
    (assert-false (ir-contains-letrec?
                   '(R-Block
                     (R-Let x (R-Literal 1))
                     (R-Lambda ((n i64)) i64 (R-Var n))))))

  (define-test "R-Letrec generates fuel check"
    (let ([code (rust-serialize '(R-Letrec loop ((x i64)) i64
                                  (R-Call loop (R-Var x))
                                  (R-Call loop (R-Literal 0))))])
      (assert-true (if (and (string-contains code "if *__fuel == 0 { return None; }")
                            (string-contains code "*__fuel -= 1")) #t #f))))

  (define-test "Recursive calls pass __fuel"
    (let ([code (rust-serialize '(R-Letrec rec ((a i64) (b i64)) i64
                                  (R-Call + (R-Var a) (R-Call rec (R-Var b) (R-Literal 0)))
                                  (R-Call rec (R-Literal 1) (R-Literal 2))))])
      (assert-true (if (string-contains code "rec(b, 0, __fuel)?") #t #f))))

  (define-test "emit-fueled-body wraps in Option match"
    (let ([code (emit-fueled-body '(R-Literal 42) 'i64)])
      (assert-true (if (and (string-contains code "Some(v) => v")
                            (string-contains code "None =>")
                            (string-contains code "result.status = 2")) #t #f)))))

(test-group capturing-closures
  (define-test "free-vars: literal"
    (assert-equal '() (free-vars 42 '())))

  (define-test "free-vars: bound var"
    (assert-equal '() (free-vars 'x '(x))))

  (define-test "free-vars: free var"
    (assert-equal '(x) (free-vars 'x '())))

  (define-test "free-vars: mixed"
    (let ([fv (free-vars '(+ x y) '(x))])
      (assert-true (if (and (memq 'y fv) (not (memq 'x fv))) #t #f))))

  (define-test "free-vars: fn binds params"
    (assert-equal '() (free-vars '(fn ((x i64)) i64 x) '())))

  (define-test "free-vars: fn with capture"
    (let ([fv (free-vars '(fn ((x i64)) i64 (+ x y)) '())])
      (assert-true (if (memq 'y fv) #t #f))))

  (define-test "scheme->rust-ir: non-capturing emits R-Lambda"
    (assert-equal 'R-Lambda
                  (car (scheme->rust-ir '(fn ((x i64)) i64 x)))))

  (define-test "scheme->rust-ir: capturing emits R-Closure"
    (assert-equal 'R-Closure
                  (car (scheme->rust-ir '(fn ((x i64)) i64 (+ x y))))))

  (define-test "scheme->rust-ir: R-Closure captures correct vars"
    (let ([ir (scheme->rust-ir '(fn ((x i64)) i64 (+ x y)))])
      (assert-true (if (and (eq? (car ir) 'R-Closure)
                            (memq 'y (cadr ir))) #t #f))))

  (define-test "R-Closure serializes with move"
    (let ([code (rust-serialize '(R-Closure (y) ((x i64)) i64 (R-Call + (R-Var x) (R-Var y))))])
      (assert-true (if (string-contains code "move |x: i64|") #t #f))))

  (define-test "R-Closure fuel cost includes captures"
    (assert-equal 3  ; 1 (closure) + 2 (captures) + 0 (body = just literal)
                  (ir-fuel-cost '(R-Closure (a b) ((x i64)) i64 (R-Literal 0)))))

  (define-test "R-Closure body NOT collected (scoping fix)"
    (assert-equal '()
                  (ir-collect-divisors '(R-Closure (a) ((x i64)) i64 (R-Call / (R-Var a) (R-Var x))))))

  (define-test "Capturing closure in R-Fn"
    (let ([code (rust-emit '(R-Fn make_adder ((n i64)) i64
                              (R-Closure (n) ((x i64)) i64 (R-Call + (R-Var n) (R-Var x)))))])
      (assert-true (if (and (string-contains code "move |x: i64|")
                            (string-contains code "fn make_adder")) #t #f))))

  (define-test "R-Closure with multiple captures"
    (let ([code (rust-serialize '(R-Closure (a b c) ((x i64)) i64
                                  (R-Call + (R-Var a) (R-Call + (R-Var b) (R-Call + (R-Var c) (R-Var x))))))])
      (assert-true (if (and (string-contains code "move")
                            (string-contains code "(a + (b + (c + x)))")) #t #f))))

  (define-test "Nested closures both capture"
    (let ([outer (scheme->rust-ir '(fn ((x i64)) i64
                                     (fn ((y i64)) i64 (+ x y))))])
      (assert-true (if (and (eq? (car outer) 'R-Lambda)
                            (eq? (car (cadddr outer)) 'R-Closure)) #t #f)))))

(test-group function-pointer-parameters
  (define-test "R-FnCall simple"
    (assert-equal "f(x, fuel_remaining)"
                  (rust-serialize '(R-FnCall (R-Var f) (R-Var x)))))

  (define-test "R-FnCall two args"
    (assert-equal "g(a, b, fuel_remaining)"
                  (rust-serialize '(R-FnCall (R-Var g) (R-Var a) (R-Var b)))))

  (define-test "R-FnCall no args"
    (assert-equal "h(fuel_remaining)"
                  (rust-serialize '(R-FnCall (R-Var h)))))

  (define-test "param-type->rust: simple scalar"
    (assert-equal "i64"
                  (param-type->rust 'i64)))

  (define-test "param-type->rust: function type"
    (assert-equal "extern \"C\" fn(i64, u64) -> i64"
                  (param-type->rust '(-> i64 i64))))

  (define-test "param-type->rust: multi-param function"
    (assert-equal "extern \"C\" fn(i64, f64, u64) -> bool"
                  (param-type->rust '(-> i64 f64 bool))))

  (define-test "ir-contains-fn-call?: R-FnCall"
    (assert-true (ir-contains-fn-call? '(R-FnCall (R-Var f) (R-Var x)))))

  (define-test "ir-contains-fn-call?: nested in R-Call"
    (assert-true (ir-contains-fn-call? '(R-Call + (R-FnCall (R-Var f) (R-Var x)) (R-Literal 1)))))

  (define-test "ir-contains-fn-call?: no fn call"
    (assert-false (ir-contains-fn-call? '(R-Call + (R-Var x) (R-Var y)))))

  (define-test "ir-contains-fn-call?: in R-If branch"
    (assert-true (ir-contains-fn-call? '(R-If (R-Var cond)
                                          (R-FnCall (R-Var f) (R-Var x))
                                          (R-Literal 0)))))

  (define-test "rust-emit with fn pointer param emits fuel_remaining"
    (let ([code (rust-emit '(R-Fn apply_fn ((f (-> i64 i64)) (x i64)) i64
                              (R-FnCall (R-Var f) (R-Var x))))])
      (assert-true (if (and (string-contains code "let fuel_remaining = fuel_in - COST")
                            (string-contains code "f(x, fuel_remaining)")) #t #f))))

  (define-test "rust-emit fn pointer param type correct"
    (let ([code (rust-emit '(R-Fn apply_fn ((f (-> i64 i64)) (x i64)) i64
                              (R-FnCall (R-Var f) (R-Var x))))])
      (assert-true (if (string-contains code "f: extern \"C\" fn(i64, u64) -> i64") #t #f))))

  (define-test "R-FnCall in recursive context"
    (let ([code (rust-serialize-with-fuel
                 '(R-FnCall (R-Var callback) (R-Var x))
                 'rec)])
      (assert-true (if (string-contains code "callback(x, fuel_remaining)") #t #f))))

  (define-test "scheme->rust-ir: (call f x) emits R-FnCall"
    (assert-equal 'R-FnCall
                  (car (scheme->rust-ir '(call f x)))))

  (define-test "scheme->rust-ir: (call f x y) emits R-FnCall with args"
    (let ([ir (scheme->rust-ir '(call g a b))])
      (assert-true (if (and (eq? (car ir) 'R-FnCall)
                            (equal? (cadr ir) '(R-Var g))
                            (equal? (caddr ir) '(R-Var a))
                            (equal? (cadddr ir) '(R-Var b))) #t #f)))))

;;; ============================================================================
;;; Run all tests
;;; ============================================================================

(run-all-tests-and-exit)
