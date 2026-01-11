;;; Test for Scheme-to-Rust Codegen Serializer

(load "core/base/prelude.ss")
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
(test "Simple function (i64)"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn add_one(x: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + 1);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn add_one ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 1))

(test-section "Rust IR Serialization - Comparison Operators")
(test "Less than" "(x < y)" (rust-serialize '(R-Call lt? (R-Var x) (R-Var y))))
(test "Less or equal" "(x <= y)" (rust-serialize '(R-Call le? (R-Var x) (R-Var y))))
(test "Greater than" "(x > y)" (rust-serialize '(R-Call gt? (R-Var x) (R-Var y))))
(test "Greater or equal" "(x >= y)" (rust-serialize '(R-Call ge? (R-Var x) (R-Var y))))
(test "Equal" "(x == y)" (rust-serialize '(R-Call eq? (R-Var x) (R-Var y))))

(test-section "Rust IR Serialization - Logical Operators")
(test "And" "(x && y)" (rust-serialize '(R-Call and (R-Var x) (R-Var y))))
(test "Or" "(x || y)" (rust-serialize '(R-Call or (R-Var x) (R-Var y))))
(test "Not" "(!x)" (rust-serialize '(R-Call not (R-Var x))))

(test-section "Rust IR Serialization - Bitwise Operators")
(test "Bitand" "(x & y)" (rust-serialize '(R-Call bitand (R-Var x) (R-Var y))))
(test "Bitor" "(x | y)" (rust-serialize '(R-Call bitor (R-Var x) (R-Var y))))
(test "Bitxor" "(x ^ y)" (rust-serialize '(R-Call bitxor (R-Var x) (R-Var y))))
(test "Shift left" "(x << y)" (rust-serialize '(R-Call shl (R-Var x) (R-Var y))))
(test "Shift right" "(x >> y)" (rust-serialize '(R-Call shr (R-Var x) (R-Var y))))

(test-section "Rust IR Serialization - Math Methods")
(test "Abs" "(x.abs())" (rust-serialize '(R-Call abs (R-Var x))))
(test "Sqrt" "(x.sqrt())" (rust-serialize '(R-Call sqrt (R-Var x))))
(test "Sin" "(x.sin())" (rust-serialize '(R-Call sin (R-Var x))))
(test "Cos" "(x.cos())" (rust-serialize '(R-Call cos (R-Var x))))
(test "Tan" "(x.tan())" (rust-serialize '(R-Call tan (R-Var x))))
(test "Log (ln)" "(x.ln())" (rust-serialize '(R-Call log (R-Var x))))
(test "Floor" "(x.floor())" (rust-serialize '(R-Call floor (R-Var x))))
(test "Ceiling" "(x.ceil())" (rust-serialize '(R-Call ceiling (R-Var x))))
(test "Neg" "(-x)" (rust-serialize '(R-Call neg (R-Var x))))
(test "Sq (powi)" "(x.powi(2))" (rust-serialize '(R-Call sq (R-Var x))))
(test "Exp" "(x.exp())" (rust-serialize '(R-Call exp (R-Var x))))
(test "Asin" "(x.asin())" (rust-serialize '(R-Call asin (R-Var x))))
(test "Acos" "(x.acos())" (rust-serialize '(R-Call acos (R-Var x))))
(test "Atan" "(x.atan())" (rust-serialize '(R-Call atan (R-Var x))))
(test "Sinh" "(x.sinh())" (rust-serialize '(R-Call sinh (R-Var x))))
(test "Cosh" "(x.cosh())" (rust-serialize '(R-Call cosh (R-Var x))))
(test "Tanh" "(x.tanh())" (rust-serialize '(R-Call tanh (R-Var x))))
(test "Expt (powf)" "(x.powf(y))" (rust-serialize '(R-Call expt (R-Var x) (R-Var y))))
(test "Pow (powf)" "(x.powf(y))" (rust-serialize '(R-Call pow (R-Var x) (R-Var y))))
(test "Atan2" "(y.atan2(x))" (rust-serialize '(R-Call atan2 (R-Var y) (R-Var x))))
(test "Hypot" "(x.hypot(y))" (rust-serialize '(R-Call hypot (R-Var x) (R-Var y))))
(test "Min" "(x.min(y))" (rust-serialize '(R-Call min (R-Var x) (R-Var y))))
(test "Max" "(x.max(y))" (rust-serialize '(R-Call max (R-Var x) (R-Var y))))
(test "Truncate" "(x.trunc())" (rust-serialize '(R-Call truncate (R-Var x))))

(test-section "Rust IR Serialization - Block")
(test "Empty block" "{}" (rust-serialize '(R-Block)))
(test "Single expr block" "{ x }" (rust-serialize '(R-Block (R-Var x))))
(test "Let + expr block" "{ let x = 42; x }" (rust-serialize '(R-Block (R-Let x (R-Literal 42)) (R-Var x))))

(test-section "Scheme to Rust IR Translation")
(test "Literal number" '(R-Literal 42) (scheme->rust-ir 42))
(test "Literal bool" '(R-Literal #t) (scheme->rust-ir #t))
(test "Variable" '(R-Var x) (scheme->rust-ir 'x))
(test "Prim add" '(R-Call add (R-Literal 1) (R-Literal 2)) (scheme->rust-ir '(prim 'add 1 2)))
(test "Prim lt?" '(R-Call lt? (R-Var x) (R-Var y)) (scheme->rust-ir '(prim 'lt? x y)))
(test "Direct +" '(R-Call + (R-Literal 1) (R-Literal 2)) (scheme->rust-ir '(+ 1 2)))
(test "If expr" '(R-If (R-Var p) (R-Literal 1) (R-Literal 0)) (scheme->rust-ir '(if p 1 0)))
(test "Let binding" '(R-Block (R-Let x (R-Literal 42)) (R-Var x)) (scheme->rust-ir '(let ((x 42)) x)))
(test "Nested" '(R-Call lt? (R-Call abs (R-Var x)) (R-Call abs (R-Var y)))
      (scheme->rust-ir '(prim 'lt? (abs x) (abs y))))

(test-section "End-to-End Compilation")
(load "core/lang/rust-compile.ss")

(display "  Compiling basic add... ")
(let ([res (compile-rust-lib "test_codegen"
                             '(R-Fn add_one ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))
                             1)])
     (if (eq? (car res) 'ok)
         (begin
          (display "✓\n")
          (cleanup-rust-lib "test_codegen"))
         (begin
          (display "✗\n")
          (display res)
          (newline))))

;; Note: comparison returns bool which can't cast to f64 directly.
;; Use if-expression to convert to numeric result.
;; Proper type handling tracked in fold-4s4q.
(display "  Compiling with math ops... ")
(let ([res (compile-rust-lib "test_math"
                             '(R-Fn sum_abs ((x f64) (y f64)) f64
                               (R-Call + (R-Call abs (R-Var x)) (R-Call abs (R-Var y))))
                             2)])
     (if (eq? (car res) 'ok)
         (begin
          (display "✓\n")
          (cleanup-rust-lib "test_math"))
         (begin
          (display "✗\n")
          (display res)
          (newline))))

(display "  Compiling with bitwise... ")
(let ([res (compile-rust-lib "test_bitwise"
                             '(R-Fn mask_and_shift ((x i64) (mask i64) (shift i64)) i64
                               (R-Call shl (R-Call bitand (R-Var x) (R-Var mask)) (R-Var shift)))
                             3)])
     (if (eq? (car res) 'ok)
         (begin
          (display "✓\n")
          (cleanup-rust-lib "test_bitwise"))
         (begin
          (display "✗\n")
          (display res)
          (newline))))

(test-section "Fuel Cost Computation")
(test "Literal cost" 0 (ir-fuel-cost '(R-Literal 42)))
(test "Variable cost" 0 (ir-fuel-cost '(R-Var x)))
(test "Simple add cost" 1 (ir-fuel-cost '(R-Call + (R-Literal 1) (R-Literal 2))))
(test "Add with vars cost" 1 (ir-fuel-cost '(R-Call + (R-Var x) (R-Var y))))
(test "Division cost" 2 (ir-fuel-cost '(R-Call / (R-Var x) (R-Var y))))
(test "Nested ops cost" 2 (ir-fuel-cost '(R-Call + (R-Call * (R-Var x) (R-Var y)) (R-Var z))))  ; 1 (add) + 1 (mul) = 2
(test "If expression cost" 1 (ir-fuel-cost '(R-If (R-Var p) (R-Literal 1) (R-Literal 0))))  ; 1 (if) + 0 (var) + max(0,0) = 1
(test "If with branches cost (max)" 4  ; 1 (if) + 1 (cond:lt) + max(2, 1) = 4
      (ir-fuel-cost '(R-If (R-Call lt? (R-Var x) (R-Var y))
                      (R-Call / (R-Var a) (R-Var b))
                      (R-Literal 0))))
(test "Block cost" 1 (ir-fuel-cost '(R-Block (R-Let x (R-Call * (R-Var a) (R-Var b))) (R-Var x))))  ; 1 (mul in let) + 0 (var) = 1
(test "Trig function cost" 3 (ir-fuel-cost '(R-Call sin (R-Var x))))
(test "Sqrt cost" 2 (ir-fuel-cost '(R-Call sqrt (R-Var x))))
(test "Op-fuel-cost: add" 1 (op-fuel-cost 'add))
(test "Op-fuel-cost: div" 2 (op-fuel-cost 'div))
(test "Op-fuel-cost: sin" 3 (op-fuel-cost 'sin))
(test "Op-fuel-cost: sq" 1 (op-fuel-cost 'sq))
(test "Op-fuel-cost: asin" 3 (op-fuel-cost 'asin))
(test "Op-fuel-cost: sinh" 3 (op-fuel-cost 'sinh))
(test "Op-fuel-cost: exp" 3 (op-fuel-cost 'exp))
(test "Op-fuel-cost: atan2" 3 (op-fuel-cost 'atan2))
(test "Op-fuel-cost: hypot" 3 (op-fuel-cost 'hypot))
(test "Op-fuel-cost: min" 1 (op-fuel-cost 'min))
(test "Op-fuel-cost: max" 1 (op-fuel-cost 'max))
(test "Op-fuel-cost: truncate" 1 (op-fuel-cost 'truncate))
(test "Op-fuel-cost: remainder" 2 (op-fuel-cost 'remainder))
(test "Op-fuel-cost: unknown" 1 (op-fuel-cost 'unknown-op))

(test-section "Autodiff Gradient Formulas")
(test "Gradient: add" '(1 1) (op-local-gradient 'add))
(test "Gradient: sub" '(1 -1) (op-local-gradient 'sub))
(test "Gradient: mul" '(b a) (op-local-gradient 'mul))
(test "Gradient: div" '((/ 1 b) (/ (- a) (* b b))) (op-local-gradient 'div))
(test "Gradient: neg" '(-1) (op-local-gradient 'neg))
(test "Gradient: sqrt" '((/ 1 (* 2 (sqrt a)))) (op-local-gradient 'sqrt))
(test "Gradient: sin" '((cos a)) (op-local-gradient 'sin))
(test "Gradient: cos" '((- (sin a))) (op-local-gradient 'cos))
(test "Gradient: exp" '((exp a)) (op-local-gradient 'exp))
(test "Gradient: log" '((/ 1 a)) (op-local-gradient 'log))
(test "Gradient: sq" '((* 2 a)) (op-local-gradient 'sq))
(test "Gradient: sinh" '((cosh a)) (op-local-gradient 'sinh))
(test "Gradient: cosh" '((sinh a)) (op-local-gradient 'cosh))
(test "Gradient: tanh" '((/ 1 (* (cosh a) (cosh a)))) (op-local-gradient 'tanh))
(test "Gradient: atan2" '((/ b (+ (* a a) (* b b))) (/ (- a) (+ (* a a) (* b b)))) (op-local-gradient 'atan2))
(test "Gradient: hypot" '((/ a (hypot a b)) (/ b (hypot a b))) (op-local-gradient 'hypot))
(test "Gradient: min" '((if (<= a b) 1 0) (if (<= a b) 0 1)) (op-local-gradient 'min))
(test "Gradient: max" '((if (>= a b) 1 0) (if (>= a b) 0 1)) (op-local-gradient 'max))
(test "Gradient: truncate" '(0) (op-local-gradient 'truncate))
(test "Gradient: non-diff lt?" #f (op-local-gradient 'lt?))
(test "Gradient: non-diff bitand" #f (op-local-gradient 'bitand))
(test "Differentiable: add" #t (op-differentiable? 'add))
(test "Differentiable: lt?" #f (op-differentiable? 'lt?))

(test-section "Rust Emit with Auto-Computed Cost")
(test "Auto-computed cost (i64 add)"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn auto_add(x: i64, y: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + y);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn auto_add ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y)))))

(test "Auto-computed cost (f64 nested ops)"
      ;; Cost should be: 1 (outer +) + 1 (inner *) = 2
      "#[repr(C)] pub struct F64Result { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn nested_ops(x: f64, y: f64, z: f64, fuel_in: u64, out: *mut F64Result) {\n    if (out as *const F64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = ((x * y) + z);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn nested_ops ((x f64) (y f64) (z f64)) f64 (R-Call + (R-Call * (R-Var x) (R-Var y)) (R-Var z)))))

(test "Explicit cost override still works"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn override_cost(x: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 999;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + 1);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn override_cost ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 999))

(test-section "Type-Safe Code Generation (M1: fold-4s4q)")

;; Bool-returning functions use BoolResult with proper value assignment
(test "Bool function uses BoolResult"
      "#[repr(C)] pub struct BoolResult { pub status: u8, pub value: u8, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn is_positive(x: i64, fuel_in: u64, out: *mut BoolResult) {\n    if (out as *const BoolResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x > 0);\n    result.status = 1;\n    result.value = if val { 1 } else { 0 };\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn is_positive ((x i64)) bool (R-Call gt? (R-Var x) (R-Literal 0)))))

;; u64-returning functions use U64Result
(test "U64 function uses U64Result"
      "#[repr(C)] pub struct U64Result { pub status: u8, pub value: u64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn double_unsigned(x: u64, fuel_in: u64, out: *mut U64Result) {\n    if (out as *const U64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x * 2);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn double_unsigned ((x u64)) u64 (R-Call * (R-Var x) (R-Literal 2)))))

;; Typed literals serialize with proper suffixes
(test-section "Typed Literals (M1: fold-4s4q)")

(test "Typed i64 literal" "42i64" (rust-serialize '(R-Literal 42 i64)))
(test "Typed f64 literal (int)" "42.0_f64" (rust-serialize '(R-Literal 42 f64)))
(test "Typed f64 literal (float)" "3.14_f64" (rust-serialize '(R-Literal 3.14 f64)))
(test "Typed u64 literal" "100u64" (rust-serialize '(R-Literal 100 u64)))
(test "Typed i32 literal" "10i32" (rust-serialize '(R-Literal 10 i32)))
(test "Typed f32 literal" "2.5_f32" (rust-serialize '(R-Literal 2.5 f32)))

;; Untyped literals use heuristics
(test "Untyped int literal" "42" (rust-serialize '(R-Literal 42)))
(test "Untyped float literal" "3.14_f64" (rust-serialize '(R-Literal 3.14)))
(test "Untyped bool true" "true" (rust-serialize '(R-Literal #t)))
(test "Untyped bool false" "false" (rust-serialize '(R-Literal #f)))

;; Ret-type helper functions
(test-section "Type Mapping Helpers (M1: fold-4s4q)")

(test "ret-type->result-struct i64" "I64Result" (ret-type->result-struct 'i64))
(test "ret-type->result-struct f64" "F64Result" (ret-type->result-struct 'f64))
(test "ret-type->result-struct bool" "BoolResult" (ret-type->result-struct 'bool))
(test "ret-type->result-struct u64" "U64Result" (ret-type->result-struct 'u64))
(test "ret-type->result-struct i32" "I32Result" (ret-type->result-struct 'i32))
(test "ret-type->result-struct f32" "F32Result" (ret-type->result-struct 'f32))
(test "ret-type->result-struct unknown" "TestResult" (ret-type->result-struct 'unknown))

(test "ret-type->value-assignment i64" "    result.value = val;\n" (ret-type->value-assignment 'i64))
(test "ret-type->value-assignment f64" "    result.value = val;\n" (ret-type->value-assignment 'f64))
(test "ret-type->value-assignment bool" "    result.value = if val { 1 } else { 0 };\n" (ret-type->value-assignment 'bool))
(test "ret-type->value-assignment i32" "    result.value = val;\n" (ret-type->value-assignment 'i32))
(test "ret-type->value-assignment f32" "    result.value = val;\n" (ret-type->value-assignment 'f32))
(test "ret-type->value-assignment unknown" "    result.value = val as f64;\n" (ret-type->value-assignment 'unknown))

;; i32/f32 function emit tests
(test "I32 function uses I32Result"
      "#[repr(C)] pub struct I32Result { pub status: u8, pub value: i32, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn add_small(x: i32, y: i32, fuel_in: u64, out: *mut I32Result) {\n    if (out as *const I32Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + y);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn add_small ((x i32) (y i32)) i32 (R-Call + (R-Var x) (R-Var y)))))

(test "F32 function uses F32Result"
      "#[repr(C)] pub struct F32Result { pub status: u8, pub value: f32, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn mul_float(x: f32, y: f32, fuel_in: u64, out: *mut F32Result) {\n    if (out as *const F32Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x * y);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn mul_float ((x f32) (y f32)) f32 (R-Call * (R-Var x) (R-Var y)))))

(test-section "Division-by-Zero Protection (M2: fold-jppr)")

;; Test ir-collect-divisors
(test "Collect divisors: simple div"
      '((R-Var y))
      (ir-collect-divisors '(R-Call / (R-Var x) (R-Var y))))

(test "Collect divisors: simple mod"
      '((R-Var y))
      (ir-collect-divisors '(R-Call mod (R-Var x) (R-Var y))))

(test "Collect divisors: nested"
      '((R-Var b) (R-Var d))
      (ir-collect-divisors '(R-Call + (R-Call / (R-Var a) (R-Var b))
                             (R-Call % (R-Var c) (R-Var d)))))

(test "Collect divisors: no division"
      '()
      (ir-collect-divisors '(R-Call + (R-Var x) (R-Var y))))

(test "Collect divisors: literal divisor"
      '((R-Literal 2))
      (ir-collect-divisors '(R-Call / (R-Var x) (R-Literal 2))))

;; Test integer-type?
(test "integer-type? i64" #t (and (integer-type? 'i64) #t))
(test "integer-type? i32" #t (and (integer-type? 'i32) #t))
(test "integer-type? u64" #t (and (integer-type? 'u64) #t))
(test "integer-type? f64" #f (integer-type? 'f64))
(test "integer-type? f32" #f (integer-type? 'f32))
(test "integer-type? bool" #f (integer-type? 'bool))

;; Test ir-divisor->guard
(test "Guard for i64 divisor var"
      "y == 0"
      (ir-divisor->guard '(R-Var y) '((x i64) (y i64))))

(test "No guard for f64 divisor var"
      #f
      (ir-divisor->guard '(R-Var y) '((x f64) (y f64))))

(test "Guard for zero literal"
      "true /* constant zero divisor */"
      (ir-divisor->guard '(R-Literal 0) '()))

(test "No guard for non-zero literal"
      #f
      (ir-divisor->guard '(R-Literal 5) '()))

;; Test emit-divisor-guards
(test "Guards for i64 division"
      "    // Division-by-zero protection\n    if y == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n"
      (emit-divisor-guards '((R-Var y)) '((x i64) (y i64))))

(test "No guards for f64 division"
      ""
      (emit-divisor-guards '((R-Var y)) '((x f64) (y f64))))

;; Test full function with i64 division
(test "I64 division emits guard"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn safe_div(x: i64, y: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    // Division-by-zero protection\n    if y == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n    let val = (x / y);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn safe_div ((x i64) (y i64)) i64 (R-Call / (R-Var x) (R-Var y)))))

;; Test f64 division has no guard (Rust handles gracefully)
(test "F64 division no guard (Inf/NaN)"
      "#[repr(C)] pub struct F64Result { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn float_div(x: f64, y: f64, fuel_in: u64, out: *mut F64Result) {\n    if (out as *const F64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x / y);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn float_div ((x f64) (y f64)) f64 (R-Call / (R-Var x) (R-Var y)))))

;; Test ir-contains-integer-var? (QA fix)
(test "Contains int var: simple" #t
      (ir-contains-integer-var? '(R-Var x) '((x i64))))
(test "Contains int var: float" #f
      (ir-contains-integer-var? '(R-Var x) '((x f64))))
(test "Contains int var: in call" #t
      (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x i64) (y i64))))
(test "Contains int var: mixed" #t
      (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x i64) (y f64))))
(test "Contains int var: all float" #f
      (ir-contains-integer-var? '(R-Call + (R-Var x) (R-Var y)) '((x f64) (y f64))))
(test "Contains int var: literal only" #f
      (ir-contains-integer-var? '(R-Literal 5) '()))

;; Test complex divisor expressions get guarded (QA fix)
(test "Guard for complex int expression"
      "(y + z) == 0"
      (ir-divisor->guard '(R-Call + (R-Var y) (R-Var z)) '((x i64) (y i64) (z i64))))
(test "No guard for complex float expression"
      #f
      (ir-divisor->guard '(R-Call + (R-Var y) (R-Var z)) '((x f64) (y f64) (z f64))))

;; Test full function with complex integer divisor
(test "Complex i64 divisor emits guard"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn div_by_sum(x: i64, y: i64, z: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 3;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    // Division-by-zero protection\n    if (y + z) == 0 {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n    let val = (x / (y + z));\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn div_by_sum ((x i64) (y i64) (z i64)) i64
                   (R-Call / (R-Var x) (R-Call + (R-Var y) (R-Var z))))))

;;; ============================================================
;;; M3: Variadic Primitives Tests
;;; ============================================================

;; Test variadic infix serialization
(test "Variadic add (3 args)"
      "((a + b) + c)"
      (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic add (4 args)"
      "(((a + b) + c) + d)"
      (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d))))

(test "Variadic add (5 args)"
      "((((a + b) + c) + d) + e)"
      (rust-serialize '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e))))

(test "Variadic mul (3 args)"
      "((x * y) * z)"
      (rust-serialize '(R-Call * (R-Var x) (R-Var y) (R-Var z))))

(test "Variadic and (3 args)"
      "((a && b) && c)"
      (rust-serialize '(R-Call and (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic or (4 args)"
      "(((a || b) || c) || d)"
      (rust-serialize '(R-Call or (R-Var a) (R-Var b) (R-Var c) (R-Var d))))

(test "Variadic bitand (3 args)"
      "((a & b) & c)"
      (rust-serialize '(R-Call bitand (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic bitor (3 args)"
      "((a | b) | c)"
      (rust-serialize '(R-Call bitor (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic bitxor (3 args)"
      "((a ^ b) ^ c)"
      (rust-serialize '(R-Call bitxor (R-Var a) (R-Var b) (R-Var c))))

;; Test variadic min/max
(test "Variadic min (3 args)"
      "(a.min(b).min(c))"
      (rust-serialize '(R-Call min (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic min (4 args)"
      "(a.min(b).min(c).min(d))"
      (rust-serialize '(R-Call min (R-Var a) (R-Var b) (R-Var c) (R-Var d))))

(test "Variadic max (3 args)"
      "(a.max(b).max(c))"
      (rust-serialize '(R-Call max (R-Var a) (R-Var b) (R-Var c))))

(test "Variadic max (4 args)"
      "(a.max(b).max(c).max(d))"
      (rust-serialize '(R-Call max (R-Var a) (R-Var b) (R-Var c) (R-Var d))))

;; Test variadic with literals
(test "Variadic add with literals"
      "((1 + 2) + 3)"
      (rust-serialize '(R-Call + (R-Literal 1) (R-Literal 2) (R-Literal 3))))

(test "Variadic mul with typed literals"
      "((1i64 * 2i64) * 3i64)"
      (rust-serialize '(R-Call * (R-Literal 1 i64) (R-Literal 2 i64) (R-Literal 3 i64))))

;; Test fuel cost for variadic ops
(test "Fuel cost: binary add (2 args)"
      1
      (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b))))

(test "Fuel cost: variadic add (3 args)"
      2
      (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b) (R-Var c))))

(test "Fuel cost: variadic add (5 args)"
      4
      (ir-fuel-cost '(R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e))))

(test "Fuel cost: variadic min (4 args)"
      3
      (ir-fuel-cost '(R-Call min (R-Var a) (R-Var b) (R-Var c) (R-Var d))))

;; Test scheme->rust-ir preserves variadic
(test "Scheme->IR variadic add"
      '(R-Call + (R-Literal 1) (R-Literal 2) (R-Literal 3) (R-Literal 4) (R-Literal 5))
      (scheme->rust-ir '(+ 1 2 3 4 5)))

(test "Scheme->IR variadic mul"
      '(R-Call * (R-Var a) (R-Var b) (R-Var c))
      (scheme->rust-ir '(* a b c)))

;; Test full function with variadic op
(test "Full variadic add function"
      "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn sum5(a: i64, b: i64, c: i64, d: i64, e: i64, fuel_in: u64, out: *mut I64Result) {\n    if (out as *const I64Result).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 4;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = ((((a + b) + c) + d) + e);\n    result.status = 1;\n    result.value = val;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn sum5 ((a i64) (b i64) (c i64) (d i64) (e i64)) i64
                   (R-Call + (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e)))))

(newline)
