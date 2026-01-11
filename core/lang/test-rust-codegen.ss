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

;;; ============================================================
;;; M3 QA: Edge Cases and Safety Tests
;;; ============================================================

;; Test 0-arg identity values
(test "0-arg add identity"
      "0"
      (rust-serialize '(R-Call +)))

(test "0-arg mul identity"
      "1"
      (rust-serialize '(R-Call *)))

(test "0-arg and identity"
      "true"
      (rust-serialize '(R-Call and)))

(test "0-arg or identity"
      "false"
      (rust-serialize '(R-Call or)))

;; Test 1-arg pass-through
(test "1-arg add pass-through"
      "x"
      (rust-serialize '(R-Call + (R-Var x))))

(test "1-arg mul pass-through"
      "42"
      (rust-serialize '(R-Call * (R-Literal 42))))

(test "1-arg and pass-through"
      "flag"
      (rust-serialize '(R-Call and (R-Var flag))))

(test "1-arg or pass-through"
      "cond"
      (rust-serialize '(R-Call or (R-Var cond))))

(test "1-arg bitand pass-through"
      "bits"
      (rust-serialize '(R-Call bitand (R-Var bits))))

;; Test comparisons are NOT variadic (fall through to default)
;; These produce invalid Rust but that's intentional - comparisons shouldn't be variadic
(test "Comparison < stays binary"
      "(a < b)"
      (rust-serialize '(R-Call < (R-Var a) (R-Var b))))

;; 3-arg comparison falls through to default (function call syntax)
;; This is expected - variadic comparisons are not supported
(test "Comparison < 3-arg fallback"
      "<(a, b, c)"
      (rust-serialize '(R-Call < (R-Var a) (R-Var b) (R-Var c))))

;; Test shifts are NOT variadic (binary only)
(test "Shift << stays binary"
      "(a << b)"
      (rust-serialize '(R-Call shl (R-Var a) (R-Var b))))

;; Test sub/div are NOT variadic (binary only)
(test "Sub - stays binary"
      "(a - b)"
      (rust-serialize '(R-Call - (R-Var a) (R-Var b))))

(test "Div / stays binary"
      "(a / b)"
      (rust-serialize '(R-Call / (R-Var a) (R-Var b))))

;; Test fuel cost for edge cases
(test "Fuel cost: 0-arg add"
      0
      (ir-fuel-cost '(R-Call +)))

(test "Fuel cost: 1-arg mul"
      0
      (ir-fuel-cost '(R-Call * (R-Var x))))

(test "Fuel cost: 1-arg sqrt (not variadic-safe)"
      2
      (ir-fuel-cost '(R-Call sqrt (R-Var x))))

(test "Fuel cost: 1-arg sin (not variadic-safe)"
      3
      (ir-fuel-cost '(R-Call sin (R-Var x))))

;; Test variadic-safe-op? helper
(test "variadic-safe: add" #t (and (variadic-safe-op? '+) #t))
(test "variadic-safe: mul" #t (and (variadic-safe-op? '*) #t))
(test "variadic-safe: and" #t (and (variadic-safe-op? 'and) #t))
(test "variadic-safe: or" #t (and (variadic-safe-op? 'or) #t))
(test "variadic-safe: sub (not)" #f (variadic-safe-op? '-))
(test "variadic-safe: div (not)" #f (variadic-safe-op? '/))
(test "variadic-safe: lt (not)" #f (variadic-safe-op? '<))
(test "variadic-safe: eq (not)" #f (variadic-safe-op? 'eq?))
(test "variadic-safe: shl (not)" #f (variadic-safe-op? 'shl))

;;; ============================================================
;;; M4: Crate Integration Tests (rust-emit-module)
;;; ============================================================

;; Test rust-emit-module generates crate imports instead of inline structs
(test "rust-emit-module uses crate import"
      #t
      (string-contains
       (rust-emit-module '(R-Fn test_fn ((x i64)) i64 (R-Var x)))
       "use crate::{I64Result}"))

(test "rust-emit-module no inline struct"
      #f
      (string-contains
       (rust-emit-module '(R-Fn test_fn ((x i64)) i64 (R-Var x)))
       "#[repr(C)]"))

(test "rust-emit-module prefixes fn with fold_"
      #t
      (string-contains
       (rust-emit-module '(R-Fn my_func ((x i64)) i64 (R-Var x)))
       "pub extern \"C\" fn fold_my_func"))

(test "rust-emit-module includes header comment"
      #t
      (string-contains
       (rust-emit-module '(R-Fn test ((x i64)) i64 (R-Var x)))
       "//! Auto-generated by The Fold codegen"))

;; Test different result types in module context
(test "rust-emit-module F64Result import"
      #t
      (string-contains
       (rust-emit-module '(R-Fn float_fn ((x f64)) f64 (R-Var x)))
       "use crate::{F64Result}"))

(test "rust-emit-module BoolResult import"
      #t
      (string-contains
       (rust-emit-module '(R-Fn bool_fn ((x i64) (y i64)) bool (R-Call < (R-Var x) (R-Var y))))
       "use crate::{BoolResult}"))

;; Helper for tests
(define (string-contains haystack needle)
  (let* ([h-len (string-length haystack)]
         [n-len (string-length needle)])
        (let loop ([i 0])
             (cond
              [(> (+ i n-len) h-len) #f]
              [(string=? (substring haystack i (+ i n-len)) needle) #t]
              [else (loop (+ i 1))]))))

;;; ============================================================
;;; M4 QA: Identifier Sanitization Tests
;;; ============================================================

;; Test sanitize-rust-ident basic functionality
(test "sanitize: normal name"
      "my_func"
      (sanitize-rust-ident "my_func"))

(test "sanitize: hyphenated name"
      "foo_bar"
      (sanitize-rust-ident "foo-bar"))

(test "sanitize: special chars"
      "set_"
      (sanitize-rust-ident "set!"))

(test "sanitize: uppercase to lower"
      "myclass"
      (sanitize-rust-ident "MyClass"))

;; Test leading number handling
(test "sanitize: leading number"
      "m_123test"
      (sanitize-rust-ident "123test"))

(test "sanitize: all numbers"
      "m_42"
      (sanitize-rust-ident "42"))

;; Test empty string handling
(test "sanitize: empty string"
      "unnamed"
      (sanitize-rust-ident ""))

;; Test Rust keyword handling
(test "sanitize: keyword fn"
      "m_fn"
      (sanitize-rust-ident "fn"))

(test "sanitize: keyword let"
      "m_let"
      (sanitize-rust-ident "let"))

(test "sanitize: keyword if"
      "m_if"
      (sanitize-rust-ident "if"))

(test "sanitize: keyword struct"
      "m_struct"
      (sanitize-rust-ident "struct"))

(test "sanitize: keyword impl"
      "m_impl"
      (sanitize-rust-ident "impl"))

;; Test rust-emit-module uses sanitized names
(test "rust-emit-module sanitizes hyphenated name"
      #t
      (string-contains
       (rust-emit-module '(R-Fn foo-bar ((x i64)) i64 (R-Var x)))
       "fn fold_foo_bar"))

(test "rust-emit-module sanitizes keyword name"
      #t
      (string-contains
       (rust-emit-module '(R-Fn fn ((x i64)) i64 (R-Var x)))
       "fn fold_m_fn"))

;;; ============================================================
;;; M5: Closure and Recursion Support (fold-49ht)
;;; ============================================================

(test-section "R-Lambda Serialization")

;; Simple lambda with one parameter
(test "Simple lambda"
      "|x: i64| -> i64 { x }"
      (rust-serialize '(R-Lambda ((x i64)) i64 (R-Var x))))

;; Lambda with multiple parameters
(test "Lambda with two params"
      "|x: f64, y: f64| -> f64 { (x + y) }"
      (rust-serialize '(R-Lambda ((x f64) (y f64)) f64 (R-Call + (R-Var x) (R-Var y)))))

;; Lambda with complex body
(test "Lambda with complex body"
      "|a: f64, b: f64| -> f64 { if (a > b) { a } else { b } }"
      (rust-serialize '(R-Lambda ((a f64) (b f64)) f64
                        (R-If (R-Call > (R-Var a) (R-Var b))
                          (R-Var a)
                          (R-Var b)))))

(test-section "R-Letrec Serialization (with fuel threading)")

;; Simple recursive function (factorial pattern) - now with fuel
(test "Letrec simple recursion"
      #t
      ;; Check key parts of fuel-aware output
      (let ([code (rust-serialize '(R-Letrec fact ((n i64)) i64
                                    (R-If (R-Call <= (R-Var n) (R-Literal 1))
                                      (R-Literal 1)
                                      (R-Call * (R-Var n) (R-Call fact (R-Call - (R-Var n) (R-Literal 1)))))
                                    (R-Var fact)))])
           (and (string-contains code "fn fact(n: i64, __fuel: &mut u64)")
                (string-contains code "if *__fuel == 0 { panic!")
                (string-contains code "*__fuel -= 1")
                (string-contains code "fact((n - 1), __fuel)"))))

;; Recursive function with two params (fibonacci-style) - now with fuel
(test "Letrec with two params"
      #t
      (let ([code (rust-serialize '(R-Letrec fib ((n i64) (acc i64)) i64
                                    (R-If (R-Call <= (R-Var n) (R-Literal 0))
                                      (R-Var acc)
                                      (R-Call fib (R-Call - (R-Var n) (R-Literal 1))
                                                  (R-Call + (R-Var acc) (R-Var n))))
                                    (R-Var fib)))])
           (and (string-contains code "fn fib(n: i64, acc: i64, __fuel: &mut u64)")
                (string-contains code "fib((n - 1), (acc + n), __fuel)"))))

(test-section "Scheme->IR for fn and fix")

;; Typed lambda translation
(test "Scheme->IR typed lambda"
      '(R-Lambda ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y)))
      (scheme->rust-ir '(fn ((x i64) (y i64)) i64 (+ x y))))

;; Untyped lambda should produce error comment (R-Literal with string)
(test "Scheme->IR untyped lambda"
      #t
      (let ([result (scheme->rust-ir '(fn (x) (+ x 1)))])
           (and (eq? (car result) 'R-Literal)
                (string? (cadr result))
                (string-contains (cadr result) "untyped lambda"))))

;; Typed fix translation
(test "Scheme->IR typed fix"
      '(R-Letrec fact ((n i64)) i64
        (R-If (R-Call <= (R-Var n) (R-Literal 1))
          (R-Literal 1)
          (R-Call * (R-Var n) (R-Call fact (R-Call - (R-Var n) (R-Literal 1)))))
        (R-Var fact))
      (scheme->rust-ir '(fix fact (fn ((n i64)) i64
                          (if (<= n 1) 1 (* n (fact (- n 1))))))))

(test-section "Fuel Cost for Closures/Recursion")

;; Lambda cost = 1 (creation) + body cost
(test "Fuel cost: simple lambda"
      2  ; 1 (closure) + 1 (add op)
      (ir-fuel-cost '(R-Lambda ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y)))))

;; Letrec cost = 1 (def) + body cost + in-expr cost
(test "Fuel cost: letrec"
      5  ; 1 (def) + 3 (body: 1 if + 0 lit + max(0, 2)) + 1 (in-expr: just var = 0, but call = 1)
      (ir-fuel-cost '(R-Letrec fact ((n i64)) i64
                      (R-If (R-Call <= (R-Var n) (R-Literal 1))
                        (R-Literal 1)
                        (R-Call * (R-Var n) (R-Var acc)))
                      (R-Call fact (R-Literal 5)))))

(test-section "Division-by-Zero in Closures")

;; Lambda bodies are NOT collected - guards would need inner scope params
;; This is intentional: nested functions need scoped guards (future work)
(test "Lambda body NOT collected (scoping fix)"
      '()
      (ir-collect-divisors '(R-Lambda ((x i64) (y i64)) i64 (R-Call / (R-Var x) (R-Var y)))))

;; Letrec: only collect from in-expr (call site), not body
;; Body divisors can't be guarded with outer params
(test "Letrec body NOT collected (scoping fix)"
      '()
      (ir-collect-divisors '(R-Letrec div_acc ((n i64)) i64
                             (R-Call / (R-Literal 100) (R-Var n))
                             (R-Call div_acc (R-Literal 5)))))

;; But division in the in-expr IS collected
(test "Letrec in-expr IS collected"
      '((R-Var x))
      (ir-collect-divisors '(R-Letrec f ((n i64)) i64
                             (R-Var n)
                             (R-Call / (R-Literal 10) (R-Var x)))))

;; Integer var check in lambda
(test "Contains int var in lambda body"
      #t
      (ir-contains-integer-var?
       '(R-Lambda ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))
       '()))

;; Float lambda - no int vars
(test "No int vars in float lambda"
      #f
      (ir-contains-integer-var?
       '(R-Lambda ((x f64)) f64 (R-Call + (R-Var x) (R-Literal 1)))
       '()))

(test-section "QA Edge Cases (Gemini Review)")

;; Higher-order function calls - R-Call with non-symbol operator
(test "R-Call with R-Var operator"
      "f(x, y)"
      (rust-serialize '(R-Call (R-Var f) (R-Var x) (R-Var y))))

(test "R-Call with nested R-Var"
      "callback(1, 2)"
      (rust-serialize '(R-Call (R-Var callback) (R-Literal 1) (R-Literal 2))))

;; Empty parameter lists
(test "Lambda with empty params"
      "|| -> i64 { 42 }"
      (rust-serialize '(R-Lambda () i64 (R-Literal 42))))

(test "Letrec with empty params (thunk)"
      #t
      ;; Even 0-arity recursive fns get __fuel param
      (let ([code (rust-serialize '(R-Letrec get_value () i64 (R-Literal 42) (R-Var get_value)))])
           (and (string-contains code "fn get_value(__fuel: &mut u64)")
                (string-contains code "if *__fuel == 0"))))

;; Scheme->IR with empty params
(test "Scheme->IR empty param lambda"
      '(R-Lambda () i64 (R-Literal 42))
      (scheme->rust-ir '(fn () i64 42)))

(test "Scheme->IR empty param fix"
      '(R-Letrec counter () i64 (R-Literal 0) (R-Var counter))
      (scheme->rust-ir '(fix counter (fn () i64 0))))

;; Single parameter (edge case)
(test "Lambda with single param"
      "|x: i64| -> i64 { (x + 1) }"
      (rust-serialize '(R-Lambda ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1)))))

(test-section "End-to-End Closure/Recursion Tests")

;; Test that R-Lambda serialization produces valid Rust closure syntax
(test "Lambda serialization syntax valid"
      #t
      ;; Verify it produces |x| -> T { ... } pattern
      (let ([code (rust-serialize '(R-Lambda ((x f64)) f64 (R-Call * (R-Var x) (R-Var x))))])
           (and (string-contains code "|x: f64|")
                (string-contains code "-> f64"))))

;; Test that R-Letrec produces valid local fn syntax with fuel
(test "Letrec serialization syntax valid"
      #t
      (let ([code (rust-serialize '(R-Letrec sum ((n i64)) i64
                                    (R-If (R-Call <= (R-Var n) (R-Literal 0))
                                      (R-Literal 0)
                                      (R-Call + (R-Var n) (R-Call sum (R-Call - (R-Var n) (R-Literal 1)))))
                                    (R-Call sum (R-Literal 10))))])
           (and (string-contains code "fn sum(n: i64, __fuel: &mut u64)")
                (string-contains code "sum(10, &mut __remaining)"))))

;; Compile a function that uses letrec internally (factorial)
(display "  Compiling factorial with letrec... ")
(let ([factorial-ir
       '(R-Fn factorial ((n i64)) i64
         (R-Letrec fact ((x i64)) i64
           (R-If (R-Call <= (R-Var x) (R-Literal 1))
             (R-Literal 1)
             (R-Call * (R-Var x) (R-Call fact (R-Call - (R-Var x) (R-Literal 1)))))
           (R-Call fact (R-Var n))))])
     (let ([res (compile-rust-lib "test_factorial" factorial-ir)])
          (if (eq? (car res) 'ok)
              (begin
               (display "✓\n")
               (cleanup-rust-lib "test_factorial"))
              (begin
               (display "✗\n")
               (display res)
               (newline)))))

;; Note: Lambda as first-class values (passed to/returned from functions)
;; requires function pointer types in FFI, which is future work

(test-section "Fuel Threading (fold-vt79)")

;; Test that ir-contains-letrec? detects R-Letrec
(test "ir-contains-letrec? positive"
      #t
      (ir-contains-letrec?
       '(R-Block
         (R-Let x (R-Literal 1))
         (R-Letrec f ((n i64)) i64 (R-Var n) (R-Call f (R-Literal 5))))))

(test "ir-contains-letrec? negative"
      #f
      (ir-contains-letrec?
       '(R-Block
         (R-Let x (R-Literal 1))
         (R-Lambda ((n i64)) i64 (R-Var n)))))

;; Test that letrec bodies have fuel check
(test "R-Letrec generates fuel check"
      #t
      (let ([code (rust-serialize '(R-Letrec loop ((x i64)) i64
                                    (R-Call loop (R-Var x))
                                    (R-Call loop (R-Literal 0))))])
           (and (string-contains code "if *__fuel == 0 { panic!")
                (string-contains code "*__fuel -= 1"))))

;; Test that recursive calls pass __fuel
(test "Recursive calls pass __fuel"
      #t
      (let ([code (rust-serialize '(R-Letrec rec ((a i64) (b i64)) i64
                                    (R-Call + (R-Var a) (R-Call rec (R-Var b) (R-Literal 0)))
                                    (R-Call rec (R-Literal 1) (R-Literal 2))))])
           (string-contains code "rec(b, 0, __fuel)")))

;; Test emit-fueled-body generates catch_unwind
(test "emit-fueled-body wraps in catch_unwind"
      #t
      (let ([code (emit-fueled-body '(R-Literal 42) 'i64)])
           (and (string-contains code "std::panic::catch_unwind")
                (string-contains code "result.status = 2"))))

(test-section "Capturing Closures (fold-13td)")

;; Test free-vars analysis
(test "free-vars: literal" '() (free-vars 42 '()))
(test "free-vars: bound var" '() (free-vars 'x '(x)))
(test "free-vars: free var" '(x) (free-vars 'x '()))
(test "free-vars: mixed"
      #t  ; Check contains y but not x (x is bound)
      (let ([fv (free-vars '(+ x y) '(x))])
           (and (memq 'y fv) (not (memq 'x fv)))))
(test "free-vars: fn binds params"
      '()  ; x is bound by the fn params
      (free-vars '(fn ((x i64)) i64 x) '()))
(test "free-vars: fn with capture"
      #t  ; y is free in the fn body
      (let ([fv (free-vars '(fn ((x i64)) i64 (+ x y)) '())])
           (and (memq 'y fv) #t)))

;; Test scheme->rust-ir emits R-Lambda for non-capturing
(test "scheme->rust-ir: non-capturing emits R-Lambda"
      'R-Lambda
      (car (scheme->rust-ir '(fn ((x i64)) i64 x))))

;; Test scheme->rust-ir emits R-Closure for capturing
(test "scheme->rust-ir: capturing emits R-Closure"
      'R-Closure
      (car (scheme->rust-ir '(fn ((x i64)) i64 (+ x y)))))

(test "scheme->rust-ir: R-Closure captures correct vars"
      #t
      (let ([ir (scheme->rust-ir '(fn ((x i64)) i64 (+ x y)))])
           (and (eq? (car ir) 'R-Closure)
                (memq 'y (cadr ir))     ; y should be in captures list
                #t)))

;; Test R-Closure serialization produces 'move'
(test "R-Closure serializes with move"
      #t
      (let ([code (rust-serialize '(R-Closure (y) ((x i64)) i64 (R-Call + (R-Var x) (R-Var y))))])
           (string-contains code "move |x: i64|")))

;; Test R-Closure fuel cost includes capture count
(test "R-Closure fuel cost includes captures"
      3  ; 1 (closure) + 2 (captures) + 0 (body = just literal)
      (ir-fuel-cost '(R-Closure (a b) ((x i64)) i64 (R-Literal 0))))

;; R-Closure body NOT collected for divisors (scoping fix)
;; Guards for closure body would need closure params, not outer params
(test "R-Closure body NOT collected (scoping fix)"
      '()
      (ir-collect-divisors '(R-Closure (a) ((x i64)) i64 (R-Call / (R-Var a) (R-Var x)))))

;; End-to-end: capturing closure in a top-level function
(test "Capturing closure in R-Fn"
      #t
      (let ([code (rust-emit '(R-Fn make_adder ((n i64)) i64
                               (R-Closure (n) ((x i64)) i64 (R-Call + (R-Var n) (R-Var x)))))])
           (and (string-contains code "move |x: i64|")
                (string-contains code "fn make_adder"))))

;; Multiple captures
(test "R-Closure with multiple captures"
      #t
      (let ([code (rust-serialize '(R-Closure (a b c) ((x i64)) i64
                                    (R-Call + (R-Var a) (R-Call + (R-Var b) (R-Call + (R-Var c) (R-Var x))))))])
           (and (string-contains code "move")
                (string-contains code "(a + (b + (c + x)))"))))

;; Nested closures
(test "Nested closures both capture"
      #t
      (let ([outer (scheme->rust-ir '(fn ((x i64)) i64
                                       (fn ((y i64)) i64 (+ x y))))])
           ;; Outer captures nothing (x is param)
           ;; Inner captures x (y is param)
           (and (eq? (car outer) 'R-Lambda)
                (eq? (car (cadddr outer)) 'R-Closure))))

(test-section "Function Pointer Parameters (fold-s2w6)")

;; Test R-FnCall serialization
(test "R-FnCall simple"
      "f(x, fuel_remaining)"
      (rust-serialize '(R-FnCall (R-Var f) (R-Var x))))

(test "R-FnCall two args"
      "g(a, b, fuel_remaining)"
      (rust-serialize '(R-FnCall (R-Var g) (R-Var a) (R-Var b))))

(test "R-FnCall no args"
      "h(fuel_remaining)"
      (rust-serialize '(R-FnCall (R-Var h))))

;; Test param-type->rust for function types
(test "param-type->rust: simple scalar"
      "i64"
      (param-type->rust 'i64))

(test "param-type->rust: function type"
      "extern \"C\" fn(i64, u64) -> i64"
      (param-type->rust '(-> i64 i64)))

(test "param-type->rust: multi-param function"
      "extern \"C\" fn(i64, f64, u64) -> bool"
      (param-type->rust '(-> i64 f64 bool)))

;; Test ir-contains-fn-call?
(test "ir-contains-fn-call?: R-FnCall"
      #t
      (ir-contains-fn-call? '(R-FnCall (R-Var f) (R-Var x))))

(test "ir-contains-fn-call?: nested in R-Call"
      #t
      (ir-contains-fn-call? '(R-Call + (R-FnCall (R-Var f) (R-Var x)) (R-Literal 1))))

(test "ir-contains-fn-call?: no fn call"
      #f
      (ir-contains-fn-call? '(R-Call + (R-Var x) (R-Var y))))

(test "ir-contains-fn-call?: in R-If branch"
      #t
      (ir-contains-fn-call? '(R-If (R-Var cond)
                               (R-FnCall (R-Var f) (R-Var x))
                               (R-Literal 0))))

;; Test rust-emit with callback param
(test "rust-emit with fn pointer param emits fuel_remaining"
      #t
      (let ([code (rust-emit '(R-Fn apply_fn ((f (-> i64 i64)) (x i64)) i64
                               (R-FnCall (R-Var f) (R-Var x))))])
           (and (string-contains code "let fuel_remaining = fuel_in - COST")
                (string-contains code "f(x, fuel_remaining)"))))

(test "rust-emit fn pointer param type correct"
      #t
      (let ([code (rust-emit '(R-Fn apply_fn ((f (-> i64 i64)) (x i64)) i64
                               (R-FnCall (R-Var f) (R-Var x))))])
           (string-contains code "f: extern \"C\" fn(i64, u64) -> i64")))

;; Test R-FnCall in rust-serialize-with-fuel (fuel threading)
(test "R-FnCall in recursive context"
      #t
      (let ([code (rust-serialize-with-fuel
                   '(R-FnCall (R-Var callback) (R-Var x))
                   'rec)])
           (string-contains code "callback(x, fuel_remaining)")))

(newline)
