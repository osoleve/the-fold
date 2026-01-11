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
(test "Simple function"
      "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn add_one(x: i64, fuel_in: u64, out: *mut TestResult) {\n    if (out as *const TestResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + 1);\n    result.status = 1;\n    result.value = val as f64;\n    result.fuel_out = fuel_in - COST;\n}"
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
(test "Auto-computed cost (simple add)"
      "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn auto_add(x: i64, y: i64, fuel_in: u64, out: *mut TestResult) {\n    if (out as *const TestResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 1;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + y);\n    result.status = 1;\n    result.value = val as f64;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn auto_add ((x i64) (y i64)) i64 (R-Call + (R-Var x) (R-Var y)))))

(test "Auto-computed cost (nested ops)"
      ;; Cost should be: 1 (outer +) + 1 (inner *) = 2
      "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn nested_ops(x: f64, y: f64, z: f64, fuel_in: u64, out: *mut TestResult) {\n    if (out as *const TestResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 2;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = ((x * y) + z);\n    result.status = 1;\n    result.value = val as f64;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn nested_ops ((x f64) (y f64) (z f64)) f64 (R-Call + (R-Call * (R-Var x) (R-Var y)) (R-Var z)))))

(test "Explicit cost override still works"
      "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n#[no_mangle]\npub extern \"C\" fn override_cost(x: i64, fuel_in: u64, out: *mut TestResult) {\n    if (out as *const TestResult).is_null() { return; }\n    let result = unsafe { &mut *out };\n    const COST: u64 = 999;\n    if fuel_in < COST {\n        result.status = 2;\n        result.fuel_out = 0;\n        return;\n    }\n    let val = (x + 1);\n    result.status = 1;\n    result.value = val as f64;\n    result.fuel_out = fuel_in - COST;\n}"
      (rust-emit '(R-Fn override_cost ((x i64)) i64 (R-Call + (R-Var x) (R-Literal 1))) 999))

(newline)
