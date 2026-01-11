;;; core/lang/rust-codegen.ss — Rust Code Generation for The Fold
;;; @module rust-codegen
;;; @requires prelude rust-mapping
;;;
;;; Serializes Rust IR into valid Rust source code and translates
;;; Scheme expressions to Rust IR for compilation via FFI.
;;;
;;; Status: Layer 1 primitives complete (arithmetic, comparison,
;;;         logical, bitwise, math methods). See README.sexp for
;;;         remaining work tracked in beads issues.
;;;
;;; Supported FFI return types:
;;;   i64  → I64Result   (64-bit signed integer)
;;;   f64  → F64Result   (64-bit float)
;;;   bool → BoolResult  (boolean as u8: 0/1)
;;;   u64  → U64Result   (64-bit unsigned integer)
;;;   i32  → I32Result   (32-bit signed integer)
;;;   f32  → F32Result   (32-bit float)
;;;   *    → TestResult  (fallback, casts to f64)
;;;
;;; Rust IR nodes:
;;;   (R-Literal value [type]) - Constants (number, bool, string)
;;;   (R-Var symbol)           - Variable reference
;;;   (R-Call op arg...)       - Operation/function call
;;;   (R-Let symbol value)     - Let binding
;;;   (R-If cond then else)    - Conditional expression
;;;   (R-Block stmt... expr)   - Statement block with final expr
;;;   (R-Fn name params ret body) - Function definition
;;;
;;; Division-by-zero protection (M2):
;;;   - Integer division/modulo generates guards before computation
;;;   - Float division returns Inf/NaN per IEEE 754 (no guard needed)
;;;   - Complex expressions containing integers are guarded conservatively
;;;
;;; Known limitations:
;;;   - Guards are hoisted to function start (may break manual checks)
;;;   - Let-bound variables not tracked (only function parameters)
;;;   - Complex expressions evaluated twice (guard + actual division)
;;;   - Future: inline guards, type environment for locals
;;;
;;; Key lessons from implementation:
;;;   1. Verify against spec (prim.ss) before marking complete
;;;   2. Test full pipeline, not just components
;;;   3. Check prelude for existing helpers before implementing
;;;   4. Every extracted value should be used (ret-type was ignored)

(load "core/base/prelude.ss")
(load "core/lang/rust-mapping.ss")

;;; ============================================================
;;; Operator Mappings
;;; ============================================================

;;; scheme-op->rust : Symbol → (or String #f)
;;; Convert a Scheme primitive operator to Rust infix/prefix syntax.
;;; Returns #f for operators requiring special handling.
(define (scheme-op->rust op)
  (case op
        ;; Arithmetic (infix)
        [(add +) "+"]
        [(sub -) "-"]
        [(mul *) "*"]
        [(div /) "/"]
        [(mod remainder %) "%"]
        ;; Comparison (infix)
        [(lt? <) "<"]
        [(le? <=) "<="]
        [(gt? >) ">"]
        [(ge? >=) ">="]
        [(eq?) "=="]
        ;; Logical (infix)
        [(and) "&&"]
        [(or) "||"]
        ;; Bitwise (infix)
        [(bitand) "&"]
        [(bitor) "|"]
        [(bitxor) "^"]
        [(shl) "<<"]
        [(shr) ">>"]
        [else #f]))

;;; scheme-op-method? : Symbol → Boolean
;;; True if operator should be emitted as method call syntax.
(define (scheme-op-method? op)
  (memq op '(abs sqrt sin cos tan asin acos atan sinh cosh tanh log exp floor ceiling round truncate)))

;;; variadic-safe-op? : Symbol → Boolean
;;; True if operator is associative and safe for variadic folding.
;;; Excludes: comparisons (<, <=, >, >=, ==), shifts (<<, >>), sub (-), div (/), mod (%).
;;; These are either non-associative or have special semantics.
(define (variadic-safe-op? op)
  (memq op '(add + mul * and or bitand bitor bitxor)))

;;; variadic-identity : Symbol → (or String #f)
;;; Return the identity value for a variadic operator, or #f if none.
;;; Used for 0-arg cases: (+) → 0, (*) → 1
(define (variadic-identity op)
  (case op
        [(add +) "0"]
        [(mul *) "1"]
        [(and) "true"]
        [(or) "false"]
        [else #f]))

;;; scheme-op->rust-method : Symbol → String
;;; Get the Rust method name for method-style ops.
(define (scheme-op->rust-method op)
  (case op
        [(abs) "abs"]
        [(sqrt) "sqrt"]
        [(sin) "sin"]
        [(cos) "cos"]
        [(tan) "tan"]
        [(asin) "asin"]
        [(acos) "acos"]
        [(atan) "atan"]
        [(sinh) "sinh"]
        [(cosh) "cosh"]
        [(tanh) "tanh"]
        [(log) "ln"]        ; Rust uses ln() for natural log
        [(exp) "exp"]
        [(floor) "floor"]
        [(ceiling) "ceil"]  ; Rust uses ceil()
        [(round) "round"]
        [(truncate) "trunc"]  ; Rust uses trunc()
        [else #f]))

;;; ============================================================
;;; IR Serialization
;;; ============================================================

;;; rust-serialize : (List α) → String
;;; Convert Rust IR to a string fragment.
;;;
;;; R-Literal format:
;;;   (R-Literal value)        - untyped, uses heuristics
;;;   (R-Literal value type)   - typed, uses explicit suffix

(define (rust-serialize ir)
  
  (cond
   
   [(not (pair? ir)) (format "/* Invalid IR: ~s */" ir)]
   
   [(eq? (car ir) 'R-Literal)
    (let* ([val (cadr ir)]
           [has-type? (and (pair? (cddr ir)) (not (null? (cddr ir))))]
           [type (if has-type? (caddr ir) #f)])
          (cond
           ;; Boolean literals - no suffix needed
           [(boolean? val) (if val "true" "false")]
           
           ;; String literals
           [(string? val) (format "\"~a\"" val)]
           
           ;; Numeric literals with explicit type
           [(and (number? val) type)
            (case type
                  [(i64) (format "~ai64" (inexact->exact (truncate val)))]
                  [(f64) (if (integer? val)
                             (format "~a.0_f64" (inexact->exact (truncate val)))
                             (format "~a_f64" val))]
                  [(u64) (format "~au64" (inexact->exact (truncate val)))]
                  [(i32) (format "~ai32" (inexact->exact (truncate val)))]
                  [(f32) (if (integer? val)
                             (format "~a.0_f32" (inexact->exact (truncate val)))
                             (format "~a_f32" val))]
                  [else (number->string val)])]
           
           ;; Numeric literals without type - use heuristics
           [(number? val)
            (cond
             ;; Flonum (has decimal or is inexact) - add suffix for safety
             [(and (inexact? val) (not (integer? val)))
              (format "~a_f64" val)]
             ;; Exact integer - leave as-is for Rust type inference
             [(integer? val)
              (number->string (inexact->exact (truncate val)))]
             ;; Default
             [else (number->string val)])]
           
           [else (format "/* ~s */" val)]))]
   
   [(eq? (car ir) 'R-Var) (symbol->string (cadr ir))]
   
   [(eq? (car ir) 'R-Call)
    (let ([op (cadr ir)]
          [args (cddr ir)])
         (cond
          ;; 0-arg identity values: (+) → 0, (*) → 1, (and) → true, (or) → false
          [(and (null? args) (variadic-identity op))
           (variadic-identity op)]
          
          ;; 1-arg pass-through for associative ops: (+ x) → x, (* x) → x
          [(and (= (length args) 1) (variadic-safe-op? op))
           (rust-serialize (car args))]
          
          ;; Variadic infix operators (n > 2): chain as left-associative binary ops
          ;; Only for associative ops: +, *, and, or, bitand, bitor, bitxor
          ;; (+ a b c d) → ((((a) + (b)) + (c)) + (d))
          ;; Excludes: comparisons, shifts, sub, div, mod (non-associative)
          [(and (variadic-safe-op? op) (> (length args) 2))
           (let loop ([acc (string-append "(" (rust-serialize (car args))
                                          " " (scheme-op->rust op) " "
                                          (rust-serialize (cadr args)) ")")]
                      [rest (cddr args)])
                (if (null? rest)
                    acc
                    (loop (string-append "(" acc
                                         " " (scheme-op->rust op) " "
                                         (rust-serialize (car rest)) ")")
                          (cdr rest))))]
          
          ;; Binary infix operators (+, -, *, /, <, <=, >, >=, ==, &&, ||, &, |, ^, <<, >>)
          [(and (scheme-op->rust op) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          " " (scheme-op->rust op) " "
                          (rust-serialize (cadr args)) ")")]
          
          ;; Unary prefix: not, bitnot
          [(and (memq op '(not bitnot)) (= (length args) 1))
           (string-append "(!" (rust-serialize (car args)) ")")]
          
          ;; Unary prefix: neg
          [(and (eq? op 'neg) (= (length args) 1))
           (string-append "(-" (rust-serialize (car args)) ")")]
          
          ;; Square: x.powi(2) - integer power is more efficient
          [(and (eq? op 'sq) (= (length args) 1))
           (string-append "(" (rust-serialize (car args)) ".powi(2))")]
          
          ;; Method calls: abs, sqrt, trig, hyperbolic, log, exp, floor, ceiling, round
          [(and (scheme-op-method? op) (= (length args) 1))
           (string-append "(" (rust-serialize (car args))
                          "." (scheme-op->rust-method op) "())")]
          
          ;; expt/pow -> powf (binary method)
          [(and (memq op '(expt pow)) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".powf(" (rust-serialize (cadr args)) "))")]
          
          ;; atan2 -> atan2 (binary method)
          [(and (eq? op 'atan2) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".atan2(" (rust-serialize (cadr args)) "))")]
          
          ;; hypot -> hypot (binary method)
          [(and (eq? op 'hypot) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".hypot(" (rust-serialize (cadr args)) "))")]
          
          ;; min/max -> method calls (binary)
          [(and (eq? op 'min) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".min(" (rust-serialize (cadr args)) "))")]
          [(and (eq? op 'max) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".max(" (rust-serialize (cadr args)) "))")]
          
          ;; min/max -> variadic method chains: (min a b c d) → a.min(b).min(c).min(d)
          [(and (eq? op 'min) (> (length args) 2))
           (let loop ([acc (rust-serialize (car args))]
                      [rest (cdr args)])
                (if (null? rest)
                    (string-append "(" acc ")")
                    (loop (string-append acc ".min(" (rust-serialize (car rest)) ")")
                          (cdr rest))))]
          [(and (eq? op 'max) (> (length args) 2))
           (let loop ([acc (rust-serialize (car args))]
                      [rest (cdr args)])
                (if (null? rest)
                    (string-append "(" acc ")")
                    (loop (string-append acc ".max(" (rust-serialize (car rest)) ")")
                          (cdr rest))))]
          
          ;; Default function call
          [else
           (string-append (symbol->string op) "("
                          (string-join (map rust-serialize args) ", ") ")")]))]
   
   [(eq? (car ir) 'R-Let)
    
    (format "let ~a = ~a;" (cadr ir) (rust-serialize (caddr ir)))]
   
   [(eq? (car ir) 'R-If)
    (format "if ~a { ~a } else { ~a }"
            (rust-serialize (cadr ir))
            (rust-serialize (caddr ir))
            (rust-serialize (cadddr ir)))]
   
   ;; R-Block: { stmt; stmt; expr }
   [(eq? (car ir) 'R-Block)
    (let ([parts (cdr ir)])
         (if (null? parts)
             "{}"
             (let ([stmts (init parts)]
                   [final (last parts)])
                  (string-append "{ "
                                 (string-join (map rust-serialize stmts) " ")
                                 (if (null? stmts) "" " ")
                                 (rust-serialize final) " }"))))]
   
   [else (format "/* Unknown IR: ~s */" ir)]))

;;; ============================================================
;;; Scheme to Rust IR Translation
;;; ============================================================

;;; scheme->rust-ir : α → (List α)
;;; Translate a Scheme expression to Rust IR.
;;;
;;; Supported forms:
;;;   literals (number, bool, string) → (R-Literal value)
;;;   symbol                          → (R-Var symbol)
;;;   (prim 'op args...)             → (R-Call op ir-args...)
;;;   (let ((x e)) body)             → (R-Block (R-Let x ir-e) body-ir)
;;;   (if cond then else)            → (R-If ir-cond ir-then ir-else)
;;;   (+ a b), (< a b), etc.         → (R-Call op ir-args...)
(define (scheme->rust-ir expr)
  (cond
   ;; Literals
   [(number? expr) `(R-Literal ,expr)]
   [(boolean? expr) `(R-Literal ,expr)]
   [(string? expr) `(R-Literal ,expr)]
   
   ;; Variables
   [(symbol? expr) `(R-Var ,expr)]
   
   ;; Compound forms
   [(pair? expr)
    (let ([head (car expr)])
         (cond
          ;; (prim 'op args...)
          [(eq? head 'prim)
           (let ([op (cadr (cadr expr))]  ; Extract symbol from (quote op)
                 [args (cddr expr)])
                `(R-Call ,op ,@(map scheme->rust-ir args)))]
          
          ;; (if cond then else)
          [(eq? head 'if)
           `(R-If ,(scheme->rust-ir (cadr expr))
             ,(scheme->rust-ir (caddr expr))
             ,(scheme->rust-ir (cadddr expr)))]
          
          ;; (let ((x e)) body) - single binding
          [(eq? head 'let)
           (let* ([binding (car (cadr expr))]
                  [name (car binding)]
                  [val (cadr binding)]
                  [body (caddr expr)])
                 `(R-Block (R-Let ,name ,(scheme->rust-ir val))
                   ,(scheme->rust-ir body)))]
          
          ;; Direct operator call: (+ a b), (< a b), etc.
          [(scheme-op->rust head)
           `(R-Call ,head ,@(map scheme->rust-ir (cdr expr)))]
          
          ;; Method-style operators: (abs x), (sqrt x), etc.
          [(scheme-op-method? head)
           `(R-Call ,head ,@(map scheme->rust-ir (cdr expr)))]
          
          ;; Unary prefix: (neg x), (not x)
          [(memq head '(neg not bitnot))
           `(R-Call ,head ,@(map scheme->rust-ir (cdr expr)))]
          
          ;; expt
          [(eq? head 'expt)
           `(R-Call expt ,@(map scheme->rust-ir (cdr expr)))]
          
          ;; Function application (fallback)
          [else
           `(R-Call ,head ,@(map scheme->rust-ir (cdr expr)))]))]
   
   [else `(R-Literal ,(format "/* unsupported: ~s */" expr))]))


;;; ============================================================
;;; Fuel Cost Computation
;;; ============================================================

;;; op-fuel-cost : Symbol → Nat
;;; Return the fuel cost of a primitive operation.
;;; Matches weighted-cost-model from cost-model.ss.
(define (op-fuel-cost op)
  (case op
        ;; Arithmetic - basic ops cost 1
        [(add sub mul + - *) 1]
        ;; Division/modulo cost 2 (more expensive)
        [(div mod remainder / %) 2]
        ;; Comparison - cheap
        [(lt? le? gt? ge? eq? < <= > >= == zero? positive? negative?) 1]
        ;; Min/max - cheap
        [(min max) 1]
        ;; Logical - cheap
        [(and or not) 1]
        ;; Bitwise - cheap
        [(bitand bitor bitxor bitnot shl shr) 1]
        ;; Unary
        [(neg abs) 1]
        ;; Math methods - transcendentals are expensive
        [(sqrt) 2]
        [(sq) 1]                                       ; square is just mul
        [(sin cos tan asin acos atan atan2) 3]
        [(sinh cosh tanh) 3]
        [(log exp hypot) 3]
        [(floor ceiling round truncate) 1]
        [(expt pow) 3]
        ;; Default
        [else 1]))

;;; ir-fuel-cost : (List α) → Nat
;;; Compute the total fuel cost of an IR expression.
;;; Uses static analysis: conditionals take max(then, else).
(define (ir-fuel-cost ir)
  (cond
   ;; Invalid IR
   [(not (pair? ir)) 0]
   
   ;; Literals cost nothing
   [(eq? (car ir) 'R-Literal) 0]
   
   ;; Variables cost nothing
   [(eq? (car ir) 'R-Var) 0]
   
   ;; Calls: op cost + arg costs
   ;; For variadic ops (n > 2 args), charge (n-1) × op-cost (chained binary ops)
   ;; 0-arg identity cases cost nothing (just return constant) - only for variadic-safe ops
   ;; 1-arg pass-through cases cost nothing (just return the arg) - only for variadic-safe ops
   [(eq? (car ir) 'R-Call)
    (let* ([op (cadr ir)]
           [args (cddr ir)]
           [n-args (length args)]
           [is-variadic-safe (or (variadic-safe-op? op) (memq op '(min max)))]
           [op-multiplier (cond
                           ;; 0-arg or 1-arg for variadic-safe ops: no op cost (identity/pass-through)
                           [(and (< n-args 2) is-variadic-safe) 0]
                           ;; 0-arg or 1-arg for other ops: still 1 op (e.g., sqrt, sin)
                           [(< n-args 2) 1]
                           ;; 2-arg: single op
                           [(= n-args 2) 1]
                           ;; n > 2 for variadic-safe ops: (n-1) ops
                           [is-variadic-safe (- n-args 1)]
                           ;; Non-variadic ops with >2 args: treat as error (1 op)
                           [else 1])])
          (+ (* op-multiplier (op-fuel-cost op))
             (apply + (map ir-fuel-cost args))))]
   
   ;; If: branch cost + condition + max(then, else)
   [(eq? (car ir) 'R-If)
    (+ 1  ; Branch instruction cost
       (ir-fuel-cost (cadr ir))    ; condition
       (max (ir-fuel-cost (caddr ir))     ; then branch
            (ir-fuel-cost (cadddr ir))))] ; else branch
   
   ;; Let: value cost only (binding itself is free)
   [(eq? (car ir) 'R-Let)
    (ir-fuel-cost (caddr ir))]
   
   ;; Block: sum of all parts
   [(eq? (car ir) 'R-Block)
    (apply + (map ir-fuel-cost (cdr ir)))]
   
   ;; Function: compute body cost
   [(eq? (car ir) 'R-Fn)
    (ir-fuel-cost (car (cddddr ir)))]
   
   ;; Unknown
   [else 0]))


;;; ============================================================
;;; Autodiff Gradient Formulas
;;; ============================================================
;;;
;;; These formulas enable future Rust-native autodiff codegen.
;;; They are designed to be 1:1 with reverse-diff.ss traced ops.
;;;
;;; Format: (op-local-gradient op) → list of gradient formulas
;;; Each formula is either:
;;;   - A number (constant gradient)
;;;   - A symbol referencing an input ('a, 'b)
;;;   - An S-expression for computed gradient

;;; op-local-gradient : Symbol → (or (List α) #f)
;;; Return the local gradient formulas for each input of an operation.
;;; Used for future Rust autodiff codegen (dual numbers or tape).
(define (op-local-gradient op)
  (case op
        ;; Binary arithmetic
        [(add +)  '(1 1)]                              ; d(a+b)/da=1, d(a+b)/db=1
        [(sub -)  '(1 -1)]                             ; d(a-b)/da=1, d(a-b)/db=-1
        [(mul *)  '(b a)]                              ; d(a*b)/da=b, d(a*b)/db=a
        [(div /)  '((/ 1 b) (/ (- a) (* b b)))]        ; d(a/b)/da=1/b, d(a/b)/db=-a/b²
        
        ;; Unary operations
        [(neg)    '(-1)]                               ; d(-a)/da=-1
        [(abs)    '((if (>= a 0) 1 -1))]               ; d|a|/da = sign(a)
        [(sq)     '((* 2 a))]                          ; d(a²)/da = 2a
        
        ;; Powers and roots
        [(sqrt)   '((/ 1 (* 2 (sqrt a))))]             ; d(√a)/da = 1/(2√a)
        [(expt pow) '((* b (expt a (- b 1)))           ; d(a^b)/da = b*a^(b-1)
                      (* (expt a b) (log a)))]         ; d(a^b)/db = a^b*ln(a)
        
        ;; Exponential and logarithm
        [(exp)    '((exp a))]                          ; d(e^a)/da = e^a
        [(log)    '((/ 1 a))]                          ; d(ln a)/da = 1/a
        
        ;; Trigonometric
        [(sin)    '((cos a))]                          ; d(sin a)/da = cos(a)
        [(cos)    '((- (sin a)))]                      ; d(cos a)/da = -sin(a)
        [(tan)    '((/ 1 (* (cos a) (cos a))))]        ; d(tan a)/da = sec²(a)
        
        ;; Inverse trigonometric
        [(asin)   '((/ 1 (sqrt (- 1 (* a a)))))]       ; d(asin a)/da = 1/√(1-a²)
        [(acos)   '((/ -1 (sqrt (- 1 (* a a)))))]      ; d(acos a)/da = -1/√(1-a²)
        [(atan)   '((/ 1 (+ 1 (* a a))))]              ; d(atan a)/da = 1/(1+a²)
        [(atan2)  '((/ b (+ (* a a) (* b b)))          ; d(atan2(a,b))/da = b/(a²+b²)
                    (/ (- a) (+ (* a a) (* b b))))]    ; d(atan2(a,b))/db = -a/(a²+b²)
        
        ;; Binary math functions
        [(hypot)  '((/ a (hypot a b))                  ; d(hypot(a,b))/da = a/hypot(a,b)
                    (/ b (hypot a b)))]                ; d(hypot(a,b))/db = b/hypot(a,b)
        [(min)    '((if (<= a b) 1 0)                  ; d(min(a,b))/da = 1 if a≤b else 0
                    (if (<= a b) 0 1))]                ; d(min(a,b))/db = 0 if a≤b else 1
        [(max)    '((if (>= a b) 1 0)                  ; d(max(a,b))/da = 1 if a≥b else 0
                    (if (>= a b) 0 1))]                ; d(max(a,b))/db = 0 if a≥b else 1
        
        ;; Hyperbolic functions
        [(sinh)   '((cosh a))]                         ; d(sinh a)/da = cosh(a)
        [(cosh)   '((sinh a))]                         ; d(cosh a)/da = sinh(a)
        [(tanh)   '((/ 1 (* (cosh a) (cosh a))))]      ; d(tanh a)/da = sech²(a) = 1/cosh²(a)
        
        ;; Rounding (non-differentiable, gradient = 0)
        [(floor ceiling round truncate) '(0)]
        
        ;; Comparison/logical (non-differentiable)
        [(lt? le? gt? ge? eq? and or not) #f]
        
        ;; Bitwise (non-differentiable)
        [(bitand bitor bitxor bitnot shl shr) #f]
        
        ;; Unknown operation
        [else #f]))

;;; op-differentiable? : Symbol → Boolean
;;; Return #t if the operation has a defined gradient.
(define (op-differentiable? op)
  (and (op-local-gradient op) #t))


;;; ============================================================
;;; Code Emission
;;; ============================================================

;;; ret-type->result-struct : Symbol → String
;;; Map a return type to its corresponding result struct name.
;;; Supported types: i64, f64, bool, u64, i32, f32
(define (ret-type->result-struct ret-type)
  (case ret-type
        [(i64) "I64Result"]
        [(f64) "F64Result"]
        [(bool) "BoolResult"]
        [(u64) "U64Result"]
        [(i32) "I32Result"]
        [(f32) "F32Result"]
        [else "TestResult"]))  ; Fallback for backwards compatibility

;;; ret-type->result-struct-def : Symbol → String
;;; Generate the inline struct definition for standalone compilation.
(define (ret-type->result-struct-def ret-type)
  (case ret-type
        [(i64) "#[repr(C)] pub struct I64Result { pub status: u8, pub value: i64, pub fuel_out: u64 }"]
        [(f64) "#[repr(C)] pub struct F64Result { pub status: u8, pub value: f64, pub fuel_out: u64 }"]
        [(bool) "#[repr(C)] pub struct BoolResult { pub status: u8, pub value: u8, pub fuel_out: u64 }"]
        [(u64) "#[repr(C)] pub struct U64Result { pub status: u8, pub value: u64, pub fuel_out: u64 }"]
        [(i32) "#[repr(C)] pub struct I32Result { pub status: u8, pub value: i32, pub fuel_out: u64 }"]
        [(f32) "#[repr(C)] pub struct F32Result { pub status: u8, pub value: f32, pub fuel_out: u64 }"]
        [else "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }"]))

;;; ret-type->value-assignment : Symbol → String
;;; Generate the value assignment line based on return type.
;;; bool needs special handling (convert to u8), others assign directly.
(define (ret-type->value-assignment ret-type)
  (case ret-type
        [(bool) "    result.value = if val { 1 } else { 0 };\n"]
        [(i64 f64 u64 i32 f32) "    result.value = val;\n"]
        [else "    result.value = val as f64;\n"]))  ; Fallback for backwards compat

;;; ============================================================
;;; Division-by-Zero Protection (M2: fold-jppr)
;;; ============================================================

;;; ir-collect-divisors : (List α) → (List (List α))
;;; Collect all divisor expressions from division/modulo operations.
;;; Returns list of (divisor-ir . is-integer-context?) pairs.
(define (ir-collect-divisors ir)
  (cond
   [(not (pair? ir)) '()]
   [(eq? (car ir) 'R-Literal) '()]
   [(eq? (car ir) 'R-Var) '()]
   [(eq? (car ir) 'R-Call)
    (let ([op (cadr ir)]
          [args (cddr ir)])
         (append
          ;; If this is div/mod, collect the second argument (divisor)
          (if (and (memq op '(div / mod remainder %))
                   (>= (length args) 2))
              (list (cadr args))  ; Second arg is the divisor
              '())
          ;; Recursively collect from all arguments
          (apply append (map ir-collect-divisors args))))]
   [(eq? (car ir) 'R-If)
    (append (ir-collect-divisors (cadr ir))    ; condition
            (ir-collect-divisors (caddr ir))   ; then
            (ir-collect-divisors (cadddr ir)))] ; else
   [(eq? (car ir) 'R-Let)
    (ir-collect-divisors (caddr ir))]          ; value expression
   [(eq? (car ir) 'R-Block)
    (apply append (map ir-collect-divisors (cdr ir)))]
   [else '()]))

;;; integer-type? : Symbol → Boolean
;;; Check if a type is an integer type (needs div-by-zero protection).
(define (integer-type? type)
  (and (memq type '(i64 i32 u64 u32 i16 u16 i8 u8)) #t))

;;; ir-contains-integer-var? : (List α) × (List (Symbol × Symbol)) → Boolean
;;; Check if an IR expression contains any integer-typed variable references.
;;; Used to conservatively guard complex expressions that might be integers.
(define (ir-contains-integer-var? ir params)
  (cond
   [(not (pair? ir)) #f]
   [(eq? (car ir) 'R-Literal) #f]
   [(eq? (car ir) 'R-Var)
    (let* ([var-name (cadr ir)]
           [param-type (assq var-name params)])
          (and param-type (integer-type? (cadr param-type))))]
   [(eq? (car ir) 'R-Call)
    (any (lambda (arg) (ir-contains-integer-var? arg params))
         (cddr ir))]
   [(eq? (car ir) 'R-If)
    (or (ir-contains-integer-var? (cadr ir) params)
        (ir-contains-integer-var? (caddr ir) params)
        (ir-contains-integer-var? (cadddr ir) params))]
   [(eq? (car ir) 'R-Let)
    (ir-contains-integer-var? (caddr ir) params)]
   [(eq? (car ir) 'R-Block)
    (any (lambda (e) (ir-contains-integer-var? e params))
         (cdr ir))]
   [else #f]))

;;; any : (α → Boolean) × (List α) → Boolean
;;; Return #t if predicate is true for any element.
(define (any pred lst)
  (cond
   [(null? lst) #f]
   [(pred (car lst)) #t]
   [else (any pred (cdr lst))]))

;;; ir-divisor->guard : (List α) × (List (Symbol × Symbol)) → (or String #f)
;;; Generate a guard expression for a divisor.
;;; Returns #f if no guard is needed (e.g., float types or non-zero literal).
;;;
;;; Known limitations (documented for future work):
;;; - Guards are hoisted to function start, not inline with division
;;; - Let-bound variables not tracked (only function parameters)
;;; - Complex expressions evaluated twice (once in guard, once in division)
(define (ir-divisor->guard divisor params)
  (cond
   ;; Literal: check at compile time
   [(and (pair? divisor) (eq? (car divisor) 'R-Literal))
    (let ([val (cadr divisor)])
         (if (and (number? val) (zero? val))
             ;; Zero literal - always error (could be caught at compile time)
             "true /* constant zero divisor */"
             ;; Non-zero literal - no guard needed
             #f))]
   ;; Variable: check if it's an integer type
   [(and (pair? divisor) (eq? (car divisor) 'R-Var))
    (let* ([var-name (cadr divisor)]
           [param-type (assq var-name params)])
          (if (and param-type (integer-type? (cadr param-type)))
              ;; Integer variable - needs guard
              (format "~a == 0" var-name)
              ;; Float or unknown - no guard (Rust handles gracefully)
              #f))]
   ;; Complex expression - guard if it contains any integer variables
   ;; Note: This evaluates the expression twice. Future optimization:
   ;; use let binding in generated Rust to cache the result.
   [else
    (if (ir-contains-integer-var? divisor params)
        ;; Contains integers - needs guard (conservative)
        (format "~a == 0" (rust-serialize divisor))
        ;; Pure floats - no guard needed
        #f)]))

;;; emit-divisor-guards : (List (List α)) × (List (Symbol × Symbol)) → String
;;; Generate guard code for all divisors that need protection.
(define (emit-divisor-guards divisors params)
  (let* ([guards (filter identity
                         (map (lambda (d) (ir-divisor->guard d params))
                              divisors))]
         [unique-guards (remove-duplicates guards)])
        (if (null? unique-guards)
            ""
            (string-append
             "    // Division-by-zero protection\n"
             (apply string-append
                    (map (lambda (guard)
                                 (format "    if ~a {\n        result.status = 3;\n        result.fuel_out = fuel_in - COST;\n        return;\n    }\n"
                                         guard))
                         unique-guards))))))

;;; remove-duplicates : (List α) → (List α)
;;; Remove duplicate elements from a list (preserving order).
(define (remove-duplicates lst)
  (let loop ([lst lst] [seen '()] [result '()])
       (cond
        [(null? lst) (reverse result)]
        [(member (car lst) seen) (loop (cdr lst) seen result)]
        [else (loop (cdr lst) (cons (car lst) seen) (cons (car lst) result))])))

;;; rust-emit : (List α) [× Nat] → String
;;; Emit a full Rust function with FFI boilerplate and fuel tracking.
;;; Cost is computed automatically from the IR body unless explicitly provided.
(define rust-emit
  (case-lambda
   [(ir) (rust-emit-impl ir #f)]
   [(ir cost) (rust-emit-impl ir cost)]))

;;; rust-emit-impl : (List α) × (or Nat #f) → String
;;; Internal implementation of rust-emit.
(define (rust-emit-impl ir explicit-cost)
  (if (not (eq? (car ir) 'R-Fn))
      (error 'rust-emit "Expected R-Fn IR" ir)
      (let* ([name (cadr ir)]
             [params (caddr ir)]
             [ret-type (cadddr ir)]
             [body (car (cddddr ir))]
             [cost (or explicit-cost (ir-fuel-cost body))]
             [rust-params (map (lambda (p) (format "~a: ~a" (car p) (cadr p))) params)]
             [result-struct (ret-type->result-struct ret-type)]
             [struct-def (ret-type->result-struct-def ret-type)]
             [value-assign (ret-type->value-assignment ret-type)]
             ;; M2: Collect divisors and generate guards
             [divisors (ir-collect-divisors body)]
             [div-guards (emit-divisor-guards divisors params)])
            (string-append
             ;; Result struct - compact for standalone compilation
             ;; In crate context, use: use fold_accel::{I64Result, F64Result, ...};
             struct-def "\n\n"
             "#[no_mangle]\n"
             (format "pub extern \"C\" fn ~a(~a, fuel_in: u64, out: *mut ~a) {\n"
                     name (string-join rust-params ", ") result-struct)
             (format "    if (out as *const ~a).is_null() { return; }\n" result-struct)
             "    let result = unsafe { &mut *out };\n"
             (format "    const COST: u64 = ~a;\n" cost)
             "    if fuel_in < COST {\n"
             "        result.status = 2;\n"
             "        result.fuel_out = 0;\n"
             "        return;\n"
             "    }\n"
             ;; M2: Insert division-by-zero guards before computation
             div-guards
             (format "    let val = ~a;\n" (rust-serialize body))
             "    result.status = 1;\n"
             value-assign
             "    result.fuel_out = fuel_in - COST;\n"
             "}"))))

;;; rust-emit-module : (List α) [× Nat] → String
;;; Emit Rust code for use within the rust-accel crate (uses crate imports).
;;; Unlike rust-emit, this does NOT include inline struct definitions.
(define rust-emit-module
  (case-lambda
   [(ir) (rust-emit-module-impl ir #f)]
   [(ir cost) (rust-emit-module-impl ir cost)]))

;;; rust-emit-module-impl : (List α) × (or Nat #f) → String
;;; Internal implementation of rust-emit-module.
(define (rust-emit-module-impl ir explicit-cost)
  (if (not (eq? (car ir) 'R-Fn))
      (error 'rust-emit-module "Expected R-Fn IR" ir)
      (let* ([name (cadr ir)]
             [params (caddr ir)]
             [ret-type (cadddr ir)]
             [body (car (cddddr ir))]
             [cost (or explicit-cost (ir-fuel-cost body))]
             [rust-params (map (lambda (p) (format "~a: ~a" (car p) (cadr p))) params)]
             [result-struct (ret-type->result-struct ret-type)]
             [value-assign (ret-type->value-assignment ret-type)]
             ;; M2: Collect divisors and generate guards
             [divisors (ir-collect-divisors body)]
             [div-guards (emit-divisor-guards divisors params)])
            (string-append
             ;; Module header with crate imports
             "//! Auto-generated by The Fold codegen - do not edit manually\n"
             "//! Source: " (symbol->string name) "\n\n"
             "use crate::{" result-struct "};\n\n"
             "#[no_mangle]\n"
             (format "pub extern \"C\" fn fold_~a(~a, fuel_in: u64, out: *mut ~a) {\n"
                     name (string-join rust-params ", ") result-struct)
             (format "    if (out as *const ~a).is_null() { return; }\n" result-struct)
             "    let result = unsafe { &mut *out };\n"
             (format "    const COST: u64 = ~a;\n" cost)
             "    if fuel_in < COST {\n"
             "        result.status = 2;\n"
             "        result.fuel_out = 0;\n"
             "        return;\n"
             "    }\n"
             ;; M2: Insert division-by-zero guards before computation
             div-guards
             (format "    let val = ~a;\n" (rust-serialize body))
             "    result.status = 1;\n"
             value-assign
             "    result.fuel_out = fuel_in - COST;\n"
             "}"))))
