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
;;; Rust IR nodes:
;;;   (R-Literal value)        - Constants (number, bool, string)
;;;   (R-Var symbol)           - Variable reference
;;;   (R-Call op arg...)       - Operation/function call
;;;   (R-Let symbol value)     - Let binding
;;;   (R-If cond then else)    - Conditional expression
;;;   (R-Block stmt... expr)   - Statement block with final expr
;;;   (R-Fn name params ret body) - Function definition
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
        [(mod) "%"]
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
  (memq op '(abs sqrt sin cos tan log floor ceiling round)))

;;; scheme-op->rust-method : Symbol → String
;;; Get the Rust method name for method-style ops.
(define (scheme-op->rust-method op)
  (case op
        [(abs) "abs"]
        [(sqrt) "sqrt"]
        [(sin) "sin"]
        [(cos) "cos"]
        [(tan) "tan"]
        [(log) "ln"]        ; Rust uses ln() for natural log
        [(floor) "floor"]
        [(ceiling) "ceil"]  ; Rust uses ceil()
        [(round) "round"]
        [else #f]))

;;; ============================================================
;;; IR Serialization
;;; ============================================================

;;; rust-serialize : (List α) → String
;;; Convert Rust IR to a string fragment.

(define (rust-serialize ir)
  
  (cond
   
   [(not (pair? ir)) (format "/* Invalid IR: ~s */" ir)]
   
   [(eq? (car ir) 'R-Literal)
    
    (let ([val (cadr ir)])
         
         (cond
          
          [(number? val) (number->string val)]
          
          [(boolean? val) (if val "true" "false")]
          
          [(string? val) (format "\"~a\"" val)]
          
          [else (format "/* ~s */" val)]))]
   
   [(eq? (car ir) 'R-Var) (symbol->string (cadr ir))]
   
   [(eq? (car ir) 'R-Call)
    (let ([op (cadr ir)]
          [args (cddr ir)])
         (cond
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
          
          ;; Method calls: abs, sqrt, sin, cos, tan, log, floor, ceiling, round
          [(and (scheme-op-method? op) (= (length args) 1))
           (string-append "(" (rust-serialize (car args))
                          "." (scheme-op->rust-method op) "())")]
          
          ;; expt -> powf (binary method)
          [(and (eq? op 'expt) (= (length args) 2))
           (string-append "(" (rust-serialize (car args))
                          ".powf(" (rust-serialize (cadr args)) "))")]
          
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
        [(div mod / %) 2]
        ;; Comparison - cheap
        [(lt? le? gt? ge? eq? < <= > >= ==) 1]
        ;; Logical - cheap
        [(and or not) 1]
        ;; Bitwise - cheap
        [(bitand bitor bitxor bitnot shl shr) 1]
        ;; Unary
        [(neg abs) 1]
        ;; Math methods - transcendentals are expensive
        [(sqrt) 2]
        [(sin cos tan) 3]
        [(log exp) 3]
        [(floor ceiling round) 1]
        [(expt) 3]
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
   [(eq? (car ir) 'R-Call)
    (let ([op (cadr ir)]
          [args (cddr ir)])
         (+ (op-fuel-cost op)
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
        
        ;; Hyperbolic functions
        [(sinh)   '((cosh a))]                         ; d(sinh a)/da = cosh(a)
        [(cosh)   '((sinh a))]                         ; d(cosh a)/da = sinh(a)
        [(tanh)   '((/ 1 (* (cosh a) (cosh a))))]      ; d(tanh a)/da = sech²(a) = 1/cosh²(a)
        
        ;; Rounding (non-differentiable, gradient = 0)
        [(floor ceiling round) '(0)]
        
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
             [rust-params (map (lambda (p) (format "~a: ~a" (car p) (cadr p))) params)])
            (string-append
             ;; TestResult struct - compact for standalone compilation
             ;; In crate context, use: use fold_accel::TestResult;
             "#[repr(C)] pub struct TestResult { pub status: u8, pub value: f64, pub fuel_out: u64 }\n\n"
             "#[no_mangle]\n"
             (format "pub extern \"C\" fn ~a(~a, fuel_in: u64, out: *mut TestResult) {\n"
                     name (string-join rust-params ", "))
             "    if (out as *const TestResult).is_null() { return; }\n"
             "    let result = unsafe { &mut *out };\n"
             (format "    const COST: u64 = ~a;\n" cost)
             "    if fuel_in < COST {\n"
             "        result.status = 2;\n"
             "        result.fuel_out = 0;\n"
             "        return;\n"
             "    }\n"
             (format "    let val = ~a;\n" (rust-serialize body))
             "    result.status = 1;\n"
             "    result.value = val as f64;\n"
             "    result.fuel_out = fuel_in - COST;\n"
             "}"))))
