;;; core/lang/rust-codegen.ss — Rust Code Generation for The Fold
;;; @module rust-codegen
;;; @requires prelude rust-mapping
;;;
;;; Serializes Rust IR into valid Rust source code.
;;;
;;; Rust IR:
;;;   (R-Literal value)
;;;   (R-Var symbol)
;;;   (R-Call op arg...)
;;;   (R-Let symbol value)
;;;   (R-If cond then else)
;;;   (R-Fn name (param...) ret body)

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

;;; scheme->rust-ir : Expr → R-IR
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
;;; Code Emission
;;; ============================================================

;;; rust-emit : (List α) × Nat → String
;;; Emit a full Rust function with FFI boilerplate and fuel tracking.
(define (rust-emit ir cost)
  (if (not (eq? (car ir) 'R-Fn))
      (error 'rust-emit "Expected R-Fn IR" ir)
      (let* ([name (cadr ir)]
             [params (caddr ir)]
             [ret-type (cadddr ir)]
             [body (car (cddddr ir))]
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
