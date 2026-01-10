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
         
         (case op
               
               [(+ - * /)
                
                (string-append "("
                               
                               (string-join (map rust-serialize args) (format " ~a " op))
                               
                               ")")]
               
               [else
                
                (string-append (symbol->string op)
                               
                               "("
                               
                               (string-join (map rust-serialize args) ", ")
                               
                               ")")]))]
   
   [(eq? (car ir) 'R-Let)
    
    (format "let ~a = ~a;" (cadr ir) (rust-serialize (caddr ir)))]
   
   [(eq? (car ir) 'R-If)
    
    (format "if ~a { ~a } else { ~a }"
            
            (rust-serialize (cadr ir))
            
            (rust-serialize (caddr ir))
            
            (rust-serialize (cadddr ir)))]
   
   [else (format "/* Unknown IR: ~s */" ir)]))



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
             "#[no_mangle]\n"
             (format "pub extern \"C\" fn ~a(~a, fuel_in: u64, out: *mut TestResult) {\n"
                     name (string-join rust-params ", "))
             "    if out.is_null() { return; }\n"
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
