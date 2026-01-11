;;; user/test-rust-pipeline.ss — End-to-end test of Rust codegen v1.0
;;; Run with: scheme --script user/test-rust-pipeline.ss

(display "=== Rust Codegen v1.0 Test Run ===\n\n")

;; Load the pipeline
(display "1. Loading modules...\n")
(load "core/lang/rust-codegen.ss")
(load "core/lang/rust-compile.ss")
(load "shell/ffi/rust-loader.ss")
(display "   Done.\n\n")

;; Test 1: Create IR manually and compile
(display "2. Creating IR for 'multiply_add' (a*b + c)...\n")
(define multiply-add-ir
  '(R-Fn multiply_add ((a i64) (b i64) (c i64)) i64
    (R-Call add
            (R-Call mul (R-Var a) (R-Var b))
            (R-Var c))))

(display "   IR: ")
(write multiply-add-ir)
(newline)

;; Show the generated Rust code
(display "\n3. Generated Rust code:\n")
(display "   ----------------------------------------\n")
(let ([code (rust-emit-module multiply-add-ir)])
     (for-each (lambda (line)
                       (display "   ")
                       (display line)
                       (newline))
               (string-split code #\newline)))
(display "   ----------------------------------------\n\n")

;; Compile to crate
(display "4. Compiling to crate...\n")
(let ([result (compile-to-crate "multiply_add" multiply-add-ir)])
     (display "   Result: ")
     (write result)
     (newline))

;; Test 2: Use scheme->rust-ir translator for the body, wrap in R-Fn
(display "\n5. Creating 'distance' (sqrt(dx^2 + dy^2))...\n")
(define distance-body
  (scheme->rust-ir '(sqrt (+ (* (- x2 x1) (- x2 x1))
                             (* (- y2 y1) (- y2 y1))))))
(define distance-ir
  `(R-Fn distance ((x1 f64) (y1 f64) (x2 f64) (y2 f64)) f64
    ,distance-body))

(display "   Computed fuel cost: ")
(display (ir-fuel-cost (list-ref distance-ir 4)))  ; body is 5th element
(newline)

(let ([result (compile-to-crate "distance" distance-ir)])
     (display "   Compile result: ")
     (write result)
     (newline))

;; Test 3: Boolean comparison function
(display "\n6. Creating 'is_positive' (x > 0) returning bool...\n")
(define is-positive-ir
  '(R-Fn is_positive ((x i64)) bool
    (R-Call gt (R-Var x) (R-Literal 0))))

(let ([result (compile-to-crate "is_positive" is-positive-ir)])
     (display "   Compile result: ")
     (write result)
     (newline))

;; Test 4: Variadic operation
(display "\n7. Creating 'sum5' with variadic add...\n")
(define sum5-ir
  '(R-Fn sum5 ((a i64) (b i64) (c i64) (d i64) (e i64)) i64
    (R-Call add (R-Var a) (R-Var b) (R-Var c) (R-Var d) (R-Var e))))

(display "   Generated code snippet:\n")
(let* ([code (rust-emit-module sum5-ir)]
       [lines (string-split code #\newline)])
      (for-each (lambda (line)
                        (when (or (string-contains line "let val")
                                  (string-contains line "COST"))
                              (display "   ")
                              (display line)
                              (newline)))
                lines))

(let ([result (compile-to-crate "sum5" sum5-ir)])
     (display "   Compile result: ")
     (write result)
     (newline))

;; List generated modules
(display "\n8. Generated modules in crate:\n")
(for-each (lambda (mod)
                  (display "   - ")
                  (display mod)
                  (newline))
          (list-generated-modules))

;; Build the crate
(display "\n9. Building crate with cargo...\n")
(let ([result (rust-build!)])
     (display "   Result: ")
     (write result)
     (newline)
     
     (when (eq? (car result) 'ok)
           ;; Load and test functions
           (display "\n10. Loading and calling functions...\n")
           
           ;; Test multiply_add
           (display "\n    Testing fold_multiply_add(3, 4, 5) [expect 17]:\n")
           (rust-load-fn! "fold_multiply_add" '(i64 i64 i64) 'i64)
           (let ([result (rust-call "fold_multiply_add" 3 4 5 1000)])
                (display "    Result: ")
                (write result)
                (newline))
           
           ;; Test distance
           (display "\n    Testing fold_distance(0, 0, 3, 4) [expect 5.0]:\n")
           (rust-load-fn! "fold_distance" '(f64 f64 f64 f64) 'f64)
           (let ([result (rust-call "fold_distance" 0.0 0.0 3.0 4.0 1000)])
                (display "    Result: ")
                (write result)
                (newline))
           
           ;; Test is_positive
           (display "\n    Testing fold_is_positive(42) [expect #t]:\n")
           (rust-load-fn! "fold_is_positive" '(i64) 'bool)
           (let ([result (rust-call "fold_is_positive" 42 1000)])
                (display "    Result: ")
                (write result)
                (newline))
           
           (display "\n    Testing fold_is_positive(-5) [expect #f]:\n")
           (let ([result (rust-call "fold_is_positive" -5 1000)])
                (display "    Result: ")
                (write result)
                (newline))
           
           ;; Test sum5
           (display "\n    Testing fold_sum5(1, 2, 3, 4, 5) [expect 15]:\n")
           (rust-load-fn! "fold_sum5" '(i64 i64 i64 i64 i64) 'i64)
           (let ([result (rust-call "fold_sum5" 1 2 3 4 5 1000)])
                (display "    Result: ")
                (write result)
                (newline))
           
           ;; Test out-of-fuel
           (display "\n    Testing out-of-fuel (fuel=0):\n")
           (let ([result (rust-call "fold_sum5" 1 2 3 4 5 0)])
                (display "    Result: ")
                (write result)
                (newline))
           
           ;; Show loader status
           (display "\n11. Loader status:\n")
           (let ([status (rust-loader-status)])
                (for-each (lambda (pair)
                                  (display "    ")
                                  (display (car pair))
                                  (display ": ")
                                  (write (cdr pair))
                                  (newline))
                          status))))

(display "\n=== Test Run Complete ===\n")
