;;; shell/ffi/ffi-core.ss — Core FFI Infrastructure for Rust Acceleration
;;;
;;; Provides Chez Scheme FFI bindings for Rust acceleration.
;;; Uses load-shared-object and foreign-procedure with define-ftype
;;; for safe C-struct interop via out-pointers.
;;;
;;; Design principles:
;;; - Never return scheme-object from Rust (GC safety)
;;; - Use #[repr(C)] structs with out-pointers
;;; - Explicit status codes (0=miss, 1=hit, 2=out-of-fuel)
;;;
;;; This is Shell code: impure, handles dynamic loading.

(load "core/base/prelude.ss")

;;; ============================================================
;;; Library Loading
;;; ============================================================

;;; Global state for library handle
(define *accel-lib-loaded* #f)
(define *accel-lib-path* #f)

;;; Possible library locations (in order of preference)
(define *accel-lib-paths*
  (list
   "shell/ffi/rust-accel/target/release/libfold_accel.so"
   "shell/ffi/rust-accel/target/debug/libfold_accel.so"
   "/usr/local/lib/libfold_accel.so"))

;;; find-accel-library : → String | #f
;;; Find the first existing library path
(define (find-accel-library)
  (let loop ([paths *accel-lib-paths*])
       (if (null? paths)
           #f
           (let ([path (car paths)])
                (if (file-exists? path)
                    path
                    (loop (cdr paths)))))))

;;; accel-load! : → Boolean
;;; Load the Rust acceleration library. Returns #t if successful.
(define (accel-load!)
  (if *accel-lib-loaded*
      #t
      (let ([path (find-accel-library)])
           (if path
               (guard (ex [else
                           (display "FFI: Failed to load library: ")
                           (display path)
                           (newline)
                           #f])
                      (load-shared-object path)
                      (set! *accel-lib-loaded* #t)
                      (set! *accel-lib-path* path)
                      (bind-foreign-procedures!)
                      (display "FFI: Loaded acceleration library: ")
                      (display path)
                      (newline)
                      #t)
               (begin
                (display "FFI: No acceleration library found")
                (newline)
                #f)))))

;;; accel-available? : → Boolean
;;; Check if acceleration is available
(define (accel-available?)
  *accel-lib-loaded*)

;;; accel-library-path : → String | #f
;;; Get the path of the loaded library
(define (accel-library-path)
  *accel-lib-path*)

;;; ============================================================
;;; Test Result Struct (Phase 1)
;;; ============================================================

;;; Define C-compatible struct for test results
;;; Matches Rust's #[repr(C)] TestResult
(define-ftype test-result-t
  (struct
   [status unsigned-8]     ; 0=miss, 1=hit, 2=out-of-fuel
   [value  double]         ; result value
   [fuel   unsigned-64]))  ; remaining fuel

;;; ============================================================
;;; Foreign Procedures (Phase 1)
;;; ============================================================

;;; Foreign procedures are bound lazily after library load
(define rust-accel-version #f)
(define rust-test-roundtrip #f)

;;; bind-foreign-procedures! : → void
;;; Bind all foreign procedures (call after library load)
(define (bind-foreign-procedures!)
  ;; Version check function
  ;; Returns version as u32 (0x000100 = 0.1.0)
  (set! rust-accel-version
        (foreign-procedure "fold_accel_version" () unsigned-32))
  
  ;; Test roundtrip function
  ;; Takes 3 doubles + fuel, writes result to out-pointer
  (set! rust-test-roundtrip
        (foreign-procedure "fold_test_roundtrip"
                           (double double double unsigned-64 (* test-result-t))
                           void)))

;;; ============================================================
;;; Scheme Wrappers
;;; ============================================================

;;; accel-version : → (Major Minor Patch) | #f
;;; Get the library version, or #f if not loaded
(define (accel-version)
  (if (accel-available?)
      (let ([v (rust-accel-version)])
           (list (bitwise-arithmetic-shift-right v 16)
                 (bitwise-and (bitwise-arithmetic-shift-right v 8) #xFF)
                 (bitwise-and v #xFF)))
      #f))

;;; test-ffi-roundtrip : Real × Real × Real × Nat → (ok Real Nat) | (out-of-fuel)
;;; Test the FFI roundtrip with fuel tracking
(define (test-ffi-roundtrip a b c fuel)
  (unless (accel-available?)
          (error 'test-ffi-roundtrip "Acceleration library not loaded"))
  
  ;; Allocate result struct
  (let* ([result-ptr (make-ftype-pointer test-result-t
                                         (foreign-alloc (ftype-sizeof test-result-t)))])
        
        ;; Call Rust function
        (rust-test-roundtrip (inexact a) (inexact b) (inexact c) fuel result-ptr)
        
        ;; Extract results
        (let ([status (ftype-ref test-result-t (status) result-ptr)]
              [value (ftype-ref test-result-t (value) result-ptr)]
              [fuel-out (ftype-ref test-result-t (fuel) result-ptr)])
             
             ;; Free the allocated memory
             (foreign-free (ftype-pointer-address result-ptr))
             
             ;; Return appropriate result based on status
             (case status
                   [(0) '(miss)]
                   [(1) `(ok ,value ,fuel-out)]
                   [(2) '(out-of-fuel)]
                   [else `(error unknown-status ,status)]))))

;;; ============================================================
;;; Self-Test
;;; ============================================================

;;; run-ffi-tests : → Boolean
;;; Run basic FFI tests, returns #t if all pass
(define (run-ffi-tests)
  (display "FFI Core Tests\n")
  (display "==============\n")
  
  ;; Test 1: Library loading
  (display "1. Loading library... ")
  (let ([loaded (accel-load!)])
       (display (if loaded "OK" "SKIP (no library)"))
       (newline)
       
       (if (not loaded)
           (begin
            (display "   (Build with: cd shell/ffi/rust-accel && cargo build --release)\n")
            #f)
           
           (begin
            ;; Test 2: Version check
            (display "2. Version check... ")
            (let ([ver (accel-version)])
                 (display ver)
                 (newline))
            
            ;; Test 3: Roundtrip with sufficient fuel
            (display "3. Roundtrip (fuel=100): ")
            (let ([result (test-ffi-roundtrip 1 2 3 100)])
                 (display result)
                 (newline)
                 (unless (and (pair? result)
                              (eq? (car result) 'ok)
                              (= (cadr result) 12.0)  ; (1+2+3)*2
                              (= (caddr result) 95))  ; 100-5
                         (display "   FAIL: expected (ok 12.0 95)\n")
                         (set! loaded #f)))
            
            ;; Test 4: Roundtrip with insufficient fuel
            (display "4. Roundtrip (fuel=2): ")
            (let ([result (test-ffi-roundtrip 1 2 3 2)])
                 (display result)
                 (newline)
                 (unless (equal? result '(out-of-fuel))
                         (display "   FAIL: expected (out-of-fuel)\n")
                         (set! loaded #f)))
            
            (display "==============\n")
            (if loaded
                (display "All tests passed!\n")
                (display "Some tests failed.\n"))
            loaded))))
