(load "core/base/prelude.ss")

(doc 'module 'ffi-core)
(doc 'description "Core FFI Infrastructure for Rust Acceleration")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'see "Provides Chez Scheme FFI bindings for Rust acceleration.")
(doc 'note "Uses load-shared-object and foreign-procedure with define-ftype for safe C-struct interop via out-pointers.")
(doc 'note "Design principles: Never return scheme-object from Rust (GC safety). Use #[repr(C)] structs with out-pointers. Explicit status codes: 0=miss, 1=hit, 2=out-of-fuel, 3=runtime-error.")

(doc 'section 'library-loading)

(define *accel-lib-loaded* #f)
  (doc *accel-lib-loaded* 'description "Global state tracking whether library is loaded")

(define *accel-lib-path* #f)
  (doc *accel-lib-path* 'description "Path to loaded library")

(define *accel-lib-paths*
  (list
   "boundary/ffi/rust-accel/target/release/libfold_accel.so"
   "boundary/ffi/rust-accel/target/debug/libfold_accel.so"
   "/usr/local/lib/libfold_accel.so"))
  (doc *accel-lib-paths* 'description "Possible library locations in order of preference")

(define (find-accel-library)
  (doc 'type (-> (| String #f)))
  (doc 'description "Find the first existing library path")
  (let loop ([paths *accel-lib-paths*])
       (if (null? paths)
           #f
           (let ([path (car paths)])
                (if (file-exists? path)
                    path
                    (loop (cdr paths)))))))

(define (accel-load!)
  (doc 'type (-> Boolean))
  (doc 'description "Load the Rust acceleration library")
  (doc 'returns "Returns #t if successful")
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

(define (accel-available?)
  (doc 'type (-> Boolean))
  (doc 'description "Check if acceleration is available")
  *accel-lib-loaded*)

(define (accel-library-path)
  (doc 'type (-> (| String #f)))
  (doc 'description "Get the path of the loaded library")
  *accel-lib-path*)

(doc 'section 'test-result-struct)

(define-ftype test-result-t
  (struct
   [status unsigned-8]
   [value  double]
   [fuel   unsigned-64]))
  (doc test-result-t 'description "C-compatible struct for test results matching Rust's #[repr(C)] TestResult")
  (doc test-result-t 'note "status: 0=miss, 1=hit, 2=out-of-fuel, 3=runtime-error")

(doc 'section 'foreign-procedures)

(define rust-accel-version #f)
  (doc rust-accel-version 'description "Foreign procedure for version check")

(define rust-test-roundtrip #f)
  (doc rust-test-roundtrip 'description "Foreign procedure for test roundtrip")

(define (bind-foreign-procedures!)
  (doc 'type (-> void))
  (doc 'description "Bind all foreign procedures (call after library load)")
  (set! rust-accel-version
        (foreign-procedure "fold_accel_version" () unsigned-32))
  (set! rust-test-roundtrip
        (foreign-procedure "fold_test_roundtrip"
                           (double double double unsigned-64 (* test-result-t))
                           void)))

(doc 'section 'scheme-wrappers)

(define (accel-version)
  (doc 'type (-> (| (List Nat Nat Nat) #f)))
  (doc 'description "Get the library version (Major Minor Patch), or #f if not loaded")
  (if (accel-available?)
      (let ([v (rust-accel-version)])
           (list (bitwise-arithmetic-shift-right v 16)
                 (bitwise-and (bitwise-arithmetic-shift-right v 8) #xFF)
                 (bitwise-and v #xFF)))
      #f))

(define (test-ffi-roundtrip a b c fuel)
  (doc 'type (-> Real Real Real Nat (| (ok Real Nat) (out-of-fuel))))
  (doc 'description "Test the FFI roundtrip with fuel tracking")
  (doc 'param 'a "First value")
  (doc 'param 'b "Second value")
  (doc 'param 'c "Third value")
  (doc 'param 'fuel "Fuel budget")
  (unless (accel-available?)
          (error 'test-ffi-roundtrip "Acceleration library not loaded"))
  (let* ([result-ptr (make-ftype-pointer test-result-t
                                         (foreign-alloc (ftype-sizeof test-result-t)))])
        (rust-test-roundtrip (inexact a) (inexact b) (inexact c) fuel result-ptr)
        (let ([status (ftype-ref test-result-t (status) result-ptr)]
              [value (ftype-ref test-result-t (value) result-ptr)]
              [fuel-out (ftype-ref test-result-t (fuel) result-ptr)])
             (foreign-free (ftype-pointer-address result-ptr))
             (case status
                   [(0) '(miss)]
                   [(1) `(ok ,value ,fuel-out)]
                   [(2) '(out-of-fuel)]
                   [(3) '(error div-by-zero)]
                   [else `(error unknown-status ,status)]))))

(doc 'section 'self-test)

(define (run-ffi-tests)
  (doc 'type (-> Boolean))
  (doc 'description "Run basic FFI tests, returns #t if all pass")
  (display "FFI Core Tests\n")
  (display "====\n")
  (display "1. Loading library... ")
  (let ([loaded (accel-load!)])
       (display (if loaded "OK" "SKIP (no library)"))
       (newline)
       (if (not loaded)
           (begin
            (display "   (Build with: cd boundary/ffi/rust-accel && cargo build --release)\n")
            #f)
           (begin
            (display "2. Version check... ")
            (let ([ver (accel-version)])
                 (display ver)
                 (newline))
            (display "3. Roundtrip (fuel=100): ")
            (let ([result (test-ffi-roundtrip 1 2 3 100)])
                 (display result)
                 (newline)
                 (unless (and (pair? result)
                              (eq? (car result) 'ok)
                              (= (cadr result) 12.0)
                              (= (caddr result) 95))
                         (display "   FAIL: expected (ok 12.0 95)\n")
                         (set! loaded #f)))
            (display "4. Roundtrip (fuel=2): ")
            (let ([result (test-ffi-roundtrip 1 2 3 2)])
                 (display result)
                 (newline)
                 (unless (equal? result '(out-of-fuel))
                         (display "   FAIL: expected (out-of-fuel)\n")
                         (set! loaded #f)))
            (display "====\n")
            (if loaded
                (display "All tests passed!\n")
                (display "Some tests failed.\n"))
            loaded))))
