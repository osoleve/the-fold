;;; shell/ffi/rust-loader.ss — Dynamic Loader for Generated Rust Functions
;;; @module rust-loader
;;; @requires ffi-core
;;;
;;; Provides dynamic loading, caching, and dispatch for generated Rust functions.
;;; This module bridges the codegen output with runtime invocation.
;;;
;;; Design:
;;; - Function registry tracks compiled functions by name
;;; - Type-safe dispatch based on declared return type
;;; - Lazy binding: foreign-procedure created on first call
;;; - Automatic library reloading when functions added
;;;
;;; Usage:
;;; (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
;;; (rust-call "fold_add_test" 2 3 1000)  ; → (ok 5 999)

(load "shell/ffi/ffi-core.ss")

;;; ============================================================
;;; Type-Safe Result Structs
;;; ============================================================

;;; Match Rust's #[repr(C)] result structs from lib.rs

(define-ftype i64-result-t
  (struct
   [status unsigned-8]     ; 1=success, 2=out-of-fuel, 3=runtime-error
   [value  integer-64]     ; i64 result value
   [fuel   unsigned-64]))  ; remaining fuel

(define-ftype f64-result-t
  (struct
   [status unsigned-8]
   [value  double]
   [fuel   unsigned-64]))

(define-ftype bool-result-t
  (struct
   [status unsigned-8]
   [value  unsigned-8]     ; 0=false, 1=true
   [fuel   unsigned-64]))

(define-ftype u64-result-t
  (struct
   [status unsigned-8]
   [value  unsigned-64]
   [fuel   unsigned-64]))

;;; ============================================================
;;; Type Mappings
;;; ============================================================

;;; Scheme type symbol → Chez ftype for parameters
;;;
;;; Bytevector types (bv, f64*, etc.) use u8* which allows passing
;;; Scheme bytevectors directly without copying. This is the key to
;;; zero-copy FFI performance.
(define (scheme-type->ftype type)
  (case type
        ;; Scalar types
        [(i64 int integer) 'integer-64]
        [(f64 float double real) 'double]
        [(bool boolean) 'unsigned-8]  ; passed as u8
        [(u64 unsigned) 'unsigned-64]
        [(i32) 'integer-32]
        [(f32) 'single-float]
        ;; Bytevector types - u8* accepts bytevectors directly (zero-copy!)
        [(bv bytevector u8* bytes) 'u8*]
        [(f64* f64-bv) 'u8*]   ; f64 bytevector (caller ensures alignment)
        [(f32* f32-bv) 'u8*]   ; f32 bytevector
        [(i64* i64-bv) 'u8*]   ; i64 bytevector
        ;; Raw pointer (for pre-allocated foreign memory)
        [(ptr void* pointer) 'void*]
        [else (error 'scheme-type->ftype "Unknown type" type)]))

;;; Scheme type symbol → result ftype
(define (scheme-type->result-ftype type)
  (case type
        [(i64 int integer) 'i64-result-t]
        [(f64 float double real) 'f64-result-t]
        [(bool boolean) 'bool-result-t]
        [(u64 unsigned) 'u64-result-t]
        [else (error 'scheme-type->result-ftype "Unknown type" type)]))

;;; Scheme type symbol → result struct size
(define (result-ftype-sizeof type)
  (case type
        [(i64 int integer) (ftype-sizeof i64-result-t)]
        [(f64 float double real) (ftype-sizeof f64-result-t)]
        [(bool boolean) (ftype-sizeof bool-result-t)]
        [(u64 unsigned) (ftype-sizeof u64-result-t)]
        [else (error 'result-ftype-sizeof "Unknown type" type)]))

;;; ============================================================
;;; Function Registry
;;; ============================================================

;;; Registry entry: (name param-types ret-type foreign-proc result-ftype-sym)
(define *rust-fn-registry* (make-hashtable string-hash string=?))

;;; rust-register-fn! : String × (List Symbol) × Symbol → Void
;;; Register a function without binding (for lazy loading).
(define (rust-register-fn! name param-types ret-type)
  (hashtable-set! *rust-fn-registry* name
                  (list name param-types ret-type #f)))

;;; rust-fn-entry : String → (List | #f)
;;; Get registry entry for a function.
(define (rust-fn-entry name)
  (hashtable-ref *rust-fn-registry* name #f))

;;; rust-fn-exists? : String → Boolean
;;; Check if function is registered.
(define (rust-fn-exists? name)
  (hashtable-contains? *rust-fn-registry* name))

;;; list-rust-fns : → (List String)
;;; List all registered function names.
(define (list-rust-fns)
  (let-values ([(keys vals) (hashtable-entries *rust-fn-registry*)])
              (vector->list keys)))

;;; clear-rust-fns! : → Void
;;; Clear the function registry (for testing/reloading).
(define (clear-rust-fns!)
  (hashtable-clear! *rust-fn-registry*))

;;; ============================================================
;;; Dynamic Binding
;;; ============================================================

;;; build-foreign-proc : String × (List Symbol) × Symbol → Procedure
;;; Create a foreign-procedure binding for a Rust function.
;;; The Rust function signature is: fn(params..., fuel: u64, out: *mut ResultT)
;;;
;;; Note: foreign-procedure is a special form requiring literal types.
;;; We use eval to construct the binding dynamically.
(define (build-foreign-proc name param-types ret-type)
  (let* ([param-ftypes (map scheme-type->ftype param-types)]
         [result-ftype-sym (scheme-type->result-ftype ret-type)]
         ;; Full signature: params + fuel + out-pointer
         [full-sig (append param-ftypes
                           '(unsigned-64)            ; fuel_in
                           (list (list '* result-ftype-sym)))])  ; out-pointer
        (eval `(foreign-procedure ,name ,full-sig void))))

;;; ensure-bound! : String → Procedure
;;; Ensure function is bound, return the foreign procedure.
(define (ensure-bound! name)
  (let ([entry (rust-fn-entry name)])
       (unless entry
               (error 'ensure-bound! "Function not registered" name))
       (let ([cached-proc (cadddr entry)])
            (if cached-proc
                cached-proc
                ;; Lazy bind
                (let* ([param-types (cadr entry)]
                       [ret-type (caddr entry)]
                       [proc (build-foreign-proc name param-types ret-type)])
                      ;; Update registry with bound procedure
                      (hashtable-set! *rust-fn-registry* name
                                      (list name param-types ret-type proc))
                      proc)))))

;;; ============================================================
;;; rust-load-fn! — Public API for Loading
;;; ============================================================

;;; rust-load-fn! : String × (List Symbol) × Symbol → (Result #t Error)
;;; Register and optionally bind a Rust function.
;;; - name: the Rust function name (e.g., "fold_add_test")
;;; - param-types: list of parameter types (e.g., '(i64 i64))
;;; - ret-type: return type (e.g., 'i64)
;;;
;;; Example:
;;; (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
(define (rust-load-fn! name param-types ret-type)
  ;; Ensure library is loaded
  (unless (accel-available?)
          (unless (accel-load!)
                  (error 'rust-load-fn! "Cannot load acceleration library")))
  
  ;; Register the function
  (rust-register-fn! name param-types ret-type)
  
  ;; Eagerly bind (could be lazy, but eager catches errors early)
  (guard (ex [else `(error bind-failed ,(condition-message ex))])
         (ensure-bound! name)
         '(ok #t)))

;;; ============================================================
;;; rust-call — Public API for Calling
;;; ============================================================

;;; rust-call : String × Any... × Nat → (Result Value Nat) | (Error)
;;; Call a registered Rust function with fuel tracking.
;;; Last argument must be fuel.
;;;
;;; Example:
;;; (rust-call "fold_add_test" 2 3 1000)  ; → (ok 5 999)
(define (rust-call name . args-with-fuel)
  ;; Validate arguments
  (when (null? args-with-fuel)
        (error 'rust-call "Missing arguments (need at least fuel)"))
  
  ;; Split args and fuel (fuel is last)
  (let* ([all-args (reverse args-with-fuel)]
         [fuel (car all-args)]
         [args (reverse (cdr all-args))])
        
        ;; Get function entry
        (let ([entry (rust-fn-entry name)])
             (unless entry
                     (error 'rust-call "Function not registered" name))
             
             ;; Validate argument count
             (let* ([param-types (cadr entry)]
                    [ret-type (caddr entry)]
                    [expected-count (length param-types)])
                   (unless (= (length args) expected-count)
                           (error 'rust-call
                                  (format "Expected ~a args, got ~a"
                                          expected-count (length args))
                                  name))
                   
                   ;; Get/bind the foreign procedure
                   (let ([proc (ensure-bound! name)])
                        ;; Allocate result struct
                        (rust-call-with-result name proc args fuel ret-type))))))

;;; rust-call-with-result : String × Proc × Args × Fuel × Type → Result
;;; Internal: call procedure and extract typed result.
(define (rust-call-with-result name proc args fuel ret-type)
  (case ret-type
        [(i64 int integer) (rust-call-i64 proc args fuel)]
        [(f64 float double real) (rust-call-f64 proc args fuel)]
        [(bool boolean) (rust-call-bool proc args fuel)]
        [(u64 unsigned) (rust-call-u64 proc args fuel)]
        [else (error 'rust-call-with-result "Unknown return type" ret-type)]))

;;; ============================================================
;;; Type-Specific Call Implementations
;;; ============================================================

;;; rust-call-i64 : Proc × Args × Fuel → Result
(define (rust-call-i64 proc args fuel)
  (let* ([result-ptr (make-ftype-pointer i64-result-t
                                         (foreign-alloc (ftype-sizeof i64-result-t)))])
        ;; Call: (proc arg1 arg2 ... fuel result-ptr)
        (apply proc (append args (list fuel result-ptr)))
        
        ;; Extract and return
        (let ([status (ftype-ref i64-result-t (status) result-ptr)]
              [value (ftype-ref i64-result-t (value) result-ptr)]
              [fuel-out (ftype-ref i64-result-t (fuel) result-ptr)])
             (foreign-free (ftype-pointer-address result-ptr))
             (status->result status value fuel-out))))

;;; rust-call-f64 : Proc × Args × Fuel → Result
(define (rust-call-f64 proc args fuel)
  (let* ([result-ptr (make-ftype-pointer f64-result-t
                                         (foreign-alloc (ftype-sizeof f64-result-t)))])
        (apply proc (append args (list fuel result-ptr)))
        
        (let ([status (ftype-ref f64-result-t (status) result-ptr)]
              [value (ftype-ref f64-result-t (value) result-ptr)]
              [fuel-out (ftype-ref f64-result-t (fuel) result-ptr)])
             (foreign-free (ftype-pointer-address result-ptr))
             (status->result status value fuel-out))))

;;; rust-call-bool : Proc × Args × Fuel → Result
(define (rust-call-bool proc args fuel)
  (let* ([result-ptr (make-ftype-pointer bool-result-t
                                         (foreign-alloc (ftype-sizeof bool-result-t)))])
        (apply proc (append args (list fuel result-ptr)))
        
        (let ([status (ftype-ref bool-result-t (status) result-ptr)]
              [value (ftype-ref bool-result-t (value) result-ptr)]
              [fuel-out (ftype-ref bool-result-t (fuel) result-ptr)])
             (foreign-free (ftype-pointer-address result-ptr))
             ;; Convert u8 to boolean
             (status->result status (not (= value 0)) fuel-out))))

;;; rust-call-u64 : Proc × Args × Fuel → Result
(define (rust-call-u64 proc args fuel)
  (let* ([result-ptr (make-ftype-pointer u64-result-t
                                         (foreign-alloc (ftype-sizeof u64-result-t)))])
        (apply proc (append args (list fuel result-ptr)))
        
        (let ([status (ftype-ref u64-result-t (status) result-ptr)]
              [value (ftype-ref u64-result-t (value) result-ptr)]
              [fuel-out (ftype-ref u64-result-t (fuel) result-ptr)])
             (foreign-free (ftype-pointer-address result-ptr))
             (status->result status value fuel-out))))

;;; status->result : Status × Value × Fuel → Result
;;; Convert status code to Scheme result.
(define (status->result status value fuel-out)
  (case status
        [(1) `(ok ,value ,fuel-out)]
        [(2) '(out-of-fuel)]
        [(3) '(error runtime-error)]
        [else `(error unknown-status ,status)]))

;;; ============================================================
;;; Convenience Functions
;;; ============================================================

;;; rust-load-and-call : String × (List Symbol) × Symbol × Args... × Fuel → Result
;;; One-shot: register, bind, and call in one step.
(define (rust-load-and-call name param-types ret-type . args-with-fuel)
  (rust-load-fn! name param-types ret-type)
  (apply rust-call name args-with-fuel))

;;; rust-reload! : → Void
;;; Force rebinding of all registered functions.
;;; Use after rebuilding the Rust library.
(define (rust-reload!)
  ;; Clear bindings but keep registrations
  (let-values ([(keys vals) (hashtable-entries *rust-fn-registry*)])
              (vector-for-each
               (lambda (name entry)
                       (hashtable-set! *rust-fn-registry* name
                                       (list (car entry)
                                             (cadr entry)
                                             (caddr entry)
                                             #f)))  ; clear proc
               keys vals))
  ;; Force library reload
  (set! *accel-lib-loaded* #f)
  (accel-load!))

;;; ============================================================
;;; Diagnostic Functions
;;; ============================================================

;;; rust-fn-info : String → Alist | #f
;;; Get information about a registered function.
(define (rust-fn-info name)
  (let ([entry (rust-fn-entry name)])
       (and entry
            `((name . ,(car entry))
              (param-types . ,(cadr entry))
              (ret-type . ,(caddr entry))
              (bound? . ,(if (cadddr entry) #t #f))))))

;;; rust-loader-status : → Alist
;;; Get loader status summary.
(define (rust-loader-status)
  `((library-loaded? . ,(accel-available?))
    (library-path . ,(accel-library-path))
    (registered-functions . ,(hashtable-size *rust-fn-registry*))
    (function-names . ,(list-rust-fns))))
