(load "boundary/ffi/ffi-core.ss")

(doc 'module 'rust-loader)
(doc 'description "Dynamic Loader for Generated Rust Functions — Provides dynamic loading, caching, and dispatch for generated Rust functions")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'requires '(ffi-core))

(doc 'design '(
  "Function registry tracks compiled functions by name"
  "Type-safe dispatch based on declared return type"
  "Lazy binding: foreign-procedure created on first call"
  "Automatic library reloading when functions added"))

(doc 'usage "
(rust-load-fn! \"fold_add_test\" '(i64 i64) 'i64)
(rust-call \"fold_add_test\" 2 3 1000)  ; → (ok 5 999)
")

(doc 'section 'type-safe-result-structs)
(doc 'note "Match Rust's #[repr(C)] result structs from lib.rs")

(define-ftype i64-result-t
  (struct
   [status unsigned-8]     ; 1=success, 2=out-of-fuel, 3=runtime-error
   [_pad   (array 7 unsigned-8)]  ; explicit padding for alignment
   [value  integer-64]     ; i64 result value
   [fuel   unsigned-64]))  ; remaining fuel

(define-ftype f64-result-t
  (struct
   [status unsigned-8]
   [_pad   (array 7 unsigned-8)]
   [value  double]
   [fuel   unsigned-64]))

(define-ftype bool-result-t
  (struct
   [status unsigned-8]
   [value  unsigned-8]     ; 0=false, 1=true
   [_pad   (array 6 unsigned-8)]
   [fuel   unsigned-64]))

(define-ftype u64-result-t
  (struct
   [status unsigned-8]
   [_pad   (array 7 unsigned-8)]
   [value  unsigned-64]
   [fuel   unsigned-64]))

;;; Layer 2: Buffer result type for operations that write to output buffers
(define-ftype buffer-result-t
  (struct
   [status unsigned-8]          ; 1=success, 2=out-of-fuel, 3=error, 4=buffer-overflow
   [_pad   (array 7 unsigned-8)]
   [bytes-written unsigned-64]  ; number of bytes written (usize)
   [fuel   unsigned-64]))

;;; ====
;;; Type Mappings
;;; ====

;;; Scheme type symbol → Chez ftype for parameters
;;;
;;; Bytevector types (bv, f64*, etc.) use u8* which allows passing
;;; Scheme bytevectors directly without copying. This is the key to
;;; zero-copy FFI performance.
;;;
;;; Function pointer types (fold-s2w6):
;;; (-> A B) maps to (* (function (A-ftype u64) B-ftype))
;;; The u64 is the fuel parameter added by The Fold calling convention.
(define (scheme-type->ftype type)
  (cond
   ;; Function pointer types: (-> A... R) → (* (function (A... u64) R))
   ;; fold-s2w6: Enable passing function pointers through FFI
   [(and (pair? type) (eq? (car type) '->))
    (let* ([type-args (cdr type)]       ; (A... R)
           [params (init type-args)]     ; A...
           [ret (last type-args)]        ; R
           [param-ftypes (map scheme-type->ftype params)]
           ;; Add fuel (u64) as last parameter per Fold calling convention
           [params-with-fuel (append param-ftypes '(unsigned-64))]
           [ret-ftype (scheme-type->ftype ret)])
      ;; Chez ftype for function pointer: (* (function (params...) ret))
      `(* (function ,params-with-fuel ,ret-ftype)))]
   ;; Scalar types
   [(memq type '(i64 int integer)) 'integer-64]
   [(memq type '(f64 float double real)) 'double]
   [(memq type '(bool boolean)) 'unsigned-8]  ; passed as u8
   [(memq type '(u64 unsigned)) 'unsigned-64]
   [(memq type '(u8 byte)) 'unsigned-8]       ; single byte
   [(eq? type 'i32) 'integer-32]
   [(eq? type 'f32) 'single-float]
   ;; Bytevector types - u8* accepts bytevectors directly (zero-copy!)
   [(memq type '(bv bytevector u8* bytes)) 'u8*]
   [(memq type '(f64* f64-bv)) 'u8*]   ; f64 bytevector (caller ensures alignment)
   [(memq type '(f32* f32-bv)) 'u8*]   ; f32 bytevector
   [(memq type '(i64* i64-bv)) 'u8*]   ; i64 bytevector
   ;; Raw pointer (for pre-allocated foreign memory)
   [(memq type '(ptr void* pointer)) 'void*]
   [else (error 'scheme-type->ftype "Unknown type" type)]))

;;; Scheme type symbol → result ftype
(define (scheme-type->result-ftype type)
  (case type
        [(i64 int integer) 'i64-result-t]
        [(f64 float double real) 'f64-result-t]
        [(bool boolean) 'bool-result-t]
        [(u64 unsigned) 'u64-result-t]
        [(buffer) 'buffer-result-t]
        [else (error 'scheme-type->result-ftype "Unknown type" type)]))

;;; Scheme type symbol → result struct size
(define (result-ftype-sizeof type)
  (case type
        [(i64 int integer) (ftype-sizeof i64-result-t)]
        [(f64 float double real) (ftype-sizeof f64-result-t)]
        [(bool boolean) (ftype-sizeof bool-result-t)]
        [(u64 unsigned) (ftype-sizeof u64-result-t)]
        [(buffer) (ftype-sizeof buffer-result-t)]
        [else (error 'result-ftype-sizeof "Unknown type" type)]))

;;; ====
;;; Function Registry
;;; ====

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

;;; ====
;;; Input Validation
;;; ====

;;; valid-c-identifier? : String → Boolean
;;; Validate that a name is a valid C identifier (letters, numbers, underscore).
;;; This prevents code injection via eval in build-foreign-proc.
(define (valid-c-identifier? s)
  (let ([len (string-length s)])
       (and (> len 0)
            (<= len 256)  ; Reasonable max length
            ;; First char must be letter or underscore
            (let ([c0 (string-ref s 0)])
                 (or (char-alphabetic? c0) (char=? c0 #\_)))
            ;; Rest must be alphanumeric or underscore
            (let loop ([i 1])
                 (if (>= i len)
                     #t
                     (let ([c (string-ref s i)])
                          (if (or (char-alphabetic? c)
                                  (char-numeric? c)
                                  (char=? c #\_))
                              (loop (+ i 1))
                              #f)))))))

;;; ====
;;; Dynamic Binding
;;; ====

;;; build-foreign-proc : String × (List Symbol) × Symbol → Procedure
;;; Create a foreign-procedure binding for a Rust function.
;;; The Rust function signature is: fn(params..., fuel: u64, out: *mut ResultT)
;;;
;;; Note: foreign-procedure is a special form requiring literal types.
;;; We use eval to construct the binding dynamically.
;;; SECURITY: name is validated to be a valid C identifier to prevent code injection.
(define (build-foreign-proc name param-types ret-type)
  ;; SECURITY: Validate function name before using in eval
  (unless (and (string? name) (valid-c-identifier? name))
          (error 'build-foreign-proc "Invalid function name (must be valid C identifier)" name))
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

;;; ====
;;; rust-load-fn! — Public API for Loading
;;; ====

;;; rust-load-fn! : String × (List Symbol) × Symbol → (Result #t Error)
;;; Register and optionally bind a Rust function.
;;; - name: the Rust function name (e.g., "fold_add_test")
;;; - param-types: list of parameter types (e.g., '(i64 i64))
;;; - ret-type: return type (e.g., 'i64)
;;;
;;; Example:
;;; (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
(define (rust-load-fn! name param-types ret-type)
  ;; SECURITY: Validate function name at entry point
  (unless (and (string? name) (valid-c-identifier? name))
          (error 'rust-load-fn! "Invalid function name (must be valid C identifier)" name))

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

;;; ====
;;; rust-call — Public API for Calling
;;; ====

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
        [(buffer) (rust-call-buffer proc args fuel)]
        [else (error 'rust-call-with-result "Unknown return type" ret-type)]))

;;; ====
;;; Type-Specific Call Implementations
;;; ====

;;; rust-call-i64 : Proc × Args × Fuel → Result
;;; BUGFIX: Use dynamic-wind to ensure foreign memory is freed on exception
(define (rust-call-i64 proc args fuel)
  (let* ([result-addr (foreign-alloc (ftype-sizeof i64-result-t))]
         [result-ptr (make-ftype-pointer i64-result-t result-addr)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           ;; Call: (proc arg1 arg2 ... fuel result-ptr)
           (apply proc (append args (list fuel result-ptr)))
           ;; Extract and return
           (let ([status (ftype-ref i64-result-t (status) result-ptr)]
                 [value (ftype-ref i64-result-t (value) result-ptr)]
                 [fuel-out (ftype-ref i64-result-t (fuel) result-ptr)])
                (status->result status value fuel-out)))
         (lambda ()
           (foreign-free result-addr)))))

;;; rust-call-f64 : Proc × Args × Fuel → Result
;;; BUGFIX: Use dynamic-wind to ensure foreign memory is freed on exception
(define (rust-call-f64 proc args fuel)
  (let* ([result-addr (foreign-alloc (ftype-sizeof f64-result-t))]
         [result-ptr (make-ftype-pointer f64-result-t result-addr)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (apply proc (append args (list fuel result-ptr)))
           (let ([status (ftype-ref f64-result-t (status) result-ptr)]
                 [value (ftype-ref f64-result-t (value) result-ptr)]
                 [fuel-out (ftype-ref f64-result-t (fuel) result-ptr)])
                (status->result status value fuel-out)))
         (lambda ()
           (foreign-free result-addr)))))

;;; rust-call-bool : Proc × Args × Fuel → Result
;;; BUGFIX: Use dynamic-wind to ensure foreign memory is freed on exception
(define (rust-call-bool proc args fuel)
  (let* ([result-addr (foreign-alloc (ftype-sizeof bool-result-t))]
         [result-ptr (make-ftype-pointer bool-result-t result-addr)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (apply proc (append args (list fuel result-ptr)))
           (let ([status (ftype-ref bool-result-t (status) result-ptr)]
                 [value (ftype-ref bool-result-t (value) result-ptr)]
                 [fuel-out (ftype-ref bool-result-t (fuel) result-ptr)])
                ;; Convert u8 to boolean
                (status->result status (not (= value 0)) fuel-out)))
         (lambda ()
           (foreign-free result-addr)))))

;;; rust-call-u64 : Proc × Args × Fuel → Result
;;; BUGFIX: Use dynamic-wind to ensure foreign memory is freed on exception
(define (rust-call-u64 proc args fuel)
  (let* ([result-addr (foreign-alloc (ftype-sizeof u64-result-t))]
         [result-ptr (make-ftype-pointer u64-result-t result-addr)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (apply proc (append args (list fuel result-ptr)))
           (let ([status (ftype-ref u64-result-t (status) result-ptr)]
                 [value (ftype-ref u64-result-t (value) result-ptr)]
                 [fuel-out (ftype-ref u64-result-t (fuel) result-ptr)])
                (status->result status value fuel-out)))
         (lambda ()
           (foreign-free result-addr)))))

;;; rust-call-buffer : Proc × Args × Fuel → Result
;;; Call procedure that returns BufferResult (for operations that write to output buffers).
;;; Returns (ok bytes-written fuel-out) on success.
(define (rust-call-buffer proc args fuel)
  (let* ([result-addr (foreign-alloc (ftype-sizeof buffer-result-t))]
         [result-ptr (make-ftype-pointer buffer-result-t result-addr)])
        (dynamic-wind
         (lambda () #f)
         (lambda ()
           (apply proc (append args (list fuel result-ptr)))
           (let ([status (ftype-ref buffer-result-t (status) result-ptr)]
                 [bytes-written (ftype-ref buffer-result-t (bytes-written) result-ptr)]
                 [fuel-out (ftype-ref buffer-result-t (fuel) result-ptr)])
                (status->result status bytes-written fuel-out)))
         (lambda ()
           (foreign-free result-addr)))))

;;; status->result : Status × Value × Fuel → Result
;;; Convert status code to Scheme result.
(define (status->result status value fuel-out)
  (case status
        [(1) `(ok ,value ,fuel-out)]
        [(2) '(out-of-fuel)]
        [(3) '(error runtime-error)]
        [(4) '(error buffer-overflow)]
        [else `(error unknown-status ,status)]))

;;; ====
;;; Convenience Functions
;;; ====

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

;;; ====
;;; Diagnostic Functions
;;; ====

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

;;; ====
;;; Callback Support (fold-s2w6)
;;; ====
;;;
;;; These functions enable passing Scheme procedures as callbacks to Rust.
;;; The Scheme procedure is wrapped via foreign-callable to produce a
;;; C-compatible function pointer that Rust can call.
;;;
;;; Usage:
;;; (define double-fn (lambda (x fuel) (* x 2)))
;;; (define cb (make-callback double-fn '(i64) 'i64))
;;; ;; Pass cb to a Rust function expecting extern "C" fn(i64, u64) -> i64

;;; *callback-registry* : Hashtable
;;; Tracks live callbacks to prevent GC from collecting them.
;;; Keys are callback addresses (integers), values are the code objects.
(define *callback-registry* (make-eq-hashtable))

;;; make-callback : Procedure × (List Type) × Type → Integer
;;; Create a C-callable function pointer from a Scheme procedure.
;;;
;;; The Scheme procedure should have signature: (arg1 arg2 ... fuel) → result
;;; where fuel is u64 (remaining computation budget).
;;;
;;; Returns an integer representing the C function pointer address.
;;; This can be passed directly to Rust functions expecting fn pointers.
;;;
;;; IMPORTANT: The callback is registered to prevent GC. Call free-callback
;;; when the callback is no longer needed.
;;;
;;; Note: foreign-callable is a special form requiring literal types.
;;; We use eval to construct the callable dynamically.
(define (make-callback proc param-types ret-type)
  (let* ([param-ftypes (map scheme-type->ftype param-types)]
         ;; Add fuel as last parameter (Fold calling convention)
         [params-with-fuel (append param-ftypes '(unsigned-64))]
         [ret-ftype (scheme-type->ftype ret-type)]
         ;; Create the foreign-callable code object using eval
         [code (eval `(foreign-callable ,proc ',params-with-fuel ',ret-ftype))]
         ;; Get the entry point (C function pointer)
         [addr (foreign-callable-entry-point code)])
    ;; Lock the code object to prevent GC
    (lock-object code)
    ;; Register for later cleanup
    (eq-hashtable-set! *callback-registry* addr code)
    ;; Return the address as an integer
    addr))

;;; free-callback : Integer → Void
;;; Release a callback created with make-callback.
;;; This unlocks the code object and removes it from the registry.
(define (free-callback addr)
  (let ([code (eq-hashtable-ref *callback-registry* addr #f)])
    (when code
      (unlock-object code)
      (eq-hashtable-delete! *callback-registry* addr))))

;;; list-callbacks : → (List Integer)
;;; List all registered callback addresses.
(define (list-callbacks)
  (let-values ([(keys vals) (hashtable-entries *callback-registry*)])
    (vector->list keys)))
