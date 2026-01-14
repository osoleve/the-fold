;;; shell/ffi/test-rust-loader.ss — Tests for Rust Function Loader
;;; Tests dynamic loading, type dispatch, and caching.

(load "core/testing/test-framework.ss")
(load "shell/ffi/rust-loader.ss")

;;; ====
;;; Test Setup
;;; ====

;;; Check if library is available
(define *lib-available* (accel-load!))

(define-test "library loads or skips gracefully"
  (assert-true (or *lib-available*
                   (begin
                    (display "[SKIP] Rust library not available\n")
                    #t))))

;;; ====
;;; Registry Tests (don't require library)
;;; ====

(test-group "Function Registry"
            
            (define-test "register function"
              (clear-rust-fns!)
              (rust-register-fn! "test_fn" '(i64 i64) 'i64)
              (assert-true (rust-fn-exists? "test_fn")))
            
            (define-test "list registered functions"
              (clear-rust-fns!)
              (rust-register-fn! "fn_a" '(i64) 'i64)
              (rust-register-fn! "fn_b" '(f64 f64) 'f64)
              (let ([fns (list-rust-fns)])
                   (assert-true (if (member "fn_a" fns) #t #f))
                   (assert-true (if (member "fn_b" fns) #t #f))
                   (assert-equal 2 (length fns))))
            
            (define-test "clear registry"
              (rust-register-fn! "temp_fn" '(i64) 'i64)
              (clear-rust-fns!)
              (assert-false (rust-fn-exists? "temp_fn"))
              (assert-equal 0 (length (list-rust-fns))))
            
            (define-test "function info returns correct data"
              (clear-rust-fns!)
              (rust-register-fn! "info_test" '(i64 f64) 'bool)
              (let ([info (rust-fn-info "info_test")])
                   (assert-true (pair? info))
                   (assert-equal "info_test" (cdr (assq 'name info)))
                   (assert-equal '(i64 f64) (cdr (assq 'param-types info)))
                   (assert-equal 'bool (cdr (assq 'ret-type info)))
                   (assert-false (cdr (assq 'bound? info)))))  ; not bound yet
            
            (define-test "loader status reports correctly"
              (clear-rust-fns!)
              (rust-register-fn! "stat_fn" '(i64) 'i64)
              (let ([status (rust-loader-status)])
                   (assert-true (pair? status))
                   (assert-equal 1 (cdr (assq 'registered-functions status)))))
            )

;;; ====
;;; Type Mapping Tests
;;; ====

(test-group "Type Mappings"
            
            (define-test "scheme-type->ftype for i64"
              (assert-equal 'integer-64 (scheme-type->ftype 'i64))
              (assert-equal 'integer-64 (scheme-type->ftype 'int))
              (assert-equal 'integer-64 (scheme-type->ftype 'integer)))
            
            (define-test "scheme-type->ftype for f64"
              (assert-equal 'double (scheme-type->ftype 'f64))
              (assert-equal 'double (scheme-type->ftype 'float))
              (assert-equal 'double (scheme-type->ftype 'real)))
            
            (define-test "scheme-type->ftype for bool"
              (assert-equal 'unsigned-8 (scheme-type->ftype 'bool))
              (assert-equal 'unsigned-8 (scheme-type->ftype 'boolean)))
            
            (define-test "scheme-type->ftype for u64"
              (assert-equal 'unsigned-64 (scheme-type->ftype 'u64))
              (assert-equal 'unsigned-64 (scheme-type->ftype 'unsigned)))
            
            (define-test "scheme-type->result-ftype"
              (assert-equal 'i64-result-t (scheme-type->result-ftype 'i64))
              (assert-equal 'f64-result-t (scheme-type->result-ftype 'f64))
              (assert-equal 'bool-result-t (scheme-type->result-ftype 'bool))
              (assert-equal 'u64-result-t (scheme-type->result-ftype 'u64)))
            )

;;; ====
;;; Live FFI Tests (require library)
;;; ====

(when *lib-available*
      
      (test-group "Live FFI Calls"
                  
                  (define-test "load and call i64 function"
                    (clear-rust-fns!)
                    ;; fold_add_test is in generated/add_test.rs
                    (let ([result (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)])
                         (assert-true (and (pair? result) (eq? (car result) 'ok))))
                    ;; Call it
                    (let ([result (rust-call "fold_add_test" 2 3 1000)])
                         (assert-true (pair? result))
                         (assert-equal 'ok (car result))
                         (assert-equal 5 (cadr result))  ; 2 + 3 = 5
                         (assert-equal 999 (caddr result))))  ; 1000 - 1 = 999
                  
                  (define-test "function is cached after first call"
                    (clear-rust-fns!)
                    (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
                    (rust-call "fold_add_test" 1 1 100)  ; trigger binding
                    (let ([info (rust-fn-info "fold_add_test")])
                         (assert-true (cdr (assq 'bound? info)))))
                  
                  (define-test "out-of-fuel returns error"
                    (clear-rust-fns!)
                    (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
                    (let ([result (rust-call "fold_add_test" 2 3 0)])  ; 0 fuel
                         (assert-equal '(out-of-fuel) result)))
                  
                  (define-test "wrong arg count errors"
                    (clear-rust-fns!)
                    (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
                    ;; Only 1 arg instead of 2 - wrap in lambda for assert-error
                    (assert-error (lambda () (rust-call "fold_add_test" 5 1000))))
                  
                  (define-test "unregistered function errors"
                    (clear-rust-fns!)
                    (assert-error (lambda () (rust-call "nonexistent_fn" 1 2 100))))
                  
                  (define-test "load-and-call convenience function"
                    (clear-rust-fns!)
                    (let ([result (rust-load-and-call "fold_add_test" '(i64 i64) 'i64 10 20 500)])
                         (assert-true (pair? result))
                         (assert-equal 'ok (car result))
                         (assert-equal 30 (cadr result))))  ; 10 + 20 = 30
                  )
      
      (test-group "Reload Functionality"
                  
                  (define-test "rust-reload! clears bindings but keeps registrations"
                    (clear-rust-fns!)
                    (rust-load-fn! "fold_add_test" '(i64 i64) 'i64)
                    (rust-call "fold_add_test" 1 1 100)  ; bind it
                    (assert-true (cdr (assq 'bound? (rust-fn-info "fold_add_test"))))
                    ;; Reload
                    (rust-reload!)
                    ;; Still registered but not bound
                    (assert-true (rust-fn-exists? "fold_add_test"))
                    (assert-false (cdr (assq 'bound? (rust-fn-info "fold_add_test")))))
                  )
      )

;;; ====
;;; Edge Cases
;;; ====

(test-group "Edge Cases"
            
            (define-test "status->result handles all status codes"
              (assert-equal '(ok 42 100) (status->result 1 42 100))
              (assert-equal '(out-of-fuel) (status->result 2 0 0))
              (assert-equal '(error runtime-error) (status->result 3 0 0))
              (assert-equal '(error unknown-status 99) (status->result 99 0 0)))
            )

;;; ====
;;; Run Tests
;;; ====

(run-all-tests)
