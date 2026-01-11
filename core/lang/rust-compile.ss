;;; core/lang/rust-compile.ss — Rust Compilation Bridge
;;; @module rust-compile
;;; @requires prelude rust-codegen
;;;
;;; Provides high-level functions to compile generated Rust code.

(load "core/base/prelude.ss")
(load "core/lang/rust-codegen.ss")

;;; compile-rust-lib : String × String → (Result String Error)
;;; Emit code to a file and compile it as a dynamic library.
(define (compile-rust-lib name ir cost)
  (let* ([source-file (string-append name ".rs")]
         [lib-file (string-append "lib" name ".so")]
         [code (rust-emit ir cost)])
        
        ;; Write source
        (with-output-to-file source-file
                             (lambda () (display code))
                             '(replace))
        
        ;; Compile using rustc
        ;; Note: This is a simplified end-to-end test.
        ;; In production, this would use Cargo or a more robust FFI loader.
        (let ([status (system (format "rustc --crate-type cdylib ~a -o ~a" source-file lib-file))])
             (if (= status 0)
                 `(ok ,lib-file)
                 `(error compile-failed ,(format "Failed to compile ~a" source-file))))))

;;; cleanup-rust-lib : String → Void
(define (cleanup-rust-lib name)
  (system (format "rm -f ~a.rs lib~a.so" name name)))
