;;; core/lang/rust-mapping.ss — Scheme to Rust Type Mapping
;;; @module rust-mapping
;;; @requires types
;;;
;;; Defines how The Fold's types map to Rust types for codegen.
;;;
;;; Principles:
;;; - C-compatible ABI for FFI safety.
;;; - Fuel is always an implicit argument/return part.
;;; - Use Rust standard types where possible.

;;; scheme-type->rust : Type → String
;;; Convert a Fold type to its Rust string representation.
(define (scheme-type->rust t)
  (cond
   ;; Base Types
   [(eq? t 'Nat) "u64"]
   [(eq? t 'Int) "i64"]
   [(eq? t 'Bool) "bool"]
   [(eq? t 'Char) "char"]
   [(eq? t 'Unit) "()"]
   [(eq? t 'Void) "!"]
   [(eq? t 'String) "String"] ; Note: In FFI this might be *const c_char or similar,
   ; but for internal codegen we use String.
   [(eq? t 'Bytes) "Vec<u8>"]
   [(eq? t 'Hash) "[u8; 33]"]
   
   ;; Function Types
   [(and (pair? t) (eq? (car t) '->))
    (let* ([params (function-param-types t)]
           [ret (function-return-type t)]
           [rust-params (map scheme-type->rust params)]
           ;; Add fuel as the last parameter
           [params-with-fuel (append rust-params '("u64"))])
          (string-append "extern \"C\" fn("
                         (join-strings ", " params-with-fuel)
                         ") -> "
                         (scheme-type->rust ret)))]
   
   ;; Fallback
   [else (format "/* Unknown type: ~s */" t)]))
