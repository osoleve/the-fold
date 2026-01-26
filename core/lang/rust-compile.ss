;;; core/lang/rust-compile.ss — Rust Compilation Bridge
;;; @module rust-compile
;;; @requires prelude rust-codegen
;;;
;;; Provides high-level functions to compile generated Rust code.
;;; Supports both standalone compilation (rustc) and crate integration (cargo).

(load "core/base/prelude.ss")
(load "core/lang/rust-codegen.ss")

;;; ====
;;; Configuration
;;; ====

;;; Path to the rust-accel crate (relative to project root)
(define *rust-accel-path* "boundary/ffi/rust-accel")

;;; Path to the generated modules directory
(define *rust-generated-path*
  (string-append *rust-accel-path* "/src/generated"))

;;; ====
;;; Standalone Compilation (rustc)
;;; ====

;;; compile-rust-lib : String × (List α) [× Nat] → (Result String Error)
;;; Emit code to a file and compile it as a dynamic library.
;;; This is for standalone testing - use compile-to-crate for production.
(define (compile-rust-lib name ir . rest)
  (let* ([cost (if (null? rest) #f (car rest))]
         [source-file (string-append name ".rs")]
         [lib-file (string-append "lib" name ".so")]
         [code (if cost (rust-emit ir cost) (rust-emit ir))])
        
        ;; Write source
        (with-output-to-file source-file
                             (lambda () (display code))
                             '(replace))
        
        ;; Compile using rustc
        (let ([status (system (format "rustc --crate-type cdylib ~a -o ~a 2>&1" source-file lib-file))])
             (if (= status 0)
                 `(ok ,lib-file)
                 `(error compile-failed ,(format "Failed to compile ~a" source-file))))))

;;; cleanup-rust-lib : String → Void
;;; Remove standalone compilation artifacts.
(define (cleanup-rust-lib name)
  (system (format "rm -f ~a.rs lib~a.so" name name)))

;;; ====
;;; Crate Integration (cargo)
;;; ====

;;; compile-to-crate : String × (List α) [× Nat] → (Result String Error)
;;; Generate Rust code and add it to the rust-accel crate.
;;; Returns the path to the generated .rs file.
(define (compile-to-crate name ir . rest)
  (let* ([cost (if (null? rest) #f (car rest))]
         [module-name (sanitize-rust-ident name)]  ; Use robust sanitization from rust-codegen
         [source-file (string-append *rust-generated-path* "/" module-name ".rs")]
         [code (if cost (rust-emit-module ir cost) (rust-emit-module ir))])
        
        ;; Write source to generated directory
        (with-output-to-file source-file
                             (lambda () (display code))
                             '(replace))
        
        ;; Update mod.rs to include the new module
        (update-generated-mod-rs! module-name)
        
        `(ok ,source-file)))

;;; update-generated-mod-rs! : String → Void
;;; Add a module declaration to generated/mod.rs if not already present.
(define (update-generated-mod-rs! module-name)
  (let* ([mod-rs-path (string-append *rust-generated-path* "/mod.rs")]
         [mod-decl (format "pub mod ~a;" module-name)]
         [current-content (call-with-input-file mod-rs-path
                                                (lambda (p) (get-string-all p)))])
        
        ;; Only add if not already present
        (unless (string-contains current-content mod-decl)
                (with-output-to-file mod-rs-path
                                     (lambda ()
                                             (display current-content)
                                             (display "\n")
                                             (display mod-decl)
                                             (display "\n"))
                                     '(replace)))))

;;; string-contains : String × String → Boolean
;;; Check if haystack contains needle.
(define (string-contains haystack needle)
  (let* ([h-len (string-length haystack)]
         [n-len (string-length needle)])
        (let loop ([i 0])
             (cond
              [(> (+ i n-len) h-len) #f]
              [(string=? (substring haystack i (+ i n-len)) needle) #t]
              [else (loop (+ i 1))]))))

;;; list-generated-modules : → (List String)
;;; List all generated modules in the crate.
(define (list-generated-modules)
  (let* ([mod-rs-path (string-append *rust-generated-path* "/mod.rs")]
         [content (call-with-input-file mod-rs-path
                                        (lambda (p) (get-string-all p)))]
         [lines (string-split content #\newline)])
        (filter-map
         (lambda (line)
                 (let ([trimmed (string-trim line)])
                      (and (string-prefix? "pub mod " trimmed)
                           (string-suffix? ";" trimmed)
                           (substring trimmed 8 (- (string-length trimmed) 1)))))
         lines)))

;;; remove-from-crate : String → (Result #t Error)
;;; Remove a generated module from the crate.
;;; Deletes the .rs file and removes from mod.rs.
(define (remove-from-crate name)
  (let* ([module-name (sanitize-rust-ident name)]
         [source-file (string-append *rust-generated-path* "/" module-name ".rs")]
         [mod-decl (format "pub mod ~a;" module-name)])
        
        ;; Remove from mod.rs
        (let* ([mod-rs-path (string-append *rust-generated-path* "/mod.rs")]
               [content (call-with-input-file mod-rs-path
                                              (lambda (p) (get-string-all p)))]
               [lines (string-split content #\newline)]
               [filtered (filter (lambda (line)
                                         (not (string=? (string-trim line) mod-decl)))
                                 lines)]
               [new-content (string-join-lines filtered)])
              (with-output-to-file mod-rs-path
                                   (lambda () (display new-content))
                                   '(replace)))
        
        ;; Delete the .rs file
        (when (file-exists? source-file)
              (delete-file source-file))
        
        '(ok #t)))

;;; string-join-lines : (List String) → String
;;; Join lines with newlines.
(define (string-join-lines lines)
  (if (null? lines)
      ""
      (let loop ([rest (cdr lines)] [acc (car lines)])
           (if (null? rest)
               acc
               (loop (cdr rest) (string-append acc "\n" (car rest)))))))

;;; string-prefix? : String × String → Boolean
(define (string-prefix? prefix str)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; string-suffix? : String × String → Boolean
(define (string-suffix? suffix str)
  (and (>= (string-length str) (string-length suffix))
       (string=? (substring str (- (string-length str) (string-length suffix))
                            (string-length str))
                 suffix)))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (let* ([chars (string->list str)]
         [trimmed (reverse (drop-while char-whitespace?
                                       (reverse (drop-while char-whitespace? chars))))])
        (list->string trimmed)))

;;; drop-while : (α → Boolean) × (List α) → (List α)
(define (drop-while pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (drop-while pred (cdr lst))]
   [else lst]))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (let loop ([chars (string->list str)] [current '()] [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) delim)
         (loop (cdr chars) '() (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; filter-map provided by prelude

;;; ====
;;; Build Commands
;;; ====

;;; rust-build! : → (Result String Error)
;;; Build the rust-accel crate with cargo.
(define (rust-build!)
  (let ([status (system (format "cd ~a && cargo build --release 2>&1" *rust-accel-path*))])
       (if (= status 0)
           `(ok ,(string-append *rust-accel-path* "/target/release/libfold_accel.so"))
           `(error build-failed "cargo build --release failed"))))

;;; rust-build-debug! : → (Result String Error)
;;; Build the rust-accel crate in debug mode.
(define (rust-build-debug!)
  (let ([status (system (format "cd ~a && cargo build 2>&1" *rust-accel-path*))])
       (if (= status 0)
           `(ok ,(string-append *rust-accel-path* "/target/debug/libfold_accel.so"))
           `(error build-failed "cargo build failed"))))

;;; rust-test! : → (Result String Error)
;;; Run the rust-accel crate tests.
(define (rust-test!)
  (let ([status (system (format "cd ~a && cargo test 2>&1" *rust-accel-path*))])
       (if (= status 0)
           '(ok "tests passed")
           `(error test-failed "cargo test failed"))))

;;; rust-clean! : → Void
;;; Clean the rust-accel crate build artifacts.
(define (rust-clean!)
  (system (format "cd ~a && cargo clean" *rust-accel-path*)))

;;; ====
;;; End-to-End Compilation
;;; ====

;;; compile-and-build! : String × (List α) [× Nat] → (Result String Error)
;;; Generate code, add to crate, and rebuild.
;;; Returns the path to the built .so file on success.
(define (compile-and-build! name ir . rest)
  (let ([compile-result (apply compile-to-crate name ir rest)])
       (if (eq? (car compile-result) 'ok)
           (rust-build!)
           compile-result)))
