;;; shell/ffi/posix-ffi.ss — POSIX FFI Bindings
;;;
;;; Provides access to POSIX system calls via Rust FFI:
;;; - getpid: true OS process ID
;;; - flock: advisory file locking (with automatic cleanup on process death)
;;; - open/close: low-level file descriptor operations with O_EXCL
;;;
;;; IMPORTANT: Always use LOCK_NB (non-blocking) with flock!
;;; Blocking flock would hang the Chez runtime (cooperative threading).
;;;
;;; Usage:
;;;   (posix-load!)                      ; Load FFI library
;;;   (posix-getpid)                     ; Get real OS PID
;;;   (posix-open path flags mode)       ; Open file, return fd
;;;   (posix-close fd)                   ; Close file descriptor
;;;   (posix-flock fd op)                ; Advisory locking
;;;   (with-flock-lock path thunk)       ; High-level: lock file during thunk
;;;
;;; This is Shell code: impure (system calls, FFI).

(load "shell/ffi/ffi-core.ss")

;;; ====
;;; FFI Type Definitions
;;; ====

;;; Result struct for int-returning POSIX calls
;;; Matches Rust's IntResult
(define-ftype posix-int-result-t
  (struct
   [status unsigned-8]     ; 0=success, 1=error
   [value  integer-32]     ; result value (fd, pid, etc.)
   [error-code integer-32])) ; errno if status != 0

;;; Result struct for status-only POSIX calls
;;; Matches Rust's StatusResult
(define-ftype posix-status-result-t
  (struct
   [status unsigned-8]     ; 0=success, 1=error
   [error-code integer-32])) ; errno if status != 0

;;; ====
;;; POSIX Constants
;;; ====

;;; flock operation constants
(define LOCK_SH 1)   ; Shared lock
(define LOCK_EX 2)   ; Exclusive lock
(define LOCK_NB 4)   ; Non-blocking (CRITICAL: always use this!)
(define LOCK_UN 8)   ; Unlock

;;; open flag constants
(define O_CREAT #o100)   ; Create file if not exists
(define O_EXCL  #o200)   ; Fail if file exists
(define O_RDWR  #o2)     ; Read-write access
(define O_RDONLY 0)      ; Read-only access
(define O_WRONLY 1)      ; Write-only access
(define O_CLOEXEC #o2000000) ; Close on exec (prevents FD inheritance)

;;; errno constants
(define EWOULDBLOCK 11)  ; Non-blocking lock would block
(define EEXIST 17)       ; File exists (O_EXCL)

;;; ====
;;; Foreign Procedure Bindings
;;; ====

(define *posix-bound* #f)

(define rust-posix-getpid #f)
(define rust-posix-open #f)
(define rust-posix-close #f)
(define rust-posix-flock #f)

;;; Get POSIX constants from Rust (ensures compatibility)
(define rust-posix-lock-sh #f)
(define rust-posix-lock-ex #f)
(define rust-posix-lock-nb #f)
(define rust-posix-lock-un #f)
(define rust-posix-o-creat #f)
(define rust-posix-o-excl #f)
(define rust-posix-o-rdwr #f)
(define rust-posix-ewouldblock #f)
(define rust-posix-eexist #f)
(define rust-posix-o-cloexec #f)

;;; bind-posix-procedures! : → Void
;;; Bind POSIX foreign procedures
(define (bind-posix-procedures!)
  (when (not *posix-bound*)
    ;; getpid
    (set! rust-posix-getpid
          (foreign-procedure "fold_posix_getpid"
                             ((* posix-int-result-t))
                             void))

    ;; open(path, flags, mode)
    (set! rust-posix-open
          (foreign-procedure "fold_posix_open"
                             (u8* size_t integer-32 unsigned-32 (* posix-int-result-t))
                             void))

    ;; close(fd)
    (set! rust-posix-close
          (foreign-procedure "fold_posix_close"
                             (integer-32 (* posix-status-result-t))
                             void))

    ;; flock(fd, operation)
    (set! rust-posix-flock
          (foreign-procedure "fold_posix_flock"
                             (integer-32 integer-32 (* posix-status-result-t))
                             void))

    ;; Constants
    (set! rust-posix-lock-sh
          (foreign-procedure "fold_posix_lock_sh" () integer-32))
    (set! rust-posix-lock-ex
          (foreign-procedure "fold_posix_lock_ex" () integer-32))
    (set! rust-posix-lock-nb
          (foreign-procedure "fold_posix_lock_nb" () integer-32))
    (set! rust-posix-lock-un
          (foreign-procedure "fold_posix_lock_un" () integer-32))
    (set! rust-posix-o-creat
          (foreign-procedure "fold_posix_o_creat" () integer-32))
    (set! rust-posix-o-excl
          (foreign-procedure "fold_posix_o_excl" () integer-32))
    (set! rust-posix-o-rdwr
          (foreign-procedure "fold_posix_o_rdwr" () integer-32))
    (set! rust-posix-ewouldblock
          (foreign-procedure "fold_posix_ewouldblock" () integer-32))
    (set! rust-posix-eexist
          (foreign-procedure "fold_posix_eexist" () integer-32))
    (set! rust-posix-o-cloexec
          (foreign-procedure "fold_posix_o_cloexec" () integer-32))

    ;; Update Scheme constants to match Rust values
    (set! LOCK_SH (rust-posix-lock-sh))
    (set! LOCK_EX (rust-posix-lock-ex))
    (set! LOCK_NB (rust-posix-lock-nb))
    (set! LOCK_UN (rust-posix-lock-un))
    (set! O_CREAT (rust-posix-o-creat))
    (set! O_EXCL (rust-posix-o-excl))
    (set! O_RDWR (rust-posix-o-rdwr))
    (set! EWOULDBLOCK (rust-posix-ewouldblock))
    (set! EEXIST (rust-posix-eexist))
    (set! O_CLOEXEC (rust-posix-o-cloexec))

    (set! *posix-bound* #t)))

;;; posix-load! : → Boolean
;;; Load acceleration library and bind POSIX procedures
(define (posix-load!)
  (if *posix-bound*
      #t
      (if (accel-load!)
          (begin
            (bind-posix-procedures!)
            #t)
          #f)))

;;; posix-available? : → Boolean
;;; Check if POSIX FFI is available
(define (posix-available?)
  (and (accel-available?) *posix-bound*))

;;; ====
;;; Scheme Wrappers
;;; ====

;;; posix-getpid : → Int
;;; Get the real OS process ID
(define (posix-getpid)
  (unless (posix-available?)
    (error 'posix-getpid "POSIX FFI not loaded"))
  (let* ([result-ptr (make-ftype-pointer posix-int-result-t
                                         (foreign-alloc (ftype-sizeof posix-int-result-t)))])
    (rust-posix-getpid result-ptr)
    (let ([status (ftype-ref posix-int-result-t (status) result-ptr)]
          [value (ftype-ref posix-int-result-t (value) result-ptr)])
      (foreign-free (ftype-pointer-address result-ptr))
      (if (= status 0)
          value
          (error 'posix-getpid "getpid failed" status)))))

;;; posix-open : String × Int × Int → Int | (error errno)
;;; Open a file and return file descriptor
;;; Returns fd on success, (error errno) on failure
(define (posix-open path flags mode)
  (unless (posix-available?)
    (error 'posix-open "POSIX FFI not loaded"))
  (let* ([path-bv (string->utf8 path)]
         [result-ptr (make-ftype-pointer posix-int-result-t
                                         (foreign-alloc (ftype-sizeof posix-int-result-t)))])
    (rust-posix-open path-bv (bytevector-length path-bv) flags mode result-ptr)
    (let ([status (ftype-ref posix-int-result-t (status) result-ptr)]
          [value (ftype-ref posix-int-result-t (value) result-ptr)]
          [errno (ftype-ref posix-int-result-t (error-code) result-ptr)])
      (foreign-free (ftype-pointer-address result-ptr))
      (if (= status 0)
          value
          (list 'error errno)))))

;;; posix-close : Int → Boolean
;;; Close a file descriptor
;;; Returns #t on success, #f on failure
(define (posix-close fd)
  (unless (posix-available?)
    (error 'posix-close "POSIX FFI not loaded"))
  (let* ([result-ptr (make-ftype-pointer posix-status-result-t
                                         (foreign-alloc (ftype-sizeof posix-status-result-t)))])
    (rust-posix-close fd result-ptr)
    (let ([status (ftype-ref posix-status-result-t (status) result-ptr)])
      (foreign-free (ftype-pointer-address result-ptr))
      (= status 0))))

;;; posix-flock : Int × Int → Boolean | (would-block)
;;; Apply advisory lock to file descriptor
;;; Returns #t on success, (would-block) if EWOULDBLOCK, #f on other error
(define (posix-flock fd operation)
  (unless (posix-available?)
    (error 'posix-flock "POSIX FFI not loaded"))
  (let* ([result-ptr (make-ftype-pointer posix-status-result-t
                                         (foreign-alloc (ftype-sizeof posix-status-result-t)))])
    (rust-posix-flock fd operation result-ptr)
    (let ([status (ftype-ref posix-status-result-t (status) result-ptr)]
          [errno (ftype-ref posix-status-result-t (error-code) result-ptr)])
      (foreign-free (ftype-pointer-address result-ptr))
      (cond
       [(= status 0) #t]
       [(= errno EWOULDBLOCK) '(would-block)]
       [else #f]))))

;;; ====
;;; High-Level Locking API
;;; ====

;;; Lock file polling configuration
(define *flock-poll-interval-ms* 10)   ; How often to retry
(define *flock-timeout-ms* 5000)       ; Max time to wait

;;; flock-sleep-ms : Int → Void
;;; Sleep for milliseconds
(define (flock-sleep-ms ms)
  (sleep (make-time 'time-duration (* ms 1000000) 0)))

;;; with-flock-lock : String × (→ α) → α
;;; Execute thunk while holding an advisory flock on path.
;;; Uses non-blocking flock with retry loop to avoid hanging Chez runtime.
;;;
;;; The lock is automatically released when:
;;; - The thunk completes (normally or via exception)
;;; - The process dies (OS kernel handles cleanup)
;;;
;;; This is the recommended way to use flock from Scheme.
(define (with-flock-lock path thunk)
  (unless (posix-available?)
    (error 'with-flock-lock "POSIX FFI not loaded"))

  (let* ([lock-path (string-append path ".flock")]
         ;; O_CLOEXEC prevents child processes from inheriting the lock FD
         [fd-result (posix-open lock-path
                                (bitwise-ior O_CREAT O_RDWR O_CLOEXEC)
                                #o644)])
    (when (and (pair? fd-result) (eq? (car fd-result) 'error))
      (error 'with-flock-lock "Failed to open lock file" lock-path fd-result))

    (let ([fd fd-result]
          [deadline (+ (time-second (current-time))
                       (/ *flock-timeout-ms* 1000))])
      ;; Acquire lock with non-blocking retry
      (let acquire-loop ()
        (let ([result (posix-flock fd (bitwise-ior LOCK_EX LOCK_NB))])
          (cond
           [(eq? result #t) #t]  ; Lock acquired
           [(equal? result '(would-block))
            ;; Check timeout
            (if (> (time-second (current-time)) deadline)
                (begin
                  (posix-close fd)
                  (error 'with-flock-lock
                         (format "Timeout acquiring lock for ~a after ~a ms"
                                 path *flock-timeout-ms*)))
                (begin
                  (flock-sleep-ms *flock-poll-interval-ms*)
                  (acquire-loop)))]
           [else
            (posix-close fd)
            (error 'with-flock-lock "flock failed" path)])))

      ;; Lock acquired - use dynamic-wind for cleanup
      (dynamic-wind
        (lambda () #f)  ; Already acquired
        thunk
        (lambda ()
          ;; Always release and close (even on exception)
          (posix-flock fd LOCK_UN)
          (posix-close fd))))))

;;; ====
;;; Testing
;;; ====

;;; run-posix-tests : → Boolean
;;; Run basic POSIX FFI tests
(define (run-posix-tests)
  (display "POSIX FFI Tests\n")
  (display "====\n")

  ;; Test 1: Load library
  (display "1. Loading POSIX FFI... ")
  (let ([loaded (posix-load!)])
    (if (not loaded)
        (begin
          (display "SKIP (library not found)\n")
          (display "   Build with: cd shell/ffi/rust-accel && cargo build --release\n")
          #f)
        (begin
          (display "OK\n")

          ;; Test 2: getpid
          (display "2. posix-getpid: ")
          (let ([pid (posix-getpid)])
            (printf "~a " pid)
            (if (> pid 0)
                (display "OK\n")
                (begin (display "FAIL\n") (set! loaded #f))))

          ;; Test 3: open/close
          (display "3. posix-open/close: ")
          (let* ([test-path "/tmp/fold-posix-test.txt"]
                 [fd (posix-open test-path (bitwise-ior O_CREAT O_RDWR) #o644)])
            (if (and (integer? fd) (>= fd 0))
                (begin
                  (printf "fd=~a " fd)
                  (if (posix-close fd)
                      (begin
                        (display "OK\n")
                        ;; Cleanup
                        (when (file-exists? test-path)
                          (delete-file test-path)))
                      (begin (display "FAIL (close)\n") (set! loaded #f))))
                (begin
                  (printf "FAIL (open returned ~a)\n" fd)
                  (set! loaded #f))))

          ;; Test 4: flock (non-blocking)
          (display "4. posix-flock (non-blocking): ")
          (let* ([test-path "/tmp/fold-flock-test.txt"]
                 [fd (posix-open test-path (bitwise-ior O_CREAT O_RDWR) #o644)])
            (if (and (integer? fd) (>= fd 0))
                (let ([lock-result (posix-flock fd (bitwise-ior LOCK_EX LOCK_NB))])
                  (if (eq? lock-result #t)
                      (let ([unlock-result (posix-flock fd LOCK_UN)])
                        (posix-close fd)
                        (when (file-exists? test-path)
                          (delete-file test-path))
                        (if unlock-result
                            (display "OK\n")
                            (begin (display "FAIL (unlock)\n") (set! loaded #f))))
                      (begin
                        (posix-close fd)
                        (printf "FAIL (lock returned ~a)\n" lock-result)
                        (set! loaded #f))))
                (begin
                  (printf "FAIL (open returned ~a)\n" fd)
                  (set! loaded #f))))

          ;; Test 5: with-flock-lock
          (display "5. with-flock-lock: ")
          (guard (ex [else
                      (display "FAIL (exception)\n")
                      (display-condition ex)
                      (newline)
                      (set! loaded #f)])
            (let ([result (with-flock-lock "/tmp/fold-with-flock-test"
                            (lambda () 'protected-value))])
              (if (eq? result 'protected-value)
                  (begin
                    (display "OK\n")
                    ;; Cleanup
                    (when (file-exists? "/tmp/fold-with-flock-test.flock")
                      (delete-file "/tmp/fold-with-flock-test.flock")))
                  (begin
                    (printf "FAIL (returned ~a)\n" result)
                    (set! loaded #f)))))

          (display "====\n")
          (if loaded
              (display "All tests passed!\n")
              (display "Some tests failed.\n"))
          loaded))))

;;; ====
;;; Initialization
;;; ====

(display "posix-ffi.ss loaded.\n")
(display "  (posix-load!)                   - Load FFI library\n")
(display "  (posix-getpid)                  - Get real OS PID\n")
(display "  (with-flock-lock path thunk)    - Execute with advisory lock\n")
(display "  (run-posix-tests)               - Run tests\n")
