;;; boundary/ffi/posix-ffi.ss — POSIX FFI Bindings
;;;
;;; Provides access to POSIX system calls via Rust FFI:
;;; - getpid: true OS process ID
;;; - flock: advisory file locking (with automatic cleanup on process death)
;;; - open/close: low-level file descriptor operations with O_EXCL
;;; - fsync/fdatasync: force data to disk for durability
;;; - stat/fstat: file metadata (size, mtime, mode, type)
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
;;;   (posix-fsync fd)                   ; Force data+metadata to disk
;;;   (posix-fdatasync fd)               ; Force data to disk (faster)
;;;   (posix-stat path)                  ; Get file metadata
;;;   (posix-fstat fd)                   ; Get metadata via fd
;;;   (with-flock-lock path thunk)       ; High-level: lock file during thunk
;;;
;;; This is Shell code: impure (system calls, FFI).

(load "boundary/ffi/ffi-core.ss")

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
(define ENOENT 2)        ; No such file or directory

;;; File type constants (from stat)
(define FILE_TYPE_OTHER 0)
(define FILE_TYPE_REGULAR 1)
(define FILE_TYPE_DIRECTORY 2)
(define FILE_TYPE_SYMLINK 3)

;;; Stat buffer size (must match Rust STAT_BUFFER_SIZE)
;;; Layout: size(8) + mtime_sec(8) + mtime_nsec(8) + mode(4) + type(4) = 32
(define *stat-buffer-size* 32)

;;; ====
;;; Foreign Procedure Bindings
;;; ====

(define *posix-bound* #f)

(define rust-posix-getpid #f)
(define rust-posix-open #f)
(define rust-posix-close #f)
(define rust-posix-flock #f)
(define rust-posix-fsync #f)
(define rust-posix-fdatasync #f)
(define rust-posix-stat #f)
(define rust-posix-fstat #f)

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

    ;; fsync(fd)
    (set! rust-posix-fsync
          (foreign-procedure "fold_posix_fsync"
                             (integer-32 (* posix-status-result-t))
                             void))

    ;; fdatasync(fd)
    (set! rust-posix-fdatasync
          (foreign-procedure "fold_posix_fdatasync"
                             (integer-32 (* posix-status-result-t))
                             void))

    ;; stat(path, out_buf, out_status)
    (set! rust-posix-stat
          (foreign-procedure "fold_posix_stat"
                             (u8* size_t u8* (* posix-status-result-t))
                             void))

    ;; fstat(fd, out_buf, out_status)
    (set! rust-posix-fstat
          (foreign-procedure "fold_posix_fstat"
                             (integer-32 u8* (* posix-status-result-t))
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
;;; Memory-Safe FFI Helpers
;;; ====

;;; call-with-foreign-alloc : Size × (Pointer → α) → α
;;; Allocate foreign memory, execute thunk with pointer, ensure cleanup.
;;; Uses dynamic-wind to guarantee foreign-free even on exceptions. (Fixed: fold-zxnb)
(define (call-with-foreign-alloc size thunk)
  (let ([addr (foreign-alloc size)])
    (dynamic-wind
      (lambda () #f)
      (lambda () (thunk addr))
      (lambda () (foreign-free addr)))))

;;; call-with-int-result : (Pointer → Void) × (Status × Value × Errno → α) → α
;;; Common pattern for int-returning POSIX calls with guaranteed cleanup.
;;; Allocates posix-int-result-t, calls ffi-proc, then result-proc with values. (Fixed: fold-zxnb)
(define (call-with-int-result ffi-proc result-proc)
  (call-with-foreign-alloc
   (ftype-sizeof posix-int-result-t)
   (lambda (addr)
     (let ([ptr (make-ftype-pointer posix-int-result-t addr)])
       (ffi-proc ptr)
       (result-proc
        (ftype-ref posix-int-result-t (status) ptr)
        (ftype-ref posix-int-result-t (value) ptr)
        (ftype-ref posix-int-result-t (error-code) ptr))))))

;;; call-with-status-result : (Pointer → Void) × (Status × Errno → α) → α
;;; Common pattern for status-returning POSIX calls with guaranteed cleanup.
;;; Allocates posix-status-result-t, calls ffi-proc, then result-proc with values. (Fixed: fold-zxnb)
(define (call-with-status-result ffi-proc result-proc)
  (call-with-foreign-alloc
   (ftype-sizeof posix-status-result-t)
   (lambda (addr)
     (let ([ptr (make-ftype-pointer posix-status-result-t addr)])
       (ffi-proc ptr)
       (result-proc
        (ftype-ref posix-status-result-t (status) ptr)
        (ftype-ref posix-status-result-t (error-code) ptr))))))

;;; ====
;;; Scheme Wrappers
;;; ====

;;; posix-getpid : → Int
;;; Get the real OS process ID
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-getpid)
  (unless (posix-available?)
    (error 'posix-getpid "POSIX FFI not loaded"))
  (call-with-int-result
   (lambda (ptr) (rust-posix-getpid ptr))
   (lambda (status value errno)
     (if (= status 0)
         value
         (error 'posix-getpid "getpid failed" status)))))

;;; posix-open : String × Int × Int → Int | (error errno)
;;; Open a file and return file descriptor
;;; Returns fd on success, (error errno) on failure
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-open path flags mode)
  (unless (posix-available?)
    (error 'posix-open "POSIX FFI not loaded"))
  (let ([path-bv (string->utf8 path)])
    (call-with-int-result
     (lambda (ptr)
       (rust-posix-open path-bv (bytevector-length path-bv) flags mode ptr))
     (lambda (status value errno)
       (if (= status 0)
           value
           (list 'error errno))))))

;;; posix-close : Int → Boolean
;;; Close a file descriptor
;;; Returns #t on success, #f on failure
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-close fd)
  (unless (posix-available?)
    (error 'posix-close "POSIX FFI not loaded"))
  (call-with-status-result
   (lambda (ptr) (rust-posix-close fd ptr))
   (lambda (status errno) (= status 0))))

;;; posix-flock : Int × Int → Boolean | (would-block)
;;; Apply advisory lock to file descriptor
;;; Returns #t on success, (would-block) if EWOULDBLOCK, #f on other error
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-flock fd operation)
  (unless (posix-available?)
    (error 'posix-flock "POSIX FFI not loaded"))
  (call-with-status-result
   (lambda (ptr) (rust-posix-flock fd operation ptr))
   (lambda (status errno)
     (cond
      [(= status 0) #t]
      [(= errno EWOULDBLOCK) '(would-block)]
      [else #f]))))

;;; ====
;;; Durability Operations
;;; ====

;;; posix-fsync : Int → Boolean
;;; Force all buffered data for fd to be written to disk.
;;; WARNING: Blocking operation, can take 10-100ms on rotating media.
;;; Returns #t on success, #f on failure.
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-fsync fd)
  (unless (posix-available?)
    (error 'posix-fsync "POSIX FFI not loaded"))
  (call-with-status-result
   (lambda (ptr) (rust-posix-fsync fd ptr))
   (lambda (status errno) (= status 0))))

;;; posix-fdatasync : Int → Boolean
;;; Force buffered data (but not metadata) for fd to be written to disk.
;;; Faster than fsync because it skips non-essential metadata like atime.
;;; PREFERRED for atomic writes where only data integrity matters.
;;; Returns #t on success, #f on failure.
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-fdatasync fd)
  (unless (posix-available?)
    (error 'posix-fdatasync "POSIX FFI not loaded"))
  (call-with-status-result
   (lambda (ptr) (rust-posix-fdatasync fd ptr))
   (lambda (status errno) (= status 0))))

;;; ====
;;; File Metadata Operations
;;; ====

;;; NOTE: We allocate stat buffers locally in each function call
;;; to ensure thread-safety. The minor allocation overhead is
;;; acceptable for correctness. (Fixed: fold-zxn6)

;;; parse-stat-buffer : Bytevector → Alist
;;; Parse stat buffer into an association list.
;;; Keys: size, mtime-sec, mtime-nsec, mode, file-type
(define (parse-stat-buffer buf)
  (list
   (cons 'size (bytevector-u64-native-ref buf 0))
   (cons 'mtime-sec (bytevector-s64-native-ref buf 8))
   (cons 'mtime-nsec (bytevector-s64-native-ref buf 16))
   (cons 'mode (bytevector-u32-native-ref buf 24))
   (cons 'file-type (bytevector-u32-native-ref buf 28))))

;;; posix-stat : String → Alist | #f
;;; Get file metadata. Returns alist with keys:
;;;   size       - file size in bytes
;;;   mtime-sec  - modification time seconds since epoch
;;;   mtime-nsec - modification time nanoseconds
;;;   mode       - file permissions
;;;   file-type  - FILE_TYPE_REGULAR, FILE_TYPE_DIRECTORY, etc.
;;; Returns #f if file does not exist or error.
;;; Thread-safe: allocates buffer locally. (Fixed: fold-zxn6)
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-stat path)
  (unless (posix-available?)
    (error 'posix-stat "POSIX FFI not loaded"))
  (let* ([path-bv (string->utf8 path)]
         [stat-buffer (make-bytevector *stat-buffer-size*)])
    (call-with-status-result
     (lambda (ptr)
       (rust-posix-stat path-bv (bytevector-length path-bv) stat-buffer ptr))
     (lambda (status errno)
       (if (= status 0)
           (parse-stat-buffer stat-buffer)
           #f)))))

;;; posix-stat-field : String × Symbol → Value | #f
;;; Get a single field from stat. More efficient than full posix-stat
;;; when only one field is needed.
(define (posix-stat-field path field)
  (let ([result (posix-stat path)])
    (and result (cdr (assq field result)))))

;;; posix-file-exists? : String → Boolean
;;; Check if file exists using stat.
(define (posix-file-exists? path)
  (and (posix-stat path) #t))

;;; posix-file-size : String → Int | #f
;;; Get file size in bytes.
(define (posix-file-size path)
  (posix-stat-field path 'size))

;;; posix-file-mtime : String → Int | #f
;;; Get file modification time as seconds since epoch.
(define (posix-file-mtime path)
  (posix-stat-field path 'mtime-sec))

;;; posix-file-type : String → Int | #f
;;; Get file type (FILE_TYPE_REGULAR, FILE_TYPE_DIRECTORY, etc.)
(define (posix-file-type path)
  (posix-stat-field path 'file-type))

;;; posix-regular-file? : String → Boolean
(define (posix-regular-file? path)
  (eqv? (posix-file-type path) FILE_TYPE_REGULAR))

;;; posix-directory? : String → Boolean
(define (posix-directory? path)
  (eqv? (posix-file-type path) FILE_TYPE_DIRECTORY))

;;; posix-fstat : Int → Alist | #f
;;; Get file metadata via file descriptor.
;;; Same return format as posix-stat.
;;; Thread-safe: allocates buffer locally. (Fixed: fold-zxn6)
;;; Memory-safe: uses dynamic-wind for cleanup. (Fixed: fold-zxnb)
(define (posix-fstat fd)
  (unless (posix-available?)
    (error 'posix-fstat "POSIX FFI not loaded"))
  (let ([stat-buffer (make-bytevector *stat-buffer-size*)])
    (call-with-status-result
     (lambda (ptr) (rust-posix-fstat fd stat-buffer ptr))
     (lambda (status errno)
       (if (= status 0)
           (parse-stat-buffer stat-buffer)
           #f)))))

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

;;; monotonic-time-ms : → Number
;;; Get current monotonic time in milliseconds.
;;; Monotonic time never goes backwards, immune to NTP/clock adjustments. (Fixed: fold-zxne)
(define (monotonic-time-ms)
  (let ([t (current-time 'time-monotonic)])
    (+ (* 1000 (time-second t))
       (quotient (time-nanosecond t) 1000000))))

;;; with-flock-lock : String × (→ α) → α
;;; Execute thunk while holding an advisory flock on path.
;;; Uses non-blocking flock with retry loop to avoid hanging Chez runtime.
;;;
;;; The lock is automatically released when:
;;; - The thunk completes (normally or via exception)
;;; - The process dies (OS kernel handles cleanup)
;;;
;;; Security: Uses O_EXCL on creation to prevent symlink attacks. (Fixed: fold-zxn7)
;;; Timing: Uses monotonic time for timeout to prevent NTP-related issues. (Fixed: fold-zxne)
;;; This is the recommended way to use flock from Scheme.
(define (with-flock-lock path thunk)
  (unless (posix-available?)
    (error 'with-flock-lock "POSIX FFI not loaded"))

  (let* ([lock-path (string-append path ".flock")]
         ;; Try exclusive creation first to prevent symlink attacks
         ;; O_CLOEXEC prevents child processes from inheriting the lock FD
         [fd-result (posix-open lock-path
                                (bitwise-ior O_CREAT O_EXCL O_RDWR O_CLOEXEC)
                                #o644)]
         ;; If file already exists (EEXIST), open it normally
         ;; This is safe because we created it exclusively on first use
         [fd-result (if (and (pair? fd-result)
                             (eq? (car fd-result) 'error)
                             (= (cadr fd-result) EEXIST))
                        (posix-open lock-path
                                    (bitwise-ior O_RDWR O_CLOEXEC)
                                    #o644)
                        fd-result)])
    (when (and (pair? fd-result) (eq? (car fd-result) 'error))
      (error 'with-flock-lock "Failed to open lock file" lock-path fd-result))

    (let ([fd fd-result]
          [deadline (+ (monotonic-time-ms) *flock-timeout-ms*)])
      ;; Acquire lock with non-blocking retry
      (let acquire-loop ()
        (let ([result (posix-flock fd (bitwise-ior LOCK_EX LOCK_NB))])
          (cond
           [(eq? result #t) #t]  ; Lock acquired
           [(equal? result '(would-block))
            ;; Check timeout using monotonic time
            (if (> (monotonic-time-ms) deadline)
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
      ;; Use a flag to prevent re-entry via captured continuations (Fixed: fold-zxnc)
      (let ([closed #f])
        (dynamic-wind
          (lambda ()
            ;; Before thunk: check if fd was already closed
            (when closed
              (error 'with-flock-lock
                     "Cannot re-enter locked region after exit (continuation re-entry)"
                     path)))
          thunk
          (lambda ()
            ;; After thunk: release lock, close fd, mark as closed
            (unless closed
              (posix-flock fd LOCK_UN)
              (posix-close fd)
              (set! closed #t))))))))

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
          (display "   Build with: cd boundary/ffi/rust-accel && cargo build --release\n")
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

          ;; Test 6: fdatasync
          (display "6. posix-fdatasync: ")
          (let* ([test-path "/tmp/fold-fdatasync-test.txt"]
                 [fd (posix-open test-path (bitwise-ior O_CREAT O_RDWR) #o644)])
            (if (and (integer? fd) (>= fd 0))
                (let ([sync-result (posix-fdatasync fd)])
                  (posix-close fd)
                  (when (file-exists? test-path)
                    (delete-file test-path))
                  (if sync-result
                      (display "OK\n")
                      (begin (display "FAIL\n") (set! loaded #f))))
                (begin
                  (printf "FAIL (open returned ~a)\n" fd)
                  (set! loaded #f))))

          ;; Test 7: fsync
          (display "7. posix-fsync: ")
          (let* ([test-path "/tmp/fold-fsync-test.txt"]
                 [fd (posix-open test-path (bitwise-ior O_CREAT O_RDWR) #o644)])
            (if (and (integer? fd) (>= fd 0))
                (let ([sync-result (posix-fsync fd)])
                  (posix-close fd)
                  (when (file-exists? test-path)
                    (delete-file test-path))
                  (if sync-result
                      (display "OK\n")
                      (begin (display "FAIL\n") (set! loaded #f))))
                (begin
                  (printf "FAIL (open returned ~a)\n" fd)
                  (set! loaded #f))))

          ;; Test 8: posix-stat
          (display "8. posix-stat: ")
          (let* ([test-path "/tmp/fold-stat-test.txt"]
                 [content "hello stat test"])
            ;; Create test file
            (call-with-output-file test-path
              (lambda (p) (put-string p content))
              '(replace))
            (let ([stat-result (posix-stat test-path)])
              (when (file-exists? test-path)
                (delete-file test-path))
              (if (and stat-result
                       (= (cdr (assq 'size stat-result)) (string-length content))
                       (= (cdr (assq 'file-type stat-result)) FILE_TYPE_REGULAR))
                  (begin
                    (printf "size=~a type=~a OK\n"
                            (cdr (assq 'size stat-result))
                            (cdr (assq 'file-type stat-result))))
                  (begin
                    (printf "FAIL (result=~a)\n" stat-result)
                    (set! loaded #f)))))

          ;; Test 9: posix-stat on directory
          (display "9. posix-stat (directory): ")
          (let ([stat-result (posix-stat "/tmp")])
            (if (and stat-result
                     (= (cdr (assq 'file-type stat-result)) FILE_TYPE_DIRECTORY))
                (display "OK\n")
                (begin
                  (printf "FAIL (result=~a)\n" stat-result)
                  (set! loaded #f))))

          ;; Test 10: posix-stat nonexistent
          (display "10. posix-stat (nonexistent): ")
          (let ([stat-result (posix-stat "/tmp/this-file-should-not-exist-fold.txt")])
            (if (not stat-result)
                (display "OK (returned #f)\n")
                (begin
                  (printf "FAIL (expected #f, got ~a)\n" stat-result)
                  (set! loaded #f))))

          ;; Test 11: posix-fstat
          (display "11. posix-fstat: ")
          (let* ([test-path "/tmp/fold-fstat-test.txt"]
                 [content "hello fstat test"])
            ;; Create test file
            (call-with-output-file test-path
              (lambda (p) (put-string p content))
              '(replace))
            (let ([fd (posix-open test-path O_RDONLY 0)])
              (if (and (integer? fd) (>= fd 0))
                  (let ([fstat-result (posix-fstat fd)])
                    (posix-close fd)
                    (when (file-exists? test-path)
                      (delete-file test-path))
                    (if (and fstat-result
                             (= (cdr (assq 'size fstat-result)) (string-length content)))
                        (begin
                          (printf "size=~a OK\n" (cdr (assq 'size fstat-result))))
                        (begin
                          (printf "FAIL (result=~a)\n" fstat-result)
                          (set! loaded #f))))
                  (begin
                    (when (file-exists? test-path)
                      (delete-file test-path))
                    (printf "FAIL (open returned ~a)\n" fd)
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
(display "  (posix-fdatasync fd)            - Force data to disk (fast)\n")
(display "  (posix-fsync fd)                - Force data+metadata to disk\n")
(display "  (posix-stat path)               - Get file metadata\n")
(display "  (posix-fstat fd)                - Get metadata via fd\n")
(display "  (with-flock-lock path thunk)    - Execute with advisory lock\n")
(display "  (run-posix-tests)               - Run tests\n")
