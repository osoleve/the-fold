;;; shell/io/file-lock.ss — File Locking Primitives
;;;
;;; Provides file locking for atomic multi-step operations.
;;; Uses a hybrid approach:
;;;   1. Process-internal mutex (for thread safety)
;;;   2. Lockfile with identity tokens (for cross-process safety)
;;;   3. Optional flock() via FFI (when available, for automatic cleanup)
;;;
;;; Design:
;;;   - Lock file contains identity token + PID + timestamp
;;;   - Stale locks are broken safely using atomic rename
;;;   - Real PID via FFI when available, fallback otherwise
;;;   - Process death handled by flock() if available
;;;
;;; This is Shell code: impure (filesystem IO, synchronization).

(load "core/base/prelude.ss")

;;; ====
;;; Configuration
;;; ====

(define *lock-poll-interval-ms* 10)    ; How often to retry acquiring lock
(define *lock-timeout-ms* 5000)        ; Max time to wait for lock
(define *lock-stale-threshold-s* 60)   ; Lock older than this is stale

;;; ====
;;; POSIX FFI Integration (Optional)
;;; ====

;;; We try to load posix-ffi for real PID and flock support.
;;; If unavailable, fall back to lockfile-only approach.

(define *posix-ffi-available* #f)
(define *posix-ffi-checked* #f)

;;; check-posix-ffi! : → Boolean
;;; Check if POSIX FFI is available, load if possible
(define (check-posix-ffi!)
  (unless *posix-ffi-checked*
    (guard (e [else #f])
      (load "shell/ffi/posix-ffi.ss")
      (when (and (top-level-bound? 'posix-load!)
                 (posix-load!))
        (set! *posix-ffi-available* #t)))
    (set! *posix-ffi-checked* #t))
  *posix-ffi-available*)

;;; ====
;;; Process ID
;;; ====

;;; *lock-identity-token* : String
;;; Unique identity for this process (generated once at load time)
;;; Used to verify lock ownership after stale lock breaking
(define *lock-identity-token*
  (let ([chars "0123456789abcdefghijklmnopqrstuvwxyz"])
    (list->string
     (let loop ([i 0] [acc '()])
       (if (= i 16)
           acc
           (loop (+ i 1)
                 (cons (string-ref chars
                                   (modulo (+ (* (time-nanosecond (current-time)) i)
                                              (time-second (current-time)))
                                           36))
                       acc)))))))

;;; current-process-id : → Number
;;; Get current process ID.
;;; Uses real PID via FFI when available, fallback to memory address otherwise.
(define (current-process-id)
  (if (check-posix-ffi!)
      (posix-getpid)
      ;; Fallback: use memory address as pseudo-PID
      (let ([addr (ftype-pointer-address (make-ftype-pointer void* (foreign-alloc 8)))])
        (foreign-free addr)
        (modulo addr 1000000))))

;;; current-time-utc : → Number
;;; Get current time as seconds since epoch.
(define (current-time-utc)
  (time-second (current-time)))

;;; ====
;;; Process-Internal Mutex
;;; ====

;;; Global mutex table for path-based locking within process
(define *file-lock-mutexes* (make-hashtable string-hash string=?))
(define *mutex-table-lock* (make-mutex))

;;; get-path-mutex : String → Mutex
;;; Get or create a mutex for a given path.
(define (get-path-mutex path)
  (with-mutex *mutex-table-lock*
    (let ([existing (hashtable-ref *file-lock-mutexes* path #f)])
      (if existing
          existing
          (let ([new-mutex (make-mutex)])
            (hashtable-set! *file-lock-mutexes* path new-mutex)
            new-mutex)))))

;;; ====
;;; Lock File Operations
;;; ====

;;; lock-file-path : String → String
;;; Get the lock file path for a given resource.
(define (lock-file-path path)
  (string-append path ".lock"))

;;; parse-lock-file : String → (token pid timestamp) | #f
;;; Parse a lock file, returning its contents or #f if invalid.
(define (parse-lock-file lock-path)
  (guard (e [else #f])
    (if (not (file-exists? lock-path))
        #f
        (call-with-input-file lock-path
          (lambda (port)
            (let* ([token (get-line port)]
                   [pid-str (get-line port)]
                   [time-str (get-line port)])
              (if (or (eof-object? token)
                      (eof-object? pid-str)
                      (eof-object? time-str))
                  #f
                  (let ([pid (string->number pid-str)]
                        [timestamp (string->number time-str)])
                    (if (and pid timestamp)
                        (list token pid timestamp)
                        #f)))))))))

;;; write-lock-content : Port → Void
;;; Write lock file content with identity token, PID, and timestamp.
(define (write-lock-content port)
  (put-string port *lock-identity-token*)
  (newline port)
  (put-string port (number->string (current-process-id)))
  (newline port)
  (put-string port (number->string (current-time-utc)))
  (newline port))

;;; try-create-lock-file : String → Boolean
;;; Attempt to create lock file atomically. Returns #t if successful.
;;; Uses exclusive creation to prevent races.
(define (try-create-lock-file lock-path)
  (guard (e [else #f])
    ;; Check if lock exists
    (if (file-exists? lock-path)
        #f
        ;; Try to create exclusively
        ;; Note: There's still a small TOCTOU window here, but the identity
        ;; token verification in break-stale-lock-safe handles conflicts
        (begin
          (call-with-output-file lock-path
            (lambda (port)
              (write-lock-content port))
            '(exclusive))  ; Chez exclusive mode
          ;; Verify we actually own the lock
          (let ([lock-info (parse-lock-file lock-path)])
            (and lock-info
                 (string=? (car lock-info) *lock-identity-token*)))))))

;;; lock-file-stale? : String → Boolean
;;; Check if a lock file is stale (older than threshold).
(define (lock-file-stale? lock-path)
  (guard (e [else #t])  ; Treat errors as stale
    (let ([lock-info (parse-lock-file lock-path)])
      (if (not lock-info)
          #t  ; Can't parse = stale
          (let* ([timestamp (caddr lock-info)]
                 [now (current-time-utc)]
                 [age (- now timestamp)])
            (> age *lock-stale-threshold-s*))))))

;;; break-stale-lock-safe : String → Boolean
;;; Safely break a stale lock using atomic rename.
;;; Returns #t if we successfully acquired the lock, #f if another process won.
;;;
;;; This fixes the race condition where two processes might both detect
;;; a stale lock and try to break it simultaneously.
(define (break-stale-lock-safe lock-path)
  (let ([temp-path (format "~a.~a.~a.breaking"
                           lock-path
                           *lock-identity-token*
                           (time-nanosecond (current-time)))])
    (guard (e [else #f])
      ;; Write our lock to temp file
      (call-with-output-file temp-path
        (lambda (port)
          (write-lock-content port))
        '(replace))

      ;; Atomically rename over the stale lock
      ;; If another process renamed first, this still succeeds but
      ;; overwrites their lock - so we must verify ownership
      (rename-file temp-path lock-path)

      ;; Verify we actually own the lock
      (let ([lock-info (parse-lock-file lock-path)])
        (and lock-info
             (string=? (car lock-info) *lock-identity-token*))))))

;;; remove-lock-file : String → Void
;;; Remove a lock file (only if we own it).
(define (remove-lock-file lock-path)
  (guard (e [else #f])
    (when (file-exists? lock-path)
      ;; Only remove if we own it
      (let ([lock-info (parse-lock-file lock-path)])
        (when (and lock-info
                   (string=? (car lock-info) *lock-identity-token*))
          (delete-file lock-path))))))

;;; ====
;;; High-Level Locking API
;;; ====

;;; acquire-file-lock : String → Boolean
;;; Acquire a file lock, waiting up to timeout. Returns #t if acquired.
(define (acquire-file-lock path)
  (let* ([lock-path (lock-file-path path)]
         [deadline (+ (current-time-utc) (/ *lock-timeout-ms* 1000))])
    (let loop ()
      (cond
        ;; Try to create lock
        [(try-create-lock-file lock-path) #t]
        ;; Check for stale lock and try to break it safely
        [(lock-file-stale? lock-path)
         (if (break-stale-lock-safe lock-path)
             #t      ; We won the race
             (loop))] ; Someone else won, try again
        ;; Check timeout
        [(> (current-time-utc) deadline) #f]
        ;; Wait and retry
        [else
         (sleep-ms *lock-poll-interval-ms*)
         (loop)]))))

;;; release-file-lock : String → Void
;;; Release a file lock.
(define (release-file-lock path)
  (remove-lock-file (lock-file-path path)))

;;; sleep-ms : Number → Void
;;; Sleep for milliseconds.
(define (sleep-ms ms)
  ;; Convert to nanoseconds for sleep
  (sleep (make-time 'time-duration (* ms 1000000) 0)))

;;; with-file-lock : String × (→ α) → α
;;; Execute thunk while holding file lock.
;;; Combines process-internal mutex with cross-process lock file.
;;;
;;; If POSIX FFI is available, also uses flock() for automatic
;;; cleanup on process death.
(define (with-file-lock path thunk)
  (let ([mutex (get-path-mutex path)])
    (with-mutex mutex
      ;; If POSIX FFI available, use flock for automatic cleanup
      (if (check-posix-ffi!)
          ;; Hybrid: flock + lockfile for belt-and-suspenders
          (with-flock-lock path
            (lambda ()
              (let ([acquired (acquire-file-lock path)])
                (unless acquired
                  (error 'with-file-lock
                         (format "Failed to acquire lock for ~a after ~a ms"
                                 path *lock-timeout-ms*)))
                (dynamic-wind
                  (lambda () #f)
                  thunk
                  (lambda () (release-file-lock path))))))
          ;; Lockfile only (no flock)
          (let ([acquired (acquire-file-lock path)])
            (unless acquired
              (error 'with-file-lock
                     (format "Failed to acquire lock for ~a after ~a ms"
                             path *lock-timeout-ms*)))
            (dynamic-wind
              (lambda () #f)
              thunk
              (lambda () (release-file-lock path))))))))

;;; ====
;;; Print Help
;;; ====

(display "file-lock.ss loaded.\n")
(display "  Lock:   (with-file-lock path thunk)\n")
(display "  Manual: (acquire-file-lock path), (release-file-lock path)\n")
(display "  Info:   (current-process-id)\n")
(when (check-posix-ffi!)
  (display "  POSIX FFI available - using flock() for automatic cleanup\n"))
