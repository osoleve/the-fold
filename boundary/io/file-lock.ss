(load "core/base/prelude.ss")

(doc 'module 'file-lock)
(doc 'description "File Locking Primitives — Provides file locking for atomic multi-step operations using a hybrid approach: process-internal mutex (for thread safety), lockfile with identity tokens (for cross-process safety), optional flock() via FFI (when available, for automatic cleanup).")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(prelude))
(doc 'note "Design: Lock file contains identity token + PID + timestamp. Stale locks are broken safely using atomic rename. Real PID via FFI when available, fallback otherwise. Process death handled by flock() if available.")

(doc 'section 'configuration)

(define *lock-poll-interval-ms* 10)    ; How often to retry acquiring lock
(define *lock-timeout-ms* 5000)        ; Max time to wait for lock
(define *lock-stale-threshold-s* 60)   ; Lock older than this is stale

(doc 'section 'posix-ffi-integration)
(doc 'note "We try to load posix-ffi for real PID and flock support. If unavailable, fall back to lockfile-only approach.")

(define *posix-ffi-available* #f)
(define *posix-ffi-checked* #f)

(define (check-posix-ffi!)
  (doc 'type (-> Bool))
  (doc 'description "Check if POSIX FFI is available, load if possible.")
  (unless *posix-ffi-checked*
    (guard (e [else #f])
      (load "boundary/ffi/posix-ffi.ss")
      (when (and (top-level-bound? 'posix-load!)
                 (posix-load!))
        (set! *posix-ffi-available* #t)))
    (set! *posix-ffi-checked* #t))
  *posix-ffi-available*)

(doc 'section 'process-id)

(doc *lock-identity-token-cache* 'type (Maybe String))
(doc *lock-identity-token-cache* 'description "Cached identity token (lazily initialized to include PID).")
(define *lock-identity-token-cache* #f)

(define (get-lock-identity-token)
  (doc 'type (-> String))
  (doc 'description "Get unique identity token for this process. Lazily initialized to include PID (which requires FFI check). Includes: PID + nanoseconds + counter for uniqueness.")
  (unless *lock-identity-token-cache*
    (let* ([pid (current-process-id)]  ; Real PID via FFI if available
           [nanos (time-nanosecond (current-time))]
           [secs (time-second (current-time))]
           [chars "0123456789abcdefghijklmnopqrstuvwxyz"]
           ;; Combine PID + time for uniqueness
           [seed (+ (* pid 1000000000) nanos (* secs 37))])
      (set! *lock-identity-token-cache*
            (string-append
             ;; Include PID directly for debugging
             (number->string pid)
             "-"
             ;; Plus random-ish suffix from time
             (list->string
              (let loop ([i 0] [s seed] [acc '()])
                (if (= i 12)
                    acc
                    (loop (+ i 1)
                          (quotient s 36)
                          (cons (string-ref chars (modulo s 36))
                                acc)))))))))
  *lock-identity-token-cache*)


(define (current-process-id)
  (doc 'type (-> Number))
  (doc 'description "Get current process ID. Uses real PID via FFI when available, fallback to memory address otherwise.")
  (doc 'export #t)
  (if (check-posix-ffi!)
      (posix-getpid)
      ;; Fallback: use memory address as pseudo-PID
      (let ([addr (ftype-pointer-address (make-ftype-pointer void* (foreign-alloc 8)))])
        (foreign-free addr)
        (modulo addr 1000000))))

(define (current-time-utc)
  (doc 'type (-> Number))
  (doc 'description "Get current time as seconds since epoch.")
  (time-second (current-time)))

(doc 'section 'process-internal-mutex)
(doc 'note "Global mutex table for path-based locking within process.")

(define *file-lock-mutexes* (make-hashtable string-hash string=?))
(define *mutex-table-lock* (make-mutex))

(define (get-path-mutex path)
  (doc 'type (-> String Mutex))
  (doc 'description "Get or create a mutex for a given path.")
  (with-mutex *mutex-table-lock*
    (let ([existing (hashtable-ref *file-lock-mutexes* path #f)])
      (if existing
          existing
          (let ([new-mutex (make-mutex)])
            (hashtable-set! *file-lock-mutexes* path new-mutex)
            new-mutex)))))

(doc 'section 'lock-file-operations)

(define (lock-file-path path)
  (doc 'type (-> String String))
  (doc 'description "Get the lock file path for a given resource.")
  (string-append path ".lock"))

(define (parse-lock-file lock-path)
  (doc 'type (-> String (Maybe (List String Number Number))))
  (doc 'description "Parse a lock file, returning its contents (token pid timestamp) or #f if invalid.")
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

(define (write-lock-content port)
  (doc 'type (-> Port Void))
  (doc 'description "Write lock file content with identity token, PID, and timestamp.")
  (put-string port (get-lock-identity-token))
  (newline port)
  (put-string port (number->string (current-process-id)))
  (newline port)
  (put-string port (number->string (current-time-utc)))
  (newline port))

(define (try-create-lock-file lock-path)
  (doc 'type (-> String Bool))
  (doc 'description "Attempt to create lock file atomically. Returns #t if successful. Uses exclusive creation to prevent races.")
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
                 (string=? (car lock-info) (get-lock-identity-token))))))))

(define (process-alive? pid)
  (doc 'type (-> Number Bool))
  (doc 'description "Check if a process with given PID exists via /proc/<pid>. Linux-specific. Returns #f for non-positive PIDs.")
  (and (> pid 0)
       (guard (e [else #f])
         (file-exists? (format "/proc/~a" pid)))))

(define (lock-file-stale? lock-path)
  (doc 'type (-> String Bool))
  (doc 'description "Check if a lock file is stale. A lock is stale if: (1) it can't be parsed, (2) its owning PID is a real OS PID that no longer exists, or (3) it's older than the stale threshold. PID check only applies when POSIX FFI is available (ensures real PIDs).")
  (guard (e [else #t])  ; Treat errors as stale
    (let ([lock-info (parse-lock-file lock-path)])
      (if (not lock-info)
          #t  ; Can't parse = stale
          (let* ([pid (cadr lock-info)]
                 [timestamp (caddr lock-info)]
                 [now (current-time-utc)]
                 [age (- now timestamp)])
            (or
             ;; PID liveness: only trust when POSIX FFI gives real PIDs
             (and *posix-ffi-available*
                  (not (process-alive? pid)))
             ;; Fallback: time-based stale detection
             (> age *lock-stale-threshold-s*)))))))

(define (break-stale-lock-safe lock-path)
  (doc 'type (-> String Bool))
  (doc 'description "Safely break a stale lock using atomic rename. Returns #t if we successfully acquired the lock, #f if another process won. This fixes the race condition where two processes might both detect a stale lock and try to break it simultaneously.")
  (let ([temp-path (format "~a.~a.~a.breaking"
                           lock-path
                           (get-lock-identity-token)
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
             (string=? (car lock-info) (get-lock-identity-token)))))))

(define (remove-lock-file lock-path)
  (doc 'type (-> String Void))
  (doc 'description "Remove a lock file (only if we own it).")
  (guard (e [else #f])
    (when (file-exists? lock-path)
      ;; Only remove if we own it
      (let ([lock-info (parse-lock-file lock-path)])
        (when (and lock-info
                   (string=? (car lock-info) (get-lock-identity-token)))
          (delete-file lock-path))))))

(doc 'section 'high-level-locking-api)

(define (acquire-file-lock path)
  (doc 'type (-> String Bool))
  (doc 'description "Acquire a file lock, waiting up to timeout. Returns #t if acquired.")
  (doc 'export #t)
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

(define (release-file-lock path)
  (doc 'type (-> String Void))
  (doc 'description "Release a file lock.")
  (doc 'export #t)
  (remove-lock-file (lock-file-path path)))

(define (sleep-ms ms)
  (doc 'type (-> Number Void))
  (doc 'description "Sleep for milliseconds.")
  ;; Convert to nanoseconds for sleep
  (sleep (make-time 'time-duration (* ms 1000000) 0)))

(define (with-file-lock path thunk)
  (doc 'type (-> String (-> α) α))
  (doc 'description "Execute thunk while holding file lock. Combines process-internal mutex with cross-process lock file. If POSIX FFI is available, also uses flock() for automatic cleanup on process death.")
  (doc 'export #t)
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

(doc 'section 'help)

;; Only show load message on first load
(unless (top-level-bound? '*file-lock-loaded*)
  (display "file-lock.ss loaded.\n")
  (display "  Lock:   (with-file-lock path thunk)\n")
  (display "  Manual: (acquire-file-lock path), (release-file-lock path)\n")
  (display "  Info:   (current-process-id)\n")
  (when (check-posix-ffi!)
    (display "  POSIX FFI available - using flock() for automatic cleanup\n")))

(define *file-lock-loaded* #t)
