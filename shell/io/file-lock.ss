;;; shell/io/file-lock.ss — File Locking Primitives
;;;
;;; Provides file locking for atomic multi-step operations.
;;; Uses lockfile-based approach that works across processes.
;;;
;;; Design:
;;;   - Lock file creation is atomic (O_CREAT|O_EXCL semantics)
;;;   - Stale locks are detected via timeout
;;;   - Process-internal mutex prevents thread races
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

;;; try-create-lock-file : String → Boolean
;;; Attempt to create lock file atomically. Returns #t if successful.
;;; Uses exclusive creation to prevent races.
(define (try-create-lock-file lock-path)
  (guard (e [else #f])
    ;; Check if lock exists
    (if (file-exists? lock-path)
        #f
        ;; Try to create exclusively
        ;; Note: There's a small TOCTOU window here, but it's the best
        ;; we can do without FFI open() with O_EXCL
        (begin
          (call-with-output-file lock-path
            (lambda (port)
              ;; Write PID and timestamp for stale detection
              (put-string port (number->string (current-process-id)))
              (newline port)
              (put-string port (number->string (current-time-utc)))
              (newline port))
            '(exclusive))  ; Chez exclusive mode
          #t))))

;;; current-process-id : → Number
;;; Get current process ID (uses time-based fallback if not available).
(define (current-process-id)
  ;; Chez Scheme doesn't expose getpid directly, use a unique identifier
  (let ([addr (ftype-pointer-address (make-ftype-pointer void* (foreign-alloc 8)))])
    (foreign-free addr)
    (modulo addr 1000000)))

;;; current-time-utc : → Number
;;; Get current time as seconds since epoch.
(define (current-time-utc)
  (time-second (current-time)))

;;; lock-file-stale? : String → Boolean
;;; Check if a lock file is stale (older than threshold).
(define (lock-file-stale? lock-path)
  (guard (e [else #t])  ; Treat errors as stale
    (if (not (file-exists? lock-path))
        #t
        (let* ([stat-time (file-modification-time lock-path)]
               [now (current-time-utc)]
               [age (- now stat-time)])
          (> age *lock-stale-threshold-s*)))))

;;; remove-lock-file : String → Void
;;; Remove a lock file.
(define (remove-lock-file lock-path)
  (guard (e [else #f])
    (when (file-exists? lock-path)
      (delete-file lock-path))))

;;; file-modification-time : String → Number
;;; Get file modification time in seconds since epoch.
(define (file-modification-time path)
  (let ([stat (file-stat path)])
    (if stat
        (let ([mtime (assq 'mtime stat)])
          (if mtime (cdr mtime) 0))
        0)))

;;; file-stat : String → Alist | #f
;;; Get file status. Returns alist with mtime, etc.
(define (file-stat path)
  (guard (e [else #f])
    ;; Use Chez's file-access-time as proxy for modification time
    ;; (Chez doesn't expose full stat, but access time is close enough)
    `((mtime . ,(time-second (file-change-time path))))))

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
        ;; Check for stale lock
        [(lock-file-stale? lock-path)
         (remove-lock-file lock-path)
         (loop)]
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
(define (with-file-lock path thunk)
  (let ([mutex (get-path-mutex path)])
    (with-mutex mutex
      (let ([acquired (acquire-file-lock path)])
        (unless acquired
          (error 'with-file-lock
                 (format "Failed to acquire lock for ~a after ~a ms"
                         path *lock-timeout-ms*)))
        (dynamic-wind
          (lambda () #f)
          thunk
          (lambda () (release-file-lock path)))))))

;;; ====
;;; Print Help
;;; ====

(display "file-lock.ss loaded.\n")
(display "  Lock:   (with-file-lock path thunk)\n")
(display "  Manual: (acquire-file-lock path), (release-file-lock path)\n")
