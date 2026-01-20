(load "core/base/prelude.ss")

(doc 'module 'atomic)
(doc 'description "Atomic File Writes — Implements atomic file writes using write-then-rename pattern. On POSIX systems, rename() is atomic, ensuring that readers always see either the old or new complete file, never partial data.")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(prelude))
(doc 'note "Key features: unique temp file names prevent collision when multiple processes write to the same target concurrently; uses real PID (via FFI when available) + nanosecond timestamp + counter; fdatasync before rename ensures data hits disk (when FFI available).")

(doc 'section 'durability)
(doc 'note "Durability guarantee (with FFI): When POSIX FFI is available, writes are durable: fdatasync() forces data to physical storage before rename(). Without FFI, only OS buffer flush is performed (data may be lost on power failure). Performance note: fdatasync is faster than fsync (skips metadata like atime), but still causes disk I/O. Use atomic writes only for critical data, not logs.")

(doc 'section 'unique-temp-file-generation)

(doc *atomic-temp-counter* 'type Number)
(doc *atomic-temp-counter* 'description "Per-process counter to ensure uniqueness even within the same nanosecond.")
(define *atomic-temp-counter* 0)

(doc *atomic-counter-mutex* 'type Mutex)
(doc *atomic-counter-mutex* 'description "Mutex to protect counter increment (Fixed: fold-zxn9).")
(define *atomic-counter-mutex* (make-mutex))

(doc *atomic-pid-mutex* 'type Mutex)
(doc *atomic-pid-mutex* 'description "Mutex to protect PID initialization (Fixed: fold-zxnd).")
(define *atomic-pid-mutex* (make-mutex))

(doc *atomic-pid* 'type (Maybe Number))
(doc *atomic-pid* 'description "Cached process ID (computed lazily).")
(define *atomic-pid* #f)

(define (get-atomic-pid)
  (doc 'type (-> Number))
  (doc 'description "Get process ID for temp file naming. Uses real PID via FFI when available, falls back to pseudo-PID. Thread-safe: uses mutex for initialization. (Fixed: fold-zxnd)")
  (doc 'export #t)
  (with-mutex *atomic-pid-mutex*
    (unless *atomic-pid*
      (set! *atomic-pid*
            (guard (e [else (pseudo-pid)])
              ;; Try to load and use POSIX FFI
              (load "boundary/ffi/posix-ffi.ss")
              (if (and (top-level-bound? 'posix-load!)
                       (posix-load!))
                  (posix-getpid)
                  (pseudo-pid))))))
  *atomic-pid*)

(define (pseudo-pid)
  (doc 'type (-> Number))
  (doc 'description "Generate a pseudo-PID when real PID is unavailable. Uses random number to avoid ASLR information leakage. (Fixed: fold-zxna)")
  (random 1000000))

(define (unique-temp-path path)
  (doc 'type (-> String String))
  (doc 'description "Generate a unique temporary file path for atomic writes. Uses: original path + PID + nanoseconds + counter. This ensures uniqueness even when multiple processes target the same file. Thread-safe: uses mutex for counter. (Fixed: fold-zxn9)")
  (doc 'export #t)
  (let ([counter-val (with-mutex *atomic-counter-mutex*
                       (set! *atomic-temp-counter* (+ 1 *atomic-temp-counter*))
                       *atomic-temp-counter*)])
    (format "~a.~a.~a.~a.tmp"
            path
            (get-atomic-pid)
            (time-nanosecond (current-time))
            counter-val)))

(doc 'section 'durability-support)

(doc *fdatasync-available* 'type (Or Bool (Quote unchecked)))
(doc *fdatasync-available* 'description "Whether fdatasync is available (lazily checked).")
(define *fdatasync-available* 'unchecked)

(define (check-fdatasync-available!)
  (doc 'type (-> Bool))
  (doc 'description "Check if fdatasync is available via POSIX FFI.")
  (when (eq? *fdatasync-available* 'unchecked)
    (set! *fdatasync-available*
          (guard (e [else #f])
            (and (top-level-bound? 'posix-available?)
                 (posix-available?)
                 (top-level-bound? 'posix-fdatasync)))))
  *fdatasync-available*)

(define (sync-port-to-disk! port)
  (doc 'type (-> Port Bool))
  (doc 'description "Sync port data to disk using fdatasync. Returns #t if sync succeeded, #f if unavailable or failed. IMPORTANT: Call this AFTER flush-output-port but BEFORE close. The port must still be open to get its file descriptor.")
  (doc 'export #t)
  (and (check-fdatasync-available!)
       (let ([fd (port-file-descriptor port)])
         (and fd (posix-fdatasync fd)))))

(doc 'section 'secure-temp-file-creation)
(doc 'note "Fixed: fold-zxn8. Security guarantees (with FFI): O_EXCL ensures we create a new file (fails if file exists), prevents symlink attacks where attacker pre-creates symlink, file is opened with truncate semantics (no trailing garbage).")

(define (open-temp-file-securely temp-path)
  (doc 'type (-> String Port))
  (doc 'description "Open a temporary file securely using O_CREAT|O_EXCL to prevent symlink attacks. Falls back to standard open if POSIX FFI is unavailable.")
  (doc 'export #t)
  (if (check-fdatasync-available!)
      ;; Secure path: use POSIX FFI with O_CREAT|O_EXCL
      (let ([fd-result (posix-open temp-path
                                   (bitwise-ior O_CREAT O_EXCL O_WRONLY O_CLOEXEC)
                                   #o644)])
        (if (and (pair? fd-result) (eq? (car fd-result) 'error))
            ;; O_EXCL failure means collision - this shouldn't happen with
            ;; our unique path generation, but handle it gracefully
            (error 'open-temp-file-securely
                   "Failed to create temp file (collision or symlink attack?)"
                   temp-path fd-result)
            ;; Convert fd to Scheme output port with transcoder
            (transcoded-port
             (open-fd-output-port fd-result (buffer-mode block))
             (native-transcoder))))
      ;; Fallback: standard Scheme open (less secure, but functional)
      ;; Note: This path is vulnerable to symlink attacks but provides
      ;; compatibility when FFI is unavailable
      (open-file-output-port temp-path
                             (file-options no-fail)  ; removed no-truncate
                             (buffer-mode block)
                             (native-transcoder))))

(doc 'section 'atomic-write-operations)

(define (atomic-write-file path writer . modes)
  (doc 'type (-> String (-> Port Void) Void))
  (doc 'description "Atomically write to a file using write-then-rename pattern. Steps: 1. Create unique temporary file securely (O_EXCL prevents symlink attacks), 2. Write content via provided writer procedure, 3. Flush Scheme buffers to OS, 4. fdatasync to force data to disk (if FFI available), 5. Close the temporary file, 6. Atomically rename temp file to target path. Security: Uses O_CREAT|O_EXCL to prevent symlink attacks. (Fixed: fold-zxn8)")
  (doc 'export #t)
  (let* ([temp-path (unique-temp-path path)]
         [write-done #f])
    ;; Use guard to clean up temp file on error
    (guard (e [else
               (unless write-done
                 (guard (cleanup-e [else #f])
                   (when (file-exists? temp-path)
                     (delete-file temp-path))))
               (raise e)])
      ;; Write to temporary file with secure creation
      (let ([port (open-temp-file-securely temp-path)])
        (guard (write-e [else (close-port port) (raise write-e)])
          (writer port)
          ;; Step 1: Flush Scheme buffers to OS
          (flush-output-port port)
          ;; Step 2: fdatasync to force data to disk (if available)
          (sync-port-to-disk! port)
          ;; Step 3: Close port
          (close-port port)))
      ;; Atomically rename temp to target
      (rename-file temp-path path)
      (set! write-done #t))))

(define (call-with-atomic-output-file path writer . modes)
  (doc 'type (-> String (-> Port α) α))
  (doc 'description "Like call-with-output-file, but atomic and durable. Returns the result of the writer procedure. Security: Uses O_CREAT|O_EXCL to prevent symlink attacks. (Fixed: fold-zxn8)")
  (doc 'export #t)
  (let* ([temp-path (unique-temp-path path)]
         [result #f]
         [write-done #f])
    ;; Use guard to clean up temp file on error
    (guard (e [else
               (unless write-done
                 (guard (cleanup-e [else #f])
                   (when (file-exists? temp-path)
                     (delete-file temp-path))))
               (raise e)])
      ;; Write to temporary file with secure creation
      (let ([port (open-temp-file-securely temp-path)])
        (guard (write-e [else (close-port port) (raise write-e)])
          (set! result (writer port))
          ;; Step 1: Flush Scheme buffers to OS
          (flush-output-port port)
          ;; Step 2: fdatasync to force data to disk (if available)
          (sync-port-to-disk! port)
          ;; Step 3: Close port
          (close-port port)))
      ;; Atomically rename temp to target
      (rename-file temp-path path)
      (set! write-done #t))
    result))
