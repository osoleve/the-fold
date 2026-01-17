;;; shell/io/atomic.ss — Atomic File Writes
;;;
;;; Implements atomic file writes using write-then-rename pattern.
;;; On POSIX systems, rename() is atomic, ensuring that readers
;;; always see either the old or new complete file, never partial data.
;;;
;;; Key features:
;;;   - Unique temp file names prevent collision when multiple processes
;;;     write to the same target concurrently
;;;   - Uses real PID (via FFI when available) + nanosecond timestamp + counter
;;;   - fdatasync before rename ensures data hits disk (when FFI available)
;;;
;;; Durability guarantee (with FFI):
;;;   When POSIX FFI is available, writes are durable: fdatasync() forces
;;;   data to physical storage before rename(). Without FFI, only OS buffer
;;;   flush is performed (data may be lost on power failure).
;;;
;;; Performance note:
;;;   fdatasync is faster than fsync (skips metadata like atime), but still
;;;   causes disk I/O. Use atomic writes only for critical data, not logs.
;;;
;;; This is Shell code: impure (filesystem IO).

(load "core/base/prelude.ss")

;;; ====
;;; Unique Temp File Generation
;;; ====

;;; *atomic-temp-counter* : Number
;;; Per-process counter to ensure uniqueness even within the same nanosecond
(define *atomic-temp-counter* 0)

;;; *atomic-counter-mutex* : Mutex
;;; Mutex to protect counter increment (Fixed: fold-zxn9)
(define *atomic-counter-mutex* (make-mutex))

;;; *atomic-pid* : Number | #f
;;; Cached process ID (computed lazily)
(define *atomic-pid* #f)

;;; get-atomic-pid : → Number
;;; Get process ID for temp file naming.
;;; Uses real PID via FFI when available, falls back to pseudo-PID.
(define (get-atomic-pid)
  (unless *atomic-pid*
    (set! *atomic-pid*
          (guard (e [else (pseudo-pid)])
            ;; Try to load and use POSIX FFI
            (load "shell/ffi/posix-ffi.ss")
            (if (and (top-level-bound? 'posix-load!)
                     (posix-load!))
                (posix-getpid)
                (pseudo-pid)))))
  *atomic-pid*)

;;; pseudo-pid : → Number
;;; Generate a pseudo-PID when real PID is unavailable.
;;; Uses memory address as unique-ish identifier.
(define (pseudo-pid)
  (let ([addr (ftype-pointer-address (make-ftype-pointer void* (foreign-alloc 8)))])
    (foreign-free addr)
    (modulo addr 1000000)))

;;; unique-temp-path : String → String
;;; Generate a unique temporary file path for atomic writes.
;;; Uses: original path + PID + nanoseconds + counter
;;; This ensures uniqueness even when multiple processes target the same file.
;;; Thread-safe: uses mutex for counter. (Fixed: fold-zxn9)
(define (unique-temp-path path)
  (let ([counter-val (with-mutex *atomic-counter-mutex*
                       (set! *atomic-temp-counter* (+ 1 *atomic-temp-counter*))
                       *atomic-temp-counter*)])
    (format "~a.~a.~a.~a.tmp"
            path
            (get-atomic-pid)
            (time-nanosecond (current-time))
            counter-val)))

;;; ====
;;; Durability Support
;;; ====

;;; *fdatasync-available* : Boolean | 'unchecked
;;; Whether fdatasync is available (lazily checked)
(define *fdatasync-available* 'unchecked)

;;; check-fdatasync-available! : → Boolean
;;; Check if fdatasync is available via POSIX FFI.
(define (check-fdatasync-available!)
  (when (eq? *fdatasync-available* 'unchecked)
    (set! *fdatasync-available*
          (guard (e [else #f])
            (and (top-level-bound? 'posix-available?)
                 (posix-available?)
                 (top-level-bound? 'posix-fdatasync)))))
  *fdatasync-available*)

;;; sync-port-to-disk! : Port → Boolean
;;; Sync port data to disk using fdatasync.
;;; Returns #t if sync succeeded, #f if unavailable or failed.
;;;
;;; IMPORTANT: Call this AFTER flush-output-port but BEFORE close.
;;; The port must still be open to get its file descriptor.
(define (sync-port-to-disk! port)
  (and (check-fdatasync-available!)
       (let ([fd (port-file-descriptor port)])
         (and fd (posix-fdatasync fd)))))

;;; ====
;;; Secure Temp File Creation (Fixed: fold-zxn8)
;;; ====

;;; open-temp-file-securely : String → Port
;;; Open a temporary file securely using O_CREAT|O_EXCL to prevent symlink attacks.
;;; Falls back to standard open if POSIX FFI is unavailable.
;;;
;;; Security guarantees (with FFI):
;;;   - O_EXCL ensures we create a new file (fails if file exists)
;;;   - Prevents symlink attacks where attacker pre-creates symlink
;;;   - File is opened with truncate semantics (no trailing garbage)
(define (open-temp-file-securely temp-path)
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

;;; ====
;;; Atomic Write Operations
;;; ====

;;; atomic-write-file : String (Port -> Void) (List Symbol) -> Void
;;; Atomically write to a file using write-then-rename pattern.
;;;
;;; Parameters:
;;;   path: Target file path
;;;   writer: Procedure that writes to the port
;;;   modes: Optional file modes (currently unused, reserved for future)
;;;
;;; Implementation:
;;;   1. Create unique temporary file securely (O_EXCL prevents symlink attacks)
;;;   2. Write content via provided writer procedure
;;;   3. Flush Scheme buffers to OS
;;;   4. fdatasync to force data to disk (if FFI available)
;;;   5. Close the temporary file
;;;   6. Atomically rename temp file to target path
;;;
;;; Security: Uses O_CREAT|O_EXCL to prevent symlink attacks. (Fixed: fold-zxn8)
;;; Durability: With FFI available, data survives power failure.
;;; Without FFI, data is in OS buffers and may be lost on crash.
(define (atomic-write-file path writer . modes)
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

;;; call-with-atomic-output-file : String (Port -> a) (List Symbol) -> a
;;; Like call-with-output-file, but atomic and durable.
;;; Returns the result of the writer procedure.
;;;
;;; Implementation:
;;;   1. Create unique temporary file securely (O_EXCL prevents symlink attacks)
;;;   2. Execute writer procedure, capturing result
;;;   3. Flush Scheme buffers to OS
;;;   4. fdatasync to force data to disk (if FFI available)
;;;   5. Close the temporary file
;;;   6. Atomically rename temp file to target path
;;;
;;; Security: Uses O_CREAT|O_EXCL to prevent symlink attacks. (Fixed: fold-zxn8)
(define (call-with-atomic-output-file path writer . modes)
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
