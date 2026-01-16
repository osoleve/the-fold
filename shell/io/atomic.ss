;;; shell/io/atomic.ss — Atomic File Writes
;;;
;;; Implements atomic file writes using write-then-rename pattern.
;;; On POSIX systems, rename() is atomic, ensuring that readers
;;; always see either the old or new complete file, never partial data.
;;;
;;; Note on durability:
;;;   - flush-output-port flushes to OS buffers
;;;   - For true durability, fsync is needed (not yet implemented)
;;;   - On crash before fsync, renamed file may have incomplete content
;;;
;;; This is Shell code: impure (filesystem IO).

(load "core/base/prelude.ss")

;;; ====
;;; Atomic Write Operations
;;; ====

;;; atomic-write-file : String (Port -> Void) (List Symbol) -> Void
;;; Atomically write to a file using write-then-rename pattern.
;;;
;;; Parameters:
;;;   path: Target file path
;;;   writer: Procedure that writes to the port
;;;   modes: Optional file modes (e.g., '(replace))
;;;
;;; Implementation:
;;;   1. Write to temporary file (path.tmp)
;;;   2. Flush and close the temporary file
;;;   3. Atomically rename temp file to target path
;;;
;;; This ensures readers never see partial writes.
(define (atomic-write-file path writer . modes)
  (let* ([temp-path (string-append path ".tmp")]
         [file-modes (if (null? modes) '(replace) (car modes))]
         [write-done #f])
    ;; Use guard to clean up temp file on error
    (guard (e [else
               (unless write-done
                 (guard (cleanup-e [else #f])
                   (when (file-exists? temp-path)
                     (delete-file temp-path))))
               (raise e)])
      ;; Write to temporary file
      (call-with-output-file temp-path
        (lambda (port)
          (writer port)
          ;; Ensure data is flushed to OS buffers
          (flush-output-port port))
        file-modes)
      ;; Atomically rename temp to target
      (rename-file temp-path path)
      (set! write-done #t))))

;;; call-with-atomic-output-file : String (Port -> a) (List Symbol) -> a
;;; Like call-with-output-file, but atomic.
;;; Returns the result of the writer procedure.
(define (call-with-atomic-output-file path writer . modes)
  (let* ([temp-path (string-append path ".tmp")]
         [file-modes (if (null? modes) '(replace) (car modes))]
         [result #f]
         [write-done #f])
    ;; Use guard to clean up temp file on error
    (guard (e [else
               (unless write-done
                 (guard (cleanup-e [else #f])
                   (when (file-exists? temp-path)
                     (delete-file temp-path))))
               (raise e)])
      ;; Write to temporary file and capture result
      (set! result
            (call-with-output-file temp-path
              (lambda (port)
                (let ([r (writer port)])
                  ;; Ensure data is flushed to OS buffers
                  (flush-output-port port)
                  r))
              file-modes))
      ;; Atomically rename temp to target
      (rename-file temp-path path)
      (set! write-done #t))
    result))
