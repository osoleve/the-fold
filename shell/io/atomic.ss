;;; shell/io/atomic.ss — Atomic File Writes
;;;
;;; Implements atomic file writes using write-then-rename pattern.
;;; On POSIX systems, rename() is atomic, ensuring that readers
;;; always see either the old or new complete file, never partial data.
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
         [file-modes (if (null? modes) '(replace) (car modes))])
    ;; Write to temporary file
    (call-with-output-file temp-path
      (lambda (port)
        (writer port)
        ;; Ensure data is flushed to disk
        (flush-output-port port))
      file-modes)
    ;; Atomically rename temp to target
    (rename-file temp-path path)))

;;; call-with-atomic-output-file : String (Port -> a) (List Symbol) -> a
;;; Like call-with-output-file, but atomic.
;;; Returns the result of the writer procedure.
(define (call-with-atomic-output-file path writer . modes)
  (let* ([temp-path (string-append path ".tmp")]
         [file-modes (if (null? modes) '(replace) (car modes))]
         [result #f])
    ;; Write to temporary file and capture result
    (set! result
          (call-with-output-file temp-path
            (lambda (port)
              (let ([r (writer port)])
                ;; Ensure data is flushed to disk
                (flush-output-port port)
                r))
            file-modes))
    ;; Atomically rename temp to target
    (rename-file temp-path path)
    result))
