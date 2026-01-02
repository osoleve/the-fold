;;; thimble/edit.ss — Text File Editing Utilities
;;;
;;; Enables source code maintenance from within the REPL.
;;; All operations are capability-gated through FS.
;;;
;;; This is Shell code: uses IO, handles files.
;;;
;;; Dependencies (must be loaded before this file):
;;;   shell/fs.ss
;;;   shell/text.ss

(load "shell/tools/string-utils.ss")

;;; ============================================================
;;; Core Text File Operations
;;; ============================================================

;;; read-text-file : FS × Path → String
;;; Read entire file as UTF-8 text.
(define (read-text-file fs path)
  (call-with-input-file path
                        (lambda (port)
                                (get-string-all port))))

;;; write-text-file! : FS × Path × String → void
;;; Write string to file, overwriting if exists.
(define (write-text-file! fs path content)
  (when (file-exists? path)
        (delete-file path))
  (call-with-output-file path
                         (lambda (port)
                                 (put-string port content))))

;;; ============================================================
;;; Line-Oriented Helpers
;;; ============================================================
;;;
;;; NOTE: string-split, string-join, string-replace, string-replace-first
;;; are provided by shell/string-utils.ss which is loaded above.

;;; file->lines : FS × Path → (List String)
;;; Read file and split into lines.
(define (file->lines fs path)
  (string-split (read-text-file fs path) #\newline))

;;; lines->file! : FS × Path × (List String) → void
;;; Join lines with newlines and write to file.
(define (lines->file! fs path lines)
  (write-text-file! fs path (string-join lines "\n")))

;;; ============================================================
;;; Transform-in-Place
;;; ============================================================

;;; edit-file! : FS × Path × (String → String) → void
;;; Apply transformation function to file contents.
(define (edit-file! fs path transform)
  (let* ([content (read-text-file fs path)]
         [new-content (transform content)])
        (write-text-file! fs path new-content)))

;;; ============================================================
;;; String Manipulation Helpers
;;; ============================================================
;;;
;;; NOTE: string-replace from prelude replaces ALL occurrences.
;;; These helpers provide first-occurrence replacement and matching.

;;; ============================================================
;;; File Replacement Operations
;;; ============================================================

;;; read-sexpr-file : FS × Path → (List S-expr)
;;; Read all top-level S-expressions from a file.
(define (read-sexpr-file fs path)
  (call-with-input-file path
                        (lambda (port)
                                (let loop ([exprs '()])
                                     (let ([expr (read port)])
                                          (if (eof-object? expr)
                                              (reverse exprs)
                                              (loop (cons expr exprs))))))))

;;; ============================================================
;;; Line Range Operations
;;; ============================================================

;;; delete-lines : (List String) × Nat × Nat → (List String)
;;; Delete lines from start to end (0-indexed, inclusive).
(define (delete-lines lines start end)
  (let loop ([lines lines] [i 0] [result '()])
       (cond
        [(null? lines) (reverse result)]
        [(and (>= i start) (<= i end))
         (loop (cdr lines) (+ i 1) result)]
        [else
         (loop (cdr lines) (+ i 1) (cons (car lines) result))])))

;;; insert-lines : (List String) × Nat × (List String) → (List String)
;;; Insert new-lines at position (0-indexed).
(define (insert-lines lines pos new-lines)
  (let loop ([lines lines] [i 0] [result '()])
       (cond
        [(and (= i pos) (not (null? new-lines)))
         (loop lines i (append (reverse new-lines) result))]
        [(null? lines) (reverse result)]
        [else
         (loop (cdr lines) (+ i 1) (cons (car lines) result))])))

;;; replace-lines : (List String) × Nat × Nat × (List String) → (List String)
;;; Replace lines from start to end with new-lines.
(define (replace-lines lines start end new-lines)
  (insert-lines (delete-lines lines start end) start new-lines))

;;; ============================================================
;;; Convenience: Edit Lines in File
;;; ============================================================

;;; edit-file-lines! : FS × Path × ((List String) → (List String)) → void
;;; Apply line transformation to file.
(define (edit-file-lines! fs path transform)
  (let* ([lines (file->lines fs path)]
         [new-lines (transform lines)])
        (lines->file! fs path new-lines)))

;;; delete-file-lines! : FS × Path × Nat × Nat → void
;;; Delete lines from start to end in file.
(define (delete-file-lines! fs path start end)
  (edit-file-lines! fs path
                    (lambda (lines) (delete-lines lines start end))))
