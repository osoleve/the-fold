;;; shell/edit.ss — Text File Editing Utilities
;;;
;;; Enables source code maintenance from within the REPL.
;;; All operations are capability-gated through FS.
;;;
;;; This is Shell code: uses IO, handles files.
;;;
;;; Dependencies (must be loaded before this file):
;;;   shell/fs.ss
;;;   shell/text.ss

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

;;; file->lines : FS × Path → (List String)
;;; Read file and split into lines.
(define (file->lines fs path)
  (string-split (read-text-file fs path) #\newline))

;;; lines->file! : FS × Path × (List String) → void
;;; Join lines with newlines and write to file.
(define (lines->file! fs path lines)
  (write-text-file! fs path (string-join lines "\n")))

;;; string-split : String × Char → (List String)
;;; Split string by delimiter character.
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) delim)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))

;;; string-join : (List String) × String → String
;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([strs (cdr strs)]
                 [result (car strs)])
        (if (null? strs)
            result
            (loop (cdr strs)
                  (string-append result sep (car strs)))))))

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

;;; string-replace : String × String × String → String
;;; Replace first occurrence of old with new in str.
(define (string-replace str old new)
  (let ([old-len (string-length old)]
        [str-len (string-length str)])
    (let loop ([i 0])
      (cond
        [(> (+ i old-len) str-len) str]  ; Not found
        [(string-match-at? str old i)
         (string-append
           (substring str 0 i)
           new
           (substring str (+ i old-len) str-len))]
        [else (loop (+ i 1))]))))

;;; string-replace-all : String × String × String → String
;;; Replace all occurrences of old with new in str.
(define (string-replace-all str old new)
  (let ([old-len (string-length old)])
    (if (= old-len 0)
        str
        (let loop ([str str])
          (let ([replaced (string-replace str old new)])
            (if (string=? replaced str)
                str
                (loop replaced)))))))

;;; string-match-at? : String × String × Nat → Boolean
;;; Check if pattern matches at position in str.
(define (string-match-at? str pattern pos)
  (let ([pat-len (string-length pattern)]
        [str-len (string-length str)])
    (and (<= (+ pos pat-len) str-len)
         (string=? pattern (substring str pos (+ pos pat-len))))))

;;; ============================================================
;;; S-Expression File Operations
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
