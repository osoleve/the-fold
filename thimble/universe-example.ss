;;; thimble/universe-example.ss — Example usage of universe-serialize library
;;;
;;; This demonstrates how to use the universe serialization library
;;; programmatically in your own Scheme code.
;;;
;;; Run with: scheme --script shell/universe-example.ss

(import (shell universe-serialize))

(display "Universe Serialization Example\n")
(display "==============================\n\n")

;;; ============================================================
;;; Example 1: Scan for all .sexp files
;;; ============================================================

(display "Example 1: Scanning for .sexp files\n")
(display "------------------------------------\n")

(define root-directory ".")
(define all-sexp-files (scan-sexp-files root-directory))

(display "Found ")
(display (length all-sexp-files))
(display " .sexp files in the universe:\n\n")

(for-each (lambda (file)
            (display "  - ")
            (display (make-relative-path root-directory file))
            (newline))
          all-sexp-files)

;;; ============================================================
;;; Example 2: Filter by directory
;;; ============================================================

(display "\nExample 2: Filtering by directory\n")
(display "-----------------------------------\n")

(define forum-files (scan-sexp-files-filtered root-directory '("forum")))

(display "Found ")
(display (length forum-files))
(display " files in forum/ directory:\n\n")

(for-each (lambda (file)
            (display "  - ")
            (display (make-relative-path root-directory file))
            (newline))
          (if (> (length forum-files) 10)
              (list-head forum-files 10)  ; Show first 10
              forum-files))

(when (> (length forum-files) 10)
  (display "  ... and ")
  (display (- (length forum-files) 10))
  (display " more\n"))

;;; ============================================================
;;; Example 3: Read and analyze content
;;; ============================================================

(display "\nExample 3: Analyzing content\n")
(display "----------------------------\n")

(define poetry-files (scan-sexp-files-filtered root-directory '("forum/poetry")))

(display "Poetry files: ")
(display (length poetry-files))
(newline)

(when (> (length poetry-files) 0)
  (display "\nFirst poetry file:\n")
  (let* ([first-file (car poetry-files)]
         [rel-path (make-relative-path root-directory first-file)]
         [contents (read-sexp-file first-file)])
    (display "  Path: ")
    (display rel-path)
    (newline)
    (when contents
      (display "  Author: ")
      (display (cdr (assoc 'author contents)))
      (newline)
      (when (assoc 'channel contents)
        (display "  Channel: ")
        (display (cdr (assoc 'channel contents)))
        (newline)))))

;;; ============================================================
;;; Example 4: Serialize to memory
;;; ============================================================

(display "\nExample 4: Serializing to memory\n")
(display "---------------------------------\n")

(define scripture-universe (serialize-universe-filtered root-directory '("scripture")))
(define scripture-files (cadr (car scripture-universe)))

(display "Serialized ")
(display (length scripture-files))
(display " scripture file(s)\n")

(for-each (lambda (entry)
            (display "  - ")
            (display (car entry))  ; path
            (newline))
          scripture-files)

;;; ============================================================
;;; Example 5: Write to file
;;; ============================================================

(display "\nExample 5: Writing to file\n")
(display "--------------------------\n")

(define output-file "example-universe-dump.sexp")

(display "Serializing full universe...\n")
(define full-universe (serialize-universe root-directory))

(display "Writing to ")
(display output-file)
(display "...\n")
(write-universe-pretty full-universe output-file)

(display "Success! File size: ")
(display (file-length output-file))
(display " bytes\n")

(display "\nYou can read it back with:\n")
(display "  (define universe (call-with-input-file \"")
(display output-file)
(display "\" read))\n")

;;; ============================================================
;;; Example 6: Custom filtering
;;; ============================================================

(display "\nExample 6: Multiple directory filter\n")
(display "-------------------------------------\n")

(define important-dirs '("forum/poetry" "forum/engineering" "scripture"))
(define filtered-universe (serialize-universe-filtered root-directory important-dirs))
(define filtered-files (cadr (car filtered-universe)))

(display "Files from ")
(display important-dirs)
(display ":\n")
(display "  Total: ")
(display (length filtered-files))
(display " files\n")

;;; ============================================================
;;; Example 7: Statistics
;;; ============================================================

(display "\nExample 7: Universe statistics\n")
(display "-------------------------------\n")

(define (count-by-directory files dir)
  (length (filter (lambda (f) (string-contains? f dir)) files)))

(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? needle (substring haystack i (+ i nlen))) #t]
        [else (loop (+ i 1))]))))

(display "Directory breakdown:\n")
(display "  forum/poetry:      ")
(display (count-by-directory all-sexp-files "forum/poetry"))
(newline)
(display "  forum/engineering: ")
(display (count-by-directory all-sexp-files "forum/engineering"))
(newline)
(display "  scripture:         ")
(display (count-by-directory all-sexp-files "scripture"))
(newline)
(display "  playpen:           ")
(display (count-by-directory all-sexp-files "playpen"))
(newline)

(display "\nExample complete!\n")
