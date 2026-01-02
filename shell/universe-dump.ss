#!/usr/bin/env scheme-script
;;; thimble/universe-dump.ss — CLI tool for dumping the universe
;;;
;;; Serializes all .sexp files in the-fold project to a single file.
;;;
;;; Usage:
;;;   scheme-script shell/universe-dump.ss [options]
;;;
;;; Options:
;;;   --root DIR          Root directory to scan (default: current directory)
;;;   --output FILE       Output file path (default: universe-dump.sexp)
;;;   --pretty            Enable pretty printing (default: compact)
;;;   --filter DIR1,DIR2  Only include files from specified directories
;;;   --help              Show this help message
;;;
;;; Examples:
;;;   # Dump entire universe with pretty printing
;;;   scheme-script shell/universe-dump.ss --pretty
;;;
;;;   # Dump only forum and scripture
;;;   scheme-script shell/universe-dump.ss --filter forum,scripture --pretty
;;;
;;;   # Specify custom root and output
;;;   scheme-script shell/universe-dump.ss --root /path/to/fold --output my-dump.sexp

(import (chezscheme)
        (shell universe-serialize))

;;; NOTE: string utilities provided by core/prelude.ss
(load "core/base/prelude.ss")

;;; ============================================================
;;; Command-line Argument Parsing
;;; ============================================================

(define (print-help)
  (display "Universe Dump Tool\n")
  (display "==================\n\n")
  (display "Serializes all .sexp files in the-fold project to a single file.\n\n")
  (display "Usage:\n")
  (display "  scheme-script shell/universe-dump.ss [options]\n\n")
  (display "Options:\n")
  (display "  --root DIR          Root directory to scan (default: current directory)\n")
  (display "  --output FILE       Output file path (default: universe-dump.sexp)\n")
  (display "  --pretty            Enable pretty printing (default: compact)\n")
  (display "  --filter DIR1,DIR2  Only include files from specified directories\n")
  (display "  --help              Show this help message\n\n")
  (display "Examples:\n")
  (display "  # Dump entire universe with pretty printing\n")
  (display "  scheme-script shell/universe-dump.ss --pretty\n\n")
  (display "  # Dump only forum and scripture\n")
  (display "  scheme-script shell/universe-dump.ss --filter forum,scripture --pretty\n\n")
  (display "  # Specify custom root and output\n")
  (display "  scheme-script shell/universe-dump.ss --root /path/to/fold --output my-dump.sexp\n"))

;;; Parse command-line arguments into options alist
(define (parse-args args)
  (let loop ([remaining args]
             [root-dir "."]
             [output-file "universe-dump.sexp"]
             [pretty? #f]
             [filter-dirs '()])
       (if (null? remaining)
           `((root . ,root-dir)
             (output . ,output-file)
             (pretty . ,pretty?)
             (filter . ,filter-dirs))
           (let ([arg (car remaining)]
                 [rest (cdr remaining)])
                (cond
                 [(string=? arg "--help")
                  (print-help)
                  (exit 0)]
                 [(string=? arg "--pretty")
                  (loop rest root-dir output-file #t filter-dirs)]
                 [(string=? arg "--root")
                  (if (null? rest)
                      (begin
                       (display "Error: --root requires an argument\n")
                       (exit 1))
                      (loop (cdr rest) (car rest) output-file pretty? filter-dirs))]
                 [(string=? arg "--output")
                  (if (null? rest)
                      (begin
                       (display "Error: --output requires an argument\n")
                       (exit 1))
                      (loop (cdr rest) root-dir (car rest) pretty? filter-dirs))]
                 [(string=? arg "--filter")
                  (if (null? rest)
                      (begin
                       (display "Error: --filter requires an argument\n")
                       (exit 1))
                      (let ([dirs (string-split (car rest) #\,)])
                           (loop (cdr rest) root-dir output-file pretty? dirs)))]
                 [else
                  (display "Error: Unknown option: ")
                  (display arg)
                  (newline)
                  (display "Use --help for usage information.\n")
                  (exit 1)])))))


;;; ============================================================
;;; Main Program
;;; ============================================================

(define (main args)
  (let* ([options (parse-args args)]
         [root-dir (cdr (assoc 'root options))]
         [output-file (cdr (assoc 'output options))]
         [pretty? (cdr (assoc 'pretty options))]
         [filter-dirs (cdr (assoc 'filter options))])
        
        ;; Print configuration
        (display "Universe Dump Configuration\n")
        (display "===========================\n")
        (display "Root directory: ")
        (display root-dir)
        (newline)
        (display "Output file: ")
        (display output-file)
        (newline)
        (display "Pretty printing: ")
        (display (if pretty? "enabled" "disabled"))
        (newline)
        (display "Filter directories: ")
        (if (null? filter-dirs)
            (display "none (all directories)")
            (begin
             (display "(")
             (for-each (lambda (d)
                               (display d)
                               (display " "))
                       filter-dirs)
             (display ")")))
        (newline)
        (newline)
        
        ;; Verify root directory exists
        (unless (file-exists? root-dir)
                (display "Error: Root directory does not exist: ")
                (display root-dir)
                (newline)
                (exit 1))
        
        (unless (file-directory? root-dir)
                (display "Error: Root path is not a directory: ")
                (display root-dir)
                (newline)
                (exit 1))
        
        ;; Scan for files
        (display "Scanning for .sexp files...\n")
        (let* ([sexp-files (if (null? filter-dirs)
                               (scan-sexp-files root-dir)
                               (scan-sexp-files-filtered root-dir filter-dirs))]
               [file-count (length sexp-files)])
              (display "Found ")
              (display file-count)
              (display " .sexp file")
              (when (not (= file-count 1))
                    (display "s"))
              (newline)
              (newline)
              
              (when (= file-count 0)
                    (display "No .sexp files found. Nothing to dump.\n")
                    (exit 0))
              
              ;; Show discovered files
              (display "Files to serialize:\n")
              (for-each (lambda (f)
                                (display "  ")
                                (display (make-relative-path root-dir f))
                                (newline))
                        sexp-files)
              (newline)
              
              ;; Serialize
              (display "Serializing universe...\n")
              (let ([universe (if (null? filter-dirs)
                                  (serialize-universe root-dir)
                                  (serialize-universe-filtered root-dir filter-dirs))])
                   
                   ;; Write to file
                   (display "Writing to ")
                   (display output-file)
                   (display "...\n")
                   (write-universe universe output-file pretty?)
                   
                   ;; Report success
                   (display "Success! Universe serialized to ")
                   (display output-file)
                   (newline)
                   
                   ;; Show file size
                   (let ([size (file-length output-file)])
                        (display "Output size: ")
                        (display size)
                        (display " bytes")
                        (if (> size 1024)
                            (begin
                             (display " (")
                             (display (quotient size 1024))
                             (display " KB)")
                             (when (> size (* 1024 1024))
                                   (display " (")
                                   (display (quotient size (* 1024 1024)))
                                   (display " MB)"))))
                        (newline))))))

;;; Entry point
(main (cdr (command-line)))
