#!/usr/bin/env scheme-script
;;; docs/technical-report/assemble.ss — Assemble technical report from chapters
;;;
;;; Usage: scheme --script docs/technical-report/assemble.ss
;;;
;;; Reads manifest.sexp, concatenates chapter files in order, writes output.
;;; Used by pre-commit hook to keep technical-report.md in sync with chapters.

(import (chezscheme))

;;; ============================================================
;;; Configuration
;;; ============================================================

(define report-dir "docs/technical-report")
(define manifest-file "docs/technical-report/manifest.sexp")
(define output-file "docs/technical-report.md")

;;; ============================================================
;;; S-expression Reader
;;; ============================================================

(define (read-manifest path)
  (call-with-input-file path read))

;;; ============================================================
;;; File Operations
;;; ============================================================

(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([lines '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (string-join (reverse lines) "\n")
              (loop (cons line lines))))))))

(define (write-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))
    'replace))

(define (string-join strs sep)
  (if (null? strs)
      ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest)
            acc
            (loop (cdr rest)
                  (string-append acc sep (car rest)))))))

;;; ============================================================
;;; Manifest Parsing
;;; ============================================================

(define (get-chapters manifest)
  (let ([chapters-entry (assq 'chapters (cdr manifest))])
    (if chapters-entry
        (cdr chapters-entry)
        (error 'get-chapters "No chapters found in manifest"))))

;;; ============================================================
;;; Assembly
;;; ============================================================

(define (assemble-report)
  (let* ([manifest (read-manifest manifest-file)]
         [chapters (get-chapters manifest)]
         [contents (map (lambda (ch)
                          (let ([path (string-append report-dir "/" ch)])
                            (if (file-exists? path)
                                (read-file path)
                                (begin
                                  (display (format "Warning: ~a not found\n" path))
                                  ""))))
                        chapters)]
         [assembled (string-join contents "\n")])
    (write-file output-file assembled)
    (display (format "Assembled ~a chapters into ~a\n"
                     (length chapters)
                     output-file))))

;;; ============================================================
;;; Entry Point
;;; ============================================================

(assemble-report)
