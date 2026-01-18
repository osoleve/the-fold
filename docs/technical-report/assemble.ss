#!/usr/bin/env scheme-script
;;; docs/technical-report/assemble.ss — Assemble technical report from chapters
;;;
;;; Usage:
;;;   scheme --script docs/technical-report/assemble.ss         # Markdown only
;;;   scheme --script docs/technical-report/assemble.ss --html  # Also build HTML
;;;
;;; Reads manifest.sexp, concatenates chapter files in order, writes output.
;;; With --html flag, also rebuilds the website HTML versions.

(import (chezscheme))

;;; ====
;;; Configuration
;;; ====

(define report-dir "docs/technical-report")
(define manifest-file "docs/technical-report/manifest.sexp")
(define output-file "docs/technical-report.md")
(define build-html? (member "--html" (command-line-arguments)))

;;; ====
;;; S-expression Reader
;;; ====

(define (read-manifest path)
  (call-with-input-file path read))

;;; ====
;;; File Operations
;;; ====

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

;;; ====
;;; Manifest Parsing
;;; ====

(define (get-chapters manifest)
  (let ([chapters-entry (assq 'chapters (cdr manifest))])
    (if chapters-entry
        (cdr chapters-entry)
        (error 'get-chapters "No chapters found in manifest"))))

;;; ====
;;; Assembly
;;; ====

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

;;; ====
;;; HTML Building
;;; ====

(define (build-html-reports)
  (display "\nBuilding HTML versions for website...\n")
  ;; Build simple HTML report
  (display "  Building technical-report.html...\n")
  (let ([result (system "scheme --script shell/tools/build-report.ss 2>&1")])
    (unless (zero? result)
      (display "  Warning: build-report.ss failed\n")))
  ;; Build navigable HTML report
  (display "  Building technical-report-nav.html...\n")
  (let ([result (system "scheme --script shell/tools/build-nav-report.ss 2>&1")])
    (unless (zero? result)
      (display "  Warning: build-nav-report.ss failed\n")))
  (display "HTML build complete.\n"))

;;; ====
;;; Entry Point
;;; ====

(assemble-report)
(when build-html?
  (build-html-reports))
