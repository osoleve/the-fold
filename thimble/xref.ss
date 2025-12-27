;;; thimble/xref.ss — Code Cross-Reference Tool
;;;
;;; Find all references to definitions across the codebase.
;;; Helps understand where functions, variables, and other definitions are used.
;;;
;;; Features:
;;;   - Find all uses of a symbol
;;;   - Find all definitions of a symbol
;;;   - Show context around each reference
;;;   - Support for filtering by directory
;;;   - Generate cross-reference reports
;;;
;;; This is Shell code: reads files, parses S-expressions.
;;;
;;; Usage:
;;;   (xref-find-uses fs "hash-block")
;;;   (xref-find-uses fs "hash-block" "core")  ; search only in core/
;;;   (xref-find-defs fs "hash-block")
;;;   (xref-report fs "hash-block" "xref-report.txt")
;;;   (xref-index fs "core")  ; build index of all definitions
;;;
;;; Dependencies:
;;;   shell/fs.ss
;;;   shell/edit.ss

;;; Set up source-directories to find shell modules
(source-directories (cons "shell" (source-directories)))

(load "fs.ss")
(load "edit.ss")

;;; ============================================================
;;; S-Expression Walking
;;; ============================================================

;;; find-symbol-in-sexpr : Symbol × S-expr × Nat → (List Nat)
;;; Find all positions where symbol appears in s-expression.
;;; Position is the nesting depth/index - used for reporting.
;;; Returns list of line numbers where symbol appears (approximate).
(define (find-symbol-in-sexpr sym sexpr context)
  (let walk ([expr sexpr]
             [results '()]
             [depth 0])
    (cond
      [(symbol? expr)
       (if (eq? expr sym)
           (cons context results)
           results)]
      [(pair? expr)
       (let ([results (walk (car expr) results (+ depth 1))])
         (walk (cdr expr) results (+ depth 1)))]
      [else results])))

;;; is-definition? : S-expr → Boolean
;;; Check if s-expression is a definition form.
(define (is-definition? expr)
  (and (pair? expr)
       (symbol? (car expr))
       (memq (car expr) '(define define-syntax let let* letrec letrec*
                          lambda λ case-lambda
                          define-record-type))))

;;; extract-defined-name : S-expr → Symbol | #f
;;; Extract the name being defined from a definition form.
(define (extract-defined-name expr)
  (cond
    [(not (pair? expr)) #f]
    [(not (pair? (cdr expr))) #f]
    [else
     (let ([form (car expr)]
           [rest (cdr expr)])
       (case form
         [(define define-syntax)
          (let ([name-part (car rest)])
            (cond
              [(symbol? name-part) name-part]
              [(pair? name-part) (car name-part)]  ; (define (name ...) ...)
              [else #f]))]
         [(let let* letrec letrec*)
          ;; Named let: (let name ...)
          (if (and (not (null? rest))
                   (symbol? (car rest)))
              (car rest)
              #f)]
         [(lambda λ case-lambda)
          #f]  ; lambdas don't define names directly
         [(define-record-type)
          (if (and (not (null? rest))
                   (symbol? (car rest)))
              (car rest)
              #f)]
         [else #f]))]))

;;; ============================================================
;;; File Analysis
;;; ============================================================

;;; Reference = (file-path line-number context-line)
;;; where context-line is the actual line of code

;;; search-file-for-symbol : FS × String × Symbol → (List Reference)
;;; Search a file for all occurrences of a symbol.
;;; Returns list of references with file path, line number, and context.
(define (search-file-for-symbol fs file-path sym)
  (guard (e [else '()])
    (let* ([content (read-text-file fs file-path)]
           [lines (string-split-lines content)]
           [port (open-input-string content)])
      ;; Read all s-expressions and track line numbers
      (let loop ([line-no 1]
                 [results '()])
        (let ([expr (guard (e [else #f])
                      (read port))])
          (cond
            [(eof-object? expr)
             (reverse results)]
            [(not expr)  ; parse error
             (reverse results)]
            [else
             ;; Check if symbol appears in this expression
             (let ([found (find-symbol-in-expr sym expr)])
               (if found
                   (let* ([context-line (get-context-line lines line-no)]
                          [ref (list file-path line-no context-line)])
                     (loop (+ line-no 1) (cons ref results)))
                   (loop (+ line-no 1) results)))]))))))

;;; find-symbol-in-expr : Symbol × S-expr → Boolean
;;; Check if symbol appears anywhere in the expression.
(define (find-symbol-in-expr sym expr)
  (cond
    [(symbol? expr) (eq? expr sym)]
    [(pair? expr)
     (or (find-symbol-in-expr sym (car expr))
         (find-symbol-in-expr sym (cdr expr)))]
    [else #f]))

;;; get-context-line : (List String) × Nat → String
;;; Get the line at the given line number (1-indexed).
(define (get-context-line lines line-no)
  (if (and (> line-no 0) (<= line-no (length lines)))
      (list-ref lines (- line-no 1))
      ""))

;;; string-split-lines : String → (List String)
;;; Split string into lines.
(define (string-split-lines str)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (cons (list->string (reverse current)) result))]
      [(char=? (car chars) #\newline)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))

;;; ============================================================
;;; Definition Finding
;;; ============================================================

;;; find-definitions-in-file : FS × String × Symbol → (List Reference)
;;; Find all definitions of a symbol in a file.
(define (find-definitions-in-file fs file-path sym)
  (guard (e [else '()])
    (let* ([content (read-text-file fs file-path)]
           [lines (string-split-lines content)]
           [port (open-input-string content)])
      (let loop ([line-no 1]
                 [results '()])
        (let ([expr (guard (e [else #f])
                      (read port))])
          (cond
            [(eof-object? expr)
             (reverse results)]
            [(not expr)
             (reverse results)]
            [else
             ;; Check if this is a definition of sym
             (let ([defined-name (extract-defined-name expr)])
               (if (and defined-name (eq? defined-name sym))
                   (let* ([context-line (get-context-line lines line-no)]
                          [ref (list file-path line-no context-line)])
                     (loop (+ line-no 1) (cons ref results)))
                   (loop (+ line-no 1) results)))]))))))

;;; ============================================================
;;; Directory Traversal
;;; ============================================================

;;; find-all-scheme-files : FS × String → (List String)
;;; Recursively find all .ss files in a directory.
(define (find-all-scheme-files fs dir-path)
  (define (scheme-file? path)
    (and (>= (string-length path) 3)
         (string=? (substring path (- (string-length path) 3) (string-length path)) ".ss")))

  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")))

  (let loop ([to-process (list dir-path)]
             [result '()])
    (if (null? to-process)
        result
        (let ([current (car to-process)]
              [rest (cdr to-process)])
          (cond
            [(not (file-exists? current))
             (loop rest result)]
            [(and (file-exists? current)
                  (guard (e [else #f])
                    (let ([entries (directory-list current)])
                      (not (null? entries))))
                  (not (scheme-file? current)))
             ;; It's a directory - add its non-skipped contents
             (let ([entries (guard (e [else '()])
                              (directory-list current))])
               (let ([full-paths (filter
                                  (lambda (name) (not (skip-dir? name)))
                                  (map (lambda (e)
                                        (string-append current "/" e))
                                      entries))])
                 (loop (append full-paths rest) result)))]
            [(scheme-file? current)
             (loop rest (cons current result))]
            [else
             (loop rest result)])))))

;;; ============================================================
;;; Public API
;;; ============================================================

;;; xref-find-uses : FS × String × [String] → void
;;; Find all uses of a symbol across the codebase.
(define (xref-find-uses fs sym . opts)
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-refs '()])

    ;; Collect all references
    (for-each
      (lambda (file)
        (let ([refs (search-file-for-symbol fs file symbol)])
          (set! all-refs (append all-refs refs))))
      files)

    ;; Display results
    (display (format "\nCross-reference for '~a' in ~a:\n" symbol dir))
    (display (format "Found ~a references in ~a files.\n\n"
                     (length all-refs)
                     (length (remove-duplicates (map car all-refs)))))

    (if (null? all-refs)
        (display "  (no references found)\n")
        (for-each
          (lambda (ref)
            (let ([file (car ref)]
                  [line-no (cadr ref)]
                  [context (caddr ref)])
              (display (format "  ~a:~a\n" file line-no))
              (display (format "    ~a\n" (string-trim context)))))
          all-refs))
    (display "\n")))

;;; xref-find-defs : FS × String × [String] → void
;;; Find all definitions of a symbol.
(define (xref-find-defs fs sym . opts)
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-defs '()])

    ;; Collect all definitions
    (for-each
      (lambda (file)
        (let ([defs (find-definitions-in-file fs file symbol)])
          (set! all-defs (append all-defs defs))))
      files)

    ;; Display results
    (display (format "\nDefinitions of '~a' in ~a:\n" symbol dir))
    (if (null? all-defs)
        (display "  (no definitions found)\n")
        (for-each
          (lambda (def)
            (let ([file (car def)]
                  [line-no (cadr def)]
                  [context (caddr def)])
              (display (format "  ~a:~a\n" file line-no))
              (display (format "    ~a\n" (string-trim context)))))
          all-defs))
    (display "\n")))

;;; xref-report : FS × String × String × [String] → void
;;; Generate a comprehensive cross-reference report.
(define (xref-report fs sym output-file . opts)
  (let* ([symbol (if (string? sym) (string->symbol sym) sym)]
         [dir (if (null? opts) "." (car opts))]
         [files (find-all-scheme-files fs dir)]
         [all-defs '()]
         [all-refs '()])

    ;; Collect definitions and references
    (for-each
      (lambda (file)
        (let ([defs (find-definitions-in-file fs file symbol)]
              [refs (search-file-for-symbol fs file symbol)])
          (set! all-defs (append all-defs defs))
          (set! all-refs (append all-refs refs))))
      files)

    ;; Write report
    (call-with-output-file output-file
      (lambda (port)
        (display (format "CROSS-REFERENCE REPORT FOR '~a'\n" symbol) port)
        (display "=====================================\n\n" port)
        (display (format "Search directory: ~a\n" dir) port)
        (display (format "Files scanned: ~a\n\n" (length files)) port)

        ;; Definitions section
        (display "DEFINITIONS\n" port)
        (display "-----------\n" port)
        (if (null? all-defs)
            (display "No definitions found.\n\n" port)
            (begin
              (display (format "Found ~a definitions:\n\n" (length all-defs)) port)
              (for-each
                (lambda (def)
                  (let ([file (car def)]
                        [line-no (cadr def)]
                        [context (caddr def)])
                    (display (format "~a:~a\n" file line-no) port)
                    (display (format "  ~a\n\n" (string-trim context)) port)))
                all-defs)))

        ;; References section
        (display "REFERENCES\n" port)
        (display "----------\n" port)
        (if (null? all-refs)
            (display "No references found.\n\n" port)
            (begin
              (display (format "Found ~a references:\n\n" (length all-refs)) port)
              (for-each
                (lambda (ref)
                  (let ([file (car ref)]
                        [line-no (cadr ref)]
                        [context (caddr ref)])
                    (display (format "~a:~a\n" file line-no) port)
                    (display (format "  ~a\n\n" (string-trim context)) port)))
                all-refs)))))

    (display (format "Report written to ~a\n" output-file))))

;;; xref-index : FS × String → void
;;; Build and display an index of all definitions in a directory.
(define (xref-index fs dir-path)
  (let* ([files (find-all-scheme-files fs dir-path)]
         [index (make-eq-hashtable)])

    ;; Build index: symbol -> list of (file line-no context)
    (for-each
      (lambda (file)
        (guard (e [else (void)])
          (let* ([content (read-text-file fs file)]
                 [lines (string-split-lines content)]
                 [port (open-input-string content)])
            (let loop ([line-no 1])
              (let ([expr (guard (e [else #f])
                            (read port))])
                (cond
                  [(eof-object? expr) (void)]
                  [(not expr) (void)]
                  [else
                   (let ([defined-name (extract-defined-name expr)])
                     (when defined-name
                       (let* ([context-line (get-context-line lines line-no)]
                              [entry (list file line-no context-line)]
                              [existing (hashtable-ref index defined-name '())])
                         (hashtable-set! index defined-name (cons entry existing)))))
                   (loop (+ line-no 1))]))))))
      files)

    ;; Display index
    (display (format "\nDefinition Index for ~a:\n" dir-path))
    (display (format "Total symbols: ~a\n\n" (hashtable-size index)))

    (let ([symbols (list-sort
                    (lambda (a b) (string<? (symbol->string a) (symbol->string b)))
                    (vector->list (hashtable-keys index)))])
      (for-each
        (lambda (sym)
          (let ([defs (hashtable-ref index sym '())])
            (display (format "~a (~a definitions)\n" sym (length defs)))
            (for-each
              (lambda (def)
                (display (format "  ~a:~a\n" (car def) (cadr def))))
              defs)))
        symbols))
    (display "\n")))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (cond
                    [(>= i len) len]
                    [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
                    [else i]))]
         [end (let loop ([i (- len 1)])
                (cond
                  [(< i 0) 0]
                  [(char-whitespace? (string-ref str i)) (loop (- i 1))]
                  [else (+ i 1)]))])
    (if (>= start end)
        ""
        (substring str start end))))

;;; remove-duplicates : (List α) → (List α)
;;; Remove duplicate elements from a list.
(define (remove-duplicates lst)
  (let loop ([remaining lst]
             [seen '()])
    (cond
      [(null? remaining) (reverse seen)]
      [(member (car remaining) seen)
       (loop (cdr remaining) seen)]
      [else
       (loop (cdr remaining) (cons (car remaining) seen))])))

(display "Cross-reference tool loaded.\n")
(display "Usage:\n")
(display "  (xref-find-uses fs \"symbol-name\" [\"directory\"])\n")
(display "  (xref-find-defs fs \"symbol-name\" [\"directory\"])\n")
(display "  (xref-report fs \"symbol-name\" \"output-file\" [\"directory\"])\n")
(display "  (xref-index fs \"directory\")\n")
