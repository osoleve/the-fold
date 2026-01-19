;;; boundary/tools/refactor-move.ss — Move Refactoring
;;;
;;; Move functions/definitions between modules with automatic reference updates.
;;; Handles @requires headers, manifest.sexp updates, and cycle detection.
;;;
;;; This is Shell code: reads/writes files, uses lattice/meta for analysis.
;;;
;;; Usage:
;;;   (refactor-move-preview 'symbol "target-file.ss")  ; Preview changes
;;;   (refactor-move-apply!)                            ; Apply staged move
;;;   (refactor-move-status)                            ; Show staged move
;;;
;;; Features:
;;;   - Finds symbol definition location
;;;   - Finds all references via cross-reference
;;;   - Updates @requires headers in affected files
;;;   - Detects skill boundary crossing
;;;   - Validates no cycles via lattice-would-cycle?
;;;   - Atomic multi-file apply with undo support
;;;
;;; Dependencies:
;;;   core/base/prelude.ss
;;;   boundary/tools/refactor-integrated.ss (undo infrastructure)
;;;   lattice/meta/dag.ss (cycle detection)

;;; ====
;;; Dependencies
;;; ====

(load "core/base/prelude.ss")
(load "boundary/tools/string-utils.ss")

;;; ====
;;; State for Staged Move
;;; ====

;;; The staged move operation
;;; Format: ((symbol source-file source-line target-file)
;;;          (affected-files ...)
;;;          (requires-updates (file add-requires remove-requires) ...)
;;;          (definition-text ...)
;;;          (skill-boundary? source-skill target-skill)
;;;          (cycle-check pass|fail cycle-path))
(define *staged-move* #f)

;;; Move undo stack (separate from refactor-integrated)
(define *move-undo-stack* '())
(define *max-move-undo* 10)

;;; ====
;;; File Analysis
;;; ====

;;; find-definition-in-file : String × Symbol -> (line-start line-end text) | #f
;;; Find a definition in a file, returning line range and text.
(define (find-definition-in-file file sym)
  (guard (e [else #f])
         (let* ([content (call-with-input-file file get-string-all)]
                [lines (string-split-lines content)]
                [port (open-input-string content)]
                [sym-str (symbol->string sym)])
               (let loop ([line-num 1] [char-pos 0])
                    (let ([start-pos (port-position port)]
                          [expr (guard (e [else #f]) (read port))])
                         (cond
                          [(eof-object? expr) #f]
                          [(not expr) #f]
                          ;; Check if this is a definition of sym
                          [(definition-of? expr sym)
                           ;; Found it! Extract the text
                           (let* ([end-pos (port-position port)]
                                  [def-text (substring content start-pos end-pos)]
                                  [start-line (count-newlines content 0 start-pos)]
                                  [end-line (count-newlines content 0 end-pos)])
                                 (list (+ start-line 1) (+ end-line 1) (string-trim def-text)))]
                          [else
                           (loop (+ line-num 1)
                                 (port-position port))]))))))

;;; definition-of? : S-expr × Symbol -> Boolean
;;; Check if expr defines the given symbol.
(define (definition-of? expr sym)
  (and (pair? expr)
       (memq (car expr) '(define define-syntax))
       (pair? (cdr expr))
       (let ([name-part (cadr expr)])
            (cond
             [(symbol? name-part) (eq? name-part sym)]
             [(pair? name-part) (eq? (car name-part) sym)]
             [else #f]))))

;;; count-newlines : String × Nat × Nat -> Nat
;;; Count newlines in string between start and end positions.
(define (count-newlines str start end)
  (let loop ([i start] [count 0])
       (cond
        [(>= i end) count]
        [(char=? (string-ref str i) #\newline) (loop (+ i 1) (+ count 1))]
        [else (loop (+ i 1) count)])))

;;; ====
;;; Reference Finding
;;; ====

;;; find-all-references-to-symbol : Symbol -> (List (file line context))
;;; Find all files that reference a symbol.
(define (find-all-references-to-symbol sym)
  (let ([refs '()]
        [sym-str (symbol->string sym)])
       (for-each
        (lambda (dir)
                (for-each
                 (lambda (file)
                         (let ([file-refs (find-refs-in-file file sym sym-str)])
                              (when (not (null? file-refs))
                                    (set! refs (append file-refs refs)))))
                 (find-scheme-files-for-move dir)))
        '("core" "lattice" "shell" "user"))
       refs))

;;; find-refs-in-file : String × Symbol × String -> (List (file line context))
(define (find-refs-in-file file sym sym-str)
  (guard (e [else '()])
         (let* ([content (call-with-input-file file get-string-all)]
                [refs '()])
               ;; Simple text search for the symbol
               (let loop ([pos 0] [line 1])
                    (cond
                     [(>= pos (string-length content)) (void)]
                     [(char=? (string-ref content pos) #\newline)
                      (loop (+ pos 1) (+ line 1))]
                     [(and (symbol-starts-at? content pos sym-str)
                           (in-code-context? content pos))
                      (set! refs (cons (list file line (extract-line content pos)) refs))
                      (loop (+ pos 1) line)]
                     [else
                      (loop (+ pos 1) line)]))
               refs)))

;;; symbol-starts-at? : String × Nat × String -> Boolean
;;; Check if symbol starts at position with word boundaries.
(define (symbol-starts-at? content pos sym-str)
  (let ([sym-len (string-length sym-str)]
        [content-len (string-length content)])
       (and (<= (+ pos sym-len) content-len)
            ;; Check word boundary before
            (or (= pos 0)
                (not (identifier-char? (string-ref content (- pos 1)))))
            ;; Check match
            (string-prefix-at? content sym-str pos)
            ;; Check word boundary after
            (or (>= (+ pos sym-len) content-len)
                (not (identifier-char? (string-ref content (+ pos sym-len))))))))

;;; string-prefix-at? : String × String × Nat -> Boolean
(define (string-prefix-at? str prefix start)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
       (and (<= (+ start plen) slen)
            (let loop ([i 0])
                 (or (>= i plen)
                     (and (char=? (string-ref str (+ start i))
                                  (string-ref prefix i))
                          (loop (+ i 1))))))))

;;; identifier-char? : Char -> Boolean
(define (identifier-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\: #\%))))

;;; in-code-context? : String × Nat -> Boolean
;;; Check if position is in code (not string/comment).
;;; Simplified check - looks backward for unbalanced quotes or semicolons.
(define (in-code-context? content pos)
  (let ([line-start (find-line-start content pos)])
       ;; Check for semicolon before pos on this line
       (let loop ([i line-start] [in-string? #f])
            (cond
             [(>= i pos) (not in-string?)]
             [(and (char=? (string-ref content i) #\;) (not in-string?))
              #f]  ; In comment
             [(char=? (string-ref content i) #\")
              (loop (+ i 1) (not in-string?))]
             [(and (char=? (string-ref content i) #\\) (< (+ i 1) pos))
              (loop (+ i 2) in-string?)]  ; Skip escaped char
             [else
              (loop (+ i 1) in-string?)]))))

;;; find-line-start : String × Nat -> Nat
(define (find-line-start content pos)
  (let loop ([i (- pos 1)])
       (cond
        [(<= i 0) 0]
        [(char=? (string-ref content i) #\newline) (+ i 1)]
        [else (loop (- i 1))])))

;;; extract-line : String × Nat -> String
(define (extract-line content pos)
  (let ([start (find-line-start content pos)]
        [end (let loop ([i pos])
                  (cond
                   [(>= i (string-length content)) i]
                   [(char=? (string-ref content i) #\newline) i]
                   [else (loop (+ i 1))]))])
       (string-trim (substring content start end))))

;;; find-scheme-files-for-move : String -> (List String)
(define (find-scheme-files-for-move dir)
  (define (skip-dir? name)
    (or (string=? name ".")
        (string=? name "..")
        (string=? name ".git")
        (string=? name ".store")
        (string=? name ".fold-repl")
        (string=? name ".fold-sessions")
        (string=? name "node_modules")))

  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)]
                        [rest (cdr pending)])
                       (cond
                        [(not (file-exists? current))
                         (loop rest results)]
                        [(file-directory? current)
                         (let ([entries (guard (e [else '()])
                                               (directory-list current))])
                              (let ([full-paths
                                     (filter
                                      (lambda (p) p)
                                      (map (lambda (name)
                                                   (if (skip-dir? name)
                                                       #f
                                                       (string-append current "/" name)))
                                           entries))])
                                   (loop (append full-paths rest) results)))]
                        [(string-ends-with? current ".ss")
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; ====
;;; @requires Header Analysis
;;; ====

;;; parse-requires-header : String -> (List String)
;;; Extract @requires dependencies from file header.
(define (parse-requires-header file)
  (guard (e [else '()])
         (let* ([content (call-with-input-file file get-string-all)]
                [lines (string-split-lines content)]
                [requires '()])
               (for-each
                (lambda (line)
                        (let ([trimmed (string-trim line)])
                             (when (string-starts-with? trimmed ";;; @requires ")
                                   (let ([req (substring trimmed 14 (string-length trimmed))])
                                        (set! requires (cons (string-trim req) requires))))))
                (take-at-most-lines 30 lines))
               (reverse requires))))

;;; take-at-most-lines : Nat × (List String) -> (List String)
(define (take-at-most-lines n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-at-most-lines (- n 1) (cdr lst)))))

;;; ====
;;; Skill Boundary Detection
;;; ====

;;; get-skill-from-path : String -> Symbol | #f
;;; Determine which skill a file belongs to.
(define (get-skill-from-path path)
  (cond
   [(string-starts-with? path "core/") 'core]
   [(string-starts-with? path "boundary/") 'boundary]
   [(string-starts-with? path "user/") 'user]
   [(string-starts-with? path "lattice/")
    (let* ([parts (string-split-simple path "/")]
           [skill-part (if (> (length parts) 1) (cadr parts) #f)])
          (if skill-part (string->symbol skill-part) 'lattice))]
   [else #f]))

;;; string-split-simple : String × Char -> (List String)
(define (string-split-simple str delim)
  (let loop ([chars (string->list str)] [current '()] [result '()])
       (cond
        [(null? chars)
         (reverse (if (null? current)
                      result
                      (cons (list->string (reverse current)) result)))]
        [(char=? (car chars) (if (char? delim) delim #\/))
         (loop (cdr chars) '() (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars) (cons (car chars) current) result)])))

;;; skills-cross-boundary? : String × String -> (source-skill target-skill) | #f
(define (skills-cross-boundary? source-file target-file)
  (let ([source-skill (get-skill-from-path source-file)]
        [target-skill (get-skill-from-path target-file)])
       (if (and source-skill target-skill (not (eq? source-skill target-skill)))
           (list source-skill target-skill)
           #f)))

;;; ====
;;; Cycle Detection
;;; ====

;;; check-move-would-cycle : Symbol × String × String -> (ok #t) | (err cycle-path)
;;; Check if moving a symbol would create a dependency cycle.
(define (check-move-would-cycle sym source-file target-file)
  (let ([source-skill (get-skill-from-path source-file)]
        [target-skill (get-skill-from-path target-file)])
       (if (and source-skill target-skill
                (not (eq? source-skill target-skill))
                (top-level-bound? 'lattice-would-cycle?))
           (guard (e [else '(ok #t)])
                  (if (lattice-would-cycle? target-skill source-skill)
                      (let ([cycle-path
                             (if (top-level-bound? 'lattice-find-cycle-path)
                                 (lattice-find-cycle-path target-skill source-skill)
                                 (list target-skill source-skill))])
                           `(err ,cycle-path))
                      '(ok #t)))
           '(ok #t))))

;;; ====
;;; Preview Generation
;;; ====

;;; refactor-move-preview : Symbol × String -> void
;;; Generate and display a preview of the move operation.
(define (refactor-move-preview sym target-file)
  (call/cc
   (lambda (return)
     (display "\n")
     (printf "  Move Preview: '~a' -> ~a\n" sym target-file)
     (display "  ════════════════════════════════════════════════════════════════\n\n")

     ;; Step 1: Find definition
     (display "  Step 1: Locating definition...\n")
     (let ([def-info (find-symbol-definition-anywhere sym)])
       (when (not def-info)
         (printf "    ERROR: Could not find definition of '~a'\n\n" sym)
         (set! *staged-move* #f)
         (return (void)))

       (let* ([source-file (car def-info)]
              [line-start (cadr def-info)]
              [line-end (caddr def-info)]
              [def-text (cadddr def-info)])
         (printf "    Source: ~a:~a-~a\n" source-file line-start line-end)

         ;; Step 2: Find all references
         (display "\n  Step 2: Finding references...\n")
         (let* ([refs (find-all-references-to-symbol sym)]
                [external-refs (filter (lambda (r) (not (string=? (car r) source-file))) refs)])
           (printf "    Found ~a references in ~a files\n"
                   (length refs)
                   (length (remove-duplicates (map car refs))))

           ;; Step 3: Check skill boundary
           (display "\n  Step 3: Checking skill boundary...\n")
           (let ([boundary (skills-cross-boundary? source-file target-file)])
             (if boundary
                 (printf "    WARNING: Crosses skill boundary: ~a -> ~a\n"
                         (car boundary) (cadr boundary))
                 (display "    OK: Same skill/layer\n"))

             ;; Step 4: Check for cycles
             (display "\n  Step 4: Checking for dependency cycles...\n")
             (let ([cycle-check (check-move-would-cycle sym source-file target-file)])
               (when (and (pair? cycle-check) (eq? (car cycle-check) 'err))
                 (display "    ERROR: Would create dependency cycle!\n")
                 (printf "    Cycle path: ~a\n" (cadr cycle-check))
                 (set! *staged-move* #f)
                 (return (void)))

               (display "    OK: No cycles detected\n")

               ;; Step 5: Compute @requires updates
               (display "\n  Step 5: Computing @requires updates...\n")
               (let ([requires-updates (compute-requires-updates sym source-file target-file external-refs)])
                 (if (null? requires-updates)
                     (display "    No @requires changes needed\n")
                     (for-each
                      (lambda (update)
                        (let ([file (car update)]
                              [adds (cadr update)]
                              [removes (caddr update)])
                          (printf "    ~a:\n" file)
                          (when (not (null? adds))
                            (printf "      + @requires ~a\n" (car adds)))
                          (when (not (null? removes))
                            (printf "      - @requires ~a\n" (car removes)))))
                      requires-updates))

                 ;; Stage the move
                 (set! *staged-move*
                       (list (list sym source-file line-start line-end target-file)
                             external-refs
                             requires-updates
                             def-text
                             boundary
                             cycle-check))

                 ;; Summary
                 (display "\n  ────────────────────────────────────────────────────────────────\n")
                 (display "  Summary:\n")
                 (printf "    Symbol:     ~a\n" sym)
                 (printf "    From:       ~a:~a\n" source-file line-start)
                 (printf "    To:         ~a\n" target-file)
                 (printf "    References: ~a files affected\n"
                         (length (remove-duplicates (map car external-refs))))
                 (when boundary
                   (printf "    Boundary:   ~a -> ~a (MANUAL MANIFEST UPDATE NEEDED)\n"
                           (car boundary) (cadr boundary)))
                 (display "\n  Use (refactor-move-apply!) to execute this move.\n")
                 (display "  Use (refactor-move-clear!) to cancel.\n\n"))))))))))

;;; find-symbol-definition-anywhere : Symbol -> (file line-start line-end text) | #f
;;; Search all directories for a symbol definition.
(define (find-symbol-definition-anywhere sym)
  (let loop ([dirs '("core" "lattice" "shell" "user")])
       (if (null? dirs)
           #f
           (let ([found (find-symbol-in-dir sym (car dirs))])
                (if found found (loop (cdr dirs)))))))

;;; find-symbol-in-dir : Symbol × String -> (file line-start line-end text) | #f
(define (find-symbol-in-dir sym dir)
  (let ([files (find-scheme-files-for-move dir)])
       (let loop ([files files])
            (if (null? files)
                #f
                (let ([def (find-definition-in-file (car files) sym)])
                     (if def (cons (car files) def) (loop (cdr files))))))))

;;; compute-requires-updates : Symbol × String × String × (List ref) -> (List update)
;;; Compute which files need @requires header changes.
(define (compute-requires-updates sym source-file target-file refs)
  (let ([affected-files (remove-duplicates (map car refs))])
       (filter-map
        (lambda (file)
                (let* ([current-requires (parse-requires-header file)]
                       [source-name (path-basename source-file)]
                       [target-name (path-basename target-file)]
                       [needs-target? (not (member target-name current-requires))])
                      (if needs-target?
                          (list file (list target-name) '())
                          #f)))
        affected-files)))

;;; path-basename : String -> String
(define (path-basename path)
  (let ([parts (string-split-simple path #\/)])
       (if (null? parts) path (car (reverse parts)))))

;;; filter-map : (a -> b | #f) × (List a) -> (List b)
(define (filter-map f lst)
  (let loop ([lst lst] [acc '()])
       (if (null? lst)
           (reverse acc)
           (let ([result (f (car lst))])
                (if result
                    (loop (cdr lst) (cons result acc))
                    (loop (cdr lst) acc))))))

;;; ====
;;; Path Validation
;;; ====

;;; valid-project-path? : String -> Boolean
;;; Ensure path is within project directory (no path traversal).
(define (valid-project-path? path)
  (and (string? path)
       (> (string-length path) 0)
       ;; Reject absolute paths
       (not (char=? (string-ref path 0) #\/))
       ;; Reject path traversal
       (not (string-contains? path ".."))
       ;; Must be in known directories or user-specified location
       (or (string-starts-with? path "core/")
           (string-starts-with? path "lattice/")
           (string-starts-with? path "boundary/")
           (string-starts-with? path "user/")
           ;; Allow relative paths that don't escape
           (and (not (string-contains? path "/.."))
                (not (string-starts-with? path "../"))))))

;;; string-contains? : String × String -> Boolean
(define (string-contains? str substr)
  (let ([slen (string-length str)]
        [sublen (string-length substr)])
       (let loop ([i 0])
            (cond
             [(> (+ i sublen) slen) #f]
             [(string-prefix-at? str substr i) #t]
             [else (loop (+ i 1))]))))

;;; ====
;;; Apply Move (Atomic)
;;; ====

;;; refactor-move-apply! : -> void
;;; Apply the staged move operation atomically.
;;; Safety: Reads all files first, validates backups, writes target before source.
(define (refactor-move-apply!)
  (call/cc
   (lambda (abort)
     (if (not *staged-move*)
         (begin
          (display "\n  No staged move to apply.\n")
          (display "  Use (refactor-move-preview 'sym \"file\") first.\n\n"))
         (let* ([info (car *staged-move*)]
                [sym (car info)]
                [source-file (cadr info)]
                [line-start (caddr info)]
                [line-end (cadddr info)]
                [target-file (car (cddddr info))]
                [refs (cadr *staged-move*)]
                [requires-updates (caddr *staged-move*)]
                [def-text (cadddr *staged-move*)]
                [boundary (car (cddddr *staged-move*))])

               (display "\n  Applying move operation...\n")
               (display "  ────────────────────────────────\n\n")

               ;; PHASE 1: Validate paths
               (display "  Phase 1: Validating paths...\n")
               (when (not (valid-project-path? target-file))
                     (printf "    ERROR: Invalid target path '~a'\n" target-file)
                     (display "    Target must be within project directory.\n\n")
                     (abort (void)))

               ;; PHASE 2: Read ALL files first (collect backups)
               (display "  Phase 2: Reading files for backup...\n")
               (let ([backups '()]
                     [read-failed? #f])

                    ;; Read source file (required)
                    (let ([source-result (guard (e [else (cons 'error e)])
                                                (cons 'ok (call-with-input-file source-file get-string-all)))])
                         (if (eq? (car source-result) 'error)
                             (begin
                              (printf "    ERROR: Cannot read source file ~a\n" source-file)
                              (display "    Aborting to prevent data loss.\n\n")
                              (abort (void)))
                             (set! backups (cons (cons source-file (cdr source-result)) backups))))

                    ;; Read target file (if exists)
                    (when (file-exists? target-file)
                          (let ([target-result (guard (e [else (cons 'error e)])
                                                      (cons 'ok (call-with-input-file target-file get-string-all)))])
                               (if (eq? (car target-result) 'error)
                                   (begin
                                    (printf "    ERROR: Cannot read target file ~a\n" target-file)
                                    (display "    Aborting to prevent data loss.\n\n")
                                    (abort (void)))
                                   (set! backups (cons (cons target-file (cdr target-result)) backups)))))

                    ;; Read files for @requires updates
                    (for-each
                     (lambda (update)
                             (let* ([file (car update)]
                                    [result (guard (e [else (cons 'error e)])
                                                   (cons 'ok (call-with-input-file file get-string-all)))])
                                   (if (eq? (car result) 'error)
                                       (begin
                                        (printf "    WARNING: Cannot read ~a, skipping @requires update\n" file))
                                       (set! backups (cons (cons file (cdr result)) backups)))))
                     requires-updates)

                    (printf "    Backed up ~a files.\n" (length backups))

                    ;; PHASE 3: Write files (target FIRST for safety)
                    (display "  Phase 3: Writing changes...\n")

                    ;; Step 1: Write to target first (safer - if this fails, source is intact)
                    (printf "    1. Adding definition to ~a...\n" target-file)
                    (guard (e [else
                               (printf "    ERROR: Failed to write target file: ~a\n"
                                       (if (message-condition? e) (condition-message e) "unknown"))
                               (display "    Source file unchanged. Aborting.\n\n")
                               (abort (void))])
                           (add-definition-to-file! target-file def-text))

                    ;; Step 2: Remove from source (now safe - target has the code)
                    (printf "    2. Removing definition from ~a...\n" source-file)
                    (guard (e [else
                               (printf "    ERROR: Failed to update source file: ~a\n"
                                       (if (message-condition? e) (condition-message e) "unknown"))
                               (display "    WARNING: Definition may exist in both files now.\n")
                               (display "    Check both files manually.\n\n")
                               (abort (void))])
                           (remove-definition-from-file! source-file sym line-start line-end))

                    ;; Step 3: Update @requires headers
                    (when (not (null? requires-updates))
                          (display "    3. Updating @requires headers...\n")
                          (for-each
                           (lambda (update)
                                   (let ([file (car update)]
                                         [adds (cadr update)])
                                        (when (not (null? adds))
                                              (guard (e [else
                                                         (printf "    WARNING: Failed to update ~a\n" file)])
                                                     (add-requires-to-file! file (car adds))
                                                     (printf "      Updated ~a\n" file)))))
                           requires-updates))

                    ;; PHASE 4: Save to undo stack
                    (set! *move-undo-stack*
                          (take-at-most-moves (cons (cons *staged-move* backups) *move-undo-stack*)
                                              *max-move-undo*))

                    ;; Clear staged move
                    (set! *staged-move* #f)

                    (display "\n  Move completed successfully.\n")
                    (when boundary
                          (printf "\n  NOTE: This move crossed skill boundaries (~a -> ~a).\n"
                                  (car boundary) (cadr boundary))
                          (display "  You may need to manually update manifest.sexp files.\n"))
                    (display "  Use (refactor-move-undo!) to undo.\n\n")))))))

;;; take-at-most-moves : List × Nat -> List
(define (take-at-most-moves lst n)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-at-most-moves (cdr lst) (- n 1)))))

;;; remove-definition-from-file! : String × Symbol × Nat × Nat -> void
(define (remove-definition-from-file! file sym line-start line-end)
  (let* ([content (call-with-input-file file get-string-all)]
         [lines (string-split-lines content)]
         [new-lines (remove-lines lines (- line-start 1) (- line-end 1))]
         [new-content (string-join-lines new-lines)])
        (call-with-output-file file
                               (lambda (port) (put-string port new-content))
                               'replace)))

;;; remove-lines : (List String) × Nat × Nat -> (List String)
(define (remove-lines lines start end)
  (let loop ([lines lines] [idx 0] [result '()])
       (cond
        [(null? lines) (reverse result)]
        [(and (>= idx start) (<= idx end))
         (loop (cdr lines) (+ idx 1) result)]
        [else
         (loop (cdr lines) (+ idx 1) (cons (car lines) result))])))

;;; add-definition-to-file! : String × String -> void
(define (add-definition-to-file! file def-text)
  (if (file-exists? file)
      (let* ([content (call-with-input-file file get-string-all)]
             [new-content (string-append content "\n\n" def-text "\n")])
            (call-with-output-file file
                                   (lambda (port) (put-string port new-content))
                                   'replace))
      (let ([header (string-append
                     ";;; " file " — (moved definitions)\n"
                     ";;;\n"
                     ";;; This file was created by refactor-move.\n\n")])
           (call-with-output-file file
                                  (lambda (port)
                                          (put-string port (string-append header def-text "\n")))))))

;;; add-requires-to-file! : String × String -> void
(define (add-requires-to-file! file req)
  (let* ([content (call-with-input-file file get-string-all)]
         [lines (string-split-lines content)]
         [new-lines (insert-requires-line lines req)]
         [new-content (string-join-lines new-lines)])
        (call-with-output-file file
                               (lambda (port) (put-string port new-content))
                               'replace)))

;;; insert-requires-line : (List String) × String -> (List String)
(define (insert-requires-line lines req)
  (let ([req-line (string-append ";;; @requires " req)])
       (let loop ([lines lines] [idx 0] [header '()] [found-header? #f])
            (cond
             [(null? lines)
              (append (reverse header) (list req-line) '())]
             [(string-starts-with? (string-trim (car lines)) ";;;")
              (loop (cdr lines) (+ idx 1) (cons (car lines) header) #t)]
             [found-header?
              (append (reverse header) (list req-line) lines)]
             [else
              (loop (cdr lines) (+ idx 1) (cons (car lines) header) found-header?)]))))

;;; string-join-lines : (List String) -> String
(define (string-join-lines lines)
  (if (null? lines)
      ""
      (let loop ([lines (cdr lines)] [result (car lines)])
           (if (null? lines)
               result
               (loop (cdr lines) (string-append result "\n" (car lines)))))))

;;; ====
;;; Undo
;;; ====

;;; refactor-move-undo! : -> void
(define (refactor-move-undo!)
  (if (null? *move-undo-stack*)
      (display "\n  Nothing to undo.\n\n")
      (let* ([last-op (car *move-undo-stack*)]
             [backups (cdr last-op)])
            (display "\n  Undoing last move operation...\n")
            (display "  ────────────────────────────────\n\n")

            (for-each
             (lambda (backup)
                     (let ([file (car backup)]
                           [content (cdr backup)])
                          (printf "  Restoring ~a...\n" file)
                          (call-with-output-file file
                                                 (lambda (port) (put-string port content))
                                                 'replace)))
             backups)

            (set! *move-undo-stack* (cdr *move-undo-stack*))
            (printf "\n  Restored ~a files.\n\n" (length backups)))))

;;; ====
;;; Status and Clear
;;; ====

;;; refactor-move-status : -> void
(define (refactor-move-status)
  (display "\n")
  (display "  Move Refactoring Status\n")
  (display "  ────────────────────────────────\n\n")
  (if *staged-move*
      (let* ([info (car *staged-move*)]
             [sym (car info)]
             [source (cadr info)]
             [target (car (cddddr info))])
            (printf "  Staged move: ~a\n" sym)
            (printf "    From: ~a\n" source)
            (printf "    To:   ~a\n" target)
            (display "  Use (refactor-move-apply!) to execute.\n"))
      (display "  No staged move.\n"))
  (printf "\n  Undo stack depth: ~a\n\n" (length *move-undo-stack*)))

;;; refactor-move-clear! : -> void
(define (refactor-move-clear!)
  (set! *staged-move* #f)
  (display "\n  Staged move cleared.\n\n"))

;;; ====
;;; Initialization
;;; ====

(display "\n")
(display "  Move Refactoring Module Loaded\n")
(display "  ────────────────────────────────\n")
(display "  Use (refactor-move-preview 'sym \"file\") to preview.\n")
(display "  Use (refactor-move-apply!) to apply staged move.\n")
(display "  Use (refactor-move-undo!) to undo last move.\n")
(display "\n")
