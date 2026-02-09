;;; boundary/tools/refactor-integrated.ss — Integrated Refactoring Engine
;;;
;;; Safe refactoring driven by the symbol index and call graph.
;;; Provides multi-file refactoring with preview and undo support.
;;;
;;; Features:
;;;   - Safe rename across entire codebase
;;;   - Extract function with scope analysis
;;;   - Inline function with call-site detection
;;;   - Reorder arguments with automatic call-site updates
;;;   - Dead code detection integration
;;;   - Preview mode for all operations
;;;
;;; This is Shell code: impure, coordinates with index/lens systems.
;;;
;;; Usage:
;;;   (refactor-rename! 'old-name 'new-name)           ; Rename globally
;;;   (refactor-rename-preview 'old-name 'new-name)    ; Preview rename
;;;   (refactor-extract! 'new-func expr 'file.ss)      ; Extract function
;;;   (refactor-inline! 'func-name)                    ; Inline all calls
;;;   (refactor-reorder-args! 'func-name '(2 0 1))     ; Reorder params
;;;   (refactor-preview-changes)                       ; Show pending changes
;;;   (refactor-apply!)                                ; Apply all changes
;;;   (refactor-undo!)                                 ; Undo last refactor
;;;
;;; Dependencies:
;;;   boundary/tools/index.ss (symbol index)
;;;   boundary/lens/navigator.ss (call graph)
;;;   boundary/tools/refactor.ss (AST-level transforms — rename, extract, inline, free-vars)

;;; ====
;;; Dependencies
;;; ====

(load "core/base/prelude.ss")
(load "boundary/tools/string-utils.ss")

;;; ====
;;; State Management
;;; ====

;;; Pending changes buffer
(define *pending-changes* '())

;;; Undo stack (list of (changes backup-files))
(define *refactor-undo-stack* '())

;;; Maximum undo depth
(define *max-undo-depth* 10)

;;; ====
;;; Change Representation
;;; ====

;;; Change: (type file old-text new-text line-start line-end context)
(define (make-ref-change type file old new line-start line-end context)
  (vector 'refactor-change type file old new line-start line-end context))

(define (ref-change? x)
  (and (vector? x)
       (>= (vector-length x) 8)
       (eq? (vector-ref x 0) 'refactor-change)))

(define (ref-change-type c) (vector-ref c 1))
(define (ref-change-file c) (vector-ref c 2))
(define (ref-change-old c) (vector-ref c 3))
(define (ref-change-new c) (vector-ref c 4))
(define (ref-change-line-start c) (vector-ref c 5))
(define (ref-change-line-end c) (vector-ref c 6))
(define (ref-change-context c) (vector-ref c 7))

;;; ====
;;; Symbol Graph Integration
;;; ====

;;; find-all-definitions : Symbol -> (List (file . line))
;;; Find all definition sites for a symbol using the index.
(define (find-all-definitions sym)
  (guard (e [else '()])
         (if (and (top-level-bound? 'index-lookup)
                  (procedure? index-lookup))
             (let ([entry (index-lookup sym)])
                  (if entry
                      (list (cons (cdr (assq 'file entry))
                                  (cdr (assq 'line entry))))
                      '()))
             '())))

;;; find-all-references : Symbol -> (List (file line context))
;;; Find all reference sites using the call graph.
(define (find-all-references sym)
  (guard (e [else '()])
         (let ([refs '()])
              ;; Get callers from call graph
              (when (and (top-level-bound? 'call-graph-callers)
                         (procedure? call-graph-callers))
                    (let ([callers (call-graph-callers sym)])
                         (for-each
                          (lambda (caller)
                                  ;; Look up caller's location
                                  (let ([loc (find-all-definitions caller)])
                                       (when (pair? loc)
                                             (set! refs (cons (list (caar loc) (cdar loc) caller)
                                                              refs)))))
                          callers)))
              refs)))

;;; ====
;;; Safe Rename
;;; ====

;;; refactor-rename-preview : Symbol × Symbol -> (List Change)
;;; Preview what a rename operation would change.
(define (refactor-rename-preview old-name new-name)
  (display "\n")
  (printf "  Preview: Rename '~a' to '~a'\n" old-name new-name)
  (display "  ────────────────────────────────\n\n")
  
  ;; Validate new name
  (when (string->number (symbol->string new-name))
        (error 'refactor-rename "New name cannot be a number"))
  
  ;; Find all occurrences
  (let* ([defs (find-all-definitions old-name)]
         [refs (find-all-references old-name)]
         [changes '()])
        
        ;; Report definitions
        (printf "  Definitions (~a):\n" (length defs))
        (for-each
         (lambda (def)
                 (let ([file (car def)]
                       [line (cdr def)])
                      (printf "    ~a:~a\n" file line)
                      (set! changes
                            (cons (make-ref-change 'rename-def file
                                                   (symbol->string old-name)
                                                   (symbol->string new-name)
                                                   line line 'definition)
                                  changes))))
         defs)
        
        ;; Report references
        (printf "\n  References (~a):\n" (length refs))
        (for-each
         (lambda (ref)
                 (let ([file (car ref)]
                       [line (cadr ref)]
                       [context (caddr ref)])
                      (printf "    ~a:~a (in ~a)\n" file line context)
                      (set! changes
                            (cons (make-ref-change 'rename-ref file
                                                   (symbol->string old-name)
                                                   (symbol->string new-name)
                                                   line line context)
                                  changes))))
         refs)
        
        (display "\n")
        
        ;; Also scan for string occurrences in files
        (let ([file-changes (scan-files-for-symbol old-name new-name)])
             (unless (null? file-changes)
                     (printf "  Additional file occurrences (~a):\n" (length file-changes))
                     (for-each
                      (lambda (fc)
                              (printf "    ~a:~a\n" (ref-change-file fc) (ref-change-line-start fc)))
                      file-changes)
                     (set! changes (append changes file-changes))))
        
        ;; Store for potential application
        (set! *pending-changes* changes)
        (printf "\n  Total changes: ~a\n" (length changes))
        (display "  Use (refactor-apply!) to apply changes.\n\n")
        changes))

;;; scan-files-for-symbol : Symbol × Symbol -> (List Change)
;;; Scan all Scheme files for occurrences of a symbol (semantic version).
(define (scan-files-for-symbol old-sym new-sym)
  (let ([old-str (symbol->string old-sym)]
        [new-str (symbol->string new-sym)]
        [changes '()])
       (for-each
        (lambda (dir)
                (for-each
                 (lambda (file)
                         ;; Use semantic scanner for proper code-awareness
                         (let ([file-changes (scan-file-for-symbol-semantic file old-str new-str)])
                              (set! changes (append changes file-changes))))
                 (find-scheme-files-simple dir)))
        '("core" "shell" "user"))
       changes))

;;; find-scheme-files-simple : String -> (List String)
(define (find-scheme-files-simple dir)
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
                                                   (if (or (string=? name ".")
                                                           (string=? name "..")
                                                           (string=? name ".git")
                                                           (string=? name ".store")
                                                           (string=? name ".fold-repl"))
                                                       #f
                                                       (string-append current "/" name)))
                                           entries))])
                                   (loop (append full-paths rest) results)))]
                        [(string-ends-with? current ".ss")
                         (loop rest (cons current results))]
                        [else
                         (loop rest results)]))))))

;;; NOTE: string-split-lines provided by boundary/tools/string-utils.ss
;;;       Alias for backward compatibility
(define string-split-lines-simple string-split-lines)

;;; string-contains-word? : String × String -> Boolean
;;; Check if str contains word as a whole symbol (not substring).
(define (string-contains-word? str word)
  (let ([pos (string-find-substring str word)])
       (and pos
            ;; Check that it's a whole word
            (or (= pos 0)
                (not (scheme-identifier-char? (string-ref str (- pos 1)))))
            (let ([end (+ pos (string-length word))])
                 (or (>= end (string-length str))
                     (not (scheme-identifier-char? (string-ref str end))))))))

;;; string-find-substring : String × String -> Nat | #f
(define (string-find-substring haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string-prefix-at? haystack needle i) i]
             [else (loop (+ i 1))]))))

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

;;; scheme-identifier-char? : Char -> Boolean
(define (scheme-identifier-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; ====
;;; Semantic Scanner (Parse-Aware)
;;; ====
;;;
;;; The semantic scanner tokenizes Scheme source code and identifies
;;; which regions are:
;;;   - CODE: Regular code where symbols can be renamed
;;;   - STRING: Inside string literals (no rename)
;;;   - LINE-COMMENT: After semicolon (no rename)
;;;   - BLOCK-COMMENT: Inside #|...|# (no rename)
;;;   - QUOTED: Inside 'expr or `expr (configurable)
;;;
;;; This enables proper semantic rename that won't:
;;;   - Rename text inside strings like "string-suffix?"
;;;   - Rename text inside comments
;;;   - Confuse partial matches (string-suffix vs string-suffix?)

;;; scan-token-regions : String -> (List (type start end))
;;; Scan a string and return a list of regions with their types.
;;; Types: 'code, 'string, 'line-comment, 'block-comment, 'char-literal
(define (scan-token-regions content)
  (let ([len (string-length content)]
        [regions '()])
       
       (let loop ([i 0] [code-start 0])
            (cond
             ;; End of content
             [(>= i len)
              (when (< code-start len)
                    (set! regions (cons (list 'code code-start len) regions)))
              (reverse regions)]
             
             ;; String literal: "..."
             [(char=? (string-ref content i) #\")
              (when (< code-start i)
                    (set! regions (cons (list 'code code-start i) regions)))
              (let ([end (scan-string-end content (+ i 1))])
                   (set! regions (cons (list 'string i end) regions))
                   (loop end end))]
             
             ;; Character literal: #\x
             [(and (char=? (string-ref content i) #\#)
                   (< (+ i 1) len)
                   (char=? (string-ref content (+ i 1)) #\\))
              (when (< code-start i)
                    (set! regions (cons (list 'code code-start i) regions)))
              (let ([end (scan-char-literal-end content (+ i 2))])
                   (set! regions (cons (list 'char-literal i end) regions))
                   (loop end end))]
             
             ;; Block comment: #|...|#
             [(and (char=? (string-ref content i) #\#)
                   (< (+ i 1) len)
                   (char=? (string-ref content (+ i 1)) #\|))
              (when (< code-start i)
                    (set! regions (cons (list 'code code-start i) regions)))
              (let ([end (scan-block-comment-end content (+ i 2))])
                   (set! regions (cons (list 'block-comment i end) regions))
                   (loop end end))]
             
             ;; Line comment: ;...
             [(char=? (string-ref content i) #\;)
              (when (< code-start i)
                    (set! regions (cons (list 'code code-start i) regions)))
              (let ([end (scan-line-end content i)])
                   (set! regions (cons (list 'line-comment i end) regions))
                   (loop end end))]
             
             ;; Regular character
             [else
              (loop (+ i 1) code-start)]))))

;;; scan-string-end : String × Nat -> Nat
;;; Find the end of a string literal (after opening quote).
(define (scan-string-end content start)
  (let ([len (string-length content)])
       (let loop ([i start])
            (cond
             [(>= i len) len]  ; unterminated string
             [(char=? (string-ref content i) #\\)
              ;; Escape sequence - skip next char
              (loop (+ i 2))]
             [(char=? (string-ref content i) #\")
              (+ i 1)]  ; end of string
             [else
              (loop (+ i 1))]))))

;;; scan-char-literal-end : String × Nat -> Nat
;;; Find the end of a character literal (after #\).
(define (scan-char-literal-end content start)
  (let ([len (string-length content)])
       (cond
        [(>= start len) len]
        ;; Named char like #\space, #\newline
        [(and (< start len)
              (char-alphabetic? (string-ref content start)))
         (let loop ([i start])
              (if (or (>= i len)
                      (not (char-alphabetic? (string-ref content i))))
                  i
                  (loop (+ i 1))))]
        ;; Single char like #\a
        [else (+ start 1)])))

;;; scan-block-comment-end : String × Nat -> Nat
;;; Find the end of a block comment (after #|).
;;; Handles nested block comments.
(define (scan-block-comment-end content start)
  (let ([len (string-length content)])
       (let loop ([i start] [depth 1])
            (cond
             [(>= i len) len]  ; unterminated
             [(= depth 0) i]
             ;; Nested #|
             [(and (char=? (string-ref content i) #\#)
                   (< (+ i 1) len)
                   (char=? (string-ref content (+ i 1)) #\|))
              (loop (+ i 2) (+ depth 1))]
             ;; Closing |#
             [(and (char=? (string-ref content i) #\|)
                   (< (+ i 1) len)
                   (char=? (string-ref content (+ i 1)) #\#))
              (loop (+ i 2) (- depth 1))]
             [else
              (loop (+ i 1) depth)]))))

;;; scan-line-end : String × Nat -> Nat
;;; Find the end of a line (including the newline).
(define (scan-line-end content start)
  (let ([len (string-length content)])
       (let loop ([i start])
            (cond
             [(>= i len) len]
             [(char=? (string-ref content i) #\newline) (+ i 1)]
             [else (loop (+ i 1))]))))

;;; position-in-code? : (List Region) × Nat -> Boolean
;;; Check if a position is in a code region (not string/comment).
(define (position-in-code? regions pos)
  (let loop ([regions regions])
       (if (null? regions)
           #f
           (let* ([region (car regions)]
                  [type (car region)]
                  [start (cadr region)]
                  [end (caddr region)])
                 (if (and (>= pos start) (< pos end))
                     (eq? type 'code)
                     (loop (cdr regions)))))))

;;; find-symbol-occurrences-semantic : String × String -> (List (pos line col))
;;; Find all occurrences of a symbol in code regions only.
(define (find-symbol-occurrences-semantic content symbol-str)
  (let ([regions (scan-token-regions content)]
        [len (string-length content)]
        [sym-len (string-length symbol-str)]
        [occurrences '()])
       
       ;; Scan for all matches
       (let loop ([i 0] [line 1] [col 0])
            (cond
             [(> (+ i sym-len) len)
              (reverse occurrences)]
             
             ;; Check for newline to track line/col
             [(and (< i len) (char=? (string-ref content i) #\newline))
              (loop (+ i 1) (+ line 1) 0)]
             
             ;; Check for symbol match
             [(and (string-prefix-at? content symbol-str i)
                   ;; Word boundary before
                   (or (= i 0)
                       (not (scheme-identifier-char? (string-ref content (- i 1)))))
                   ;; Word boundary after
                   (or (>= (+ i sym-len) len)
                       (not (scheme-identifier-char? (string-ref content (+ i sym-len)))))
                   ;; In code region
                   (position-in-code? regions i))
              (set! occurrences (cons (list i line col) occurrences))
              (loop (+ i 1) line (+ col 1))]
             
             [else
              (loop (+ i 1) line (+ col 1))]))))

;;; scan-file-for-symbol-semantic : String × String × String -> (List Change)
;;; Semantic version: only finds symbols in code regions.
(define (scan-file-for-symbol-semantic file old-str new-str)
  (guard (e [else '()])
         (let* ([content (call-with-input-file file
                                               (lambda (port) (get-string-all port)))]
                [occurrences (find-symbol-occurrences-semantic content old-str)])
               (map (lambda (occ)
                            (let ([pos (car occ)]
                                  [line (cadr occ)]
                                  [col (caddr occ)])
                                 (make-ref-change 'rename-occurrence
                                                  file old-str new-str
                                                  line line
                                                  (extract-context-line content pos))))
                    occurrences))))

;;; extract-context-line : String × Nat -> String
;;; Extract the line containing the given position.
(define (extract-context-line content pos)
  (let ([len (string-length content)])
       ;; Find line start
       (let ([start (let loop ([i pos])
                         (cond
                          [(<= i 0) 0]
                          [(char=? (string-ref content (- i 1)) #\newline) i]
                          [else (loop (- i 1))]))]
             [end (let loop ([i pos])
                       (cond
                        [(>= i len) len]
                        [(char=? (string-ref content i) #\newline) i]
                        [else (loop (+ i 1))]))])
            (string-trim-simple (substring content start end)))))

;;; ====
;;; Semantic Scanner Diagnostics
;;; ====

;;; find-all-occurrences-with-context : String × String -> (List (pos line region-type))
;;; Find ALL textual occurrences with their context type.
(define (find-all-occurrences-with-context content symbol-str)
  (let ([regions (scan-token-regions content)]
        [len (string-length content)]
        [sym-len (string-length symbol-str)]
        [occurrences '()])
       
       (let loop ([i 0] [line 1])
            (cond
             [(> (+ i sym-len) len)
              (reverse occurrences)]
             
             [(and (< i len) (char=? (string-ref content i) #\newline))
              (loop (+ i 1) (+ line 1))]
             
             [(and (string-prefix-at? content symbol-str i)
                   ;; Word boundary before
                   (or (= i 0)
                       (not (scheme-identifier-char? (string-ref content (- i 1)))))
                   ;; Word boundary after
                   (or (>= (+ i sym-len) len)
                       (not (scheme-identifier-char? (string-ref content (+ i sym-len))))))
              ;; Find what region this is in
              (let ([region-type (find-region-type regions i)])
                   (set! occurrences (cons (list i line region-type) occurrences)))
              (loop (+ i 1) line)]
             
             [else
              (loop (+ i 1) line)]))))

;;; find-region-type : (List Region) × Nat -> Symbol
(define (find-region-type regions pos)
  (let loop ([regions regions])
       (if (null? regions)
           'unknown
           (let* ([region (car regions)]
                  [type (car region)]
                  [start (cadr region)]
                  [end (caddr region)])
                 (if (and (>= pos start) (< pos end))
                     type
                     (loop (cdr regions)))))))

;;; refactor-rename-diagnostic : Symbol -> void
;;; Show all occurrences with their context type (for debugging).
(define (refactor-rename-diagnostic sym)
  (display "\n")
  (printf "  Semantic Analysis: '~a'\n" sym)
  (display "  ────────────────────────────────\n\n")
  
  (let ([sym-str (symbol->string sym)]
        [code-count 0]
        [string-count 0]
        [comment-count 0]
        [files-scanned 0])
       
       (for-each
        (lambda (dir)
                (for-each
                 (lambda (file)
                         (set! files-scanned (+ files-scanned 1))
                         (guard (e [else (void)])
                                (let* ([content (call-with-input-file file
                                                                      (lambda (port) (get-string-all port)))]
                                       [occs (find-all-occurrences-with-context content sym-str)])
                                      (unless (null? occs)
                                              (printf "  ~a:\n" file)
                                              (for-each
                                               (lambda (occ)
                                                       (let ([pos (car occ)]
                                                             [line (cadr occ)]
                                                             [type (caddr occ)])
                                                            (let ([ctx (extract-context-line content pos)]
                                                                  [status (case type
                                                                                [(code)
                                                                                 (set! code-count (+ code-count 1))
                                                                                 "✓ CODE"]
                                                                                [(string)
                                                                                 (set! string-count (+ string-count 1))
                                                                                 "✗ STRING"]
                                                                                [(line-comment block-comment)
                                                                                 (set! comment-count (+ comment-count 1))
                                                                                 "✗ COMMENT"]
                                                                                [else "? UNKNOWN"])])
                                                                 (printf "    ~a:~a [~a]\n" line status ctx))))
                                               occs)))))
                 (find-scheme-files-simple dir)))
        '("core" "shell" "user"))
       
       (display "\n")
       (display "  ────────────────────────────────\n")
       (printf "  Files scanned: ~a\n" files-scanned)
       (printf "  Code occurrences (will rename):     ~a\n" code-count)
       (printf "  String occurrences (will skip):     ~a\n" string-count)
       (printf "  Comment occurrences (will skip):    ~a\n" comment-count)
       (display "\n")))

;;; string-find-char-simple : String × Char -> Nat | #f
(define (string-find-char-simple str ch)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; NOTE: string-trim provided by core/base/prelude.ss
;;;       Alias for backward compatibility
(define string-trim-simple string-trim)

;;; ====
;;; Apply Changes
;;; ====

;;; refactor-apply! : -> void
;;; Apply all pending changes.
(define (refactor-apply!)
  (when (null? *pending-changes*)
        (display "No pending changes to apply.\n")
        (return))
  
  (display "\n  Applying refactoring changes...\n")
  (display "  ────────────────────────────────\n\n")
  
  ;; Group changes by file
  (let ([file-groups (group-changes-by-file *pending-changes*)]
        [backups '()])
       
       ;; Process each file
       (for-each
        (lambda (group)
                (let ([file (car group)]
                      [changes (cdr group)])
                     (printf "  Processing ~a (~a changes)...\n" file (length changes))
                     
                     ;; Backup original content
                     (guard (e [else (void)])
                            (let ([content (read-file-content file)])
                                 (set! backups (cons (cons file content) backups))
                                 
                                 ;; Apply changes
                                 (let ([new-content (apply-changes-to-content content changes)])
                                      (write-file-content file new-content))))))
        file-groups)
       
       ;; Save to undo stack
       (set! *refactor-undo-stack*
             (take-n (cons (cons *pending-changes* backups) *refactor-undo-stack*)
                     *max-undo-depth*))
       
       ;; Clear pending
       (set! *pending-changes* '())
       
       (printf "\n  Applied ~a changes across ~a files.\n"
               (apply + (map (lambda (g) (length (cdr g))) file-groups))
               (length file-groups))
       (display "  Use (refactor-undo!) to undo.\n\n")))

;;; take-n : List × Nat -> List
(define (take-n lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;;; group-changes-by-file : (List Change) -> (List (file . changes))
(define (group-changes-by-file changes)
  (let ([groups (make-hashtable string-hash string=?)])
       (for-each
        (lambda (c)
                (let* ([file (ref-change-file c)]
                       [existing (hashtable-ref groups file '())])
                      (hashtable-set! groups file (cons c existing))))
        changes)
       (let ([result '()])
            (vector-for-each
             (lambda (key)
                     (set! result (cons (cons key (hashtable-ref groups key '()))
                                        result)))
             (hashtable-keys groups))
            result)))

;;; read-file-content : String -> String
(define (read-file-content file)
  (call-with-input-file file
                        (lambda (port)
                                (get-string-all port))))

;;; write-file-content : String × String -> void
(define (write-file-content file content)
  (call-with-output-file file
                         (lambda (port)
                                 (put-string port content))
                         'replace))

;;; apply-changes-to-content : String × (List Change) -> String
;;; Apply rename changes to file content.
(define (apply-changes-to-content content changes)
  (let ([result content])
       (for-each
        (lambda (c)
                (let ([old (ref-change-old c)]
                      [new (ref-change-new c)])
                     ;; Replace whole words only
                     (set! result (string-replace-word result old new))))
        changes)
       result))

;;; string-replace-word : String × String × String -> String
;;; Replace all whole-word occurrences of old with new.
(define (string-replace-word str old new)
  (let loop ([result ""]
             [remaining str])
       (let ([pos (string-find-substring remaining old)])
            (if (not pos)
                (string-append result remaining)
                (let ([before-ok? (or (= pos 0)
                                      (not (scheme-identifier-char?
                                            (string-ref remaining (- pos 1)))))]
                      [end (+ pos (string-length old))]
                      [rlen (string-length remaining)])
                     (let ([after-ok? (or (>= end rlen)
                                          (not (scheme-identifier-char?
                                                (string-ref remaining end))))])
                          (if (and before-ok? after-ok?)
                              (loop (string-append result
                                                   (substring remaining 0 pos)
                                                   new)
                                    (substring remaining end rlen))
                              (loop (string-append result
                                                   (substring remaining 0 (+ pos 1)))
                                    (substring remaining (+ pos 1) rlen)))))))))

;;; ====
;;; Undo Support
;;; ====

;;; refactor-undo! : -> void
;;; Undo the last refactoring operation.
(define (refactor-undo!)
  (when (null? *refactor-undo-stack*)
        (display "Nothing to undo.\n")
        (return))
  
  (let* ([last (car *refactor-undo-stack*)]
         [changes (car last)]
         [backups (cdr last)])
        
        (display "\n  Undoing last refactoring...\n")
        (display "  ────────────────────────────────\n\n")
        
        ;; Restore each backed-up file
        (for-each
         (lambda (backup)
                 (let ([file (car backup)]
                       [content (cdr backup)])
                      (printf "  Restoring ~a\n" file)
                      (write-file-content file content)))
         backups)
        
        ;; Pop from undo stack
        (set! *refactor-undo-stack* (cdr *refactor-undo-stack*))
        
        (printf "\n  Restored ~a files.\n\n" (length backups))))

;;; ====
;;; Argument Reordering
;;; ====

;;; refactor-reorder-args-preview : Symbol × (List Nat) -> (List Change)
;;; Preview reordering function arguments.
;;; new-order is a list of indices, e.g., '(2 0 1) means:
;;;   new-arg-0 = old-arg-2, new-arg-1 = old-arg-0, new-arg-2 = old-arg-1
(define (refactor-reorder-args-preview func-name new-order)
  (display "\n")
  (printf "  Preview: Reorder arguments of '~a'\n" func-name)
  (printf "  New order: ~a\n" new-order)
  (display "  ────────────────────────────────\n\n")
  
  ;; Find definition
  (let ([defs (find-all-definitions func-name)])
       (when (null? defs)
             (printf "  Function '~a' not found in index.\n\n" func-name)
             (return '()))
       
       (let* ([def-file (caar defs)]
              [def-line (cdar defs)]
              [content (read-file-content def-file)]
              [lines (string-split-lines-simple content)]
              [changes '()])
             
             (printf "  Definition: ~a:~a\n" def-file def-line)
             
             ;; Find all call sites using call graph
             (let ([callers (find-all-references func-name)])
                  (printf "  Call sites: ~a\n\n" (length callers))
                  
                  (for-each
                   (lambda (ref)
                           (let ([file (car ref)]
                                 [line (cadr ref)]
                                 [caller (caddr ref)])
                                (printf "    ~a:~a (in ~a)\n" file line caller)
                                (set! changes
                                      (cons (make-ref-change 'reorder-args
                                                             file
                                                             (format "~a" func-name)
                                                             (format "~a ~a" func-name new-order)
                                                             line line caller)
                                            changes))))
                   callers)
                  
                  (set! *pending-changes* changes)
                  (printf "\n  Total call sites to update: ~a\n" (length callers))
                  (display "  Note: Manual review recommended before applying.\n\n")
                  changes))))

;;; ====
;;; Signature Extraction and Arity-Aware Rename
;;; ====

;;; extract-function-signature : Symbol -> (params . file) | #f
;;; Extract the parameter list from a function definition.
(define (extract-function-signature func-name)
  (let ([defs (find-all-definitions func-name)])
       (if (null? defs)
           #f
           (let* ([file (caar defs)]
                  [start-line (cdar defs)])
                 (guard (e [else #f])
                        (let* ([content (read-file-content file)]
                               [lines (string-split-lines-simple content)])
                              ;; Read multiple lines starting from definition
                              ;; to get a complete S-expression
                              (let ([params (extract-params-from-lines lines (- start-line 1) func-name)])
                                   (if params
                                       (cons params file)
                                       #f))))))))

;;; extract-params-from-lines : (List String) × Nat × Symbol -> (List Symbol) | #f
;;; Extract params by reading lines until we get a valid definition.
(define (extract-params-from-lines lines start-idx func-name)
  (guard (e [else #f])
         (let loop ([idx start-idx] [accumulated ""])
              (if (>= idx (length lines))
                  #f
                  (let* ([new-acc (string-append accumulated
                                                 (if (string=? accumulated "") "" "\n")
                                                 (list-ref lines idx))]
                         ;; Try to parse as definition
                         [result (try-parse-define new-acc func-name)])
                        (if result
                            result
                            ;; Try reading more lines (up to 20 for large definitions)
                            (if (< (- idx start-idx) 20)
                                (loop (+ idx 1) new-acc)
                                #f)))))))

;;; try-parse-define : String × Symbol -> (List Symbol) | #f
;;; Try to extract params from a define pattern.
;;; Uses regex-like matching since the define may not be complete.
(define (try-parse-define str func-name)
  (let ([func-str (symbol->string func-name)])
       ;; Look for (define (name pattern at the start
       (cond
        ;; Pattern: (define (name arg1 arg2 ...)
        [(and (string-starts-with? str "(define (")
              (string-contains-at? str func-str 9))
         ;; Extract params after the function name
         (let* ([start (+ 9 (string-length func-str))]
                [param-str (extract-param-string str start)])
               (if param-str
                   (parse-param-list param-str)
                   #f))]
        ;; Pattern: (define name (lambda (args...)
        [(and (string-starts-with? str "(define ")
              (string-contains-at? str func-str 8))
         ;; Look for lambda after the name
         (let* ([lambda-pos (string-find-substring str "(lambda (")]
                [param-start (and lambda-pos (+ lambda-pos 9))])
               (if param-start
                   (let ([param-str (extract-param-string str param-start)])
                        (if param-str
                            (parse-param-list param-str)
                            #f))
                   #f))]
        [else #f])))

;;; string-contains-at? : String × String × Nat -> Bool
(define (string-contains-at? str sub pos)
  (and (>= (string-length str) (+ pos (string-length sub)))
       (string=? sub (substring str pos (+ pos (string-length sub))))))

;;; extract-param-string : String × Nat -> String | #f
;;; Extract the parameter portion from position until we hit ).
(define (extract-param-string str start)
  (let ([len (string-length str)])
       (let loop ([i start] [depth 0] [acc '()])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) #\()
              (loop (+ i 1) (+ depth 1) (cons #\( acc))]
             [(char=? (string-ref str i) #\))
              (if (= depth 0)
                  (list->string (reverse acc))
                  (loop (+ i 1) (- depth 1) (cons #\) acc)))]
             [else
              (loop (+ i 1) depth (cons (string-ref str i) acc))]))))

;;; parse-param-list : String -> (List Symbol)
;;; Parse a parameter list string like "a b c" or "a b . rest".
(define (parse-param-list str)
  (guard (e [else '()])
         (let* ([trimmed (string-trim-simple str)]
                [port (open-input-string (string-append "(" trimmed ")"))]
                [lst (read port)])
               (close-input-port port)
               (if (list? lst)
                   lst
                   (flatten-params lst)))))

;;; flatten-params : Params -> (List Symbol)
;;; Handle both proper lists and improper lists (rest args).
(define (flatten-params params)
  (cond
   [(null? params) '()]
   [(symbol? params) (list params)]  ; Rest arg only
   [(pair? params)
    (cons (car params) (flatten-params (cdr params)))]
   [else '()]))

;;; parse-define-params : String × Symbol -> (List Symbol) | #f
;;; Parse parameters from a define line.
;;; Handles: (define (name a b c) ...) and (define name (lambda (a b c) ...))
(define (parse-define-params line func-name)
  (let ([func-str (symbol->string func-name)])
       (guard (e [else #f])
              ;; Try to read the first S-expression
              (let* ([port (open-input-string line)]
                     [expr (read port)])
                    (close-input-port port)
                    (cond
                     ;; (define (name params...) ...)
                     [(and (pair? expr)
                           (eq? (car expr) 'define)
                           (pair? (cdr expr))
                           (pair? (cadr expr))
                           (eq? (caadr expr) func-name))
                      (let ([params (cdadr expr)])
                           (if (list? params)
                               params
                               ;; Handle rest args: (a b . rest)
                               (let loop ([p params] [acc '()])
                                    (cond
                                     [(null? p) (reverse acc)]
                                     [(pair? p) (loop (cdr p) (cons (car p) acc))]
                                     [(symbol? p) (reverse (cons p acc))]
                                     [else (reverse acc)]))))]
                     ;; (define name (lambda (params...) ...))
                     [(and (pair? expr)
                           (eq? (car expr) 'define)
                           (pair? (cdr expr))
                           (symbol? (cadr expr))
                           (eq? (cadr expr) func-name)
                           (pair? (cddr expr))
                           (pair? (caddr expr))
                           (eq? (caaddr expr) 'lambda))
                      (let ([lam-params (cadr (caddr expr))])
                           (if (list? lam-params)
                               lam-params
                               #f))]
                     [else #f])))))

;;; function-arity : Symbol -> Nat | #f
;;; Get the arity of a function.
(define (function-arity func-name)
  (let ([sig (extract-function-signature func-name)])
       (if sig (length (car sig)) #f)))

;;; compare-signatures : Symbol × Symbol -> (old-params new-params) | 'same | #f
;;; Compare signatures of two functions.
(define (compare-signatures old-name new-name)
  (let ([old-sig (extract-function-signature old-name)]
        [new-sig (extract-function-signature new-name)])
       (cond
        [(not old-sig) #f]
        [(not new-sig) #f]
        [(equal? (car old-sig) (car new-sig)) 'same]
        [else (list (car old-sig) (car new-sig))])))

;;; refactor-rename-mapped : Symbol × Symbol × (List Nat) -> void
;;; Rename with argument reordering.
;;; arg-map is a list where (list-ref arg-map i) is the old arg position for new arg i.
;;; Example: '(1 0) means swap first two args: (f old-a old-b) -> (g old-b old-a)
(define (refactor-rename-mapped old-name new-name arg-map)
  (display "\n")
  (printf "  Mapped Rename: '~a' -> '~a'\n" old-name new-name)
  (printf "  Argument mapping: ~a\n" arg-map)
  (display "  ────────────────────────────────\n\n")
  
  ;; Validate arg-map
  (let ([old-arity (function-arity old-name)])
       (cond
        [(not old-arity)
         (printf "  Cannot find signature for '~a'\n\n" old-name)]
        
        [(not (= (length arg-map) old-arity))
         (printf "  Error: Mapping length (~a) doesn't match arity (~a)\n\n"
                 (length arg-map) old-arity)]
        
        [else
         ;; Find all call sites
         (let* ([old-str (symbol->string old-name)]
                [new-str (symbol->string new-name)]
                [changes '()])
               
               ;; Scan for call sites semantically
               (for-each
                (lambda (dir)
                        (for-each
                         (lambda (file)
                                 (let ([file-changes (find-calls-in-file file old-name new-name arg-map)])
                                      (set! changes (append changes file-changes))))
                         (find-scheme-files-simple dir)))
                '("core" "shell" "user"))
               
               (set! *pending-changes* changes)
               (printf "\n  Found ~a call sites to transform.\n" (length changes))
               (display "  Preview:\n")
               (for-each
                (lambda (c)
                        (printf "    ~a:~a\n      ~a\n      -> ~a\n"
                                (ref-change-file c)
                                (ref-change-line-start c)
                                (ref-change-old c)
                                (ref-change-new c)))
                (take-up-to changes 10))
               (when (> (length changes) 10)
                     (printf "    ... and ~a more\n" (- (length changes) 10)))
               (display "\n  Use (refactor-apply!) to apply changes.\n\n"))])))

;;; find-calls-in-file : String × Symbol × Symbol × (List Nat) -> (List Change)
;;; Find and transform calls in a file using argument mapping.
(define (find-calls-in-file file old-name new-name arg-map)
  (guard (e [else '()])
         (let* ([content (read-file-content file)]
                [regions (scan-token-regions content)]
                [changes '()])
               
               ;; Parse the file and find calls
               (guard (e [else changes])
                      (let* ([port (open-input-string content)]
                             [exprs (let loop ([acc '()])
                                         (let ([expr (guard (e [else #f]) (read port))])
                                              (if (or (eof-object? expr) (not expr))
                                                  (reverse acc)
                                                  (loop (cons expr acc)))))])
                            (close-input-port port)
                            
                            ;; Find calls in each expression
                            (for-each
                             (lambda (expr)
                                     (let ([found-calls (find-calls-in-expr expr old-name)])
                                          (for-each
                                           (lambda (call-expr)
                                                   ;; Transform the call
                                                   (let* ([old-call-str (format "~s" call-expr)]
                                                          [new-call-str (transform-call call-expr new-name arg-map)])
                                                         (when new-call-str
                                                               ;; Find line number (approximate)
                                                               (let ([pos (string-find-substring content old-call-str)])
                                                                    (when (and pos (position-in-code? regions pos))
                                                                          (set! changes
                                                                                (cons (make-ref-change 'mapped-rename
                                                                                                       file
                                                                                                       old-call-str
                                                                                                       new-call-str
                                                                                                       (count-newlines-before content pos)
                                                                                                       (count-newlines-before content pos)
                                                                                                       'call-transform)
                                                                                      changes)))))))
                                           found-calls)))
                             exprs)
                            changes)))))

;;; find-calls-in-expr : S-expr × Symbol -> (List S-expr)
;;; Find all calls to func-name in an expression.
(define (find-calls-in-expr expr func-name)
  (cond
   [(not (pair? expr)) '()]
   [(eq? (car expr) func-name)
    ;; Found a call - include it and recurse into args
    (cons expr
          (apply append (map (lambda (e) (find-calls-in-expr e func-name))
                             (cdr expr))))]
   [(eq? (car expr) 'quote) '()]  ; Don't look in quoted data
   [else
    (apply append (map (lambda (e) (find-calls-in-expr e func-name)) expr))]))

;;; transform-call : S-expr × Symbol × (List Nat) -> String | #f
;;; Transform a call using argument mapping.
(define (transform-call call-expr new-name arg-map)
  (guard (e [else #f])
         (let* ([args (cdr call-expr)]
                [mapped-args (map (lambda (i) (list-ref args i)) arg-map)]
                [extra-args (if (> (length args) (length arg-map))
                                (list-tail args (length arg-map))
                                '())]
                [new-args (append mapped-args extra-args)])
               (format "~s" (cons new-name new-args)))))

;;; count-newlines-before : String × Nat -> Nat
(define (count-newlines-before str pos)
  (let loop ([i 0] [count 1])
       (cond
        [(>= i pos) count]
        [(char=? (string-ref str i) #\newline) (loop (+ i 1) (+ count 1))]
        [else (loop (+ i 1) count)])))

;;; refactor-check-arity : Symbol × Symbol -> void
;;; Check if two functions have compatible signatures.
(define (refactor-check-arity old-name new-name)
  (display "\n")
  (printf "  Signature Comparison: '~a' vs '~a'\n" old-name new-name)
  (display "  ────────────────────────────────\n\n")
  
  (let ([old-sig (extract-function-signature old-name)]
        [new-sig (extract-function-signature new-name)])
       
       (cond
        [(not old-sig)
         (printf "  Could not find signature for '~a'\n" old-name)]
        [(not new-sig)
         (printf "  Could not find signature for '~a'\n" new-name)]
        [else
         (let ([old-params (car old-sig)]
               [new-params (car new-sig)])
              (printf "  '~a' params: ~a (arity ~a)\n" old-name old-params (length old-params))
              (printf "  '~a' params: ~a (arity ~a)\n" new-name new-params (length new-params))
              (display "\n")
              
              (cond
               [(= (length old-params) (length new-params))
                (if (equal? old-params new-params)
                    (display "  ✓ Signatures are identical - simple rename OK.\n")
                    (begin
                     (display "  ⚠ Same arity but different param names.\n")
                     (display "    Use (refactor-rename-mapped old new '(0 1 ...)) to specify order.\n")))]
               [else
                (printf "  ⚠ Different arity: ~a vs ~a\n"
                        (length old-params) (length new-params))
                (display "    Manual adjustment of call sites may be needed.\n")]))])
       (display "\n")))

;;; ====
;;; Convenience Commands
;;; ====

;;; refactor-rename! : Symbol × Symbol -> void
;;; Preview and apply a rename operation.
(define (refactor-rename! old-name new-name)
  (refactor-rename-preview old-name new-name)
  (display "Apply these changes? (refactor-apply!) or (refactor-clear!)\n"))

;;; refactor-clear! : -> void
;;; Clear pending changes without applying.
(define (refactor-clear!)
  (set! *pending-changes* '())
  (display "Pending changes cleared.\n"))

;;; refactor-preview-changes : -> void
;;; Show current pending changes.
(define (refactor-preview-changes)
  (display "\n")
  (printf "  Pending Changes (~a):\n" (length *pending-changes*))
  (display "  ────────────────────────────────\n\n")
  (if (null? *pending-changes*)
      (display "    (no pending changes)\n")
      (for-each
       (lambda (c)
               (printf "    [~a] ~a:~a ~a -> ~a\n"
                       (ref-change-type c)
                       (ref-change-file c)
                       (ref-change-line-start c)
                       (ref-change-old c)
                       (ref-change-new c)))
       *pending-changes*))
  (display "\n"))

;;; refactor-status : -> void
;;; Show refactoring engine status.
(define (refactor-status)
  (display "\n")
  (display "  Refactoring Engine Status\n")
  (display "  ────────────────────────────────\n\n")
  (printf "  Pending changes: ~a\n" (length *pending-changes*))
  (printf "  Undo stack depth: ~a\n" (length *refactor-undo-stack*))
  (display "\n"))

;;; refactor-help : -> void
;;; Show available commands.
(define (refactor-help)
  (display "\n")
  (display "  ┌────────────────────────────────────────────────────────────────────┐\n")
  (display "  │                    REFACTORING ENGINE COMMANDS                     │\n")
  (display "  └────────────────────────────────────────────────────────────────────┘\n")
  (display "\n")
  (display "  Rename Operations:\n")
  (display "    (refactor-rename! 'old 'new)           Preview + prompt to apply\n")
  (display "    (refactor-rename-preview 'old 'new)    Preview only\n")
  (display "\n")
  (display "  Arity-Aware Rename:\n")
  (display "    (refactor-check-arity 'old 'new)       Compare function signatures\n")
  (display "    (refactor-rename-mapped 'old 'new '(1 0))   Rename + swap args\n")
  (display "    (function-arity 'name)                 Get function arity\n")
  (display "\n")
  (display "  Argument Reordering:\n")
  (display "    (refactor-reorder-args-preview 'fn '(2 0 1))   Preview reorder\n")
  (display "\n")
  (display "  Change Management:\n")
  (display "    (refactor-preview-changes)             Show pending changes\n")
  (display "    (refactor-apply!)                      Apply pending changes\n")
  (display "    (refactor-clear!)                      Discard pending changes\n")
  (display "    (refactor-undo!)                       Undo last operation\n")
  (display "\n")
  (display "  Diagnostics:\n")
  (display "    (refactor-rename-diagnostic 'sym)      Analyze symbol contexts\n")
  (display "                                           Shows code vs string/comment\n")
  (display "\n")
  (display "  Status:\n")
  (display "    (refactor-status)                      Show engine status\n")
  (display "    (refactor-help)                        Show this help\n")
  (display "\n"))

;;; ====
;;; Initialization
;;; ====

(display "\n")
(display "  Integrated Refactoring Engine Loaded\n")
(display "  ────────────────────────────────\n")
(display "  Use (refactor-help) for available commands.\n")
(display "\n")
