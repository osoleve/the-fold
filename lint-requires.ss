;;; lint-requires.ss — Find unused (require ...) statements across the codebase.
;;;
;;; Usage: scheme --script lint-requires.ss [--verbose] [--fix] [--check] [--files f1.ss f2.ss ...]
;;;
;;; Walks all .ss files in lattice/ and core/, analyzes each for unused requires
;;; using lattice/meta/unused-requires.ss, and reports findings.
;;;
;;; The export lookup is built from *module-paths* — for each registered module,
;;; we parse the file and extract top-level define names. A require is "unused"
;;; if none of a module's defines appear as referenced symbols in the file.

;; Guard against meta-printf not being bound in standalone mode
(when (not (top-level-bound? 'meta-printf))
  (eval '(define meta-printf printf)))

(load "core/lang/module.ss")
(require 'prelude)
(require 'meta/unused-requires)

;;; ====
;;; Configuration
;;; ====

(define *verbose* #f)
(define *fix-mode* #f)
(define *check-mode* #f)
(define *explicit-files* #f)

(let loop ([args (command-line-arguments)])
  (cond
    [(null? args) (void)]
    [(string=? (car args) "--verbose")
     (set! *verbose* #t)
     (loop (cdr args))]
    [(string=? (car args) "--fix")
     (set! *fix-mode* #t)
     (loop (cdr args))]
    [(string=? (car args) "--check")
     (set! *check-mode* #t)
     (loop (cdr args))]
    [(string=? (car args) "--files")
     ;; All remaining args are file paths
     (set! *explicit-files* (cdr args))]
    [else (loop (cdr args))]))

;;; ====
;;; Export Lookup Builder
;;; ====

;;; extract-defines : (List SExp) -> (List Symbol)
;;; Extract all top-level define names from parsed S-expressions.
;;; Handles: (define name ...), (define (name args...) ...), (define-record-type name ...)
(define (extract-defines sexps)
  (filter-map
    (lambda (sexp)
      (and (pair? sexp)
           (case (car sexp)
             [(define)
              (and (pair? (cdr sexp))
                   (cond
                     [(symbol? (cadr sexp)) (cadr sexp)]
                     [(and (pair? (cadr sexp)) (symbol? (caadr sexp)))
                      (caadr sexp)]
                     [else #f]))]
             [(define-record-type)
              (and (pair? (cdr sexp))
                   (symbol? (cadr sexp))
                   (cadr sexp))]
             [(define-syntax)
              (and (pair? (cdr sexp))
                   (symbol? (cadr sexp))
                   (cadr sexp))]
             [else #f])))
    sexps))

;;; read-file-sexps : String -> (List SExp)
;;; Read all S-expressions from a file. Returns '() on read errors.
(define (read-file-sexps path)
  (guard (e [else '()])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([datum (read port)])
            (if (eof-object? datum)
                (reverse acc)
                (loop (cons datum acc)))))))))

;;; build-export-lookup : -> (Symbol -> (List Symbol))
;;; Build the export lookup function from *module-paths*.
;;; For each registered module, parse its file and extract defines.
(define (build-export-lookup)
  (let ([cache (make-eq-hashtable)])
    ;; Pre-populate cache from *module-paths*
    (let ([keys (hashtable-keys *module-paths*)])
      (vector-for-each
        (lambda (mod-name)
          (let* ([path (hashtable-ref *module-paths* mod-name #f)]
                 [sexps (if path (read-file-sexps path) '())]
                 [defines (extract-defines sexps)])
            (hashtable-set! cache mod-name defines)))
        keys))
    ;; Return lookup function
    (lambda (mod-name)
      (hashtable-ref cache mod-name '()))))

;;; ====
;;; File Discovery
;;; ====

;;; find-scheme-files : String -> (List String)
;;; Walk directory tree for .ss files, excluding test files and README.sexp.
(define (find-scheme-files dir)
  (let ([result '()])
    (define (walk path)
      (guard (e [else (void)])
        (let ([entries (directory-list path)])
          (for-each
            (lambda (entry)
              (let ([full (string-append path "/" entry)])
                (cond
                  [(and (> (string-length entry) 0)
                        (char=? (string-ref entry 0) #\.))
                   ;; Skip hidden files/dirs
                   (void)]
                  [(file-directory? full)
                   (walk full)]
                  [(and (> (string-length entry) 3)
                        (string=? (substring entry (- (string-length entry) 3) (string-length entry)) ".ss")
                        ;; Skip test files — they have legitimate requires for test subjects
                        (not (and (> (string-length entry) 5)
                                  (string=? (substring entry 0 5) "test-"))))
                   (set! result (cons full result))])))
            entries))))
    (walk dir)
    (list-sort string<? result)))

;;; ====
;;; Analysis
;;; ====

(define (run-lint)
  (printf "Building export lookup from module registry...~n")
  (let* ([export-lookup (build-export-lookup)]
         [keys (hashtable-keys *module-paths*)]
         [n-modules (vector-length keys)])
    (printf "  ~a modules indexed~n" n-modules)

    ;; Use explicit file list or walk lattice/ and core/
    (let ([files (if *explicit-files*
                     *explicit-files*
                     (append (find-scheme-files "lattice")
                             (find-scheme-files "core")))])
      (printf "Scanning ~a source files...~n~n" (length files))

      (let ([total-unused 0]
            [files-with-unused 0]
            [all-findings '()])
        ;; Analyze each file
        (for-each
          (lambda (path)
            (let* ([sexps (read-file-sexps path)]
                   [analysis (analyze-file-requires sexps export-lookup)]
                   [unused (analysis-unused analysis)]
                   [uncertain (analysis-uncertain analysis)])
              (when (pair? unused)
                (set! files-with-unused (+ files-with-unused 1))
                (set! total-unused (+ total-unused (length unused)))
                (set! all-findings (cons (cons path unused) all-findings))
                (printf "~a~n" path)
                (for-each
                  (lambda (mod)
                    (printf "  unused: ~a~n" mod))
                  unused))
              (when (and *verbose* (pair? uncertain))
                (printf "~a~n" path)
                (for-each
                  (lambda (mod)
                    (printf "  uncertain: ~a~n" mod))
                  uncertain))))
          files)

        ;; Summary
        (printf "~n--- Summary ---~n")
        (printf "Files scanned:     ~a~n" (length files))
        (printf "Files with unused: ~a~n" files-with-unused)
        (printf "Total unused:      ~a~n" total-unused)

        ;; Fix mode: remove unused requires
        (when (and *fix-mode* (pair? all-findings))
          (printf "~n--- Fixing ---~n")
          (for-each
            (lambda (finding)
              (let ([path (car finding)]
                    [unused-mods (cdr finding)])
                (if (member path *skip-files*)
                    (printf "  skipped: ~a (aggregator module)~n" path)
                    (fix-file! path unused-mods))))
            all-findings))

        ;; Check mode: exit 1 if any unused requires found
        (when (and *check-mode* (> total-unused 0))
          (exit 1))))))

;;; ====
;;; Skip List
;;; ====

;;; Files where "unused" requires are intentional (aggregator/loader modules).
(define *skip-files*
  '("lattice/tiles/boardcraft.ss"))

;;; ====
;;; Auto-fix
;;; ====

;;; fix-file! : String (List Symbol) -> Void
;;; Remove unused require arguments from a file. Handles both single-arg
;;; (require 'mod) and multi-arg (require 'a 'b 'c) forms.
;;; If ALL args in a multi-arg require are unused, removes the entire line(s).
;;; If only some are unused, rewrites the require to keep only the used ones.
(define (fix-file! path unused-mods)
  (guard (e [else
    (printf "  ERROR fixing ~a: ~a~n" path (condition-message e))])
    (let* ([content (call-with-input-file path
                      (lambda (port)
                        (get-string-all port)))]
           [new-content (remove-unused-requires content unused-mods)]
           [removed (count-removed content new-content unused-mods)])
      ;; Only write if something changed
      (when (not (string=? content new-content))
        (call-with-output-file path
          (lambda (port)
            (put-string port new-content))
          'replace)
        (printf "  fixed: ~a (removed ~a require~a)~n"
                path
                (length unused-mods)
                (if (= (length unused-mods) 1) "" "s"))))))

;;; remove-unused-requires : String (List Symbol) -> String
;;; Process file content line by line. Handles:
;;;   (require 'unused)              -> remove entire line
;;;   (require 'used 'unused)        -> (require 'used)
;;;   (require 'unused1 'unused2)    -> remove entire line
;;;   Multi-line requires            -> collapse and rewrite
(define (remove-unused-requires content unused-mods)
  (let* ([lines (string-split content #\newline)]
         [processed (process-lines lines unused-mods)])
    (string-join processed "\n")))

;;; process-lines : (List String) (List Symbol) -> (List String)
;;; Walk lines, detecting require forms and rewriting/removing as needed.
(define (process-lines lines unused-mods)
  (if (null? lines) '()
      (let ([line (car lines)]
            [rest (cdr lines)])
        (cond
          ;; Check if this line has a require with unused module
          [(line-has-unused-require? line unused-mods)
           (let ([rewritten (rewrite-require-line line unused-mods)])
             (if rewritten
                 ;; Some args remain — keep rewritten line
                 (cons rewritten (process-lines rest unused-mods))
                 ;; All args removed — skip line (and trailing blank if any)
                 (process-lines rest unused-mods)))]
          [;; Also handle @requires annotations in comments
           (line-has-unused-at-requires? line unused-mods)
           (let ([rewritten (rewrite-at-requires line unused-mods)])
             (cons rewritten (process-lines rest unused-mods)))]
          [else
           (cons line (process-lines rest unused-mods))]))))

;;; line-has-unused-require? : String (List Symbol) -> Bool
(define (line-has-unused-require? line unused-mods)
  (and (string-contains line "(require ")
       (exists (lambda (mod)
                 (string-contains line (format "'~a" mod)))
               unused-mods)))

;;; rewrite-require-line : String (List Symbol) -> (Maybe String)
;;; Returns #f if entire require should be removed, or rewritten string.
(define (rewrite-require-line line unused-mods)
  (let* ([trimmed (string-trim line)]
         ;; Extract the indentation
         [indent (let loop ([i 0])
                   (if (and (< i (string-length line))
                            (char-whitespace? (string-ref line i)))
                       (loop (+ i 1))
                       (substring line 0 i)))]
         ;; Parse require args from the line
         [all-mods (extract-require-mods-from-line trimmed)]
         [kept (filter (lambda (m) (not (memq m unused-mods))) all-mods)])
    (cond
      [(null? kept) #f]  ;; All removed
      [(= (length kept) 1)
       (string-append indent (format "(require '~a)" (car kept)))]
      [else
       (string-append indent "(require "
                      (string-join (map (lambda (m) (format "'~a" m)) kept) " ")
                      ")")])))

;;; extract-require-mods-from-line : String -> (List Symbol)
;;; Parse module names from a require expression in text form.
(define (extract-require-mods-from-line line)
  (let ([len (string-length line)])
    (let loop ([i 0] [mods '()])
      (cond
        [(>= i len) (reverse mods)]
        [(and (< (+ i 1) len)
              (char=? (string-ref line i) #\')
              (not (char=? (string-ref line (+ i 1)) #\))))
         ;; Found a quoted symbol — extract until space, ) or end
         (let sym-loop ([j (+ i 1)] [chars '()])
           (cond
             [(>= j len)
              (loop j (cons (string->symbol (list->string (reverse chars))) mods))]
             [(or (char=? (string-ref line j) #\))
                  (char=? (string-ref line j) #\space)
                  (char=? (string-ref line j) #\newline))
              (loop j (cons (string->symbol (list->string (reverse chars))) mods))]
             [else
              (sym-loop (+ j 1) (cons (string-ref line j) chars))]))]
        [else (loop (+ i 1) mods)]))))

;;; line-has-unused-at-requires? : String (List Symbol) -> Bool
;;; Check if ;;; @requires line mentions unused modules.
(define (line-has-unused-at-requires? line unused-mods)
  (and (string-contains line "@requires")
       (exists (lambda (mod)
                 (string-contains line (symbol->string mod)))
               unused-mods)))

;;; rewrite-at-requires : String (List Symbol) -> String
;;; Remove unused module names from ;;; @requires annotation.
(define (rewrite-at-requires line unused-mods)
  (let* ([parts (string-split line #\space)]
         [filtered (filter (lambda (p)
                             (not (exists (lambda (mod)
                                            (string=? p (symbol->string mod)))
                                          unused-mods)))
                           parts)])
    (string-join filtered " ")))

(define (count-removed old new unused-mods)
  (length unused-mods))

;;; ====
;;; Helpers
;;; ====

(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (let loop ([i 0])
      (cond
        [(> (+ i nlen) hlen) #f]
        [(string=? (substring haystack i (+ i nlen)) needle) #t]
        [else (loop (+ i 1))]))))

(define (string-split str ch)
  (let ([len (string-length str)])
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i len)
         (reverse (cons (substring str start len) acc))]
        [(char=? (string-ref str i) ch)
         (loop (+ i 1) (+ i 1) (cons (substring str start i) acc))]
        [else (loop (+ i 1) start acc)]))))

(define (string-join strs sep)
  (if (null? strs) ""
      (let loop ([rest (cdr strs)] [acc (car strs)])
        (if (null? rest) acc
            (loop (cdr rest) (string-append acc sep (car rest)))))))

;;; ====
;;; Entry
;;; ====

(run-lint)
