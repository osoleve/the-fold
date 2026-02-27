(load "core/base/prelude.ss")

(doc 'module 'autodoc)
(doc 'description "Auto-Documentation Extractor. Generates REPL-friendly docs from code including signatures, examples, and derived summaries. Stores as structured S-expression data.")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ==== 
;;; Data Structures
;;; ==== 

;;; Doc entry structure:
;;;   ((name        . Symbol)
;;;    (kind        . define|define-syntax|macro)
;;;    (file        . String)
;;;    (line        . Number)
;;;    (signature   . String | #f)
;;;    (docstring   . String | #f)
;;;    (category    . Symbol)
;;;    (visibility  . public|private)
;;;    (examples    . (List Example))
;;;    (see-also    . (List Symbol))
;;;    (usage       . String | #f)      ; for macros
;;;    (module      . Symbol | #f))

;;; Example structure:
;;;   ((source   . String)
;;;    (expected . String)
;;;    (label    . String | #f)
;;;    (from     . String))

;;; ==== 
;;; Global State
;;; ==== 

;;; *autodoc-registry* : Hashtable Symbol → Doc-Entry
(define *autodoc-registry* (make-eq-hashtable))

;;; *orphan-examples* : List of examples that couldn't be matched
(define *orphan-examples* '())

;;; *autodoc-categories* : Hashtable Symbol → (List Symbol)
(define *autodoc-categories* (make-eq-hashtable))

;;; *autodoc-initialized* : Boolean
(define *autodoc-initialized* #f)

;;; ==== 
;;; Path Utilities
;;; ==== 

;;; derive-category : String → Symbol
;;; Derive category from file path.
(define (derive-category path)
  (cond
   ;; Core (language kernel)
   [(string-contains? path "core/types") 'types]
   [(string-contains? path "core/blocks") 'blocks]
   [(string-contains? path "core/lang") 'lang]
   [(string-contains? path "core/base") 'base]
   [(string-contains? path "core/util") 'util]
   ;; Lattice (skill tree)
   [(string-contains? path "lattice/linalg") 'linalg]
   [(string-contains? path "lattice/data") 'data]
   [(string-contains? path "lattice/algebra") 'algebra]
   [(string-contains? path "lattice/random") 'random]
   [(string-contains? path "lattice/numeric") 'numeric]
   [(string-contains? path "lattice/geometry") 'geometry]
   [(string-contains? path "lattice/autodiff") 'autodiff]
   [(string-contains? path "lattice/query") 'query]
   [(string-contains? path "lattice/dsl") 'dsl]
   [(string-contains? path "lattice/info") 'info]
   [(string-contains? path "lattice/physics") 'physics]
   [(string-contains? path "lattice/sim") 'sim]
   [(string-contains? path "lattice/automata") 'automata]
   [(string-contains? path "lattice/number-theory") 'number-theory]
   [(string-contains? path "lattice/pipeline") 'pipeline]
   [(string-contains? path "lattice/tiles") 'tiles]
   ;; FP subdirectories
   [(string-contains? path "lattice/fp/control") 'fp-control]
   [(string-contains? path "lattice/fp/numeric") 'fp-numeric]
   [(string-contains? path "lattice/fp/parsing") 'fp-parsing]
   [(string-contains? path "lattice/fp/meta") 'fp-meta]
   [(string-contains? path "lattice/fp/data") 'fp-data]
   [(string-contains? path "lattice/fp/game") 'fp-game]
   [(string-contains? path "lattice/symbolic") 'symbolic]
   [(string-contains? path "lattice/fp/measure") 'fp-measure]
   [(string-contains? path "lattice/fp/control-systems") 'fp-control-systems]
   [(string-contains? path "lattice/rewrite") 'rewrite]
   [(string-contains? path "lattice/fp/analysis") 'fp-analysis]
   [(string-contains? path "lattice/fp") 'fp]
   ;; Boundary and user
   [(string-contains? path "boundary") 'boundary]
   [(string-contains? path "user") 'user]
   [else 'other]))

;;; derive-visibility : Symbol → Symbol
;;; Determine visibility from symbol name.
(define (derive-visibility name)
  (let ([s (symbol->string name)])
       (if (and (> (string-length s) 0)
                (or (char=? (string-ref s 0) #\_)
                    (char=? (string-ref s 0) #\%)))
           'private
           'public)))

;;; ==== 
;;; File Reading Utilities
;;; ==== 

;;; read-file-lines : String → (List String)
(define (autodoc-read-file-lines path)
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

;;; read-file-sexps : String → (List Sexp) | #f
;;; Read all s-expressions from file using (read).
(define (read-file-sexps path)
  (guard (e [else #f])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([exprs '()])
                                            (let ([expr (read port)])
                                                 (if (eof-object? expr)
                                                     (reverse exprs)
                                                     (loop (cons expr exprs)))))))))

;;; ==== 
;;; Docstring Parsing
;;; ==== 

;;; parse-docstring-block : (List String) Int → (values String String (List Symbol))
;;; Parse docstring comments above a definition.
;;; Returns: (values summary description see-also)
(define (parse-docstring-block lines start-line)
  ;; Look backwards from start-line for ;;; comments
  (let loop ([i (- start-line 2)]  ; 0-indexed, line before definition
             [doc-lines '()])
       (if (or (< i 0) (null? (list-ref-safe lines i)))
           (parse-doc-sections doc-lines)
           (let ([line (list-ref lines i)])
                (if (string-starts-with? (string-trim line) ";;;")
                    (loop (- i 1) (cons line doc-lines))
                    (parse-doc-sections doc-lines))))))

;;; list-ref-safe : (List a) Int → a | #f
(define (list-ref-safe lst i)
  (if (or (< i 0) (>= i (length lst)))
      #f
      (list-ref lst i)))

;;; parse-doc-sections : (List String) → (values String String (List Symbol))
;;; Parse structured docstring into summary, description, see-also.
(define (parse-doc-sections lines)
  (if (null? lines)
      (values #f #f '())
      (let* ([cleaned (map strip-comment-prefix lines)]
             [joined (string-join cleaned "\n")]
             [parts (string-split-paragraphs joined)]
             [summary (if (null? parts) #f (car parts))]
             [rest (if (null? parts) '() (cdr parts))]
             [see-also-line (find-see-also-line cleaned)]
             [see-also (if see-also-line
                           (parse-see-also see-also-line)
                           '())]
             [description (string-join
                           (filter (lambda (p) 
                                           (not (string-starts-with? p "See also:")))
                                   rest)
                           "\n\n")])
            (values summary
                    (if (string=? description "") #f description)
                    see-also))))

;;; strip-comment-prefix : String → String
(define (strip-comment-prefix line)
  (let ([trimmed (string-trim line)])
       (if (string-starts-with? trimmed ";;; ")
           (substring trimmed 4 (string-length trimmed))
           (if (string-starts-with? trimmed ";;;")
               (substring trimmed 3 (string-length trimmed))
               trimmed))))

;;; string-split-paragraphs : String → (List String)
;;; Split string on double-newline (paragraph breaks).
(define (string-split-paragraphs s)
  (let ([lines (string-split s #\newline)])
       ;; Group lines into paragraphs (separated by blank lines)
       (let loop ([lines lines] [current '()] [result '()])
            (cond
             [(null? lines)
              (let ([final (if (null? current)
                               result
                               (cons (string-join (reverse current) "\n") result))])
                   (filter (lambda (p) (not (string=? (string-trim p) "")))
                           (reverse final)))]
             [(string=? (string-trim (car lines)) "")
              ;; Blank line - end current paragraph
              (if (null? current)
                  (loop (cdr lines) '() result)
                  (loop (cdr lines) '()
                        (cons (string-join (reverse current) "\n") result)))]
             [else
              ;; Continue current paragraph
              (loop (cdr lines) (cons (car lines) current) result)]))))

;;; find-see-also-line : (List String) → String | #f
(define (find-see-also-line lines)
  (find (lambda (line)
                (string-starts-with? (string-trim line) "See also:"))
        lines))

;;; parse-see-also : String → (List Symbol)
(define (parse-see-also line)
  (let ([colon-pos (string-find-char line #\:)])
       (if (not colon-pos)
           '()
           (let* ([after-colon (substring line (+ 1 colon-pos) (string-length line))]
                  [parts (string-split after-colon #\,)])
                 (map (lambda (p) (string->symbol (string-trim p)))
                      (filter (lambda (p) (not (string=? (string-trim p) ""))) parts))))))

;;; ==== 
;;; Signature Extraction
;;; ==== 

;;; extract-signature : (List String) Int → String | #f
;;; Extract signature from ;;; name : type comment above definition.
(define (extract-signature lines line-num)
  (if (< line-num 2)
      #f
      (let ([prev-line (list-ref-safe lines (- line-num 2))])
           (and prev-line
                (let ([trimmed (string-trim prev-line)])
                     (and (string-starts-with? trimmed ";;; ")
                          (let ([colon-pos (string-find-char trimmed #\:)])
                               (and colon-pos
                                    (> colon-pos 4)
                                    (string-trim
                                     (substring trimmed
                                                (+ colon-pos 1)
                                                (string-length trimmed)))))))))))

;;; string-find-char : String Char → Int | #f
(define (string-find-char str ch)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; ==== 
;;; Definition Extraction
;;; ==== 

;;; scan-definitions : String → (List Doc-Entry)
;;; Scan a source file and extract all definitions.
(define (scan-definitions path)
  (let ([lines (autodoc-read-file-lines path)])
       (if (null? lines)
           '()
           (let ([category (derive-category path)]
                 [module-name (extract-module-name lines)])
                (let loop ([i 0] [results '()])
                     (if (>= i (length lines))
                         (reverse results)
                         (let* ([line (list-ref lines i)]
                                [trimmed (string-trim line)]
                                [entry (parse-definition trimmed path (+ i 1) lines category module-name)])
                               (loop (+ i 1)
                                     (if entry (cons entry results) results)))))))))

;;; parse-definition : String String Int (List String) Symbol (Symbol | #f) → Doc-Entry | #f
(define (parse-definition line path line-num lines category module-name)
  (cond
   [(string-starts-with? line "(define ")
    (parse-define-entry line path line-num lines category module-name)]
   [(string-starts-with? line "(define-syntax ")
    (parse-syntax-entry line path line-num lines category module-name)]
   [else #f]))

;;; parse-define-entry : String String Int (List String) Symbol (Symbol | #f) → Doc-Entry | #f
(define (parse-define-entry line path line-num lines category module-name)
  (let ([rest (substring line 8 (string-length line))])
       (cond
        ;; (define (name ...) ...)
        [(and (> (string-length rest) 0)
              (char=? (string-ref rest 0) (integer->char 40)))
         (let ([name-end (find-name-end rest 1)])
              (and name-end
                   (let* ([name-str (substring rest 1 name-end)]
                          [name (string->symbol name-str)]
                          [sig (extract-signature lines line-num)])
                         (call-with-values
                          (lambda () (parse-docstring-block lines line-num))
                          (lambda (summary desc see-also)
                                  `((name . ,name)
                                    (kind . define)
                                    (file . ,path)
                                    (line . ,line-num)
                                    (signature . ,sig)
                                    (docstring . ,(or summary desc))
                                    (category . ,category)
                                    (visibility . ,(derive-visibility name))
                                    (examples . ())
                                    (see-also . ,see-also)
                                    (usage . #f)
                                    (module . ,module-name)))))))]
        ;; (define name ...)
        [else
         (let ([name-end (find-name-end rest 0)])
              (and name-end
                   (let* ([name-str (substring rest 0 name-end)]
                          [name (string->symbol name-str)]
                          [sig (extract-signature lines line-num)])
                         (call-with-values
                          (lambda () (parse-docstring-block lines line-num))
                          (lambda (summary desc see-also)
                                  `((name . ,name)
                                    (kind . define)
                                    (file . ,path)
                                    (line . ,line-num)
                                    (signature . ,sig)
                                    (docstring . ,(or summary desc))
                                    (category . ,category)
                                    (visibility . ,(derive-visibility name))
                                    (examples . ())
                                    (see-also . ,see-also)
                                    (usage . #f)
                                    (module . ,module-name)))))))])))

;;; parse-syntax-entry : String String Int (List String) Symbol (Symbol | #f) → Doc-Entry | #f
(define (parse-syntax-entry line path line-num lines category module-name)
  (let ([rest (substring line 15 (string-length line))])
       (let ([name-end (find-name-end rest 0)])
            (and name-end
                 (let* ([name-str (substring rest 0 name-end)]
                        [name (string->symbol name-str)])
                       (call-with-values
                        (lambda () (parse-docstring-block lines line-num))
                        (lambda (summary desc see-also)
                                `((name . ,name)
                                  (kind . syntax)
                                  (file . ,path)
                                  (line . ,line-num)
                                  (signature . #f)
                                  (docstring . ,(or summary desc))
                                  (category . ,category)
                                  (visibility . ,(derive-visibility name))
                                  (examples . ())
                                  (see-also . ,see-also)
                                  (usage . ,(extract-usage-pattern lines line-num))
                                  (module . ,module-name))))))))) 

;;; extract-usage-pattern : (List String) Int → String | #f
;;; Extract usage pattern from comments for macros.
(define (extract-usage-pattern lines line-num)
  (if (< line-num 2)
      #f
      (let loop ([i (- line-num 2)] [usage-lines '()])
           (if (< i 0)
               (if (null? usage-lines) #f (string-join usage-lines "\n"))
               (let ([line (list-ref lines i)])
                    (if (and (string-contains? line "Usage:")
                             (string-starts-with? (string-trim line) ";;;"))
                        (let ([usage-pos (string-find-pos line "Usage:")])
                             (if (not usage-pos)
                                 (if (null? usage-lines) #f (string-join usage-lines "\n"))
                                 (string-trim (substring line (+ usage-pos 6) (string-length line)))))
                        (if (and (not (null? usage-lines))
                                 (string-starts-with? (string-trim line) ";;;"))
                            (loop (- i 1) (cons (strip-comment-prefix line) usage-lines))
                            (if (null? usage-lines) #f (string-join usage-lines "\n")))))))))

;;; string-find-pos : String String → Int | #f
(define (string-find-pos haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) i]
             [else (loop (+ i 1))]))))

;;; find-name-end : String Int → Int | #f
(define (find-name-end str start)
  (let ([len (string-length str)])
       (let loop ([i start])
            (cond
             [(>= i len) (if (> i start) i #f)]
             [(char-name-constituent? (string-ref str i)) (loop (+ i 1))]
             [(> i start) i]
             [else #f]))))

;;; char-name-constituent? : Char → Boolean
(define (char-name-constituent? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memv c '(#\- #\_ #\? #\! #\* #\+ #\/ #\< #\> #\= #\:))))

;;; extract-module-name : (List String) → Symbol | #f
(define (extract-module-name lines)
  (let loop ([i 0])
       (if (>= i (min 20 (length lines)))
           #f
           (let ([line (list-ref lines i)])
                (if (string-contains? line "@module ")
                    (let* ([pos (string-find-pos line "@module ")]
                           [rest (substring line (+ pos 8) (string-length line))]
                           [name-end (find-name-end rest 0)])
                          (if name-end
                              (string->symbol (substring rest 0 name-end))
                              #f))
                    (loop (+ i 1)))))))

;;; ==== 
;;; Example Extraction (AST-based)
;;; ==== 

;;; extract-examples-from-file : String → (List Example)
;;; Use (read) to parse test file and extract examples.
(define (extract-examples-from-file path)
  (let ([exprs (read-file-sexps path)])
       (if (not exprs)
           '()
           (let loop ([exprs exprs] [examples '()])
                (if (null? exprs)
                    (reverse examples)
                    (let ([example (extract-example-from-sexp (car exprs) path)])
                         (loop (cdr exprs)
                               (if example
                                   (cons example examples)
                                   examples))))))))

;;; extract-example-from-sexp : Sexp String → Example | #f
(define (extract-example-from-sexp expr path)
  (cond
   ;; (test "label" expected actual)
   [(and (pair? expr)
         (eq? (car expr) 'test)
         (>= (length expr) 4))
    (let ([label (cadr expr)]
          [expected (caddr expr)]
          [actual (cadddr expr)])
         `((source . ,(format "~s" actual))
           (expected . ,(format "~s" expected))
           (label . ,(if (string? label) label #f))
           (from . ,path)
           (target . ,(extract-target-function actual))))]
   
   ;; (assert-equal expected actual)
   [(and (pair? expr)
         (eq? (car expr) 'assert-equal)
         (>= (length expr) 3))
    (let ([expected (cadr expr)]
          [actual (caddr expr)])
         `((source . ,(format "~s" actual))
           (expected . ,(format "~s" expected))
           (label . #f)
           (from . ,path)
           (target . ,(extract-target-function actual))))]
   
   ;; (define-test name body...)
   [(and (pair? expr)
         (eq? (car expr) 'define-test)
         (>= (length expr) 2))
    (let ([name (cadr expr)])
         `((source . ,(format "~s" expr))
           (expected . "passes")
           (label . ,(format "test: ~a" name))
           (from . ,path)
           (target . ,name)))]
   
   [else #f]))

;;; extract-target-function : Sexp → Symbol | #f
;;; Extract the primary function being tested from an expression.
(define (extract-target-function expr)
  (cond
   [(not (pair? expr)) #f]
   [(symbol? (car expr)) (car expr)]
   [(pair? (car expr)) (extract-target-function (car expr))]
   [else #f]))

;;; ==== 
;;; Index Building
;;; ==== 

;;; scan-directory : String String → (List String)
;;; Find all .ss files in directory recursively.
(define (scan-directory dir ext)
  (guard (e [else '()])
         (let loop ([pending (list dir)] [results '()])
              (if (null? pending)
                  results
                  (let ([current (car pending)])
                       (if (file-directory? current)
                           (let ([entries (map (lambda (e) 
                                                       (string-append current "/" e))
                                               (directory-list current))])
                                (loop (append (cdr pending) entries) results))
                           (if (string-ends-with? current ext)
                               (loop (cdr pending) (cons current results))
                               (loop (cdr pending) results))))))))

;;; build-autodoc-index! : → void
;;; Scan codebase and build documentation index.
(define (build-autodoc-index!)
  ;; Clear existing state
  (hashtable-clear! *autodoc-registry*)
  (hashtable-clear! *autodoc-categories*)
  (set! *orphan-examples* '())
  
  (display "Building autodoc index...\n")
  
  ;; Scan source files for definitions
  (let ([source-files (append
                       (scan-directory "core" ".ss")
                       (scan-directory "shell" ".ss")
                       (scan-directory "lattice" ".ss"))])
       (display (format "  Scanning ~a source files...\n" (length source-files)))
       (for-each
        (lambda (path)
                ;; Skip test files for definitions
                (unless (string-contains? path "/test-")
                        (let ([entries (scan-definitions path)])
                             (for-each
                              (lambda (entry)
                                      (let ([name (cdr (assq 'name entry))]
                                            [cat (cdr (assq 'category entry))])
                                           ;; Register in main registry
                                           (hashtable-set! *autodoc-registry* name entry)
                                           ;; Register in category index
                                           (let ([existing (hashtable-ref *autodoc-categories* cat '())])
                                                (hashtable-set! *autodoc-categories* cat
                                                                (cons name existing))))) 
                              entries))))
        source-files))
  
  ;; Scan test files for examples
  (let ([test-files (filter (lambda (p) (string-contains? p "/test-"))
                            (append
                             (scan-directory "core" ".ss")
                             (scan-directory "shell" ".ss")
                             (scan-directory "lattice" ".ss")))])
       (display (format "  Scanning ~a test files for examples...\n" (length test-files)))
       (for-each
        (lambda (path)
                (let ([examples (extract-examples-from-file path)])
                     (for-each
                      (lambda (ex)
                              (let* ([target (cdr (assq 'target ex))]
                                     [entry (and target (hashtable-ref *autodoc-registry* target #f))])
                                    (if entry
                                        ;; Add example to existing entry
                                        (let* ([current-examples (cdr (assq 'examples entry))]
                                               [updated-entry (update-alist entry 'examples 
                                                                            (cons ex current-examples))])
                                              (hashtable-set! *autodoc-registry* target updated-entry))
                                        ;; Orphaned example
                                        (set! *orphan-examples* (cons ex *orphan-examples*))))) 
                      examples)))
        test-files))
  
  (set! *autodoc-initialized* #t)
  (display "Autodoc index complete.\n")
  (doc-stats))

;;; update-alist : Alist Key Value → Alist
(define (update-alist alist key value)
  (map (lambda (pair)
               (if (eq? (car pair) key)
                   (cons key value)
                   pair))
       alist))

;;; ==== 
;;; Display Functions
;;; ==== 

;;; display-doc-entry : Doc-Entry → void
(define (display-doc-entry entry)
  (let ([name (cdr (assq 'name entry))]
        [kind (cdr (assq 'kind entry))]
        [file (cdr (assq 'file entry))]
        [line (cdr (assq 'line entry))]
        [sig (cdr (assq 'signature entry))]
        [doc (cdr (assq 'docstring entry))]
        [cat (cdr (assq 'category entry))]
        [vis (cdr (assq 'visibility entry))]
        [examples (cdr (assq 'examples entry))]
        [see-also (cdr (assq 'see-also entry))]
        [usage (cdr (assq 'usage entry))]
        [mod (cdr (assq 'module entry))])
       
       (display "\n")
       (printf  "  --- ~a~a " name
               (if (eq? vis 'private) " [internal]" ""))
       (display "----------------------------------------------\n")
       (display "\n")
       
       (printf  "  Type: ~a    Category: ~a\n" kind cat)
       (when mod (printf "  Module: ~a\n" mod))
       (printf  "  Location: ~a:~a\n" file line)
       (display "\n")
       
       (when sig
             (display "  Signature:\n")
             (printf  "    ~a\n\n" sig))
       
       (when usage
             (display "  Usage:\n")
             (printf  "    ~a\n\n" usage))
       
       (when doc
             (display "  Description:\n")
             (printf  "    ~a\n\n" doc))
       
       (unless (null? examples)
               (display "  Examples:\n")
               (for-each
                (lambda (ex)
                        (let ([src (cdr (assq 'source ex))]
                              [exp (cdr (assq 'expected ex))]
                              [lbl (cdr (assq 'label ex))])
                             (when lbl (printf "    ; ~a\n" lbl))
                             (printf "    ~a → ~a\n" src exp)))
                (take 5 examples))
               (when (> (length examples) 5)
                     (printf "    ... and ~a more examples\n" (- (length examples) 5)))
               (display "\n"))
       
       (unless (null? see-also)
               (display "  See also: ")
               (display (string-join (map symbol->string see-also) ", "))
               (display "\n"))
       
       (display "\n")))

;;; ====
;;; REPL Commands
;;; ==== 

;;; doc : Symbol → void
;;; Show documentation for a symbol.
(define (doc name)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let ([entry (hashtable-ref *autodoc-registry* name #f)])
       (if entry
           (display-doc-entry entry)
           (begin
            (printf "\n  Symbol '~a' not found in autodoc index.\n\n" name)
            (let ([suggestions (doc-search (symbol->string name))])
                 (unless (null? suggestions)
                         (display "  Did you mean one of:\n")
                         (for-each (lambda (s) (printf "    ~a\n" s))
                                   (take 5 suggestions))
                         (display "\n"))))))) 

;;; doc-search : String → (List Symbol)
;;; Search for symbols matching pattern.
(define (doc-search pattern)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let* ([pattern-lower (string-downcase pattern)]
         [keys (hashtable-keys *autodoc-registry*)]
         [matches '()])
        (vector-for-each
         (lambda (name)
                 (when (string-contains? (string-downcase (symbol->string name)) pattern-lower)
                       (set! matches (cons name matches))))
         keys)
        (sort-symbols matches)))

;;; doc-category : Symbol → void
;;; List symbols in a category.
(define (doc-category cat)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let ([symbols (hashtable-ref *autodoc-categories* cat '())])
       (display "\n")
       (printf "  Category: ~a\n" cat)
       (display "  ------------------------------------------------------------\n\n")
       (if (null? symbols)
           (display "  (no symbols in this category)\n")
           (begin
            (printf "  ~a symbols:\n\n" (length symbols))
            (for-each
             (lambda (name)
                     (let ([entry (hashtable-ref *autodoc-registry* name #f)])
                          (when entry
                                (let ([sig (cdr (assq 'signature entry))])
                                     (if sig
                                         (printf "    ~a : ~a\n" name sig)
                                         (printf "    ~a\n" name))))))
             (sort-symbols symbols))))
       (display "\n")))

;;; doc-examples : Symbol → void
;;; Show only examples for a symbol.
(define (doc-examples name)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let ([entry (hashtable-ref *autodoc-registry* name #f)])
       (if entry
           (let ([examples (cdr (assq 'examples entry))])
                (display "\n")
                (printf "  Examples for ~a:\n\n" name)
                (if (null? examples)
                    (display "    (no examples found)\n")
                    (for-each
                     (lambda (ex)
                             (let ([src (cdr (assq 'source ex))]
                                   [exp (cdr (assq 'expected ex))]
                                   [lbl (cdr (assq 'label ex))]
                                   [from (cdr (assq 'from ex))])
                                  (when lbl (printf "    ; ~a\n" lbl))
                                  (printf "    ~a → ~a\n" src exp)
                                  (printf "      from: ~a\n\n" from)))
                     examples))
                (display "\n"))
           (printf "\n  Symbol '~a' not found.\n\n" name))))

;;; doc-stats : → void
;;; Show documentation coverage statistics.
(define (doc-stats)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let* ([total (hashtable-size *autodoc-registry*)]
         [with-sig 0]
         [with-doc 0]
         [with-examples 0]
         [public 0]
         [private 0])
        (vector-for-each
         (lambda (name)
                 (let ([entry (hashtable-ref *autodoc-registry* name #f)])
                      (when entry
                            (when (cdr (assq 'signature entry))
                                  (set! with-sig (+ with-sig 1)))
                            (when (cdr (assq 'docstring entry))
                                  (set! with-doc (+ with-doc 1)))
                            (unless (null? (cdr (assq 'examples entry)))
                                    (set! with-examples (+ with-examples 1)))
                            (if (eq? (cdr (assq 'visibility entry)) 'public)
                                (set! public (+ public 1))
                                (set! private (+ private 1))))))
         (hashtable-keys *autodoc-registry*))
        
        (display "\n")
        (display "  --- AUTODOC STATISTICS ------------------------------------------\n")
        (display "\n")
        (printf  "  Total symbols indexed:  ~a\n" total)
        (printf  "    Public:               ~a\n" public)
        (printf  "    Private/Internal:     ~a\n" private)
        (display "\n")
        (printf  "  With signatures:        ~a / ~a (~a%%)\n" 
                with-sig total (if (> total 0) (quotient (* 100 with-sig) total) 0))
        (printf  "  With docstrings:        ~a / ~a (~a%%)\n" 
                with-doc total (if (> total 0) (quotient (* 100 with-doc) total) 0))
        (printf  "  With examples:          ~a / ~a (~a%%)\n" 
                with-examples total (if (> total 0) (quotient (* 100 with-examples) total) 0))
        (display "\n")
        (printf  "  Orphaned examples:      ~a\n" (length *orphan-examples*))
        (printf  "  Categories:             ~a\n" (hashtable-size *autodoc-categories*))
        (display "\n")))

;;; doc-categories : → void
;;; List all available categories.
(define (doc-categories)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (let ([cats '()])
       (vector-for-each
        (lambda (cat)
                (let ([count (length (hashtable-ref *autodoc-categories* cat '()))])
                     (set! cats (cons (cons cat count) cats))))
        (hashtable-keys *autodoc-categories*))
       (display "\n")
       (display "  Available categories:\n\n")
       (for-each
        (lambda (pair)
                (printf "    ~a~a(~a symbols)\n"
                        (car pair)
                        (make-string (max 1 (- 25 (string-length (symbol->string (car pair))))) #\space)
                        (cdr pair)))
        (sort (lambda (a b) (string<? (symbol->string (car a)) 
                                      (symbol->string (car b))))
              cats))
       (display "\n")))

;;; doc-refresh! : → void
;;; Rebuild the documentation index.
(define (doc-refresh!)
  (set! *autodoc-initialized* #f)
  (build-autodoc-index!))

;;; sort-symbols : (List Symbol) → (List Symbol)
(define (sort-symbols syms)
  (list-sort (lambda (a b)
                     (string<? (symbol->string a) (symbol->string b)))
             syms))

;;; ==== 
;;; Help System Integration
;;; ==== 

;;; autodoc-lookup : Symbol → Entry | #f
;;; Check if symbol exists in autodoc (for help integration).
(define (autodoc-lookup name)
  (unless *autodoc-initialized*
          (build-autodoc-index!))
  (hashtable-ref *autodoc-registry* name #f))

;;; Register hooks with help.ss (if loaded)
;;; These hooks allow (help 'name) to dispatch to autodoc for non-primitives.
(when (top-level-bound? '*autodoc-help-handler*)
      (set! *autodoc-help-handler* doc)
      (set! *autodoc-lookup* autodoc-lookup)
      (set! *autodoc-search-handler* doc-search))

;;; ==== 
;;; Initialization Message
;;; ==== 

(display "\nAutodoc system loaded.\n")
(display "Commands: (doc 'name), (doc-search \"pattern\"), (doc-category 'cat)\n")
(display "          (doc-examples 'name), (doc-stats), (doc-categories)\n")
(display "          (doc-refresh!) to rebuild index\n")
(when (top-level-bound? '*autodoc-help-handler*)
      (display "          (help 'name) now works for all symbols\n"))
(display "\n")
