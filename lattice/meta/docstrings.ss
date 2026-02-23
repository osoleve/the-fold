(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))

(doc 'module 'docstrings)
(doc 'description "Extract ;;; docstrings from source files and associate them with function definitions for improved search indexing.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Docstring format: ;;; function-name : type-signature followed by description lines, then (define (function-name args...) ...)")

(doc 'section 'state)

(doc *docstrings* 'description "Global docstring cache: symbol -> string")
(define *docstrings* hamt-empty)

(doc 'section 'pure-parsing)

(doc parse-docstrings 'type (-> (List String) (List (Pair Symbol String))))
(doc parse-docstrings 'description "Parse lines looking for docstring + define patterns")
(define (parse-docstrings lines)
  (let loop ([lines lines]
             [pending-doc '()]
             [results '()])
       (if (null? lines)
           results
           (let ([line (car lines)])
                (cond
                 ;; Docstring line (;;; ...)
                 [(docstring-line? line)
                  (loop (cdr lines)
                        (cons (extract-docstring-text line) pending-doc)
                        results)]
                 ;; Definition line following docstring
                 [(and (not (null? pending-doc))
                       (define-line? line))
                  (let ([name (extract-define-name line)])
                       (if name
                           (loop (cdr lines)
                                 '()
                                 (cons (cons name (join-docstring (reverse pending-doc)))
                                       results))
                           (loop (cdr lines) '() results)))]
                 ;; Any other line clears pending docstring
                 [else
                  (loop (cdr lines) '() results)])))))

;;; docstring-line? : String -> Boolean
;;; Check if line is a docstring (starts with ;;;)
(define (docstring-line? line)
  (let ([trimmed (string-trim-left-doc line)])
       (and (>= (string-length trimmed) 3)
            (string=? (substring trimmed 0 3) ";;;"))))

;;; extract-docstring-text : String -> String
;;; Extract the text after ;;;
(define (extract-docstring-text line)
  (let* ([trimmed (string-trim-left-doc line)]
         [after-prefix (substring trimmed 3 (string-length trimmed))])
        (string-trim-left-doc after-prefix)))

;;; define-line? : String -> Boolean
;;; Check if line starts a define form
(define (define-line? line)
  (let ([trimmed (string-trim-left-doc line)])
       (and (>= (string-length trimmed) 7)
            (string=? (substring trimmed 0 7) "(define"))))

;;; extract-define-name : String -> Symbol | #f
;;; Extract the name being defined
;;; Handles both (define (name ...) and (define name ...)
(define (extract-define-name line)
  (guard (e [else #f])
         (let* ([trimmed (string-trim-left-doc line)]
                ;; Skip "(define "
                [after-define (substring trimmed 8 (string-length trimmed))]
                [cleaned (string-trim-left-doc after-define)])
               (cond
                ;; (define (name args...) ...)
                [(and (> (string-length cleaned) 0)
                      (char=? (string-ref cleaned 0) #\())
                 (let ([inner (substring cleaned 1 (string-length cleaned))])
                      (extract-first-symbol inner))]
                ;; (define name ...)
                [else
                 (extract-first-symbol cleaned)]))))

;;; extract-first-symbol : String -> Symbol | #f
;;; Extract the first symbol from a string
(define (extract-first-symbol str)
  (let ([len (string-length str)])
       (if (= len 0)
           #f
           (let loop ([i 0])
                (if (>= i len)
                    (string->symbol str)
                    (let ([c (string-ref str i)])
                         (if (or (char-whitespace? c)
                                 (char=? c #\))
                                 (char=? c #\())
                             (if (= i 0)
                                 #f
                                 (string->symbol (substring str 0 i)))
                             (loop (+ i 1)))))))))

;;; join-docstring : (List String) -> String
;;; Join docstring lines into a single string
(define (join-docstring lines)
  (if (null? lines)
      ""
      (let loop ([rest (cdr lines)]
                 [result (car lines)])
           (if (null? rest)
               result
               (loop (cdr rest)
                     (string-append result " " (car rest)))))))

;;; string-trim-left-doc : String -> String
;;; Remove leading whitespace (module-local to avoid collision with source-loc's version)
(define (string-trim-left-doc str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (if (>= i len)
                ""
                (if (char-whitespace? (string-ref str i))
                    (loop (+ i 1))
                    (substring str i len))))))

(doc 'section 'cache-population)

;;; populate-docstrings! : (List (Pair Symbol String)) -> Void
;;; Populate the cache from a list of (symbol . docstring) pairs.
;;; Called from boundary orchestrator after I/O.
(define (populate-docstrings! entries)
  (set! *docstrings*
        (fold-left (lambda (acc entry)
                     (hamt-assoc (car entry) (cdr entry) acc))
                   hamt-empty
                   entries)))

(doc 'section 'cache-lookup)

(doc get-docstring 'type (-> Symbol (Maybe String)))
(doc get-docstring 'description "Get the docstring for a function")
(define (get-docstring sym)
  (guard (e [else #f])
         (hamt-lookup sym *docstrings*)))

(doc docstring-terms 'type (-> Symbol (List Symbol)))
(doc docstring-terms 'description "Get search terms from a function's docstring")
(define (docstring-terms sym)
  (let ([doc (get-docstring sym)])
       (if (and doc (string? doc) (> (string-length doc) 0))
           (tokenize doc)
           '())))

(doc 'section 'repl-interface)

(unless (top-level-bound? '*docstrings-banner-shown*)
  (meta-printf "docstrings.ss loaded.\n")
  (meta-printf "  (get-docstring 'fn)            - Get docstring for function\n")
  (meta-printf "  (docstring-terms 'fn)          - Get search terms from docstring\n"))
(set-top-level-value! '*docstrings-banner-shown* #t)
