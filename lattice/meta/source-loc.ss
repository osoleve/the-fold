(unless (top-level-bound? 'hamt-empty) (load "lattice/data/hamt.ss"))

;;; @module source-loc
;;; @requires hamt
(doc 'module 'source-loc)
(doc 'description "Source location tracking for jump-to-definition workflows")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'section 'state)

;;; Global source location cache: symbol -> (file . line)
(define *source-locations* hamt-empty)

;;; ====
;;; Pure Parsing (operates on string data, no I/O)
;;; ====

;;; parse-definitions-with-lines : String × (List String) -> (List (Symbol File Line))
;;; Parse lines tracking definitions and their line numbers
(define (parse-definitions-with-lines path lines)
  (let loop ([lines lines]
             [line-num 1]
             [results '()])
       (if (null? lines)
           results
           (let* ([line (car lines)]
                  [name (try-extract-define-name line)])
                 (if name
                     (loop (cdr lines)
                           (+ line-num 1)
                           (cons (list name path line-num) results))
                     (loop (cdr lines)
                           (+ line-num 1)
                           results))))))

;;; try-extract-define-name : String -> Symbol | #f
;;; Try to extract a defined name from a line
;;; Note: Check longer keywords first to avoid prefix matching issues
(define (try-extract-define-name line)
  (let ([trimmed (string-trim-left-loc line)])
       (cond
        ;; (define-test "name" ...) - skip these
        [(and (>= (string-length trimmed) 12)
              (string=? (substring trimmed 0 12) "(define-test"))
         #f]
        ;; (define-record-type name ...)
        [(and (>= (string-length trimmed) 19)
              (string=? (substring trimmed 0 19) "(define-record-type"))
         (extract-name-after-keyword trimmed 19)]
        ;; (define-syntax name ...)
        [(and (>= (string-length trimmed) 14)
              (string=? (substring trimmed 0 14) "(define-syntax"))
         (extract-name-after-keyword trimmed 14)]
        ;; (define (name ...) ...) or (define name ...)
        ;; Check for "(define " or "(define(" to avoid matching define-*
        [(and (>= (string-length trimmed) 8)
              (string=? (substring trimmed 0 7) "(define")
              (let ([next-char (string-ref trimmed 7)])
                   (or (char-whitespace? next-char)
                       (char=? next-char #\())))
         (extract-name-from-define trimmed)]
        [else #f])))

;;; extract-name-from-define : String -> Symbol | #f
;;; Extract name from a define form
(define (extract-name-from-define line)
  (guard (e [else #f])
         (let* ([after-define (substring line 7 (string-length line))]
                [cleaned (string-trim-left-loc after-define)])
               (cond
                ;; (define (name args...) ...)
                [(and (> (string-length cleaned) 0)
                      (char=? (string-ref cleaned 0) #\())
                 (let ([inner (substring cleaned 1 (string-length cleaned))])
                      (extract-first-sym inner))]
                ;; (define name ...)
                [else
                 (extract-first-sym cleaned)]))))

;;; extract-name-after-keyword : String × Int -> Symbol | #f
;;; Extract name after a keyword at given position
(define (extract-name-after-keyword line start-pos)
  (guard (e [else #f])
         (if (>= start-pos (string-length line))
             #f
             (let* ([after-keyword (substring line start-pos (string-length line))]
                    [cleaned (string-trim-left-loc after-keyword)])
                   (extract-first-sym cleaned)))))

;;; extract-first-sym : String -> Symbol | #f
;;; Extract the first symbol from a string
(define (extract-first-sym str)
  (let ([len (string-length str)])
       (if (= len 0)
           #f
           (let loop ([i 0])
                (if (>= i len)
                    (if (> i 0)
                        (string->symbol str)
                        #f)
                    (let ([c (string-ref str i)])
                         (if (or (char-whitespace? c)
                                 (char=? c #\))
                                 (char=? c #\()
                                 (char=? c #\")
                                 (char=? c #\'))
                             (if (= i 0)
                                 #f
                                 (string->symbol (substring str 0 i)))
                             (loop (+ i 1)))))))))

;;; string-trim-left-loc : String -> String
;;; Remove leading whitespace
(define (string-trim-left-loc str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (if (>= i len)
                ""
                (if (char-whitespace? (string-ref str i))
                    (loop (+ i 1))
                    (substring str i len))))))

;;; ====
;;; Cache Lookup (pure — reads from populated hashtable)
;;; ====

;;; get-source-location : Symbol -> (File . Line) | #f
;;; Get the source location for a symbol
(define (get-source-location sym)
  (guard (e [else #f])
         (hamt-lookup sym *source-locations*)))

;;; format-source-location : Symbol -> String | #f
;;; Get formatted "file:line" string for a symbol
(define (format-source-location sym)
  (let ([loc (get-source-location sym)])
       (if loc
           (format "~a:~a" (car loc) (cdr loc))
           #f)))

;;; source-location-count : -> Nat
;;; Get number of indexed definitions
(define (source-location-count)
  (hamt-size *source-locations*))

;;; ====
;;; Cache Population (called from boundary orchestrator)
;;; ====

;;; populate-source-locations! : (List (Symbol File Line)) -> Void
;;; Populate the cache from a list of definition records.
;;; Prefers definitions from canonical module files (via *export-module-map*
;;; and *module-paths*) over incidental definitions in other files.
(define (populate-source-locations! defs)
  (let ([canonical-file-for
         (if (and (top-level-bound? '*export-module-map*)
                  (top-level-bound? '*module-paths*))
             (lambda (sym)
               (let ([mod (hamt-lookup sym *export-module-map*)])
                 (and mod (hashtable-ref *module-paths* mod #f))))
             (lambda (sym) #f))])
    (set! *source-locations*
          (fold-left
           (lambda (acc def)
             (let ([name (car def)]
                   [file (cadr def)]
                   [line (caddr def)])
               (let ([existing (hamt-lookup name acc)]
                     [canonical (canonical-file-for name)])
                 (cond
                   ;; No existing entry — store
                   [(not existing)
                    (hamt-assoc name (cons file line) acc)]
                   ;; This file IS the canonical source — always overwrite
                   [(and canonical (string=? file canonical))
                    (hamt-assoc name (cons file line) acc)]
                   ;; Existing file is already canonical — keep it
                   [(and canonical (string=? (car existing) canonical))
                    acc]
                   ;; Neither is canonical — first wins (keep existing)
                   [else acc]))))
           hamt-empty
           defs))))
