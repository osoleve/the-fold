(load "core/base/prelude.ss")
(load "boundary/meta/manifest-sync.ss")

(doc 'module 'export-annotator)
(doc 'description "Add (doc 'export #t) annotations to functions declared in manifests")
(doc 'layer 'boundary)
(doc 'purity 'partial)

;;; ====
;;; File Editing
;;; ====

;;; read-file-lines : String -> (List String) | #f
(define (read-file-lines path)
  (guard (e [else #f])
    (call-with-input-file path
      (lambda (port)
        (let loop ([acc '()])
          (let ([line (get-line port)])
            (if (eof-object? line)
                (reverse acc)
                (loop (cons line acc)))))))))

;;; write-file-lines : String × (List String) -> Void
(define (write-file-lines path lines)
  (call-with-output-file path
    (lambda (port)
      (for-each
       (lambda (line) (put-string port line) (newline port))
       lines))
    'truncate))

;;; ====
;;; Pattern Matching
;;; ====

;;; find-define-line : (List String) × Symbol -> (Nat . 'func|'value) | #f
;;; Find the line number (0-indexed) where a symbol is defined
;;; Returns (line-num . def-type) where def-type is 'func or 'value
(define (find-define-line lines sym)
  (let ([sym-str (symbol->string sym)])
    (let loop ([lines lines] [idx 0])
      (cond
        [(null? lines) #f]
        [else
         (let ([match (define-line-matches? (car lines) sym-str)])
           (if match
               (cons idx match)  ; Return (line-num . type)
               (loop (cdr lines) (+ idx 1))))]))))

;;; define-line-matches? : String × String -> Boolean | 'func | 'value
;;; Check if a line is a define for the given symbol name
;;; Returns 'func for (define (name ...)), 'value for (define name ...), #f otherwise
(define (define-line-matches? line name)
  (let ([line (string-trim-line line)])
    (cond
     ;; (define (name ...) - function definition
     [(and (string-prefix-ex? "(define (" line)
           (define-func-name-matches? line name))
      'func]
     ;; (define name - value definition
     [(and (string-prefix-ex? "(define " line)
           (not (string-prefix-ex? "(define (" line))
           (define-var-name-matches? line name))
      'value]
     [else #f])))

;;; string-trim-line : String -> String
(define (string-trim-line str)
  (let loop ([i 0])
    (if (>= i (string-length str))
        str
        (if (char-whitespace? (string-ref str i))
            (loop (+ i 1))
            (substring str i (string-length str))))))

;;; string-prefix-ex? : String × String -> Boolean
(define (string-prefix-ex? prefix str)
  (let ([plen (string-length prefix)]
        [slen (string-length str)])
    (and (>= slen plen)
         (string=? prefix (substring str 0 plen)))))

;;; define-func-name-matches? : String × String -> Boolean
;;; For "(define (name" patterns
(define (define-func-name-matches? line name)
  (let* ([prefix "(define ("]
         [plen (string-length prefix)]
         [nlen (string-length name)])
    (and (> (string-length line) (+ plen nlen))  ; Must have char after name
         (string=? name (substring line plen (+ plen nlen)))
         (let ([next-char (string-ref line (+ plen nlen))])
           (or (char=? next-char #\space)
               (char=? next-char #\))
               (char=? next-char #\newline))))))

;;; define-var-name-matches? : String × String -> Boolean
;;; For "(define name" patterns
(define (define-var-name-matches? line name)
  (let* ([prefix "(define "]
         [plen (string-length prefix)]
         [nlen (string-length name)])
    (and (>= (string-length line) (+ plen nlen))
         (string=? name (substring line plen (+ plen nlen)))
         (or (= (string-length line) (+ plen nlen))
             (let ([next-char (string-ref line (+ plen nlen))])
               (or (char=? next-char #\space)
                   (char=? next-char #\newline)))))))

;;; ====
;;; Annotation Logic
;;; ====

;;; needs-export-annotation? : (List String) × Nat -> Boolean
;;; Check if the definition already has an export annotation
;;; Checks both inside body and before definition (for targeted docs)
(define (needs-export-annotation? lines def-line)
  ;; Check lines before the definition for targeted export doc
  (let check-before ([idx (- def-line 1)] [depth 0])
    (if (or (< idx 0) (> depth 3))
        ;; Check lines after the definition for contextual export doc
        (let check-after ([idx (+ def-line 1)] [depth 1])
          (cond
            [(>= idx (length lines)) #t]  ; Reached end, no annotation found
            [(>= depth 5) #t]  ; Searched 5 lines deep, no annotation
            [else
             (let ([line (list-ref lines idx)])
               (cond
                 ;; Found export annotation
                 [(string-contains-ex? line "(doc 'export #t)") #f]
                 ;; Still in function body (has doc forms or continuation)
                 [(or (string-contains-ex? line "(doc '")
                      (string-blank-ex? line))
                  (check-after (+ idx 1) (+ depth 1))]
                 ;; Reached actual code without finding export
                 [else #t]))]))
        (let ([line (list-ref lines idx)])
          (cond
            ;; Found targeted export annotation before define
            [(string-contains-ex? line "'export #t)") #f]
            ;; Empty line or other doc - keep checking
            [(or (string-blank-ex? line)
                 (string-contains-ex? line "(doc "))
             (check-before (- idx 1) (+ depth 1))]
            ;; Reached other code - stop checking before
            [else
             ;; Check after definition instead
             (let check-after ([idx (+ def-line 1)] [depth 1])
               (cond
                 [(>= idx (length lines)) #t]
                 [(>= depth 5) #t]
                 [else
                  (let ([line (list-ref lines idx)])
                    (cond
                      [(string-contains-ex? line "(doc 'export #t)") #f]
                      [(or (string-contains-ex? line "(doc '")
                           (string-blank-ex? line))
                       (check-after (+ idx 1) (+ depth 1))]
                      [else #t]))]))])))))

;;; string-contains-ex? : String × String -> Boolean
(define (string-contains-ex? str substr)
  (let ([slen (string-length str)]
        [sslen (string-length substr)])
    (let loop ([i 0])
      (cond
        [(> (+ i sslen) slen) #f]
        [(string=? substr (substring str i (+ i sslen))) #t]
        [else (loop (+ i 1))]))))

;;; string-blank-ex? : String -> Boolean
(define (string-blank-ex? str)
  (let loop ([i 0])
    (cond
      [(>= i (string-length str)) #t]
      [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
      [else #f])))

;;; ====
;;; Insertion
;;; ====

;;; insert-export-annotation : (List String) × Nat × Symbol × Symbol -> (List String)
;;; Insert export annotation
;;; For 'func: insert (doc 'export #t) after the define line (inside body)
;;; For 'value: insert (doc sym 'export #t) before the define line
(define (insert-export-annotation lines def-line def-type sym)
  (let* ([before (take-lines lines def-line)]
         [def-ln (list-ref lines def-line)]
         [after (drop-lines lines (+ def-line 1))])
    (if (eq? def-type 'func)
        ;; Function: insert contextual doc inside body
        (let* ([indent (detect-indent after)]
               [annotation (string-append indent "(doc 'export #t)")])
          (append before (list def-ln annotation) after))
        ;; Value: insert targeted doc before define
        (let* ([indent (get-line-indent def-ln)]
               [annotation (string-append indent "(doc " (symbol->string sym) " 'export #t)")])
          (append before (list annotation def-ln) after)))))

;;; get-line-indent : String -> String
;;; Get the leading whitespace of a line
(define (get-line-indent str)
  (let loop ([i 0] [acc '()])
    (if (or (>= i (string-length str))
            (not (char-whitespace? (string-ref str i))))
        (list->string (reverse acc))
        (loop (+ i 1) (cons (string-ref str i) acc)))))

;;; take-lines : (List String) × Nat -> (List String)
(define (take-lines lst n)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take-lines (cdr lst) (- n 1)))))

;;; drop-lines : (List String) × Nat -> (List String)
(define (drop-lines lst n)
  (if (or (null? lst) (<= n 0))
      lst
      (drop-lines (cdr lst) (- n 1))))

;;; detect-indent : (List String) -> String
;;; Detect indentation from the first non-empty line
(define (detect-indent lines)
  (if (null? lines)
      "  "
      (let ([first (car lines)])
        (if (string-blank-ex? first)
            (detect-indent (cdr lines))
            (let loop ([i 0] [acc '()])
              (if (or (>= i (string-length first))
                      (not (char-whitespace? (string-ref first i))))
                  (list->string (reverse acc))
                  (loop (+ i 1) (cons (string-ref first i) acc))))))))

;;; ====
;;; Main Logic
;;; ====

;;; annotate-file-exports : String × (List Symbol) -> Nat
;;; Add export annotations to specified symbols in a file
;;; Returns count of annotations added
(define (annotate-file-exports path symbols)
  (let ([lines (read-file-lines path)])
    (if (not lines)
        0
        (let loop ([syms symbols] [lines lines] [count 0])
          (if (null? syms)
              (begin
                (when (> count 0)
                  (write-file-lines path lines))
                count)
              (let* ([sym (car syms)]
                     [def-info (find-define-line lines sym)])
                (if (not def-info)
                    (loop (cdr syms) lines count)
                    (let ([def-line (car def-info)]
                          [def-type (cdr def-info)])
                      (if (needs-export-annotation? lines def-line)
                          (let ([new-lines (insert-export-annotation lines def-line def-type sym)])
                            (loop (cdr syms) new-lines (+ count 1)))
                          (loop (cdr syms) lines count))))))))))

;;; annotate-skill : String -> Nat
;;; Add export annotations to all manifest-declared exports in a skill
;;; Returns total count of annotations added
(define (annotate-skill skill-dir)
  (let ([edits (generate-export-edits skill-dir)])
    (if (null? edits)
        (begin
          (printf "No annotations needed for ~a~n" skill-dir)
          0)
        (let ([by-file (group-by-file edits)])
          (let ([total 0])
            (for-each
             (lambda (file-group)
               (let* ([path (car file-group)]
                      [symbols (cdr file-group)]
                      [count (annotate-file-exports path symbols)])
                 (when (> count 0)
                   (printf "  ~a: +~a annotations~n" path count))
                 (set! total (+ total count))))
             by-file)
            (printf "Total: ~a annotations added to ~a~n" total skill-dir)
            total)))))

;;; group-by-file : (List (file . symbol)) -> (List (file . (List symbol)))
(define (group-by-file edits)
  (let ([table (make-hashtable string-hash string=?)])
    (for-each
     (lambda (edit)
       (let* ([file (car edit)]
              [sym (cdr edit)]
              [existing (hashtable-ref table file '())])
         (hashtable-set! table file (cons sym existing))))
     edits)
    (let-values ([(keys vals) (hashtable-entries table)])
      (map cons (vector->list keys) (vector->list vals)))))

;;; ====
;;; Dry Run
;;; ====

;;; annotate-skill-dry-run : String -> Nat
;;; Preview what annotations would be added
(define (annotate-skill-dry-run skill-dir)
  (let ([edits (generate-export-edits skill-dir)])
    (printf "Dry run for ~a:~n" skill-dir)
    (if (null? edits)
        (begin
          (printf "  No annotations needed~n")
          0)
        (let ([by-file (group-by-file edits)])
          (let ([total 0])
            (for-each
             (lambda (file-group)
               (let* ([path (car file-group)]
                      [lines (read-file-lines path)]
                      [symbols (cdr file-group)])
                 (when lines
                   (for-each
                    (lambda (sym)
                      (let ([def-info (find-define-line lines sym)])
                        (when def-info
                          (let ([def-line (car def-info)]
                                [def-type (cdr def-info)])
                            (when (needs-export-annotation? lines def-line)
                              (printf "  ~a: ~a (~a, line ~a)~n" path sym def-type (+ def-line 1))
                              (set! total (+ total 1)))))))
                    symbols))))
             by-file)
            (printf "Would add ~a annotations~n" total)
            total)))))
