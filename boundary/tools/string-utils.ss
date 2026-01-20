(load "core/base/prelude.ss")

(doc 'module 'string-utils)
(doc 'description "String utilities re-exported from prelude.ss with shell-specific extensions")
(doc 'layer 'boundary)
(doc 'purity 'total)
(doc 'note "Canonical source is core/prelude.ss; this adds shell-specific aliases and extensions")

(doc 'section 'shell-aliases)

(define (string-prefix? prefix str)
  (doc 'type "(-> String String Bool)")
  (doc 'description "Check if str starts with prefix")
  (doc 'note "Argument order is (prefix str) for consistency with shell code patterns")
  (string-starts-with? str prefix))

(define (string-suffix? suffix str)
  (doc 'type "(-> String String Bool)")
  (doc 'description "Check if str ends with suffix")
  (doc 'note "Argument order is (suffix str) for consistency with shell code patterns")
  (string-ends-with? str suffix))

(doc 'section 'shell-extensions)

(define (string-index str ch)
  (doc 'type "(-> String Char (Option Integer))")
  (doc 'description "Find first occurrence of character in string")
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (+ i 1))]))))

;;; string-index-right : String × Char → Integer | #f
;;; Find last occurrence of character in string.
(define (string-index-right str ch)
  (let ([len (string-length str)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) #f]
             [(char=? (string-ref str i) ch) i]
             [else (loop (- i 1))]))))

;;; string-split-flex : String × (String | Char) → (List String)
;;; More flexible string-split that accepts either a char or string delimiter.
;;; Use this when you need string delimiters; use string-split for char delimiters.
(define (string-split-flex str delimiter)
  (let ([delimiter (if (char? delimiter) (string delimiter) delimiter)])
       (cond
        [(= (string-length delimiter) 0)
         ;; Split into individual characters
         (let loop ([i 0]
                    [result '()])
              (if (>= i (string-length str))
                  (reverse result)
                  (loop (+ i 1)
                        (cons (substring str i (+ i 1)) result))))]
        [else
         ;; Split by delimiter
         (let ([delim-len (string-length delimiter)])
              (let loop ([start 0]
                         [result '()])
                   (let ([idx (string-index-of (substring str start (string-length str)) delimiter)])
                        (if idx
                            (let ([pos (+ start idx)])
                                 (loop (+ pos delim-len)
                                       (cons (substring str start pos) result)))
                            (reverse (cons (substring str start (string-length str)) result))))))])))

;;; string-split : String × (String | Char) → (List String)
;;; Redefine string-split to use the flexible version in shell code.
(define string-split string-split-flex)

;;; string-split-lines : String → (List String)
;;; Split string into lines, handling \n, \r\n, and \r.
(define (string-split-lines str)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        ;; Handle CRLF
        [(and (char=? (car chars) #\return)
              (not (null? (cdr chars)))
              (char=? (cadr chars) #\newline))
         (loop (cddr chars)
               '()
               (cons (list->string (reverse current)) result))]
        ;; Handle LF or CR
        [(or (char=? (car chars) #\newline)
             (char=? (car chars) #\return))
         (loop (cdr chars)
               '()
               (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

;;; string-replace-first : String × String × String → String
;;; Replace only the first occurrence of old with new.
(define (string-replace-first str old new)
  (let ([idx (string-index-of str old)])
       (if idx
           (string-append (substring str 0 idx)
                          new
                          (substring str (+ idx (string-length old)) (string-length str)))
           str)))

(printf "  String utilities loaded (via prelude.ss)\n")
