(doc 'module 'string-core)
(doc 'description "Foundational string utilities: splitting, joining, trimming, predicates. Pure functions with no dependencies on other string modules.")
(doc 'layer 'core)

(doc 'section 'character-predicates)

(define (whitespace? ch)
  (doc 'type (-> Char Boolean))
  (doc 'description "Check if character is whitespace.")
  (doc 'export #t)
  (or (char=? ch #\space)
      (char=? ch (integer->char 9))   ; tab
      (char=? ch (integer->char 10))  ; newline
      (char=? ch (integer->char 13)))) ; carriage return

(doc 'section 'basic-operations)

(define (string-join strs sep)
  (doc 'type (-> (List String) String String))
  (doc 'description "Join strings with separator.")
  (doc 'export #t)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

(define (string-split str delim)
  (doc 'type (-> String Char (List String)))
  (doc 'description "Split string by single-character delimiter.")
  (doc 'export #t)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
       (cond
        [(null? chars)
         (reverse (cons (list->string (reverse current)) result))]
        [(char=? (car chars) delim)
         (loop (cdr chars)
               '()
               (cons (list->string (reverse current)) result))]
        [else
         (loop (cdr chars)
               (cons (car chars) current)
               result)])))

(define (string-reverse str)
  (doc 'type (-> String String))
  (doc 'description "Reverse a string.")
  (doc 'export #t)
  (list->string (reverse (string->list str))))

(doc 'section 'trimming)

(define (string-trim-left str)
  (doc 'type (-> String String))
  (doc 'description "Remove leading whitespace.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) ""]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else (substring str i len)]))))

(define (string-trim-right str)
  (doc 'type (-> String String))
  (doc 'description "Remove trailing whitespace.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) ""]
             [(whitespace? (string-ref str i)) (loop (- i 1))]
             [else (substring str 0 (+ i 1))]))))

(define (string-trim str)
  (doc 'type (-> String String))
  (doc 'description "Remove leading and trailing whitespace.")
  (doc 'export #t)
  (string-trim-left (string-trim-right str)))

(doc 'section 'predicates)

(define (string-empty? str)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if string is empty.")
  (doc 'export #t)
  (= (string-length str) 0))

(define (string-blank? str)
  (doc 'type (-> String Boolean))
  (doc 'description "Check if string is empty or contains only whitespace.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

(define (string-all-match? str predicate)
  (doc 'type (-> String (-> Char Boolean) Boolean))
  (doc 'description "Check if all characters in string satisfy predicate.")
  (doc 'export #t)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(predicate (string-ref str i)) (loop (+ i 1))]
             [else #f]))))
