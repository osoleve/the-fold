;;; core/base/string/string-core.ss — Core String Operations
;;;
;;; Foundational string utilities: splitting, joining, trimming, predicates.
;;; These are pure functions with no dependencies on other string modules.

;;; ====
;;; Character Predicates
;;; ====

;;; whitespace? : Char → Boolean
;;; Check if character is whitespace.
(define (whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch (integer->char 9))   ; tab
      (char=? ch (integer->char 10))  ; newline
      (char=? ch (integer->char 13)))) ; carriage return

;;; ====
;;; Basic Operations
;;; ====

;;; string-join : (List String) × String → String
;;; Join strings with separator.
(define (string-join strs sep)
  (if (null? strs)
      ""
      (fold-left (lambda (acc s) (string-append acc sep s))
                 (car strs)
                 (cdr strs))))

;;; string-split : String × Char → (List String)
;;; Split string by single-character delimiter.
(define (string-split str delim)
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

;;; string-reverse : String → String
;;; Reverse a string.
(define (string-reverse str)
  (list->string (reverse (string->list str))))

;;; ====
;;; Trimming
;;; ====

;;; string-trim-left : String → String
;;; Remove leading whitespace.
(define (string-trim-left str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) ""]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else (substring str i len)]))))

;;; string-trim-right : String → String
;;; Remove trailing whitespace.
(define (string-trim-right str)
  (let ([len (string-length str)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) ""]
             [(whitespace? (string-ref str i)) (loop (- i 1))]
             [else (substring str 0 (+ i 1))]))))

;;; string-trim : String → String
;;; Remove leading and trailing whitespace.
(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;;; ====
;;; Predicates
;;; ====

;;; string-empty? : String → Boolean
;;; Check if string is empty.
(define (string-empty? str)
  (= (string-length str) 0))

;;; string-blank? : String → Boolean
;;; Check if string is empty or contains only whitespace.
(define (string-blank? str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

;;; string-all-match? : String × (Char → Boolean) → Boolean
;;; Check if all characters in string satisfy predicate.
(define (string-all-match? str predicate)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(predicate (string-ref str i)) (loop (+ i 1))]
             [else #f]))))
