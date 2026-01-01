;;; string-utils.ss — String Utility Primitives for The Fold
;;;
;;; Essential string operations that should be in every standard library.
;;; These are foundational primitives used by higher-level tools.
;;;
;;; TIER ASSIGNMENT:
;;;   Tier 5: Basic string operations (split, join, trim)
;;;   Tier 6: Search and comparison operations

;;; ============================================================
;;; String Searching (Tier 6)
;;; ============================================================

;;; string-contains? : String String → Boolean
;;; Check if haystack contains needle as substring.
;;; Returns #t if found, #f otherwise.
(define (string-contains? haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) #t]  ; empty string is always contained
        [(> n-len h-len) #f]
        [else
         (let loop ([i 0])
              (cond
               [(> (+ i n-len) h-len) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) #t]
               [else (loop (+ i 1))]))])))

;;; string-starts-with? : String String → Boolean
;;; Check if string starts with given prefix.
(define (string-starts-with? str prefix)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (<= pre-len str-len)
            (string=? (substring str 0 pre-len) prefix))))

;;; string-ends-with? : String String → Boolean
;;; Check if string ends with given suffix.
(define (string-ends-with? str suffix)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (<= suf-len str-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

;;; string-index-of : String String → (Maybe Integer)
;;; Find first occurrence of needle in haystack.
;;; Returns index or #f if not found.
(define (string-index-of haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) 0]
        [(> n-len h-len) #f]
        [else
         (let loop ([i 0])
              (cond
               [(> (+ i n-len) h-len) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) i]
               [else (loop (+ i 1))]))])))

;;; string-last-index-of : String String → (Maybe Integer)
;;; Find last occurrence of needle in haystack.
;;; Returns index or #f if not found.
(define (string-last-index-of haystack needle)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) h-len]
        [(> n-len h-len) #f]
        [else
         (let loop ([i (- h-len n-len)]
                    [result #f])
              (cond
               [(< i 0) result]
               [(string=? (substring haystack i (+ i n-len)) needle) i]
               [else (loop (- i 1) result)]))])))

;;; ============================================================
;;; String Splitting and Joining (Tier 5)
;;; ============================================================

;;; string-split : String (String | Char) → (List String)
;;; Split string by delimiter (accepts either string or character).
(define (string-split str delimiter)
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
                            (let ([end (+ start idx)])
                                 (loop (+ end delim-len)
                                       (cons (substring str start end) result)))
                            (reverse (cons (substring str start (string-length str)) result))))))])))

;;; string-join : (List String) String → String
;;; Join list of strings with separator.
(define (string-join strings separator)
  (cond
   [(null? strings) ""]
   [(null? (cdr strings)) (car strings)]
   [else
    (let loop ([strs (cdr strings)]
               [result (car strings)])
         (if (null? strs)
             result
             (loop (cdr strs)
                   (string-append result separator (car strs)))))]))

;;; ============================================================
;;; String Trimming (Tier 5)
;;; ============================================================

;;; whitespace? : Char → Boolean
(define (whitespace? ch)
  (or (char=? ch #\space)
      (char=? ch #\tab)
      (char=? ch #\newline)
      (char=? ch #\return)))

;;; string-trim-left : String → String
(define (string-trim-left str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) ""]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else (substring str i len)]))))

;;; string-trim-right : String → String
(define (string-trim-right str)
  (let ([len (string-length str)])
       (let loop ([i (- len 1)])
            (cond
             [(< i 0) ""]
             [(whitespace? (string-ref str i)) (loop (- i 1))]
             [else (substring str 0 (+ i 1))]))))

;;; string-trim : String → String
(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;;; ============================================================
;;; String Transformation (Tier 5)
;;; ============================================================

;;; string-replace : String String String → String
(define (string-replace str old new)
  (let ([old-len (string-length old)])
       (cond
        [(= old-len 0) str]
        [else
         (let loop ([start 0]
                    [result ""])
              (let ([idx (string-index-of (substring str start (string-length str)) old)])
                   (if idx
                       (let ([pos (+ start idx)])
                            (loop (+ pos old-len)
                                  (string-append result
                                                 (substring str start pos)
                                                 new)))
                       (string-append result (substring str start (string-length str))))))])))

;;; string-reverse : String → String
(define (string-reverse str)
  (list->string (reverse (string->list str))))

;;; ============================================================
;;; String Predicates (Tier 6)
;;; ============================================================

;;; string-empty? : String → Boolean
(define (string-empty? str)
  (= (string-length str) 0))

;;; string-blank? : String → Boolean
(define (string-blank? str)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(whitespace? (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

;;; string-all-match? : String (Char → Boolean) → Boolean
(define (string-all-match? str predicate)
  (let ([len (string-length str)])
       (let loop ([i 0])
            (cond
             [(>= i len) #t]
             [(predicate (string-ref str i)) (loop (+ i 1))]
             [else #f]))))

;;; ============================================================
;;; String Case Conversion (Tier 5)
;;; ============================================================

;;; string-upcase : String → String
;;; Convert string to uppercase.
(define (string-upcase str)
  (list->string (map char-upcase (string->list str))))

;;; string-downcase : String → String
;;; Convert string to lowercase.
(define (string-downcase str)
  (list->string (map char-downcase (string->list str))))

;;; ============================================================
;;; String Padding (Tier 5)
;;; ============================================================

;;; string-pad-left : String Integer Char → String
(define (string-pad-left str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append (make-string (- width len) pad-char) str))))

;;; string-pad-right : String Integer Char → String
(define (string-pad-right str width pad-char)
  (let ([len (string-length str)])
       (if (>= len width)
           str
           (string-append str (make-string (- width len) pad-char)))))

(printf "✓ String utilities loaded\n")
