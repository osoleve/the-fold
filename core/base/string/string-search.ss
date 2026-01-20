(doc 'module 'string-search)
(doc 'description "Find, match, and replace operations for strings. Includes prefix/suffix checks, substring search, and edit distance.")
(doc 'layer 'core)

(doc 'section 'prefix-suffix-matching)

(define (string-starts-with? str prefix)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if string starts with prefix.")
  (doc 'export #t)
  (let ([str-len (string-length str)]
        [pre-len (string-length prefix)])
       (and (<= pre-len str-len)
            (string=? (substring str 0 pre-len) prefix))))

(doc string-prefix? 'type (-> String String Boolean))
(doc string-prefix? 'description "Alias for string-starts-with? (common naming convention).")
(doc string-prefix? 'export #t)
(define string-prefix? string-starts-with?)

(define (string-ends-with? str suffix)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if string ends with suffix.")
  (doc 'export #t)
  (let ([str-len (string-length str)]
        [suf-len (string-length suffix)])
       (and (<= suf-len str-len)
            (string=? (substring str (- str-len suf-len) str-len) suffix))))

(doc 'section 'substring-search)

(define (string-contains? haystack needle)
  (doc 'type (-> String String Boolean))
  (doc 'description "Check if haystack contains needle as substring.")
  (doc 'export #t)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) #t]
        [(> n-len h-len) #f]
        [else
         (let loop ([i 0])
              (cond
               [(> (+ i n-len) h-len) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) #t]
               [else (loop (+ i 1))]))])))

(define (string-index-of haystack needle)
  (doc 'type (-> String String (Maybe Integer)))
  (doc 'description "Find first occurrence of needle in haystack. Returns index or #f if not found.")
  (doc 'export #t)
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

(define (string-last-index-of haystack needle)
  (doc 'type (-> String String (Maybe Integer)))
  (doc 'description "Find last occurrence of needle in haystack. Returns index or #f if not found.")
  (doc 'export #t)
  (let ([h-len (string-length haystack)]
        [n-len (string-length needle)])
       (cond
        [(= n-len 0) h-len]
        [(> n-len h-len) #f]
        [else
         (let loop ([i (- h-len n-len)])
              (cond
               [(< i 0) #f]
               [(string=? (substring haystack i (+ i n-len)) needle) i]
               [else (loop (- i 1))]))])))

(doc 'section 'replace)

(define (string-replace str old new)
  (doc 'type (-> String String String String))
  (doc 'description "Replace all occurrences of old with new.")
  (doc 'export #t)
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

(doc 'section 'fuzzy-matching)

(define (edit-distance s1 s2)
  (doc 'type (-> String String Nat))
  (doc 'description "Compute the Levenshtein distance between two strings.")
  (doc 'export #t)
  (let ([len1 (string-length s1)]
        [len2 (string-length s2)])
       (cond
        [(= len1 0) len2]
        [(= len2 0) len1]
        [else
         (let ([matrix (make-vector (* (+ len1 1) (+ len2 1)) 0)])
              ;; Initialize first row and column
              (do ([i 0 (+ i 1)]) ((> i len1))
                  (vector-set! matrix (* i (+ len2 1)) i))
              (do ([j 0 (+ j 1)]) ((> j len2))
                  (vector-set! matrix j j))
              ;; Fill matrix
              (do ([i 1 (+ i 1)])
                  ((> i len1))
                  (do ([j 1 (+ j 1)])
                      ((> j len2))
                      (let* ([cost (if (char=? (string-ref s1 (- i 1))
                                               (string-ref s2 (- j 1)))
                                       0 1)]
                             [del (+ 1 (vector-ref matrix (+ (* (- i 1) (+ len2 1)) j)))]
                             [ins (+ 1 (vector-ref matrix (+ (* i (+ len2 1)) (- j 1))))]
                             [sub (+ cost (vector-ref matrix (+ (* (- i 1) (+ len2 1)) (- j 1))))])
                            (vector-set! matrix (+ (* i (+ len2 1)) j) (min del ins sub)))))
              ;; Return final distance
              (vector-ref matrix (+ (* len1 (+ len2 1)) len2)))])))
