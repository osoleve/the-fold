;;; core/query/patterns-parse.ss --- Inline Metadata Tag Parser
;;;
;;; Extracts @-tags from text for queryable metadata.
;;;
;;; Syntax:
;;;   @key:value  - key-value pair
;;;   @key        - standalone flag (value = #t)
;;;   @key:multi:part - hierarchical tags
;;;
;;; This is Shell-tier code: pure text processing, defensive parsing.
;;; No dependencies on Core or other Shell modules.
;;;
;;; Usage:
;;;   (extract-tags "Hello @status:complete world @todo")
;;;   => ((status . "complete") (todo . #t))
;;;
;;;   (extract-tag-positions "Hello @status:complete")
;;;   => ((status "complete" 6 22))  ; start and end positions

;;; ====
;;; Tag Pattern Matching
;;; ====

;;; A tag is: @key or @key:value
;;; - Key starts with lowercase letter [a-z]
;;; - Key contains [a-z0-9-]
;;; - Value (optional) contains [a-z0-9_./-]
;;; - Must follow whitespace or line start

;;; char-key-start? : Char -> Boolean
;;; True if char can start a tag key.
(define (char-key-start? c)
  (and (char>=? c #\a) (char<=? c #\z)))

;;; char-key? : Char -> Boolean
;;; True if char can appear in a tag key.
(define (char-key? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\0) (char<=? c #\9))
      (char=? c #\-)))

;;; char-value? : Char -> Boolean
;;; True if char can appear in a tag value.
;;; Allows: alphanumeric, hyphen, dot, slash, underscore, colon (for hierarchical tags)
(define (char-value? c)
  (or (and (char>=? c #\a) (char<=? c #\z))
      (and (char>=? c #\A) (char<=? c #\Z))
      (and (char>=? c #\0) (char<=? c #\9))
      (char=? c #\-)
      (char=? c #\.)
      (char=? c #\/)
      (char=? c #\_)
      (char=? c #\:)))

;;; char-whitespace? : Char -> Boolean
;;; True if char is whitespace.
(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\newline)
      (char=? c #\tab)
      (char=? c #\return)))

;;; ====
;;; Tag Extraction (Main API)
;;; ====

;;; extract-tags : String -> (Listof (Pair Symbol (U String #t)))
;;; Extract all @-tags from text.
;;; Returns alist where:
;;;   - Key is a symbol (tag name)
;;;   - Value is string (if @key:value) or #t (if @key alone)
;;;
;;; Example:
;;;   (extract-tags "Hello @status:complete @todo")
;;;   => ((status . "complete") (todo . #t))
(define (extract-tags text)
  (let ([positions (extract-tag-positions text)])
       (map (lambda (pos)
                    (let ([key (car pos)]
                          [val (cadr pos)])
                         (cons key (or val #t))))
            positions)))

;;; extract-tag-positions : String -> (Listof (List Symbol (U String #f) Nat Nat))
;;; Extract tags with their positions.
;;; Returns list of (key value start-pos end-pos)
;;;
;;; Example:
;;;   (extract-tag-positions "Hi @todo:fix this")
;;;   => ((todo "fix" 3 12))
(define (extract-tag-positions text)
  (let ([len (string-length text)])
       (let loop ([i 0] [results '()])
            (if (>= i len)
                (reverse results)
                (if (and (char=? (string-ref text i) #\@)
                         (valid-tag-start? text i))
                    (let ([tag-info (parse-tag-at text i)])
                         (if tag-info
                             (loop (cadddr tag-info)
                                   (cons tag-info results))
                             (loop (+ i 1) results)))
                    (loop (+ i 1) results))))))

;;; valid-tag-start? : String x Nat -> Boolean
;;; True if @ at position i is a valid tag start.
;;; Must be at line start or preceded by whitespace.
(define (valid-tag-start? text i)
  (or (= i 0)
      (let ([prev-char (string-ref text (- i 1))])
           (char-whitespace? prev-char))))

;;; parse-tag-at : String x Nat -> (U (List Symbol (U String #f) Nat Nat) #f)
;;; Parse a tag starting at position i (the @).
;;; Returns (key value start end) or #f if not a valid tag.
(define (parse-tag-at text i)
  (let ([len (string-length text)]
        [start i])
       ;; Skip the @
       (let ([j (+ i 1)])
            (if (>= j len)
                #f
                (let ([first-char (string-ref text j)])
                     (if (not (char-key-start? first-char))
                         #f
                         ;; Parse the key
                         (let key-loop ([k j])
                              (if (or (>= k len)
                                      (not (char-key? (string-ref text k))))
                                  ;; End of key
                                  (if (= k j)
                                      #f  ; Empty key
                                      (let ([key-str (substring text j k)]
                                            [key-sym (string->symbol
                                                      (string-downcase
                                                       (substring text j k)))])
                                           ;; Check for value
                                           (if (and (< k len)
                                                    (char=? (string-ref text k) #\:))
                                               ;; Parse value
                                               (let value-loop ([v (+ k 1)])
                                                    (if (or (>= v len)
                                                            (not (char-value? (string-ref text v))))
                                                        ;; End of value
                                                        (if (= v (+ k 1))
                                                            ;; Empty value - treat as flag
                                                            (list key-sym #f start k)
                                                            (list key-sym
                                                                  (substring text (+ k 1) v)
                                                                  start v))
                                                        (value-loop (+ v 1))))
                                               ;; No value - it's a flag
                                               (list key-sym #f start k))))
                                  (key-loop (+ k 1))))))))))

;;; ====
;;; String Utilities
;;; ====

;;; string-downcase : String -> String
;;; Convert string to lowercase.
(define (string-downcase s)
  (list->string
   (map char-downcase (string->list s))))

;;; ====
;;; Tag Validation
;;; ====

;;; valid-tag-key? : String -> Boolean
;;; True if string is a valid tag key.
(define (valid-tag-key? s)
  (and (> (string-length s) 0)
       (char-key-start? (string-ref s 0))
       (let loop ([i 1])
            (if (>= i (string-length s))
                #t
                (and (char-key? (string-ref s i))
                     (loop (+ i 1)))))))

;;; valid-tag-value? : String -> Boolean
;;; True if string is a valid tag value.
(define (valid-tag-value? s)
  (and (> (string-length s) 0)
       (let loop ([i 0])
            (if (>= i (string-length s))
                #t
                (and (char-value? (string-ref s i))
                     (loop (+ i 1)))))))

;;; ====
;;; Tag Formatting
;;; ====

;;; format-tag : Symbol x (U String #t) -> String
;;; Format a tag as @key or @key:value.
(define (format-tag key value)
  (if (eq? value #t)
      (string-append "@" (symbol->string key))
      (string-append "@" (symbol->string key) ":" value)))

;;; tags->string : (Listof (Pair Symbol (U String #t))) -> String
;;; Format multiple tags as space-separated string.
(define (tags->string tags)
  (if (null? tags)
      ""
      (let loop ([ts tags] [acc ""])
           (if (null? ts)
               acc
               (loop (cdr ts)
                     (string-append acc
                                    (if (string=? acc "") "" " ")
                                    (format-tag (caar ts) (cdar ts))))))))

;;; ====
;;; Tag Filtering and Lookup
;;; ====

;;; has-tag? : (Listof (Pair Symbol Any)) x Symbol -> Boolean
;;; True if tags contain the given key.
(define (has-tag? tags key)
  (and (assq key tags) #t))

;;; get-tag : (Listof (Pair Symbol Any)) x Symbol -> (U String #t #f)
;;; Get value for a tag key, or #f if not present.
(define (get-tag tags key)
  (let ([pair (assq key tags)])
       (if pair (cdr pair) #f)))

;;; filter-tags-by-key : (Listof (Pair Symbol Any)) x (Symbol -> Boolean) -> List
;;; Filter tags by a predicate on keys.
(define (filter-tags-by-key tags pred)
  (filter (lambda (pair) (pred (car pair))) tags))

;;; filter : (α → Boolean) × (List α) → (List α)
;;; Helper: filter
(define (filter pred lst)
  (cond
   [(null? lst) '()]
   [(pred (car lst)) (cons (car lst) (filter pred (cdr lst)))]
   [else (filter pred (cdr lst))]))

;;; ====
;;; Defensive Parsing (Security)
;;; ====

;;; sanitize-tag-value : String -> String
;;; Remove potentially dangerous patterns from tag values.
;;; - No shell metacharacters: $ ` & | ; && ||
;;; - No path traversal: ../
;;; - No control characters
(define (sanitize-tag-value s)
  ;; First, check for path traversal patterns (before filtering chars)
  (if (has-path-traversal? s)
      ""  ; Reject completely if path traversal detected
      ;; Then, remove dangerous characters
      (list->string
       (filter (lambda (c)
                       (and (char>=? c #\space)  ; No control chars
                            (char<=? c #\~)       ; Printable ASCII
                            (not (memv c '(#\$ #\` #\& #\| #\; #\\)))))
               (string->list s)))))

;;; has-path-traversal? : String -> Boolean
;;; True if string contains path traversal sequences.
;;; Blocks: absolute paths (/...), and '..' anywhere in path context
(define (has-path-traversal? s)
  (let ([len (string-length s)])
       (cond
        [(= len 0) #f]
        ;; Block absolute paths starting with /
        [(char=? (string-ref s 0) #\/) #t]
        ;; Check for '..' sequences
        [else
         (let loop ([i 0])
              (cond
               [(>= i (- len 1)) #f]  ; Not enough chars left for ..
               [(and (char=? (string-ref s i) #\.)
                     (char=? (string-ref s (+ i 1)) #\.))
                ;; Found ".." - dangerous if:
                ;; - at start of string (../foo)
                ;; - at end of string (foo/..)
                ;; - followed by / or \ (foo/../bar)
                ;; - preceded by / (foo/..)
                (let ([at-start (= i 0)]
                      [at-end (>= (+ i 2) len)]
                      [after-slash (and (> i 0) (char=? (string-ref s (- i 1)) #\/))]
                      [before-slash (and (< (+ i 2) len)
                                         (or (char=? (string-ref s (+ i 2)) #\/)
                                             (char=? (string-ref s (+ i 2)) #\\)))])
                     (if (or at-start at-end after-slash before-slash)
                         #t  ; Path traversal detected
                         (loop (+ i 1))))]
               [else (loop (+ i 1))]))])))

;;; safe-extract-tags : String -> (Listof (Pair Symbol (U String #t)))
;;; Extract tags with sanitized values.
(define (safe-extract-tags text)
  (map (lambda (pair)
               (let ([key (car pair)]
                     [val (cdr pair)])
                    (cons key (if (string? val)
                                  (sanitize-tag-value val)
                                  val))))
       (extract-tags text)))
