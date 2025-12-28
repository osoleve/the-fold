;;; playpen/security-utils.ss — Security Validation Utilities
;;;
;;; Common security functions for input validation, sanitization,
;;; and protection against common vulnerabilities.

;;; ============================================================
;;; String Utility Dependencies
;;; ============================================================

;;; string-trim : String → String
(define (string-trim str)
  (string-trim-left (string-trim-right str)))

;;; string-trim-left : String → String
(define (string-trim-left str)
  (let ([len (string-length str)])
    (let loop ([i 0])
      (cond
        [(>= i len) ""]
        [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
        [else (substring str i len)]))))

;;; string-trim-right : String → String
(define (string-trim-right str)
  (let ([len (string-length str)])
    (let loop ([i (- len 1)])
      (cond
        [(< i 0) ""]
        [(char-whitespace? (string-ref str i)) (loop (- i 1))]
        [else (substring str 0 (+ i 1))]))))

;;; string-empty? : String → Boolean
(define (string-empty? str)
  (= (string-length str) 0))

;;; char-whitespace? : Char → Boolean
(define (char-whitespace? c)
  (or (char=? c #\space)
      (char=? c #\tab)
      (char=? c #\newline)
      (char=? c #\return)))

;;; string-contains? : String × String → Boolean
(define (string-contains? haystack needle)
  (and (string? haystack)
       (string? needle)
       (let ([haystack-len (string-length haystack)]
             [needle-len (string-length needle)])
         (if (> needle-len haystack-len)
             #f
             (let loop ([i 0])
               (cond
                 [(> (+ i needle-len) haystack-len) #f]
                 [(string=? (substring haystack i (+ i needle-len)) needle) #t]
                 [else (loop (+ i 1))]))))))

;;; string-map : (Char → Char) × String → String
(define (string-map proc str)
  (list->string (map proc (string->list str))))

;;; string-replace : String × String × String → String
(define (string-replace str old new)
  (if (or (not (string? str)) (not (string? old)) (not (string? new)))
      str
      (let ([old-len (string-length old)]
            [str-len (string-length str)])
        (if (zero? old-len)
            str
            (let loop ([i 0] [result '()])
              (cond
                [(> i (- str-len old-len))
                 (string-append (list->string (reverse result))
                               (substring str i str-len))]
                [(string=? (substring str i (+ i old-len)) old)
                 (loop (+ i old-len) (append (reverse (string->list new)) result))]
                [else
                 (loop (+ i 1) (cons (string-ref str i) result))]))))))

;;; string-split : String × Char → (List String)
(define (string-split str delim)
  (if (not (string? str))
      '()
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
                 result)]))))

;;; string-join : (List String) × String → String
(define (string-join strings sep)
  (if (not (list? strings))
      ""
      (let loop ([remaining strings] [result ""] [first? #t])
        (cond
          [(null? remaining) result]
          [(not (string? (car remaining))) (loop (cdr remaining) result first?)]
          [first? (loop (cdr remaining) (car remaining) #f)]
          [else (loop (cdr remaining) 
                     (string-append result sep (car remaining))
                     #f)]))))

;;; string-index-of : String × String → Nat | #f
(define (string-index-of haystack needle)
  (if (or (not (string? haystack)) (not (string? needle)))
      #f
      (let ([haystack-len (string-length haystack)]
            [needle-len (string-length needle)])
        (if (> needle-len haystack-len)
            #f
            (let loop ([i 0])
              (cond
                [(> (+ i needle-len) haystack-len) #f]
                [(string=? (substring haystack i (+ i needle-len)) needle) i]
                [else (loop (+ i 1))]))))))

;;; ============================================================
;;; Input Validation Functions
;;; ============================================================

;;; valid-string? : Any × Nat × Nat → Bool
;;; Check if value is a valid string within length bounds
(define (valid-string? str min-len max-len)
  (and (string? str)
       (>= (string-length str) min-len)
       (<= (string-length str) max-len)))

;;; valid-integer? : Any × Int × Int → Bool
;;; Check if value is a valid integer within range
(define (valid-integer? val min-val max-val)
  (and (integer? val)
       (>= val min-val)
       (<= val max-val)))

;;; valid-symbol? : Any × (List Symbol) → Bool
;;; Check if value is a valid symbol from allowed list
(define (valid-symbol? sym allowed-symbols)
  (and (symbol? sym)
       (memq sym allowed-symbols)))

;;; ============================================================
;;; String Sanitization
;;; ============================================================

;;; sanitize-path : String → String | #f
;;; Remove path traversal sequences and validate path format
(define (sanitize-path path-str)
  (if (not (string? path-str))
      #f
      (let* ([cleaned (string-trim path-str)]
             [no-traversal (string-replace 
                            (string-replace cleaned ".." "")
                            "~" "")]
             [no-slashes (string-replace no-traversal "/" "")]
             [no-backslashes (string-replace no-slashes "\\" "")])
        (if (string-empty? no-backslashes)
            #f
            no-backslashes))))

;;; sanitize-filename : String → String | #f
;;; Remove dangerous characters from filenames
(define (sanitize-filename filename)
  (if (not (string? filename))
      #f
      (let* ([cleaned (string-trim filename)]
             [no-control (string-map 
                         (lambda (c)
                           (if (or (< (char->integer c) 32)
                                   (> (char->integer c) 126))
                               #\_
                               c))
                         cleaned)]
             [no-special (string-replace 
                         (string-replace 
                          (string-replace no-control "<" "_")
                          ">" "_")
                         "&" "_")])
        (if (or (string-empty? no-special)
                (string-contains? no-special ".."))
            #f
            no-special))))

;;; escape-html : String → String
;;; Basic HTML entity escaping
(define (escape-html str)
  (if (not (string? str))
      ""
      (string-replace
       (string-replace
        (string-replace str "&" "&amp;")
        "<" "&lt;")
       ">" "&gt;")))

;;; ============================================================
;;; Bounds Checking
;;; ============================================================

;;; safe-string-length : String × Nat → Nat
;;; Return string length or max value, whichever is smaller
(define (safe-string-length str max-len)
  (if (not (string? str))
      0
      (min (string-length str) max-len)))

;;; safe-list-take : (List Any) × Nat → (List Any)
;;; Safely take up to n elements from list
(define (safe-list-take lst n)
  (if (not (list? lst))
      '()
      (let loop ([remaining lst] [count n] [result '()])
        (cond
          [(or (null? remaining) (<= count 0)) (reverse result)]
          [else (loop (cdr remaining) (- count 1) (cons (car remaining) result))]))))

;;; safe-vector-ref : Vector × Nat × Any → Any
;;; Safe vector access with default value
(define (safe-vector-ref vec idx default)
  (if (and (vector? vec) 
           (valid-integer? idx 0 (- (vector-length vec) 1)))
      (vector-ref vec idx)
      default))

;;; ============================================================
;;; Type Validation
;;; ============================================================

;;; validate-block-content : Any → Bool
;;; Validate block content before processing
(define (validate-block-content content)
  (and (string? content)
       (valid-string? content 1 10000)  ; Max 10KB content
       (not (string-contains? content "\000"))))  ; No null bytes

;;; validate-point : Any × Nat × Nat → Bool
;;; Validate point coordinates within canvas bounds
(define (validate-point pt max-x max-y)
  (and (pair? pt)
       (valid-integer? (car pt) 0 max-x)
       (valid-integer? (cdr pt) 0 max-y)))

;;; ============================================================
;;; Security Logging
;;; ============================================================

;;; log-security-event : Symbol × String → Void
;;; Log security events for monitoring
(define (log-security-event event-type details)
  (printf "[SECURITY] ~a: ~a\n" event-type details))

;;; log-invalid-input : String × Any → Void
;;; Log invalid input attempts
(define (log-invalid-input context input)
  (log-security-event 'invalid-input 
                      (format "~a: ~s" context input)))

;;; ============================================================
;;; Safe String Operations
;;; ============================================================

;;; safe-string-replace : String × String × String × Nat → String
;;; Safe string replace with length limits
(define (safe-string-replace str old new max-result-len)
  (if (not (and (string? str) (string? old) (string? new)))
      str
      (let ([result (string-replace str old new)])
        (if (> (string-length result) max-result-len)
            (begin
              (log-security-event 'string-truncated
                                 (format "Result too long: ~a chars" (string-length result)))
              (substring result 0 max-result-len))
            result))))

;;; safe-string-split : String × Char × Nat → (List String)
;;; Safe string split with result size limit
(define (safe-string-split str delim max-parts)
  (if (not (string? str))
      '()
      (let ([parts (string-split str delim)])
        (if (> (length parts) max-parts)
            (begin
              (log-security-event 'split-truncated
                                 (format "Too many parts: ~a" (length parts)))
              (safe-list-take parts max-parts))
            parts))))

;;; ============================================================
;;; Configuration Constants
;;; ============================================================

;;; Maximum allowed values for common operations
(define MAX_STRING_LENGTH 10000)
(define MAX_LIST_LENGTH 1000)
(define MAX_FILENAME_LENGTH 255)
(define MAX_PATH_LENGTH 4096)
(define MAX_RECURSION_DEPTH 100)

;;; ============================================================
;;; Safe Template Rendering
;;; ============================================================

;;; safe-template-render : String × (List (Pair String String)) → String
;;; Safe template rendering with input validation
(define (safe-template-render template bindings)
  (if (not (valid-string? template 1 MAX_STRING_LENGTH))
      (begin
        (log-invalid-input "template" template)
        "")
      (let loop ([tmpl template] [remaining bindings] [depth 0])
        (cond
          [(null? remaining) tmpl]
          [(> depth MAX_RECURSION_DEPTH)
           (log-security-event 'template-recursion-limit "Max depth exceeded")
           tmpl]
          [else
           (let* ([binding (car remaining)]
                  [key (car binding)]
                  [value (cdr binding)]
                  [placeholder (string-append "{{" key "}}")])
             (if (and (valid-string? key 1 100)
                      (valid-string? value 0 MAX_STRING_LENGTH))
                 (loop (safe-string-replace tmpl placeholder value MAX_STRING_LENGTH)
                       (cdr remaining)
                       (+ depth 1))
                 (begin
                   (log-invalid-input "template-binding" (cons key value))
                   (loop tmpl (cdr remaining) (+ depth 1)))))]))))

;;; Security utilities are now available for use by other files
;;; No explicit export needed - all functions are globally available