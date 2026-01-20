(load "core/base/prelude.ss")

(doc 'module 'error-fmt)
(doc 'description "Enhanced Error Formatter - Improved error messages with context, colors, and suggestions")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'dependencies '(core/base/prelude))

(doc 'note "Fixes ~s placeholder bugs in exploration scripts by providing proper error message formatting")
(doc 'note "String utilities provided by core/prelude.ss: string-contains?, string-join, string-replace")
(doc 'note "Unique to this module: string-replace-once, string-find")

(doc 'section 'configuration)

(define *use-colors* #t)
(doc *use-colors* 'type 'Bool)
(doc *use-colors* 'description "Enable/disable ANSI color codes in output")

(define *show-suggestions* #t)
(doc *show-suggestions* 'type 'Bool)
(doc *show-suggestions* 'description "Enable/disable contextual suggestions")

(define *max-context-lines* 3)
(doc *max-context-lines* 'type 'Nat)
(doc *max-context-lines* 'description "Number of context lines to show around errors")

(doc 'section 'ansi-colors)

(define *color-reset* "\x1b;[0m")
(define *color-red* "\x1b;[31m")
(define *color-yellow* "\x1b;[33m")
(define *color-blue* "\x1b;[34m")
(define *color-gray* "\x1b;[90m")
(define *color-bold* "\x1b;[1m")

(doc 'section 'error-levels)

(define-record-type error-level
  (fields
   name
   color
   symbol))

(doc error-level 'description "Error severity level with display attributes")
(doc error-level 'fields '((name . Symbol) (color . String) (symbol . String)))

(define *level-error*
  (make-error-level 'error *color-red* "ERROR"))

(define *level-warning*
  (make-error-level 'warning *color-yellow* "WARN"))

(define *level-info*
  (make-error-level 'info *color-blue* "INFO"))

(doc 'section 'main-api)

(doc format-error 'export #t)
(define (format-error condition)
  (doc 'type '(-> Condition String))
  (doc 'description "Format a condition object as a readable error message")
  (guard (e [else
             (format-error-simple
              (format "Error formatting error: ~a" e))])
         (let* ([msg (condition-message condition)]
                [who (if (who-condition? condition)
                         (condition-who condition)
                         #f)]
                [irritants (if (irritants-condition? condition)
                               (condition-irritants condition)
                               '())])
               (format-error-full
                *level-error*
                msg
                who
                irritants
                #f #f #f))))

(doc format-error-simple 'export #t)
(define (format-error-simple msg)
  (doc 'type '(-> String String))
  (doc 'description "Format a simple error message")
  (format-error-full *level-error* msg #f '() #f #f #f))

(doc format-error-with-location 'export #t)
(define (format-error-with-location msg file line col)
  (doc 'type '(-> String Path Nat Nat String))
  (doc 'description "Format error with source location")
  (format-error-full *level-error* msg #f '() file line col))

(define (format-error-full level msg who irritants file line col)
  (doc 'type '(-> ErrorLevel String (Maybe Symbol) (List Any) (Maybe Path) (Maybe Nat) (Maybe Nat) String))
  (doc 'description "Full error formatting with all options")
  (string-append
   (format-error-header level who file line col)
   "\n"
   (format-message msg irritants)
   "\n"
   (if (and file line)
       (format-source-context file line col)
       "")
   (if *show-suggestions*
       (format-suggestions msg)
       "")
   "\n"))

(doc 'section 'header-formatting)

(define (format-error-header level who file line col)
  (doc 'type '(-> ErrorLevel (Maybe Symbol) (Maybe Path) (Maybe Nat) (Maybe Nat) String))
  (doc 'description "Format error header line")
  (let ([level-str (colorize (error-level-symbol level)
                             (error-level-color level)
                             *color-bold*)]
        [who-str (if who
                     (format " [~a]" who)
                     "")]
        [loc-str (if (and file line)
                     (format " at ~a:~a" file line)
                     "")])
       (string-append level-str who-str loc-str)))

(doc 'section 'message-formatting)

(define (format-message template irritants)
  (doc 'type '(-> String (List Any) String))
  (doc 'description "Format message string, filling in placeholders - FIXES the ~s and ~:s placeholder bugs")
  (guard (e [else template])
         (if (null? irritants)
             (fix-unfilled-placeholders template)
             (fill-placeholders template irritants))))

(define (fix-unfilled-placeholders str)
  (doc 'type '(-> String String))
  (doc 'description "Replace unfilled format directives with safe defaults")
  (let* ([fixed (string-replace str "~s" "<value>")]
         [fixed2 (string-replace fixed "~:s" "<value>")]
         [fixed3 (string-replace fixed2 "~a" "<value>")]
         [fixed4 (string-replace fixed3 "~d" "<number>")])
        fixed4))

(define (fill-placeholders template irritants)
  (doc 'type '(-> String (List Any) String))
  (doc 'description "Fill format directives with actual values")
  (guard (e [else (manual-substitute template irritants)])
         (apply format template irritants)))

(define (manual-substitute template irritants)
  (doc 'type '(-> String (List Any) String))
  (doc 'description "Manually substitute placeholders (fallback)")
  (let loop ([str template]
             [vals irritants])
       (if (null? vals)
           (fix-unfilled-placeholders str)
           (let* ([pattern (if (string-contains? str "~s") "~s"
                               (if (string-contains? str "~a") "~a"
                                   (if (string-contains? str "~d") "~d"
                                       #f)))]
                  [replacement (format "~a" (car vals))])
                 (if pattern
                     (loop (string-replace-once str pattern replacement)
                           (cdr vals))
                     str)))))

(define (string-replace-once str pattern replacement)
  (doc 'type '(-> String String String String))
  (doc 'description "Replace first occurrence of pattern")
  (let ([idx (string-find str pattern)])
       (if idx
           (string-append
            (substring str 0 idx)
            replacement
            (substring str (+ idx (string-length pattern))
                       (string-length str)))
           str)))

(define (string-find haystack needle)
  (doc 'type '(-> String String (Maybe Nat)))
  (doc 'description "Find first occurrence of needle in haystack")
  (let ([nlen (string-length needle)]
        [hlen (string-length haystack)])
       (let loop ([i 0])
            (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? needle (substring haystack i (+ i nlen))) i]
             [else (loop (+ i 1))]))))

(doc 'section 'source-context)

(define (format-source-context file line col)
  (doc 'type '(-> Path Nat Nat String))
  (doc 'description "Show source code around error location")
  (guard (e [else ""])
         (let* ([lines (read-file-lines file)]
                [start (max 1 (- line *max-context-lines*))]
                [end (min (length lines) (+ line *max-context-lines*))]
                [context-lines (list-slice lines (- start 1) end)])
               (string-append
                "\n"
                (colorize "Source:" *color-gray* "")
                "\n"
                (format-lines-with-numbers context-lines start line col)))))

(define (format-lines-with-numbers lines start-num error-line error-col)
  (doc 'type '(-> (List String) Nat Nat Nat String))
  (doc 'description "Format source lines with line numbers and error pointer")
  (let loop ([ls lines]
             [num start-num]
             [result ""])
       (if (null? ls)
           result
           (let* ([line (car ls)]
                  [is-error-line? (= num error-line)]
                  [prefix (format "~4d | " num)]
                  [formatted-line (if is-error-line?
                                      (colorize line *color-red* "")
                                      (colorize line *color-gray* ""))]
                  [pointer (if is-error-line?
                               (format "     | ~a~a\n"
                                       (make-string (max 0 (- error-col 1)) #\space)
                                       (colorize "^" *color-red* *color-bold*))
                               "")])
                 (loop (cdr ls)
                       (+ num 1)
                       (string-append result prefix formatted-line "\n" pointer))))))

(doc 'section 'suggestions)

(define (format-suggestions msg)
  (doc 'type '(-> String String))
  (doc 'description "Provide helpful suggestions based on error message")
  (let ([suggestions (suggest-fixes msg)])
       (if (null? suggestions)
           ""
           (string-append
            "\n"
            (colorize "Suggestions:" *color-blue* *color-bold*)
            "\n"
            (string-join
             (map (lambda (s)
                          (format "  • ~a" s))
                  suggestions)
             "\n")
            "\n"))))

(define (suggest-fixes msg)
  (doc 'type '(-> String (List String)))
  (doc 'description "Analyze error and suggest fixes")
  (cond
   [(string-contains? msg "not bound")
    '("Check for typos in variable name"
      "Verify the variable is defined before use"
      "Check if module is loaded")]
   [(string-contains? msg "wrong number of arguments")
    '("Check function signature"
      "Verify all required parameters are provided")]
   [(string-contains? msg "type")
    '("Check type annotations"
      "Verify input types match expectations")]
   [(string-contains? msg "file")
    '("Check file path is correct"
      "Verify file exists and is readable")]
   [else '()]))

(doc 'section 'colorization)

(define (colorize text color modifier)
  (doc 'type '(-> String String String String))
  (doc 'description "Apply ANSI color codes to text")
  (if *use-colors*
      (string-append modifier color text *color-reset*)
      text))

(doc 'section 'utilities)

(define (make-string n ch)
  (doc 'type '(-> Nat Char String))
  (doc 'description "Create string of n copies of ch")
  (list->string (make-list-of n ch)))

(define (make-list-of n x)
  (doc 'type '(-> Nat α (List α)))
  (doc 'description "Create list of n copies of x")
  (if (<= n 0)
      '()
      (cons x (make-list-of (- n 1) x))))

(define (list-slice lst start end)
  (doc 'type '(-> (List α) Nat Nat (List α)))
  (doc 'description "Extract sublist from start to end (exclusive)")
  (let loop ([ls lst]
             [i 0]
             [result '()])
       (cond
        [(null? ls) (reverse result)]
        [(< i start) (loop (cdr ls) (+ i 1) result)]
        [(>= i end) (reverse result)]
        [else (loop (cdr ls) (+ i 1) (cons (car ls) result))])))

(define (read-file-lines path)
  (doc 'type '(-> Path (List String)))
  (doc 'description "Read all lines from file")
  (guard (e [else '()])
         (call-with-input-file path
                               (lambda (port)
                                       (let loop ([lines '()])
                                            (let ([line (get-line port)])
                                                 (if (eof-object? line)
                                                     (reverse lines)
                                                     (loop (cons line lines)))))))))

(doc 'section 'public-api)

(doc error-to-string 'export #t)
(define (error-to-string condition)
  (doc 'type '(-> Condition String))
  (doc 'description "Convert condition to string (public API)")
  (format-error condition))

(display "\n")
(display "Enhanced error formatter loaded.\n")
(display "\n")
(display "Features:\n")
(display "  • Fixes ~s and ~:s placeholder bugs\n")
(display "  • Color-coded severity levels\n")
(display "  • Source location highlighting\n")
(display "  • Contextual suggestions\n")
(display "\n")
(display "Usage:\n")
(display "  (format-error condition)                 - Format condition\n")
(display "  (format-error-simple \"message\")          - Simple error\n")
(display "  (format-error-with-location msg f l c)   - With location\n")
(display "  (error-to-string condition)              - Convert to string\n")
(display "\n")
(display "Configuration:\n")
(display "  (set! *use-colors* #f)                   - Disable colors\n")
(display "  (set! *show-suggestions* #f)             - Disable suggestions\n")
(display "\n")
