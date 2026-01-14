;;; shell/string-utils.ss — String Utilities (Shell Layer)
;;;
;;; This module re-exports string utilities from core/prelude.ss
;;; and adds shell-specific extensions.
;;;
;;; CANONICAL SOURCE: core/prelude.ss
;;; Shell code should load this file (or prelude.ss directly).
;;;
;;; Functions from prelude.ss:
;;;   string-join, string-split (char only), string-trim, string-trim-left,
;;;   string-trim-right, string-contains?, string-starts-with?, string-ends-with?,
;;;   string-index-of, string-last-index-of, string-replace, string-reverse,
;;;   string-empty?, string-blank?, string-all-match?, string-upcase,
;;;   string-downcase, string-pad-left, string-pad-right, whitespace?,
;;;   edit-distance
;;;
;;; Shell-specific additions:
;;;   string-prefix? (alias for string-starts-with?)
;;;   string-suffix? (alias for string-ends-with?)
;;;   string-split-flex (accepts char or string delimiter)
;;;   string-split-lines (handles Unix/Windows line endings)
;;;   string-index (find char index)
;;;   string-index-right (find last char index)
;;;   string-replace-first (replace only first occurrence)

(load "core/base/prelude.ss")

;;; ====
;;; Shell-Specific Aliases
;;; ====

;;; string-prefix? : String × String → Boolean
;;; Check if str starts with prefix.
;;; Note: Argument order is (prefix str) for consistency with shell code patterns.
(define (string-prefix? prefix str)
  (string-starts-with? str prefix))

;;; string-suffix? : String × String → Boolean
;;; Check if str ends with suffix.
;;; Note: Argument order is (suffix str) for consistency with shell code patterns.
(define (string-suffix? suffix str)
  (string-ends-with? str suffix))

;;; ====
;;; Shell-Specific Extensions
;;; ====

;;; string-index : String × Char → Integer | #f
;;; Find first occurrence of character in string.
(define (string-index str ch)
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
