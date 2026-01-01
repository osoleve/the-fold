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
;;;   string-downcase, string-pad-left, string-pad-right, whitespace?
;;;
;;; Shell-specific additions:
;;;   string-prefix? (alias for string-starts-with?)
;;;   string-suffix? (alias for string-ends-with?)
;;;   string-split-flex (accepts char or string delimiter)

(load "core/prelude.ss")

;;; ============================================================
;;; Shell-Specific Aliases
;;; ============================================================

;;; string-prefix? : String × String → Boolean
;;; Alias for string-starts-with? (common in shell code)
(define string-prefix? string-starts-with?)

;;; string-suffix? : String × String → Boolean
;;; Alias for string-ends-with? (common in shell code)
(define string-suffix? string-ends-with?)

;;; ============================================================
;;; Shell-Specific Extensions
;;; ============================================================

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
                            (let ([end (+ start idx)])
                                 (loop (+ end delim-len)
                                       (cons (substring str start end) result)))
                            (reverse (cons (substring str start (string-length str)) result))))))])))

(printf "  String utilities loaded (via prelude.ss)\n")
