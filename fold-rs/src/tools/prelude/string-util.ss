; string-util.ss - String utility functions
; Part of The Fold prelude

; ============================================
; String manipulation
; ============================================

; words: Split string on whitespace
; (words "hello world  foo") => ("hello" "world" "foo")
(define (words str)
  (filter (fn (s) (not (string-empty? s))) (string-split str " ")))

; unwords: Join strings with spaces
; (unwords '("hello" "world")) => "hello world"
(define (unwords strs)
  (if (null? strs)
      ""
      (foldl (fn (acc s) (string-append acc " " s))
             (car strs)
             (cdr strs))))

; lines: Split string on newlines
; (lines "a\nb\nc") => ("a" "b" "c")
(define (lines str)
  (string-split str "\n"))

; unlines: Join strings with newlines
; (unlines '("a" "b" "c")) => "a\nb\nc"
(define (unlines strs)
  (if (null? strs)
      ""
      (foldl (fn (acc s) (string-append acc "\n" s))
             (car strs)
             (cdr strs))))

; join: Join strings with separator
; (join ", " '("a" "b" "c")) => "a, b, c"
(define (join sep strs)
  (if (null? strs)
      ""
      (foldl (fn (acc s) (string-append acc sep s))
             (car strs)
             (cdr strs))))

; ============================================
; Module exports
; ============================================

(module-exports
  words
  unwords
  lines
  unlines
  join)
