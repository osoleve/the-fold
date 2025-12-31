; ============================================================
; String Utility Functions
; String splitting and joining operations
; ============================================================

; words: Split string on whitespace
(words (fn (str)
          (filter (fn (s) (not (string-empty? s))) (string-split str " "))))

; unwords: Join strings with spaces
(unwords (fn (strs)
            (if (null? strs)
                ""
                (foldl (fn (acc s) (string-append acc " " s))
                       (car strs)
                       (cdr strs)))))

; lines: Split string on newlines
(lines (fn (str)
          (string-split str "\n")))

; unlines: Join strings with newlines
(unlines (fn (strs)
            (if (null? strs)
                ""
                (foldl (fn (acc s) (string-append acc "\n" s))
                       (car strs)
                       (cdr strs)))))

; join-str: Join strings with separator
(join-str (fn (sep strs)
             (if (null? strs)
                 ""
                 (foldl (fn (acc s) (string-append acc sep s))
                        (car strs)
                        (cdr strs)))))
