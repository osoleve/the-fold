; ============================================================
; String - Formatting Operations
; Padding, justification, number formatting, pluralization
; Part of string.ss module
; ============================================================

; --- Padding and Justification ---

; char-repeat: Create string of n copies of a character
; Helper for padding functions
(char-repeat (fn (n c)
                 (string-repeat (list->string (list c)) n)))

; string-pad-left: Pad string on left to given length
; (string-pad-left 5 #\* "hi") => "***hi"
(string-pad-left (fn (len pad-char s)
                     (let ((current-len (string-length s)))
                          (if (>= current-len len)
                              s
                              (string-append (char-repeat (- len current-len) pad-char) s)))))

; string-pad-right: Pad string on right to given length
; (string-pad-right 5 #\* "hi") => "hi***"
(string-pad-right (fn (len pad-char s)
                      (let ((current-len (string-length s)))
                           (if (>= current-len len)
                               s
                               (string-append s (char-repeat (- len current-len) pad-char))))))

; string-center: Center string with padding
; (string-center 7 #\- "hi") => "--hi---"
(string-center (fn (len pad-char s)
                   (let ((current-len (string-length s)))
                        (let ((total-pad (- len current-len)))
                             (if (<= total-pad 0)
                                 s
                                 (let ((left-pad (/ total-pad 2)))
                                      (let ((right-pad (- total-pad left-pad)))
                                           (string-append
                                            (char-repeat left-pad pad-char)
                                            s
                                            (char-repeat right-pad pad-char)))))))))

; string-ljust: Left justify (alias for pad-right)
; (string-ljust 10 "hi") => "hi        "
(string-ljust (fn (width s)
                  (string-pad-right width #\space s)))

; string-rjust: Right justify (alias for pad-left)
; (string-rjust 10 "hi") => "        hi"
(string-rjust (fn (width s)
                  (string-pad-left width #\space s)))

; string-pad: Generic padding (width, char) - pads left
(string-pad (fn (s width char)
                (string-pad-left width char s)))

; --- String Repetition ---

; string-replicate: Repeat string n times
; (string-replicate 3 "ab") => "ababab"
(string-replicate (fix string-replicate
                       (fn (n s)
                           (if (<= n 0)
                               ""
                               (string-append s (string-replicate (- n 1) s))))))

; string-repeat: Repeat string n times (alias, reversed args)
; (string-repeat "ab" 3) => "ababab"
(string-repeat (fn (s n)
                   (string-replicate n s)))

; repeat-string: Repeat string n times (alternate name)
(repeat-string string-repeat)

; --- String Interspersing ---

; string-intersperse: Insert separator between characters
; (string-intersperse #\- "abc") => "a-b-c"
(string-intersperse (fn (sep s)
                        (list->string (intersperse sep (string->list s)))))

; string-intercalate: Join list of strings with separator (alias for string-join)
(string-intercalate string-join)

; --- Number Formatting ---

; number->string-padded: Convert number to zero-padded string
; (number->string-padded 3 42) => "042"
(number->string-padded (fn (width n)
                           (string-pad-left width #\0 (number->string n))))

; format-number: Format number with thousand separators
; (format-number 1234567) => "1,234,567"
(format-number (fn (n)
                   (let ((s (number->string (if (< n 0) (- 0 n) n)))
                         (chars (reverse (string->list s))))
                        (let ((grouped (fix group-3
                                            (fn (lst acc count)
                                                (if (null? lst)
                                                    acc
                                                    (if (and (= count 3) (not (null? lst)))
                                                        (group-3 lst (cons #\, acc) 0)
                                                        (group-3 (cdr lst) (cons (car lst) acc) (+ count 1)))))))
                              (result (list->string (grouped chars '() 0))))
                             (if (< n 0)
                                 (string-append "-" result)
                                 result)))))

; --- Word Operations ---

; string-words: Split string into words (by whitespace)
(string-words (fn (s)
                  (filter (fn (w) (not (string-empty? w)))
                          (string-split s " "))))

; string-lines: Split string into lines
(string-lines (fn (s)
                  (string-split s "\n")))

; string-unwords: Join words with space
(string-unwords (fn (words)
                    (string-join words " ")))

; string-unlines: Join lines with newline
(string-unlines (fn (lines)
                    (string-join lines "\n")))

; --- Pluralization ---

; pluralize-simple: Add 's' based on count
; (pluralize-simple 1 "cat") => "cat"
; (pluralize-simple 5 "cat") => "cats"
(pluralize-simple (fn (count word)
                      (if (= count 1)
                          word
                          (string-append word "s"))))

; pluralize: Pluralize with custom plural form
; (pluralize 1 "apple" "apples") => "apple"
; (pluralize 5 "mouse" "mice") => "mice"
(pluralize (fn (count singular plural)
               (if (= count 1)
                   singular
                   plural)))

; pluralize-with: Alias for pluralize
(pluralize-with pluralize)

; --- Humanization ---

; humanize-list: Join list with commas and "and"
; (humanize-list '("a" "b" "c")) => "a, b, and c"
(humanize-list (fn (items)
                   (let ((n (length items)))
                        (if (= n 0) ""
                            (if (= n 1) (car items)
                                (if (= n 2) (string-append (car items) " and " (cadr items))
                                    (string-append
                                     (string-join (take items (- n 1)) ", ")
                                     ", and "
                                     (last items))))))))

; oxford-comma: Alias for humanize-list
(oxford-comma humanize-list)

; --- Normalization ---

; string-normalize: Normalize whitespace (collapse multiple spaces)
(string-normalize (fn (s)
                      (string-join (filter (fn (w) (not (string-empty? w)))
                                           (string-split s " "))
                                   " ")))

; --- Function Aliases (for consistency) ---

(string-words-fn (fn (s) (string-words s)))
(string-lines-fn (fn (s) (string-lines s)))
(string-join-fn (fn (strs sep) (string-join strs sep)))
(string-trim-fn (fn (s) (string-trim s)))
(string-repeat-fn (fn (n s) (string-replicate n s)))

; --- Module Exports ---
; (see exports.ss for exported symbols)
