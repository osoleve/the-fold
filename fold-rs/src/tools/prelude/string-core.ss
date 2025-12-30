; ============================================================
; String - Core Operations
; Basic string manipulation: length, access, conversion, case
; Part of string.ss module
; ============================================================

; --- Predicates ---

; string-empty?: Check if string is empty
(string-empty? (fn (s) (= (string-length s) 0)))

; --- Character Access ---

; string-first: Get first character of string
; (string-first "hello") => #\h
(string-first (fn (s)
                  (if (string-empty? s) #f (string-ref s 0))))

; string-last: Get last character of string
; (string-last "hello") => #\o
(string-last (fn (s)
                 (if (string-empty? s) #f (string-ref s (- (string-length s) 1)))))

; string-chars: Convert string to list of characters (alias)
(string-chars string->list)

; --- Substring Operations ---

; string-take: Take first n characters
; (string-take 3 "hello") => "hel"
(string-take (fn (n s)
                 (substring s 0 (min n (string-length s)))))

; string-drop: Drop first n characters
; (string-drop 2 "hello") => "llo"
(string-drop (fn (n s)
                 (substring s (min n (string-length s)) (string-length s))))

; string-take-right: Take last n characters
; (string-take-right 3 "hello") => "llo"
(string-take-right (fn (n s)
                       (let ((len (string-length s)))
                            (substring s (max 0 (- len n)) len))))

; string-drop-right: Drop last n characters
; (string-drop-right 2 "hello") => "hel"
(string-drop-right (fn (n s)
                       (let ((len (string-length s)))
                            (substring s 0 (max 0 (- len n))))))

; --- String Transformations ---

; string-reverse: Reverse a string
; (string-reverse "hello") => "olleh"
(string-reverse (fn (s)
                    (list->string (reverse (string->list s)))))

; string-copy: Copy a string (identity in immutable context)
(string-copy (fn (s) s))

; --- Case Transformations ---

; string-capitalize: Capitalize first letter
(string-capitalize (fn (s)
                       (if (string-empty? s)
                           s
                           (string-append (string-upcase (substring s 0 1))
                                          (substring s 1 (string-length s))))))

; string-title-case: Capitalize first letter of each word
(string-title-case (fn (s)
                       (string-join (map string-capitalize (string-split s " ")) " ")))

; --- Whitespace Handling ---

; string-trim-left: Remove leading whitespace
(string-trim-left (fn (s)
                      (string-drop-while (fn (c) (or (eq? c #\space) (eq? c #\tab) (eq? c #\newline))) s)))

; string-trim-right: Remove trailing whitespace
(string-trim-right (fn (s)
                       (string-reverse (string-trim-left (string-reverse s)))))

; string-trim: Remove leading and trailing whitespace
(string-trim (fn (s)
                 (string-trim-right (string-trim-left s))))

; string-blank?: Check if string is empty or whitespace only
(string-blank? (fn (s)
                   (string-empty? (string-trim s))))

; --- HOFs on Strings ---

; string-map: Apply function to each character
(string-map (fn (f s)
                (list->string (map f (string->list s)))))

; string-filter: Keep characters satisfying predicate
(string-filter (fn (p s)
                   (list->string (filter p (string->list s)))))

; string-foldl: Left fold over string characters
(string-foldl (fn (f acc s)
                  (foldl f acc (string->list s))))

; string-foldr: Right fold over string characters
(string-foldr (fn (f acc s)
                  (foldr f acc (string->list s))))

; string-any: Check if any character satisfies predicate
(string-any (fn (p s)
                (any p (string->list s))))

; string-all: Check if all characters satisfy predicate
(string-all (fn (p s)
                (all p (string->list s))))

; string-take-while: Take characters while predicate holds
(string-take-while (fn (p s)
                       (list->string (take-while p (string->list s)))))

; string-drop-while: Drop characters while predicate holds
(string-drop-while (fn (p s)
                       (list->string (drop-while p (string->list s)))))

; string-count: Count characters satisfying predicate
(string-count (fn (p s)
                  (length (filter p (string->list s)))))

; string-partition: Partition string by predicate
(string-partition (fn (p s)
                      (let ((parts (partition p (string->list s))))
                           (list (list->string (car parts)) (list->string (cadr parts))))))

; --- Character Utilities ---

; string-count-char: Count occurrences of character in string
(string-count-char (fn (c s)
                       (length (filter (fn (ch) (eq? ch c)) (string->list s)))))

; string-replace-char: Replace all occurrences of character
(string-replace-char (fn (old new s)
                         (list->string (map (fn (c) (if (eq? c old) new c)) (string->list s)))))

; string-squeeze: Remove consecutive duplicate characters
(string-squeeze (fn (s)
                    (list->string (dedupe (string->list s)))))

; string-rotate: Rotate string by n positions
(string-rotate (fn (n s)
                   (list->string (rotate-list n (string->list s)))))

; string-interleave: Interleave two strings
(string-interleave (fn (s1 s2)
                       (list->string (concat (zip-with list (string->list s1) (string->list s2))))))

; --- Module Exports ---
; (see exports.ss for exported symbols)
