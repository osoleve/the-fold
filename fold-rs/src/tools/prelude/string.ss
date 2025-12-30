; ============================================================
; String Higher-Order Functions and Utilities
; String manipulation using list-based transformations
; ============================================================

; -- String HOFs --

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

; string-find: Find first character satisfying predicate
(string-find (fn (p s)
                 (find-if p (string->list s))))

; string-find-index: Find index of first character satisfying predicate
(string-find-index (fn (p s)
                       (let ((go (fix go
                                      (fn (i chars)
                                          (if (null? chars)
                                              #f
                                              (if (p (car chars))
                                                  i
                                                  (go (+ i 1) (cdr chars))))))))
                            (go 0 (string->list s)))))

; -- String predicates --

; string-empty?: Check if string is empty
(string-empty? (fn (s) (= (string-length s) 0)))

; string-contains?: Check if string contains substring
(string-contains? (fn (needle haystack)
                      (let ((needle-chars (string->list needle))
                            (haystack-chars (string->list haystack)))
                           (let ((check (fix check
                                             (fn (remaining)
                                                 (if (< (length remaining) (length needle-chars))
                                                     #f
                                                     (if (prefix? needle-chars remaining)
                                                         #t
                                                         (check (cdr remaining))))))))
                                (check haystack-chars)))))

; string-prefix?: Check if string starts with prefix
(string-prefix? (fn (pre s)
                    (prefix? (string->list pre) (string->list s))))

; string-suffix?: Check if string ends with suffix
(string-suffix? (fn (suf s)
                    (suffix? (string->list suf) (string->list s))))

; -- String transformations --

; string-reverse: Reverse a string
(string-reverse (fn (s)
                    (list->string (reverse (string->list s)))))

; string-take: Take first n characters
(string-take (fn (n s)
                 (list->string (take (string->list s) n))))

; string-drop: Drop first n characters
(string-drop (fn (n s)
                 (list->string (drop (string->list s) n))))

; string-take-while: Take characters while predicate holds
(string-take-while (fn (p s)
                       (list->string (take-while p (string->list s)))))

; string-drop-while: Drop characters while predicate holds
(string-drop-while (fn (p s)
                       (list->string (drop-while p (string->list s)))))

; string-trim-left: Remove leading whitespace
(string-trim-left (fn (s)
                      (string-drop-while (fn (c) (or (eq? c #\space) (eq? c #\tab) (eq? c #\newline))) s)))

; string-trim-right: Remove trailing whitespace
(string-trim-right (fn (s)
                       (string-reverse (string-trim-left (string-reverse s)))))

; string-trim: Remove leading and trailing whitespace
(string-trim (fn (s)
                 (string-trim-right (string-trim-left s))))

; string-pad-left: Pad string on left to given length
(string-pad-left (fn (len pad-char s)
                     (let ((current-len (string-length s)))
                          (if (>= current-len len)
                              s
                              (string-append (list->string (replicate (- len current-len) pad-char)) s)))))

; string-pad-right: Pad string on right to given length
(string-pad-right (fn (len pad-char s)
                      (let ((current-len (string-length s)))
                           (if (>= current-len len)
                               s
                               (string-append s (list->string (replicate (- len current-len) pad-char)))))))

; string-center: Center string with padding
(string-center (fn (len pad-char s)
                   (let ((current-len (string-length s))
                         (total-pad (- len current-len)))
                        (if (<= total-pad 0)
                            s
                            (let ((left-pad (/ total-pad 2))
                                  (right-pad (- total-pad (/ total-pad 2))))
                                 (string-append
                                  (list->string (replicate left-pad pad-char))
                                  s
                                  (list->string (replicate right-pad pad-char))))))))

; string-capitalize: Capitalize first letter
(string-capitalize (fn (s)
                       (if (string-empty? s)
                           s
                           (string-append (string-upcase (substring s 0 1))
                                          (substring s 1 (string-length s))))))

; string-title-case: Capitalize first letter of each word
(string-title-case (fn (s)
                       (string-join (map string-capitalize (string-split s " ")) " ")))

; -- Character utilities --

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

; -- String building --

; string-replicate: Repeat string n times
(string-replicate (fn (n s)
                      (if (<= n 0)
                          ""
                          (string-append s (string-replicate (- n 1) s)))))

; string-intersperse: Insert separator between characters
(string-intersperse (fn (sep s)
                        (list->string (intersperse sep (string->list s)))))

; string-intercalate: Join list of strings with separator
(string-intercalate string-join)

; -- String algorithms --

; edit-distance: Levenshtein edit distance
(edit-distance (fn (s1 s2)
                   (let ((chars1 (string->list s1))
                         (chars2 (string->list s2))
                         (m (string-length s1))
                         (n (string-length s2)))
                        (if (= m 0) n
                            (if (= n 0) m
                                (let ((compute (fix compute
                                                    (fn (i j memo)
                                                        (if (> i m)
                                                            (cdr (assoc (list m n) memo))
                                                            (if (> j n)
                                                                (compute (+ i 1) 1 memo)
                                                                (let ((cost (if (eq? (nth chars1 (- i 1))
                                                                                     (nth chars2 (- j 1)))
                                                                                0 1))
                                                                      (del (+ 1 (cdr (assoc (list (- i 1) j) memo))))
                                                                      (ins (+ 1 (cdr (assoc (list i (- j 1)) memo))))
                                                                      (sub (+ cost (cdr (assoc (list (- i 1) (- j 1)) memo)))))
                                                                     (compute i (+ j 1)
                                                                              (cons (cons (list i j) (min del (min ins sub)))
                                                                                    memo)))))))))
                                     (let ((init-memo (append
                                                       (map (fn (i) (cons (list i 0) i)) (range 0 (+ m 1)))
                                                       (map (fn (j) (cons (list 0 j) j)) (range 0 (+ n 1))))))
                                          (compute 1 1 init-memo))))))))

; hamming-distance: Count positions where strings differ
(hamming-distance (fn (s1 s2)
                      (length (filter (fn (p) (not (eq? (car p) (cdr p))))
                                      (zip (string->list s1) (string->list s2))))))

; longest-common-prefix: Find longest common prefix of two strings
(longest-common-prefix (fn (s1 s2)
                           (let ((chars1 (string->list s1))
                                 (chars2 (string->list s2)))
                                (let ((go (fix go
                                               (fn (c1 c2 acc)
                                                   (if (or (null? c1) (null? c2))
                                                       (reverse acc)
                                                       (if (eq? (car c1) (car c2))
                                                           (go (cdr c1) (cdr c2) (cons (car c1) acc))
                                                           (reverse acc)))))))
                                     (list->string (go chars1 chars2 '()))))))

; longest-common-suffix: Find longest common suffix of two strings
(longest-common-suffix (fn (s1 s2)
                           (string-reverse (longest-common-prefix (string-reverse s1) (string-reverse s2)))))

; string-similarity: Ratio of matching characters (0.0 to 1.0)
(string-similarity (fn (s1 s2)
                       (let ((len1 (string-length s1))
                             (len2 (string-length s2)))
                            (if (and (= len1 0) (= len2 0))
                                1
                                (let ((max-len (max len1 len2)))
                                     (/ (- max-len (edit-distance s1 s2)) max-len))))))

; fuzzy-match?: Check if strings are similar within threshold
(fuzzy-match? (fn (threshold s1 s2)
                  (>= (string-similarity s1 s2) threshold)))

; string-normalize: Normalize whitespace (collapse multiple spaces)
(string-normalize (fn (s)
                      (string-join (filter (fn (w) (not (string-empty? w)))
                                           (string-split s " "))
                                   " ")))

; string-tokenize: Split into tokens by predicate
(string-tokenize (fn (is-delimiter? s)
                     (let ((chars (string->list s)))
                          (let ((go (fix go
                                         (fn (cs current tokens)
                                             (if (null? cs)
                                                 (if (null? current)
                                                     (reverse tokens)
                                                     (reverse (cons (list->string (reverse current)) tokens)))
                                                 (if (is-delimiter? (car cs))
                                                     (if (null? current)
                                                         (go (cdr cs) '() tokens)
                                                         (go (cdr cs) '()
                                                             (cons (list->string (reverse current)) tokens)))
                                                     (go (cdr cs) (cons (car cs) current) tokens)))))))
                               (go chars '() '())))))
