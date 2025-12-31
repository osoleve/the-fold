; ============================================================
; String - Search and Split Operations
; Finding, splitting, joining, replacing, prefix/suffix checks
; Part of string.ss module
; ============================================================

; --- Prefix/Suffix Tests ---

; string-prefix?: Check if string starts with prefix
(string-prefix? (fn (pre s)
                    (prefix? (string->list pre) (string->list s))))

; string-suffix?: Check if string ends with suffix
(string-suffix? (fn (suf s)
                    (suffix? (string->list suf) (string->list s))))

; string-starts-with?: Check if string s starts with prefix
; (string-starts-with? "hello world" "hello") => #t
(string-starts-with? (fn (s prefix)
                         (let ((plen (string-length prefix)))
                              (and (>= (string-length s) plen)
                                   (string=? (substring s 0 plen) prefix)))))

; string-ends-with?: Check if string s ends with suffix
; (string-ends-with? "hello world" "world") => #t
(string-ends-with? (fn (s suffix)
                       (let ((slen (string-length s))
                             (suflen (string-length suffix)))
                            (and (>= slen suflen)
                                 (string=? (substring s (- slen suflen) slen) suffix)))))

; --- Searching ---

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

; string-index-of: Find index of first occurrence of substring
; (string-index-of "hello world" "wor") => 6
(string-index-of (fn (haystack needle)
                     (let ((nlen (string-length needle))
                           (hlen (string-length haystack)))
                          (let ((search (fix search
                                             (fn (i)
                                                 (if (> (+ i nlen) hlen)
                                                     #f
                                                     (if (string=? (substring haystack i (+ i nlen)) needle)
                                                         i
                                                         (search (+ i 1))))))))
                               (search 0)))))

; --- Splitting ---

; string-split-at: Split string at index
; (string-split-at 3 "hello") => ("hel" "lo")
(string-split-at (fn (idx s)
                     (list (substring s 0 (min idx (string-length s)))
                           (substring s (min idx (string-length s)) (string-length s)))))

; split-when: Split list when predicate is true
; (split-when odd? '(2 4 1 6 3 8)) => ((2 4) (6) (8))
(split-when (fix split-when
                 (fn (f lst)
                     (if (null? lst)
                         '(())
                         (if (f (car lst))
                             (cons '() (split-when f (cdr lst)))
                             (let ((rest (split-when f (cdr lst))))
                                  (cons (cons (car lst) (car rest))
                                        (cdr rest))))))))

; string-split-char: Split string by delimiter character
; (string-split-char "a,b,c" #\,) => ("a" "b" "c")
(string-split-char (fn (s delim)
                       (let ((chars (string->list s)))
                            (map list->string (split-when (fn (c) (eq? c delim)) chars)))))

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

; --- Replacing ---

; string-replace: Replace first occurrence of old with new
(string-replace (fix string-replace
                     (fn (old new s)
                         (let ((idx (string-index-of s old)))
                              (if (not idx)
                                  s
                                  (string-append
                                   (substring s 0 idx)
                                   new
                                   (substring s (+ idx (string-length old)) (string-length s))))))))

; string-replace-all: Replace all occurrences of old with new
(string-replace-all (fix string-replace-all
                         (fn (old new s)
                             (let ((idx (string-index-of s old)))
                                  (if (not idx)
                                      s
                                      (string-replace-all
                                       old new
                                       (string-append
                                        (substring s 0 idx)
                                        new
                                        (substring s (+ idx (string-length old)) (string-length s)))))))))

; string-replace-first: Alias for string-replace
(string-replace-first string-replace)

; --- String Distance Algorithms ---

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

; --- Module Exports ---
; (see exports.ss for exported symbols)
