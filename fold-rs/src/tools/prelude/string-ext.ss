; ============================================================
; Extended String Functions
; Canonical versions - no duplicates
; ============================================================

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

; string-empty?: Check if string is empty
; (string-empty? "") => #t
(string-empty? (fn (s) (= (string-length s) 0)))

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

; string-pad-left: Pad string on left to width
; (string-pad-left 5 #\* "hi") => "***hi"
(string-pad-left (fn (width pad-char s)
                     (let ((len (string-length s)))
                          (if (>= len width)
                              s
                              (string-append (make-string (- width len) pad-char) s)))))

; string-pad-right: Pad string on right to width
; (string-pad-right 5 #\* "hi") => "hi***"
(string-pad-right (fn (width pad-char s)
                      (let ((len (string-length s)))
                           (if (>= len width)
                               s
                               (string-append s (make-string (- width len) pad-char))))))

; string-center: Center string to width
; (string-center 7 #\- "hi") => "--hi---"
(string-center (fn (width pad-char s)
                      (let ((len (string-length s)))
                           (if (>= len width)
                               s
                               (let ((total-pad (- width len)))
                                    (let ((left-pad (/ total-pad 2)))
                                         (let ((right-pad (- total-pad left-pad)))
                                              (string-append
                                               (make-string left-pad pad-char)
                                               s
                                               (make-string right-pad pad-char)))))))))

; string-first: Get first character of string
; (string-first "hello") => #\h
(string-first (fn (s)
                  (if (string-empty? s) #f (string-ref s 0))))

; string-last: Get last character of string
; (string-last "hello") => #\o
(string-last (fn (s)
                 (if (string-empty? s) #f (string-ref s (- (string-length s) 1)))))

; string-reverse: Reverse a string
; (string-reverse "hello") => "olleh"
(string-reverse (fn (s)
                    (list->string (reverse (string->list s)))))
