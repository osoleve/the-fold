; ============================================================
; Path Manipulation Utilities
; File system path operations
; ============================================================

; -- Path separators --

; path-separator: The path separator character
(path-separator #\/)

; -- Basic path operations --

; path-split: Split path into components
(path-split (fn (path)
                (filter (fn (s) (not (string-empty? s)))
                        (string-split path "/"))))

; path-join: Join path components
(path-join (fn (components)
               (string-join components "/")))

; path-join-2: Join two paths
(path-join-2 (fn (p1 p2)
                 (if (string-empty? p1)
                     p2
                     (if (string-empty? p2)
                         p1
                         (if (string-suffix? "/" p1)
                             (string-append p1 p2)
                             (string-append p1 "/" p2))))))

; -- Path extraction --

; path-dirname: Get directory portion of path
(path-dirname (fn (path)
                  (let ((components (path-split path)))
                       (if (null? components)
                           "."
                           (if (null? (cdr components))
                               (if (string-prefix? "/" path) "/" ".")
                               (let ((dir-parts (reverse (cdr (reverse components)))))
                                    (if (string-prefix? "/" path)
                                        (string-append "/" (path-join dir-parts))
                                        (path-join dir-parts))))))))

; path-basename: Get filename portion of path
(path-basename (fn (path)
                   (let ((components (path-split path)))
                        (if (null? components)
                            ""
                            (last components)))))

; path-extension: Get file extension (including dot)
(path-extension (fn (path)
                    (let ((base (path-basename path))
                          (chars (string->list base)))
                         (let ((find-dot (fix find-dot
                                              (fn (cs acc)
                                                  (if (null? cs)
                                                      ""
                                                      (if (eq? (car cs) #\.)
                                                          (list->string (cons #\. (reverse acc)))
                                                          (find-dot (cdr cs) (cons (car cs) acc))))))))
                              (find-dot (reverse chars) '())))))

; path-stem: Get filename without extension
(path-stem (fn (path)
               (let ((base (path-basename path))
                     (ext (path-extension path)))
                    (if (string-empty? ext)
                        base
                        (substring base 0 (- (string-length base) (string-length ext)))))))

; -- Path predicates --

; path-absolute?: Check if path is absolute
(path-absolute? (fn (path)
                    (and (not (string-empty? path))
                         (eq? (car (string->list path)) #\/))))

; path-relative?: Check if path is relative
(path-relative? (fn (path)
                    (not (path-absolute? path))))

; path-hidden?: Check if file is hidden (starts with .)
(path-hidden? (fn (path)
                  (let ((base (path-basename path)))
                       (and (not (string-empty? base))
                            (eq? (car (string->list base)) #\.)))))

; -- Path transformations --

; path-normalize: Normalize path (resolve . and ..)
(path-normalize (fn (path)
                    (let ((is-absolute (path-absolute? path))
                          (components (path-split path)))
                         (let ((normalize (fix normalize
                                               (fn (cs acc)
                                                   (if (null? cs)
                                                       (reverse acc)
                                                       (let ((c (car cs)))
                                                            (if (string=? c ".")
                                                                (normalize (cdr cs) acc)
                                                                (if (string=? c "..")
                                                                    (if (null? acc)
                                                                        (normalize (cdr cs) (if is-absolute '() (list "..")))
                                                                        (normalize (cdr cs) (cdr acc)))
                                                                    (normalize (cdr cs) (cons c acc))))))))))
                              (let ((normalized (normalize components '())))
                                   (if is-absolute
                                       (if (null? normalized)
                                           "/"
                                           (string-append "/" (path-join normalized)))
                                       (if (null? normalized)
                                           "."
                                           (path-join normalized))))))))

; path-parent: Get parent directory
(path-parent path-dirname)

; path-with-extension: Replace extension
(path-with-extension (fn (new-ext path)
                         (let ((dir (path-dirname path))
                               (stem (path-stem path)))
                              (path-join-2 dir (string-append stem new-ext)))))

; path-add-suffix: Add suffix before extension
(path-add-suffix (fn (suffix path)
                     (let ((dir (path-dirname path))
                           (stem (path-stem path))
                           (ext (path-extension path)))
                          (path-join-2 dir (string-append stem suffix ext)))))

; -- Path analysis --

; path-components: Get list of path components
(path-components path-split)

; path-depth: Get depth of path (number of components)
(path-depth (fn (path)
                (length (path-split path))))

; path-common-prefix: Find common prefix of two paths
(path-common-prefix (fn (path1 path2)
                        (let ((c1 (path-split path1))
                              (c2 (path-split path2)))
                             (let ((common (fix common
                                                (fn (xs ys acc)
                                                    (if (or (null? xs) (null? ys))
                                                        (reverse acc)
                                                        (if (string=? (car xs) (car ys))
                                                            (common (cdr xs) (cdr ys) (cons (car xs) acc))
                                                            (reverse acc)))))))
                                  (path-join (common c1 c2 '()))))))

; path-relative-to: Make path relative to base
(path-relative-to (fn (base path)
                      (let ((base-parts (path-split base))
                            (path-parts (path-split path)))
                           (let ((skip-common (fix skip-common
                                                   (fn (bs ps)
                                                       (if (or (null? bs) (null? ps))
                                                           (cons bs ps)
                                                           (if (string=? (car bs) (car ps))
                                                               (skip-common (cdr bs) (cdr ps))
                                                               (cons bs ps)))))))
                                (let ((result (skip-common base-parts path-parts)))
                                     (let ((ups (replicate (length (car result)) ".."))
                                           (rest (cdr result)))
                                          (path-join (append ups rest))))))))

; -- Glob pattern matching --

; glob-match?: Check if path matches glob pattern
; Supports * (any chars) and ? (single char)
(glob-match? (fn (pattern path)
                 (let ((match (fix match
                                   (fn (p s)
                                       (if (null? p)
                                           (null? s)
                                           (let ((pc (car p)))
                                                (if (eq? pc #\*)
                                                    (if (null? (cdr p))
                                                        #t  ; * at end matches everything
                                                        (any id (map (fn (i)
                                                                         (match (cdr p) (drop (string->list path) i)))
                                                                     (range 0 (+ (length s) 1)))))
                                                    (if (eq? pc #\?)
                                                        (if (null? s)
                                                            #f
                                                            (match (cdr p) (cdr s)))
                                                        (if (null? s)
                                                            #f
                                                            (if (eq? pc (car s))
                                                                (match (cdr p) (cdr s))
                                                                #f))))))))))
                      (match (string->list pattern) (string->list path)))))

; glob-filter: Filter paths by glob pattern
(glob-filter (fn (pattern paths)
                 (filter (fn (p) (glob-match? pattern p)) paths)))
