;;; meta/query.ss — Tag Query Tools
;;;
;;; Layer 2 of the metadata system: querying posts by tags.
;;;
;;; Dependencies:
;;;   - meta/parse.ss
;;;   - forum/tools.ss
;;;   - shell/fs.ss
;;;
;;; Usage:
;;;   (find-tagged fs 'status "complete")   ; Find posts with @status:complete
;;;   (list-all-tags fs)                    ; Inventory of all tags in use
;;;   (tag-histogram fs)                    ; Frequency counts
;;;
;;; This is Shell-tier code: reads from CAS, defensive patterns.

;;; ============================================================
;;; Dependencies
;;; ============================================================

;;; Assumes these are already loaded:
;;;   - meta/parse.ss (extract-tags, has-tag?, get-tag)
;;;   - forum/tools.ss (collect-channel, list-channels)
;;;   - shell/fs.ss (fs-fetch, etc.)

;;; ============================================================
;;; Finding Posts by Tag
;;; ============================================================

;;; find-tagged : FS × Symbol × (U String #t) → (Listof Alist)
;;; Find all forum posts that have a specific tag.
;;; If value is #t, matches any value for that key.
;;;
;;; Example:
;;;   (find-tagged (fs) 'status "complete")
;;;   → list of posts with @status:complete
(define (find-tagged fs key value)
  (let ([all-posts (collect-all-posts fs)])
    (filter
      (lambda (post)
        (let* ([body (cdr (assq 'body post))]
               [tags (extract-tags body)])
          (and (has-tag? tags key)
               (or (eq? value #t)
                   (equal? (get-tag tags key) value)))))
      all-posts)))

;;; find-tagged-any : FS × Symbol → (Listof Alist)
;;; Find all posts that have a tag key, regardless of value.
(define (find-tagged-any fs key)
  (find-tagged fs key #t))

;;; collect-all-posts : FS → (Listof Alist)
;;; Collect all posts from all channels.
(define (collect-all-posts fs)
  (let ([channels (list-channels fs)])
    (apply append
           (map (lambda (ch) (collect-channel fs ch))
                channels))))

;;; ============================================================
;;; Tag Inventory
;;; ============================================================

;;; list-all-tags : FS → (Listof Symbol)
;;; Get a list of all unique tag keys used across all posts.
(define (list-all-tags fs)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                 (let ([body (cdr (assq 'body post))])
                                   (extract-tags body)))
                               all-posts))]
         [keys (map car all-tags)])
    (unique keys)))

;;; unique : (Listof α) → (Listof α)
;;; Remove duplicates from a list.
(define (unique lst)
  (let loop ([items lst] [seen '()])
    (if (null? items)
        (reverse seen)
        (if (member (car items) seen)
            (loop (cdr items) seen)
            (loop (cdr items) (cons (car items) seen))))))

;;; tag-histogram : FS → (Listof (Pair Symbol Nat))
;;; Count occurrences of each tag key.
;;; Returns alist sorted by frequency (highest first).
(define (tag-histogram fs)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                 (let ([body (cdr (assq 'body post))])
                                   (extract-tags body)))
                               all-posts))]
         [keys (map car all-tags)]
         [counts (count-occurrences keys)])
    (sort-by-count counts)))

;;; count-occurrences : (Listof Symbol) → (Listof (Pair Symbol Nat))
;;; Count how many times each symbol appears.
(define (count-occurrences symbols)
  (let loop ([syms symbols] [counts '()])
    (if (null? syms)
        counts
        (let* ([sym (car syms)]
               [existing (assq sym counts)])
          (if existing
              (loop (cdr syms)
                    (cons (cons sym (+ (cdr existing) 1))
                          (remove-assq sym counts)))
              (loop (cdr syms)
                    (cons (cons sym 1) counts)))))))

;;; remove-assq : Symbol × Alist → Alist
;;; Remove first pair with matching key.
(define (remove-assq key alist)
  (filter (lambda (pair) (not (eq? (car pair) key))) alist))

;;; sort-by-count : (Listof (Pair Symbol Nat)) → (Listof (Pair Symbol Nat))
;;; Sort by count, highest first.
(define (sort-by-count pairs)
  (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs))

;;; ============================================================
;;; Tag Value Analysis
;;; ============================================================

;;; tag-values : FS × Symbol → (Listof String)
;;; Get all unique values used with a specific tag key.
(define (tag-values fs key)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                 (let ([body (cdr (assq 'body post))])
                                   (extract-tags body)))
                               all-posts))]
         [matching (filter (lambda (t) (eq? (car t) key)) all-tags)]
         [values (map cdr matching)]
         [string-values (filter string? values)])
    (unique string-values)))

;;; tag-value-histogram : FS × Symbol → (Listof (Pair String Nat))
;;; Count occurrences of each value for a tag key.
(define (tag-value-histogram fs key)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                 (let ([body (cdr (assq 'body post))])
                                   (extract-tags body)))
                               all-posts))]
         [matching (filter (lambda (t) (eq? (car t) key)) all-tags)]
         [values (filter string? (map cdr matching))]
         [counts (count-string-occurrences values)])
    (sort-by-count-strings counts)))

;;; count-string-occurrences : (Listof String) → (Listof (Pair String Nat))
(define (count-string-occurrences strings)
  (let loop ([strs strings] [counts '()])
    (if (null? strs)
        counts
        (let* ([s (car strs)]
               [existing (assoc s counts)])
          (if existing
              (loop (cdr strs)
                    (cons (cons s (+ (cdr existing) 1))
                          (remove-assoc s counts)))
              (loop (cdr strs)
                    (cons (cons s 1) counts)))))))

;;; remove-assoc : String × Alist → Alist
(define (remove-assoc key alist)
  (filter (lambda (pair) (not (equal? (car pair) key))) alist))

;;; sort-by-count-strings : (Listof (Pair String Nat)) → (Listof (Pair String Nat))
(define (sort-by-count-strings pairs)
  (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs))

;;; ============================================================
;;; Display Functions
;;; ============================================================

;;; print-tagged : FS × Symbol × (U String #t) → void
;;; Print all posts with a specific tag.
(define (print-tagged fs key value)
  (let ([posts (find-tagged fs key value)])
    (if (null? posts)
        (display (format "No posts found with @~a~a\n"
                        key
                        (if (string? value)
                            (string-append ":" value)
                            "")))
        (begin
          (display (format "=== Posts with @~a~a (~a found) ===\n\n"
                          key
                          (if (string? value)
                              (string-append ":" value)
                              "")
                          (length posts)))
          (for-each
            (lambda (post)
              (display (format-post post))
              (display "---\n\n"))
            posts)))))

;;; print-tag-histogram : FS → void
;;; Display tag frequency histogram.
(define (print-tag-histogram fs)
  (let ([hist (tag-histogram fs)])
    (display "=== Tag Frequency ===\n\n")
    (if (null? hist)
        (display "No tags found.\n")
        (for-each
          (lambda (pair)
            (display (format "  @~a: ~a\n" (car pair) (cdr pair))))
          hist))
    (newline)))

;;; print-tags : FS → void
;;; Display all unique tags in use.
(define (print-tags fs)
  (let ([tags (list-all-tags fs)])
    (display "=== Tags in Use ===\n\n")
    (if (null? tags)
        (display "No tags found.\n")
        (for-each
          (lambda (tag)
            (display (format "  @~a\n" tag)))
          tags))
    (newline)))

;;; ============================================================
;;; Convenience Aliases
;;; ============================================================

;;; find : Symbol × String → (Listof Alist)
;;; Convenience function using default FS.
;;; Usage: (find @status 'complete)
;;; Note: Requires (fs) to be defined (from shell/repl.ss)
(define (find-by-tag key value)
  (find-tagged (fs) key value))

;;; tags : → void
;;; Show all tags (convenience wrapper).
(define (tags)
  (print-tags (fs)))

;;; tag-report : → void
;;; Show tag histogram (convenience wrapper).
(define (tag-report)
  (print-tag-histogram (fs)))
