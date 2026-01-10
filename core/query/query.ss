;;; core/query/query.ss --- Tag Query Tools
;;;
;;; Layer 2 of the metadata system: querying posts by tags.
;;;
;;; Dependencies:
;;;   - core/query/patterns-parse.ss
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
;;;   - core/query/patterns-parse.ss (extract-tags, has-tag?, get-tag)
;;;   - forum/tools.ss (collect-channel, list-channels)
;;;   - shell/fs.ss (fs-fetch, etc.)

;;; ============================================================
;;; Finding Posts by Tag
;;; ============================================================

;;; safe-get-body : Alist → String
;;; Safely extract body field from post, returning empty string if missing.
(define (safe-get-body post)
  (let ([body-pair (assq 'body post)])
       (if body-pair
           (cdr body-pair)
           "")))

;;; find-tagged : FSCap × Symbol × (Union String Boolean) → (List Alist)
;;; Find all forum posts that have a specific tag.
;;; If value is #t, matches any value for that key.
;;;
;;; Example:
;;;   (find-tagged (fs) 'status "complete")
;;;   -> list of posts with @status:complete
(define (find-tagged fs key value)
  (let ([all-posts (collect-all-posts fs)])
       (filter
        (lambda (post)
                (let* ([body (safe-get-body post)]
                       [tags (extract-tags body)])
                      (and (has-tag? tags key)
                           (or (eq? value #t)
                               (equal? (get-tag tags key) value)))))
        all-posts)))

;;; find-tagged-any : FSCap × Symbol → (List Alist)
;;; Find all posts that have a tag key, regardless of value.
(define (find-tagged-any fs key)
  (find-tagged fs key #t))

;;; collect-all-posts : FSCap → (List Alist)
;;; Collect all posts from all channels.
(define (collect-all-posts fs)
  (let ([channels (list-channels fs)])
       (apply append
              (map (lambda (ch) (collect-channel fs ch))
                   channels))))

;;; ============================================================
;;; Tag Inventory
;;; ============================================================

;;; list-all-tags : FSCap → (List Symbol)
;;; Get a list of all unique tag keys used across all posts.
(define (list-all-tags fs)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                       (let ([body (safe-get-body post)])
                                            (extract-tags body)))
                               all-posts))]
         [keys (map car all-tags)])
        (unique keys)))

;;; unique : (List α) → (List α)
;;; Remove duplicates from a list, preserving order (first occurrence wins).
;;; Uses hash table for O(N) complexity instead of O(N^2) with member.
(define (unique lst)
  (let ([seen (make-hashtable equal-hash equal?)])
       (let loop ([items lst] [acc '()])
            (if (null? items)
                (reverse acc)
                (let ([x (car items)])
                     (if (hashtable-contains? seen x)
                         (loop (cdr items) acc)
                         (begin
                          (hashtable-set! seen x #t)
                          (loop (cdr items) (cons x acc)))))))))

;;; tag-histogram : FSCap → (List (Pair Symbol Nat))
;;; Count occurrences of each tag key.
;;; Returns alist sorted by frequency (highest first).
(define (tag-histogram fs)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                       (let ([body (safe-get-body post)])
                                            (extract-tags body)))
                               all-posts))]
         [keys (map car all-tags)]
         [counts (count-occurrences keys)])
        (sort-by-count counts)))

;;; count-occurrences : (List Symbol) → (List (Pair Symbol Nat))
;;; Count how many times each symbol appears.
;;; Uses hash table for O(N) complexity instead of O(N^2) with remove-assq.
(define (count-occurrences symbols)
  (let ([counts (make-hashtable equal-hash equal?)])
       (for-each (lambda (x)
                         (hashtable-set! counts x
                                         (+ 1 (hashtable-ref counts x 0))))
                 symbols)
       (let-values ([(keys vals) (hashtable-entries counts)])
                   (map cons (vector->list keys) (vector->list vals)))))

;;; remove-assq : Symbol × Alist → Alist
;;; Remove first pair with matching key.
(define (remove-assq key alist)
  (filter (lambda (pair) (not (eq? (car pair) key))) alist))

;;; sort-by-count : (List (Pair Symbol Nat)) → (List (Pair Symbol Nat))
;;; Sort by count, highest first.
(define (sort-by-count pairs)
  (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs))

;;; ============================================================
;;; Tag Value Analysis
;;; ============================================================

;;; tag-values : FSCap × Symbol → (List String)
;;; Get all unique values used with a specific tag key.
(define (tag-values fs key)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                       (let ([body (safe-get-body post)])
                                            (extract-tags body)))
                               all-posts))]
         [matching (filter (lambda (t) (eq? (car t) key)) all-tags)]
         [values (map cdr matching)]
         [string-values (filter string? values)])
        (unique string-values)))

;;; tag-value-histogram : FSCap × Symbol → (List (Pair String Nat))
;;; Count occurrences of each value for a tag key.
(define (tag-value-histogram fs key)
  (let* ([all-posts (collect-all-posts fs)]
         [all-tags (apply append
                          (map (lambda (post)
                                       (let ([body (safe-get-body post)])
                                            (extract-tags body)))
                               all-posts))]
         [matching (filter (lambda (t) (eq? (car t) key)) all-tags)]
         [values (filter string? (map cdr matching))]
         [counts (count-string-occurrences values)])
        (sort-by-count-strings counts)))

;;; count-string-occurrences : (List String) → (List (Pair String Nat))
;;; Uses hash table for O(N) complexity instead of O(N^2) with remove-assoc.
(define (count-string-occurrences strings)
  (let ([counts (make-hashtable equal-hash equal?)])
       (for-each (lambda (s)
                         (hashtable-set! counts s
                                         (+ 1 (hashtable-ref counts s 0))))
                 strings)
       (let-values ([(keys vals) (hashtable-entries counts)])
                   (map cons (vector->list keys) (vector->list vals)))))

;;; remove-assoc : String × Alist → Alist
(define (remove-assoc key alist)
  (filter (lambda (pair) (not (equal? (car pair) key))) alist))

;;; sort-by-count-strings : (List (Pair String Nat)) → (List (Pair String Nat))
(define (sort-by-count-strings pairs)
  (list-sort (lambda (a b) (> (cdr a) (cdr b))) pairs))

;;; ============================================================
;;; Display Functions
;;; ============================================================

;;; print-tagged : FSCap × Symbol × (Union String Boolean) → Void
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

;;; print-tag-histogram : FSCap → Void
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

;;; print-tags : FSCap → Void
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

;;; find-by-tag : Symbol × String → (List Alist)
;;; Convenience function using default FS.
;;; Usage: (find @status 'complete)
;;; Note: Requires (fs) to be defined (from shell/repl.ss)
(define (find-by-tag key value)
  (find-tagged (fs) key value))

;;; tags : → Void
;;; Show all tags (convenience wrapper).
(define (tags)
  (print-tags (fs)))

;;; tag-report : → Void
;;; Show tag histogram (convenience wrapper).
(define (tag-report)
  (print-tag-histogram (fs)))
