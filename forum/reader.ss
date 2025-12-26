;;; forum/reader.ss — Forum Reading Convenience Tools
;;;
;;; Higher-level convenience functions for browsing and interacting
;;; with the Merkle log forum.
;;;
;;; This is Shell code: uses IO, defensive patterns.
;;;
;;; Dependencies (must be loaded before this file):
;;;   core/block.ss
;;;   core/sha256.ss
;;;   shell/fs.ss
;;;   forum/tools.ss

;;; ============================================================
;;; Printing Posts
;;; ============================================================

;;; print-latest : FS × Symbol × Nat → void
;;; Print the N most recent posts from a channel.
(define (print-latest fs channel n)
  (let ([posts (collect-channel fs channel)])
    (cond
      [(null? posts)
       (display (format "Channel #~a is empty.\n" channel))]
      [else
       (display (format "=== Latest ~a posts from #~a ===\n\n"
                       (min n (length posts)) channel))
       (let ([recent (take (min n (length posts)) posts)])
         (for-each
           (lambda (post)
             (display (format-post post))
             (display "---\n\n"))
           recent))])))

;;; print-all : FS × Symbol → void
;;; Print all posts in a channel (newest first).
(define (print-all fs channel)
  (let ([posts (collect-channel fs channel)])
    (cond
      [(null? posts)
       (display (format "Channel #~a is empty.\n" channel))]
      [else
       (display (format "=== All posts from #~a (~a total) ===\n\n"
                       channel (length posts)))
       (for-each
         (lambda (post)
           (display (format-post post))
           (display "---\n\n"))
         posts)])))

;;; ============================================================
;;; Searching
;;; ============================================================

;;; search-posts : FS × Symbol × String → (List Alist)
;;; Find posts containing a substring in their body.
;;; Search is case-sensitive.
(define (search-posts fs channel substring)
  (let ([posts (collect-channel fs channel)])
    (filter
      (lambda (post)
        (let ([body (cdr (assq 'body post))])
          (string-contains? body substring)))
      posts)))

;;; string-contains? : String × String → Boolean
;;; Check if haystack contains needle.
(define (string-contains? haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (and (<= nlen hlen)
         (let loop ([i 0])
           (cond
             [(> (+ i nlen) hlen) #f]
             [(string=? (substring haystack i (+ i nlen)) needle) #t]
             [else (loop (+ i 1))])))))

;;; ============================================================
;;; Filtering by Author
;;; ============================================================

;;; posts-by-author : FS × Symbol × Symbol → (List Alist)
;;; Get all posts by a specific author in a channel.
(define (posts-by-author fs channel author)
  (let ([posts (collect-channel fs channel)])
    (filter
      (lambda (post)
        (eq? (cdr (assq 'author post)) author))
      posts)))

;;; ============================================================
;;; Channel Statistics
;;; ============================================================

;;; channel-stats : FS × Symbol → Alist
;;; Return statistics about a channel:
;;;   - post-count: number of posts
;;;   - authors: list of unique authors
;;;   - first-post: timestamp of oldest post
;;;   - last-post: timestamp of newest post
(define (channel-stats fs channel)
  (let ([posts (collect-channel fs channel)])
    (if (null? posts)
        '((post-count . 0)
          (authors . ())
          (first-post . #f)
          (last-post . #f))
        (let* ([count (length posts)]
               [authors (unique-authors posts)]
               [first (last posts)]
               [last (car posts)]
               [first-time (cdr (assq 'timestamp first))]
               [last-time (cdr (assq 'timestamp last))])
          `((post-count . ,count)
            (authors . ,authors)
            (first-post . ,first-time)
            (last-post . ,last-time))))))

;;; unique-authors : (List Alist) → (List Symbol)
;;; Extract unique author names from posts.
(define (unique-authors posts)
  (let loop ([posts posts] [seen '()])
    (if (null? posts)
        seen
        (let ([author (cdr (assq 'author (car posts)))])
          (if (memq author seen)
              (loop (cdr posts) seen)
              (loop (cdr posts) (cons author seen)))))))

;;; ============================================================
;;; Forum Summary
;;; ============================================================

;;; forum-summary : FS → void
;;; Print a summary of all channels with post counts.
(define (forum-summary fs)
  (let ([channels (list-channels fs)])
    (display "=== Forum Summary ===\n\n")
    (if (null? channels)
        (display "No channels found.\n")
        (begin
          (display (format "~a channel~a found:\n\n"
                          (length channels)
                          (if (= (length channels) 1) "" "s")))
          (for-each
            (lambda (channel)
              (let* ([posts (collect-channel fs channel)]
                     [count (length posts)]
                     [authors (unique-authors posts)])
                (display (format "#~a: ~a post~a, ~a author~a\n"
                                channel
                                count
                                (if (= count 1) "" "s")
                                (length authors)
                                (if (= (length authors) 1) "" "s")))))
            channels)))
    (newline)))

;;; ============================================================
;;; Utilities
;;; ============================================================

;;; take is provided by core/prelude.ss as (take n lst)

;;; last : (List α) → α
;;; Return last element of list.
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; ============================================================
;;; Pretty Printing Enhanced
;;; ============================================================

;;; print-post-summary : Alist → void
;;; Print a one-line summary of a post.
(define (print-post-summary post)
  (let ([author (cdr (assq 'author post))]
        [timestamp (cdr (assq 'timestamp post))]
        [body (cdr (assq 'body post))])
    (let ([preview (truncate-string body 50)])
      (display (format "~a @ ~a: ~a\n" author timestamp preview)))))

;;; truncate-string : String × Nat → String
;;; Truncate string to max length, adding "..." if truncated.
(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

;;; print-search-results : (List Alist) × String → void
;;; Pretty-print search results with context.
(define (print-search-results posts query)
  (display (format "=== Search results for \"~a\" ===\n\n" query))
  (if (null? posts)
      (display "No matches found.\n")
      (begin
        (display (format "Found ~a match~a:\n\n"
                        (length posts)
                        (if (= (length posts) 1) "" "es")))
        (for-each
          (lambda (post)
            (display (format-post post))
            (display "---\n\n"))
          posts))))
