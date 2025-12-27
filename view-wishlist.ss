;;; View wishlist items
(load "thimble/repl.ss")

(printf "\n=== WISHLIST ITEMS ===\n\n")

;; Get all wishlist posts
(define wishlist-posts
  (filter
    (lambda (p) (equal? (assoc-ref (block-payload p) 'channel) "wishlist"))
    (read-forum-posts (fs))))

(printf "Found ~a wishlist items:\n\n" (length wishlist-posts))

;; Display each item
(for-each
  (lambda (post)
    (let ([payload (block-payload post)])
      (printf "─────────────────────────────────────────\n")
      (printf "Title: ~a\n" (assoc-ref payload 'title))
      (printf "Author: ~a\n" (assoc-ref payload 'author))
      (printf "Date: ~a\n" (assoc-ref payload 'timestamp))
      (printf "\nBody:\n~a\n\n"
        (let ([body (assoc-ref payload 'body)])
          (if (> (string-length body) 500)
              (string-append (substring body 0 500) "...")
              body)))))
  wishlist-posts)

(printf "─────────────────────────────────────────\n")
