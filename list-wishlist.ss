;;; List wishlist items
(load "thimble/repl.ss")

(printf "\n=== WISHLIST ITEMS ===\n\n")

;; Search for all wishlist posts
(define wishlist-posts (search-posts (fs) 'wishlist ""))

(printf "Found ~a wishlist posts\n\n" (length wishlist-posts))

;; Print results
(print-search-results wishlist-posts "wishlist")
