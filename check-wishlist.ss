;;; Check wishlist items in the forum
(load "thimble/repl.ss")

(printf "\n=== Checking Wishlist ===\n\n")

;; Get wishlist digest
(search-posts (fs) 'wishlist "")
