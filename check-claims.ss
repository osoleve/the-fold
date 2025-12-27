;;; Check if anyone has claimed wishlist items
(load "thimble/repl.ss")

(printf "\n=== Checking for Claimed Wishlist Items ===\n\n")

;; Search terms for the wishlist items
(define search-terms
  '("store api" "store-put" "store-get"
    "navigation" "find-blocks-by-tag"
    "string utilities" "string-split" "string-join"
    "query language" "datalog"
    "collection utilities" "map-blocks"
    "visualization export"
    "schema system"
    "testing framework"
    "block explorer"
    "graph algorithms"))

;; Search engineering channel for mentions
(printf "Searching engineering channel...\n")
(for-each
  (lambda (term)
    (let ([results (search-posts (fs) 'engineering term)])
      (when (> (length results) 0)
        (printf "\nFound ~a post(s) mentioning '~a':\n" (length results) term)
        (for-each
          (lambda (post)
            (let* ([payload (block-payload post)]
                   [title (assoc-ref payload 'title)])
              (printf "  - ~a\n" title)))
          results))))
  search-terms)

;; Search chat channel for claims
(printf "\n\nSearching chat for 'working on' or 'claiming'...\n")
(define claiming-posts
  (append
    (search-posts (fs) 'chat "working on")
    (search-posts (fs) 'chat "claiming")
    (search-posts (fs) 'chat "will build")
    (search-posts (fs) 'chat "I'll implement")))

(printf "Found ~a chat messages about claiming work\n" (length claiming-posts))
(for-each
  (lambda (post)
    (let* ([payload (block-payload post)]
           [body (assoc-ref payload 'body)]
           [author (assoc-ref payload 'author)])
      (printf "\n~a: ~a\n" author
        (if (> (string-length body) 200)
            (string-append (substring body 0 200) "...")
            body))))
  (take claiming-posts (min 10 (length claiming-posts))))
