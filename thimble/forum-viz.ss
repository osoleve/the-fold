;;; shell/forum-viz.ss — Forum Visualization
;;;
;;; Visualize the Merkle log structure of forum channels.
;;; Display post chains, trees, and relationships.
;;;
;;; This is Shell code: uses IO, defensive.

;;; ============================================================
;;; ASCII Tree Drawing
;;; ============================================================

;;; draw-forum-tree : Symbol → void
;;; Draw an ASCII tree of a forum channel's post structure.
(define (draw-forum-tree channel)
  (let* ([fs (mint-fs-capability ".store")]
         [posts (collect-channel fs channel)]
         [head-hash (fs-read-head fs channel)])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display (format "║  📊 FORUM TREE: #~a~a║\n"
                    channel
                    (make-string (max 0 (- 47 (string-length (symbol->string channel))))
                                #\space)))
    (display "╠══════════════════════════════════════════════════════════════════╣\n")

    (if (null? posts)
        (display "║  (empty channel)                                                 ║\n")
        (begin
          (display (format "║  Total posts: ~a~a║\n"
                          (length posts)
                          (make-string (max 0 (- 51 (string-length (number->string (length posts)))))
                                      #\space)))
          (display (format "║  Head: ~a...~a║\n"
                          (substring (hash->hex head-hash) 0 8)
                          (make-string 43 #\space)))))

    (display "╚══════════════════════════════════════════════════════════════════╝\n\n")

    (unless (null? posts)
      ;; Show chronological chain
      (display "  Chronological chain (oldest → newest):\n\n")
      (let loop ([ps (reverse posts)]  ; Reverse to show oldest first
                 [indent ""])
        (unless (null? ps)
          (let* ([p (car ps)]
                 [author (cdr (assq 'author p))]
                 [timestamp (cdr (assq 'timestamp p))]
                 [body (cdr (assq 'body p))]
                 [title (extract-title body)])
            (display (format "  ~a├─ ~a by ~a (~a)\n"
                            indent
                            title
                            author
                            (substring timestamp 0 10)))
            (loop (cdr ps) indent)))))))

;;; extract-title : String → String
;;; Extract first line or heading from post body.
(define (extract-title body)
  (let ([lines (string-split body #\newline)])
    (if (null? lines)
        "(untitled)"
        (let ([first (car lines)])
          (cond
            [(string-prefix? "## " first)
             (substring first 3 (min (string-length first) 50))]
            [(string-prefix? "# " first)
             (substring first 2 (min (string-length first) 50))]
            [else
             (substring first 0 (min (string-length first) 50))])))))

;;; string-prefix? is now provided by shell/fs.ss

;;; string-split : String × Char → List<String>
(define (string-split str delim)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (if (null? current)
                   result
                   (cons (list->string (reverse current)) result)))]
      [(char=? (car chars) delim)
       (loop (cdr chars)
             '()
             (cons (list->string (reverse current)) result))]
      [else
       (loop (cdr chars)
             (cons (car chars) current)
             result)])))

;;; ============================================================
;;; Post Statistics
;;; ============================================================

;;; forum-stats : → void
;;; Display statistics across all forum channels.
(define (forum-stats)
  (let* ([fs (mint-fs-capability ".store")]
         [channels '(chat engineering art poetry philosophy design arena wishlist)]
         [channel-counts (map (lambda (ch)
                               (cons ch (length (collect-channel fs ch))))
                             channels)]
         [total (apply + (map cdr channel-counts))])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display "║                    📊 FORUM STATISTICS                           ║\n")
    (display "╠══════════════════════════════════════════════════════════════════╣\n")
    (display (format "║  Total posts: ~a~a║\n"
                    total
                    (make-string (max 0 (- 49 (string-length (number->string total))))
                                #\space)))
    (display "╠══════════════════════════════════════════════════════════════════╣\n")

    (for-each
      (lambda (pair)
        (let ([ch (car pair)]
              [count (cdr pair)])
          (when (> count 0)
            (let* ([bar-width (quotient (* count 40) (max total 1))]
                   [bar (make-string bar-width #\█)]
                   [padding (make-string (- 40 bar-width) #\space)])
              (display (format "║  #~a: ~a~a ~a ~a║\n"
                              ch
                              (make-string (max 0 (- 12 (string-length (symbol->string ch)))) #\space)
                              bar
                              padding
                              count))))))
      (list-sort (lambda (a b) (> (cdr a) (cdr b))) channel-counts))

    (display "╚══════════════════════════════════════════════════════════════════╝\n")))

;;; ============================================================
;;; Author Activity
;;; ============================================================

;;; author-activity : → void
;;; Display post counts by author.
(define (author-activity)
  (let* ([fs (mint-fs-capability ".store")]
         [channels '(chat engineering art poetry philosophy design arena wishlist)]
         [all-posts (apply append (map (lambda (ch) (collect-channel fs ch)) channels))]
         [authors (get-unique-authors all-posts)]
         [author-counts (map (lambda (author)
                              (cons author (count-posts-by-author all-posts author)))
                            authors)]
         [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b))) author-counts)]
         [total (length all-posts)])

    (display "\n")
    (display "╔══════════════════════════════════════════════════════════════════╗\n")
    (display "║                    📊 AUTHOR ACTIVITY                            ║\n")
    (display "╠══════════════════════════════════════════════════════════════════╣\n")

    (for-each
      (lambda (pair)
        (let* ([author (car pair)]
               [count (cdr pair)]
               [bar-width (quotient (* count 35) (max total 1))]
               [bar (make-string bar-width #\█)]
               [padding (make-string (- 35 bar-width) #\space)])
          (display (format "║  ~a: ~a~a ~a ~a║\n"
                          author
                          (make-string (max 0 (- 15 (string-length (symbol->string author)))) #\space)
                          bar
                          padding
                          count))))
      sorted)

    (display "╚══════════════════════════════════════════════════════════════════╝\n")))

;;; get-unique-authors : List<Post> → List<Symbol>
(define (get-unique-authors posts)
  (delete-duplicates
    (map (lambda (p) (cdr (assq 'author p))) posts)))

;;; delete-duplicates : List → List
(define (delete-duplicates lst)
  (let loop ([l lst] [seen '()])
    (cond
      [(null? l) (reverse seen)]
      [(memq (car l) seen) (loop (cdr l) seen)]
      [else (loop (cdr l) (cons (car l) seen))])))

;;; count-posts-by-author : List<Post> × Symbol → Number
(define (count-posts-by-author posts author)
  (length (filter (lambda (p)
                   (eq? (cdr (assq 'author p)) author))
                 posts)))

;;; filter : Procedure × List → List
(define (filter pred lst)
  (let loop ([l lst] [result '()])
    (cond
      [(null? l) (reverse result)]
      [(pred (car l)) (loop (cdr l) (cons (car l) result))]
      [else (loop (cdr l) result)])))

;;; ============================================================
;;; Help
;;; ============================================================

;;; forum-viz-help : → void
(define (forum-viz-help)
  (display "
  ╔══════════════════════════════════════════════════════════════════╗
  ║                    📊 FORUM VISUALIZATION HELP                   ║
  ╚══════════════════════════════════════════════════════════════════╝

  COMMANDS:
    (draw-forum-tree 'CHANNEL)  Visualize post chain for channel
    (forum-stats)               Show statistics across all channels
    (author-activity)           Show post counts by author
    (forum-viz-help)            Show this help

  EXAMPLES:
    (draw-forum-tree 'engineering)   ; Show engineering channel tree
    (draw-forum-tree 'poetry)        ; Show poetry channel tree
    (forum-stats)                    ; Overall forum statistics
    (author-activity)                ; Who's posting the most?

  CHANNELS:
    chat, engineering, art, poetry, philosophy, design, arena, wishlist
"))
