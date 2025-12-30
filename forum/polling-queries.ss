;;; polling-queries.ss
;;; Real filesystem queries for daemon polling of agent consultation tags

(load "forum/tag-parser.ss")
(load "forum/tools.ss")
(load "thimble/fs.ss")

(define (iso8601-to-unix-timestamp iso-str)
  "Convert ISO 8601 timestamp to Unix seconds (simplified)"
  (if (and (string? iso-str) (>= (string-length iso-str) 19))
      (let* ([year (string->number (substring iso-str 0 4))]
             [month (string->number (substring iso-str 5 7))]
             [day (string->number (substring iso-str 8 10))]
             [hour (string->number (substring iso-str 11 13))]
             [minute (string->number (substring iso-str 14 16))]
             [second (string->number (substring iso-str 17 19))]
             [days-in-months '(0 31 59 90 120 151 181 212 243 273 304 334)]
             [leap-years (quotient (- year 1969) 4)]
             [century-adjustments (quotient (- year 1) 100)]
             [days-since-1970 (+ (* (- year 1970) 365)
                                 leap-years
                                 (list-ref days-in-months (- month 1))
                                 day
                                 -1)])
            (+ (* days-since-1970 86400)
               (* hour 3600)
               (* minute 60)
               second))
      0))

(define (posts-since-timestamp since-seconds)
  "Get all posts from forum channels since timestamp"
  (let ([fs (mint-fs-capability ".store")])
       (let loop ([channels '(philosophy engineering poetry art design requests wishlist)]
                  [result '()])
            (if (null? channels)
                (reverse result)
                (let* ([channel (car channels)]
                       [all-posts (collect-channel fs channel)])
                      (let inner-loop ([posts all-posts]
                                       [accum result])
                           (if (null? posts)
                               (loop (cdr channels) accum)
                               (let* ([post (car posts)]
                                      [ts-str (cdr (assoc 'timestamp post))]
                                      [ts (iso8601-to-unix-timestamp ts-str)])
                                     (if (>= ts since-seconds)
                                         (inner-loop (cdr posts)
                                                     (cons
                                                      (list (cons 'channel channel)
                                                            (cons 'author (cdr (assoc 'author post)))
                                                            (cons 'timestamp ts-str)
                                                            (cons 'body (cdr (assoc 'body post))))
                                                      accum))
                                         (inner-loop (cdr posts) accum))))))))))

(define (find-posts-with-agent-tags since-seconds)
  "Find posts with agent tags since timestamp"
  (let loop ([posts (posts-since-timestamp since-seconds)]
             [result '()])
       (if (null? posts)
           (reverse result)
           (let* ([post (car posts)]
                  [body (cdr (assoc 'body post))]
                  [tags (extract-agent-tags body)])
                 (if (and tags (not (null? tags)))
                     (loop (cdr posts)
                           (cons (list (cons 'channel (cdr (assoc 'channel post)))
                                       (cons 'author (cdr (assoc 'author post)))
                                       (cons 'timestamp (cdr (assoc 'timestamp post)))
                                       (cons 'tags tags))
                                 result))
                     (loop (cdr posts) result))))))

;;; Functions are available after loading:
;;; - posts-since-timestamp
;;; - find-posts-with-agent-tags
