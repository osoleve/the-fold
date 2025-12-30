; Stream utilities - lazy sequences
; Streams are represented as (list 'stream head tail-thunk)
; where tail-thunk is a zero-argument function that produces the next stream

; stream-cons: Create a lazy stream node
(stream-cons (fn (head tail-thunk)
                 (list 'stream head tail-thunk)))

; stream-head: Get head of stream
(stream-head (fn (s)
                 (if (and (pair? s) (eq? (car s) 'stream))
                     (cadr s)
                     (error "not a stream"))))

; stream-tail: Get tail of stream (forces thunk)
(stream-tail (fn (s)
                 (if (and (pair? s) (eq? (car s) 'stream))
                     ((caddr s))
                     (error "not a stream"))))

; stream-null: Empty stream marker
(stream-null (list 'stream-null))

; stream-null?: Check if stream is empty
(stream-null? (fn (s)
                  (and (pair? s) (eq? (car s) 'stream-null))))

; stream-take: Take n elements from stream
(stream-take (fix stream-take
                  (fn (n s)
                      (if (or (<= n 0) (stream-null? s))
                          '()
                          (cons (stream-head s) (stream-take (- n 1) (stream-tail s)))))))

; stream-map: Map function over stream
(stream-map (fix stream-map
                 (fn (f s)
                     (if (stream-null? s)
                         stream-null
                         (stream-cons (f (stream-head s))
                                      (fn () (stream-map f (stream-tail s))))))))

; stream-filter: Filter stream by predicate
(stream-filter (fix stream-filter
                    (fn (p s)
                        (if (stream-null? s)
                            stream-null
                            (if (p (stream-head s))
                                (stream-cons (stream-head s)
                                             (fn () (stream-filter p (stream-tail s))))
                                (stream-filter p (stream-tail s)))))))

; stream-from: Infinite stream starting at n
(stream-from (fix stream-from
                  (fn (n)
                      (stream-cons n (fn () (stream-from (+ n 1)))))))

; stream-iterate: Infinite stream by iterating function
(stream-iterate (fix stream-iterate
                     (fn (f x)
                         (stream-cons x (fn () (stream-iterate f (f x)))))))

; stream-repeat: Infinite stream of same value
(stream-repeat (fix stream-repeat
                    (fn (x)
                        (stream-cons x (fn () (stream-repeat x))))))

; stream-zip-with: Zip two streams with function
(stream-zip-with (fix stream-zip-with
                      (fn (f s1 s2)
                          (if (or (stream-null? s1) (stream-null? s2))
                              stream-null
                              (stream-cons (f (stream-head s1) (stream-head s2))
                                           (fn () (stream-zip-with f (stream-tail s1) (stream-tail s2))))))))
