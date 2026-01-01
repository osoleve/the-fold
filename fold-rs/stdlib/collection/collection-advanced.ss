; ============================================
; Advanced Collection Data Structures
; ============================================
; Provides: Trie, Ring Buffer, Finger Tree
; These are persistent functional data structures

; ============================================
; Trie (Prefix Tree)
; ============================================
; Trie = (value . children-alist)
; where children-alist is ((char . trie) ...)

; trie-empty: Create empty trie
(trie-empty (cons #f '()))

; trie-value: Get value at trie node
(trie-value car)

; trie-children: Get children alist
(trie-children cdr)

; trie-insert: Insert key-value pair (key is list of chars)
(trie-insert (fix trie-insert
                  (fn (trie key value)
                      (if (null? key)
                          (cons value (trie-children trie))
                          (let ((c (car key))
                                (rest (cdr key))
                                (children (trie-children trie)))
                               (let ((child (assoc c children)))
                                    (if child
                                        (cons (trie-value trie)
                                              (assoc-set c (trie-insert (cdr child) rest value) children))
                                        (cons (trie-value trie)
                                              (cons (cons c (trie-insert trie-empty rest value)) children)))))))))

; trie-lookup: Look up key in trie
(trie-lookup (fix trie-lookup
                  (fn (trie key)
                      (if (null? key)
                          (trie-value trie)
                          (let ((c (car key))
                                (rest (cdr key))
                                (children (trie-children trie)))
                               (let ((child (assoc c children)))
                                    (if child
                                        (trie-lookup (cdr child) rest)
                                        #f)))))))

; trie-has-key?: Check if key exists
(trie-has-key? (fn (trie key)
                   (not (not (trie-lookup trie key)))))

; string->key: Convert string to key (list of chars)
(string->key string->list)

; trie-insert-string: Insert with string key
(trie-insert-string (fn (trie str value)
                        (trie-insert trie (string->key str) value)))

; trie-lookup-string: Lookup with string key
(trie-lookup-string (fn (trie str)
                        (trie-lookup trie (string->key str))))

; ============================================
; Ring Buffer (Circular Buffer)
; ============================================
; Ring = (capacity write-pos read-pos buffer-list)

; ring-make: Create ring buffer with capacity
(ring-make (fn (capacity)
               (list capacity 0 0 (replicate capacity #f))))

; ring-capacity: Get buffer capacity
(ring-capacity car)

; ring-write-pos: Get write position
(ring-write-pos cadr)

; ring-read-pos: Get read position
(ring-read-pos caddr)

; ring-buffer: Get buffer list
(ring-buffer cadddr)

; ring-empty?: Check if buffer is empty
(ring-empty? (fn (r)
                 (= (ring-write-pos r) (ring-read-pos r))))

; ring-full?: Check if buffer is full
(ring-full? (fn (r)
                (= (mod (+ (ring-write-pos r) 1) (ring-capacity r))
                   (ring-read-pos r))))

; ring-size: Current number of elements
(ring-size (fn (r)
               (mod (- (ring-write-pos r) (ring-read-pos r) (- (ring-capacity r)))
                    (ring-capacity r))))

; ring-write: Write element to buffer (drops if full)
(ring-write (fn (x r)
                (if (ring-full? r)
                    r
                    (let ((cap (ring-capacity r))
                          (wp (ring-write-pos r))
                          (rp (ring-read-pos r))
                          (buf (ring-buffer r)))
                         (list cap
                               (mod (+ wp 1) cap)
                               rp
                               (list-set wp x buf))))))

; ring-read: Read element from buffer
; Returns (new-ring . value) or (ring . #f) if empty
(ring-read (fn (r)
               (if (ring-empty? r)
                   (cons r #f)
                   (let ((cap (ring-capacity r))
                         (wp (ring-write-pos r))
                         (rp (ring-read-pos r))
                         (buf (ring-buffer r)))
                        (cons (list cap wp (mod (+ rp 1) cap) buf)
                              (list-ref buf rp))))))

; ring-peek: Peek at next element without removing
(ring-peek (fn (r)
               (if (ring-empty? r)
                   #f
                   (list-ref (ring-buffer r) (ring-read-pos r)))))

; ring-to-list: Convert to list (oldest first)
(ring-to-list (fn (r)
                  ((fix collect-rec
                        (fn (rng acc)
                            (if (ring-empty? rng)
                                (reverse acc)
                                (let ((result (ring-read rng)))
                                     (collect-rec (car result) (cons (cdr result) acc))))))
                   r '())))

; ============================================
; Finger Tree (Simplified Deque Operations)
; ============================================
; FingerTree = (empty) | (single x) | (deep prefix middle suffix)

; finger-empty: Empty finger tree
(finger-empty (list 'empty))

; finger-empty?: Check if empty
(finger-empty? (fn (ft)
                   (eq? (car ft) 'empty)))

; finger-single: Single element tree
(finger-single (fn (x)
                   (list 'single x)))

; finger-single?: Check if single
(finger-single? (fn (ft)
                    (eq? (car ft) 'single)))

; finger-deep: Deep tree with prefix, middle, suffix
(finger-deep (fn (prefix middle suffix)
                 (list 'deep prefix middle suffix)))

; finger-push-front: Add to front
(finger-push-front (fn (x ft)
                       (if (finger-empty? ft)
                           (finger-single x)
                           (if (finger-single? ft)
                               (finger-deep (list x) finger-empty (list (cadr ft)))
                               (finger-deep (cons x (cadr ft))
                                            (caddr ft)
                                            (cadddr ft))))))

; finger-push-back: Add to back
(finger-push-back (fn (x ft)
                      (if (finger-empty? ft)
                          (finger-single x)
                          (if (finger-single? ft)
                              (finger-deep (list (cadr ft)) finger-empty (list x))
                              (finger-deep (cadr ft)
                                           (caddr ft)
                                           (snoc (cadddr ft) x))))))

; finger-peek-front: Get front element
(finger-peek-front (fn (ft)
                       (if (finger-empty? ft)
                           #f
                           (if (finger-single? ft)
                               (cadr ft)
                               (car (cadr ft))))))

; finger-peek-back: Get back element
(finger-peek-back (fn (ft)
                      (if (finger-empty? ft)
                          #f
                          (if (finger-single? ft)
                              (cadr ft)
                              (last (cadddr ft))))))
