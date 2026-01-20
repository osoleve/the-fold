(load "lattice/info/entropy.ss")

(doc 'module 'coding)
(doc 'description "Coding theory functions: Huffman, arithmetic, LZ78, RLE, and channel coding")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'huffman-coding)

(define (make-huffman-leaf symbol prob)
  (doc 'type '(-> Symbol Real Node))
  (doc 'description "Create Huffman tree leaf node")
  (list 'leaf symbol prob))

(define (make-huffman-internal left right)
  (doc 'type '(-> Node Node Node))
  (doc 'description "Create Huffman tree internal node")
  (list 'internal (cons left right) (+ (huffman-prob left) (huffman-prob right))))

(define (huffman-leaf? node)
  (doc 'type '(-> Node Boolean))
  (eq? (car node) 'leaf))

(define (huffman-symbol node)
  (doc 'type '(-> Node Symbol))
  (cadr node))

(define (huffman-children node)
  (doc 'type '(-> Node (Pair Node Node)))
  (cadr node))

(define (huffman-prob node)
  (doc 'type '(-> Node Real))
  (caddr node))

(define (build-huffman-tree symbol-probs)
  (doc 'type '(-> (List (Pair Symbol Real)) Node))
  (doc 'description "Build Huffman tree from symbol-probability pairs")
  (if (null? symbol-probs)
      (make-huffman-leaf 'empty 0)
      (if (null? (cdr symbol-probs))
          ;; Single symbol: just return as leaf
          (make-huffman-leaf (caar symbol-probs) (cdar symbol-probs))
          ;; Build tree from priority queue
          (let ([leaves (map (lambda (sp)
                                     (make-huffman-leaf (car sp) (cdr sp)))
                             symbol-probs)])
               (huffman-build-loop (sort-by-prob leaves))))))

(define (huffman-build-loop nodes)
  (doc 'type '(-> (List Node) Node))
  (doc 'description "Main Huffman algorithm loop: combine two lowest-prob nodes")
  (if (null? (cdr nodes))
      (car nodes)  ; Only one node left: it's the root
      (let* ([sorted (sort-by-prob nodes)]
             [n1 (car sorted)]
             [n2 (cadr sorted)]
             [rest (cddr sorted)]
             [combined (make-huffman-internal n1 n2)])
            (huffman-build-loop (cons combined rest)))))

(define (sort-by-prob nodes)
  (doc 'type '(-> (List Node) (List Node)))
  (doc 'description "Sort nodes by probability (ascending)")
  (list-sort (lambda (a b) (< (huffman-prob a) (huffman-prob b))) nodes))

(define (list-sort less? lst)
  (doc 'type '(-> (-> α α Boolean) (List α) (List α)))
  (doc 'description "Simple insertion sort for small alphabets")
  (if (null? lst)
      '()
      (insert-sorted less? (car lst) (list-sort less? (cdr lst)))))

(define (insert-sorted less? x sorted)
  (doc 'type '(-> (-> α α Boolean) α (List α) (List α)))
  (if (null? sorted)
      (list x)
      (if (less? x (car sorted))
          (cons x sorted)
          (cons (car sorted) (insert-sorted less? x (cdr sorted))))))

(define (huffman-codes tree)
  (doc 'type '(-> Node (List (Pair Symbol String))))
  (doc 'description "Extract code assignments from Huffman tree")
  (huffman-codes-helper tree ""))

(define (huffman-codes-helper node prefix)
  (doc 'type '(-> Node String (List (Pair Symbol String))))
  (if (huffman-leaf? node)
      (list (cons (huffman-symbol node) prefix))
      (let* ([children (huffman-children node)]
             [left (car children)]
             [right (cdr children)])
            (append (huffman-codes-helper left (string-append prefix "0"))
                    (huffman-codes-helper right (string-append prefix "1"))))))

(define (huffman-encode message code-table)
  (doc 'type '(-> (List Symbol) (List (Pair Symbol String)) String))
  (doc 'description "Encode message using Huffman code table")
  (apply string-append
         (map (lambda (sym)
                      (let ([entry (assq sym code-table)])
                           (if entry
                               (cdr entry)
                               (error "huffman-encode" "symbol not in code table" sym))))
              message)))

(define (huffman-decode bitstring tree)
  (doc 'type '(-> String Node (List Symbol)))
  (doc 'description "Decode bitstring using Huffman tree")
  (huffman-decode-helper bitstring tree tree '()))

(define (huffman-decode-helper bits root current acc)
  (doc 'type '(-> String Node Node (List Symbol) (List Symbol)))
  (cond
   [(and (string=? bits "") (huffman-leaf? current))
    (reverse (cons (huffman-symbol current) acc))]
   [(string=? bits "")
    (if (eq? current root)
        (reverse acc)
        (error "huffman-decode" "incomplete code"))]
   [(huffman-leaf? current)
    (huffman-decode-helper bits root root (cons (huffman-symbol current) acc))]
   [else
    (let* ([bit (string-ref bits 0)]
           [rest (substring bits 1 (string-length bits))]
           [children (huffman-children current)]
           [next (if (char=? bit #\0) (car children) (cdr children))])
          (huffman-decode-helper rest root next acc))]))

(define (huffman-average-length code-table symbol-probs)
  (doc 'type '(-> (List (Pair Symbol String)) (List (Pair Symbol Real)) Real))
  (doc 'description "Compute average code length given codes and probabilities")
  (fold-left + 0
             (map (lambda (sp)
                          (let* ([sym (car sp)]
                                 [prob (cdr sp)]
                                 [code (cdr (assq sym code-table))])
                                (* prob (string-length code))))
                  symbol-probs)))

(define (huffman-efficiency code-table symbol-probs)
  (doc 'type '(-> (List (Pair Symbol String)) (List (Pair Symbol Real)) Real))
  (doc 'description "Ratio of entropy to average code length (ideally close to 1)")
  (let* ([probs (map cdr symbol-probs)]
         [h (entropy probs)]
         [avg-len (huffman-average-length code-table symbol-probs)])
        (if (= avg-len 0)
            1.0  ; Degenerate case
            (/ h avg-len))))

(doc 'section 'arithmetic-coding)

(define (arithmetic-encode message symbol-probs)
  (doc 'type '(-> (List Symbol) (List (Pair Symbol Real)) (Pair Real Real)))
  (doc 'description "Encode message to interval [low, high) via arithmetic coding")
  (let ([cumulative (build-cumulative-probs symbol-probs)])
       (arithmetic-encode-helper message cumulative 0.0 1.0)))

(define (arithmetic-encode-helper message cumulative low high)
  (doc 'type '(-> (List Symbol) (List (List Symbol Real Real)) Real Real (Pair Real Real)))
  (if (null? message)
      (cons low high)
      (let* ([sym (car message)]
             [range (- high low)]
             [sym-interval (assq sym cumulative)]
             [sym-low (cadr sym-interval)]
             [sym-high (caddr sym-interval)]
             [new-low (+ low (* range sym-low))]
             [new-high (+ low (* range sym-high))])
            (arithmetic-encode-helper (cdr message) cumulative new-low new-high))))

(define (build-cumulative-probs symbol-probs)
  (doc 'type '(-> (List (Pair Symbol Real)) (List (List Symbol Real Real))))
  (doc 'description "Build cumulative probability table for arithmetic coding")
  (let loop ([remaining symbol-probs] [cumsum 0.0] [acc '()])
       (if (null? remaining)
           (reverse acc)
           (let* ([sym (caar remaining)]
                  [prob (cdar remaining)]
                  [new-cumsum (+ cumsum prob)])
                 (loop (cdr remaining)
                       new-cumsum
                       (cons (list sym cumsum new-cumsum) acc))))))

(define (arithmetic-decode interval symbol-probs length)
  (doc 'type '(-> (Pair Real Real) (List (Pair Symbol Real)) Nat (List Symbol)))
  (doc 'description "Decode interval back to message of given length")
  (let ([cumulative (build-cumulative-probs symbol-probs)]
        [code (/ (+ (car interval) (cdr interval)) 2)])  ; Use midpoint
       (arithmetic-decode-helper code cumulative length '())))

(define (arithmetic-decode-helper code cumulative length acc)
  (doc 'type '(-> Real (List (List Symbol Real Real)) Nat (List Symbol) (List Symbol)))
  (if (= length 0)
      (reverse acc)
      (let* ([sym-interval (find-symbol-for-code code cumulative)]
             [sym (car sym-interval)]
             [sym-low (cadr sym-interval)]
             [sym-high (caddr sym-interval)]
             [new-code (/ (- code sym-low) (- sym-high sym-low))])
            (arithmetic-decode-helper new-code cumulative (- length 1) (cons sym acc)))))

(define (find-symbol-for-code code cumulative)
  (doc 'type '(-> Real (List (List Symbol Real Real)) (List Symbol Real Real)))
  (let loop ([remaining cumulative])
       (if (null? remaining)
           (error "find-symbol-for-code" "code out of range")
           (let* ([entry (car remaining)]
                  [low (cadr entry)]
                  [high (caddr entry)])
                 (if (and (>= code low) (< code high))
                     entry
                     (loop (cdr remaining)))))))

(define (arithmetic-code-length interval)
  (doc 'type '(-> (Pair Real Real) Real))
  (doc 'description "Bits needed to represent interval (theoretical)")
  (let ([width (- (cdr interval) (car interval))])
       (if (<= width 0)
           +inf.0
           (- (log2 width)))))

(doc 'section 'lempel-ziv-coding)

(define (lz78-encode message)
  (doc 'type '(-> (List Symbol) (List (Pair Nat Symbol))))
  (doc 'description "Encode using LZ78 dictionary-based compression")
  (lz78-encode-helper message '() 0 '()))

(define (lz78-encode-helper remaining dict next-index acc)
  (doc 'type '(-> (List Symbol) (List (Pair Nat (List Symbol))) Nat (List (Pair Nat Symbol)) (List (Pair Nat Symbol))))
  (if (null? remaining)
      (reverse acc)
      (let* ([match (lz78-find-longest-match remaining dict)]
             [match-idx (car match)]
             [match-len (cadr match)]
             [next-sym (if (< match-len (length remaining))
                           (list-ref remaining match-len)
                           'EOF)]
             [consume (+ match-len (if (eq? next-sym 'EOF) 0 1))]
             [new-phrase (take-n consume remaining)]
             [new-dict (append dict (list (cons next-index new-phrase)))]
             [new-remaining (drop-n consume remaining)])
            (lz78-encode-helper new-remaining
                                new-dict
                                (+ next-index 1)
                                (cons (cons match-idx next-sym) acc)))))

(define (lz78-find-longest-match message dict)
  (doc 'type '(-> (List Symbol) (List (Pair Nat (List Symbol))) (List Nat Nat)))
  (doc 'description "Find longest dictionary match")
  (let loop ([entries dict] [best-idx 0] [best-len 0])
       (if (null? entries)
           (list best-idx best-len)
           (let* ([entry (car entries)]
                  [idx (car entry)]
                  [phrase (cdr entry)]
                  [match-len (prefix-match-length phrase message)])
                 (if (> match-len best-len)
                     (loop (cdr entries) (+ idx 1) match-len)  ; idx+1 because 0 is empty match
                     (loop (cdr entries) best-idx best-len))))))

(define (prefix-match-length a b)
  (doc 'type '(-> (List α) (List α) Nat))
  (doc 'description "Length of common prefix between two lists")
  (let loop ([a a] [b b] [len 0])
       (if (or (null? a) (null? b))
           len
           (if (equal? (car a) (car b))
               (loop (cdr a) (cdr b) (+ len 1))
               len))))

(define (lz78-decode encoded)
  (doc 'type '(-> (List (Pair Nat Symbol)) (List Symbol)))
  (doc 'description "Decode LZ78 compressed data")
  (lz78-decode-helper encoded '() '()))

(define (lz78-decode-helper remaining dict acc)
  (doc 'type '(-> (List (Pair Nat Symbol)) (List (Pair Nat (List Symbol))) (List Symbol) (List Symbol)))
  (if (null? remaining)
      (reverse acc)
      (let* ([entry (car remaining)]
             [idx (car entry)]
             [sym (cdr entry)]
             [prefix (if (= idx 0)
                         '()
                         (cdr (list-ref dict (- idx 1))))]
             [phrase (if (eq? sym 'EOF)
                         prefix
                         (append prefix (list sym)))]
             [new-dict (append dict (list (cons (length dict) phrase)))])
            (lz78-decode-helper (cdr remaining) new-dict (append (reverse phrase) acc)))))

(define (take-n n lst)
  (doc 'type '(-> Nat (List α) (List α)))
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (drop-n n lst)
  (doc 'type '(-> Nat (List α) (List α)))
  (if (or (<= n 0) (null? lst))
      lst
      (drop-n (- n 1) (cdr lst))))

(doc 'section 'run-length-encoding)

(define (rle-encode message)
  (doc 'type '(-> (List α) (List (Pair α Nat))))
  (doc 'description "Run-length encode a sequence")
  (if (null? message)
      '()
      (rle-encode-helper (cdr message) (car message) 1 '())))

(define (rle-encode-helper remaining current count acc)
  (doc 'type '(-> (List α) α Nat (List (Pair α Nat)) (List (Pair α Nat))))
  (cond
   [(null? remaining)
    (reverse (cons (cons current count) acc))]
   [(equal? (car remaining) current)
    (rle-encode-helper (cdr remaining) current (+ count 1) acc)]
   [else
    (rle-encode-helper (cdr remaining)
                       (car remaining)
                       1
                       (cons (cons current count) acc))]))

(define (rle-decode encoded)
  (doc 'type '(-> (List (Pair α Nat)) (List α)))
  (doc 'description "Decode run-length encoded data")
  (apply append
         (map (lambda (pair)
                      (replicate (cdr pair) (car pair)))
              encoded)))

(define (replicate n x)
  (doc 'type '(-> Nat α (List α)))
  (if (<= n 0)
      '()
      (cons x (replicate (- n 1) x))))

(doc 'section 'channel-coding)

(define (parity-encode bits)
  (doc 'type '(-> (List Nat) (List Nat)))
  (doc 'description "Add even parity bit to end of message")
  (let ([parity (fold-left (lambda (acc b) (bitwise-xor acc b)) 0 bits)])
       (append bits (list parity))))

(define (parity-check bits)
  (doc 'type '(-> (List Nat) Boolean))
  (doc 'description "Check if message has valid even parity")
  (= (fold-left (lambda (acc b) (bitwise-xor acc b)) 0 bits) 0))

(define (parity-decode bits)
  (doc 'type '(-> (List Nat) (List Nat)))
  (doc 'description "Remove parity bit (returns message without checking)")
  (if (null? bits)
      '()
      (take-n (- (length bits) 1) bits)))

(define (bitwise-xor a b)
  (doc 'type '(-> Nat Nat Nat))
  (doc 'description "XOR for single bits (0 or 1)")
  (if (= a b) 0 1))

(doc 'section 'repetition-code)

(define (repetition-encode bits n)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Encode by repeating each bit n times")
  (apply append (map (lambda (b) (replicate n b)) bits)))

(define (repetition-decode bits n)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Decode by majority vote on each n-bit group")
  (if (null? bits)
      '()
      (cons (majority-vote (take-n n bits))
            (repetition-decode (drop-n n bits) n))))

(define (majority-vote bits)
  (doc 'type '(-> (List Nat) Nat))
  (doc 'description "Return the majority bit (0 or 1)")
  (let ([ones (length (filter (lambda (b) (= b 1)) bits))])
       (if (> ones (/ (length bits) 2)) 1 0)))

(doc 'section 'hamming-code)

(define (hamming74-encode data)
  (doc 'type '(-> (List Nat) (List Nat)))
  (doc 'description "Encode 4 data bits into 7-bit Hamming(7,4) codeword")
  (if (not (= (length data) 4))
      (error "hamming74-encode" "expected 4 data bits")
      (let* ([d1 (list-ref data 0)]
             [d2 (list-ref data 1)]
             [d3 (list-ref data 2)]
             [d4 (list-ref data 3)]
             ;; Parity bits
             [p1 (bitwise-xor (bitwise-xor d1 d2) d4)]
             [p2 (bitwise-xor (bitwise-xor d1 d3) d4)]
             [p4 (bitwise-xor (bitwise-xor d2 d3) d4)])
            ;; Output: p1 p2 d1 p4 d2 d3 d4 (positions 1-7)
            (list p1 p2 d1 p4 d2 d3 d4))))

(define (hamming74-decode codeword)
  (doc 'type '(-> (List Nat) (List Nat)))
  (doc 'description "Decode 7-bit Hamming codeword, correcting single bit error")
  (if (not (= (length codeword) 7))
      (error "hamming74-decode" "expected 7-bit codeword")
      (let* ([p1 (list-ref codeword 0)]
             [p2 (list-ref codeword 1)]
             [d1 (list-ref codeword 2)]
             [p4 (list-ref codeword 3)]
             [d2 (list-ref codeword 4)]
             [d3 (list-ref codeword 5)]
             [d4 (list-ref codeword 6)]
             ;; Syndrome calculation
             [s1 (bitwise-xor (bitwise-xor (bitwise-xor p1 d1) d2) d4)]
             [s2 (bitwise-xor (bitwise-xor (bitwise-xor p2 d1) d3) d4)]
             [s4 (bitwise-xor (bitwise-xor (bitwise-xor p4 d2) d3) d4)]
             [syndrome (+ s1 (* 2 s2) (* 4 s4))]
             ;; Correct error if syndrome is non-zero
             [corrected (if (= syndrome 0)
                            codeword
                            (flip-bit-at codeword (- syndrome 1)))])
            ;; Extract data bits from corrected codeword
            (list (list-ref corrected 2)
                  (list-ref corrected 4)
                  (list-ref corrected 5)
                  (list-ref corrected 6)))))

(define (flip-bit-at bits idx)
  (doc 'type '(-> (List Nat) Nat (List Nat)))
  (doc 'description "Flip the bit at given index")
  (let loop ([remaining bits] [i 0] [acc '()])
       (if (null? remaining)
           (reverse acc)
           (if (= i idx)
               (loop (cdr remaining) (+ i 1) (cons (bitwise-xor (car remaining) 1) acc))
               (loop (cdr remaining) (+ i 1) (cons (car remaining) acc))))))

(define (hamming74-syndrome codeword)
  (doc 'type '(-> (List Nat) Nat))
  (doc 'description "Calculate syndrome (0 means no error)")
  (let* ([p1 (list-ref codeword 0)]
         [p2 (list-ref codeword 1)]
         [d1 (list-ref codeword 2)]
         [p4 (list-ref codeword 3)]
         [d2 (list-ref codeword 4)]
         [d3 (list-ref codeword 5)]
         [d4 (list-ref codeword 6)]
         [s1 (bitwise-xor (bitwise-xor (bitwise-xor p1 d1) d2) d4)]
         [s2 (bitwise-xor (bitwise-xor (bitwise-xor p2 d1) d3) d4)]
         [s4 (bitwise-xor (bitwise-xor (bitwise-xor p4 d2) d3) d4)])
        (+ s1 (* 2 s2) (* 4 s4))))

(doc 'section 'code-properties)

(define (hamming-distance a b)
  (doc 'type '(-> (List Nat) (List Nat) Nat))
  (doc 'description "Number of positions where two codewords differ")
  (if (or (null? a) (null? b))
      0
      (+ (if (= (car a) (car b)) 0 1)
         (hamming-distance (cdr a) (cdr b)))))

(define (minimum-distance codewords)
  (doc 'type '(-> (List (List Nat)) Nat))
  (doc 'description "Minimum Hamming distance among all codeword pairs")
  (if (or (null? codewords) (null? (cdr codewords)))
      +inf.0
      (let ([first (car codewords)]
            [rest (cdr codewords)])
           (min (apply min (map (lambda (c) (hamming-distance first c)) rest))
                (minimum-distance rest)))))

(define (code-rate k n)
  (doc 'type '(-> Nat Nat Real))
  (doc 'description "Code rate = k/n (information bits / total bits)")
  (if (<= n 0)
      0
      (/ k n)))

(define (error-correcting-capability min-distance)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Number of errors correctable = floor((d-1)/2)")
  (quotient (- min-distance 1) 2))

(define (error-detecting-capability min-distance)
  (doc 'type '(-> Nat Nat))
  (doc 'description "Number of errors detectable = d-1")
  (- min-distance 1))

(doc 'section 'compression-metrics)

(define (compression-ratio original-size compressed-size)
  (doc 'type '(-> Nat Nat Real))
  (doc 'description "Original size / Compressed size")
  (if (<= compressed-size 0)
      +inf.0
      (/ original-size compressed-size)))

(define (space-savings original-size compressed-size)
  (doc 'type '(-> Nat Nat Real))
  (doc 'description "1 - (compressed / original), as percentage-like value")
  (if (<= original-size 0)
      0
      (- 1 (/ compressed-size original-size))))
