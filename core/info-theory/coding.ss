;;; core/info-theory/coding.ss — Coding Theory Functions
;;;
;;; Implements fundamental coding algorithms:
;;;   - Huffman coding (optimal prefix-free codes)
;;;   - Arithmetic coding (optimal fractional-bit codes)
;;;   - Lempel-Ziv compression (dictionary-based)
;;;   - Channel coding (error detection/correction)
;;;
;;; This is Core code: pure, total, assumes valid input.
;;;
;;; Dependencies:
;;;   - entropy.ss (for entropy calculations)

(load "core/info-theory/entropy.ss")

;;; ============================================================
;;; Huffman Coding
;;; ============================================================
;;;
;;; Huffman coding builds an optimal prefix-free code for a given
;;; probability distribution. The average code length approaches
;;; the entropy H(X) as closely as possible with integer bit lengths.

;;; huffman-node : (Symbol | (Node × Node)) × Number → Node
;;; A Huffman tree node is either a leaf (symbol, prob) or
;;; an internal node ((left . right), prob).
(define (make-huffman-leaf symbol prob)
  (list 'leaf symbol prob))

(define (make-huffman-internal left right)
  (list 'internal (cons left right) (+ (huffman-prob left) (huffman-prob right))))

(define (huffman-leaf? node)
  (eq? (car node) 'leaf))

(define (huffman-symbol node)
  (cadr node))

(define (huffman-children node)
  (cadr node))

(define (huffman-prob node)
  (caddr node))

;;; build-huffman-tree : (List (Symbol × Number)) → Node
;;; Build Huffman tree from symbol-probability pairs.
;;; Input: ((sym1 . prob1) (sym2 . prob2) ...)
(define (build-huffman-tree symbol-probs)
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

;;; huffman-build-loop : (List Node) → Node
;;; Main Huffman algorithm: repeatedly combine two lowest-prob nodes.
(define (huffman-build-loop nodes)
  (if (null? (cdr nodes))
      (car nodes)  ; Only one node left: it's the root
      (let* ([sorted (sort-by-prob nodes)]
             [n1 (car sorted)]
             [n2 (cadr sorted)]
             [rest (cddr sorted)]
             [combined (make-huffman-internal n1 n2)])
            (huffman-build-loop (cons combined rest)))))

;;; sort-by-prob : (List Node) → (List Node)
;;; Sort nodes by probability (ascending).
(define (sort-by-prob nodes)
  (list-sort (lambda (a b) (< (huffman-prob a) (huffman-prob b))) nodes))

;;; list-sort : (a a → Boolean) (List a) → (List a)
;;; Simple insertion sort (sufficient for small alphabets).
(define (list-sort less? lst)
  (if (null? lst)
      '()
      (insert-sorted less? (car lst) (list-sort less? (cdr lst)))))

(define (insert-sorted less? x sorted)
  (if (null? sorted)
      (list x)
      (if (less? x (car sorted))
          (cons x sorted)
          (cons (car sorted) (insert-sorted less? x (cdr sorted))))))

;;; huffman-codes : Node → (List (Symbol × String))
;;; Extract code assignments from Huffman tree.
;;; Returns ((sym1 . "0110") (sym2 . "01") ...)
(define (huffman-codes tree)
  (huffman-codes-helper tree ""))

(define (huffman-codes-helper node prefix)
  (if (huffman-leaf? node)
      (list (cons (huffman-symbol node) prefix))
      (let* ([children (huffman-children node)]
             [left (car children)]
             [right (cdr children)])
            (append (huffman-codes-helper left (string-append prefix "0"))
                    (huffman-codes-helper right (string-append prefix "1"))))))

;;; huffman-encode : (List Symbol) × (List (Symbol × String)) → String
;;; Encode a message using Huffman code table.
(define (huffman-encode message code-table)
  (apply string-append
         (map (lambda (sym)
                      (let ([entry (assq sym code-table)])
                           (if entry
                               (cdr entry)
                               (error "huffman-encode" "symbol not in code table" sym))))
              message)))

;;; huffman-decode : String × Node → (List Symbol)
;;; Decode a bitstring using Huffman tree.
(define (huffman-decode bitstring tree)
  (huffman-decode-helper bitstring tree tree '()))

(define (huffman-decode-helper bits root current acc)
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

;;; huffman-average-length : (List (Symbol × String)) × (List (Symbol × Number)) → Number
;;; Compute average code length given codes and probabilities.
(define (huffman-average-length code-table symbol-probs)
  (fold-left + 0
             (map (lambda (sp)
                          (let* ([sym (car sp)]
                                 [prob (cdr sp)]
                                 [code (cdr (assq sym code-table))])
                                (* prob (string-length code))))
                  symbol-probs)))

;;; huffman-efficiency : (List (Symbol × String)) × (List (Symbol × Number)) → Number
;;; Ratio of entropy to average code length (should be close to 1).
(define (huffman-efficiency code-table symbol-probs)
  (let* ([probs (map cdr symbol-probs)]
         [h (entropy probs)]
         [avg-len (huffman-average-length code-table symbol-probs)])
        (if (= avg-len 0)
            1.0  ; Degenerate case
            (/ h avg-len))))

;;; ============================================================
;;; Arithmetic Coding
;;; ============================================================
;;;
;;; Arithmetic coding represents a message as a subinterval of [0,1).
;;; It achieves optimal compression (approaches entropy exactly).

;;; arithmetic-encode : (List Symbol) × (List (Symbol × Number)) → (Number × Number)
;;; Encode message to an interval [low, high).
;;; Returns the interval as (low . high).
(define (arithmetic-encode message symbol-probs)
  (let ([cumulative (build-cumulative-probs symbol-probs)])
       (arithmetic-encode-helper message cumulative 0.0 1.0)))

(define (arithmetic-encode-helper message cumulative low high)
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

;;; build-cumulative-probs : (List (Symbol × Number)) → (List (Symbol × Number × Number))
;;; Build cumulative probability table: ((sym low high) ...)
(define (build-cumulative-probs symbol-probs)
  (let loop ([remaining symbol-probs] [cumsum 0.0] [acc '()])
       (if (null? remaining)
           (reverse acc)
           (let* ([sym (caar remaining)]
                  [prob (cdar remaining)]
                  [new-cumsum (+ cumsum prob)])
                 (loop (cdr remaining)
                       new-cumsum
                       (cons (list sym cumsum new-cumsum) acc))))))

;;; arithmetic-decode : (Number × Number) × (List (Symbol × Number)) × Int → (List Symbol)
;;; Decode interval back to message of given length.
(define (arithmetic-decode interval symbol-probs length)
  (let ([cumulative (build-cumulative-probs symbol-probs)]
        [code (/ (+ (car interval) (cdr interval)) 2)])  ; Use midpoint
       (arithmetic-decode-helper code cumulative length '())))

(define (arithmetic-decode-helper code cumulative length acc)
  (if (= length 0)
      (reverse acc)
      (let* ([sym-interval (find-symbol-for-code code cumulative)]
             [sym (car sym-interval)]
             [sym-low (cadr sym-interval)]
             [sym-high (caddr sym-interval)]
             [new-code (/ (- code sym-low) (- sym-high sym-low))])
            (arithmetic-decode-helper new-code cumulative (- length 1) (cons sym acc)))))

(define (find-symbol-for-code code cumulative)
  (let loop ([remaining cumulative])
       (if (null? remaining)
           (error "find-symbol-for-code" "code out of range")
           (let* ([entry (car remaining)]
                  [low (cadr entry)]
                  [high (caddr entry)])
                 (if (and (>= code low) (< code high))
                     entry
                     (loop (cdr remaining)))))))

;;; arithmetic-code-length : (Number × Number) → Number
;;; Bits needed to represent an interval (theoretical).
(define (arithmetic-code-length interval)
  (let ([width (- (cdr interval) (car interval))])
       (if (<= width 0)
           +inf.0
           (- (log2 width)))))

;;; ============================================================
;;; Lempel-Ziv Compression (LZ78-style)
;;; ============================================================
;;;
;;; LZ78 builds a dictionary of seen phrases during encoding.
;;; Each output is (dictionary-index, next-symbol).

;;; lz78-encode : (List Symbol) → (List (Int × Symbol))
;;; Encode using LZ78 algorithm.
;;; Returns list of (index, symbol) pairs.
(define (lz78-encode message)
  (lz78-encode-helper message '() 0 '()))

(define (lz78-encode-helper remaining dict next-index acc)
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

;;; lz78-find-longest-match : (List Symbol) × Dict → (Int × Int)
;;; Find longest dictionary match. Returns (index, length).
(define (lz78-find-longest-match message dict)
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

;;; prefix-match-length : (List a) × (List a) → Int
;;; Length of common prefix between two lists.
(define (prefix-match-length a b)
  (let loop ([a a] [b b] [len 0])
       (if (or (null? a) (null? b))
           len
           (if (equal? (car a) (car b))
               (loop (cdr a) (cdr b) (+ len 1))
               len))))

;;; lz78-decode : (List (Int × Symbol)) → (List Symbol)
;;; Decode LZ78 compressed data.
(define (lz78-decode encoded)
  (lz78-decode-helper encoded '() '()))

(define (lz78-decode-helper remaining dict acc)
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

;;; take-n / drop-n : Int × (List a) → (List a)
(define (take-n n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (drop-n n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-n (- n 1) (cdr lst))))

;;; ============================================================
;;; Run-Length Encoding
;;; ============================================================

;;; rle-encode : (List Symbol) → (List (Symbol × Int))
;;; Run-length encode a sequence.
(define (rle-encode message)
  (if (null? message)
      '()
      (rle-encode-helper (cdr message) (car message) 1 '())))

(define (rle-encode-helper remaining current count acc)
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

;;; rle-decode : (List (Symbol × Int)) → (List Symbol)
;;; Decode run-length encoded data.
(define (rle-decode encoded)
  (apply append
         (map (lambda (pair)
                      (replicate (cdr pair) (car pair)))
              encoded)))

;;; replicate : Int × a → (List a)
(define (replicate n x)
  (if (<= n 0)
      '()
      (cons x (replicate (- n 1) x))))

;;; ============================================================
;;; Channel Coding: Error Detection
;;; ============================================================

;;; parity-encode : (List Bit) → (List Bit)
;;; Add even parity bit to end of message.
(define (parity-encode bits)
  (let ([parity (fold-left (lambda (acc b) (bitwise-xor acc b)) 0 bits)])
       (append bits (list parity))))

;;; parity-check : (List Bit) → Boolean
;;; Check if message has valid even parity.
(define (parity-check bits)
  (= (fold-left (lambda (acc b) (bitwise-xor acc b)) 0 bits) 0))

;;; parity-decode : (List Bit) → (List Bit)
;;; Remove parity bit (returns message without checking).
(define (parity-decode bits)
  (if (null? bits)
      '()
      (take-n (- (length bits) 1) bits)))

;;; bitwise-xor : Int × Int → Int
;;; XOR for single bits (0 or 1).
(define (bitwise-xor a b)
  (if (= a b) 0 1))

;;; ============================================================
;;; Channel Coding: Repetition Code
;;; ============================================================

;;; repetition-encode : (List Bit) × Int → (List Bit)
;;; Encode by repeating each bit n times.
(define (repetition-encode bits n)
  (apply append (map (lambda (b) (replicate n b)) bits)))

;;; repetition-decode : (List Bit) × Int → (List Bit)
;;; Decode by majority vote on each n-bit group.
(define (repetition-decode bits n)
  (if (null? bits)
      '()
      (cons (majority-vote (take-n n bits))
            (repetition-decode (drop-n n bits) n))))

;;; majority-vote : (List Bit) → Bit
;;; Return the majority bit (0 or 1).
(define (majority-vote bits)
  (let ([ones (length (filter (lambda (b) (= b 1)) bits))])
       (if (> ones (/ (length bits) 2)) 1 0)))

;;; ============================================================
;;; Channel Coding: Hamming Code (7,4)
;;; ============================================================
;;;
;;; Hamming(7,4) encodes 4 data bits into 7 bits, correcting 1 error.
;;; Positions 1,2,4 are parity bits; positions 3,5,6,7 are data.

;;; hamming74-encode : (List Bit) → (List Bit)
;;; Encode 4 data bits into 7-bit Hamming codeword.
(define (hamming74-encode data)
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

;;; hamming74-decode : (List Bit) → (List Bit)
;;; Decode 7-bit Hamming codeword, correcting single bit error.
;;; Returns 4 data bits.
(define (hamming74-decode codeword)
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

;;; flip-bit-at : (List Bit) × Int → (List Bit)
;;; Flip the bit at given index.
(define (flip-bit-at bits idx)
  (let loop ([remaining bits] [i 0] [acc '()])
       (if (null? remaining)
           (reverse acc)
           (if (= i idx)
               (loop (cdr remaining) (+ i 1) (cons (bitwise-xor (car remaining) 1) acc))
               (loop (cdr remaining) (+ i 1) (cons (car remaining) acc))))))

;;; hamming74-syndrome : (List Bit) → Int
;;; Calculate syndrome (0 means no error).
(define (hamming74-syndrome codeword)
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

;;; ============================================================
;;; Code Properties
;;; ============================================================

;;; hamming-distance : (List Bit) × (List Bit) → Int
;;; Number of positions where two codewords differ.
(define (hamming-distance a b)
  (if (or (null? a) (null? b))
      0
      (+ (if (= (car a) (car b)) 0 1)
         (hamming-distance (cdr a) (cdr b)))))

;;; minimum-distance : (List (List Bit)) → Int
;;; Minimum Hamming distance among all codeword pairs.
(define (minimum-distance codewords)
  (if (or (null? codewords) (null? (cdr codewords)))
      +inf.0
      (let ([first (car codewords)]
            [rest (cdr codewords)])
           (min (apply min (map (lambda (c) (hamming-distance first c)) rest))
                (minimum-distance rest)))))

;;; code-rate : Int × Int → Number
;;; Code rate = k/n (information bits / total bits).
(define (code-rate k n)
  (if (<= n 0)
      0
      (/ k n)))

;;; error-correcting-capability : Int → Int
;;; Number of errors correctable = floor((d-1)/2).
(define (error-correcting-capability min-distance)
  (quotient (- min-distance 1) 2))

;;; error-detecting-capability : Int → Int
;;; Number of errors detectable = d-1.
(define (error-detecting-capability min-distance)
  (- min-distance 1))

;;; ============================================================
;;; Compression Metrics
;;; ============================================================

;;; compression-ratio : Int × Int → Number
;;; Original size / Compressed size.
(define (compression-ratio original-size compressed-size)
  (if (<= compressed-size 0)
      +inf.0
      (/ original-size compressed-size)))

;;; space-savings : Int × Int → Number
;;; 1 - (compressed / original), as percentage-like value.
(define (space-savings original-size compressed-size)
  (if (<= original-size 0)
      0
      (- 1 (/ compressed-size original-size))))
