;;; fabric/stitches/fp/rope.ss — Rope Library
;;;
;;; Ropes are a data structure for efficiently manipulating long strings.
;;; They provide:
;;; - O(log n) insertions and deletions
;;; - O(log n) concatenation
;;; - O(log n) character access
;;; - O(log n) substring extraction
;;;
;;; Internally, a rope is a balanced binary tree where leaves contain
;;; string chunks and internal nodes track the total length of their
;;; left subtree for efficient indexing.
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Rope Type
;;; ============================================================

;;; Rope = Leaf string | Node weight left right
;;; weight = length of entire left subtree

(define rope-max-leaf-size 64)

(define (make-rope-leaf str)
  (list 'rope-leaf str (string-length str)))

(define (rope-leaf? x)
  (and (pair? x) (eq? (car x) 'rope-leaf)))

(define (rope-leaf-str r)
  (cadr r))

(define (rope-leaf-len r)
  (caddr r))

(define (make-rope-node left right)
  (list 'rope-node (rope-length left) left right))

(define (rope-node? x)
  (and (pair? x) (eq? (car x) 'rope-node)))

(define (rope-node-weight r)
  (cadr r))

(define (rope-node-left r)
  (caddr r))

(define (rope-node-right r)
  (cadddr r))

(define rope-empty (make-rope-leaf ""))

(define (rope-empty? r)
  (and (rope-leaf? r) (= (rope-leaf-len r) 0)))

(define (rope? x)
  (or (rope-leaf? x) (rope-node? x)))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Get length - O(1) for leaf, O(1) for node
(define (rope-length r)
  (cond
   [(rope-leaf? r) (rope-leaf-len r)]
   [(rope-node? r)
    (+ (rope-node-weight r) (rope-length (rope-node-right r)))]
   [else 0]))

;;; Get character at index - O(log n)
(define (rope-ref r i)
  (cond
   [(rope-leaf? r)
    (if (and (>= i 0) (< i (rope-leaf-len r)))
        (just (string-ref (rope-leaf-str r) i))
        nothing)]
   [(rope-node? r)
    (let ([w (rope-node-weight r)])
         (if (< i w)
             (rope-ref (rope-node-left r) i)
             (rope-ref (rope-node-right r) (- i w))))]
   [else nothing]))

;;; Concatenate two ropes - O(log n)
(define (rope-append r1 r2)
  (cond
   [(rope-empty? r1) r2]
   [(rope-empty? r2) r1]
   ;; Merge small leaves
   [(and (rope-leaf? r1) (rope-leaf? r2)
         (<= (+ (rope-leaf-len r1) (rope-leaf-len r2)) rope-max-leaf-size))
    (make-rope-leaf (string-append (rope-leaf-str r1) (rope-leaf-str r2)))]
   [else (make-rope-node r1 r2)]))

;;; ============================================================
;;; Split Operations
;;; ============================================================

;;; Split at index - O(log n)
;;; Returns (left . right) where left has characters [0, i) and right has [i, end)
(define (rope-split r i)
  (cond
   [(rope-leaf? r)
    (let ([str (rope-leaf-str r)]
          [len (rope-leaf-len r)])
         (cond
          [(<= i 0) (cons rope-empty r)]
          [(>= i len) (cons r rope-empty)]
          [else (cons (make-rope-leaf (substring str 0 i))
                      (make-rope-leaf (substring str i len)))]))]
   [(rope-node? r)
    (let ([w (rope-node-weight r)]
          [left (rope-node-left r)]
          [right (rope-node-right r)])
         (cond
          [(<= i 0) (cons rope-empty r)]
          [(>= i (rope-length r)) (cons r rope-empty)]
          [(<= i w)
           ;; Split point is in left subtree
           (let ([split (rope-split left i)])
                (cons (car split)
                      (rope-append (cdr split) right)))]
          [else
           ;; Split point is in right subtree
           (let ([split (rope-split right (- i w))])
                (cons (rope-append left (car split))
                      (cdr split)))]))]
   [else (cons rope-empty rope-empty)]))

;;; ============================================================
;;; Insert and Delete
;;; ============================================================

;;; Insert string at position - O(log n)
(define (rope-insert r i str)
  (if (string=? str "")
      r
      (let ([split (rope-split r i)])
           (rope-append (rope-append (car split) (string->rope str))
                        (cdr split)))))

;;; Delete range [start, end) - O(log n)
(define (rope-delete r start end)
  (let* ([split1 (rope-split r start)]
         [split2 (rope-split (cdr split1) (- end start))])
        (rope-append (car split1) (cdr split2))))

;;; Replace range [start, end) with string - O(log n)
(define (rope-replace r start end str)
  (rope-insert (rope-delete r start end) start str))

;;; ============================================================
;;; Substring Operations
;;; ============================================================

;;; Extract substring [start, end) - O(log n)
(define (rope-substring r start end)
  (let* ([split1 (rope-split r start)]
         [split2 (rope-split (cdr split1) (- end start))])
        (car split2)))

;;; Take first n characters
(define (rope-take r n)
  (car (rope-split r n)))

;;; Drop first n characters
(define (rope-drop r n)
  (cdr (rope-split r n)))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert string to rope
(define (string->rope str)
  (if (<= (string-length str) rope-max-leaf-size)
      (make-rope-leaf str)
      ;; Split large strings into balanced tree
      (let* ([len (string-length str)]
             [mid (quotient len 2)])
            (make-rope-node (string->rope (substring str 0 mid))
                            (string->rope (substring str mid len))))))

;;; Convert rope to string - O(n)
(define (rope->string r)
  (let ([result (make-string (rope-length r))])
       (rope-write-to-string! r result 0)
       result))

(define (rope-write-to-string! r str pos)
  (cond
   [(rope-leaf? r)
    (let* ([leaf-str (rope-leaf-str r)]
           [len (rope-leaf-len r)])
          (do ([i 0 (+ i 1)])
              ((= i len) (+ pos len))
              (string-set! str (+ pos i) (string-ref leaf-str i))))]
   [(rope-node? r)
    (let ([new-pos (rope-write-to-string! (rope-node-left r) str pos)])
         (rope-write-to-string! (rope-node-right r) str new-pos))]
   [else pos]))

;;; Convert list of chars to rope
(define (chars->rope chars)
  (string->rope (list->string chars)))

;;; Convert rope to list of chars
(define (rope->chars r)
  (string->list (rope->string r)))

;;; ============================================================
;;; Iteration
;;; ============================================================

;;; Fold left over characters
(define (rope-fold-left f init r)
  (fold-left f init (rope->chars r)))

;;; Fold right over characters
(define (rope-fold-right f init r)
  (fold-right f init (rope->chars r)))

;;; Map function over characters
(define (rope-map f r)
  (chars->rope (map f (rope->chars r))))

;;; Filter characters
(define (rope-filter pred r)
  (chars->rope (filter pred (rope->chars r))))

;;; For-each over characters
(define (rope-for-each f r)
  (for-each f (rope->chars r)))

;;; ============================================================
;;; Search Operations
;;; ============================================================

;;; Find first index of character - O(n)
(define (rope-index-of r char)
  (let ([len (rope-length r)])
       (let loop ([i 0])
            (if (>= i len)
                nothing
                (let ([c (from-just (rope-ref r i))])
                     (if (char=? c char)
                         (just i)
                         (loop (+ i 1))))))))

;;; Find first index of substring - O(n*m)
(define (rope-find r pattern)
  (let ([str (rope->string r)]
        [pat-len (string-length pattern)]
        [str-len (rope-length r)])
       (let loop ([i 0])
            (if (> (+ i pat-len) str-len)
                nothing
                (if (string-prefix? (substring str i (min (+ i pat-len) str-len)) pattern)
                    (just i)
                    (loop (+ i 1)))))))

;;; Helper: check if s2 starts with s1
(define (string-prefix? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

;;; Count occurrences of character
(define (rope-count-char r char)
  (rope-fold-left (lambda (acc c) (if (char=? c char) (+ acc 1) acc)) 0 r))

;;; ============================================================
;;; Line Operations (for text editors)
;;; ============================================================

;;; Count lines (number of newlines + 1)
(define (rope-line-count r)
  (+ 1 (rope-count-char r (string-ref "
" 0))))

;;; Get line at index (0-based)
(define (rope-get-line r line-num)
  (let* ([chars (rope->chars r)]
         [newline (string-ref "
" 0)])
        (let loop ([chars chars] [current-line 0] [acc '()])
             (cond
              [(null? chars)
               (if (= current-line line-num)
                   (just (list->string (reverse acc)))
                   nothing)]
              [(char=? (car chars) newline)
               (if (= current-line line-num)
                   (just (list->string (reverse acc)))
                   (loop (cdr chars) (+ current-line 1) '()))]
              [else
               (if (= current-line line-num)
                   (loop (cdr chars) current-line (cons (car chars) acc))
                   (loop (cdr chars) current-line acc))]))))

;;; Get position (offset) of line start
(define (rope-line-start r line-num)
  (let* ([chars (rope->chars r)]
         [newline (string-ref "
" 0)])
        (let loop ([chars chars] [current-line 0] [pos 0])
             (cond
              [(= current-line line-num) (just pos)]
              [(null? chars) nothing]
              [(char=? (car chars) newline)
               (loop (cdr chars) (+ current-line 1) (+ pos 1))]
              [else
               (loop (cdr chars) current-line (+ pos 1))]))))

;;; ============================================================
;;; Balancing
;;; ============================================================

;;; Rebalance a rope (rebuild as balanced tree)
(define (rope-rebalance r)
  (string->rope (rope->string r)))

;;; Get tree depth
(define (rope-depth r)
  (cond
   [(rope-leaf? r) 0]
   [(rope-node? r)
    (+ 1 (max (rope-depth (rope-node-left r))
              (rope-depth (rope-node-right r))))]
   [else 0]))

;;; ============================================================
;;; Comparison
;;; ============================================================

(define (rope-equal? r1 r2)
  (string=? (rope->string r1) (rope->string r2)))

(define (rope-compare r1 r2)
  (let ([s1 (rope->string r1)]
        [s2 (rope->string r2)])
       (cond
        [(string<? s1 s2) -1]
        [(string>? s1 s2) 1]
        [else 0])))

;;; ============================================================
;;; Display
;;; ============================================================

(define (rope->display r)
  (let ([s (rope->string r)])
       (if (> (string-length s) 40)
           (format "Rope[~a...](len=~a)" (substring s 0 37) (rope-length r))
           (format "Rope[~a]" s))))

;;; ============================================================
;;; Monoid Operations
;;; ============================================================

(define rope-mempty rope-empty)
(define rope-mappend rope-append)

(define (rope-mconcat ropes)
  (fold-left rope-append rope-empty ropes))

;;; ============================================================
;;; Builder Pattern
;;; ============================================================

;;; Rope builder for efficient incremental construction
(define (rope-builder)
  rope-empty)

(define (builder-append-rope! b r)
  (rope-append b r))

(define (builder-append-string! b str)
  (rope-append b (string->rope str)))

(define (builder-append-char! b c)
  (rope-append b (make-rope-leaf (string c))))

(define (builder-append-line! b str)
  (rope-append (rope-append b (string->rope str))
               (make-rope-leaf "
")))

(define (builder-finish b)
  b)

;;; ============================================================
;;; Advanced Operations
;;; ============================================================

;;; Reverse a rope
(define (rope-reverse r)
  (chars->rope (reverse (rope->chars r))))

;;; Uppercase
(define (rope-upcase r)
  (string->rope (string-upcase-manual (rope->string r))))

(define (string-upcase-manual s)
  (list->string (map char-upcase (string->list s))))

;;; Lowercase
(define (rope-downcase r)
  (string->rope (string-downcase-manual (rope->string r))))

(define (string-downcase-manual s)
  (list->string (map char-downcase (string->list s))))

;;; Trim whitespace
(define (rope-trim r)
  (string->rope (string-trim-manual (rope->string r))))

(define (string-trim-manual s)
  (let* ([chars (string->list s)]
         [trimmed (trim-whitespace-left (trim-whitespace-right chars))])
        (list->string trimmed)))

(define (trim-whitespace-left chars)
  (if (or (null? chars) (not (char-whitespace? (car chars))))
      chars
      (trim-whitespace-left (cdr chars))))

(define (trim-whitespace-right chars)
  (reverse (trim-whitespace-left (reverse chars))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Types:
;;;   rope?, rope-leaf?, rope-node?, rope-empty, rope-empty?
;;;
;;; Core operations:
;;;   rope-length, rope-ref, rope-append
;;;
;;; Split/join:
;;;   rope-split, rope-insert, rope-delete, rope-replace
;;;
;;; Substring:
;;;   rope-substring, rope-take, rope-drop
;;;
;;; Conversions:
;;;   string->rope, rope->string, chars->rope, rope->chars
;;;
;;; Iteration:
;;;   rope-fold-left, rope-fold-right, rope-map, rope-filter, rope-for-each
;;;
;;; Search:
;;;   rope-index-of, rope-find, rope-count-char
;;;
;;; Line operations:
;;;   rope-line-count, rope-get-line, rope-line-start
;;;
;;; Balancing:
;;;   rope-rebalance, rope-depth
;;;
;;; Comparison:
;;;   rope-equal?, rope-compare
;;;
;;; Monoid:
;;;   rope-mempty, rope-mappend, rope-mconcat
;;;
;;; Builder:
;;;   rope-builder, builder-append-rope!, builder-append-string!,
;;;   builder-append-char!, builder-append-line!, builder-finish
;;;
;;; Advanced:
;;;   rope-reverse, rope-upcase, rope-downcase, rope-trim
