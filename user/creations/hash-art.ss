;;; hash-art.ss - Generative Art from Hash Values
;;; Creates visual patterns from cryptographic hashes

;;; ====
;;; HASH TO PATTERN CONVERTERS
;;; ====

(define (hex-char->number c)
  "Convert hex character to number 0-15"
  (cond
   ((char<=? #\0 c #\9) (- (char->integer c) (char->integer #\0)))
   ((char<=? #\a c #\f) (+ 10 (- (char->integer c) (char->integer #\a))))
   ((char<=? #\A c #\F) (+ 10 (- (char->integer c) (char->integer #\A))))
   (else 0)))

(define (hash->numbers hash-str)
  "Convert hash string to list of numbers"
  (map hex-char->number (string->list hash-str)))

(define (take-n lst n)
  (if (or (null? lst) (zero? n))
      '()
      (cons (car lst) (take-n (cdr lst) (- n 1)))))

;;; ====
;;; PATTERN GENERATORS
;;; ====

(define (hash-grid hash-str size)
  "Create grid pattern from hash (uses first size*size values)"
  (let* ((nums (hash->numbers hash-str))
         (needed (* size size))
         (values (take-n nums needed))
         (chars " .:-=+*#@"))
        (let loop ((i 0) (result ""))
             (if (>= i (length values))
                 result
                 (let* ((val (list-ref values i))
                        (char-idx (modulo val (string-length chars)))
                        (char (string-ref chars char-idx))
                        (newline (if (and (> i 0)
                                          (zero? (modulo (+ i 1) size)))
                                     "\n" "")))
                       (loop (+ i 1)
                             (string-append result (string char) " " newline)))))))

(define (hash-bars hash-str count)
  "Create horizontal bar chart from hash values"
  (let* ((nums (hash->numbers hash-str))
         (values (take-n nums count)))
        (apply string-append
               (map (lambda (n)
                            (string-append
                             (make-string (+ n 1) #\*)
                             " "
                             (number->string n)
                             "\n"))
                    values))))

(define (hash-wave hash-str width height)
  "Create wave pattern using hash values"
  (let* ((nums (hash->numbers hash-str))
         (wave-values (take-n nums width)))
        (apply string-append
               (let y-loop ((y (- height 1)) (lines '()))
                    (if (< y 0)
                        lines
                        (y-loop (- y 1)
                                (cons
                                 (string-append
                                  (apply string-append
                                         (map (lambda (val)
                                                      (let ((scaled (inexact->exact
                                                                     (floor (* (/ val 15.0)
                                                                               (- height 1))))))
                                                           (if (= scaled y) "* " "  ")))
                                              wave-values))
                                  "\n")
                                 lines)))))))

(define (hash-spiral hash-str size)
  "Create spiral pattern from hash"
  (let* ((nums (hash->numbers hash-str))
         (grid (make-vector (* size size) #\space))
         (center (quotient size 2)))
        
        ;; Fill spiral
        (let spiral-loop ((nums nums)
                          (x center) (y center)
                          (dx 0) (dy -1)
                          (segment-len 1)
                          (segment-pos 0)
                          (segments-done 0)
                          (i 0))
             (when (and (< i (* size size))
                        (not (null? nums)))
                   (let* ((idx (+ x (* y size)))
                          (val (car nums))
                          (chars ".:-=+*#@")
                          (char (string-ref chars (modulo val (string-length chars)))))
                         (when (and (>= x 0) (< x size)
                                    (>= y 0) (< y size))
                               (vector-set! grid idx char))
                         
                         (let* ((new-x (+ x dx))
                                (new-y (+ y dy))
                                (new-pos (+ segment-pos 1))
                                (new-done (if (>= new-pos segment-len)
                                              (+ segments-done 1)
                                              segments-done))
                                (new-len (if (= new-done 2)
                                             (+ segment-len 1)
                                             segment-len))
                                (turn (>= new-pos segment-len))
                                (new-dx (if turn (- dy) dx))
                                (new-dy (if turn dx dy))
                                (final-pos (if turn 0 new-pos))
                                (final-done (if turn 0 new-done)))
                               (spiral-loop (cdr nums) new-x new-y
                                            new-dx new-dy
                                            new-len final-pos final-done
                                            (+ i 1))))))
        
        ;; Render grid
        (let loop ((y 0) (result ""))
             (if (>= y size)
                 result
                 (loop (+ y 1)
                       (string-append result
                                      (apply string
                                             (let x-loop ((x 0) (chars '()))
                                                  (if (>= x size)
                                                      (reverse chars)
                                                      (x-loop (+ x 1)
                                                              (cons (vector-ref grid (+ x (* y size)))
                                                                    chars)))))
                                      "\n"))))))

(define (hash-symmetry hash-str size)
  "Create symmetrical pattern from hash (mandala-like)"
  (let* ((nums (hash->numbers hash-str))
         (half (quotient size 2))
         (needed (* half half))
         (values (take-n nums needed))
         (chars " .:+=*#@")
         (grid (make-vector (* size size) #\space)))
        
        ;; Fill quadrant and mirror
        (let loop ((i 0))
             (when (< i (length values))
                   (let* ((val (list-ref values i))
                          (x (modulo i half))
                          (y (quotient i half))
                          (char-idx (modulo val (string-length chars)))
                          (char (string-ref chars char-idx)))
                         ;; Four-way symmetry
                         (when (< y half)
                               (vector-set! grid (+ x (* y size)) char)
                               (vector-set! grid (+ (- size 1 x) (* y size)) char)
                               (vector-set! grid (+ x (* (- size 1 y) size)) char)
                               (vector-set! grid (+ (- size 1 x) (* (- size 1 y) size)) char))
                         (loop (+ i 1)))))
        
        ;; Render
        (let loop ((y 0) (result ""))
             (if (>= y size)
                 result
                 (loop (+ y 1)
                       (string-append result
                                      (list->string
                                       (let x-loop ((x 0) (chars '()))
                                            (if (>= x size)
                                                (reverse chars)
                                                (x-loop (+ x 1)
                                                        (cons (vector-ref grid (+ x (* y size)))
                                                              (cons #\space chars))))))
                                      "\n"))))))

;;; ====
;;; DEMONSTRATION
;;; ====

(define (hash-art-demo)
  (display "\n=== HASH ART GENERATOR ===\n")
  (display "Creating generative art from cryptographic hashes...\n\n")
  
  (let ((sample-hash "00c1d3299e2ffd2712c3d0e4dffa93ee"))
       
       (display "1. GRID PATTERN (8x8)\n")
       (display (hash-grid sample-hash 8))
       (display "\n")
       
       (display "2. BAR CHART (16 values)\n")
       (display (hash-bars sample-hash 16))
       (display "\n")
       
       (display "3. WAVE PATTERN (32 wide, 10 high)\n")
       (display (hash-wave sample-hash 32 10))
       (display "\n")
       
       (display "4. SPIRAL PATTERN (15x15)\n")
       (display (hash-spiral sample-hash 15))
       (display "\n")
       
       (display "5. SYMMETRY/MANDALA (15x15)\n")
       (display (hash-symmetry sample-hash 15))
       (display "\n"))
  
  (display "Try with any hash string:\n")
  (display "  (hash-grid \"your-hash-here\" 10)\n")
  (display "  (hash-spiral \"abc123def456\" 20)\n")
  (display "  (hash-symmetry \"fedcba987654\" 16)\n\n"))

;; Run demo
(hash-art-demo)
