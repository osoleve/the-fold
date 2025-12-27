;;; shell/paren-check.ss — Parenthesis Balance Checker
;;;
;;; Analyzes Scheme files for parenthesis balance issues.
;;; Reports running balance per line and highlights imbalances.
;;;
;;; Usage:
;;;   (paren-check "path/to/file.ss")           ; Full report
;;;   (paren-check "path/to/file.ss" 100 150)   ; Lines 100-150 only
;;;   (paren-balance "path/to/file.ss")         ; Just the final balance
;;;
;;; This is Shell code: file IO for analysis.

;;; ============================================================
;;; Core Balance Calculation
;;; ============================================================

;;; count-parens : String -> (Values Int Int Int)
;;; Count open and close parens in a string.
;;; Returns (opens closes balance-delta).
;;; Handles (, ), [, ], {, } and ignores chars in strings/comments.
(define (count-parens line)
  (let loop ([i 0]
             [opens 0]
             [closes 0]
             [in-string #f]
             [in-comment #f])
    (if (>= i (string-length line))
        (values opens closes (- opens closes))
        (let ([c (string-ref line i)])
          (cond
            ;; Already in comment - skip rest of line
            [in-comment
             (values opens closes (- opens closes))]

            ;; String handling
            [(and in-string (char=? c #\\))
             ;; Escape sequence - skip next char
             (loop (+ i 2) opens closes in-string in-comment)]
            [(and in-string (char=? c #\"))
             ;; End string
             (loop (+ i 1) opens closes #f in-comment)]
            [in-string
             ;; Inside string - ignore parens
             (loop (+ i 1) opens closes in-string in-comment)]
            [(char=? c #\")
             ;; Start string
             (loop (+ i 1) opens closes #t in-comment)]

            ;; Comment start
            [(char=? c #\;)
             (loop (+ i 1) opens closes in-string #t)]

            ;; Count parens
            [(or (char=? c #\() (char=? c #\[) (char=? c #\{))
             (loop (+ i 1) (+ opens 1) closes in-string in-comment)]
            [(or (char=? c #\)) (char=? c #\]) (char=? c #\}))
             (loop (+ i 1) opens (+ closes 1) in-string in-comment)]

            ;; Other chars
            [else
             (loop (+ i 1) opens closes in-string in-comment)])))))

;;; ============================================================
;;; File Analysis
;;; ============================================================

;;; analyze-file : String -> (List LineInfo)
;;; Analyze a file and return line-by-line paren info.
;;; Each LineInfo is (line-num opens closes running-balance line-text)
(define (analyze-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([line-num 1]
                 [running 0]
                 [results '()])
        (let ([line (get-line port)])
          (if (eof-object? line)
              (reverse results)
              (let-values ([(opens closes delta) (count-parens line)])
                (let ([new-running (+ running delta)])
                  (loop (+ line-num 1)
                        new-running
                        (cons (list line-num opens closes new-running line)
                              results))))))))))

;;; ============================================================
;;; Reporting
;;; ============================================================

;;; format-balance : Int -> String
;;; Format balance with indicator.
(define (format-balance n)
  (cond
    [(> n 0) (string-append "+" (number->string n))]
    [(< n 0) (number->string n)]
    [else "0"]))

;;; paren-check : String [Int Int] -> void
;;; Print paren balance report for a file.
;;; Optional start/end line numbers to focus on a range.
(define paren-check
  (case-lambda
    [(path)
     (paren-check path 1 999999)]
    [(path start-line end-line)
     (let ([results (analyze-file path)])
       (display "\n")
       (display "Parenthesis Balance Report: ")
       (display path)
       (display "\n")
       (display (make-string 60 #\─))
       (display "\n")
       (display "Line   Open Close  Balance  Preview\n")
       (display (make-string 60 #\─))
       (display "\n")

       (for-each
         (lambda (info)
           (let ([line-num (car info)]
                 [opens (cadr info)]
                 [closes (caddr info)]
                 [running (cadddr info)]
                 [text (car (cddddr info))])
             (when (and (>= line-num start-line)
                        (<= line-num end-line)
                        (or (not (= opens 0))
                            (not (= closes 0))
                            (< running 0)))  ; Always show negative balance
               ;; Line number
               (display (pad-left (number->string line-num) 5))
               (display "  ")
               ;; Opens
               (display (pad-left (number->string opens) 4))
               (display "  ")
               ;; Closes
               (display (pad-left (number->string closes) 5))
               (display "  ")
               ;; Running balance with indicator
               (let ([bal-str (format-balance running)])
                 (display (pad-left bal-str 7))
                 (when (< running 0)
                   (display " ⚠️")))
               (display "  ")
               ;; Preview (truncated)
               (display (truncate-string (string-trim text) 30))
               (newline))))
         results)

       (display (make-string 60 #\─))
       (display "\n")

       ;; Final summary
       (let ([final-balance (if (null? results)
                                0
                                (cadddr (car (reverse results))))])
         (display "Final balance: ")
         (display (format-balance final-balance))
         (cond
           [(= final-balance 0)
            (display " ✓ Balanced\n")]
           [(> final-balance 0)
            (display " ✗ ")
            (display final-balance)
            (display " unclosed open paren(s)\n")]
           [else
            (display " ✗ ")
            (display (- final-balance))
            (display " extra close paren(s)\n")])
         (display "\n")))]))

;;; paren-balance : String -> Int
;;; Return just the final balance (0 = balanced).
(define (paren-balance path)
  (let ([results (analyze-file path)])
    (if (null? results)
        0
        (cadddr (car (reverse results))))))

;;; find-imbalance : String -> (List LineInfo)
;;; Return only lines where balance goes negative.
(define (find-imbalance path)
  (filter
    (lambda (info)
      (< (cadddr info) 0))
    (analyze-file path)))

;;; ============================================================
;;; String Utilities
;;; ============================================================

(define (pad-left str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append (make-string (- width len) #\space) str))))

(define (truncate-string str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(define (string-trim str)
  (let* ([len (string-length str)]
         [start (let loop ([i 0])
                  (if (or (>= i len)
                          (not (char-whitespace? (string-ref str i))))
                      i
                      (loop (+ i 1))))]
         [end (let loop ([i (- len 1)])
                (if (or (< i 0)
                        (not (char-whitespace? (string-ref str i))))
                    (+ i 1)
                    (loop (- i 1))))])
    (if (>= start end)
        ""
        (substring str start end))))

;;; ============================================================
;;; Quick Diagnostic
;;; ============================================================

;;; paren-diag : String Int -> void
;;; Show detailed diagnosis around a specific line.
(define (paren-diag path target-line)
  (let* ([start (max 1 (- target-line 10))]
         [end (+ target-line 10)]
         [results (analyze-file path)])
    (display "\n")
    (display "Diagnosis around line ")
    (display target-line)
    (display ":\n")
    (display (make-string 70 #\─))
    (display "\n")

    (for-each
      (lambda (info)
        (let ([line-num (car info)]
              [opens (cadr info)]
              [closes (caddr info)]
              [running (cadddr info)]
              [text (car (cddddr info))])
          (when (and (>= line-num start) (<= line-num end))
            ;; Marker for target line
            (if (= line-num target-line)
                (display ">>> ")
                (display "    "))
            ;; Line number
            (display (pad-left (number->string line-num) 4))
            (display " [")
            (display (format-balance running))
            (display "] ")
            ;; Full line (truncated)
            (display (truncate-string text 55))
            (newline))))
      results)

    (display (make-string 70 #\─))
    (display "\n\n")))
