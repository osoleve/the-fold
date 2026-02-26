(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)
(require 'meta/fuel-vocab)

;;; @module meta/fuel-normalize
;;; @requires prelude meta/fuel-vocab

(doc 'module 'meta/fuel-normalize)
(doc 'description "Normalize manifest fuel-bound fields into machine-readable fuel-vocab
S-expression forms. Handles 3 input formats:
  1. Already valid fuel-cost S-expressions -> pass through
  2. Bare numbers -> (constant k)
  3. Free-text strings like \"O(n^2)\" -> (quadratic n)
Unparseable values are flagged, not silently dropped.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ====
;;; String Parsing
;;; ====

(doc 'section 'string-parsing)

;;; parse-fuel-string : String -> (Union FuelCost 'unparseable)
;;; Parse a free-text fuel bound string into a fuel-vocab form.
;;; Handles common O-notation patterns.
(doc parse-fuel-string 'type '(-> String (Union FuelCost Symbol)))
(doc parse-fuel-string 'export #t)
(define (parse-fuel-string str)
  (let ([trimmed (string-trim-fn str)])
    (cond
      ;; Exact matches for common patterns
      [(string=? trimmed "O(1)")          (fuel/constant 1)]
      [(string=? trimmed "O(n)")          (fuel/linear 'n)]
      [(string=? trimmed "O(n^2)")        (fuel/quadratic 'n)]
      [(string=? trimmed "O(n^3)")        (fuel/cubic 'n)]
      [(string=? trimmed "O(n log n)")    (fuel/log-linear 'n)]
      [(string=? trimmed "O(log n)")      (fuel/logarithmic 'n)]
      [(string=? trimmed "O(2^n)")        (fuel/exponential 'n)]
      [(string=? trimmed "O(n^4)")        (fuel/polynomial 'n 4)]
      [(string=? trimmed "O(n^5)")        (fuel/polynomial 'n 5)]

      ;; Patterns with variable names
      [(parse-o-pattern trimmed) => values]

      ;; Multi-part: "O(X) for ..., O(Y) for ..."
      ;; Take the max of all parsed parts
      [(multi-part-fuel-string trimmed) => values]

      ;; Everything else
      [else 'unparseable])))

;;; parse-o-pattern : String -> (Maybe FuelCost)
;;; Try to parse "O(expr)" patterns with various inner expressions.
(define (parse-o-pattern str)
  (and (>= (string-length str) 4)
       (char=? (string-ref str 0) #\O)
       (char=? (string-ref str 1) #\()
       (char=? (string-ref str (- (string-length str) 1)) #\))
       (let ([inner (substring str 2 (- (string-length str) 1))])
         (parse-complexity-expr inner))))

;;; parse-complexity-expr : String -> (Maybe FuelCost)
;;; Parse the inside of O(...).
;;; Handles: n, n^k, n log n, log n, 2^n, k (constant)
(define (parse-complexity-expr expr)
  (let ([trimmed (string-trim-fn expr)])
    (cond
      ;; "1" -> constant
      [(string=? trimmed "1") (fuel/constant 1)]

      ;; "n" or single variable
      [(single-variable? trimmed)
       (fuel/linear (string->symbol trimmed))]

      ;; "n^k" pattern
      [(parse-power trimmed) => values]

      ;; "n log n" or "n*log(n)" pattern
      [(parse-nlogn trimmed) => values]

      ;; "log n" or "log(n)"
      [(parse-logn trimmed) => values]

      ;; "2^n" pattern
      [(parse-exp trimmed) => values]

      ;; Bare number (constant)
      [(string->number trimmed)
       => (lambda (k) (fuel/constant (inexact->exact (ceiling k))))]

      [else #f])))

;;; ====
;;; Pattern Matchers
;;; ====

(define (single-variable? s)
  (and (> (string-length s) 0)
       (char-alphabetic? (string-ref s 0))
       (let loop ([i 1])
         (or (>= i (string-length s))
             (and (or (char-alphabetic? (string-ref s i))
                      (char-numeric? (string-ref s i))
                      (char=? (string-ref s i) #\_))
                  (loop (+ i 1)))))))

(define (parse-power s)
  ;; Match "X^Y" where X is a variable and Y is a number
  (let ([caret (string-index-fn s #\^)])
    (and caret
         (> caret 0)
         (< caret (- (string-length s) 1))
         (let ([base (string-trim-fn (substring s 0 caret))]
               [exp-str (string-trim-fn (substring s (+ caret 1) (string-length s)))])
           (and (single-variable? base)
                (string->number exp-str)
                (let ([k (inexact->exact (string->number exp-str))]
                      [var (string->symbol base)])
                  (case k
                    [(2) (fuel/quadratic var)]
                    [(3) (fuel/cubic var)]
                    [else (fuel/polynomial var k)])))))))

(define (parse-nlogn s)
  ;; Match "X log X" or "X*log(X)" patterns
  (let ([space-log (string-contains-fn s " log ")])
    (and space-log
         (let ([var-str (string-trim-fn (substring s 0 space-log))])
           (and (single-variable? var-str)
                (fuel/log-linear (string->symbol var-str)))))))

(define (parse-logn s)
  ;; Match "log X" or "log(X)"
  (and (>= (string-length s) 5)
       (string=? "log " (substring s 0 4))
       (let ([var-str (string-trim-fn (substring s 4 (string-length s)))])
         ;; Strip parens if present
         (let ([clean (if (and (> (string-length var-str) 0)
                               (char=? (string-ref var-str 0) #\()
                               (char=? (string-ref var-str (- (string-length var-str) 1)) #\)))
                          (substring var-str 1 (- (string-length var-str) 1))
                          var-str)])
           (and (single-variable? clean)
                (fuel/logarithmic (string->symbol clean)))))))

(define (parse-exp s)
  ;; Match "2^X" or "k^X"
  (let ([caret (string-index-fn s #\^)])
    (and caret
         (> caret 0)
         (let ([base-str (string-trim-fn (substring s 0 caret))]
               [var-str (string-trim-fn (substring s (+ caret 1) (string-length s)))])
           (and (string->number base-str)
                (single-variable? var-str)
                (fuel/exponential (string->symbol var-str)))))))

;;; ====
;;; Multi-Part Strings
;;; ====

(define (multi-part-fuel-string str)
  ;; Split on ", " and try to parse each "O(...) for ..." segment
  (let* ([parts (split-on-comma str)]
         [parsed (filter-map parse-fuel-segment parts)])
    (cond
      [(null? parsed) #f]
      [(= (length parsed) 1) (car parsed)]
      [else
       ;; Combine with max
       (let loop ([rest (cdr parsed)] [acc (car parsed)])
         (if (null? rest)
             acc
             (loop (cdr rest) (fuel/max acc (car rest)))))])))

(define (parse-fuel-segment seg)
  ;; Parse "O(n^2) for n entities" -> just parse the O(...) part
  (let ([trimmed (string-trim-fn seg)])
    (cond
      ;; Starts with "O(" — extract up to matching ")"
      [(and (>= (string-length trimmed) 4)
            (char=? (string-ref trimmed 0) #\O)
            (char=? (string-ref trimmed 1) #\())
       (let ([close-paren (string-index-fn trimmed #\))])
         (and close-paren
              (parse-o-pattern (substring trimmed 0 (+ close-paren 1)))))]
      [else #f])))

(define (split-on-comma str)
  (let loop ([chars (string->list str)]
             [current '()]
             [result '()])
    (cond
      [(null? chars)
       (reverse (if (null? current)
                    result
                    (cons (list->string (reverse current)) result)))]
      [(and (char=? (car chars) #\,)
            (pair? (cdr chars))
            (char=? (cadr chars) #\space))
       (loop (cddr chars) '()
             (if (null? current) result
                 (cons (list->string (reverse current)) result)))]
      [else
       (loop (cdr chars) (cons (car chars) current) result)])))

;;; ====
;;; Top-Level Normalizer
;;; ====

(doc 'section 'normalizer)

;;; normalize-fuel-bound : Any -> (Values FuelCost Symbol)
;;; Normalize a fuel-bound field from a manifest.
;;; Returns one of:
;;;   A valid FuelCost form (new or already-valid)
;;;   'already-valid (input was already a valid fuel-cost form)
;;;   'unparseable (couldn't normalize)
(doc normalize-fuel-bound 'type '(-> Any (Union FuelCost Symbol)))
(doc normalize-fuel-bound 'export #t)
(define (normalize-fuel-bound val)
  (cond
    ;; Already a valid fuel-cost S-expression
    [(and (pair? val) (fuel-cost-valid? val))
     'already-valid]
    ;; Bare number -> constant
    [(number? val)
     (fuel/constant (if (exact? val) val (inexact->exact (ceiling val))))]
    ;; String -> try to parse
    [(string? val)
     (parse-fuel-string val)]
    ;; Unknown type
    [else 'unparseable]))

;;; ====
;;; Batch Report
;;; ====

(doc 'section 'report)

;;; normalize-all-fuel-bounds : (List ManifestData) -> Report
;;; Normalize fuel bounds for all manifests and produce a report.
;;; Returns: ((already-valid . N) (normalized . N) (unparseable . ((skill . value) ...)))
(doc normalize-all-fuel-bounds 'type '(-> (List ManifestData) Report))
(doc normalize-all-fuel-bounds 'export #t)
(define (normalize-all-fuel-bounds manifests)
  (let ([already 0] [normalized 0] [failed '()])
    (for-each
     (lambda (manifest)
       (let* ([name (cdr (assq 'name manifest))]
              [fuel (cdr (assq 'fuel-bound manifest))]
              [result (normalize-fuel-bound fuel)])
         (cond
           [(eq? result 'already-valid)
            (set! already (+ already 1))]
           [(eq? result 'unparseable)
            (set! failed (cons (cons name fuel) failed))]
           [else
            (set! normalized (+ normalized 1))])))
     manifests)
    `((already-valid . ,already)
      (normalized . ,normalized)
      (unparseable . ,(reverse failed)))))

;;; print-fuel-report : Report -> Void
(doc print-fuel-report 'export #t)
(define (print-fuel-report report)
  (let ([already (cdr (assq 'already-valid report))]
        [normalized (cdr (assq 'normalized report))]
        [failed (cdr (assq 'unparseable report))])
    (printf "Fuel bound normalization:\n")
    (printf "  Already valid: ~a\n" already)
    (printf "  Normalized:    ~a\n" normalized)
    (printf "  Unparseable:   ~a\n" (length failed))
    (when (pair? failed)
      (printf "\nUnparseable fuel bounds:\n")
      (for-each
       (lambda (entry)
         (printf "  ~a: ~s\n" (car entry) (cdr entry)))
       failed))))

;;; ====
;;; String Helpers
;;; ====

(define (string-trim-fn s)
  (let* ([len (string-length s)]
         [start (let loop ([i 0])
                  (if (and (< i len) (char-whitespace? (string-ref s i)))
                      (loop (+ i 1))
                      i))]
         [end (let loop ([i len])
                (if (and (> i start) (char-whitespace? (string-ref s (- i 1))))
                    (loop (- i 1))
                    i))])
    (substring s start end)))

(define (string-index-fn s ch)
  (let ([len (string-length s)])
    (let loop ([i 0])
      (cond
        [(>= i len) #f]
        [(char=? (string-ref s i) ch) i]
        [else (loop (+ i 1))]))))

(define (string-contains-fn s sub)
  (let ([slen (string-length s)]
        [sublen (string-length sub)])
    (let loop ([i 0])
      (cond
        [(> (+ i sublen) slen) #f]
        [(string=? sub (substring s i (+ i sublen))) i]
        [else (loop (+ i 1))]))))
