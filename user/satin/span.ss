;;; playpen/satin/span.ss — Source location tracking
;;;
;;; Spans track source locations for error reporting.
;;; Every Satin form carries a span for precise error messages.

;;; ====
;;; Span Structure
;;; ====

;;; A span is: (span file line column end-line end-column)
;;; - file: string, source file name
;;; - line: 1-based line number
;;; - column: 0-based column offset
;;; - end-line: ending line (often same as line)
;;; - end-column: ending column

(define (make-span file line column end-line end-column)
  (list 'span file line column end-line end-column))

(define (span? x)
  (and (pair? x)
       (eq? (car x) 'span)
       (= (length x) 6)))

(define (span-file s) (list-ref s 1))
(define (span-line s) (list-ref s 2))
(define (span-column s) (list-ref s 3))
(define (span-end-line s) (list-ref s 4))
(define (span-end-column s) (list-ref s 5))

;;; no-span : → Span
;;; A sentinel for forms without source location.
(define no-span (make-span "<unknown>" 0 0 0 0))

(define (no-span? s)
  (and (span? s)
       (equal? (span-file s) "<unknown>")))

;;; ====
;;; Span Formatting
;;; ====

;;; span->string : Span → String
;;; Format span for display: "file:line:column"
(define (span->string s)
  (if (no-span? s)
      "<unknown location>"
      (string-append (span-file s) ":"
                     (number->string (span-line s)) ":"
                     (number->string (span-column s)))))

;;; span-context : Span → String
;;; Format span with context for multi-line display.
(define (span-context s)
  (if (no-span? s)
      "  at: <unknown location>
"
      (string-append "  at: " (span-file s) "
"
                     "  line " (number->string (span-line s))
                     ", column " (number->string (span-column s)) "
")))

;;; ====
;;; Span Combination
;;; ====

;;; span-union : Span × Span → Span
;;; Combine two spans to cover both regions.
(define (span-union s1 s2)
  (cond
   [(no-span? s1) s2]
   [(no-span? s2) s1]
   [else
    (let* ([file (span-file s1)]
           [start-line (min (span-line s1) (span-line s2))]
           [start-col (if (= (span-line s1) (span-line s2))
                          (min (span-column s1) (span-column s2))
                          (if (< (span-line s1) (span-line s2))
                              (span-column s1)
                              (span-column s2)))]
           [end-line (max (span-end-line s1) (span-end-line s2))]
           [end-col (if (= (span-end-line s1) (span-end-line s2))
                        (max (span-end-column s1) (span-end-column s2))
                        (if (> (span-end-line s1) (span-end-line s2))
                            (span-end-column s1)
                            (span-end-column s2)))])
          (make-span file start-line start-col end-line end-col))]))

;;; ====
;;; AST Annotation
;;; ====

;;; Forms carry spans via (with-span span form) wrapper.
(define (with-span span form)
  (list 'with-span span form))

(define (with-span? x)
  (and (pair? x)
       (eq? (car x) 'with-span)
       (= (length x) 3)))

(define (with-span-span x) (cadr x))
(define (with-span-form x) (caddr x))

;;; get-span : Form → Span
;;; Extract span from annotated form, or return no-span.
(define (get-span x)
  (if (with-span? x)
      (with-span-span x)
      no-span))

;;; strip-span : Form → Form
;;; Remove span wrapper from form.
(define (strip-span x)
  (if (with-span? x)
      (with-span-form x)
      x))

;;; deep-strip-spans : Form → Form
;;; Recursively remove all span wrappers.
(define (deep-strip-spans x)
  (cond
   [(with-span? x) (deep-strip-spans (with-span-form x))]
   [(pair? x) (cons (deep-strip-spans (car x))
                    (deep-strip-spans (cdr x)))]
   [else x]))
