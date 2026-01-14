;;; core/base/span.ss — Source Location Spans
;;; @module span
;;; @requires prelude
;;;
;;; Defines the Span data structure for source location tracking.
;;; Moved from core/lang/span.ss to resolve dependency cycle.
;;;
;;; Span = (span file line column end-line end-column)
;;;
;;; This is Core code: pure, total.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Source Spans
;;; ====

;;; make-span : String × Nat × Nat × Nat × Nat → Span
;;; Create a source span.
(define (make-span file line column end-line end-column)
  (list 'span file line column end-line end-column))

;;; span? : Any → Boolean
(define (span? x)
  (and (pair? x) (eq? (car x) 'span)))

;;; span-file : Span → String
;;; Span accessors
(define (span-file s) (list-ref s 1))
;;; span-line : Span → Nat
(define (span-line s) (list-ref s 2))
;;; span-column : Span → Nat
(define (span-column s) (list-ref s 3))
;;; span-end-line : Span → Nat
(define (span-end-line s) (list-ref s 4))
;;; span-end-column : Span → Nat
(define (span-end-column s) (list-ref s 5))

;;; no-span : Span
;;; Placeholder span for when location is unknown.
(define no-span (make-span "<unknown>" 0 0 0 0))

;;; merge-spans : Span × Span → Span
;;; Create a span from start of first to end of second.
(define (merge-spans s1 s2)
  (make-span (span-file s1)
             (span-line s1)
             (span-column s1)
             (span-end-line s2)
             (span-end-column s2)))

;;; format-span : Span → String
;;; Format span for display.
(define (format-span s)
  (if (span? s)
      (string-append (span-file s) ":"
                     (number->string (span-line s)) ":"
                     (number->string (span-column s)))
      "<unknown>"))
