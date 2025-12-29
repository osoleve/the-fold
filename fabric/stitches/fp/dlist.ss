;;; fabric/stitches/fp/dlist.ss — Difference List Library
;;;
;;; Difference lists provide O(1) append operations by representing
;;; lists as functions from tail to complete list. This is useful for
;;; building large lists incrementally without quadratic behavior.
;;;
;;; A DList is represented as: (lambda (tail) (append front tail))
;;; More efficiently: a function that prepends elements to its argument
;;;
;;; Features:
;;; - O(1) append/concat
;;; - O(1) cons/snoc
;;; - O(n) conversion to/from list
;;; - Monoid instance
;;; - Builder pattern for efficient list construction
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; DList Type
;;; ============================================================

;;; A DList is a function: List -> List
;;; It prepends its elements to the given tail

(define (dlist? x)
  (procedure? x))

;;; Empty difference list
(define dlist-empty
  (lambda (tail) tail))

;;; Singleton difference list
(define (dlist-singleton x)
  (lambda (tail) (cons x tail)))

;;; Check if dlist is empty (expensive - requires materialization)
(define (dlist-empty? dl)
  (null? (dl '())))

;;; ============================================================
;;; Conversions
;;; ============================================================

;;; Convert list to difference list - O(n)
(define (list->dlist lst)
  (lambda (tail) (append lst tail)))

;;; Convert difference list to list - O(n)
(define (dlist->list dl)
  (dl '()))

;;; Convert string to dlist of chars
(define (string->dlist str)
  (list->dlist (string->list str)))

;;; Convert dlist of chars to string
(define (dlist->string dl)
  (list->string (dlist->list dl)))

;;; ============================================================
;;; Core Operations
;;; ============================================================

;;; Append two difference lists - O(1)
(define (dlist-append dl1 dl2)
  (lambda (tail) (dl1 (dl2 tail))))

;;; Prepend element to front - O(1)
(define (dlist-cons x dl)
  (lambda (tail) (cons x (dl tail))))

;;; Append element to back - O(1)
(define (dlist-snoc dl x)
  (lambda (tail) (dl (cons x tail))))

;;; Concatenate list of dlists - O(k) where k is number of dlists
(define (dlist-concat dls)
  (fold-right dlist-append dlist-empty dls))

;;; ============================================================
;;; Building Operations
;;; ============================================================

;;; Build dlist from multiple elements
(define (dlist . xs)
  (list->dlist xs))

;;; Replicate element n times
(define (dlist-replicate n x)
  (if (<= n 0)
      dlist-empty
      (dlist-cons x (dlist-replicate (- n 1) x))))

;;; Build from range [start, end)
(define (dlist-range start end)
  (if (>= start end)
      dlist-empty
      (dlist-cons start (dlist-range (+ start 1) end))))

;;; Unfold to build dlist
(define (dlist-unfold pred f seed)
  (if (pred seed)
      dlist-empty
      (let ([x (f seed)])
           (dlist-cons (car x) (dlist-unfold pred f (cdr x))))))

;;; ============================================================
;;; Transformations
;;; ============================================================

;;; Map function over dlist - O(n)
(define (dlist-map f dl)
  (list->dlist (map f (dlist->list dl))))

;;; Filter dlist by predicate - O(n)
(define (dlist-filter pred dl)
  (list->dlist (filter pred (dlist->list dl))))

;;; FlatMap (concatMap) - O(n*m)
(define (dlist-flatmap f dl)
  (dlist-concat (map f (dlist->list dl))))

;;; Fold over dlist - O(n)
(define (dlist-fold-left f init dl)
  (fold-left f init (dlist->list dl)))

(define (dlist-fold-right f init dl)
  (fold-right f init (dlist->list dl)))

;;; ============================================================
;;; Queries
;;; ============================================================

;;; Get length - O(n)
(define (dlist-length dl)
  (length (dlist->list dl)))

;;; Get first element - O(n)
(define (dlist-head dl)
  (let ([lst (dlist->list dl)])
       (if (null? lst)
           (error 'dlist-head "empty dlist")
           (car lst))))

;;; Get tail - O(n)
(define (dlist-tail dl)
  (let ([lst (dlist->list dl)])
       (if (null? lst)
           (error 'dlist-tail "empty dlist")
           (list->dlist (cdr lst)))))

;;; Safe head
(define (dlist-head-maybe dl)
  (let ([lst (dlist->list dl)])
       (if (null? lst)
           nothing
           (just (car lst)))))

;;; Take first n elements - O(n)
(define (dlist-take n dl)
  (list->dlist (take-list n (dlist->list dl))))

(define (take-list n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take-list (- n 1) (cdr lst)))))

;;; Drop first n elements - O(n)
(define (dlist-drop n dl)
  (list->dlist (drop-list n (dlist->list dl))))

(define (drop-list n lst)
  (if (or (<= n 0) (null? lst))
      lst
      (drop-list (- n 1) (cdr lst))))

;;; ============================================================
;;; String Building
;;; ============================================================

;;; Efficient string builder using dlists
(define (string-builder)
  dlist-empty)

(define (builder-append builder str)
  (dlist-append builder (string->dlist str)))

(define (builder-append-char builder c)
  (dlist-snoc builder c))

(define (builder-append-line builder str)
  (dlist-append builder
                (dlist-snoc (string->dlist str)
                            (string-ref "
" 0))))

(define (builder->string builder)
  (dlist->string builder))

;;; Join strings with separator
(define (dlist-intercalate sep dls)
  (if (null? dls)
      dlist-empty
      (fold-left (lambda (acc dl)
                         (dlist-append acc (dlist-append sep dl)))
                 (car dls)
                 (cdr dls))))

;;; Join list of strings
(define (string-join-dlist strs sep)
  (builder->string
   (dlist-intercalate (string->dlist sep)
                      (map string->dlist strs))))

;;; ============================================================
;;; Monoid Operations
;;; ============================================================

;;; DList forms a monoid with dlist-empty and dlist-append
(define dlist-mempty dlist-empty)
(define dlist-mappend dlist-append)

;;; Fold with monoid
(define (dlist-mconcat dls)
  (dlist-concat dls))

;;; ============================================================
;;; Show/Display
;;; ============================================================

;;; Convert to displayable string
(define (dlist-show dl)
  (let ([lst (dlist->list dl)])
       (string-append "DList["
                      (fold-left (lambda (acc x)
                                         (if (string=? acc "")
                                             (format "~a" x)
                                             (string-append acc ", " (format "~a" x))))
                                 ""
                                 lst)
                      "]")))

;;; ============================================================
;;; Comparison
;;; ============================================================

;;; Compare two dlists (by their list representations)
(define (dlist-equal? dl1 dl2)
  (equal? (dlist->list dl1) (dlist->list dl2)))

;;; ============================================================
;;; Reversing
;;; ============================================================

;;; Reverse a dlist - O(n)
(define (dlist-reverse dl)
  (list->dlist (reverse (dlist->list dl))))

;;; ============================================================
;;; Specialized Builders
;;; ============================================================

;;; Build dlist by repeatedly applying function
(define (dlist-iterate n f x)
  (if (<= n 0)
      dlist-empty
      (dlist-cons x (dlist-iterate (- n 1) f (f x)))))

;;; Cycle elements (lazy-ish, takes n copies)
(define (dlist-cycle n dl)
  (if (<= n 0)
      dlist-empty
      (dlist-append dl (dlist-cycle (- n 1) dl))))

;;; Intersperse element between dlist elements
(define (dlist-intersperse sep dl)
  (let ([lst (dlist->list dl)])
       (if (null? lst)
           dlist-empty
           (list->dlist (intersperse-list sep lst)))))

(define (intersperse-list sep lst)
  (if (or (null? lst) (null? (cdr lst)))
      lst
      (cons (car lst) (cons sep (intersperse-list sep (cdr lst))))))

;;; ============================================================
;;; Zipping
;;; ============================================================

;;; Zip two dlists
(define (dlist-zip dl1 dl2)
  (list->dlist (zip-lists (dlist->list dl1) (dlist->list dl2))))

(define (zip-lists lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (cons (car lst1) (car lst2))
            (zip-lists (cdr lst1) (cdr lst2)))))

;;; Zip with function
(define (dlist-zip-with f dl1 dl2)
  (list->dlist (zip-with-lists f (dlist->list dl1) (dlist->list dl2))))

(define (zip-with-lists f lst1 lst2)
  (if (or (null? lst1) (null? lst2))
      '()
      (cons (f (car lst1) (car lst2))
            (zip-with-lists f (cdr lst1) (cdr lst2)))))

;;; ============================================================
;;; Partitioning
;;; ============================================================

;;; Split dlist at position
(define (dlist-split-at n dl)
  (let ([lst (dlist->list dl)])
       (cons (list->dlist (take-list n lst))
             (list->dlist (drop-list n lst)))))

;;; Partition by predicate
(define (dlist-partition pred dl)
  (let ([lst (dlist->list dl)])
       (cons (list->dlist (filter pred lst))
             (list->dlist (filter (lambda (x) (not (pred x))) lst)))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Types:
;;;   dlist?, dlist-empty, dlist-empty?, dlist-singleton
;;;
;;; Conversions:
;;;   list->dlist, dlist->list, string->dlist, dlist->string
;;;
;;; Core operations:
;;;   dlist-append, dlist-cons, dlist-snoc, dlist-concat
;;;
;;; Building:
;;;   dlist, dlist-replicate, dlist-range, dlist-unfold
;;;
;;; Transformations:
;;;   dlist-map, dlist-filter, dlist-flatmap, dlist-fold-left,
;;;   dlist-fold-right, dlist-reverse
;;;
;;; Queries:
;;;   dlist-length, dlist-head, dlist-tail, dlist-head-maybe,
;;;   dlist-take, dlist-drop
;;;
;;; String building:
;;;   string-builder, builder-append, builder-append-char,
;;;   builder-append-line, builder->string, string-join-dlist
;;;
;;; Monoid:
;;;   dlist-mempty, dlist-mappend, dlist-mconcat
;;;
;;; Specialized:
;;;   dlist-iterate, dlist-cycle, dlist-intersperse
;;;
;;; Zipping:
;;;   dlist-zip, dlist-zip-with
;;;
;;; Partitioning:
;;;   dlist-split-at, dlist-partition
;;;
;;; Comparison:
;;;   dlist-equal?, dlist-show
