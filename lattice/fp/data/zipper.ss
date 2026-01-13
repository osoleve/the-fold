;;; lattice/fp/data/zipper.ss — Functional Zippers
;;;
;;; A zipper is a cursor into a data structure, enabling efficient local
;;; navigation and modification. This module implements list zippers with
;;; O(1) movement and modification at the focus point.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - List zipper type (left context, focus, right context)
;;;   - Navigation operations (left, right, start, end)
;;;   - Modification operations (get, set, modify, insert, delete)
;;;   - Conversion functions (to/from list)
;;;   - Functor and Foldable instances
;;;
;;; References:
;;;   - Huet, "The Zipper" (1997) JFP 7(5):549-554
;;;   - McBride, "The Derivative of a Regular Type is its Type of One-Hole Contexts"
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

;;; ============================================================
;;; List Zipper Type
;;; ============================================================
;;;
;;; A list zipper consists of:
;;;   - left:  Elements to the left of focus (reversed, closest first)
;;;   - focus: The current element (wrapped in Maybe for empty case)
;;;   - right: Elements to the right of focus
;;;
;;; For list [1, 2, 3, 4, 5] with focus on 3:
;;;   left  = (2 1)      ; reversed: 2 is closest to focus
;;;   focus = (just 3)
;;;   right = (4 5)
;;;
;;; Empty zipper has focus = nothing, left = (), right = ()
;;;
;;; Design note: Navigation allows moving "past-end" (focus=nothing after last
;;; element) but not "before-start". This asymmetry supports append semantics
;;; where you can navigate past the end and insert. The past-end state is useful
;;; for building lists incrementally. There is no symmetric before-start state.
;;;
;;; ListZipper α = {left: (List α), focus: (Maybe α), right: (List α)}

;;; zipper-tag : Symbol
;;; Tag for zipper data type.
(define zipper-tag 'list-zipper)

;;; make-zipper : (List α) × (Maybe α) × (List α) → (ListZipper α)
;;; Internal constructor.
(define (make-zipper left focus right)
  (list zipper-tag left focus right))

;;; zipper? : α → Bool
;;; Check if value is a list zipper.
(define (zipper? z)
  (and (pair? z)
       (eq? (car z) zipper-tag)
       (= (length z) 4)))

;;; zipper-left : (ListZipper α) → (List α)
;;; Get left context (reversed).
(define (zipper-left z)
  (if (zipper? z)
      (list-ref z 1)
      (error 'zipper-left "not a zipper")))

;;; zipper-focus-maybe : (ListZipper α) → (Maybe α)
;;; Get focus as Maybe.
(define (zipper-focus-maybe z)
  (if (zipper? z)
      (list-ref z 2)
      (error 'zipper-focus-maybe "not a zipper")))

;;; zipper-right : (ListZipper α) → (List α)
;;; Get right context.
(define (zipper-right z)
  (if (zipper? z)
      (list-ref z 3)
      (error 'zipper-right "not a zipper")))

;;; ============================================================
;;; Constructors
;;; ============================================================

;;; zipper-empty : (ListZipper α)
;;; The empty zipper.
(define zipper-empty
  (make-zipper '() nothing '()))

;;; zipper-empty? : (ListZipper α) → Bool
;;; Check if zipper is empty.
(define (zipper-empty? z)
  (and (zipper? z)
       (null? (zipper-left z))
       (nothing? (zipper-focus-maybe z))
       (null? (zipper-right z))))

;;; zipper-singleton : α → (ListZipper α)
;;; Create zipper with single element at focus.
(define (zipper-singleton x)
  (make-zipper '() (just x) '()))

;;; list->zipper : (List α) → (ListZipper α)
;;; Convert list to zipper, focusing on first element.
(define (list->zipper lst)
  (if (null? lst)
      zipper-empty
      (make-zipper '() (just (car lst)) (cdr lst))))

;;; zipper->list : (ListZipper α) → (List α)
;;; Convert zipper back to list.
(define (zipper->list z)
  (let ([left (zipper-left z)]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (append (reverse left)
            (if (nothing? focus)
                '()
                (cons (from-just focus) right)))))

;;; zipper-from-position : (List α) × Nat → (ListZipper α)
;;; Create zipper focused at given position (0-indexed).
;;; If position exceeds list length, focuses past the end.
(define (zipper-from-position lst pos)
  (let loop ([left '()] [rest lst] [n pos])
    (cond
      [(null? rest)
       (make-zipper left nothing '())]
      [(<= n 0)
       (make-zipper left (just (car rest)) (cdr rest))]
      [else
       (loop (cons (car rest) left) (cdr rest) (- n 1))])))

;;; ============================================================
;;; Navigation
;;; ============================================================

;;; zipper-has-focus? : (ListZipper α) → Bool
;;; Check if zipper has a focused element.
(define (zipper-has-focus? z)
  (just? (zipper-focus-maybe z)))

;;; zipper-can-go-left? : (ListZipper α) → Bool
;;; Check if leftward movement is possible.
(define (zipper-can-go-left? z)
  (not (null? (zipper-left z))))

;;; zipper-can-go-right? : (ListZipper α) → Bool
;;; Check if rightward movement is possible.
(define (zipper-can-go-right? z)
  (or (and (zipper-has-focus? z)
           (not (null? (zipper-right z))))
      (and (not (zipper-has-focus? z))
           (not (null? (zipper-right z))))))

;;; zipper-left! : (ListZipper α) → (Maybe (ListZipper α))
;;; Move focus left. Returns nothing if at start.
(define (zipper-left! z)
  (let ([left (zipper-left z)]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (if (null? left)
        nothing
        (just
         (make-zipper
          (cdr left)
          (just (car left))
          (if (nothing? focus)
              right
              (cons (from-just focus) right)))))))

;;; zipper-right! : (ListZipper α) → (Maybe (ListZipper α))
;;; Move focus right. Returns nothing if at end.
(define (zipper-right! z)
  (let ([left (zipper-left z)]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (cond
      ;; Has focus and more to the right
      [(and (just? focus) (not (null? right)))
       (just
        (make-zipper
         (cons (from-just focus) left)
         (just (car right))
         (cdr right)))]
      ;; Has focus but nothing to the right - move past end
      [(and (just? focus) (null? right))
       (just
        (make-zipper
         (cons (from-just focus) left)
         nothing
         '()))]
      ;; No focus but has right elements (shouldn't happen in normal use)
      [(and (nothing? focus) (not (null? right)))
       (just
        (make-zipper
         left
         (just (car right))
         (cdr right)))]
      ;; No focus and nothing to the right
      [else nothing])))

;;; zipper-start : (ListZipper α) → (ListZipper α)
;;; Move to the start of the list.
(define (zipper-start z)
  (let loop ([current z])
    (let ([moved (zipper-left! current)])
      (if (nothing? moved)
          current
          (loop (from-just moved))))))

;;; zipper-end : (ListZipper α) → (ListZipper α)
;;; Move to the end of the list (last element focused).
(define (zipper-end z)
  (let loop ([current z])
    (let ([moved (zipper-right! current)])
      (if (nothing? moved)
          current
          (let ([next (from-just moved)])
            ;; Stop if we moved past the last element
            (if (nothing? (zipper-focus-maybe next))
                current
                (loop next)))))))

;;; zipper-goto : (ListZipper α) × Nat → (ListZipper α)
;;; Move to specific position (0-indexed). Clamps to valid range.
(define (zipper-goto z pos)
  (zipper-from-position (zipper->list z) pos))

;;; zipper-position : (ListZipper α) → Nat
;;; Get current position (0-indexed).
(define (zipper-position z)
  (length (zipper-left z)))

;;; zipper-length : (ListZipper α) → Nat
;;; Get total length of the underlying list.
(define (zipper-length z)
  (+ (length (zipper-left z))
     (if (zipper-has-focus? z) 1 0)
     (length (zipper-right z))))

;;; ============================================================
;;; Focus Operations
;;; ============================================================

;;; zipper-focus : (ListZipper α) → α
;;; Get the focused element. Error if no focus.
(define (zipper-focus z)
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        (error 'zipper-focus "no focused element")
        (from-just focus))))

;;; zipper-focus-or : (ListZipper α) × α → α
;;; Get focused element or default if none.
(define (zipper-focus-or z default)
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        default
        (from-just focus))))

;;; zipper-set : (ListZipper α) × α → (ListZipper α)
;;; Set the focused element. Error if no focus.
(define (zipper-set z x)
  (if (not (zipper-has-focus? z))
      (error 'zipper-set "no focused element to set")
      (make-zipper (zipper-left z)
                   (just x)
                   (zipper-right z))))

;;; zipper-modify : (ListZipper α) × (α → α) → (ListZipper α)
;;; Modify the focused element. Error if no focus.
(define (zipper-modify z f)
  (if (not (zipper-has-focus? z))
      (error 'zipper-modify "no focused element to modify")
      (make-zipper (zipper-left z)
                   (just (f (from-just (zipper-focus-maybe z))))
                   (zipper-right z))))

;;; ============================================================
;;; Insertion
;;; ============================================================

;;; zipper-insert-left : (ListZipper α) × α → (ListZipper α)
;;; Insert element to the left of focus.
;;; If no focus, inserts at current position.
(define (zipper-insert-left z x)
  (make-zipper (cons x (zipper-left z))
               (zipper-focus-maybe z)
               (zipper-right z)))

;;; zipper-insert-right : (ListZipper α) × α → (ListZipper α)
;;; Insert element to the right of focus.
;;; Design: If past-end (no focus), element becomes new focus. This provides
;;; convenient append semantics where the newly inserted element is focused.
;;; Contrast with zipper-insert-left which preserves focus state.
(define (zipper-insert-right z x)
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        (make-zipper (zipper-left z)
                     (just x)
                     (zipper-right z))
        (make-zipper (zipper-left z)
                     focus
                     (cons x (zipper-right z))))))

;;; zipper-insert-focus : (ListZipper α) × α → (ListZipper α)
;;; Insert element as the new focus, pushing old focus right.
(define (zipper-insert-focus z x)
  (let ([focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (make-zipper (zipper-left z)
                 (just x)
                 (if (nothing? focus)
                     right
                     (cons (from-just focus) right)))))

;;; ============================================================
;;; Deletion
;;; ============================================================

;;; zipper-delete : (ListZipper α) → (ListZipper α)
;;; Delete focused element. Focus moves right if possible, else left.
(define (zipper-delete z)
  (if (not (zipper-has-focus? z))
      z  ; Nothing to delete
      (let ([left (zipper-left z)]
            [right (zipper-right z)])
        (cond
          ;; Has elements to the right - focus moves right
          [(not (null? right))
           (make-zipper left
                        (just (car right))
                        (cdr right))]
          ;; Has elements to the left - focus moves left
          [(not (null? left))
           (make-zipper (cdr left)
                        (just (car left))
                        '())]
          ;; Only element - return empty zipper
          [else zipper-empty]))))

;;; zipper-delete-left : (ListZipper α) → (ListZipper α)
;;; Delete element immediately to the left of focus.
;;; No-op if nothing to the left.
(define (zipper-delete-left z)
  (let ([left (zipper-left z)])
    (if (null? left)
        z
        (make-zipper (cdr left)
                     (zipper-focus-maybe z)
                     (zipper-right z)))))

;;; zipper-delete-right : (ListZipper α) → (ListZipper α)
;;; Delete element immediately to the right of focus.
;;; No-op if nothing to the right.
(define (zipper-delete-right z)
  (let ([right (zipper-right z)])
    (if (null? right)
        z
        (make-zipper (zipper-left z)
                     (zipper-focus-maybe z)
                     (cdr right)))))

;;; ============================================================
;;; Bulk Operations
;;; ============================================================

;;; zipper-map : (α → β) × (ListZipper α) → (ListZipper β)
;;; Map function over all elements in zipper.
(define (zipper-map f z)
  (make-zipper (map f (zipper-left z))
               (maybe-fmap f (zipper-focus-maybe z))
               (map f (zipper-right z))))

;;; zipper-filter : (α → Bool) × (ListZipper α) → (ListZipper α)
;;; Filter elements. Focus removed if it doesn't match predicate.
(define (zipper-filter pred z)
  (list->zipper (filter pred (zipper->list z))))

;;; zipper-fold-left : (β × α → β) × β × (ListZipper α) → β
;;; Left fold over zipper elements in order.
(define (zipper-fold-left f init z)
  (fold-left f init (zipper->list z)))

;;; zipper-fold-right : (α × β → β) × β × (ListZipper α) → β
;;; Right fold over zipper elements.
(define (zipper-fold-right f init z)
  (fold-right f init (zipper->list z)))

;;; zipper-find : (α → Bool) × (ListZipper α) → (Maybe (ListZipper α))
;;; Find first element matching predicate, returning zipper focused there.
(define (zipper-find pred z)
  (let loop ([current (zipper-start z)])
    (cond
      [(not (zipper-has-focus? current)) nothing]
      [(pred (zipper-focus current)) (just current)]
      [else
       (let ([next (zipper-right! current)])
         (if (nothing? next)
             nothing
             (loop (from-just next))))])))

;;; zipper-find-index : (α → Bool) × (ListZipper α) → (Maybe Nat)
;;; Find index of first element matching predicate.
(define (zipper-find-index pred z)
  (let loop ([lst (zipper->list z)] [idx 0])
    (cond
      [(null? lst) nothing]
      [(pred (car lst)) (just idx)]
      [else (loop (cdr lst) (+ idx 1))])))

;;; ============================================================
;;; Context Operations
;;; ============================================================

;;; zipper-context : (ListZipper α) → (List α)
;;; Get all elements except focus (in order).
(define (zipper-context z)
  (append (reverse (zipper-left z))
          (zipper-right z)))

;;; zipper-take-left : (ListZipper α) × Nat → (List α)
;;; Take n elements to the left (in left-to-right order).
(define (zipper-take-left z n)
  (reverse (take (min n (length (zipper-left z))) (zipper-left z))))

;;; zipper-take-right : (ListZipper α) × Nat → (List α)
;;; Take n elements to the right.
(define (zipper-take-right z n)
  (take (min n (length (zipper-right z))) (zipper-right z)))

;;; zipper-window : (ListZipper α) × Nat × Nat → (List α)
;;; Get elements in window around focus (left-count, right-count).
;;; Includes focus in the middle.
(define (zipper-window z left-count right-count)
  (append (zipper-take-left z left-count)
          (if (zipper-has-focus? z)
              (cons (zipper-focus z) (zipper-take-right z right-count))
              (zipper-take-right z right-count))))

;;; ============================================================
;;; Splitting and Joining
;;; ============================================================

;;; zipper-split : (ListZipper α) → (Pair (List α) (List α))
;;; Split at focus into (left-elements, focus+right-elements).
(define (zipper-split z)
  (cons (reverse (zipper-left z))
        (if (zipper-has-focus? z)
            (cons (zipper-focus z) (zipper-right z))
            (zipper-right z))))

;;; zipper-append : (ListZipper α) × (List α) → (ListZipper α)
;;; Append list to the right of zipper.
(define (zipper-append z lst)
  (make-zipper (zipper-left z)
               (zipper-focus-maybe z)
               (append (zipper-right z) lst)))

;;; zipper-prepend : (List α) × (ListZipper α) → (ListZipper α)
;;; Prepend list to the beginning of the underlying list.
(define (zipper-prepend lst z)
  (make-zipper (append (zipper-left z) (reverse lst))
               (zipper-focus-maybe z)
               (zipper-right z)))

;;; ============================================================
;;; Type Class Instances
;;; ============================================================

;;; zipper-functor : (Functor ListZipper)
;;; The Functor instance for ListZipper.
(define zipper-functor
  (list 'functor zipper-map))

;;; zipper-fmap : (α → β) × (ListZipper α) → (ListZipper β)
;;; Alias for zipper-map.
(define zipper-fmap zipper-map)

;;; ============================================================
;;; Comonad Instance
;;; ============================================================
;;;
;;; ListZipper forms a Comonad with:
;;;   - extract: Get the focused element
;;;   - extend: Apply contextual function to all positions
;;;
;;; Comonad laws:
;;;   1. extend extract = id
;;;   2. extract . extend f = f
;;;   3. extend f . extend g = extend (f . extend g)

;;; zipper-extract : (ListZipper α) → α
;;; Extract the focused element (Comonad extract).
;;; Partial: errors on empty zipper.
(define zipper-extract zipper-focus)

;;; zipper-extend : ((ListZipper α) → β) × (ListZipper α) → (ListZipper β)
;;; Extend a contextual function to all positions.
;;; For each position, applies f to the zipper focused at that position.
(define (zipper-extend f z)
  (if (zipper-empty? z)
      zipper-empty
      (let* ([lst (zipper->list z)]
             [len (length lst)]
             [results (map (lambda (i)
                            (f (zipper-from-position lst i)))
                          (iota len))]
             [current-pos (zipper-position z)])
        (zipper-from-position results current-pos))))

;;; zipper-duplicate : (ListZipper α) → (ListZipper (ListZipper α))
;;; Duplicate: zipper of all possible focuses.
(define (zipper-duplicate z)
  (zipper-extend (lambda (x) x) z))

;;; zipper-comonad : (Comonad ListZipper)
;;; The Comonad instance for ListZipper.
(define zipper-comonad
  (list 'comonad
        zipper-functor
        zipper-extract
        zipper-extend))

;;; ============================================================
;;; Equality and Display
;;; ============================================================

;;; zipper-equal? : (ListZipper α) × (ListZipper α) → Bool
;;; Check equality of two zippers.
(define (zipper-equal? z1 z2)
  (and (equal? (zipper-left z1) (zipper-left z2))
       (equal? (zipper-focus-maybe z1) (zipper-focus-maybe z2))
       (equal? (zipper-right z1) (zipper-right z2))))

;;; zipper->string : (ListZipper α) → String
;;; Convert zipper to string representation.
;;; Shows: [left...] >focus< [right...]
(define (zipper->string z)
  (let ([left (reverse (zipper-left z))]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (string-append
     "["
     (if (null? left)
         ""
         (string-append
          (apply string-append
                 (map (lambda (x) (string-append (format "~a" x) " "))
                      left))))
     (if (nothing? focus)
         "|"
         (string-append ">" (format "~a" (from-just focus)) "<"))
     (if (null? right)
         ""
         (string-append
          " "
          (apply string-append
                 (map (lambda (x) (string-append (format "~a" x) " "))
                      (drop-right right 1)))
          (if (null? right)
              ""
              (format "~a" (last right)))))
     "]")))

;;; ============================================================
;;; Helper: iota
;;; ============================================================

;;; iota : Nat → (List Nat)
;;; Generate list [0, 1, ..., n-1].
(define (iota n)
  (let loop ([i 0] [acc '()])
    (if (>= i n)
        (reverse acc)
        (loop (+ i 1) (cons i acc)))))

;;; last : (List α) → α
;;; Get last element of non-empty list.
(define (last lst)
  (if (null? (cdr lst))
      (car lst)
      (last (cdr lst))))

;;; drop-right : (List α) × Nat → (List α)
;;; Drop n elements from right of list.
(define (drop-right lst n)
  (let ([len (length lst)])
    (if (<= len n)
        '()
        (take (- len n) lst))))

;;; take : Nat × (List α) → (List α)
;;; Take first n elements.
(define (take n lst)
  (if (or (<= n 0) (null? lst))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))
