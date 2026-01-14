;;; lattice/data/set.ss — Unordered Set
;;;
;;; Immutable functional set with no duplicates.
;;; Uses equal? for membership comparison.
;;;
;;; Set α = (List α)
;;;
;;; Note: Simple list-based implementation. For large sets,
;;; consider balanced trees or hash-based structures.
;;;
;;; TIER: 0 (no lattice dependencies)

;;; set-empty : (Set α)
;;; The empty set.
(define set-empty '())

;;; set-empty? : (Set α) → Boolean
;;; Check if set is empty.
(define (set-empty? set)
  (null? set))

;;; set-member? : α Set → Boolean
;;; Check if element is in set.
(define (set-member? elem set)
  (cond
   [(null? set) #f]
   [(equal? elem (car set)) #t]
   [else (set-member? elem (cdr set))]))

;;; set-add : α Set → Set
;;; Add element to set. Returns new set.
;;; If element already exists, returns original set.
(define (set-add elem set)
  (if (set-member? elem set)
      set
      (cons elem set)))

;;; set-remove : α Set → Set
;;; Remove element from set. Returns new set.
(define (set-remove elem set)
  (let loop ([remaining set]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? elem (car remaining))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; set-union : Set Set → Set
;;; Union of two sets.
(define (set-union set1 set2)
  (let loop ([remaining set1]
             [result set2])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (set-add (car remaining) result)))))

;;; set-intersection : Set Set → Set
;;; Intersection of two sets.
(define (set-intersection set1 set2)
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

;;; set-difference : Set Set → Set
;;; Elements in set1 but not in set2.
(define (set-difference set1 set2)
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) acc)]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; set-subset? : Set Set → Boolean
;;; Check if set1 is a subset of set2.
(define (set-subset? set1 set2)
  (let loop ([remaining set1])
       (cond
        [(null? remaining) #t]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining))]
        [else #f])))

;;; set-size : (Set α) → Nat
;;; Get number of elements in set.
(define (set-size set)
  (length set))

;;; set->list : (Set α) → (List α)
;;; Convert set to list (arbitrary order).
(define (set->list set)
  set)

;;; list->set : (List α) → (Set α)
;;; Convert list to set (removes duplicates).
(define (list->set lst)
  (let loop ([remaining lst]
             [acc set-empty])
       (if (null? remaining)
           acc
           (loop (cdr remaining) (set-add (car remaining) acc)))))
