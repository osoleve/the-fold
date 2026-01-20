(load "core/base/prelude.ss")

(doc 'module 'set)
(doc 'description "Unordered Set")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)
(doc 'note "Immutable functional set with no duplicates")
(doc 'note "Simple list-based implementation. For large sets, consider balanced trees")

(define set-empty '())
(doc set-empty 'type 'Set)
(doc set-empty 'description "The empty set")

(define (set-empty? set)
  (doc 'type (-> Set Boolean))
  (doc 'description "Check if set is empty")
  (null? set))

(define (set-member? elem set)
  (doc 'type (-> α Set Boolean))
  (doc 'description "Check if element is in set")
  (cond
   [(null? set) #f]
   [(equal? elem (car set)) #t]
   [else (set-member? elem (cdr set))]))

(define (set-add elem set)
  (doc 'type (-> α Set Set))
  (doc 'description "Add element to set")
  (if (set-member? elem set)
      set
      (cons elem set)))

(define (set-remove elem set)
  (doc 'type (-> α Set Set))
  (doc 'description "Remove element from set")
  (let loop ([remaining set]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? elem (car remaining))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

(define (set-union set1 set2)
  (doc 'type (-> Set Set Set))
  (doc 'description "Union of two sets")
  (let loop ([remaining set1]
             [result set2])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (set-add (car remaining) result)))))

(define (set-intersection set1 set2)
  (doc 'type (-> Set Set Set))
  (doc 'description "Intersection of two sets")
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

(define (set-difference set1 set2)
  (doc 'type (-> Set Set Set))
  (doc 'description "Elements in set1 but not in set2")
  (let loop ([remaining set1]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining) acc)]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

(define (set-subset? set1 set2)
  (doc 'type (-> Set Set Boolean))
  (doc 'description "Check if set1 is a subset of set2")
  (let loop ([remaining set1])
       (cond
        [(null? remaining) #t]
        [(set-member? (car remaining) set2)
         (loop (cdr remaining))]
        [else #f])))

(define (set-size set)
  (doc 'type (-> Set Nat))
  (doc 'description "Get number of elements in set")
  (length set))

(define (set->list set)
  (doc 'type (-> Set (List α)))
  (doc 'description "Convert set to list (arbitrary order)")
  set)

(define (list->set lst)
  (doc 'type (-> (List α) Set))
  (doc 'description "Convert list to set (removes duplicates)")
  (let loop ([remaining lst]
             [acc set-empty])
       (if (null? remaining)
           acc
           (loop (cdr remaining) (set-add (car remaining) acc)))))
