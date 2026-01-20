(load "core/base/prelude.ss")

(doc 'module 'dict)
(doc 'description "Key-Value Dictionary")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)
(doc 'note "Immutable functional dictionary (map) using association list")
(doc 'note "Simple alist-based implementation. For large dictionaries, consider balanced trees")

(define dict-empty '())
(doc dict-empty 'type 'Dict)
(doc dict-empty 'description "The empty dictionary")

(define (dict-empty? dict)
  (doc 'type (-> Dict Boolean))
  (doc 'description "Check if dictionary is empty")
  (null? dict))

(define (dict-lookup key dict)
  (doc 'type (-> κ Dict (Maybe ν)))
  (doc 'description "Look up value by key")
  (let ([pair (assoc key dict)])
       (if pair
           (cdr pair)
           #f)))

(define (dict-has-key? key dict)
  (doc 'type (-> κ Dict Boolean))
  (doc 'description "Check if key exists in dictionary")
  (if (assoc key dict) #t #f))

(define (dict-assoc key value dict)
  (doc 'type (-> κ ν Dict Dict))
  (doc 'description "Associate key with value")
  (doc 'note "If key exists, updates value. Otherwise adds new entry")
  (cons (cons key value)
        (dict-dissoc key dict)))

(define (dict-dissoc key dict)
  (doc 'type (-> κ Dict Dict))
  (doc 'description "Remove key from dictionary")
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? key (car (car remaining)))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

(define (dict-keys dict)
  (doc 'type (-> Dict (List κ)))
  (doc 'description "Get list of all keys")
  (map car dict))

(define (dict-values dict)
  (doc 'type (-> Dict (List ν)))
  (doc 'description "Get list of all values")
  (map cdr dict))

(define (dict-entries dict)
  (doc 'type (-> Dict (List (Pair κ ν))))
  (doc 'description "Get list of all key-value pairs")
  dict)

(define (dict-merge dict1 dict2)
  (doc 'type (-> Dict Dict Dict))
  (doc 'description "Merge two dictionaries")
  (doc 'note "If keys overlap, dict2 wins")
  (let loop ([remaining dict2]
             [result dict1])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (dict-assoc (car (car remaining))
                             (cdr (car remaining))
                             result)))))

(define (dict-map-values f dict)
  (doc 'type (-> (-> ν μ) Dict Dict))
  (doc 'description "Apply function to all values, keeping keys the same")
  (map (lambda (pair)
               (cons (car pair) (f (cdr pair))))
       dict))

(define (dict-filter pred dict)
  (doc 'type (-> (-> κ ν Boolean) Dict Dict))
  (doc 'description "Filter dictionary by predicate on key-value pairs")
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(pred (car (car remaining)) (cdr (car remaining)))
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

(define (dict-size dict)
  (doc 'type (-> Dict Nat))
  (doc 'description "Get number of key-value pairs")
  (length dict))

(define (dict->alist dict)
  (doc 'type (-> Dict (List (Pair κ ν))))
  (doc 'description "Convert dictionary to association list (identity)")
  dict)

(define (alist->dict alist)
  (doc 'type (-> (List (Pair κ ν)) Dict))
  (doc 'description "Convert association list to dictionary (identity)")
  alist)
