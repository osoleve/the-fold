;;; lattice/data/dict.ss — Key-Value Dictionary
;;;
;;; Immutable functional dictionary (map) using association list.
;;; Uses equal? for key comparison.
;;;
;;; Dict κ ν = (List (Pair κ ν))
;;;
;;; Note: Simple alist-based implementation. For large dictionaries,
;;; consider balanced trees or hash tables.
;;;
;;; TIER: 0 (no lattice dependencies)

;;; dict-empty : Dict
;;; The empty dictionary.
(define dict-empty '())

;;; dict-empty? : Dict → Boolean
;;; Check if dictionary is empty.
(define (dict-empty? dict)
  (null? dict))

;;; dict-lookup : κ Dict → (Maybe ν)
;;; Look up value by key. Returns #f if not found.
(define (dict-lookup key dict)
  (let ([pair (assoc key dict)])
       (if pair
           (cdr pair)
           #f)))

;;; dict-has-key? : κ Dict → Boolean
;;; Check if key exists in dictionary.
(define (dict-has-key? key dict)
  (if (assoc key dict) #t #f))

;;; dict-assoc : κ ν Dict → Dict
;;; Associate key with value. Returns new dictionary.
;;; If key exists, updates value. Otherwise adds new entry.
(define (dict-assoc key value dict)
  (cons (cons key value)
        (dict-dissoc key dict)))

;;; dict-dissoc : κ Dict → Dict
;;; Remove key from dictionary. Returns new dictionary.
(define (dict-dissoc key dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(equal? key (car (car remaining)))
         (append (reverse acc) (cdr remaining))]
        [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; dict-keys : Dict → (List κ)
;;; Get list of all keys.
(define (dict-keys dict)
  (map car dict))

;;; dict-values : Dict → (List ν)
;;; Get list of all values.
(define (dict-values dict)
  (map cdr dict))

;;; dict-entries : Dict → (List (Pair κ ν))
;;; Get list of all key-value pairs.
(define (dict-entries dict)
  dict)

;;; dict-merge : Dict Dict → Dict
;;; Merge two dictionaries. If keys overlap, dict2 wins.
(define (dict-merge dict1 dict2)
  (let loop ([remaining dict2]
             [result dict1])
       (if (null? remaining)
           result
           (loop (cdr remaining)
                 (dict-assoc (car (car remaining))
                             (cdr (car remaining))
                             result)))))

;;; dict-map-values : (ν → μ) Dict → Dict
;;; Apply function to all values, keeping keys the same.
(define (dict-map-values f dict)
  (map (lambda (pair)
               (cons (car pair) (f (cdr pair))))
       dict))

;;; dict-filter : (κ ν → Boolean) Dict → Dict
;;; Filter dictionary by predicate on key-value pairs.
(define (dict-filter pred dict)
  (let loop ([remaining dict]
             [acc '()])
       (cond
        [(null? remaining) (reverse acc)]
        [(pred (car (car remaining)) (cdr (car remaining)))
         (loop (cdr remaining) (cons (car remaining) acc))]
        [else (loop (cdr remaining) acc)])))

;;; dict-size : Dict → Nat
;;; Get number of key-value pairs.
(define (dict-size dict)
  (length dict))

;;; dict->alist : (Dict κ ν) → (List (Pair κ ν))
;;; Convert dictionary to association list (identity).
(define (dict->alist dict)
  dict)

;;; alist->dict : (List (Pair κ ν)) → (Dict κ ν)
;;; Convert association list to dictionary (identity).
(define (alist->dict alist)
  alist)
