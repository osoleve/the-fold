;;; lattice/data/alist.ss — Association List Convenience Helpers
;;; @module data/alist
;;; @requires prelude

(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'data/alist)
(doc 'description "Pure association list operations using eq? for key comparison")
(doc 'layer 'lattice)
(doc 'tier 0)
(doc 'purity 'total)
(doc 'note "Complements dict.ss (which uses equal? via assoc). This module uses eq? via assq, matching the dominant pattern across the lattice where keys are symbols.")

;;; --- Lookup ---

(doc alist-ref 'type '(-> Alist Key (Maybe Value)))
(doc alist-ref 'description "Look up key in alist, returning the value or #f if not found")
(doc alist-ref 'export #t)
(define (alist-ref alist key)
  (let ([pair (assq key alist)])
    (if pair (cdr pair) #f)))

(doc alist-ref/default 'type '(-> Alist Key Value Value))
(doc alist-ref/default 'description "Look up key in alist, returning the value or default if not found")
(doc alist-ref/default 'export #t)
(define (alist-ref/default alist key default)
  (let ([pair (assq key alist)])
    (if pair (cdr pair) default)))

;;; --- Mutation (functional) ---

(doc alist-set 'type '(-> Alist Key Value Alist))
(doc alist-set 'description "Functionally set key to value, replacing existing entry or adding new one")
(doc alist-set 'export #t)
(define (alist-set alist key value)
  (cons (cons key value) (alist-remove alist key)))

(doc alist-update 'type '(-> Alist Key (-> Value Value) Value Alist))
(doc alist-update 'description "Apply fn to current value at key (or default if missing), storing the result")
(doc alist-update 'export #t)
(define (alist-update alist key fn default)
  (let ([current (alist-ref/default alist key default)])
    (alist-set alist key (fn current))))

(doc alist-remove 'type '(-> Alist Key Alist))
(doc alist-remove 'description "Remove the first entry with the given key")
(doc alist-remove 'export #t)
(define (alist-remove alist key)
  (let loop ([remaining alist] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(eq? key (car (car remaining)))
       (append (reverse acc) (cdr remaining))]
      [else (loop (cdr remaining) (cons (car remaining) acc))])))

;;; --- Merge ---

(doc alist-merge 'type '(-> Alist Alist Alist))
(doc alist-merge 'description "Merge two alists; entries in b win on key conflict")
(doc alist-merge 'export #t)
(define (alist-merge a b)
  (let loop ([remaining b] [result a])
    (if (null? remaining)
        result
        (loop (cdr remaining)
              (alist-set result
                         (car (car remaining))
                         (cdr (car remaining)))))))

;;; --- Projection ---

(doc alist-keys 'type '(-> Alist (List Key)))
(doc alist-keys 'description "Extract the list of keys from an alist")
(doc alist-keys 'export #t)
(define (alist-keys alist)
  (map car alist))

(doc alist-values 'type '(-> Alist (List Value)))
(doc alist-values 'description "Extract the list of values from an alist")
(doc alist-values 'export #t)
(define (alist-values alist)
  (map cdr alist))

;;; --- Transform ---

(doc alist-map 'type '(-> (-> Key Value Value) Alist Alist))
(doc alist-map 'description "Map over alist values; fn receives key and value, returns new value")
(doc alist-map 'export #t)
(define (alist-map fn alist)
  (map (lambda (pair)
         (cons (car pair) (fn (car pair) (cdr pair))))
       alist))
