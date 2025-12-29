;;; fabric/stitches/fp/trie.ss — Trie (Prefix Tree) Data Structure
;;;
;;; A pure functional implementation of tries (prefix trees) for efficient
;;; string/sequence operations like prefix matching, autocomplete, and
;;; dictionary lookups.
;;;
;;; Features:
;;; - Generic tries over any key sequence
;;; - String-specialized operations
;;; - Insertion, lookup, deletion
;;; - Prefix matching and completion
;;; - Key enumeration
;;; - Trie merging and folding
;;;
;;; This module builds on prelude.ss and combinators.ss.

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Trie Type
;;; ============================================================

;;; Trie = (trie value children)
;;; - value: Maybe value (Just v if this node is end of a key)
;;; - children: Alist (key-element -> Trie)

(define (make-trie value children)
  (list 'trie value children))

(define (trie? x)
  (and (list? x) (= (length x) 3) (eq? (car x) 'trie)))

(define (trie-value t) (cadr t))
(define (trie-children t) (caddr t))

;;; Empty trie
(define empty-trie (make-trie nothing '()))

;;; Check if trie is empty (no values anywhere)
(define (trie-empty? t)
  (and (nothing? (trie-value t))
       (null? (trie-children t))))

;;; ============================================================
;;; Basic Operations
;;; ============================================================

;;; Insert a key-value pair into trie
;;; key is a list of elements
(define (trie-insert t key value)
  (if (null? key)
      (make-trie (just value) (trie-children t))
      (let* ([k (car key)]
             [rest (cdr key)]
             [children (trie-children t)]
             [child (assoc k children)]
             [child-trie (if child (cdr child) empty-trie)]
             [new-child (trie-insert child-trie rest value)]
             [new-children (cons (cons k new-child)
                                 (filter (lambda (c) (not (equal? (car c) k)))
                                         children))])
        (make-trie (trie-value t) new-children))))

;;; Lookup a key in trie, returns Maybe value
(define (trie-lookup t key)
  (if (null? key)
      (trie-value t)
      (let* ([k (car key)]
             [child (assoc k (trie-children t))])
        (if child
            (trie-lookup (cdr child) (cdr key))
            nothing))))

;;; Check if key exists in trie
(define (trie-contains? t key)
  (just? (trie-lookup t key)))

;;; Delete a key from trie
;;; Returns new trie without the key
(define (trie-delete t key)
  (if (null? key)
      (make-trie nothing (trie-children t))
      (let* ([k (car key)]
             [children (trie-children t)]
             [child (assoc k children)])
        (if child
            (let* ([new-child (trie-delete (cdr child) (cdr key))]
                   [new-children
                    (if (trie-empty? new-child)
                        (filter (lambda (c) (not (equal? (car c) k))) children)
                        (cons (cons k new-child)
                              (filter (lambda (c) (not (equal? (car c) k))) children)))])
              (make-trie (trie-value t) new-children))
            t))))

;;; ============================================================
;;; Prefix Operations
;;; ============================================================

;;; Get subtrie rooted at prefix
;;; Returns Maybe Trie
(define (trie-subtrie t prefix)
  (if (null? prefix)
      (just t)
      (let ([child (assoc (car prefix) (trie-children t))])
        (if child
            (trie-subtrie (cdr child) (cdr prefix))
            nothing))))

;;; Check if any key starts with prefix
(define (trie-has-prefix? t prefix)
  (just? (trie-subtrie t prefix)))

;;; Get all keys with given prefix
(define (trie-keys-with-prefix t prefix)
  (let ([sub (trie-subtrie t prefix)])
    (if (just? sub)
        (map (lambda (suffix) (append prefix suffix))
             (trie-keys (from-just sub)))
        '())))

;;; Get all values with given prefix
(define (trie-values-with-prefix t prefix)
  (let ([sub (trie-subtrie t prefix)])
    (if (just? sub)
        (trie-values (from-just sub))
        '())))

;;; ============================================================
;;; Key/Value Enumeration
;;; ============================================================

;;; Get all keys in trie
(define (trie-keys t)
  (let ([self-keys (if (just? (trie-value t)) '(()) '())]
        [child-keys (fold-left
                     (lambda (acc child)
                       (append acc
                               (map (lambda (k) (cons (car child) k))
                                    (trie-keys (cdr child)))))
                     '()
                     (trie-children t))])
    (append self-keys child-keys)))

;;; Get all values in trie
(define (trie-values t)
  (let ([self-values (if (just? (trie-value t))
                         (list (from-just (trie-value t)))
                         '())]
        [child-values (fold-left
                       (lambda (acc child)
                         (append acc (trie-values (cdr child))))
                       '()
                       (trie-children t))])
    (append self-values child-values)))

;;; Get all key-value pairs
(define (trie-entries t)
  (let ([self-entries (if (just? (trie-value t))
                          (list (cons '() (from-just (trie-value t))))
                          '())]
        [child-entries (fold-left
                        (lambda (acc child)
                          (append acc
                                  (map (lambda (e)
                                         (cons (cons (car child) (car e))
                                               (cdr e)))
                                       (trie-entries (cdr child)))))
                        '()
                        (trie-children t))])
    (append self-entries child-entries)))

;;; Count number of keys in trie
(define (trie-size t)
  (+ (if (just? (trie-value t)) 1 0)
     (fold-left (lambda (acc child) (+ acc (trie-size (cdr child))))
                0
                (trie-children t))))

;;; ============================================================
;;; String-Specialized Operations
;;; ============================================================

;;; Insert with string key
(define (trie-insert-string t str value)
  (trie-insert t (string->list str) value))

;;; Lookup with string key
(define (trie-lookup-string t str)
  (trie-lookup t (string->list str)))

;;; Check if string key exists
(define (trie-contains-string? t str)
  (trie-contains? t (string->list str)))

;;; Delete with string key
(define (trie-delete-string t str)
  (trie-delete t (string->list str)))

;;; Get all string keys with prefix
(define (trie-complete t prefix-str)
  (map list->string
       (trie-keys-with-prefix t (string->list prefix-str))))

;;; Build trie from list of strings (values are strings themselves)
(define (strings->trie strs)
  (fold-left (lambda (t s) (trie-insert-string t s s))
             empty-trie
             strs))

;;; Build trie from alist of (string . value) pairs
(define (alist->trie pairs)
  (fold-left (lambda (t p) (trie-insert-string t (car p) (cdr p)))
             empty-trie
             pairs))

;;; ============================================================
;;; Trie Transformations
;;; ============================================================

;;; Map a function over all values in trie
(define (trie-map f t)
  (let ([v (trie-value t)])
    (make-trie (if (just? v) (just (f (from-just v))) nothing)
               (map (lambda (child)
                      (cons (car child) (trie-map f (cdr child))))
                    (trie-children t)))))

;;; Filter trie by predicate on values
(define (trie-filter pred t)
  (let* ([new-value (let ([v (trie-value t)])
                      (if (and (just? v) (pred (from-just v)))
                          v
                          nothing))]
         [new-children
          (filter-map (lambda (child)
                        (let ([filtered (trie-filter pred (cdr child))])
                          (if (trie-empty? filtered)
                              #f
                              (cons (car child) filtered))))
                      (trie-children t))])
    (make-trie new-value new-children)))

;;; Helper: filter and map
(define (filter-map f lst)
  (fold-right (lambda (x acc)
                (let ([result (f x)])
                  (if result (cons result acc) acc)))
              '()
              lst))

;;; Fold over all values in trie
(define (trie-fold f init t)
  (let* ([acc (if (just? (trie-value t))
                  (f init (from-just (trie-value t)))
                  init)])
    (fold-left (lambda (a child)
                 (trie-fold f a (cdr child)))
               acc
               (trie-children t))))

;;; Fold over all key-value pairs
(define (trie-fold-entries f init t)
  (trie-fold-entries* f init t '()))

(define (trie-fold-entries* f init t prefix)
  (let* ([acc (if (just? (trie-value t))
                  (f init (reverse prefix) (from-just (trie-value t)))
                  init)])
    (fold-left (lambda (a child)
                 (trie-fold-entries* f a (cdr child) (cons (car child) prefix)))
               acc
               (trie-children t))))

;;; ============================================================
;;; Trie Merging
;;; ============================================================

;;; Merge two tries, using f to combine values for same key
(define (trie-merge-with f t1 t2)
  (let* ([v1 (trie-value t1)]
         [v2 (trie-value t2)]
         [new-value (cond
                      [(and (just? v1) (just? v2))
                       (just (f (from-just v1) (from-just v2)))]
                      [(just? v1) v1]
                      [else v2])]
         ;; Get all child keys from both
         [keys1 (map car (trie-children t1))]
         [keys2 (map car (trie-children t2))]
         [all-keys (union equal? keys1 keys2)]
         ;; Merge children
         [new-children
          (map (lambda (k)
                 (let ([c1 (assoc k (trie-children t1))]
                       [c2 (assoc k (trie-children t2))])
                   (cons k
                         (cond
                           [(and c1 c2) (trie-merge-with f (cdr c1) (cdr c2))]
                           [c1 (cdr c1)]
                           [else (cdr c2)]))))
               all-keys)])
    (make-trie new-value new-children)))

;;; Merge two tries, second overwrites first for conflicts
(define (trie-merge t1 t2)
  (trie-merge-with (lambda (v1 v2) v2) t1 t2))

;;; Helper: set union
(define (union eq? xs ys)
  (fold-left (lambda (acc x)
               (if (exists (lambda (y) (eq? x y)) acc) acc (cons x acc)))
             ys xs))

;;; ============================================================
;;; Longest Prefix Match
;;; ============================================================

;;; Find longest prefix of key that exists in trie
;;; Returns (prefix . value) or #f
(define (trie-longest-prefix t key)
  (trie-longest-prefix* t key '() #f))

(define (trie-longest-prefix* t key prefix last-match)
  (let ([current-match (if (just? (trie-value t))
                           (cons (reverse prefix) (from-just (trie-value t)))
                           last-match)])
    (if (null? key)
        current-match
        (let ([child (assoc (car key) (trie-children t))])
          (if child
              (trie-longest-prefix* (cdr child) (cdr key)
                                    (cons (car key) prefix)
                                    current-match)
              current-match)))))

;;; String version of longest prefix match
(define (trie-longest-prefix-string t str)
  (let ([result (trie-longest-prefix t (string->list str))])
    (if result
        (cons (list->string (car result)) (cdr result))
        #f)))

;;; ============================================================
;;; Common Prefix
;;; ============================================================

;;; Find the longest common prefix of all keys in trie
(define (trie-common-prefix t)
  (if (or (nothing? (trie-value t))
          (> (length (trie-children t)) 1)
          (null? (trie-children t)))
      (if (= (length (trie-children t)) 1)
          (let ([child (car (trie-children t))])
            (cons (car child)
                  (trie-common-prefix (cdr child))))
          '())
      '()))

;;; ============================================================
;;; Trie Visualization
;;; ============================================================

;;; Convert trie to string representation
(define (trie->string t)
  (trie->string* t 0))

(define (trie->string* t depth)
  (let ([indent (make-string (* depth 2) #\space)]
        [value-str (if (just? (trie-value t))
                       (format " = ~a" (from-just (trie-value t)))
                       "")])
    (string-append
     indent "[" value-str "]\n"
     (fold-left (lambda (acc child)
                  (string-append acc
                                 indent "  " (format "~a" (car child)) ":\n"
                                 (trie->string* (cdr child) (+ depth 2))))
                ""
                (trie-children t)))))

;;; ============================================================
;;; Specialized Trie Builders
;;; ============================================================

;;; Build a trie as a set (values are #t)
(define (trie-set keys)
  (fold-left (lambda (t k) (trie-insert t k #t))
             empty-trie
             keys))

;;; Build a trie from strings as a set
(define (trie-string-set strs)
  (fold-left (lambda (t s) (trie-insert-string t s #t))
             empty-trie
             strs))

;;; Build a frequency trie (count occurrences)
(define (trie-frequency keys)
  (fold-left (lambda (t k)
               (let ([current (trie-lookup t k)])
                 (trie-insert t k (+ 1 (if (just? current)
                                           (from-just current)
                                           0)))))
             empty-trie
             keys))

;;; String frequency trie
(define (trie-string-frequency strs)
  (trie-frequency (map string->list strs)))

;;; ============================================================
;;; Autocomplete with Ranking
;;; ============================================================

;;; Get completions sorted by value (assuming numeric values)
(define (trie-complete-ranked t prefix-str limit)
  (let* ([prefix (string->list prefix-str)]
         [sub (trie-subtrie t prefix)])
    (if (just? sub)
        (let* ([entries (trie-entries (from-just sub))]
               [full-entries (map (lambda (e)
                                    (cons (append prefix (car e)) (cdr e)))
                                  entries)]
               [sorted (list-sort (lambda (a b) (> (cdr a) (cdr b)))
                                  full-entries)])
          (map (lambda (e) (list->string (car e)))
               (take limit sorted)))
        '())))

;;; Take first n elements of list
(define (take n lst)
  (if (or (null? lst) (<= n 0))
      '()
      (cons (car lst) (take (- n 1) (cdr lst)))))

;;; ============================================================
;;; Wildcard Matching
;;; ============================================================

;;; Match keys with wildcard (represented as 'wild symbol)
;;; Returns list of matching values
(define (trie-match-wildcard t pattern)
  (trie-match-wildcard* t pattern '()))

(define (trie-match-wildcard* t pattern acc)
  (if (null? pattern)
      (if (just? (trie-value t))
          (cons (from-just (trie-value t)) acc)
          acc)
      (let ([p (car pattern)]
            [rest (cdr pattern)])
        (if (eq? p 'wild)
            ;; Match any single element
            (fold-left (lambda (a child)
                         (trie-match-wildcard* (cdr child) rest a))
                       acc
                       (trie-children t))
            ;; Match specific element
            (let ([child (assoc p (trie-children t))])
              (if child
                  (trie-match-wildcard* (cdr child) rest acc)
                  acc))))))

;;; ============================================================
;;; Exports Summary
;;; ============================================================

;;; Types:
;;;   make-trie, trie?, empty-trie, trie-empty?, trie-value, trie-children
;;;
;;; Basic operations:
;;;   trie-insert, trie-lookup, trie-contains?, trie-delete
;;;
;;; Prefix operations:
;;;   trie-subtrie, trie-has-prefix?, trie-keys-with-prefix,
;;;   trie-values-with-prefix
;;;
;;; Enumeration:
;;;   trie-keys, trie-values, trie-entries, trie-size
;;;
;;; String operations:
;;;   trie-insert-string, trie-lookup-string, trie-contains-string?,
;;;   trie-delete-string, trie-complete, strings->trie, alist->trie
;;;
;;; Transformations:
;;;   trie-map, trie-filter, trie-fold, trie-fold-entries
;;;
;;; Merging:
;;;   trie-merge, trie-merge-with
;;;
;;; Matching:
;;;   trie-longest-prefix, trie-longest-prefix-string,
;;;   trie-common-prefix, trie-match-wildcard
;;;
;;; Specialized builders:
;;;   trie-set, trie-string-set, trie-frequency, trie-string-frequency
;;;
;;; Autocomplete:
;;;   trie-complete-ranked
;;;
;;; Visualization:
;;;   trie->string
