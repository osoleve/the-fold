;;; pairs-and-alists.ss --- Pairs and Association Lists
;;;
;;; Pairs are the building block of lists; alists provide key-value storage.
;;;
;;; Run with: scheme --script docs/tutorials/examples/data-structures/pairs-and-alists.ss

;;; ====
;;; Pairs (cons cells)
;;; ====

(display "=== Pairs ===\n")

;;; cons creates a pair
(define p1 (cons 1 2))
(format #t "pair (1 . 2): ~a~%" p1)

;;; car and cdr access the parts
(format #t "car: ~a, cdr: ~a~%" (car p1) (cdr p1))

;;; Pairs can hold any values
(define p2 (cons 'name "Alice"))
(format #t "pair (name . \"Alice\"): ~a~%" p2)

;;; Nested pairs
(define p3 (cons (cons 1 2) (cons 3 4)))
(format #t "nested: ~a~%" p3)
(format #t "caar: ~a, cdar: ~a~%" (caar p3) (cdar p3))

;;; ====
;;; Lists ARE Pairs
;;; ====

(display "\n=== Lists as Pairs ===\n")

;;; A list is a chain of pairs ending in '()
;;;   (1 2 3) = (1 . (2 . (3 . ())))

(define lst '(1 2 3))
(format #t "list: ~a~%" lst)
(format #t "car: ~a~%" (car lst))
(format #t "cdr: ~a~%" (cdr lst))
(format #t "cddr: ~a~%" (cddr lst))
(format #t "cdddr: ~a~%" (cdddr lst))  ; the empty list

;;; pair? vs list?
(format #t "\npair? ~a = ~a~%" lst (pair? lst))
(format #t "list? ~a = ~a~%" lst (list? lst))
(format #t "pair? (1 . 2) = ~a~%" (pair? p1))
(format #t "list? (1 . 2) = ~a~%" (list? p1))  ; #f - improper list

;;; ====
;;; Association Lists (alists)
;;; ====

(display "\n=== Association Lists ===\n")

;;; An alist is a list of pairs: ((key1 . val1) (key2 . val2) ...)
(define person
  '((name . "Bob")
    (age . 30)
    (city . "Seattle")))

(format #t "alist: ~a~%" person)

;;; assoc finds by key
(format #t "assoc 'age: ~a~%" (assoc 'age person))
(format #t "assoc 'name: ~a~%" (assoc 'name person))
(format #t "assoc 'missing: ~a~%" (assoc 'missing person))

;;; Get just the value
(define (alist-ref key alist)
  (let ([pair (assoc key alist)])
    (and pair (cdr pair))))

(format #t "age value: ~a~%" (alist-ref 'age person))

;;; ====
;;; Building Alists
;;; ====

(display "\n=== Building Alists ===\n")

;;; Add or update a key
(define (alist-set key val alist)
  (cons (cons key val)
        (filter (lambda (p) (not (eq? (car p) key))) alist)))

(define updated (alist-set 'age 31 person))
(format #t "after birthday: ~a~%" updated)

(define extended (alist-set 'email "bob@example.com" person))
(format #t "with email: ~a~%" extended)

;;; Remove a key
(define (alist-remove key alist)
  (filter (lambda (p) (not (eq? (car p) key))) alist))

(format #t "without city: ~a~%" (alist-remove 'city person))

;;; ====
;;; assq and assv variants
;;; ====

(display "\n=== assq vs assoc ===\n")

;;; assq uses eq? (pointer equality) - faster for symbols
;;; assv uses eqv? (works for numbers)
;;; assoc uses equal? (works for strings, lists)

(define sym-alist '((a . 1) (b . 2) (c . 3)))
(format #t "assq 'b: ~a~%" (assq 'b sym-alist))

(define num-alist '((1 . "one") (2 . "two") (3 . "three")))
(format #t "assv 2: ~a~%" (assv 2 num-alist))

(define str-alist '(("hello" . 1) ("world" . 2)))
(format #t "assoc \"world\": ~a~%" (assoc "world" str-alist))

;;; ====
;;; Practical Example: Simple Config
;;; ====

(display "\n=== Example: Configuration ===\n")

(define config
  '((database . ((host . "localhost")
                 (port . 5432)
                 (name . "mydb")))
    (server . ((port . 8080)
               (debug . #t)))
    (features . (logging caching))))

(format #t "Full config: ~a~%~%" config)

;;; Nested access
(define (config-get path cfg)
  (fold-left (lambda (acc key)
               (and acc (alist-ref key acc)))
             cfg
             path))

(format #t "database host: ~a~%" (config-get '(database host) config))
(format #t "server port: ~a~%" (config-get '(server port) config))
(format #t "features: ~a~%" (alist-ref 'features config))

(display "\nDone!\n")
