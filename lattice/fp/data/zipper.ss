(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")

(doc 'module 'zipper)
(doc 'description "Functional Zippers - A zipper is a cursor into a data structure, enabling efficient local navigation and modification. This module implements list zippers with O(1) movement and modification at the focus point.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'references)
(doc 'note "Huet, \"The Zipper\" (1997) JFP 7(5):549-554")
(doc 'note "McBride, \"The Derivative of a Regular Type is its Type of One-Hole Contexts\"")

(doc 'section 'zipper-type)
(doc 'note "A list zipper consists of: left (reversed, closest first), focus (Maybe α), right. For list [1, 2, 3, 4, 5] with focus on 3: left = (2 1), focus = (just 3), right = (4 5). Empty zipper has focus = nothing, left = (), right = (). Navigation allows moving past-end (focus=nothing after last element) but not before-start. This asymmetry supports append semantics.")

(define zipper-tag 'list-zipper)

(define (make-zipper left focus right)
  (doc 'type '(-> (List α) (Maybe α) (List α) (ListZipper α)))
  (doc 'description "Internal constructor")
  (list zipper-tag left focus right))

(define (zipper? z)
  (doc 'type '(-> α Bool))
  (doc 'description "Check if value is a list zipper")
  (and (pair? z)
       (eq? (car z) zipper-tag)
       (= (length z) 4)))

(define (zipper-left z)
  (doc 'type '(-> (ListZipper α) (List α)))
  (doc 'description "Get left context (reversed)")
  (if (zipper? z)
      (list-ref z 1)
      (error 'zipper-left "not a zipper")))

(define (zipper-focus-maybe z)
  (doc 'type '(-> (ListZipper α) (Maybe α)))
  (doc 'description "Get focus as Maybe")
  (if (zipper? z)
      (list-ref z 2)
      (error 'zipper-focus-maybe "not a zipper")))

(define (zipper-right z)
  (doc 'type '(-> (ListZipper α) (List α)))
  (doc 'description "Get right context")
  (if (zipper? z)
      (list-ref z 3)
      (error 'zipper-right "not a zipper")))

(doc 'section 'constructors)

(define zipper-empty
  (make-zipper '() nothing '()))

(define (zipper-empty? z)
  (doc 'type '(-> (ListZipper α) Bool))
  (doc 'description "Check if zipper is empty")
  (and (zipper? z)
       (null? (zipper-left z))
       (nothing? (zipper-focus-maybe z))
       (null? (zipper-right z))))

(define (zipper-singleton x)
  (doc 'type '(-> α (ListZipper α)))
  (doc 'description "Create zipper with single element at focus")
  (make-zipper '() (just x) '()))

(define (list->zipper lst)
  (doc 'type '(-> (List α) (ListZipper α)))
  (doc 'description "Convert list to zipper, focusing on first element")
  (if (null? lst)
      zipper-empty
      (make-zipper '() (just (car lst)) (cdr lst))))

(define (zipper->list z)
  (doc 'type '(-> (ListZipper α) (List α)))
  (doc 'description "Convert zipper back to list")
  (let ([left (zipper-left z)]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (append (reverse left)
            (if (nothing? focus)
                '()
                (cons (from-just focus) right)))))

(define (zipper-from-position lst pos)
  (doc 'type '(-> (List α) Nat (ListZipper α)))
  (doc 'description "Create zipper focused at given position (0-indexed). If position exceeds list length, focuses past the end")
  (let loop ([left '()] [rest lst] [n pos])
    (cond
      [(null? rest)
       (make-zipper left nothing '())]
      [(<= n 0)
       (make-zipper left (just (car rest)) (cdr rest))]
      [else
       (loop (cons (car rest) left) (cdr rest) (- n 1))])))

(doc 'section 'navigation)

(define (zipper-has-focus? z)
  (doc 'type '(-> (ListZipper α) Bool))
  (doc 'description "Check if zipper has a focused element")
  (just? (zipper-focus-maybe z)))

(define (zipper-can-go-left? z)
  (doc 'type '(-> (ListZipper α) Bool))
  (doc 'description "Check if leftward movement is possible")
  (not (null? (zipper-left z))))

(define (zipper-can-go-right? z)
  (doc 'type '(-> (ListZipper α) Bool))
  (doc 'description "Check if rightward movement is possible")
  (or (and (zipper-has-focus? z)
           (not (null? (zipper-right z))))
      (and (not (zipper-has-focus? z))
           (not (null? (zipper-right z))))))

(define (zipper-left! z)
  (doc 'type '(-> (ListZipper α) (Maybe (ListZipper α))))
  (doc 'description "Move focus left. Returns nothing if at start")
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

(define (zipper-right! z)
  (doc 'type '(-> (ListZipper α) (Maybe (ListZipper α))))
  (doc 'description "Move focus right. Returns nothing if at end")
  (let ([left (zipper-left z)]
        [focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (cond
      [(and (just? focus) (not (null? right)))
       (just
        (make-zipper
         (cons (from-just focus) left)
         (just (car right))
         (cdr right)))]
      [(and (just? focus) (null? right))
       (just
        (make-zipper
         (cons (from-just focus) left)
         nothing
         '()))]
      [(and (nothing? focus) (not (null? right)))
       (just
        (make-zipper
         left
         (just (car right))
         (cdr right)))]
      [else nothing])))

(define (zipper-start z)
  (doc 'type '(-> (ListZipper α) (ListZipper α)))
  (doc 'description "Move to the start of the list")
  (let loop ([current z])
    (let ([moved (zipper-left! current)])
      (if (nothing? moved)
          current
          (loop (from-just moved))))))

(define (zipper-end z)
  (doc 'type '(-> (ListZipper α) (ListZipper α)))
  (doc 'description "Move to the end of the list (last element focused)")
  (let loop ([current z])
    (let ([moved (zipper-right! current)])
      (if (nothing? moved)
          current
          (let ([next (from-just moved)])
            (if (nothing? (zipper-focus-maybe next))
                current
                (loop next)))))))

(define (zipper-goto z pos)
  (doc 'type '(-> (ListZipper α) Nat (ListZipper α)))
  (doc 'description "Move to specific position (0-indexed). Clamps to valid range")
  (zipper-from-position (zipper->list z) pos))

(define (zipper-position z)
  (doc 'type '(-> (ListZipper α) Nat))
  (doc 'description "Get current position (0-indexed)")
  (length (zipper-left z)))

(define (zipper-length z)
  (doc 'type '(-> (ListZipper α) Nat))
  (doc 'description "Get total length of the underlying list")
  (+ (length (zipper-left z))
     (if (zipper-has-focus? z) 1 0)
     (length (zipper-right z))))

(doc 'section 'focus-operations)

(define (zipper-focus z)
  (doc 'type '(-> (ListZipper α) α))
  (doc 'description "Get the focused element. Error if no focus")
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        (error 'zipper-focus "no focused element")
        (from-just focus))))

(define (zipper-focus-or z default)
  (doc 'type '(-> (ListZipper α) α α))
  (doc 'description "Get focused element or default if none")
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        default
        (from-just focus))))

(define (zipper-set z x)
  (doc 'type '(-> (ListZipper α) α (ListZipper α)))
  (doc 'description "Set the focused element. Error if no focus")
  (if (not (zipper-has-focus? z))
      (error 'zipper-set "no focused element to set")
      (make-zipper (zipper-left z)
                   (just x)
                   (zipper-right z))))

(define (zipper-modify z f)
  (doc 'type '(-> (ListZipper α) (-> α α) (ListZipper α)))
  (doc 'description "Modify the focused element. Error if no focus")
  (if (not (zipper-has-focus? z))
      (error 'zipper-modify "no focused element to modify")
      (make-zipper (zipper-left z)
                   (just (f (from-just (zipper-focus-maybe z))))
                   (zipper-right z))))

(doc 'section 'insertion)

(define (zipper-insert-left z x)
  (doc 'type '(-> (ListZipper α) α (ListZipper α)))
  (doc 'description "Insert element to the left of focus. If no focus, inserts at current position")
  (make-zipper (cons x (zipper-left z))
               (zipper-focus-maybe z)
               (zipper-right z)))

(define (zipper-insert-right z x)
  (doc 'type '(-> (ListZipper α) α (ListZipper α)))
  (doc 'description "Insert element to the right of focus. If past-end (no focus), element becomes new focus for convenient append semantics")
  (let ([focus (zipper-focus-maybe z)])
    (if (nothing? focus)
        (make-zipper (zipper-left z)
                     (just x)
                     (zipper-right z))
        (make-zipper (zipper-left z)
                     focus
                     (cons x (zipper-right z))))))

(define (zipper-insert-focus z x)
  (doc 'type '(-> (ListZipper α) α (ListZipper α)))
  (doc 'description "Insert element as the new focus, pushing old focus right")
  (let ([focus (zipper-focus-maybe z)]
        [right (zipper-right z)])
    (make-zipper (zipper-left z)
                 (just x)
                 (if (nothing? focus)
                     right
                     (cons (from-just focus) right)))))

(doc 'section 'deletion)

(define (zipper-delete z)
  (doc 'type '(-> (ListZipper α) (ListZipper α)))
  (doc 'description "Delete focused element. Focus moves right if possible, else left")
  (if (not (zipper-has-focus? z))
      z
      (let ([left (zipper-left z)]
            [right (zipper-right z)])
        (cond
          [(not (null? right))
           (make-zipper left
                        (just (car right))
                        (cdr right))]
          [(not (null? left))
           (make-zipper (cdr left)
                        (just (car left))
                        '())]
          [else zipper-empty]))))

(define (zipper-delete-left z)
  (doc 'type '(-> (ListZipper α) (ListZipper α)))
  (doc 'description "Delete element immediately to the left of focus. No-op if nothing to the left")
  (let ([left (zipper-left z)])
    (if (null? left)
        z
        (make-zipper (cdr left)
                     (zipper-focus-maybe z)
                     (zipper-right z)))))

(define (zipper-delete-right z)
  (doc 'type '(-> (ListZipper α) (ListZipper α)))
  (doc 'description "Delete element immediately to the right of focus. No-op if nothing to the right")
  (let ([right (zipper-right z)])
    (if (null? right)
        z
        (make-zipper (zipper-left z)
                     (zipper-focus-maybe z)
                     (cdr right)))))

(doc 'section 'bulk-operations)

(define (zipper-map f z)
  (doc 'type '(-> (-> α β) (ListZipper α) (ListZipper β)))
  (doc 'description "Map function over all elements in zipper")
  (make-zipper (map f (zipper-left z))
               (maybe-fmap f (zipper-focus-maybe z))
               (map f (zipper-right z))))

(define (zipper-filter pred z)
  (doc 'type '(-> (-> α Bool) (ListZipper α) (ListZipper α)))
  (doc 'description "Filter elements. Focus removed if it doesn't match predicate")
  (list->zipper (filter pred (zipper->list z))))

(define (zipper-fold-left f init z)
  (doc 'type '(-> (-> β α β) β (ListZipper α) β))
  (doc 'description "Left fold over zipper elements in order")
  (fold-left f init (zipper->list z)))

(define (zipper-fold-right f init z)
  (doc 'type '(-> (-> α β β) β (ListZipper α) β))
  (doc 'description "Right fold over zipper elements")
  (fold-right f init (zipper->list z)))

(define (zipper-find pred z)
  (doc 'type '(-> (-> α Bool) (ListZipper α) (Maybe (ListZipper α))))
  (doc 'description "Find first element matching predicate, returning zipper focused there")
  (let loop ([current (zipper-start z)])
    (cond
      [(not (zipper-has-focus? current)) nothing]
      [(pred (zipper-focus current)) (just current)]
      [else
       (let ([next (zipper-right! current)])
         (if (nothing? next)
             nothing
             (loop (from-just next))))])))

(define (zipper-find-index pred z)
  (doc 'type '(-> (-> α Bool) (ListZipper α) (Maybe Nat)))
  (doc 'description "Find index of first element matching predicate")
  (let loop ([lst (zipper->list z)] [idx 0])
    (cond
      [(null? lst) nothing]
      [(pred (car lst)) (just idx)]
      [else (loop (cdr lst) (+ idx 1))])))

(doc 'section 'context-operations)

(define (zipper-context z)
  (doc 'type '(-> (ListZipper α) (List α)))
  (doc 'description "Get all elements except focus (in order)")
  (append (reverse (zipper-left z))
          (zipper-right z)))

(define (zipper-take-left z n)
  (doc 'type '(-> (ListZipper α) Nat (List α)))
  (doc 'description "Take n elements to the left (in left-to-right order)")
  (reverse (take (min n (length (zipper-left z))) (zipper-left z))))

(define (zipper-take-right z n)
  (doc 'type '(-> (ListZipper α) Nat (List α)))
  (doc 'description "Take n elements to the right")
  (take (min n (length (zipper-right z))) (zipper-right z)))

(define (zipper-window z left-count right-count)
  (doc 'type '(-> (ListZipper α) Nat Nat (List α)))
  (doc 'description "Get elements in window around focus (left-count, right-count). Includes focus in the middle")
  (append (zipper-take-left z left-count)
          (if (zipper-has-focus? z)
              (cons (zipper-focus z) (zipper-take-right z right-count))
              (zipper-take-right z right-count))))

(doc 'section 'splitting-and-joining)

(define (zipper-split z)
  (doc 'type '(-> (ListZipper α) (Pair (List α) (List α))))
  (doc 'description "Split at focus into (left-elements, focus+right-elements)")
  (cons (reverse (zipper-left z))
        (if (zipper-has-focus? z)
            (cons (zipper-focus z) (zipper-right z))
            (zipper-right z))))

(define (zipper-append z lst)
  (doc 'type '(-> (ListZipper α) (List α) (ListZipper α)))
  (doc 'description "Append list to the right of zipper")
  (make-zipper (zipper-left z)
               (zipper-focus-maybe z)
               (append (zipper-right z) lst)))

(define (zipper-prepend lst z)
  (doc 'type '(-> (List α) (ListZipper α) (ListZipper α)))
  (doc 'description "Prepend list to the beginning of the underlying list")
  (make-zipper (append (zipper-left z) (reverse lst))
               (zipper-focus-maybe z)
               (zipper-right z)))

(doc 'section 'type-class-instances)

(define zipper-functor
  (list 'functor zipper-map))

(define zipper-fmap zipper-map)

(doc 'section 'comonad-instance)
(doc 'note "ListZipper forms a Comonad with extract (get focused element) and extend (apply contextual function to all positions). Comonad laws: 1. extend extract = id, 2. extract . extend f = f, 3. extend f . extend g = extend (f . extend g)")

(define zipper-extract zipper-focus)

(define (zipper-extend f z)
  (doc 'type '(-> (-> (ListZipper α) β) (ListZipper α) (ListZipper β)))
  (doc 'description "Extend a contextual function to all positions. For each position, applies f to the zipper focused at that position. Optimized O(N) implementation")
  (if (zipper-empty? z)
      zipper-empty
      (let* ([current-pos (zipper-position z)]
             [start (zipper-start z)]
             [results (zipper-extend-collect f start)])
        (zipper-from-position results current-pos))))

(define (zipper-extend-collect f z)
  (doc 'type '(-> (-> (ListZipper α) β) (ListZipper α) (List β)))
  (doc 'description "Helper: traverse zipper collecting f(z) at each focused position")
  (let loop ([current z] [acc '()])
    (if (not (zipper-has-focus? current))
        (reverse acc)
        (let* ([result (f current)]
               [new-acc (cons result acc)]
               [next (zipper-right! current)])
          (if (nothing? next)
              (reverse new-acc)
              (loop (from-just next) new-acc))))))

(define (zipper-duplicate z)
  (doc 'type '(-> (ListZipper α) (ListZipper (ListZipper α))))
  (doc 'description "Duplicate: zipper of all possible focuses")
  (zipper-extend (lambda (x) x) z))

(define zipper-comonad
  (list 'comonad
        zipper-functor
        zipper-extract
        zipper-extend))

(doc 'section 'equality-and-display)

(define (zipper-equal? z1 z2)
  (doc 'type '(-> (ListZipper α) (ListZipper α) Bool))
  (doc 'description "Check equality of two zippers")
  (and (equal? (zipper-left z1) (zipper-left z2))
       (equal? (zipper-focus-maybe z1) (zipper-focus-maybe z2))
       (equal? (zipper-right z1) (zipper-right z2))))

(define (zipper->string z)
  (doc 'type '(-> (ListZipper α) String))
  (doc 'description "Convert zipper to string representation. Shows: [left...] >focus< [right...]")
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

(doc 'section 'helpers)

;; iota, last provided by prelude

(define (drop-right lst n)
  (doc 'type '(-> (List α) Nat (List α)))
  (doc 'description "Drop n elements from right of list")
  (let ([len (length lst)])
    (if (<= len n)
        '()
        (take (- len n) lst))))

;; take provided by prelude
