;;; @module zipper-lens
;;; @requires prelude combinators templates zipper tree-zipper

(require 'prelude)
(require 'combinators)
(require 'templates)
(require 'zipper)
(require 'tree-zipper)

(doc 'module 'zipper-lens)
(doc 'description "Zipper-Lens Integration - Connects zippers with the lens library for unified navigation and modification patterns. Provides lenses for zipper components and lens-like traversals for navigation.")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'integration-points)
(doc 'note "1. Lenses INTO zippers (focus on zipper components)")
(doc 'note "2. Zippers AS lenses (navigate and modify nested data)")
(doc 'note "3. Comonad-lens connection (extend with lens operations)")

(doc 'section 'references)
(doc 'note "Lenses, Folds, and Traversals (Edward Kmett)")
(doc 'note "The Essence of the Iterator Pattern (Gibbons & Oliveira)")

(doc 'section 'lenses-into-list-zippers)

(define zipper-focus-lens
  (make-lens
   zipper-focus
   (lambda (a z) (zipper-set z a))))

(define zipper-focus-maybe-lens
  (make-lens
   zipper-focus-maybe
   (lambda (maybe-a z)
     (make-zipper (zipper-left z) maybe-a (zipper-right z)))))

(define zipper-left-lens
  (make-lens
   zipper-left
   (lambda (left z)
     (make-zipper left (zipper-focus-maybe z) (zipper-right z)))))

(define zipper-right-lens
  (make-lens
   zipper-right
   (lambda (right z)
     (make-zipper (zipper-left z) (zipper-focus-maybe z) right))))

(define zipper-position-lens
  (make-lens
   zipper-position
   (lambda (pos z) (zipper-goto z pos))))

(doc 'section 'lenses-into-tree-zippers)

(define tree-zipper-focus-lens
  (make-lens
   tree-zipper-focus
   (lambda (new-tree z) (tree-zipper-set-tree z new-tree))))

(define tree-zipper-value-lens
  (make-lens
   tree-zipper-get
   (lambda (v z) (tree-zipper-set z v))))

(define tree-zipper-children-lens
  (make-lens
   (lambda (z) (tree-children (tree-zipper-focus z)))
   (lambda (children z)
     (let ([focus (tree-zipper-focus z)])
       (tree-zipper-set-tree z
         (make-tree (tree-value focus) children))))))

(define tree-zipper-crumbs-lens
  (make-lens
   tree-zipper-crumbs
   (lambda (crumbs z)
     (make-tree-zipper (tree-zipper-focus z) crumbs))))

(define tree-zipper-depth-lens
  (make-lens
   tree-zipper-depth
   (lambda (_depth z) z)))

(doc 'section 'navigation-as-affine-traversals)
(doc 'note "Navigation operations can be viewed as affine traversals (they may fail to find a target). We model these as functions returning Maybe.")

(define (make-affine getter setter)
  (doc 'type '(-> (-> s (Maybe a)) (-> a s s) Affine))
  (doc 'description "An affine is like a lens that may not have a focus")
  (list 'affine getter setter))

(define (affine? x)
  (doc 'type '(-> α Bool))
  (and (pair? x) (eq? (car x) 'affine)))

(define (affine-getter a) (cadr a))
(define (affine-setter a) (caddr a))

(define (preview-affine affine s)
  (doc 'type '(-> Affine s (Maybe a)))
  (doc 'description "Try to get the focus")
  ((affine-getter affine) s))

(define (set-affine affine a s)
  (doc 'type '(-> Affine a s s))
  (doc 'description "Set if focus exists, otherwise return unchanged")
  (let ([result ((affine-getter affine) s)])
    (if (nothing? result)
        s
        ((affine-setter affine) a s))))

(define (over-affine affine f s)
  (doc 'type '(-> Affine (-> a a) s s))
  (doc 'description "Modify if focus exists")
  (let ([result ((affine-getter affine) s)])
    (if (nothing? result)
        s
        ((affine-setter affine) (f (from-just result)) s))))

(doc 'section 'list-zipper-navigation-affines)

(define zipper-left-affine
  (make-affine
   zipper-left!
   (lambda (new-z _old-z) new-z)))

(define zipper-right-affine
  (make-affine
   zipper-right!
   (lambda (new-z _old-z) new-z)))

(define (zipper-nth-affine n)
  (doc 'type '(-> Nat (Affine (ListZipper α) α)))
  (doc 'description "Focus on the nth element relative to current position")
  (make-affine
   (lambda (z)
     (let* ([current-pos (zipper-position z)]
            [target-pos (+ current-pos n)]
            [len (zipper-length z)])
       (if (and (>= target-pos 0) (< target-pos len))
           (let ([z2 (zipper-goto z target-pos)])
             (if (zipper-has-focus? z2)
                 (just (zipper-focus z2))
                 nothing))
           nothing)))
   (lambda (a z)
     (let* ([current-pos (zipper-position z)]
            [target-pos (+ current-pos n)])
       (zipper-set (zipper-goto z target-pos) a)))))

(doc 'section 'tree-zipper-navigation-affines)

(define tree-up-affine
  (make-affine
   tree-zipper-up
   (lambda (new-z _old-z) new-z)))

(define tree-down-affine
  (make-affine
   tree-zipper-down
   (lambda (new-z _old-z) new-z)))

(define tree-left-affine
  (make-affine
   tree-zipper-left
   (lambda (new-z _old-z) new-z)))

(define tree-right-affine
  (make-affine
   tree-zipper-right
   (lambda (new-z _old-z) new-z)))

(define (tree-nth-child-affine n)
  (doc 'type '(-> Nat (Affine (TreeZipper α) (TreeZipper α))))
  (make-affine
   (lambda (z) (tree-zipper-nth-child z n))
   (lambda (new-z _old-z) new-z)))

(define (tree-child-value-affine n)
  (doc 'type '(-> Nat (Affine (TreeZipper α) α)))
  (doc 'description "Focus on the value of the nth child without navigating there")
  (make-affine
   (lambda (z)
     (let ([child-z (tree-zipper-nth-child z n)])
       (if (nothing? child-z)
           nothing
           (just (tree-zipper-get (from-just child-z))))))
   (lambda (a z)
     (let ([child-z (tree-zipper-nth-child z n)])
       (if (nothing? child-z)
           z
           (let* ([cz (from-just child-z)]
                  [modified (tree-zipper-set cz a)])
             (from-just (tree-zipper-up modified))))))))

(doc 'section 'zipper-to-lens-adapter)

(define (follow-path affines z)
  (doc 'type '(-> (List (Affine s s)) s (Maybe s)))
  (doc 'description "Follow a path of navigation affines to create a lens. Returns nothing if any step fails")
  (let loop ([remaining affines] [current z])
    (if (null? remaining)
        (just current)
        (let ([result (preview-affine (car remaining) current)])
          (if (nothing? result)
              nothing
              (loop (cdr remaining) (from-just result)))))))

(define (zipper-to-lens z)
  (doc 'type '(-> (ListZipper α) (Lens (List α) α)))
  (doc 'description "Convert a list zipper to a lens that focuses on the same position. The zipper encodes where in the list; the lens accesses that position")
  (let ([pos (zipper-position z)])
    (lens-nth pos)))

(define (tree-path-to-lens indices)
  (doc 'type '(-> (List Nat) (Lens (Tree α) α)))
  (doc 'description "Convert a list of child indices to a lens for tree access")
  (if (null? indices)
      (make-lens tree-value
                 (lambda (v t) (make-tree v (tree-children t))))
      (let* ([child-idx (car indices)]
             [rest-lens (tree-path-to-lens (cdr indices))]
             [child-lens
              (make-lens
               (lambda (t)
                 (let ([children (tree-children t)])
                   (if (< child-idx (length children))
                       (list-ref children child-idx)
                       (error 'tree-path-to-lens "child index out of bounds"))))
               (lambda (new-child t)
                 (let ([children (tree-children t)])
                   (make-tree (tree-value t)
                              (list-set children child-idx new-child)))))])
        (lens-compose child-lens rest-lens))))

(define (list-set lst idx val)
  (doc 'type '(-> (List α) Nat α (List α)))
  (doc 'description "Helper: set element at index")
  (let loop ([i 0] [xs lst] [acc '()])
    (if (null? xs)
        (reverse acc)
        (if (= i idx)
            (loop (+ i 1) (cdr xs) (cons val acc))
            (loop (+ i 1) (cdr xs) (cons (car xs) acc))))))

(doc 'section 'comonad-lens-connection)

(define (extend-with-lens lens f z)
  (doc 'type '(-> Lens (-> (ListZipper α) β) (ListZipper α) (ListZipper β)))
  (doc 'description "Apply a contextual function that uses a lens, to all positions")
  (zipper-extend
   (lambda (z2)
     (if (zipper-has-focus? z2)
         (f (view lens z2) z2)
         (f #f z2)))
   z))

(define (map-with-context lens f z)
  (doc 'type '(-> Lens (-> α Context β) (ListZipper α) (ListZipper β)))
  (doc 'description "Map over elements with access to their local context via a lens")
  (zipper-extend
   (lambda (z2)
     (if (zipper-has-focus? z2)
         (f (view lens z2)
            (list (zipper-take-left z2 1)
                  (zipper-take-right z2 1)))
         #f))
   z))

(define neighbor-lens
  (make-lens
   (lambda (z)
     (append (zipper-take-left z 1)
             (zipper-take-right z 1)))
   (lambda (neighbors z)
     z)))

(define (window-lens left-size right-size)
  (doc 'type '(-> Nat Nat (Lens (ListZipper α) (List α))))
  (doc 'description "Lens focusing on a window around the focus")
  (make-lens
   (lambda (z) (zipper-window z left-size right-size))
   (lambda (window z)
     z)))

(doc 'section 'traversal-utilities)

(define (zipper-each f z)
  (doc 'type '(-> (-> α β) (ListZipper α) (ListZipper β)))
  (doc 'description "Apply function to each element (like map, but preserves position)")
  (zipper-map f z))

(define (zipper-all? pred z)
  (doc 'type '(-> (-> α Bool) (ListZipper α) Bool))
  (doc 'description "Check if predicate holds for all elements")
  (let ([as-list (zipper->list z)])
    (andmap pred as-list)))

(define (zipper-any? pred z)
  (doc 'type '(-> (-> α Bool) (ListZipper α) Bool))
  (doc 'description "Check if predicate holds for any element")
  (let ([as-list (zipper->list z)])
    (ormap pred as-list)))

;; ormap, andmap provided by prelude

(define (zipper-collect f z)
  (doc 'type '(-> (-> α (Maybe β)) (ListZipper α) (List β)))
  (doc 'description "Collect results from a partial function applied to each element")
  (let loop ([current (zipper-start z)] [acc '()])
    (if (not (zipper-has-focus? current))
        (reverse acc)
        (let* ([result (f (zipper-focus current))]
               [new-acc (if (just? result)
                           (cons (from-just result) acc)
                           acc)]
               [next (zipper-right! current)])
          (if (nothing? next)
              (reverse new-acc)
              (loop (from-just next) new-acc))))))

(doc 'section 'composed-lens-paths)

(define at-focus zipper-focus-lens)

(define (at-left n)
  (doc 'type '(-> Nat (Lens (ListZipper α) α)))
  (doc 'description "Lens for the nth element to the left of focus (1 = immediate left). n must be >= 1")
  (if (< n 1)
      (error 'at-left "n must be >= 1 (use at-focus for n=0)")
      (make-lens
       (lambda (z)
         (let ([left (zipper-left z)])
           (if (> n (length left))
               (error 'at-left "index out of bounds")
               (list-ref left (- n 1)))))
       (lambda (a z)
         (let ([left (zipper-left z)])
           (if (> n (length left))
               (error 'at-left "index out of bounds")
               (make-zipper (list-set left (- n 1) a)
                            (zipper-focus-maybe z)
                            (zipper-right z))))))))

(define (at-right n)
  (doc 'type '(-> Nat (Lens (ListZipper α) α)))
  (doc 'description "Lens for the nth element to the right of focus (1 = immediate right). n must be >= 1")
  (if (< n 1)
      (error 'at-right "n must be >= 1 (use at-focus for n=0)")
      (make-lens
       (lambda (z)
         (let ([right (zipper-right z)])
           (if (> n (length right))
               (error 'at-right "index out of bounds")
               (list-ref right (- n 1)))))
       (lambda (a z)
         (let ([right (zipper-right z)])
           (if (> n (length right))
               (error 'at-right "index out of bounds")
               (make-zipper (zipper-left z)
                            (zipper-focus-maybe z)
                            (list-set right (- n 1) a))))))))
