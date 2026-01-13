((name "data")
 (description "Lazy and navigable data structures for functional programming.
  Provides infinite streams with demand-driven evaluation and
  zippers for efficient cursor-based navigation and modification.")

 (modules
  ((file "stream.ss")
   (purpose "Lazy streams (potentially infinite lists)")
   (exports
    (stream-nil "The empty stream")
    (stream-nil? "Test for empty stream")
    (stream-cons "Construct a stream cell (head eager, tail lazy)")
    (stream-car "Extract the head of a stream")
    (stream-cdr "Force and return the tail of a stream")
    (stream-take "Take first n elements as a list")
    (stream-drop "Drop first n elements, return remaining stream")
    (stream-ref "Access the nth element (0-indexed)")
    (stream-map "Map a function over a stream")
    (stream-filter "Filter stream by predicate")
    (stream-append "Append two streams (first must be finite)")
    (stream-interleave "Interleave two streams alternately")
    (stream-flatmap "Map and flatten (monadic bind)")
    (stream-iterate "Generate stream by repeated function application")
    (stream-unfold "Generate stream from seed and step function")
    (stream-zip "Zip two streams into stream of pairs")
    (stream-zip-with "Zip with a combining function")
    (list->stream "Convert a list to a stream")
    (stream->list "Convert entire stream to list (must be finite)")
    (stream-from "Infinite stream of integers from n")
    (stream-repeat "Infinite stream of a constant value"))
   (example
    ";; Infinite stream of natural numbers
     (define nats (stream-from 0))

     ;; First 5 even numbers
     (stream-take 5 (stream-filter even? nats))
     ;; => (0 2 4 6 8)

     ;; Fibonacci sequence via co-recursion
     (define fibs
       (stream-cons 0
         (stream-cons 1
           (stream-zip-with + fibs (stream-cdr fibs)))))
     (stream-take 10 fibs)
     ;; => (0 1 1 2 3 5 8 13 21 34)"))

  ((file "zipper.ss")
   (purpose "List zipper for cursor-based navigation and modification")
   (exports
    (zipper-empty "The empty zipper")
    (zipper-singleton "Create zipper with single focused element")
    (list->zipper "Convert list to zipper, focus on first element")
    (zipper->list "Convert zipper back to list")
    (zipper-from-position "Create zipper focused at given index")
    (zipper-left! "Move focus left (returns Maybe)")
    (zipper-right! "Move focus right (returns Maybe)")
    (zipper-start "Move to first element")
    (zipper-end "Move to last element")
    (zipper-focus "Get focused element")
    (zipper-set "Replace focused element")
    (zipper-modify "Apply function to focused element")
    (zipper-insert-left "Insert element to left of focus")
    (zipper-insert-right "Insert element to right of focus")
    (zipper-delete "Remove focused element, shift focus")
    (zipper-map "Map function over all elements")
    (zipper-extend "Comonad extend: apply contextual function at each position")
    (zipper-extract "Comonad extract: get focused element"))
   (example
    ";; Navigate and modify a list
     (define z (list->zipper '(1 2 3 4 5)))
     (zipper-focus z)  ;; => 1

     ;; Move right twice
     (define z2 (from-just (zipper-right! (from-just (zipper-right! z)))))
     (zipper-focus z2)  ;; => 3

     ;; Modify the focus
     (zipper->list (zipper-set z2 100))  ;; => (1 2 100 4 5)

     ;; Comonad: compute local sum at each position
     (define (local-sum zz)
       (+ (zipper-focus-or (from-just (zipper-left! zz)) 0)
          (zipper-focus zz)
          (zipper-focus-or (from-just (zipper-right! zz)) 0)))
     (zipper->list (zipper-extend local-sum z))
     ;; => (3 6 9 12 9)  ; sum of neighbors at each position")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities")
  ("lattice/fp/meta/combinators.ss" "Maybe type for zipper focus"))

 (notes
  "Streams use thunks to delay evaluation of the tail, enabling
   representation of infinite sequences. Only elements that are
   actually demanded are ever computed.

   Zippers provide O(1) navigation and modification at a cursor position.
   The classic representation uses (left, focus, right) where left is
   reversed for efficient movement. ListZipper forms a Comonad, enabling
   contextual computations like moving averages or cellular automata.

   Key patterns:
   - stream-iterate for sequences defined by x, f(x), f(f(x)), ...
   - stream-unfold for stateful generation
   - stream-interleave for fair enumeration of two infinite streams
   - zipper-extend for applying a local computation at every position
   - Co-recursive definitions where a stream references itself"))
