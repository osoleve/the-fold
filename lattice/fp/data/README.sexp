((name "data")
 (description "Lazy data structures for functional programming.
  Provides infinite streams with demand-driven evaluation,
  enabling elegant expression of infinite sequences and
  co-recursive definitions.")

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
     ;; => (0 1 1 2 3 5 8 13 21 34)")))

 (dependencies
  ("core/base/prelude.ss" "Base utilities"))

 (notes
  "Streams use thunks to delay evaluation of the tail, enabling
   representation of infinite sequences. Only elements that are
   actually demanded are ever computed.

   Key patterns:
   - stream-iterate for sequences defined by x, f(x), f(f(x)), ...
   - stream-unfold for stateful generation
   - stream-interleave for fair enumeration of two infinite streams
   - Co-recursive definitions where a stream references itself"))
