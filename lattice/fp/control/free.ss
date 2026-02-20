;;; lattice/fp/control/free.ss — Free Monad
;;; @module free
;;; @requires prelude combinators

(require 'prelude)
(require 'combinators)

(doc 'module 'free)
(doc 'purity 'total)
(doc 'description "Free Monad Representation Free f a is either: ('pure a)                    — A pure value ('free functor-value)        — A suspended computation Where functor-value is (f (Free f a))")
(doc 'layer 'lattice)
(define (pure-free x)
  (list 'pure x))

;;; free : f (Free f a) -> Free f a
;;; Wrap a functor value.
(define (free fx)
  (list 'free fx))

;;; pure-free? : Free f a -> Boolean
(define (pure-free? fr)
  (and (pair? fr) (eq? (car fr) 'pure)))

;;; free-suspended? : Free f a -> Boolean
(define (free-suspended? fr)
  (and (pair? fr) (eq? (car fr) 'free)))

;;; from-pure-free : Free f a -> a
;;; Extract value from Pure (partial).
(define (from-pure-free fr)
  (cadr fr))

;;; from-free : Free f a -> f (Free f a)
;;; Extract functor value from Free (partial).
(define (from-free fr)
  (cadr fr))

;;; ====
;;; Functor Concept
;;; ====
;;;
;;; A functor has fmap : (a -> b) -> f a -> f b
;;; We pass fmap explicitly since we don't have typeclasses.

;;; ====
;;; Monad Operations
;;; ====

;;; free-map : (a -> b) -> (f a -> f b) -> Free f a -> Free f b
;;; Map over the Free monad. Requires the underlying functor's fmap.
(define (free-map f fmap fr)
  (cond
   [(pure-free? fr)
    (pure-free (f (from-pure-free fr)))]
   [(free-queue? fr)
    ;; For queued free, map over the final result
    (make-free-queue (free-queue-base fr)
                     (free-queue-fmap fr)
                     (append (free-queue-conts fr)
                             (list (lambda (x) (pure-free (f x))))))]
   [else
    (free (fmap (lambda (inner) (free-map f fmap inner))
                (from-free fr)))]))

;;; ====
;;; Queue-based Free Bind (avoids O(n²) worst case)
;;; ====
;;;
;;; PERFORMANCE NOTE:
;;; The naive free-bind implementation is O(n²) for left-associative chains:
;;;   ((a >>= b) >>= c) >>= d
;;; Each bind traverses the entire left structure to attach continuations.
;;;
;;; This implementation uses a continuation queue to avoid the O(n²) problem.
;;; Instead of rebuilding nested structures, we accumulate continuations in
;;; a list and apply them all at once when the free monad is interpreted.
;;;
;;; IMPORTANT CAVEAT:
;;; This implementation uses `append` to add continuations to the queue,
;;; which is O(queue_length) per bind operation. For n binds, total cost
;;; is O(n²) in the worst case if all binds hit the queue path.
;;;
;;; However, in typical use (mixed pure values and suspended computations),
;;; the performance is much better than naive Free because:
;;; 1. Pure values short-circuit immediately
;;; 2. We only traverse the queue once at interpretation time
;;; 3. The queue is typically short in practice
;;;
;;; For TRUE O(1) per-bind performance, use the Codensity monad in
;;; lattice/fp/category/kan-extension.ss, which represents continuations
;;; as nested lambdas (function composition is O(1)).
;;;
;;; Free-Queue: ('free-queue base-free fmap continuation-queue)
;;; where continuation-queue is a list of (a -> Free f b) functions.
;;;
;;; This is the "bind queue" approach: we defer the actual traversal until
;;; the free monad is run, at which point we apply all queued continuations.

;;; make-free-queue : Free f a -> (f a -> f b) -> (List (a -> Free f b)) -> Free-Queue f b
(define (make-free-queue base fmap conts)
  (list 'free-queue base fmap conts))

;;; free-queue? : Any -> Boolean
(define (free-queue? x)
  (and (pair? x) (eq? (car x) 'free-queue)))

;;; free-queue-base : Free-Queue f a -> Free f x
(define (free-queue-base q)
  (list-ref q 1))

;;; free-queue-fmap : Free-Queue f a -> (f a -> f b)
(define (free-queue-fmap q)
  (list-ref q 2))

;;; free-queue-conts : Free-Queue f a -> (List (x -> Free f a))
(define (free-queue-conts q)
  (list-ref q 3))

;;; free-bind : (f a -> f b) -> Free f a -> (a -> Free f b) -> Free f b
;;; Avoids O(n²) tree-rebuilding via continuation queue.
;;; Left-associative chains like ((a >>= b) >>= c) >>= d append
;;; to the continuation queue instead of rebuilding structure.
;;; Note: append is O(queue_length), not O(1). See header for details.
(define (free-bind fmap fr f)
  (cond
   ;; Pure value: apply continuation immediately (O(1))
   [(pure-free? fr) (f (from-pure-free fr))]
   ;; Queue: append continuation (O(queue_length) due to append)
   [(free-queue? fr)
    (make-free-queue (free-queue-base fr)
                     (free-queue-fmap fr)
                     (append (free-queue-conts fr) (list f)))]
   ;; Suspended: wrap in queue with single continuation (O(1))
   [(free-suspended? fr)
    (make-free-queue fr fmap (list f))]))

;;; free-normalize : (f a -> f b) -> Free f a -> Free f a
;;; Convert queue form back to standard form for interpretation.
;;; This is called by interpreters when they need to process the free monad.
(define (free-normalize fmap fr)
  (if (free-queue? fr)
      (free-apply-queue (free-queue-fmap fr)
                        (free-queue-base fr)
                        (free-queue-conts fr))
      fr))

;;; free-apply-queue : (f a -> f b) -> Free f a -> (List (a -> Free f b)) -> Free f b
;;; Apply queued continuations to a free monad.
;;; This is where the actual work happens, but only once at run time.
(define (free-apply-queue fmap fr conts)
  (if (null? conts)
      fr
      (free-apply-queue-step fmap fr conts)))

;;; free-apply-queue-step : (f a -> f b) -> Free f a -> (List (a -> Free f b)) -> Free f b
;;; Step through the free monad, threading continuations.
(define (free-apply-queue-step fmap fr conts)
  (cond
   [(null? conts) fr]
   [(pure-free? fr)
    ;; Pure value: apply first continuation, continue with rest
    (let ([next ((car conts) (from-pure-free fr))])
         (free-apply-queue fmap (free-normalize fmap next) (cdr conts)))]
   [(free-queue? fr)
    ;; Nested queue: flatten by prepending inner queue's continuations
    (free-apply-queue-step (free-queue-fmap fr)
                           (free-queue-base fr)
                           (append (free-queue-conts fr) conts))]
   [(free-suspended? fr)
    ;; Suspended: wrap continuation to apply remaining queue after response
    (free (fmap (lambda (inner)
                        (free-apply-queue fmap inner conts))
                (from-free fr)))]))

;;; free-then : (f a -> f b) -> Free f a -> Free f b -> Free f b
;;; Sequence, discarding first result.
(define (free-then fmap fr1 fr2)
  (free-bind fmap fr1 (lambda (_) fr2)))

;;; free-ap : (f a -> f b) -> Free f (a -> b) -> Free f a -> Free f b
;;; Applicative apply for Free.
(define (free-ap fmap fr-f fr-a)
  (free-bind fmap fr-f
             (lambda (f)
                     (free-bind fmap fr-a
                                (lambda (a)
                                        (pure-free (f a)))))))

;;; ====
;;; Lifting into Free
;;; ====

;;; lift-free : (a -> f a) -> a -> Free f a
;;; Lift a value into a suspended Free using the functor's point.
;;; For command-style DSLs, this wraps a command.
(define (lift-free wrap x)
  (free (wrap (pure-free x))))

;;; ====
;;; Interpreters
;;; ====

;;; fold-free : (a -> b) -> (f b -> b) -> (f a -> f b) -> Free f a -> b
;;; Catamorphism for Free. Interprets the structure.
;;; - on-pure: what to do with pure values
;;; - on-free: how to combine functor layer
;;; - fmap: the functor's map
;;; Handles the queue form by normalizing first.
(define (fold-free on-pure on-free fmap fr)
  (cond
   [(pure-free? fr)
    (on-pure (from-pure-free fr))]
   [(free-queue? fr)
    ;; Normalize queue form before folding
    (fold-free on-pure on-free fmap (free-normalize fmap fr))]
   [else
    (on-free (fmap (lambda (inner) (fold-free on-pure on-free fmap inner))
                   (from-free fr)))]))

;;; iter-free : (f a -> a) -> (f a -> f b) -> Free f a -> a
;;; Iterate the Free structure, collapsing it to a value.
(define (iter-free collapse fmap fr)
  (fold-free identity collapse fmap fr))

;;; run-free : (a -> m a) -> (f (m a) -> m a) -> (f a -> f b) -> Free f a -> m a
;;; Run a Free computation into a monad m.
;;; - m-pure: pure for the target monad
;;; - interpret: how to interpret each functor layer into the monad
;;; - fmap: the functor's map
(define (run-free m-pure interpret fmap fr)
  (fold-free m-pure interpret fmap fr))

;;; ====
;;; Common Functor: Command Pattern
;;; ====
;;;
;;; A common pattern is to define commands as a functor where
;;; each command holds a continuation for the next action.
;;;
;;; Example: Console DSL
;;;   PrintLine msg next      — print message, then continue
;;;   ReadLine (line -> next) — read line, pass to continuation
;;;
;;; We represent this as:
;;;   ('print-line msg next)
;;;   ('read-line k)

;;; ====
;;; Example: Key-Value Store DSL
;;; ====

;;; KV Store commands:
;;;   ('get key k)      — get value for key, pass to k
;;;   ('put key val next) — put value, then continue
;;;   ('delete key next)  — delete key, then continue

;;; kv-get : α → (Free KVF (Maybe β))
;;; Get value for key from KV store.
(define (kv-get key)
  (free (list 'get key pure-free)))

;;; kv-put : α × β → (Free KVF Unit)
;;; Put key-value pair in KV store.
(define (kv-put key val)
  (free (list 'put key val (pure-free '()))))

;;; kv-delete : α → (Free KVF Unit)
;;; Delete key from KV store.
(define (kv-delete key)
  (free (list 'delete key (pure-free '()))))

;;; kv-fmap : (α → β) × (KVF α) → (KVF β)
;;; Functor instance for KV commands.
(define (kv-fmap f cmd)
  (let ([tag (car cmd)])
       (cond
        [(eq? tag 'get)
         (let ([key (cadr cmd)]
               [k (caddr cmd)])
              (list 'get key (lambda (v) (f (k v)))))]
        [(eq? tag 'put)
         (let ([key (cadr cmd)]
               [val (caddr cmd)]
               [next (cadddr cmd)])
              (list 'put key val (f next)))]
        [(eq? tag 'delete)
         (let ([key (cadr cmd)]
               [next (caddr cmd)])
              (list 'delete key (f next)))]
        [else (error 'kv-fmap "Unknown command")])))

;;; run-kv : (Free KVF α) → (Alist → (α . Alist))
;;; Interpret KV DSL as stateful computation over an alist.
;;; Handles the queue form by normalizing first.
(define (run-kv program)
  (lambda (store)
          (cond
           [(pure-free? program)
            (cons (from-pure-free program) store)]
           [(free-queue? program)
            ;; Normalize queue form before running
            ((run-kv (free-normalize kv-fmap program)) store)]
           [else
            (let* ([cmd (from-free program)]
                   [tag (car cmd)])
                  (cond
                   [(eq? tag 'get)
                    (let* ([key (cadr cmd)]
                           [k (caddr cmd)]
                           [pair (assoc key store)]
                           [value (if pair (just (cdr pair)) nothing)]
                           [next (k value)])
                          ((run-kv next) store))]
                   [(eq? tag 'put)
                    (let* ([key (cadr cmd)]
                           [val (caddr cmd)]
                           [next (cadddr cmd)]
                           [new-store (cons (cons key val)
                                            (filter (lambda (p) (not (equal? (car p) key)))
                                                    store))])
                          ((run-kv next) new-store))]
                   [(eq? tag 'delete)
                    (let* ([key (cadr cmd)]
                           [next (caddr cmd)]
                           [new-store (filter (lambda (p) (not (equal? (car p) key)))
                                              store)])
                          ((run-kv next) new-store))]
                   [else (error 'run-kv "Unknown command")]))])))

;;; ====
;;; Example: Console DSL
;;; ====

;;; Console commands:
;;;   ('print msg next)    — print message, continue
;;;   ('read k)            — read input, pass to k

;;; console-print : String → (Free ConsoleF Unit)
;;; Print a message to console.
(define (console-print msg)
  (free (list 'print msg (pure-free '()))))

;;; console-read : (Free ConsoleF String)
;;; Read a line from console.
(define console-read
  (free (list 'read pure-free)))

;;; console-fmap : (α → β) × (ConsoleF α) → (ConsoleF β)
;;; Functor instance for Console commands.
(define (console-fmap f cmd)
  (let ([tag (car cmd)])
       (cond
        [(eq? tag 'print)
         (let ([msg (cadr cmd)]
               [next (caddr cmd)])
              (list 'print msg (f next)))]
        [(eq? tag 'read)
         (let ([k (cadr cmd)])
              (list 'read (lambda (s) (f (k s)))))]
        [else (error 'console-fmap "Unknown command")])))

;;; run-console-pure : (Free ConsoleF α) × (List String) → (α . (List String))
;;; Pure interpreter: uses list of strings as mock input, collects output.
;;; Handles the queue form by normalizing first.
(define (run-console-pure program inputs)
  (let loop ([prog program] [ins inputs] [outs '()])
       (cond
        [(pure-free? prog)
         (cons (from-pure-free prog) (reverse outs))]
        [(free-queue? prog)
         ;; Normalize queue form before running
         (loop (free-normalize console-fmap prog) ins outs)]
        [else
         (let* ([cmd (from-free prog)]
                [tag (car cmd)])
               (cond
                [(eq? tag 'print)
                 (let ([msg (cadr cmd)]
                       [next (caddr cmd)])
                      (loop next ins (cons msg outs)))]
                [(eq? tag 'read)
                 (let ([k (cadr cmd)])
                      (if (null? ins)
                          (error 'run-console-pure "No more input")
                          (loop (k (car ins)) (cdr ins) outs)))]
                [else (error 'run-console-pure "Unknown command")]))])))

;;; ====
;;; Free Monad Combinators
;;; ====

;;; free-sequence : (f a -> f b) -> (List (Free f a)) -> Free f (List a)
;;; Sequence a list of Free computations.
(define (free-sequence fmap frees)
  (if (null? frees)
      (pure-free '())
      (free-bind fmap (car frees)
                 (lambda (x)
                         (free-bind fmap (free-sequence fmap (cdr frees))
                                    (lambda (xs)
                                            (pure-free (cons x xs))))))))

;;; free-map-m : (f a -> f b) -> (a -> Free f b) -> (List a) -> Free f (List b)
;;; Map a Free-returning function over a list.
(define (free-map-m fmap f lst)
  (free-sequence fmap (map f lst)))

;;; free-for-each : (f a -> f b) -> (List a) -> (a -> Free f ()) -> Free f ()
;;; Execute a Free action for each element.
(define (free-for-each fmap lst f)
  (if (null? lst)
      (pure-free '())
      (free-then fmap (f (car lst))
                 (free-for-each fmap (cdr lst) f))))

;;; free-when : (f a -> f b) -> Boolean -> Free f () -> Free f ()
;;; Conditional execution.
(define (free-when fmap condition action)
  (if condition action (pure-free '())))

;;; free-unless : (f a -> f b) -> Boolean -> Free f () -> Free f ()
(define (free-unless fmap condition action)
  (free-when fmap (not condition) action))

;;; ====
;;; Optimization: View Patterns
;;; ====

;;; Sometimes we want to inspect a Free structure without running it.

;;; free-commands : (f a -> f b) -> Free f a -> List
;;; Extract a list of commands (for debugging/optimization).
;;; Handles the queue form by normalizing first.
(define (free-commands fmap fr)
  (cond
   [(pure-free? fr) '()]
   [(free-queue? fr)
    ;; Normalize queue form before inspecting
    (free-commands fmap (free-normalize fmap fr))]
   [else
    (let ([cmd (from-free fr)])
         (cons cmd
               ;; Would need to access the continuation, which varies by command type
               ;; This is a simplified version
               '()))]))

;;; ====
;;; Coyoneda: Functor from Any Type
;;; ====
;;;
;;; Sometimes we have a type that isn't a functor but we want to use Free.
;;; Coyoneda f a = exists b. (b -> a, f b)
;;; This gives us fmap for free!

;;; make-coyoneda : (f α) → (Coyoneda f α)
;;; Lift a functor value into Coyoneda.
(define (make-coyoneda fa)
  (list 'coyoneda identity fa))

;;; coyoneda? : Any → Boolean
(define (coyoneda? x)
  (and (pair? x) (eq? (car x) 'coyoneda)))

;;; coyoneda-map : (α → β) × (Coyoneda f α) → (Coyoneda f β)
;;; Map over a Coyoneda value.
(define (coyoneda-map f cy)
  (let ([g (cadr cy)]
        [fa (caddr cy)])
       (list 'coyoneda (compose f g) fa)))

;;; lower-coyoneda : ((α → β) → (f α) → (f β)) × (Coyoneda f α) → (f α)
;;; Lower Coyoneda back to the original functor (requires fmap).
(define (lower-coyoneda fmap cy)
  (let ([f (cadr cy)]
        [fa (caddr cy)])
       (fmap f fa)))

;;; ====
;;; Example Usage (for documentation)
;;; ====
;;;
;;; ;; Define a KV store program
;;; (define kv-program
;;;   (free-bind kv-fmap (kv-put 'x 10)
;;;              (lambda (_)
;;;                (free-bind kv-fmap (kv-put 'y 20)
;;;                           (lambda (_)
;;;                             (free-bind kv-fmap (kv-get 'x)
;;;                                        (lambda (mx)
;;;                                          (free-bind kv-fmap (kv-get 'y)
;;;                                                     (lambda (my)
;;;                                                       (pure-free
;;;                                                        (+ (from-just mx)
;;;                                                           (from-just my))))))))))))
;;;
;;; ((run-kv kv-program) '())
;;; ; => (30 . ((y . 20) (x . 10)))
;;;
;;; ;; Console program
;;; (define greet
;;;   (free-bind console-fmap (console-print "What is your name?")
;;;              (lambda (_)
;;;                (free-bind console-fmap console-read
;;;                           (lambda (name)
;;;                             (console-print (string-append "Hello, " name "!")))))))
;;;
;;; (run-console-pure greet '("Alice"))
;;; ; => (() . ("What is your name?" "Hello, Alice!"))
