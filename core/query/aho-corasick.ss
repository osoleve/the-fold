;;; core/query/aho-corasick.ss --- Aho-Corasick Multi-Pattern String Matching
;;;
;;; Dogfooding new data structures: Queue for BFS, Dict for transitions
(load "core/data/data-structures.ss")

;;; State = (id Dict Set Nat) where Dict: Char -> Nat, Set of patterns,  Nat is failure link
(define-record-type ac-state
  (fields id trans output fail))

;;; make-state : Nat → ACState
(define (make-state id)
  (make-ac-state id dict-empty set-empty 0))

;;; build-trie : (List String) → (Vector ACState)
;;; Build trie with mutable vector to avoid O(N^2) copying
(define (build-trie patterns)
  (let* ([total-chars (fold-left (lambda (acc p) (+ acc (string-length p))) 0 patterns)]
         [capacity (max 64 (+ total-chars 1))]  ; Pre-allocate with estimated capacity
         [states (make-vector capacity #f)]
         [size 1])  ; Current size (starts at 1 for root state)
        (vector-set! states 0 (make-state 0))
        (let loop-patterns ([patterns patterns]
                            [next-id 1])
             (if (null? patterns)
                 (vector-copy states 0 size)  ; Return only used portion
                 (let ([new-id (insert-pattern-mut! (car patterns) states
                                                    (lambda () size)
                                                    (lambda (new-size) (set! size new-size))
                                                    next-id)])
                      (loop-patterns (cdr patterns) new-id))))))

;;; insert-pattern-mut! : String × (Vector ACState) × (→ Nat) × (→ Nat Void) × Nat → Nat
;;; Mutable version: mutates states vector in place
(define (insert-pattern-mut! pattern states get-size set-size! next-id)
  (let loop-chars ([chars (string->list pattern)]
                   [sid 0]
                   [next-id next-id])
       (if (null? chars)
           ;; Mark pattern end
           (let* ([state (vector-ref states sid)]
                  [new-output (set-add pattern (ac-state-output state))]
                  [new-state (make-ac-state sid
                                            (ac-state-trans state)
                                            new-output
                                            (ac-state-fail state))])
                 (vector-set! states sid new-state)
                 next-id)
           ;; Process char
           (let* ([ch (car chars)]
                  [state (vector-ref states sid)]
                  [trans (ac-state-trans state)]
                  [next (dict-lookup ch trans)])
                 (if next
                     (loop-chars (cdr chars) next next-id)
                     ;; Create new state
                     (let* ([new-state (make-state next-id)]
                            [new-trans (dict-assoc ch next-id trans)]
                            [updated-parent (make-ac-state sid
                                                           new-trans
                                                           (ac-state-output state)
                                                           (ac-state-fail state))])
                           (vector-set! states next-id new-state)
                           (set-size! (+ (get-size) 1))
                           (vector-set! states sid updated-parent)
                           (loop-chars (cdr chars) next-id (+ next-id 1))))))))

;;; compute-failures : (Vector ACState) → (Vector ACState)
;;; Compute failures using Queue BFS (dogfooding!) with in-place mutation
(define (compute-failures states)
  (let* ([root (vector-ref states 0)]
         [children (dict-values (ac-state-trans root))]
         [init-q (fold-left (lambda (q child) (queue-enqueue child q))
                            queue-empty
                            children)])
        (bfs-mut! states init-q)
        states))

;;; bfs-mut! : (Vector ACState) × Queue → Void
(define (bfs-mut! states queue)
  (if (queue-empty? queue)
      (void)
      (let-values ([(q2 sid) (queue-dequeue queue)])
                  (let* ([state (vector-ref states sid)]
                         [trans (ac-state-trans state)])
                        (let loop-trans ([keys (dict-keys trans)]
                                         [q q2])
                             (if (null? keys)
                                 (bfs-mut! states q)
                                 (let* ([ch (car keys)]
                                        [child-id (dict-lookup ch trans)]
                                        [fail-id (find-fail states sid ch)]
                                        [child (vector-ref states child-id)]
                                        [fail-state (vector-ref states fail-id)]
                                        [new-output (set-union (ac-state-output child)
                                                               (ac-state-output fail-state))]
                                        [new-child (make-ac-state child-id
                                                                  (ac-state-trans child)
                                                                  new-output
                                                                  fail-id)])
                                       (vector-set! states child-id new-child)
                                       (loop-trans (cdr keys)
                                                   (queue-enqueue child-id q)))))))))

;;; find-fail : (Vector ACState) × Nat × Char → Nat
(define (find-fail states sid ch)
  (let* ([state (vector-ref states sid)]
         [fail-id (ac-state-fail state)])
        (if (= fail-id 0)
            (let ([next (dict-lookup ch (ac-state-trans (vector-ref states 0)))])
                 (if next next 0))
            (let ([next (dict-lookup ch (ac-state-trans (vector-ref states fail-id)))])
                 (if next
                     next
                     (find-fail states fail-id ch))))))

;;; Main API

;;; make-automaton : (List String) → (Vector ACState)
(define (make-automaton patterns)
  (compute-failures (build-trie patterns)))

;;; search : (Vector ACState) × String → (List (Pair Nat String))
(define (search automaton text)
  (let loop ([chars (string->list text)]
             [pos 0]
             [sid 0]
             [matches '()])
       (if (null? chars)
           (reverse matches)
           (let* ([ch (car chars)]
                  [next-sid (get-next automaton sid ch)]
                  [state (vector-ref automaton next-sid)]
                  [outputs (set->list (ac-state-output state))])
                 (loop (cdr chars)
                       (+ pos 1)
                       next-sid
                       (append (map (lambda (p)
                                            (cons (- (+ pos 1) (string-length p)) p))
                                    outputs)
                               matches))))))

;;; get-next : (Vector ACState) × Nat × Char → Nat
(define (get-next automaton sid ch)
  (let ([next (dict-lookup ch (ac-state-trans (vector-ref automaton sid)))])
       (if next
           next
           (if (= sid 0)
               0
               (get-next automaton (ac-state-fail (vector-ref automaton sid)) ch)))))

(printf "Aho-Corasick loaded (dogfooding Queue + Dict + Set!)\n")
