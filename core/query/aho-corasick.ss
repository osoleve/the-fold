;;; core/query/aho-corasick.ss --- Aho-Corasick Multi-Pattern String Matching
;;;
;;; Dogfooding new data structures: Queue for BFS, Dict for transitions
(load "core/data/data-structures.ss")

;;; State = (id Dict Set Nat) where Dict: Char -> Nat, Set of patterns,  Nat is failure link
(define-record-type ac-state
  (fields id trans output fail))

(define (make-state id)
  (make-ac-state id dict-empty set-empty 0))

;;; Build trie
(define (build-trie patterns)
  (let loop-patterns ([patterns patterns]
                      [states (vector (make-state 0))]
                      [next-id 1])
       (if (null? patterns)
           states
           (let-values ([(new-states new-id)
                         (insert-pattern (car patterns) states next-id)])
                       (loop-patterns (cdr patterns) new-states new-id)))))

(define (insert-pattern pattern states next-id)
  (let loop-chars ([chars (string->list pattern)]
                   [sid 0]
                   [states states]
                   [next-id next-id])
       (if (null? chars)
           ;; Mark pattern end
           (let* ([state (vector-ref states sid)]
                  [new-output (set-add pattern (ac-state-output state))]
                  [new-state (make-ac-state sid
                                            (ac-state-trans state)
                                            new-output
                                            (ac-state-fail state))])
                 (values (vec-set states sid new-state) next-id))
           ;; Process char
           (let* ([ch (car chars)]
                  [state (vector-ref states sid)]
                  [trans (ac-state-trans state)]
                  [next (dict-lookup ch trans)])
                 (if next
                     (loop-chars (cdr chars) next states next-id)
                     ;; Create new state
                     (let* ([new-state (make-state next-id)]
                            [states2 (vec-append states new-state)]
                            [new-trans (dict-assoc ch next-id trans)]
                            [updated-parent (make-ac-state sid
                                                           new-trans
                                                           (ac-state-output state)
                                                           (ac-state-fail state))]
                            [states3 (vec-set states2 sid updated-parent)])
                           (loop-chars (cdr chars) next-id states3 (+ next-id 1))))))))

(define (vec-append vec elem)
  (let* ([len (vector-length vec)]
         [new (make-vector (+ len 1))])
        (do ([i 0 (+ i 1)])
            ((= i len))
            (vector-set! new i (vector-ref vec i)))
        (vector-set! new len elem)
        new))

(define (vec-set vec idx val)
  (let* ([len (vector-length vec)]
         [new (make-vector len)])
        (do ([i 0 (+ i 1)])
            ((= i len))
            (vector-set! new i (vector-ref vec i)))
        (vector-set! new idx val)
        new))

;;; Compute failures using Queue BFS (dogfooding!)
(define (compute-failures states)
  (let* ([root (vector-ref states 0)]
         [children (dict-values (ac-state-trans root))]
         [init-q (fold-left (lambda (q child) (queue-enqueue child q))
                            queue-empty
                            children)])
        (bfs states init-q)))

(define (bfs states queue)
  (if (queue-empty? queue)
      states
      (let-values ([(q2 sid) (queue-dequeue queue)])
                  (let* ([state (vector-ref states sid)]
                         [trans (ac-state-trans state)])
                        (let loop-trans ([keys (dict-keys trans)]
                                         [states states]
                                         [q q2])
                             (if (null? keys)
                                 (bfs states q)
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
                                                                  fail-id)]
                                        [new-states (vec-set states child-id new-child)])
                                       (loop-trans (cdr keys)
                                                   new-states
                                                   (queue-enqueue child-id q)))))))))

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
(define (make-automaton patterns)
  (compute-failures (build-trie patterns)))

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

(define (get-next automaton sid ch)
  (let ([next (dict-lookup ch (ac-state-trans (vector-ref automaton sid)))])
       (if next
           next
           (if (= sid 0)
               0
               (get-next automaton (ac-state-fail (vector-ref automaton sid)) ch)))))

(printf "Aho-Corasick loaded (dogfooding Queue + Dict + Set!)\n")
