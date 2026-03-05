;;; lattice/fp/parsing/fsm.ss — Finite State Machines
;;; @module fsm
;;; @requires prelude combinators sort

(require 'prelude)
(require 'combinators)
(require 'sort)

(doc 'module 'fsm)
(doc 'description "Finite State Machine Library — A pure functional implementation of finite state machines supporting deterministic (DFA), non-deterministic (NFA), and epsilon-NFA automata.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'features '(dfa nfa epsilon-nfa construction execution nfa-to-dfa minimization composition language-operations moore-mealy visualization))

;;; Local list predicate — avoids dependence on R6RS for-all which can be
;;; shadowed by quickcheck's for-all in the interaction environment.
(define (every pred lst)
  (doc 'export #t)
  (or (null? lst)
      (and (pred (car lst))
           (every pred (cdr lst)))))

(doc 'section 'finite-state-machine-types)

(define (make-fsm states alphabet transitions start accepting . epsilon)
  (doc 'export #t)
  (doc 'type '(-> (List State) (List Symbol) (List Transition) State (List State) (List EpsilonTransition) FSM))
  (doc 'description "Create FSM with states, alphabet, transitions, start state, accepting states, and optional epsilon-transitions")
  (list 'fsm states alphabet transitions start accepting
        (if (null? epsilon) '() (car epsilon))
        '()))  ; Empty assertions list for backward compatibility

(define (make-fsm-with-assertions states alphabet transitions start accepting epsilon assertions)
  (doc 'export #t)
  (doc 'type '(-> (List State) (List Symbol) (List Transition) State (List State) (List EpsilonTransition) (List Assertion) FSM))
  (doc 'description "Create FSM with assertion transitions (for anchors and lookahead)")
  (list 'fsm states alphabet transitions start accepting epsilon assertions))

(define (fsm? x)
  (doc 'export #t)
  (doc 'type '(-> α Boolean))
  (and (list? x) (>= (length x) 7) (eq? (car x) 'fsm)))

(define (fsm-states fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List State)))
  (list-ref fsm 1))

(define (fsm-alphabet fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List Symbol)))
  (list-ref fsm 2))

(define (fsm-transitions fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List Transition)))
  (list-ref fsm 3))

(define (fsm-start fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM State))
  (list-ref fsm 4))

(define (fsm-accepting fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List State)))
  (list-ref fsm 5))

(define (fsm-epsilon fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List EpsilonTransition)))
  (list-ref fsm 6))

(define (fsm-assertions fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List Assertion)))
  (doc 'description "Get assertion transitions (anchors, lookahead). Format: (from-state type ...args target-state)")
  (if (>= (length fsm) 8)
      (list-ref fsm 7)
      '()))

(define (fsm-deterministic? fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM Boolean))
  (doc 'description "Check if FSM is deterministic (no epsilon, single transitions)")
  (and (null? (fsm-epsilon fsm))
       (let ([trans (fsm-transitions fsm)])
            (every (lambda (t) (<= (length (cdr t)) 1)) trans))))

(doc 'section 'fsm-construction-helpers)

(define (dfa states alphabet transitions start accepting)
  (doc 'export #t)
  (doc 'type '(-> (List State) (List Symbol) (List (Tuple State Symbol State)) State (List State) FSM))
  (doc 'description "Create a simple DFA from explicit parts")
  (make-fsm states alphabet
            (map (lambda (t)
                         (cons (cons (car t) (cadr t)) (list (caddr t))))
                 transitions)
            start accepting))

(define (nfa states alphabet transitions start accepting)
  (doc 'export #t)
  (doc 'type '(-> (List State) (List Symbol) (List Transition) State (List State) FSM))
  (doc 'description "Create an NFA with possible multiple transitions")
  (make-fsm states alphabet transitions start accepting))

(define (epsilon-nfa states alphabet transitions start accepting epsilon-trans)
  (doc 'export #t)
  (doc 'type '(-> (List State) (List Symbol) (List Transition) State (List State) (List EpsilonTransition) FSM))
  (doc 'description "Create an ε-NFA")
  (make-fsm states alphabet transitions start accepting epsilon-trans))

(doc 'section 'fsm-execution)

(define (fsm-delta fsm state input)
  (doc 'export #t)
  (doc 'type '(-> FSM State Symbol (List State)))
  (doc 'description "Get transition targets from state on input")
  (let ([key (cons state input)])
       (let ([found (assoc key (fsm-transitions fsm))])
            (if found (cdr found) '()))))

;;; get-all-epsilon-targets : FSM × State → (List State)
;;; Get all epsilon targets from a state (handles multiple entries for same state)
(define (get-all-epsilon-targets fsm state)
  (doc 'export #t)
  (fold-left (lambda (acc entry)
                     (if (equal? (car entry) state)
                         (append (cdr entry) acc)
                         acc))
             '()
             (fsm-epsilon fsm)))

(define (epsilon-closure fsm state)
  (doc 'export #t)
  (doc 'type '(-> FSM State (List State)))
  (doc 'description "Get epsilon-closure of a state")
  (let loop ([frontier (list state)] [visited '()])
       (if (null? frontier)
           visited
           (let ([s (car frontier)])
                (if (member s visited)
                    (loop (cdr frontier) visited)
                    (let* ([eps-targets (get-all-epsilon-targets fsm s)]
                           [new-frontier (append eps-targets (cdr frontier))])
                          (loop new-frontier (cons s visited))))))))

;;; epsilon-closure-set : FSM × (List State) → (List State)
;;; Get epsilon-closure of a set of states
(define (epsilon-closure-set fsm states)
  (doc 'export #t)
  (fold-left (lambda (acc s)
                     (union equal? acc (epsilon-closure fsm s)))
             '()
             states))

;;; union : (α × α → Boolean) × (List α) × (List α) → (List α)
;;; Helper: set union
(define (union eq? xs ys)
  (doc 'export #t)
  (fold-left (lambda (acc x)
                     (if (exists (lambda (y) (eq? x y)) acc) acc (cons x acc)))
             ys xs))

;;; fsm-move : FSM × (List State) × Symbol → (List State)
;;; Move from a set of states on input, then epsilon-close
(define (fsm-move fsm states input)
  (doc 'export #t)
  (let* ([direct-targets
          (fold-left (lambda (acc s)
                             (union equal? acc (fsm-delta fsm s input)))
                     '()
                     states)])
        (epsilon-closure-set fsm direct-targets)))

(define (fsm-run fsm input)
  (doc 'export #t)
  (doc 'type '(-> FSM (Union String (List Symbol)) (Option (List State))))
  (doc 'description "Run FSM on input sequence. Returns: Just final-states if any accepting, Nothing otherwise")
  (let* ([input-list (if (string? input)
                         (string->list input)
                         input)]
         [start-states (epsilon-closure fsm (fsm-start fsm))]
         [final-states
          (fold-left (lambda (states sym)
                             (fsm-move fsm states sym))
                     start-states
                     input-list)])
        (if (exists (lambda (s) (member s (fsm-accepting fsm))) final-states)
            (just final-states)
            nothing)))

(define (fsm-accepts? fsm input)
  (doc 'export #t)
  (doc 'type '(-> FSM (Union String (List Symbol)) Boolean))
  (doc 'description "Check if FSM accepts input")
  (if (null? (fsm-assertions fsm))
      (just? (fsm-run fsm input))
      (just? (fsm-run-with-assertions fsm input))))

;;; ============================================================
;;; Assertion-aware execution
;;; ============================================================

;;; Check if an assertion succeeds given context
(define (assertion-succeeds? assertion pos input-vec len)
  (doc 'export #t)
  (let ([type (cadr assertion)])
    (cond
     [(eq? type 'anchor)
      (let ([anchor-type (caddr assertion)])
        (cond
         [(eq? anchor-type 'start) (= pos 0)]
         [(eq? anchor-type 'end) (= pos len)]
         [else #f]))]
     [(eq? type 'lookahead)
      (let* ([inner-fsm (caddr assertion)]
             [positive? (cadddr assertion)]
             ;; Get remaining input as string
             [remaining (list->string
                         (let loop ([i pos] [acc '()])
                           (if (>= i len)
                               (reverse acc)
                               (loop (+ i 1) (cons (vector-ref input-vec i) acc)))))]
             ;; Lookahead checks if inner pattern matches a PREFIX of remaining input
             ;; (not the entire remaining string)
             [matches? (fsm-accepts-prefix-with-assertions? inner-fsm remaining)])
        (if positive? matches? (not matches?)))]
     [else #f])))

;;; Get assertion targets from a state that succeed in current context
(define (get-assertion-targets fsm state pos input-vec len)
  (doc 'export #t)
  (fold-left (lambda (acc assertion)
               (if (and (equal? (car assertion) state)
                        (assertion-succeeds? assertion pos input-vec len))
                   ;; Target is last element of assertion
                   (cons (car (reverse assertion)) acc)
                   acc))
             '()
             (fsm-assertions fsm)))

;;; Epsilon-closure with assertion support
(define (epsilon-closure-with-context fsm state pos input-vec len)
  (doc 'export #t)
  (let loop ([frontier (list state)] [visited '()])
    (if (null? frontier)
        visited
        (let ([s (car frontier)])
          (if (member s visited)
              (loop (cdr frontier) visited)
              (let* ([eps-targets (get-all-epsilon-targets fsm s)]
                     [assert-targets (get-assertion-targets fsm s pos input-vec len)]
                     [all-targets (append eps-targets assert-targets)]
                     [new-frontier (append all-targets (cdr frontier))])
                (loop new-frontier (cons s visited))))))))

;;; Epsilon-closure-set with context
(define (epsilon-closure-set-with-context fsm states pos input-vec len)
  (doc 'export #t)
  (fold-left (lambda (acc s)
               (union equal? acc (epsilon-closure-with-context fsm s pos input-vec len)))
             '()
             states))

;;; Move with context (for assertion-aware execution)
(define (fsm-move-with-context fsm states input pos input-vec len)
  (doc 'export #t)
  (let* ([direct-targets
          (fold-left (lambda (acc s)
                       (union equal? acc (fsm-delta fsm s input)))
                     '()
                     states)])
    (epsilon-closure-set-with-context fsm direct-targets pos input-vec len)))

;;; Run FSM with assertion support
(define (fsm-run-with-assertions fsm input)
  (doc 'export #t)
  (doc 'type '(-> FSM (Union String (List Symbol)) (Option (List State))))
  (doc 'description "Run FSM with assertion support (anchors, lookahead)")
  (let* ([input-list (if (string? input) (string->list input) input)]
         [input-vec (list->vector input-list)]
         [len (vector-length input-vec)]
         ;; Start with epsilon-closure at position 0
         [start-states (epsilon-closure-with-context fsm (fsm-start fsm) 0 input-vec len)])
    ;; Process each input symbol
    (let loop ([states start-states] [pos 0])
      (if (= pos len)
          ;; At end: check for accepting state
          (if (exists (lambda (s) (member s (fsm-accepting fsm))) states)
              (just states)
              nothing)
          ;; Process next character
          (let ([sym (vector-ref input-vec pos)]
                [next-pos (+ pos 1)])
            (let ([next-states (fsm-move-with-context fsm states sym next-pos input-vec len)])
              (if (null? next-states)
                  nothing
                  (loop next-states next-pos))))))))

;;; Check if FSM accepts a PREFIX of input (for lookahead semantics)
;;; Returns #t if FSM reaches an accepting state at any point, even with input remaining
(define (fsm-accepts-prefix-with-assertions? fsm input)
  (doc 'export #t)
  (doc 'type '(-> FSM (Union String (List Symbol)) Boolean))
  (doc 'description "Check if FSM accepts any prefix of input (for lookahead)")
  (let* ([input-list (if (string? input) (string->list input) input)]
         [input-vec (list->vector input-list)]
         [len (vector-length input-vec)]
         [start-states (epsilon-closure-with-context fsm (fsm-start fsm) 0 input-vec len)])
    ;; Check if we're already in accepting state (empty prefix)
    (let loop ([states start-states] [pos 0])
      (cond
       ;; If currently in accepting state, prefix accepted
       [(exists (lambda (s) (member s (fsm-accepting fsm))) states) #t]
       ;; If no more input, we're done (didn't find accepting state)
       [(= pos len) #f]
       ;; Process next character
       [else
        (let ([sym (vector-ref input-vec pos)]
              [next-pos (+ pos 1)])
          (let ([next-states (fsm-move-with-context fsm states sym next-pos input-vec len)])
            (if (null? next-states)
                #f  ; Dead end
                (loop next-states next-pos))))]))))

(doc 'section 'fsm-language-operations)

(define (fsm-reachable fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM (List State)))
  (doc 'description "Get reachable states from start")
  (let loop ([frontier (list (fsm-start fsm))] [visited '()])
       (if (null? frontier)
           visited
           (let ([s (car frontier)])
                (if (member s visited)
                    (loop (cdr frontier) visited)
                    (let* ([eps-targets (get-all-epsilon-targets fsm s)]
                           [trans-targets
                            (fold-left (lambda (acc sym)
                                               (union equal? acc (fsm-delta fsm s sym)))
                                       '()
                                       (fsm-alphabet fsm))]
                           [all-targets (union equal? eps-targets trans-targets)])
                          (loop (append all-targets (cdr frontier))
                                (cons s visited))))))))

(define (fsm-empty? fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM Boolean))
  (doc 'description "Check if FSM language is empty")
  (let ([reachable (fsm-reachable fsm)])
       (not (exists (lambda (s) (member s (fsm-accepting fsm))) reachable))))

(doc 'section 'nfa-to-dfa-conversion)

(define (state-set->name states)
  (doc 'export #t)
  (doc 'type '(-> (List State) Symbol))
  (doc 'description "Generate fresh state name from state set")
  (string->symbol
   (string-append "{"
                  (fold-left (lambda (acc s)
                                     (if (string=? acc "")
                                         (symbol->string s)
                                         (string-append acc "," (symbol->string s))))
                             ""
                             (sort-by symbol<? states))
                  "}")))

(define (nfa->dfa nfa)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Convert NFA to DFA using subset construction. If NFA has assertions (anchors/lookahead), returns NFA unchanged since assertions require position-aware execution.")
  ;; Assertions don't translate to DFA - return NFA unchanged
  (if (not (null? (fsm-assertions nfa)))
      nfa
      (let* ([alphabet (fsm-alphabet nfa)]
         [start-set (epsilon-closure nfa (fsm-start nfa))]
         [start-name (state-set->name start-set)])
        (let loop ([worklist (list start-set)]
                   [visited '()]
                   [dfa-states (list start-name)]
                   [dfa-trans '()]
                   [dfa-accepting '()])
             (if (null? worklist)
                 (make-fsm dfa-states alphabet dfa-trans start-name dfa-accepting)
                 (let ([current (car worklist)])
                      (if (member current visited)
                          (loop (cdr worklist) visited dfa-states dfa-trans dfa-accepting)
                          (let* ([current-name (state-set->name current)]
                                 ;; Check if accepting
                                 [is-accepting
                                  (exists (lambda (s) (member s (fsm-accepting nfa))) current)]
                                 ;; Compute transitions for each alphabet symbol
                                 [new-trans-and-states
                                  (map (lambda (sym)
                                               (let* ([target-set (fsm-move nfa current sym)]
                                                      [target-name (if (null? target-set)
                                                                       'dead
                                                                       (state-set->name target-set))])
                                                     (list sym target-set target-name)))
                                       alphabet)]
                                 ;; Filter non-empty transitions
                                 [valid-trans (filter (lambda (x) (not (null? (cadr x))))
                                                      new-trans-and-states)]
                                 ;; Build transition entries
                                 [new-trans (map (lambda (x)
                                                         (cons (cons current-name (car x))
                                                               (list (caddr x))))
                                                 valid-trans)]
                                 ;; New state sets to explore
                                 [new-state-sets (map cadr valid-trans)]
                                 ;; New state names
                                 [new-state-names (map caddr valid-trans)])
                                (loop (append new-state-sets (cdr worklist))
                                      (cons current visited)
                                      (union equal? new-state-names dfa-states)
                                      (append new-trans dfa-trans)
                                      (if is-accepting
                                          (cons current-name dfa-accepting)
                                          dfa-accepting))))))))))

(doc 'section 'fsm-composition)

(define *fsm-counter* 0)
(doc *fsm-counter* 'export #t)
(doc *fsm-counter* 'type 'Nat)
(doc *fsm-counter* 'description "Counter for generating fresh state names")

(define (fsm-fresh-state prefix)
  (doc 'export #t)
  (doc 'type '(-> String Symbol))
  (doc 'description "Generate fresh state name with prefix")
  (set! *fsm-counter* (+ *fsm-counter* 1))
  (string->symbol (string-append prefix (number->string *fsm-counter*))))

(define (fsm-rename fsm prefix)
  (doc 'export #t)
  (doc 'type '(-> FSM String FSM))
  (doc 'description "Rename states in FSM to avoid collisions")
  (let* ([rename (lambda (s)
                         (string->symbol
                          (string-append prefix (symbol->string s))))]
         [new-states (map rename (fsm-states fsm))]
         [new-start (rename (fsm-start fsm))]
         [new-accepting (map rename (fsm-accepting fsm))]
         [new-trans (map (lambda (t)
                                 (cons (cons (rename (caar t)) (cdar t))
                                       (map rename (cdr t))))
                         (fsm-transitions fsm))]
         [new-eps (map (lambda (e)
                               (cons (rename (car e))
                                     (map rename (cdr e))))
                       (fsm-epsilon fsm))]
         ;; Rename states in assertions: (from-state type ...args target-state)
         ;; For anchor: (s0 'anchor 'start s1) or (s0 'anchor 'end s1)
         ;; For lookahead: (s0 'lookahead inner-fsm positive? s1)
         [new-assertions (map (lambda (a)
                                (let* ([from (car a)]
                                       [type (cadr a)]
                                       [rest (cddr a)]           ; Everything after from and type
                                       [target (car (reverse rest))]
                                       [middle (reverse (cdr (reverse rest)))])  ; Everything between type and target
                                  (append (list (rename from) type)
                                          middle
                                          (list (rename target)))))
                              (fsm-assertions fsm))])
        (make-fsm-with-assertions new-states (fsm-alphabet fsm) new-trans
                                  new-start new-accepting new-eps new-assertions)))

(define (fsm-union fsm1 fsm2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM FSM))
  (doc 'description "Union of two FSMs (accepts if either accepts)")
  (let* ([m1 (fsm-rename fsm1 "a")]
         [m2 (fsm-rename fsm2 "b")]
         [new-start (fsm-fresh-state "u")]
         [all-states (cons new-start
                           (append (fsm-states m1) (fsm-states m2)))]
         [all-alphabet (union equal? (fsm-alphabet m1) (fsm-alphabet m2))]
         [all-trans (append (fsm-transitions m1) (fsm-transitions m2))]
         [all-accepting (append (fsm-accepting m1) (fsm-accepting m2))]
         [new-eps (cons (cons new-start (list (fsm-start m1) (fsm-start m2)))
                        (append (fsm-epsilon m1) (fsm-epsilon m2)))]
         [all-assertions (append (fsm-assertions m1) (fsm-assertions m2))])
        (make-fsm-with-assertions all-states all-alphabet all-trans
                                  new-start all-accepting new-eps all-assertions)))

(define (fsm-concat fsm1 fsm2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM FSM))
  (doc 'description "Concatenation of two FSMs (accepts m1 then m2)")
  (let* ([m1 (fsm-rename fsm1 "c")]
         [m2 (fsm-rename fsm2 "d")]
         [all-states (append (fsm-states m1) (fsm-states m2))]
         [all-alphabet (union equal? (fsm-alphabet m1) (fsm-alphabet m2))]
         [all-trans (append (fsm-transitions m1) (fsm-transitions m2))]
         ;; Add epsilon from m1 accepting to m2 start
         [bridge-eps (map (lambda (a) (cons a (list (fsm-start m2))))
                          (fsm-accepting m1))]
         [all-eps (append bridge-eps
                          (fsm-epsilon m1)
                          (fsm-epsilon m2))]
         [all-assertions (append (fsm-assertions m1) (fsm-assertions m2))])
        (make-fsm-with-assertions all-states all-alphabet all-trans
                                  (fsm-start m1) (fsm-accepting m2) all-eps all-assertions)))

(define (fsm-star fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Kleene star of FSM (zero or more repetitions)")
  (let* ([m (fsm-rename fsm "s")]
         [new-start (fsm-fresh-state "k")]
         [all-states (cons new-start (fsm-states m))]
         ;; Epsilon from new start to old start
         [start-eps (cons new-start (list (fsm-start m)))]
         ;; Epsilon from accepting back to old start
         [loop-eps (map (lambda (a) (cons a (list (fsm-start m))))
                        (fsm-accepting m))]
         [all-eps (cons start-eps (append loop-eps (fsm-epsilon m)))]
         ;; New start is also accepting (accepts empty)
         [all-accepting (cons new-start (fsm-accepting m))])
        (make-fsm-with-assertions all-states (fsm-alphabet m) (fsm-transitions m)
                                  new-start all-accepting all-eps (fsm-assertions m))))

(define (fsm-plus fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Kleene plus (one or more repetitions)")
  (fsm-concat fsm (fsm-star fsm)))

(define (fsm-optional fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Optional (zero or one)")
  (let* ([m (fsm-rename fsm "o")]
         [new-start (fsm-fresh-state "opt")]
         [all-states (cons new-start (fsm-states m))]
         [start-eps (cons new-start (list (fsm-start m)))]
         [all-eps (cons start-eps (fsm-epsilon m))]
         ;; New start is accepting
         [all-accepting (cons new-start (fsm-accepting m))])
        (make-fsm-with-assertions all-states (fsm-alphabet m) (fsm-transitions m)
                                  new-start all-accepting all-eps (fsm-assertions m))))

(doc 'section 'fsm-builders)

(define (fsm-char c)
  (doc 'export #t)
  (doc 'type '(-> Char FSM))
  (doc 'description "FSM that accepts exactly one character")
  (let ([s0 (fsm-fresh-state "q")]
        [s1 (fsm-fresh-state "q")])
       (make-fsm (list s0 s1)
                 (list c)
                 (list (cons (cons s0 c) (list s1)))
                 s0
                 (list s1))))

(define (fsm-epsilon-lang)
  (doc 'export #t)
  (doc 'type '(-> Unit FSM))
  (doc 'description "FSM that accepts empty string only")
  (let ([s0 (fsm-fresh-state "e")])
       (make-fsm (list s0) '() '() s0 (list s0))))

(define (fsm-any-of chars)
  (doc 'export #t)
  (doc 'type '(-> (List Char) FSM))
  (doc 'description "FSM that accepts any single character from list")
  (if (null? chars)
      (make-fsm (list 'empty) '() '() 'empty '())  ; Empty language
      (fold-left fsm-union
                 (fsm-char (car chars))
                 (map fsm-char (cdr chars)))))

(define (fsm-literal str)
  (doc 'export #t)
  (doc 'type '(-> String FSM))
  (doc 'description "FSM that accepts a literal string")
  (let ([chars (string->list str)])
       (if (null? chars)
           (fsm-epsilon-lang)
           (fold-left fsm-concat
                      (fsm-char (car chars))
                      (map fsm-char (cdr chars))))))

(doc 'section 'fsm-minimization)

(define (fsm-minimize dfa)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Minimize a DFA using partition refinement. If FSM has assertions, returns unchanged since assertions require position-aware execution.")
  ;; Assertions don't survive minimization - return unchanged
  (if (not (null? (fsm-assertions dfa)))
      dfa
      (if (not (fsm-deterministic? dfa))
          (fsm-minimize (nfa->dfa dfa))
      (let* ([states (fsm-states dfa)]
             [alphabet (fsm-alphabet dfa)]
             [accepting (fsm-accepting dfa)]
             [non-accepting (filter (lambda (s) (not (member s accepting))) states)]
             ;; Initial partition: accepting vs non-accepting
             [initial-partition
              (filter (lambda (p) (not (null? p)))
                      (list accepting non-accepting))])
            (let refine ([partition initial-partition])
                 (let ([new-partition (refine-partition dfa partition alphabet)])
                      (if (= (length new-partition) (length partition))
                          (build-minimized-dfa dfa partition)
                          (refine new-partition))))))))

;;; refine-partition : FSM × (List (List State)) × (List Symbol) → (List (List State))
;;; Refine partition by checking transitions
(define (refine-partition dfa partition alphabet)
  (doc 'export #t)
  (fold-left (lambda (p block)
                     (append (split-block dfa block partition alphabet)
                             (filter (lambda (b) (not (equal? b block))) p)))
             '()
             partition))

;;; split-block : FSM × (List State) × (List (List State)) × (List Symbol) → (List (List State))
;;; Split a block if states have different transition targets
(define (split-block dfa block partition alphabet)
  (doc 'export #t)
  (if (<= (length block) 1)
      (list block)
      (let loop ([remaining (cdr block)]
                 [groups (list (list (car block)))])
           (if (null? remaining)
               groups
               (let* ([s (car remaining)]
                      [matching-group
                       (find (lambda (g)
                                     (states-equivalent? dfa s (car g) partition alphabet))
                             groups)])
                     (if matching-group
                         (loop (cdr remaining)
                               (cons (cons s matching-group)
                                     (filter (lambda (g) (not (eq? g matching-group))) groups)))
                         (loop (cdr remaining)
                               (cons (list s) groups))))))))

;;; states-equivalent? : FSM × State × State × (List (List State)) × (List Symbol) → Boolean
;;; Check if two states are equivalent (same behavior)
(define (states-equivalent? dfa s1 s2 partition alphabet)
  (doc 'export #t)
  (every (lambda (sym)
                   (let ([t1 (fsm-delta dfa s1 sym)]
                         [t2 (fsm-delta dfa s2 sym)])
                        (cond
                         [(and (null? t1) (null? t2)) #t]
                         [(or (null? t1) (null? t2)) #f]
                         [else (same-partition-block? (car t1) (car t2) partition)])))
           alphabet))

;;; same-partition-block? : State × State × (List (List State)) → Boolean
;;; Check if two states are in the same partition block
(define (same-partition-block? s1 s2 partition)
  (doc 'export #t)
  (exists (lambda (block)
                  (and (member s1 block) (member s2 block)))
          partition))

;;; build-minimized-dfa : FSM × (List (List State)) → FSM
;;; Build minimized DFA from partition
(define (build-minimized-dfa dfa partition)
  (doc 'export #t)
  (let* ([state-to-block
          (lambda (s)
                  (find (lambda (b) (member s b)) partition))]
         [block-name
          (lambda (block)
                  (state-set->name block))]
         [new-states (map block-name partition)]
         [start-block (state-to-block (fsm-start dfa))]
         [new-start (block-name start-block)]
         [accepting-blocks
          (filter (lambda (b)
                          (exists (lambda (s) (member s (fsm-accepting dfa))) b))
                  partition)]
         [new-accepting (map block-name accepting-blocks)]
         [new-trans
          (fold-left
           (lambda (acc block)
                   (let ([rep (car block)]
                         [bname (block-name block)])
                        (append
                         (filter-map
                          (lambda (sym)
                                  (let ([targets (fsm-delta dfa rep sym)])
                                       (if (null? targets)
                                           #f
                                           (let ([target-block (state-to-block (car targets))])
                                                (cons (cons bname sym)
                                                      (list (block-name target-block)))))))
                          (fsm-alphabet dfa))
                         acc)))
           '()
           partition)])
        (make-fsm new-states (fsm-alphabet dfa) new-trans new-start new-accepting)))

;;; filter-map provided by prelude

(doc 'section 'fsm-visualization)

(define (dot-escape-string str)
  (doc 'export #t)
  (doc 'type '(-> String String))
  (doc 'description "Escape special characters for DOT format strings. Handles: backslash, double quote, newline, carriage return, tab")
  (let loop ([chars (string->list str)] [acc '()])
       (if (null? chars)
           (list->string (reverse acc))
           (let ([c (car chars)])
                (cond
                 [(char=? c #\\)
                  (loop (cdr chars) (cons #\\ (cons #\\ acc)))]
                 [(char=? c #\")
                  (loop (cdr chars) (cons #\" (cons #\\ acc)))]
                 [(char=? c #\newline)
                  (loop (cdr chars) (cons #\n (cons #\\ acc)))]
                 [(char=? c #\return)
                  (loop (cdr chars) (cons #\r (cons #\\ acc)))]
                 [(char=? c #\tab)
                  (loop (cdr chars) (cons #\t (cons #\\ acc)))]
                 [else
                  (loop (cdr chars) (cons c acc))])))))

;;; dot-escape-symbol : Symbol → String
;;; Escape a symbol for use in DOT output
(define (dot-escape-symbol sym)
  (doc 'export #t)
  (dot-escape-string (symbol->string sym)))

(define (fsm->dot fsm . name)
  (doc 'export #t)
  (doc 'type '(-> FSM (Option String) String))
  (doc 'description "Convert FSM to DOT format for Graphviz. All state names and labels are escaped to prevent DOT injection")
  (let ([graph-name (if (null? name) "FSM" (dot-escape-string (car name)))])
       (string-append
        "digraph \"" graph-name "\" {
"
        "  rankdir=LR;
"
        "  node [shape=circle];
"
        ;; Mark accepting states
        (fold-left (lambda (acc s)
                           (string-append acc "  \"" (dot-escape-symbol s) "\" [shape=doublecircle];
"))
                   ""
                   (fsm-accepting fsm))
        ;; Start arrow
        "  __start__ [shape=none,label=\"\"];
"
        "  __start__ -> \"" (dot-escape-symbol (fsm-start fsm)) "\";
"
        ;; Regular transitions
        (fold-left (lambda (acc t)
                           (let ([from (caar t)]
                                 [input (cdar t)]
                                 [tos (cdr t)])
                                (fold-left (lambda (acc2 to)
                                                   (string-append acc2
                                                                  "  \"" (dot-escape-symbol from)
                                                                  "\" -> \"" (dot-escape-symbol to)
                                                                  "\" [label=\""
                                                                  (dot-escape-string
                                                                   (if (char? input)
                                                                       (string input)
                                                                       (symbol->string input)))
                                                                  "\"];
"))
                                           acc
                                           tos)))
                   ""
                   (fsm-transitions fsm))
        ;; Epsilon transitions
        (fold-left (lambda (acc e)
                           (let ([from (car e)]
                                 [tos (cdr e)])
                                (fold-left (lambda (acc2 to)
                                                   (string-append acc2
                                                                  "  \"" (dot-escape-symbol from)
                                                                  "\" -> \"" (dot-escape-symbol to)
                                                                  "\" [label=\"\\u03b5\",style=dashed];
"))
                                           acc
                                           tos)))
                   ""
                   (fsm-epsilon fsm))
        "}
")))

(doc 'section 'fsm-to-string)

(define (fsm->string fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM String))
  (doc 'description "Convert FSM to human-readable string representation")
  (string-append
   "FSM:
"
   "  States: " (format "~a" (fsm-states fsm)) "
"
   "  Alphabet: " (format "~a" (fsm-alphabet fsm)) "
"
   "  Start: " (symbol->string (fsm-start fsm)) "
"
   "  Accepting: " (format "~a" (fsm-accepting fsm)) "
"
   "  Transitions:
"
   (fold-left (lambda (acc t)
                      (string-append acc "    "
                                     (format "~a" (car t))
                                     " -> "
                                     (format "~a" (cdr t))
                                     "
"))
              ""
              (fsm-transitions fsm))
   (if (null? (fsm-epsilon fsm))
       ""
       (string-append
        "  Epsilon:
"
        (fold-left (lambda (acc e)
                           (string-append acc "    "
                                          (symbol->string (car e))
                                          " -> "
                                          (format "~a" (cdr e))
                                          "
"))
                   ""
                   (fsm-epsilon fsm))))))

(doc 'section 'product-construction)

(define (fsm-intersect dfa1 dfa2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM FSM))
  (doc 'description "Intersection of two DFAs")
  (let* ([m1 (if (fsm-deterministic? dfa1) dfa1 (nfa->dfa dfa1))]
         [m2 (if (fsm-deterministic? dfa2) dfa2 (nfa->dfa dfa2))]
         [alphabet (filter (lambda (a) (member a (fsm-alphabet m2)))
                           (fsm-alphabet m1))]
         [pair-name (lambda (s1 s2)
                            (string->symbol
                             (string-append "(" (symbol->string s1) "," (symbol->string s2) ")")))]
         [start-pair (pair-name (fsm-start m1) (fsm-start m2))])
        (let loop ([worklist (list (cons (fsm-start m1) (fsm-start m2)))]
                   [visited '()]
                   [states (list start-pair)]
                   [trans '()]
                   [accepting '()])
             (if (null? worklist)
                 (make-fsm states alphabet trans start-pair accepting)
                 (let ([current (car worklist)])
                      (if (member current visited)
                          (loop (cdr worklist) visited states trans accepting)
                          (let* ([s1 (car current)]
                                 [s2 (cdr current)]
                                 [cur-name (pair-name s1 s2)]
                                 [is-accepting (and (member s1 (fsm-accepting m1))
                                                    (member s2 (fsm-accepting m2)))]
                                 [new-info
                                  (fold-left
                                   (lambda (acc sym)
                                           (let ([t1 (fsm-delta m1 s1 sym)]
                                                 [t2 (fsm-delta m2 s2 sym)])
                                                (if (or (null? t1) (null? t2))
                                                    acc
                                                    (let* ([next-pair (cons (car t1) (car t2))]
                                                           [next-name (pair-name (car t1) (car t2))])
                                                          (list (cons next-pair (car acc))
                                                                (cons next-name (cadr acc))
                                                                (cons (cons (cons cur-name sym) (list next-name))
                                                                      (caddr acc)))))))
                                   (list '() '() '())
                                   alphabet)]
                                 [new-pairs (car new-info)]
                                 [new-names (cadr new-info)]
                                 [new-trans (caddr new-info)])
                                (loop (append new-pairs (cdr worklist))
                                      (cons current visited)
                                      (union equal? new-names states)
                                      (append new-trans trans)
                                      (if is-accepting (cons cur-name accepting) accepting)))))))))

(define (fsm-complete dfa)
  ;; Helper: check if a transition exists and has non-empty targets
  (define (has-valid-transition? key transitions)
    (let ([found (assoc key transitions)])
         (and found (not (null? (cdr found))))))
  ;; Helper: remove transitions with empty target lists
  (define (filter-empty-transitions transitions)
    (filter (lambda (t) (not (null? (cdr t)))) transitions))
  (let* ([states (fsm-states dfa)]
         [alphabet (fsm-alphabet dfa)]
         [transitions (fsm-transitions dfa)]
         [dead-state '__dead__]
         ;; Check if we need a dead state (any missing or empty transitions)
         [missing-trans
          (fold-left
           (lambda (acc state)
                   (fold-left
                    (lambda (acc2 sym)
                            (let ([key (cons state sym)])
                                 (if (has-valid-transition? key transitions)
                                     acc2
                                     (cons (cons key (list dead-state)) acc2))))
                    acc
                    alphabet))
           '()
           states)]
         ;; Add self-loops for dead state on all symbols
         [dead-loops
          (map (lambda (sym) (cons (cons dead-state sym) (list dead-state)))
               alphabet)]
         ;; Filter out empty transitions from original
         [clean-transitions (filter-empty-transitions transitions)])
        (if (null? missing-trans)
            ;; DFA is already complete
            dfa
            ;; Add dead state and all missing transitions
            (make-fsm (cons dead-state states)
                      alphabet
                      (append missing-trans dead-loops clean-transitions)
                      (fsm-start dfa)
                      (fsm-accepting dfa)))))
(doc fsm-complete 'type '(-> FSM FSM))
(doc fsm-complete 'description "Make a DFA complete by adding explicit dead state with self-loops for all missing transitions")

(define (fsm-complement dfa)
  (doc 'export #t)
  (let* ([m0 (if (fsm-deterministic? dfa) dfa (nfa->dfa dfa))]
         [m (fsm-complete m0)]
         [non-accepting (filter (lambda (s) (not (member s (fsm-accepting m))))
                                (fsm-states m))])
        (make-fsm (fsm-states m) (fsm-alphabet m) (fsm-transitions m)
                  (fsm-start m) non-accepting)))
(doc fsm-complement 'type '(-> FSM FSM))
(doc fsm-complement 'description "Complement of a DFA. First completes the DFA (adds explicit dead state for missing transitions), then flips accepting and non-accepting states")

(doc 'section 'fsm-derived-language-operations)

(define (fsm-difference fsm1 fsm2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM FSM))
  (doc 'description "Difference of two FSMs: L(M1) \\ L(M2). Accepts strings in M1 but not M2.")
  (fsm-intersect fsm1 (fsm-complement fsm2)))

(define (fsm-subset? fsm1 fsm2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM Boolean))
  (doc 'description "Check if L(M1) ⊆ L(M2). True when every string accepted by M1 is also accepted by M2.")
  (fsm-empty? (fsm-difference fsm1 fsm2)))

(define (fsm-equivalent? fsm1 fsm2)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM Boolean))
  (doc 'description "Check if L(M1) = L(M2). True when both FSMs accept exactly the same language.")
  (and (fsm-subset? fsm1 fsm2)
       (fsm-subset? fsm2 fsm1)))

(define (fsm-reverse fsm)
  (doc 'export #t)
  (doc 'type '(-> FSM FSM))
  (doc 'description "Reverse of an FSM: accepts the reverse of every string in L(M). Reverses all transitions, swaps start and accepting states.")
  (let* ([m (fsm-rename fsm "r")]
         [new-start (fsm-fresh-state "rev")]
         ;; Reverse all regular transitions: (from . sym) -> (to) becomes (to . sym) -> (from)
         [rev-trans
          (fold-left
           (lambda (acc t)
             (let ([from (caar t)]
                   [sym (cdar t)]
                   [targets (cdr t)])
               (fold-left
                (lambda (acc2 to)
                  (let ([key (cons to sym)]
                        [existing (assoc (cons to sym) acc2)])
                    (if existing
                        ;; Append to existing transition
                        (map (lambda (entry)
                               (if (equal? (car entry) key)
                                   (cons key (union equal? (list from) (cdr entry)))
                                   entry))
                             acc2)
                        (cons (cons key (list from)) acc2))))
                acc
                targets)))
           '()
           (fsm-transitions m))]
         ;; Reverse epsilon transitions similarly
         [rev-eps
          (fold-left
           (lambda (acc e)
             (let ([from (car e)]
                   [targets (cdr e)])
               (fold-left
                (lambda (acc2 to)
                  (let ([existing (assoc to acc2)])
                    (if existing
                        (map (lambda (entry)
                               (if (equal? (car entry) to)
                                   (cons to (union equal? (list from) (cdr entry)))
                                   entry))
                             acc2)
                        (cons (cons to (list from)) acc2))))
                acc
                targets)))
           '()
           (fsm-epsilon m))]
         ;; New start epsilon-transitions to all old accepting states
         [start-eps (cons new-start (fsm-accepting m))]
         ;; Old start becomes the only accepting state
         [all-states (cons new-start (fsm-states m))]
         [all-eps (cons start-eps rev-eps)])
    (make-fsm all-states (fsm-alphabet m) rev-trans
              new-start (list (fsm-start m)) all-eps)))

(doc 'section 'state-machine-simulation)

(define (make-moore fsm outputs)
  (doc 'export #t)
  (doc 'type '(-> FSM (Alist State α) Moore))
  (doc 'description "Moore machine: output depends on current state")
  (list 'moore fsm outputs))

(define (moore? x)
  (doc 'export #t)
  (doc 'type '(-> β Boolean))
  (and (list? x) (>= (length x) 3) (eq? (car x) 'moore)))

(define (moore-fsm m)
  (doc 'export #t)
  (doc 'type '(-> Moore FSM))
  (cadr m))

(define (moore-outputs m)
  (doc 'export #t)
  (doc 'type '(-> Moore (Alist State α)))
  (caddr m))

(define (moore-run machine input)
  (doc 'export #t)
  (doc 'type '(-> Moore (Union String (List Symbol)) (List α)))
  (doc 'description "Run Moore machine, returning sequence of outputs")
  (let* ([fsm (moore-fsm machine)]
         [outputs (moore-outputs machine)]
         [input-list (if (string? input) (string->list input) input)]
         [get-output (lambda (state)
                             (let ([found (assoc state outputs)])
                                  (if found (cdr found) #f)))])
        (let loop ([states (epsilon-closure fsm (fsm-start fsm))]
                   [inputs input-list]
                   [result (list (get-output (fsm-start fsm)))])
             (if (null? inputs)
                 (reverse (filter identity result))
                 (let* ([new-states (fsm-move fsm states (car inputs))]
                        [state-output (if (null? new-states)
                                          #f
                                          (get-output (car new-states)))])
                       (loop new-states (cdr inputs) (cons state-output result)))))))

(define (make-mealy fsm trans-outputs)
  (doc 'export #t)
  (doc 'type '(-> FSM (Alist (Pair State Symbol) α) Mealy))
  (doc 'description "Mealy machine: output depends on transition")
  (list 'mealy fsm trans-outputs))

(define (mealy? x)
  (doc 'export #t)
  (doc 'type '(-> β Boolean))
  (and (list? x) (>= (length x) 3) (eq? (car x) 'mealy)))

(define (mealy-fsm m)
  (doc 'export #t)
  (doc 'type '(-> Mealy FSM))
  (cadr m))

(define (mealy-outputs m)
  (doc 'export #t)
  (doc 'type '(-> Mealy (Alist (Pair State Symbol) α)))
  (caddr m))

(define (mealy-run machine input)
  (doc 'export #t)
  (doc 'type '(-> Mealy (Union String (List Symbol)) (List α)))
  (doc 'description "Run Mealy machine, returning sequence of outputs")
  (let* ([fsm (mealy-fsm machine)]
         [outputs (mealy-outputs machine)]
         [input-list (if (string? input) (string->list input) input)]
         [get-output (lambda (state sym)
                             (let ([found (assoc (cons state sym) outputs)])
                                  (if found (cdr found) #f)))])
        (let loop ([states (epsilon-closure fsm (fsm-start fsm))]
                   [inputs input-list]
                   [result '()])
             (if (null? inputs)
                 (reverse (filter identity result))
                 (let* ([sym (car inputs)]
                        [output (if (null? states)
                                    #f
                                    (get-output (car states) sym))]
                        [new-states (fsm-move fsm states sym)])
                       (loop new-states (cdr inputs) (cons output result)))))))

;;; ====
;;; Exports Summary
;;; ====

;;; Types:
;;;   make-fsm, fsm?, fsm-states, fsm-alphabet, fsm-transitions,
;;;   fsm-start, fsm-accepting, fsm-epsilon, fsm-deterministic?
;;;
;;; Construction:
;;;   dfa, nfa, epsilon-nfa, fsm-char, fsm-epsilon-lang,
;;;   fsm-any-of, fsm-literal
;;;
;;; Execution:
;;;   fsm-run, fsm-accepts?, fsm-delta, epsilon-closure
;;;
;;; Operations:
;;;   fsm-union, fsm-concat, fsm-star, fsm-plus, fsm-optional,
;;;   fsm-intersect, fsm-complement, fsm-difference, fsm-reverse,
;;;   nfa->dfa, fsm-minimize
;;;
;;; Queries:
;;;   fsm-reachable, fsm-empty?, fsm-subset?, fsm-equivalent?
;;;
;;; Moore/Mealy:
;;;   make-moore, moore?, moore-run, make-mealy, mealy?, mealy-run
;;;
;;; Visualization:
;;;   fsm->dot, fsm->string
