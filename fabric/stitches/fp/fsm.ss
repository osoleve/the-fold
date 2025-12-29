;;; fabric/stitches/fp/fsm.ss — Finite State Machine Library
;;;
;;; A pure functional implementation of finite state machines supporting
;;; deterministic (DFA), non-deterministic (NFA), and epsilon-NFA automata.
;;;
;;; Features:
;;; - DFA/NFA/ε-NFA construction
;;; - State machine execution
;;; - NFA to DFA conversion (subset construction)
;;; - ε-NFA to NFA conversion
;;; - State machine composition (union, concat, kleene)
;;; - Minimization (Hopcroft's algorithm)
;;; - Language operations
;;;
;;; This module builds on combinators.ss for Maybe type and basic combinators.

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Finite State Machine Types
;;; ============================================================

;;; FSM = (fsm states alphabet transitions start accepting epsilon-transitions)
;;; - states: List of state symbols
;;; - alphabet: List of input symbols
;;; - transitions: Alist ((state . input) -> List state)
;;; - start: Initial state
;;; - accepting: List of accepting states
;;; - epsilon-transitions: Alist (state -> List state) for ε-moves

(define (make-fsm states alphabet transitions start accepting . epsilon)
  (list 'fsm states alphabet transitions start accepting
        (if (null? epsilon) '() (car epsilon))))

(define (fsm? x)
  (and (list? x) (= (length x) 7) (eq? (car x) 'fsm)))

(define (fsm-states fsm) (list-ref fsm 1))
(define (fsm-alphabet fsm) (list-ref fsm 2))
(define (fsm-transitions fsm) (list-ref fsm 3))
(define (fsm-start fsm) (list-ref fsm 4))
(define (fsm-accepting fsm) (list-ref fsm 5))
(define (fsm-epsilon fsm) (list-ref fsm 6))

;;; Check if FSM is deterministic (no epsilon, single transitions)
(define (fsm-deterministic? fsm)
  (and (null? (fsm-epsilon fsm))
       (let ([trans (fsm-transitions fsm)])
            (for-all (lambda (t) (<= (length (cdr t)) 1)) trans))))

;;; ============================================================
;;; FSM Construction Helpers
;;; ============================================================

;;; Create a simple DFA from explicit parts
(define (dfa states alphabet transitions start accepting)
  (make-fsm states alphabet
            (map (lambda (t)
                         (cons (cons (car t) (cadr t)) (list (caddr t))))
                 transitions)
            start accepting))

;;; Create an NFA with possible multiple transitions
(define (nfa states alphabet transitions start accepting)
  (make-fsm states alphabet transitions start accepting))

;;; Create an ε-NFA
(define (epsilon-nfa states alphabet transitions start accepting epsilon-trans)
  (make-fsm states alphabet transitions start accepting epsilon-trans))

;;; ============================================================
;;; FSM Execution
;;; ============================================================

;;; Get transition targets from state on input
(define (fsm-delta fsm state input)
  (let ([key (cons state input)])
       (let ([found (assoc key (fsm-transitions fsm))])
            (if found (cdr found) '()))))

;;; Get epsilon-closure of a state
;;; Get all epsilon targets from a state (handles multiple entries for same state)
(define (get-all-epsilon-targets fsm state)
  (fold-left (lambda (acc entry)
                     (if (equal? (car entry) state)
                         (append (cdr entry) acc)
                         acc))
             '()
             (fsm-epsilon fsm)))

(define (epsilon-closure fsm state)
  (let loop ([frontier (list state)] [visited '()])
       (if (null? frontier)
           visited
           (let ([s (car frontier)])
                (if (member s visited)
                    (loop (cdr frontier) visited)
                    (let* ([eps-targets (get-all-epsilon-targets fsm s)]
                           [new-frontier (append eps-targets (cdr frontier))])
                          (loop new-frontier (cons s visited))))))))

;;; Get epsilon-closure of a set of states
(define (epsilon-closure-set fsm states)
  (fold-left (lambda (acc s)
                     (union equal? acc (epsilon-closure fsm s)))
             '()
             states))

;;; Helper: set union
(define (union eq? xs ys)
  (fold-left (lambda (acc x)
                     (if (exists (lambda (y) (eq? x y)) acc) acc (cons x acc)))
             ys xs))

;;; Move from a set of states on input, then epsilon-close
(define (fsm-move fsm states input)
  (let* ([direct-targets
          (fold-left (lambda (acc s)
                             (union equal? acc (fsm-delta fsm s input)))
                     '()
                     states)])
        (epsilon-closure-set fsm direct-targets)))

;;; Run FSM on input sequence
;;; Returns: Just final-states if any accepting, Nothing otherwise
(define (fsm-run fsm input)
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

;;; Check if FSM accepts input
(define (fsm-accepts? fsm input)
  (just? (fsm-run fsm input)))

;;; ============================================================
;;; FSM Language Operations
;;; ============================================================

;;; Get reachable states from start
(define (fsm-reachable fsm)
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

;;; Check if FSM language is empty
(define (fsm-empty? fsm)
  (let ([reachable (fsm-reachable fsm)])
       (not (exists (lambda (s) (member s (fsm-accepting fsm))) reachable))))

;;; ============================================================
;;; NFA to DFA Conversion (Subset Construction)
;;; ============================================================

;;; Generate fresh state name from state set
(define (state-set->name states)
  (string->symbol
   (string-append "{"
                  (fold-left (lambda (acc s)
                                     (if (string=? acc "")
                                         (symbol->string s)
                                         (string-append acc "," (symbol->string s))))
                             ""
                             (list-sort symbol<? states))
                  "}")))

(define (symbol<? a b)
  (string<? (symbol->string a) (symbol->string b)))

;;; Convert NFA to DFA using subset construction
(define (nfa->dfa nfa)
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
                                          dfa-accepting)))))))))

;;; ============================================================
;;; FSM Composition
;;; ============================================================

;;; Generate fresh state names
(define *fsm-counter* 0)
(define (fsm-fresh-state prefix)
  (set! *fsm-counter* (+ *fsm-counter* 1))
  (string->symbol (string-append prefix (number->string *fsm-counter*))))

;;; Rename states in FSM to avoid collisions
(define (fsm-rename fsm prefix)
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
                       (fsm-epsilon fsm))])
        (make-fsm new-states (fsm-alphabet fsm) new-trans new-start new-accepting new-eps)))

;;; Union of two FSMs (accepts if either accepts)
(define (fsm-union fsm1 fsm2)
  (let* ([m1 (fsm-rename fsm1 "a")]
         [m2 (fsm-rename fsm2 "b")]
         [new-start (fsm-fresh-state "u")]
         [all-states (cons new-start
                           (append (fsm-states m1) (fsm-states m2)))]
         [all-alphabet (union equal? (fsm-alphabet m1) (fsm-alphabet m2))]
         [all-trans (append (fsm-transitions m1) (fsm-transitions m2))]
         [all-accepting (append (fsm-accepting m1) (fsm-accepting m2))]
         [new-eps (cons (cons new-start (list (fsm-start m1) (fsm-start m2)))
                        (append (fsm-epsilon m1) (fsm-epsilon m2)))])
        (make-fsm all-states all-alphabet all-trans new-start all-accepting new-eps)))

;;; Concatenation of two FSMs (accepts m1 then m2)
(define (fsm-concat fsm1 fsm2)
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
                          (fsm-epsilon m2))])
        (make-fsm all-states all-alphabet all-trans
                  (fsm-start m1) (fsm-accepting m2) all-eps)))

;;; Kleene star of FSM (zero or more repetitions)
(define (fsm-star fsm)
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
        (make-fsm all-states (fsm-alphabet m) (fsm-transitions m)
                  new-start all-accepting all-eps)))

;;; Kleene plus (one or more repetitions)
(define (fsm-plus fsm)
  (fsm-concat fsm (fsm-star fsm)))

;;; Optional (zero or one)
(define (fsm-optional fsm)
  (let* ([m (fsm-rename fsm "o")]
         [new-start (fsm-fresh-state "opt")]
         [all-states (cons new-start (fsm-states m))]
         [start-eps (cons new-start (list (fsm-start m)))]
         [all-eps (cons start-eps (fsm-epsilon m))]
         ;; New start is accepting
         [all-accepting (cons new-start (fsm-accepting m))])
        (make-fsm all-states (fsm-alphabet m) (fsm-transitions m)
                  new-start all-accepting all-eps)))

;;; ============================================================
;;; FSM Builders
;;; ============================================================

;;; FSM that accepts exactly one character
(define (fsm-char c)
  (let ([s0 (fsm-fresh-state "q")]
        [s1 (fsm-fresh-state "q")])
       (make-fsm (list s0 s1)
                 (list c)
                 (list (cons (cons s0 c) (list s1)))
                 s0
                 (list s1))))

;;; FSM that accepts empty string only
(define (fsm-epsilon-lang)
  (let ([s0 (fsm-fresh-state "e")])
       (make-fsm (list s0) '() '() s0 (list s0))))

;;; FSM that accepts any single character from list
(define (fsm-any-of chars)
  (if (null? chars)
      (make-fsm (list 'empty) '() '() 'empty '())  ; Empty language
      (fold-left fsm-union
                 (fsm-char (car chars))
                 (map fsm-char (cdr chars)))))

;;; FSM that accepts a literal string
(define (fsm-literal str)
  (let ([chars (string->list str)])
       (if (null? chars)
           (fsm-epsilon-lang)
           (fold-left fsm-concat
                      (fsm-char (car chars))
                      (map fsm-char (cdr chars))))))

;;; ============================================================
;;; FSM Minimization (Hopcroft's Algorithm)
;;; ============================================================

;;; Minimize a DFA using partition refinement
(define (fsm-minimize dfa)
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
                          (refine new-partition)))))))

;;; Refine partition by checking transitions
(define (refine-partition dfa partition alphabet)
  (fold-left (lambda (p block)
                     (append (split-block dfa block partition alphabet)
                             (filter (lambda (b) (not (equal? b block))) p)))
             '()
             partition))

;;; Split a block if states have different transition targets
(define (split-block dfa block partition alphabet)
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

;;; Check if two states are equivalent (same behavior)
(define (states-equivalent? dfa s1 s2 partition alphabet)
  (for-all (lambda (sym)
                   (let ([t1 (fsm-delta dfa s1 sym)]
                         [t2 (fsm-delta dfa s2 sym)])
                        (cond
                         [(and (null? t1) (null? t2)) #t]
                         [(or (null? t1) (null? t2)) #f]
                         [else (same-partition-block? (car t1) (car t2) partition)])))
           alphabet))

;;; Check if two states are in the same partition block
(define (same-partition-block? s1 s2 partition)
  (exists (lambda (block)
                  (and (member s1 block) (member s2 block)))
          partition))

;;; Build minimized DFA from partition
(define (build-minimized-dfa dfa partition)
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

;;; Helper: filter and map
(define (filter-map f lst)
  (fold-right (lambda (x acc)
                      (let ([result (f x)])
                           (if result (cons result acc) acc)))
              '()
              lst))

;;; ============================================================
;;; FSM Visualization
;;; ============================================================

;;; Convert FSM to DOT format for Graphviz
(define (fsm->dot fsm . name)
  (let ([graph-name (if (null? name) "FSM" (car name))])
       (string-append
        "digraph " graph-name " {
"
        "  rankdir=LR;
"
        "  node [shape=circle];
"
        ;; Mark accepting states
        (fold-left (lambda (acc s)
                           (string-append acc "  " (symbol->string s) " [shape=doublecircle];
"))
                   ""
                   (fsm-accepting fsm))
        ;; Start arrow
        "  __start__ [shape=none,label=\"\"];
"
        "  __start__ -> " (symbol->string (fsm-start fsm)) ";
"
        ;; Regular transitions
        (fold-left (lambda (acc t)
                           (let ([from (caar t)]
                                 [input (cdar t)]
                                 [tos (cdr t)])
                                (fold-left (lambda (acc2 to)
                                                   (string-append acc2
                                                                  "  " (symbol->string from)
                                                                  " -> " (symbol->string to)
                                                                  " [label=\""
                                                                  (if (char? input)
                                                                      (string input)
                                                                      (symbol->string input))
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
                                                                  "  " (symbol->string from)
                                                                  " -> " (symbol->string to)
                                                                  " [label=\"ε\",style=dashed];
"))
                                           acc
                                           tos)))
                   ""
                   (fsm-epsilon fsm))
        "}
")))

;;; ============================================================
;;; FSM to String
;;; ============================================================

(define (fsm->string fsm)
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

;;; ============================================================
;;; Product Construction (Intersection)
;;; ============================================================

;;; Intersection of two DFAs
(define (fsm-intersect dfa1 dfa2)
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

;;; Complement of a DFA (requires complete DFA)
(define (fsm-complement dfa)
  (let* ([m (if (fsm-deterministic? dfa) dfa (nfa->dfa dfa))]
         [non-accepting (filter (lambda (s) (not (member s (fsm-accepting m))))
                                (fsm-states m))])
        (make-fsm (fsm-states m) (fsm-alphabet m) (fsm-transitions m)
                  (fsm-start m) non-accepting)))

;;; ============================================================
;;; State Machine Simulation with Actions
;;; ============================================================

;;; Moore machine: output depends on current state
;;; (moore-machine fsm outputs) where outputs: alist (state -> output)
(define (make-moore fsm outputs)
  (list 'moore fsm outputs))

(define (moore? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'moore)))

(define (moore-fsm m) (cadr m))
(define (moore-outputs m) (caddr m))

;;; Run Moore machine, returning sequence of outputs
(define (moore-run machine input)
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

;;; Mealy machine: output depends on transition
;;; (mealy-machine fsm transition-outputs) where transition-outputs: alist ((state . input) -> output)
(define (make-mealy fsm trans-outputs)
  (list 'mealy fsm trans-outputs))

(define (mealy? x)
  (and (list? x) (>= (length x) 3) (eq? (car x) 'mealy)))

(define (mealy-fsm m) (cadr m))
(define (mealy-outputs m) (caddr m))

;;; Run Mealy machine, returning sequence of outputs
(define (mealy-run machine input)
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

;;; ============================================================
;;; Exports Summary
;;; ============================================================

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
;;;   fsm-intersect, fsm-complement, nfa->dfa, fsm-minimize
;;;
;;; Queries:
;;;   fsm-reachable, fsm-empty?
;;;
;;; Moore/Mealy:
;;;   make-moore, moore?, moore-run, make-mealy, mealy?, mealy-run
;;;
;;; Visualization:
;;;   fsm->dot, fsm->string
