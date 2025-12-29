;;; fabric/stitches/fp/alternative.ss — Alternative and MonadPlus
;;;
;;; Alternative extends Applicative with choice and failure.
;;; MonadPlus extends Monad with the same operations.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Alternative: empty and choice (<|>)
;;;   - MonadPlus: mzero and mplus
;;;   - Derived operations: some, many, optional, guard
;;;   - Instances for Maybe, List, Parser
;;;   - Utility combinators for search and backtracking
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Laws:
;;;   empty <|> x = x                    (left identity)
;;;   x <|> empty = x                    (right identity)
;;;   (x <|> y) <|> z = x <|> (y <|> z)  (associativity)
;;;
;;; MonadPlus Laws:
;;;   mzero >>= f = mzero                (left zero)
;;;   m >>= \_ -> mzero = mzero          (right zero) [for some interpretations]

(load "prelude.ss")
(load "fp/combinators.ss")

;;; ============================================================
;;; Alternative Type Class
;;; ============================================================
;;;
;;; Alternative f requires:
;;;   - empty : f a           (identity for <|>)
;;;   - alt   : f a -> f a -> f a  (choice operator)
;;;   - plus Applicative operations (pure, ap)

;;; make-alternative : (() -> f a) -> (f a -> f a -> f a) -> (a -> f a) -> (f (a->b) -> f a -> f b) -> Alternative f
(define (make-alternative empty-fn alt-fn pure-fn ap-fn)
  (list 'alternative empty-fn alt-fn pure-fn ap-fn))

;;; alternative? : Any -> Boolean
(define (alternative? x)
  (and (pair? x) (eq? (car x) 'alternative)))

;;; alt-empty : Alternative f -> f a
(define (alt-empty alt)
  ((list-ref alt 1)))

;;; alt-or : Alternative f -> f a -> f a -> f a
;;; The <|> operator.
(define (alt-or alt fa fb)
  ((list-ref alt 2) fa fb))

;;; alt-pure : Alternative f -> a -> f a
(define (alt-pure alt a)
  ((list-ref alt 3) a))

;;; alt-ap : Alternative f -> f (a -> b) -> f a -> f b
(define (alt-ap alt ff fa)
  ((list-ref alt 4) ff fa))

;;; ============================================================
;;; MonadPlus Type Class
;;; ============================================================
;;;
;;; MonadPlus m requires:
;;;   - mzero  : m a           (identity for mplus)
;;;   - mplus  : m a -> m a -> m a  (choice operator)
;;;   - plus Monad operations (return, bind)

;;; make-monad-plus : (() -> m a) -> (m a -> m a -> m a) -> (a -> m a) -> (m a -> (a -> m b) -> m b) -> MonadPlus m
(define (make-monad-plus mzero-fn mplus-fn return-fn bind-fn)
  (list 'monad-plus mzero-fn mplus-fn return-fn bind-fn))

;;; monad-plus? : Any -> Boolean
(define (monad-plus? x)
  (and (pair? x) (eq? (car x) 'monad-plus)))

;;; mp-zero : MonadPlus m -> m a
(define (mp-zero mp)
  ((list-ref mp 1)))

;;; mp-plus : MonadPlus m -> m a -> m a -> m a
(define (mp-plus mp ma mb)
  ((list-ref mp 2) ma mb))

;;; mp-return : MonadPlus m -> a -> m a
(define (mp-return mp a)
  ((list-ref mp 3) a))

;;; mp-bind : MonadPlus m -> m a -> (a -> m b) -> m b
(define (mp-bind mp ma f)
  ((list-ref mp 4) ma f))

;;; ============================================================
;;; Maybe Alternative Instance
;;; ============================================================

;;; maybe-empty : Maybe a
(define maybe-empty nothing)

;;; maybe-alt : Maybe a -> Maybe a -> Maybe a
;;; Returns first Just, or Nothing if both are Nothing.
(define (maybe-alt ma mb)
  (if (just? ma) ma mb))

;;; maybe-alternative : Alternative Maybe
(define maybe-alternative
  (make-alternative
   (lambda () nothing)
   maybe-alt
   just
   (lambda (mf ma)
     (if (nothing? mf)
         nothing
         (if (nothing? ma)
             nothing
             (just ((from-just mf) (from-just ma))))))))

;;; maybe-monad-plus : MonadPlus Maybe
(define maybe-monad-plus
  (make-monad-plus
   (lambda () nothing)
   maybe-alt
   just
   (lambda (ma f)
     (if (nothing? ma)
         nothing
         (f (from-just ma))))))

;;; ============================================================
;;; List Alternative Instance
;;; ============================================================

;;; list-empty : List a
(define list-empty '())

;;; list-alt : List a -> List a -> List a
;;; Concatenates the lists (non-deterministic choice).
(define (list-alt la lb)
  (append la lb))

;;; list-alternative : Alternative List
(define list-alternative
  (make-alternative
   (lambda () '())
   list-alt
   list
   (lambda (lf la)
     (apply append (map (lambda (f) (map f la)) lf)))))

;;; list-monad-plus : MonadPlus List
(define list-monad-plus
  (make-monad-plus
   (lambda () '())
   list-alt
   list
   (lambda (la f)
     (apply append (map f la)))))

;;; ============================================================
;;; Either Alternative Instance
;;; ============================================================

;;; either-alt : Either e a -> Either e a -> Either e a
;;; Returns first Right, or last Left if both are Left.
(define (either-alt ea eb)
  (if (right? ea) ea eb))

;;; either-alternative : e -> Alternative (Either e)
;;; Needs a default error value for empty.
(define (either-alternative default-error)
  (make-alternative
   (lambda () (left default-error))
   either-alt
   right
   (lambda (ef ea)
     (if (left? ef)
         ef
         (if (left? ea)
             ea
             (right ((from-right ef) (from-right ea))))))))

;;; ============================================================
;;; Derived Combinators
;;; ============================================================

;;; asum : Alternative f -> List (f a) -> f a
;;; Fold alternatives together.
(define (asum alt fas)
  (foldr (lambda (fa acc) (alt-or alt fa acc))
         (alt-empty alt)
         fas))

;;; msum : MonadPlus m -> List (m a) -> m a
;;; Fold monad-plus values together.
(define (msum mp mas)
  (foldr (lambda (ma acc) (mp-plus mp ma acc))
         (mp-zero mp)
         mas))

;;; guard : Alternative f -> Bool -> f ()
;;; Succeed with unit if true, fail otherwise.
(define (guard alt pred)
  (if pred
      (alt-pure alt '())
      (alt-empty alt)))

;;; guard-mp : MonadPlus m -> Bool -> m ()
(define (guard-mp mp pred)
  (if pred
      (mp-return mp '())
      (mp-zero mp)))

;;; optional : Alternative f -> f a -> f (Maybe a)
;;; Try the action, return Just on success, Nothing on failure.
(define (optional alt fa)
  (alt-or alt
          (alt-ap alt (alt-pure alt just) fa)
          (alt-pure alt nothing)))

;;; ============================================================
;;; Many and Some (with fuel for totality)
;;; ============================================================
;;;
;;; many : Alternative f -> f a -> Int -> f (List a)
;;; Zero or more, with fuel limit.
;;;
;;; some : Alternative f -> f a -> Int -> f (List a)
;;; One or more, with fuel limit.

;;; many-fuel : Alternative f -> f a -> Int -> f (List a)
;;; Zero or more occurrences (greedy, with fuel).
(define (many-fuel alt fa fuel)
  (if (<= fuel 0)
      (alt-pure alt '())
      (alt-or alt
              (some-fuel alt fa fuel)
              (alt-pure alt '()))))

;;; some-fuel : Alternative f -> f a -> Int -> f (List a)
;;; One or more occurrences (greedy, with fuel).
(define (some-fuel alt fa fuel)
  (if (<= fuel 0)
      (alt-empty alt)
      (alt-ap alt
              (alt-ap alt
                      (alt-pure alt (lambda (x) (lambda (xs) (cons x xs))))
                      fa)
              (many-fuel alt fa (- fuel 1)))))

;;; ============================================================
;;; Choice Combinators
;;; ============================================================

;;; choice : Alternative f -> List (f a) -> f a
;;; Same as asum, try each alternative in order.
(define choice asum)

;;; try-all : MonadPlus m -> (a -> m b) -> List a -> m b
;;; Try applying f to each element until one succeeds.
(define (try-all mp f lst)
  (msum mp (map f lst)))

;;; first-match : MonadPlus m -> (a -> Bool) -> List a -> m a
;;; Return first element matching predicate.
(define (first-match mp pred lst)
  (try-all mp
           (lambda (x) (if (pred x) (mp-return mp x) (mp-zero mp)))
           lst))

;;; filter-m : MonadPlus m -> (a -> m Bool) -> List a -> m (List a)
;;; Monadic filter.
(define (filter-m mp pred lst)
  (if (null? lst)
      (mp-return mp '())
      (mp-bind mp (pred (car lst))
               (lambda (keep?)
                 (mp-bind mp (filter-m mp pred (cdr lst))
                          (lambda (rest)
                            (mp-return mp (if keep?
                                              (cons (car lst) rest)
                                              rest))))))))

;;; ============================================================
;;; Non-deterministic Search
;;; ============================================================

;;; amb : MonadPlus m -> a -> a -> m a
;;; Binary non-deterministic choice.
(define (amb mp a b)
  (mp-plus mp (mp-return mp a) (mp-return mp b)))

;;; amb-list : MonadPlus m -> List a -> m a
;;; Choose non-deterministically from a list.
(define (amb-list mp lst)
  (msum mp (map (lambda (x) (mp-return mp x)) lst)))

;;; require : MonadPlus m -> Bool -> m ()
;;; Require condition, fail if false.
(define (require mp pred)
  (guard-mp mp pred))

;;; ============================================================
;;; Interleaving
;;; ============================================================

;;; interleave : List a -> List a -> List a
;;; Interleave two lists fairly.
(define (interleave la lb)
  (cond
    [(null? la) lb]
    [(null? lb) la]
    [else (cons (car la) (interleave lb (cdr la)))]))

;;; interleave-m : List a -> List b -> List (Pair a b)
;;; All combinations, interleaved fairly.
(define (interleave-m la lb)
  (if (null? la)
      '()
      (interleave
       (map (lambda (b) (cons (car la) b)) lb)
       (interleave-m (cdr la) lb))))

;;; ============================================================
;;; Logic Programming Primitives
;;; ============================================================

;;; conde : MonadPlus m -> List (m a) -> m a
;;; Like Prolog's disjunction.
(define (conde mp clauses)
  (msum mp clauses))

;;; conj : MonadPlus m -> List (m ()) -> m ()
;;; Like Prolog's conjunction.
(define (conj mp goals)
  (if (null? goals)
      (mp-return mp '())
      (mp-bind mp (car goals)
               (lambda (_)
                 (conj mp (cdr goals))))))

;;; fresh : MonadPlus m -> (a -> m b) -> m a -> m b
;;; Introduce a fresh variable (really just bind).
(define (fresh mp gen body)
  (mp-bind mp gen body))

;;; ============================================================
;;; Collecting Results
;;; ============================================================

;;; collect-all : List a  (MonadPlus List results)
;;; For the list monad, just returns all solutions.
(define (collect-all solutions) solutions)

;;; collect-first : List a -> Maybe a
;;; Get first solution.
(define (collect-first solutions)
  (if (null? solutions) nothing (just (car solutions))))

;;; collect-n : Int -> List a -> List a
;;; Get first n solutions.
(define (collect-n n solutions)
  (if (or (= n 0) (null? solutions))
      '()
      (cons (car solutions) (collect-n (- n 1) (cdr solutions)))))

;;; ============================================================
;;; Backtracking Monad (explicit)
;;; ============================================================

;;; The list monad is already a backtracking monad.
;;; These are convenience wrappers.

;;; bt-return : a -> List a
(define bt-return list)

;;; bt-bind : List a -> (a -> List b) -> List b
(define (bt-bind ma f)
  (apply append (map f ma)))

;;; bt-fail : List a
(define bt-fail '())

;;; bt-choice : List a -> List a -> List a
(define bt-choice append)

;;; bt-guard : Bool -> List ()
(define (bt-guard pred)
  (if pred '(()) '()))

;;; bt-run : List a -> List a
(define bt-run identity)

;;; ============================================================
;;; Cut (Pruning)
;;; ============================================================
;;;
;;; Cut is tricky in pure FP. We simulate it with once.

;;; once : List a -> List a
;;; Commit to first solution (like Prolog cut).
(define (once solutions)
  (if (null? solutions)
      '()
      (list (car solutions))))

;;; if-then-else : List a -> (a -> List b) -> List b -> List b
;;; Soft cut: if first succeeds, commit to it.
(define (if-then-else condition then-branch else-branch)
  (let ([solutions (bt-run condition)])
    (if (null? solutions)
        else-branch
        (bt-bind solutions then-branch))))

;;; ============================================================
;;; Utility Functions
;;; ============================================================

;;; between : Int -> Int -> List Int
;;; Generate range as list (for list monad).
(define (between lo hi)
  (if (> lo hi)
      '()
      (cons lo (between (+ lo 1) hi))))

;;; permutations : List a -> List (List a)
;;; All permutations of a list.
(define (permutations lst)
  (if (null? lst)
      '(())
      (bt-bind (amb-list list-monad-plus lst)
               (lambda (x)
                 (bt-bind (permutations (remove-first x lst))
                          (lambda (rest)
                            (bt-return (cons x rest))))))))

;;; remove-first : a -> List a -> List a
;;; Remove first occurrence.
(define (remove-first x lst)
  (cond
    [(null? lst) '()]
    [(equal? x (car lst)) (cdr lst)]
    [else (cons (car lst) (remove-first x (cdr lst)))]))

;;; subsets : List a -> List (List a)
;;; All subsets of a list.
(define (subsets lst)
  (if (null? lst)
      '(())
      (let ([rest (subsets (cdr lst))])
        (append rest
                (map (lambda (s) (cons (car lst) s)) rest)))))

;;; ============================================================
;;; Example: N-Queens
;;; ============================================================

;;; safe? : Int -> List Int -> Bool
;;; Check if placing a queen at column col is safe given previous placements.
(define (safe? col placements)
  (safe-helper? col 1 placements))

(define (safe-helper? col dist placements)
  (or (null? placements)
      (let ([other (car placements)])
        (and (not (= col other))
             (not (= col (+ other dist)))
             (not (= col (- other dist)))
             (safe-helper? col (+ dist 1) (cdr placements))))))

;;; n-queens : Int -> List (List Int)
;;; Solve N-Queens problem.
(define (n-queens n)
  (n-queens-helper n n '()))

(define (n-queens-helper n remaining placements)
  (if (= remaining 0)
      (bt-return placements)
      (bt-bind (between 1 n)
               (lambda (col)
                 (bt-bind (bt-guard (safe? col placements))
                          (lambda (_)
                            (n-queens-helper n (- remaining 1)
                                             (cons col placements))))))))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Maybe as Alternative
;;; (alt-or maybe-alternative nothing (just 42))  ; => (just 42)
;;; (alt-or maybe-alternative (just 1) (just 2))  ; => (just 1)
;;;
;;; ;; List as MonadPlus (non-determinism)
;;; (mp-bind list-monad-plus '(1 2 3)
;;;          (lambda (x) (list x (* x 10))))
;;; ; => (1 10 2 20 3 30)
;;;
;;; ;; Guard for filtering
;;; (bt-bind (between 1 10)
;;;          (lambda (x)
;;;            (bt-bind (bt-guard (even? x))
;;;                     (lambda (_) (bt-return x)))))
;;; ; => (2 4 6 8 10)
;;;
;;; ;; N-Queens
;;; (n-queens 4)  ; => ((3 1 4 2) (2 4 1 3))
;;;
;;; ;; Permutations
;;; (permutations '(1 2 3))
;;; ; => ((1 2 3) (1 3 2) (2 1 3) (2 3 1) (3 1 2) (3 2 1))

