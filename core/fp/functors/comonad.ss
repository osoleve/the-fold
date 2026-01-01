;;; fabric/stitches/fp/comonad.ss — Comonad Abstraction
;;;
;;; Comonads are the categorical dual of monads. Where monads compose
;;; computations that produce values in a context, comonads compose
;;; computations that consume values from a context.
;;;
;;; A comonad has:
;;;   - extract : w a -> a           (get current focus)
;;;   - extend  : (w a -> b) -> w a -> w b  (apply contextual function)
;;;   - duplicate : w a -> w (w a)   (derived from extend)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Identity comonad (trivial)
;;;   - Env comonad (read-only environment, dual of Reader)
;;;   - Store comonad (position + accessor, basis for lenses)
;;;   - Stream comonad (infinite sequences)
;;;   - Zipper comonad (list with focus)
;;;   - Traced comonad (dual of Writer)
;;;   - Comonad transformers
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss
;;;
;;; Comonad Laws:
;;;   extract . extend f     = f
;;;   extend extract         = id
;;;   extend f . extend g    = extend (f . extend g)

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Identity Comonad
;;; ============================================================
;;;
;;; The trivial comonad - just a wrapper.

;;; identity-extract : a -> a
(define identity-extract identity)

;;; identity-extend : (a -> b) -> a -> b
(define (identity-extend f x) (f x))

;;; identity-duplicate : a -> a
(define identity-duplicate identity)

;;; ============================================================
;;; Env Comonad (CoReader)
;;; ============================================================
;;;
;;; Env e a = (e, a) — a value with an environment
;;; Dual of Reader: provides read-only context alongside values.

;;; make-env : e -> a -> Env e a
(define (make-env env val)
  (cons env val))

;;; env-value : Env e a -> a
(define env-value cdr)

;;; env-context : Env e a -> e
(define env-context car)

;;; env-extract : Env e a -> a
;;; Get the current value.
(define env-extract env-value)

;;; env-extend : (Env e a -> b) -> Env e a -> Env e b
;;; Apply a function that uses the whole context.
(define (env-extend f env)
  (make-env (env-context env) (f env)))

;;; env-duplicate : Env e a -> Env e (Env e a)
;;; Create nested environment.
(define (env-duplicate env)
  (make-env (env-context env) env))

;;; env-asks : (e -> e') -> Env e a -> Env e' a
;;; Transform the environment.
(define (env-asks f env)
  (make-env (f (env-context env)) (env-value env)))

;;; env-local : (e -> e) -> (Env e a -> b) -> Env e a -> b
;;; Run with modified environment.
(define (env-local modify f env)
  (f (make-env (modify (env-context env)) (env-value env))))

;;; ============================================================
;;; Store Comonad
;;; ============================================================
;;;
;;; Store s a = (s -> a, s) — a position and an accessor
;;; Dual of State: represents a value that depends on position.
;;; This is the comonad underlying lenses.

;;; make-store : (s -> a) -> s -> Store s a
(define (make-store peek pos)
  (list 'store peek pos))

;;; store? : Any -> Boolean
(define (store? x)
  (and (pair? x) (eq? (car x) 'store)))

;;; store-peek : Store s a -> s -> a
(define (store-peek s)
  (list-ref s 1))

;;; store-pos : Store s a -> s
(define (store-pos s)
  (list-ref s 2))

;;; store-extract : Store s a -> a
;;; Get value at current position.
(define (store-extract s)
  ((store-peek s) (store-pos s)))

;;; store-extend : (Store s a -> b) -> Store s a -> Store s b
;;; Apply a function at all positions.
(define (store-extend f s)
  (make-store (lambda (pos)
                      (f (make-store (store-peek s) pos)))
              (store-pos s)))

;;; store-duplicate : Store s a -> Store s (Store s a)
;;; Create nested store.
(define (store-duplicate s)
  (make-store (lambda (pos)
                      (make-store (store-peek s) pos))
              (store-pos s)))

;;; store-seek : s -> Store s a -> Store s a
;;; Move to new position.
(define (store-seek pos s)
  (make-store (store-peek s) pos))

;;; store-seeks : (s -> s) -> Store s a -> Store s a
;;; Transform position.
(define (store-seeks f s)
  (make-store (store-peek s) (f (store-pos s))))

;;; store-experiment : (s -> f s) -> Store s a -> f a
;;; Functorial position transformation (requires functor f).
(define (store-experiment fmap f s)
  (fmap (store-peek s) (f (store-pos s))))

;;; ============================================================
;;; Stream Comonad
;;; ============================================================
;;;
;;; Stream a = (a, () -> Stream a) — infinite sequence
;;; The canonical example of a comonad.

;;; make-stream : a -> (() -> Stream a) -> Stream a
(define (make-stream head tail-thunk)
  (cons head tail-thunk))

;;; stream-head : Stream a -> a
(define stream-head car)

;;; stream-tail : Stream a -> Stream a
(define (stream-tail s)
  ((cdr s)))

;;; stream-extract : Stream a -> a
(define stream-extract stream-head)

;;; stream-extend : (Stream a -> b) -> Stream a -> Stream b
;;; Apply function to stream and all its tails.
(define (stream-extend f s)
  (make-stream (f s)
               (lambda () (stream-extend f (stream-tail s)))))

;;; stream-duplicate : Stream a -> Stream (Stream a)
;;; Create stream of all tails.
(define (stream-duplicate s)
  (make-stream s
               (lambda () (stream-duplicate (stream-tail s)))))

;;; stream-take : Number -> Stream a -> List a
;;; Take first n elements.
(define (stream-take n s)
  (if (= n 0)
      '()
      (cons (stream-head s)
            (stream-take (- n 1) (stream-tail s)))))

;;; stream-drop : Number -> Stream a -> Stream a
;;; Drop first n elements.
(define (stream-drop n s)
  (if (= n 0)
      s
      (stream-drop (- n 1) (stream-tail s))))

;;; stream-nth : Number -> Stream a -> a
;;; Get nth element (0-indexed).
(define (stream-nth n s)
  (if (= n 0)
      (stream-head s)
      (stream-nth (- n 1) (stream-tail s))))

;;; stream-map : (a -> b) -> Stream a -> Stream b
(define (stream-map f s)
  (make-stream (f (stream-head s))
               (lambda () (stream-map f (stream-tail s)))))

;;; stream-iterate : (a -> a) -> a -> Stream a
;;; Create stream by repeated application.
(define (stream-iterate f x)
  (make-stream x
               (lambda () (stream-iterate f (f x)))))

;;; stream-repeat : a -> Stream a
;;; Infinite stream of same value.
(define (stream-repeat x)
  (make-stream x
               (lambda () (stream-repeat x))))

;;; stream-unfold : (s -> a) -> (s -> s) -> s -> Stream a
;;; General stream constructor.
(define (stream-unfold view step seed)
  (make-stream (view seed)
               (lambda () (stream-unfold view step (step seed)))))

;;; stream-zip-with : (a -> b -> c) -> Stream a -> Stream b -> Stream c
(define (stream-zip-with f s1 s2)
  (make-stream (f (stream-head s1) (stream-head s2))
               (lambda () (stream-zip-with f (stream-tail s1) (stream-tail s2)))))

;;; ============================================================
;;; Zipper Comonad
;;; ============================================================
;;;
;;; Zipper a = (List a, a, List a) — list with focus
;;; Represents a position in a list with context.

;;; make-zipper : List a -> a -> List a -> Zipper a
(define (make-zipper left focus right)
  (list 'zipper left focus right))

;;; zipper? : Any -> Boolean
(define (zipper? x)
  (and (pair? x) (eq? (car x) 'zipper)))

;;; zipper-left : Zipper a -> List a
(define (zipper-left z) (list-ref z 1))

;;; zipper-focus : Zipper a -> a
(define (zipper-focus z) (list-ref z 2))

;;; zipper-right : Zipper a -> List a
(define (zipper-right z) (list-ref z 3))

;;; zipper-extract : Zipper a -> a
(define zipper-extract zipper-focus)

;;; list->zipper : List a -> Maybe (Zipper a)
;;; Convert list to zipper focused on first element.
(define (list->zipper lst)
  (if (null? lst)
      nothing
      (just (make-zipper '() (car lst) (cdr lst)))))

;;; zipper->list : Zipper a -> List a
;;; Convert zipper back to list.
(define (zipper->list z)
  (append (reverse (zipper-left z))
          (cons (zipper-focus z) (zipper-right z))))

;;; zipper-move-left : Zipper a -> Maybe (Zipper a)
;;; Move focus left.
(define (zipper-move-left z)
  (if (null? (zipper-left z))
      nothing
      (just (make-zipper (cdr (zipper-left z))
                         (car (zipper-left z))
                         (cons (zipper-focus z) (zipper-right z))))))

;;; zipper-move-right : Zipper a -> Maybe (Zipper a)
;;; Move focus right.
(define (zipper-move-right z)
  (if (null? (zipper-right z))
      nothing
      (just (make-zipper (cons (zipper-focus z) (zipper-left z))
                         (car (zipper-right z))
                         (cdr (zipper-right z))))))

;;; zipper-modify : (a -> a) -> Zipper a -> Zipper a
;;; Modify focused element.
(define (zipper-modify f z)
  (make-zipper (zipper-left z)
               (f (zipper-focus z))
               (zipper-right z)))

;;; zipper-set : a -> Zipper a -> Zipper a
;;; Replace focused element.
(define (zipper-set x z)
  (make-zipper (zipper-left z) x (zipper-right z)))

;;; zipper-extend : (Zipper a -> b) -> Zipper a -> Zipper b
;;; Apply function at all positions.
(define (zipper-extend f z)
  (define (go-left z result)
    (let ([mz (zipper-move-left z)])
         (if (nothing? mz)
             result
             (let ([z2 (from-just mz)])
                  (go-left z2 (cons (f z2) result))))))
  (define (go-right z result)
    (let ([mz (zipper-move-right z)])
         (if (nothing? mz)
             result
             (let ([z2 (from-just mz)])
                  (go-right z2 (cons (f z2) result))))))
  (let* ([left-values (go-left z '())]
         [right-values (reverse (go-right z '()))])
        (make-zipper left-values (f z) right-values)))

;;; zipper-duplicate : Zipper a -> Zipper (Zipper a)
;;; Create zipper of all positions.
(define (zipper-duplicate z)
  (zipper-extend identity z))

;;; ============================================================
;;; Traced Comonad (CoWriter)
;;; ============================================================
;;;
;;; Traced m a = m -> a — function from monoid to value
;;; Dual of Writer: consumes a monoid-valued "trace".

;;; make-traced : (m -> a) -> Traced m a
(define (make-traced f)
  (list 'traced f))

;;; traced? : Any -> Boolean
(define (traced? x)
  (and (pair? x) (eq? (car x) 'traced)))

;;; traced-fn : Traced m a -> (m -> a)
(define (traced-fn t) (list-ref t 1))

;;; run-traced : Traced m a -> m -> a
(define (run-traced t m)
  ((traced-fn t) m))

;;; traced-extract : Monoid m -> Traced m a -> a
;;; Extract at empty trace.
(define (traced-extract m-empty t)
  (run-traced t m-empty))

;;; traced-extend : (m -> m -> m) -> (Traced m a -> b) -> Traced m a -> Traced m b
;;; Apply function to shifted versions.
(define (traced-extend m-append f t)
  (make-traced (lambda (m1)
                       (f (make-traced (lambda (m2)
                                               (run-traced t (m-append m1 m2))))))))

;;; traced-duplicate : (m -> m -> m) -> Traced m a -> Traced m (Traced m a)
(define (traced-duplicate m-append t)
  (traced-extend m-append identity t))

;;; traced-listen : (m -> m -> m) -> m -> Traced m a -> Traced m (a, m)
;;; Expose the trace alongside the value.
(define (traced-listen m-append m-empty t)
  (make-traced (lambda (m)
                       (cons (run-traced t m) m))))

;;; ============================================================
;;; Comonad Combinators
;;; ============================================================

;;; co-map : (a -> b) -> (w a -> a) -> (w a -> w b)
;;; Map via extend and extract.
(define (co-map f extract extend wa)
  (extend (lambda (w) (f (extract w))) wa))

;;; co-join : w a -> w (w a)
;;; Alias for duplicate.
(define co-join identity)

;;; (=>>) : w a -> (w a -> b) -> w b
;;; Flipped extend (like >>= but for comonads).
(define (=>> extend wa f)
  (extend f wa))

;;; (<<=) : (w a -> b) -> w a -> w b
;;; Extend with reversed arguments.
(define (<<=) identity)  ; Just extend

;;; co-sequence : List (w a) -> w (List a)
;;; Requires comonad operations.
(define (co-sequence extract extend ws)
  (if (null? ws)
      (extend (lambda (_) '()) (car ws))  ; Need at least one
      (let ([combine (lambda (w acc)
                             (extend (lambda (wa)
                                             (cons (extract wa) (extract acc)))
                                     w))])
           (fold-right combine
                       (extend (lambda (wa) (list (extract wa)))
                               (car (reverse ws)))
                       (cdr (reverse ws))))))

;;; ============================================================
;;; Comonad Transformers
;;; ============================================================

;;; EnvT e w a = w (e, a)
;;; Add environment to any comonad.

;;; make-env-t : w (e, a) -> EnvT e w a
(define (make-env-t wea)
  (list 'env-t wea))

;;; env-t? : Any -> Boolean
(define (env-t? x)
  (and (pair? x) (eq? (car x) 'env-t)))

;;; run-env-t : EnvT e w a -> w (e, a)
(define (run-env-t et) (list-ref et 1))

;;; env-t-extract : (w (e, a) -> (e, a)) -> EnvT e w a -> a
(define (env-t-extract w-extract et)
  (cdr (w-extract (run-env-t et))))

;;; env-t-extend : (w a -> w b) -> (EnvT e w a -> b) -> EnvT e w a -> EnvT e w b
(define (env-t-extend w-extend w-extract f et)
  (make-env-t
   (w-extend (lambda (wea)
                     (let ([e (car (w-extract wea))])
                          (cons e (f (make-env-t wea)))))
             (run-env-t et))))

;;; ============================================================
;;; Practical Examples
;;; ============================================================

;;; moving-average : Number -> Stream Number -> Stream Number
;;; Calculate moving average over a window.
(define (moving-average window-size)
  (lambda (s)
          (stream-extend
           (lambda (stream)
                   (let ([values (stream-take window-size stream)])
                        (/ (apply + values) window-size)))
           s)))

;;; cellular-rule : (a -> a -> a -> a) -> Zipper a -> a
;;; Generic cellular automaton rule.
(define (cellular-rule rule z)
  (let* ([left-val (if (null? (zipper-left z))
                       (zipper-focus z)
                       (car (zipper-left z)))]
         [right-val (if (null? (zipper-right z))
                        (zipper-focus z)
                        (car (zipper-right z)))])
        (rule left-val (zipper-focus z) right-val)))

;;; cellular-step : (a -> a -> a -> a) -> Zipper a -> Zipper a
;;; One step of cellular automaton.
(define (cellular-step rule z)
  (zipper-extend (lambda (zp) (cellular-rule rule zp)) z))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Stream comonad: moving average
;;; (define nums (stream-iterate add1 1))
;;; (stream-take 5 nums)  ; => (1 2 3 4 5)
;;; (stream-take 5 ((moving-average 3) nums))  ; => (2 3 4 5 6)
;;;
;;; ;; Store comonad: image processing
;;; (define pixel-store (make-store (lambda (pos) (get-pixel-at pos)) (cons 0 0)))
;;; (store-extract pixel-store)  ; => pixel at (0,0)
;;;
;;; ;; Zipper: cellular automaton
;;; (define rule-110
;;;   (lambda (l c r)
;;;     (case (list l c r)
;;;       [((1 1 1)) 0]
;;;       [((1 1 0)) 1]
;;;       [((1 0 1)) 1]
;;;       [((1 0 0)) 0]
;;;       [((0 1 1)) 1]
;;;       [((0 1 0)) 1]
;;;       [((0 0 1)) 1]
;;;       [((0 0 0)) 0])))
;;; (define initial (from-just (list->zipper '(0 0 0 1 0 0 0))))
;;; (zipper->list (cellular-step rule-110 initial))
;;;
;;; ;; Env comonad: context-dependent computation
;;; (define with-config (make-env '((debug . #t) (limit . 100)) 42))
;;; (env-extract with-config)  ; => 42
;;; (env-extend (lambda (e)
;;;               (if (cdr (assoc 'debug (env-context e)))
;;;                   (string-append "DEBUG: " (number->string (env-value e)))
;;;                   (number->string (env-value e))))
;;;             with-config)
