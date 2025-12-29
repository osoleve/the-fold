;;; fabric/stitches/fp/bifunctor.ss — Bifunctor Typeclass
;;;
;;; A Bifunctor is a type constructor with two type parameters that is
;;; a functor in both arguments. Common examples: Either, Pair, Validation.
;;;
;;; class Bifunctor p where
;;;   bimap  :: (a -> b) -> (c -> d) -> p a c -> p b d
;;;   first  :: (a -> b) -> p a c -> p b c
;;;   second :: (c -> d) -> p a c -> p a d
;;;
;;; Laws:
;;;   bimap id id = id                              (identity)
;;;   bimap f g . bimap h i = bimap (f . h) (g . i) (composition)
;;;   first id = id
;;;   second id = id
;;;   first f = bimap f id
;;;   second g = bimap id g
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss

(load "fabric/stitches/prelude.ss")
(load "fabric/stitches/fp/combinators.ss")

;;; ============================================================
;;; Bifunctor Type
;;; ============================================================

;;; make-bifunctor : (a -> b -> p a b) -> ((a -> c) -> p a b -> p c b) -> ((b -> d) -> p a b -> p a d) -> Bifunctor p
;;; Create a bifunctor from first-map, second-map operations.
;;; bimap is derived from first and second.
(define (make-bifunctor bf-first bf-second)
  (list 'bifunctor bf-first bf-second))

;;; bifunctor? : Any -> Boolean
(define (bifunctor? x)
  (and (pair? x) (eq? (car x) 'bifunctor)))

;;; bifunctor-first : Bifunctor p -> (a -> b) -> p a c -> p b c
;;; Map over the first type parameter.
(define (bifunctor-first bf)
  (list-ref bf 1))

;;; bifunctor-second : Bifunctor p -> (c -> d) -> p a c -> p a d
;;; Map over the second type parameter.
(define (bifunctor-second bf)
  (list-ref bf 2))

;;; bf-bimap : Bifunctor p -> (a -> b) -> (c -> d) -> p a c -> p b d
;;; Map over both type parameters.
(define (bf-bimap bf f g x)
  ((bifunctor-second bf) g ((bifunctor-first bf) f x)))

;;; bf-first : Bifunctor p -> (a -> b) -> p a c -> p b c
;;; Convenience wrapper for bifunctor-first.
(define (bf-first bf f x)
  ((bifunctor-first bf) f x))

;;; bf-second : Bifunctor p -> (c -> d) -> p a c -> p a d
;;; Convenience wrapper for bifunctor-second.
(define (bf-second bf g x)
  ((bifunctor-second bf) g x))

;;; ============================================================
;;; Pair Bifunctor
;;; ============================================================
;;;
;;; Pair (a, b) is a bifunctor where:
;;;   first f (a, b) = (f a, b)
;;;   second g (a, b) = (a, g b)

(define pair-bifunctor
  (make-bifunctor
   ;; first
   (lambda (f p) (cons (f (car p)) (cdr p)))
   ;; second
   (lambda (g p) (cons (car p) (g (cdr p))))))

;;; pair-bimap : (a -> b) -> (c -> d) -> (a . c) -> (b . d)
(define (pair-bimap f g p)
  (bf-bimap pair-bifunctor f g p))

;;; pair-first : (a -> b) -> (a . c) -> (b . c)
(define (pair-map-first f p)
  (bf-first pair-bifunctor f p))

;;; pair-second : (c -> d) -> (a . c) -> (a . d)
(define (pair-map-second g p)
  (bf-second pair-bifunctor g p))

;;; ============================================================
;;; Either Bifunctor
;;; ============================================================
;;;
;;; Either e a is a bifunctor where:
;;;   first f (Left e) = Left (f e)
;;;   first f (Right a) = Right a
;;;   second g (Left e) = Left e
;;;   second g (Right a) = Right (g a)

(define either-bifunctor
  (make-bifunctor
   ;; first (maps over Left)
   (lambda (f e)
           (if (left? e)
               (left (f (from-left e)))
               e))
   ;; second (maps over Right)
   (lambda (g e)
           (if (right? e)
               (right (g (from-right e)))
               e))))

;;; either-bimap : (e -> e') -> (a -> a') -> Either e a -> Either e' a'
(define (either-bimap f g e)
  (bf-bimap either-bifunctor f g e))

;;; Note: map-left and map-right are already in combinators.ss
;;; These are aliases that use the bifunctor explicitly.

;;; either-map-left : (e -> e') -> Either e a -> Either e' a
(define (either-map-left f e)
  (bf-first either-bifunctor f e))

;;; either-map-right : (a -> a') -> Either e a -> Either e a'
(define (either-map-right g e)
  (bf-second either-bifunctor g e))

;;; ============================================================
;;; Validation Bifunctor
;;; ============================================================
;;;
;;; Validation e a is a bifunctor similar to Either:
;;;   first f (Failure es) = Failure (map f es)
;;;   first f (Success a) = Success a
;;;   second g (Failure es) = Failure es
;;;   second g (Success a) = Success (g a)

;;; validation-success : a -> Validation e a
(define (validation-mk-success x)
  (list 'success x))

;;; validation-failure : (List e) -> Validation e a
(define (validation-mk-failure errs)
  (list 'failure errs))

;;; validation-success? : Validation e a -> Boolean
(define (validation-is-success? v)
  (and (pair? v) (eq? (car v) 'success)))

;;; validation-failure? : Validation e a -> Boolean
(define (validation-is-failure? v)
  (and (pair? v) (eq? (car v) 'failure)))

;;; from-validation-success : Validation e a -> a
(define (from-validation-success v)
  (cadr v))

;;; from-validation-failure : Validation e a -> (List e)
(define (from-validation-failure v)
  (cadr v))

(define validation-bifunctor
  (make-bifunctor
   ;; first (maps over each error in Failure)
   (lambda (f v)
           (if (validation-is-failure? v)
               (validation-mk-failure (map f (from-validation-failure v)))
               v))
   ;; second (maps over Success)
   (lambda (g v)
           (if (validation-is-success? v)
               (validation-mk-success (g (from-validation-success v)))
               v))))

;;; validation-bimap : (e -> e') -> (a -> a') -> Validation e a -> Validation e' a'
(define (validation-bimap f g v)
  (bf-bimap validation-bifunctor f g v))

;;; validation-map-errors : (e -> e') -> Validation e a -> Validation e' a
(define (validation-map-errors f v)
  (bf-first validation-bifunctor f v))

;;; validation-map-success : (a -> a') -> Validation e a -> Validation e a'
(define (validation-map-success g v)
  (bf-second validation-bifunctor g v))

;;; ============================================================
;;; These (Const/Tagged) Bifunctor
;;; ============================================================
;;;
;;; These a b = This a | That b | These a b
;;; A bifunctor that holds one, the other, or both values.

;;; make-this : a -> These a b
(define (make-this x)
  (list 'this x))

;;; make-that : b -> These a b
(define (make-that y)
  (list 'that y))

;;; make-these : a -> b -> These a b
(define (make-these x y)
  (list 'these x y))

;;; this? : These a b -> Boolean
(define (this? t)
  (and (pair? t) (eq? (car t) 'this)))

;;; that? : These a b -> Boolean
(define (that? t)
  (and (pair? t) (eq? (car t) 'that)))

;;; these? : These a b -> Boolean
(define (these? t)
  (and (pair? t) (eq? (car t) 'these)))

;;; from-this : These a b -> a
(define (from-this t)
  (cadr t))

;;; from-that : These a b -> b
(define (from-that t)
  (cadr t))

;;; from-these-fst : These a b -> a
(define (from-these-fst t)
  (cadr t))

;;; from-these-snd : These a b -> b
(define (from-these-snd t)
  (caddr t))

(define these-bifunctor
  (make-bifunctor
   ;; first
   (lambda (f t)
           (cond
            [(this? t) (make-this (f (from-this t)))]
            [(that? t) t]
            [(these? t) (make-these (f (from-these-fst t)) (from-these-snd t))]))
   ;; second
   (lambda (g t)
           (cond
            [(this? t) t]
            [(that? t) (make-that (g (from-that t)))]
            [(these? t) (make-these (from-these-fst t) (g (from-these-snd t)))]))))

;;; these-bimap : (a -> a') -> (b -> b') -> These a b -> These a' b'
(define (these-bimap f g t)
  (bf-bimap these-bifunctor f g t))

;;; ============================================================
;;; Law Verification
;;; ============================================================

;;; verify-bifunctor-identity : Bifunctor p -> p a b -> Boolean
;;; Check that bimap id id = id
(define (verify-bifunctor-identity bf x)
  (equal? x (bf-bimap bf identity identity x)))

;;; verify-bifunctor-first-identity : Bifunctor p -> p a b -> Boolean
;;; Check that first id = id
(define (verify-bifunctor-first-identity bf x)
  (equal? x (bf-first bf identity x)))

;;; verify-bifunctor-second-identity : Bifunctor p -> p a b -> Boolean
;;; Check that second id = id
(define (verify-bifunctor-second-identity bf x)
  (equal? x (bf-second bf identity x)))

;;; verify-bifunctor-first-second : Bifunctor p -> (a -> b) -> (c -> d) -> p a c -> Boolean
;;; Check that bimap f g = first f . second g
(define (verify-bifunctor-composition bf f g x)
  (equal? (bf-bimap bf f g x)
          (bf-first bf f (bf-second bf g x))))

;;; ============================================================
;;; Derived Operations
;;; ============================================================

;;; bf-void-left : Bifunctor p -> a -> p b c -> p a c
;;; Replace left contents with a constant value.
(define (bf-void-left bf x p)
  (bf-first bf (const x) p))

;;; bf-void-right : Bifunctor p -> c -> p a b -> p a c
;;; Replace right contents with a constant value.
(define (bf-void-right bf x p)
  (bf-second bf (const x) p))

;;; ============================================================
;;; Example Usage (for documentation)
;;; ============================================================
;;;
;;; ;; Pair bifunctor
;;; (pair-bimap add1 string-length (cons 1 "hello"))
;;; ; => (2 . 5)
;;;
;;; ;; Either bifunctor
;;; (either-bimap string-upcase add1 (left "error"))
;;; ; => (left "ERROR")
;;; (either-bimap string-upcase add1 (right 5))
;;; ; => (right 6)
;;;
;;; ;; Validation bifunctor
;;; (validation-bimap string-upcase add1 (validation-mk-failure '("bad input")))
;;; ; => (failure ("BAD INPUT"))
;;;
;;; ;; These bifunctor
;;; (these-bimap add1 string-upcase (make-these 1 "hi"))
;;; ; => (these 2 "HI")
