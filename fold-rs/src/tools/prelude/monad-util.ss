; monad-util.ss - Additional monad utilities
; Part of The Fold prelude

; ============================================
; Maybe monad utilities
; ============================================

; bind-maybe: Monadic bind for Maybe
; (bind-maybe (just 5) (fn (x) (just (* x 2)))) => (just 10)
(define (bind-maybe m f)
  (if (nothing? m)
      m
      (f (from-just m))))

; sequence-maybes: Sequence list of maybes into maybe of list
; (sequence-maybes (list (just 1) (just 2))) => (just (1 2))
; (sequence-maybes (list (just 1) nothing)) => nothing
(define (sequence-maybes ms)
  (foldl (fn (acc m)
             (if (nothing? acc)
                 acc
                 (if (nothing? m)
                     m
                     (just (append (from-just acc) (list (from-just m)))))))
         (just '())
         ms))

; traverse-maybe: Map and sequence for Maybe
; (traverse-maybe (fn (x) (just (* x 2))) '(1 2 3)) => (just (2 4 6))
(define (traverse-maybe f lst)
  (sequence-maybes (map f lst)))

; ap-maybe: Applicative apply for Maybe
; (ap-maybe (just inc) (just 5)) => (just 6)
(define (ap-maybe mf mx)
  (if (nothing? mf)
      mf
      (if (nothing? mx)
          mx
          (just ((from-just mf) (from-just mx))))))

; maybe-to-list: Convert Maybe to list (empty or singleton)
; (maybe-to-list (just 5)) => (5)
; (maybe-to-list nothing) => ()
(define (maybe-to-list m)
  (if (nothing? m)
      '()
      (list (from-just m))))

; list-to-maybe: Convert list to Maybe (Nothing if empty)
; (list-to-maybe '(1 2 3)) => (just 1)
; (list-to-maybe '()) => nothing
(define (list-to-maybe lst)
  (if (null? lst)
      nothing
      (just (car lst))))

; cat-maybes: Filter and extract Just values from list of Maybes
; (cat-maybes (list (just 1) nothing (just 3))) => (1 3)
(define (cat-maybes lst)
  (map from-just (filter (fn (m) (not (nothing? m))) lst)))

; ============================================
; Either monad utilities
; ============================================

; bind-either: Monadic bind for Either
; (bind-either (right 5) (fn (x) (right (* x 2)))) => (right 10)
(define (bind-either m f)
  (if (left? m)
      m
      (f (from-right m))))

; sequence-eithers: Sequence list of eithers into either of list
; (sequence-eithers (list (right 1) (right 2))) => (right (1 2))
; (sequence-eithers (list (right 1) (left "error"))) => (left "error")
(define (sequence-eithers es)
  (foldl (fn (acc e)
             (if (left? acc)
                 acc
                 (if (left? e)
                     e
                     (right (append (from-right acc) (list (from-right e)))))))
         (right '())
         es))

; traverse-either: Map and sequence for Either
; (traverse-either (fn (x) (right (* x 2))) '(1 2 3)) => (right (2 4 6))
(define (traverse-either f lst)
  (sequence-eithers (map f lst)))

; ap-either: Applicative apply for Either
; (ap-either (right inc) (right 5)) => (right 6)
(define (ap-either ef ex)
  (if (left? ef)
      ef
      (if (left? ex)
          ex
          (right ((from-right ef) (from-right ex))))))

; either-to-maybe: Convert Either to Maybe
; (either-to-maybe (right 5)) => (just 5)
; (either-to-maybe (left "err")) => nothing
(define (either-to-maybe e)
  (if (left? e)
      nothing
      (just (from-right e))))

; maybe-to-either: Convert Maybe to Either with default error
; (maybe-to-either "error" (just 5)) => (right 5)
; (maybe-to-either "error" nothing) => (left "error")
(define (maybe-to-either err m)
  (if (nothing? m)
      (left err)
      (right (from-just m))))

; ============================================
; Module exports
; ============================================

(module-exports
 bind-maybe
 sequence-maybes
 traverse-maybe
 ap-maybe
 maybe-to-list
 list-to-maybe
 cat-maybes
 bind-either
 sequence-eithers
 traverse-either
 ap-either
 either-to-maybe
 maybe-to-either)
