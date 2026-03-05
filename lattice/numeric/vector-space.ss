(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module vector-space
;;; @requires prelude
(require 'prelude)

(doc 'module 'vector-space)
(doc 'description "Lightweight computational vector-space abstraction for numerical solvers.
A vspace bundles the operations {add, sub, scale, madd, norm, zero, dim} over
an opaque vector type, enabling generic algorithms (ODE integrators, optimizers)
to work with lists, Scheme vectors, sparse representations, etc.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Vector Space Record
;;; ============================================================

(doc 'section 'vspace-record)

;;; A vspace is: (vspace add sub scale madd norm zero dim)
;;;   add   : V × V → V
;;;   sub   : V × V → V
;;;   scale : Number × V → V
;;;   madd  : V × Number × V → V    ;; v1 + s * v2
;;;   norm  : V → Number             ;; Euclidean norm
;;;   zero  : Nat → V                ;; zero vector of dimension n
;;;   dim   : V → Nat                ;; dimension of vector

(define (make-vspace add sub scale madd norm zero dim)
  (doc 'export #t)
  (doc 'type '(-> (-> V V V) (-> V V V) (-> Number V V)
                  (-> V Number V V) (-> V Number)
                  (-> Nat V) (-> V Nat) VSpace))
  (doc 'description "Create a vector space from its operations")
  (vector 'vspace add sub scale madd norm zero dim))

(define (vspace? x)
  (doc 'export #t)
  (and (vector? x) (= (vector-length x) 8) (eq? (vector-ref x 0) 'vspace)))

(define (vspace-add vs)    (doc 'export #t) (vector-ref vs 1))
(define (vspace-sub vs)    (doc 'export #t) (vector-ref vs 2))
(define (vspace-scale vs)  (doc 'export #t) (vector-ref vs 3))
(define (vspace-madd vs)   (doc 'export #t) (vector-ref vs 4))
(define (vspace-norm vs)   (doc 'export #t) (vector-ref vs 5))
(define (vspace-zero vs)   (doc 'export #t) (vector-ref vs 6))
(define (vspace-dim vs)    (doc 'export #t) (vector-ref vs 7))

;;; ============================================================
;;; List Vector Space
;;; ============================================================

(doc 'section 'list-vspace)

(doc 'description "Vector space over lists of numbers. State vectors are plain
lists — the representation used by physics/sim ODE solvers.")

(doc list-vspace 'export #t)
(define list-vspace
  (make-vspace
    (lambda (a b) (map + a b))                                    ; add
    (lambda (a b) (map - a b))                                    ; sub
    (lambda (s v) (map (lambda (x) (* s x)) v))                  ; scale
    (lambda (v1 s v2) (map (lambda (a b) (+ a (* s b))) v1 v2))  ; madd
    (lambda (v) (sqrt (apply + (map (lambda (x) (* x x)) v))))   ; norm
    (lambda (n) (make-list n 0))                                  ; zero
    length))                                                      ; dim

;;; ============================================================
;;; Scheme Vector Space
;;; ============================================================

(doc 'section 'scheme-vec-vspace)

(doc 'description "Vector space over Scheme vectors (#(...)).
The representation used by PDE solvers and linalg.")

(doc scheme-vec-vspace 'export #t)
(define scheme-vec-vspace
  (let ([vec-add
          (lambda (a b)
            (let* ([n (vector-length a)]
                   [r (make-vector n 0)])
              (do ([i 0 (+ i 1)]) ((= i n) r)
                (vector-set! r i (+ (vector-ref a i) (vector-ref b i))))))]
        [vec-sub
          (lambda (a b)
            (let* ([n (vector-length a)]
                   [r (make-vector n 0)])
              (do ([i 0 (+ i 1)]) ((= i n) r)
                (vector-set! r i (- (vector-ref a i) (vector-ref b i))))))]
        [vec-scale
          (lambda (s v)
            (let* ([n (vector-length v)]
                   [r (make-vector n 0)])
              (do ([i 0 (+ i 1)]) ((= i n) r)
                (vector-set! r i (* s (vector-ref v i))))))]
        [vec-madd
          (lambda (v1 s v2)
            (let* ([n (vector-length v1)]
                   [r (make-vector n 0)])
              (do ([i 0 (+ i 1)]) ((= i n) r)
                (vector-set! r i (+ (vector-ref v1 i) (* s (vector-ref v2 i)))))))]
        [vec-norm
          (lambda (v)
            (let ([n (vector-length v)])
              (let loop ([i 0] [sum 0.0])
                (if (= i n)
                    (sqrt sum)
                    (loop (+ i 1) (+ sum (* (vector-ref v i) (vector-ref v i))))))))]
        [vec-zero
          (lambda (n) (make-vector n 0))])
    (make-vspace vec-add vec-sub vec-scale vec-madd vec-norm vec-zero vector-length)))
