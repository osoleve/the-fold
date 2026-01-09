;;; core/linalg/vec-common.ss --- Common Vector Infrastructure
;;;
;;; Provides macros for generating dimension-specific vector operations.
;;; This eliminates code duplication between vec2.ss and vec3.ss while
;;; maintaining full performance (macros expand at compile time).
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; The approach here uses explicit macro invocations where the caller
;;; provides all necessary names. This is more verbose but works reliably
;;; with Chez Scheme's syntax-rules which cannot concatenate symbols.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")


;;; ============================================================
;;; 2D Vector Generation Macros
;;; ============================================================

;;; generate-vec2-core : Generate constructor, predicate, accessors
(define-syntax generate-vec2-core
  (syntax-rules ()
                [(_ ctor pred? get-x get-y to-list from-list zero one unit-x unit-y)
                 (begin
                  ;; Constructor
                  (define (ctor x y)
                    (list 'ctor x y))
                  
                  ;; Predicate
                  (define (pred? v)
                    (and (pair? v) (eq? (car v) 'ctor)))
                  
                  ;; Accessors
                  (define (get-x v) (cadr v))
                  (define (get-y v) (caddr v))
                  
                  ;; List conversion
                  (define (to-list v)
                    (list (get-x v) (get-y v)))
                  
                  (define (from-list lst)
                    (ctor (car lst) (cadr lst)))
                  
                  ;; Standard vectors
                  (define (zero) (ctor 0 0))
                  (define (one) (ctor 1 1))
                  (define (unit-x) (ctor 1 0))
                  (define (unit-y) (ctor 0 1)))]))


;;; generate-vec2-arithmetic : Generate +, -, *, /, scale
(define-syntax generate-vec2-arithmetic
  (syntax-rules ()
                [(_ ctor get-x get-y
                    v-add v-sub v-neg v-mul v-div v-scale v-scale-inv)
                 (begin
                  (define (v-add a b)
                    (ctor (+ (get-x a) (get-x b))
                          (+ (get-y a) (get-y b))))
                  
                  (define (v-sub a b)
                    (ctor (- (get-x a) (get-x b))
                          (- (get-y a) (get-y b))))
                  
                  (define (v-neg v)
                    (ctor (- (get-x v)) (- (get-y v))))
                  
                  (define (v-mul a b)
                    (ctor (* (get-x a) (get-x b))
                          (* (get-y a) (get-y b))))
                  
                  (define (v-div a b)
                    (ctor (/ (get-x a) (get-x b))
                          (/ (get-y a) (get-y b))))
                  
                  (define (v-scale v s)
                    (ctor (* (get-x v) s)
                          (* (get-y v) s)))
                  
                  (define (v-scale-inv v s)
                    (ctor (/ (get-x v) s)
                          (/ (get-y v) s))))]))


;;; generate-vec2-products : Generate dot, magnitude, distance, normalize
(define-syntax generate-vec2-products
  (syntax-rules ()
                [(_ ctor get-x get-y zero-fn v-sub v-scale-inv
                    v-dot v-mag-sq v-mag v-length v-dist-sq v-dist v-norm v-unit v-set-mag)
                 (begin
                  (define (v-dot a b)
                    (+ (* (get-x a) (get-x b))
                       (* (get-y a) (get-y b))))
                  
                  (define (v-mag-sq v)
                    (+ (* (get-x v) (get-x v))
                       (* (get-y v) (get-y v))))
                  
                  (define (v-mag v)
                    (sqrt (v-mag-sq v)))
                  
                  (define v-length v-mag)
                  
                  (define (v-dist-sq a b)
                    (v-mag-sq (v-sub a b)))
                  
                  (define (v-dist a b)
                    (v-mag (v-sub a b)))
                  
                  (define (v-norm v)
                    (let ([mag (v-mag v)])
                         (if (< mag 1e-10)
                             (zero-fn)
                             (v-scale-inv v mag))))
                  
                  (define v-unit v-norm)
                  
                  (define (v-set-mag v new-mag)
                    (let ([normalized (v-norm v)])
                         (ctor (* (get-x normalized) new-mag)
                               (* (get-y normalized) new-mag)))))]))


;;; generate-vec2-interpolation : Generate lerp, slerp, move-towards
(define-syntax generate-vec2-interpolation
  (syntax-rules ()
                [(_ ctor get-x get-y v-add v-sub v-scale v-norm v-mag v-angle-between
                    v-lerp v-slerp v-move-towards)
                 (begin
                  (define (v-lerp a b t)
                    (v-add (v-scale a (- 1 t))
                           (v-scale b t)))
                  
                  (define (v-slerp a b t)
                    (let* ([theta (v-angle-between a b)]
                           [sin-theta (sin theta)])
                          (if (< (abs sin-theta) 1e-10)
                              (v-lerp a b t)
                              (v-add (v-scale a (/ (sin (* (- 1 t) theta)) sin-theta))
                                     (v-scale b (/ (sin (* t theta)) sin-theta))))))
                  
                  (define (v-move-towards a b max-distance)
                    (let* ([delta (v-sub b a)]
                           [dist (v-mag delta)])
                          (if (<= dist max-distance)
                              b
                              (v-add a (v-scale (v-norm delta) max-distance))))))]))


;;; generate-vec2-projection : Generate project, reject, reflect
(define-syntax generate-vec2-projection
  (syntax-rules ()
                [(_ ctor get-x get-y zero-fn v-sub v-scale v-dot v-mag-sq
                    v-project v-reject v-reflect)
                 (begin
                  (define (v-project a b)
                    (let ([b-mag-sq (v-mag-sq b)])
                         (if (< b-mag-sq 1e-10)
                             (zero-fn)
                             (v-scale b (/ (v-dot a b) b-mag-sq)))))
                  
                  (define (v-reject a b)
                    (v-sub a (v-project a b)))
                  
                  (define (v-reflect v normal)
                    (v-sub v (v-scale normal (* 2 (v-dot v normal))))))]))


;;; generate-vec2-comparison : Generate equal?, nearly-equal?, zero?
(define-syntax generate-vec2-comparison
  (syntax-rules ()
                [(_ get-x get-y v-mag-sq
                    v-equal? v-nearly-equal? v-zero?)
                 (begin
                  (define (v-equal? a b)
                    (and (= (get-x a) (get-x b))
                         (= (get-y a) (get-y b))))
                  
                  (define (v-nearly-equal? a b epsilon)
                    (and (< (abs (- (get-x a) (get-x b))) epsilon)
                         (< (abs (- (get-y a) (get-y b))) epsilon)))
                  
                  (define (v-zero? v)
                    (and (< (abs (get-x v)) 1e-10)
                         (< (abs (get-y v)) 1e-10))))]))


;;; generate-vec2-utilities : Generate min, max, abs, floor, ceil, etc.
(define-syntax generate-vec2-utilities
  (syntax-rules ()
                [(_ ctor get-x get-y v-mag-sq v-set-mag
                    v-min v-max v-clamp v-abs v-floor v-ceil v-round
                    v-map v-fold v-sum v-product v-clamp-mag v-limit)
                 (begin
                  (define (v-min a b)
                    (ctor (min (get-x a) (get-x b))
                          (min (get-y a) (get-y b))))
                  
                  (define (v-max a b)
                    (ctor (max (get-x a) (get-x b))
                          (max (get-y a) (get-y b))))
                  
                  (define (v-clamp v min-v max-v)
                    (v-min (v-max v min-v) max-v))
                  
                  (define (v-abs v)
                    (ctor (abs (get-x v)) (abs (get-y v))))
                  
                  (define (v-floor v)
                    (ctor (floor (get-x v)) (floor (get-y v))))
                  
                  (define (v-ceil v)
                    (ctor (ceiling (get-x v)) (ceiling (get-y v))))
                  
                  (define (v-round v)
                    (ctor (round (get-x v)) (round (get-y v))))
                  
                  (define (v-map f v)
                    (ctor (f (get-x v)) (f (get-y v))))
                  
                  (define (v-fold f init v)
                    (f (f init (get-x v)) (get-y v)))
                  
                  (define (v-sum v)
                    (+ (get-x v) (get-y v)))
                  
                  (define (v-product v)
                    (* (get-x v) (get-y v)))
                  
                  (define (v-clamp-mag v min-mag max-mag)
                    (let ([mag-sq (v-mag-sq v)])
                         (cond
                          [(< mag-sq (* min-mag min-mag)) (v-set-mag v min-mag)]
                          [(> mag-sq (* max-mag max-mag)) (v-set-mag v max-mag)]
                          [else v])))
                  
                  (define (v-limit v max-mag)
                    (let ([mag-sq (v-mag-sq v)])
                         (if (> mag-sq (* max-mag max-mag))
                             (v-set-mag v max-mag)
                             v))))]))


;;; ============================================================
;;; 3D Vector Generation Macros
;;; ============================================================

;;; generate-vec3-core : Generate constructor, predicate, accessors
(define-syntax generate-vec3-core
  (syntax-rules ()
                [(_ ctor pred? get-x get-y get-z get-ref to-list from-list
                    zero one unit-x unit-y unit-z)
                 (begin
                  (define (ctor x y z)
                    (list 'ctor x y z))
                  
                  (define (pred? v)
                    (and (pair? v) (eq? (car v) 'ctor)))
                  
                  (define (get-x v) (list-ref v 1))
                  (define (get-y v) (list-ref v 2))
                  (define (get-z v) (list-ref v 3))
                  
                  (define (get-ref v i)
                    (list-ref v (+ i 1)))
                  
                  (define (to-list v)
                    (list (get-x v) (get-y v) (get-z v)))
                  
                  (define (from-list lst)
                    (ctor (car lst) (cadr lst) (caddr lst)))
                  
                  (define (zero) (ctor 0 0 0))
                  (define (one) (ctor 1 1 1))
                  (define (unit-x) (ctor 1 0 0))
                  (define (unit-y) (ctor 0 1 0))
                  (define (unit-z) (ctor 0 0 1)))]))


;;; generate-vec3-arithmetic : Generate +, -, *, /, scale
(define-syntax generate-vec3-arithmetic
  (syntax-rules ()
                [(_ ctor get-x get-y get-z
                    v-add v-sub v-neg v-mul v-div v-scale v-scale-inv)
                 (begin
                  (define (v-add a b)
                    (ctor (+ (get-x a) (get-x b))
                          (+ (get-y a) (get-y b))
                          (+ (get-z a) (get-z b))))
                  
                  (define (v-sub a b)
                    (ctor (- (get-x a) (get-x b))
                          (- (get-y a) (get-y b))
                          (- (get-z a) (get-z b))))
                  
                  (define (v-neg v)
                    (ctor (- (get-x v)) (- (get-y v)) (- (get-z v))))
                  
                  (define (v-mul a b)
                    (ctor (* (get-x a) (get-x b))
                          (* (get-y a) (get-y b))
                          (* (get-z a) (get-z b))))
                  
                  (define (v-div a b)
                    (ctor (/ (get-x a) (get-x b))
                          (/ (get-y a) (get-y b))
                          (/ (get-z a) (get-z b))))
                  
                  (define (v-scale v s)
                    (ctor (* (get-x v) s)
                          (* (get-y v) s)
                          (* (get-z v) s)))
                  
                  (define (v-scale-inv v s)
                    (ctor (/ (get-x v) s)
                          (/ (get-y v) s)
                          (/ (get-z v) s))))]))


;;; generate-vec3-products : Generate dot, cross, magnitude, distance, normalize
(define-syntax generate-vec3-products
  (syntax-rules ()
                [(_ ctor get-x get-y get-z zero-fn v-sub v-scale v-scale-inv
                    v-dot v-cross v-triple-scalar v-triple-vector
                    v-mag-sq v-mag v-length v-dist-sq v-dist
                    v-norm v-norm-or v-set-mag)
                 (begin
                  (define (v-dot a b)
                    (+ (* (get-x a) (get-x b))
                       (* (get-y a) (get-y b))
                       (* (get-z a) (get-z b))))
                  
                  (define (v-cross a b)
                    (ctor (- (* (get-y a) (get-z b)) (* (get-z a) (get-y b)))
                          (- (* (get-z a) (get-x b)) (* (get-x a) (get-z b)))
                          (- (* (get-x a) (get-y b)) (* (get-y a) (get-x b)))))
                  
                  (define (v-triple-scalar a b c)
                    (v-dot a (v-cross b c)))
                  
                  (define (v-triple-vector a b c)
                    (v-sub (v-scale b (v-dot a c))
                           (v-scale c (v-dot a b))))
                  
                  (define (v-mag-sq v)
                    (+ (* (get-x v) (get-x v))
                       (* (get-y v) (get-y v))
                       (* (get-z v) (get-z v))))
                  
                  (define (v-mag v)
                    (sqrt (v-mag-sq v)))
                  
                  (define v-length v-mag)
                  
                  (define (v-dist-sq a b)
                    (v-mag-sq (v-sub a b)))
                  
                  (define (v-dist a b)
                    (sqrt (v-dist-sq a b)))
                  
                  (define (v-norm v)
                    (let ([mag (v-mag v)])
                         (if (< mag 1e-10)
                             (zero-fn)
                             (v-scale-inv v mag))))
                  
                  (define (v-norm-or v default)
                    (let ([mag (v-mag v)])
                         (if (< mag 1e-10)
                             default
                             (v-scale-inv v mag))))
                  
                  (define (v-set-mag v mag)
                    (v-scale (v-norm v) mag)))]))


;;; generate-vec3-interpolation : Generate lerp, slerp, nlerp
(define-syntax generate-vec3-interpolation
  (syntax-rules ()
                [(_ ctor get-x get-y get-z v-add v-scale v-norm v-dot
                    v-lerp v-slerp v-nlerp)
                 (begin
                  (define (v-lerp a b t)
                    (v-add (v-scale a (- 1 t))
                           (v-scale b t)))
                  
                  (define (v-slerp a b t)
                    (let* ([dot (v-dot a b)]
                           [dot-clamped (max -1.0 (min 1.0 dot))]
                           [theta (acos dot-clamped)]
                           [sin-theta (sin theta)])
                          (if (< (abs sin-theta) 1e-10)
                              (v-norm (v-lerp a b t))
                              (let ([s0 (/ (sin (* (- 1 t) theta)) sin-theta)]
                                    [s1 (/ (sin (* t theta)) sin-theta)])
                                   (v-add (v-scale a s0)
                                          (v-scale b s1))))))
                  
                  (define (v-nlerp a b t)
                    (v-norm (v-lerp a b t))))]))


;;; generate-vec3-projection : Generate project, reject, reflect, refract
(define-syntax generate-vec3-projection
  (syntax-rules ()
                [(_ ctor get-x get-y get-z zero-fn v-add v-sub v-scale v-dot v-mag-sq
                    v-project v-reject v-reflect v-refract)
                 (begin
                  (define (v-project a b)
                    (let ([b-mag-sq (v-mag-sq b)])
                         (if (< b-mag-sq 1e-10)
                             (zero-fn)
                             (v-scale b (/ (v-dot a b) b-mag-sq)))))
                  
                  (define (v-reject a b)
                    (v-sub a (v-project a b)))
                  
                  (define (v-reflect v n)
                    (v-sub v (v-scale n (* 2 (v-dot v n)))))
                  
                  (define (v-refract v n eta)
                    (let* ([cos-i (- (v-dot v n))]
                           [sin2-t (* eta eta (- 1 (* cos-i cos-i)))])
                          (if (> sin2-t 1.0)
                              #f  ; Total internal reflection
                              (let ([cos-t (sqrt (- 1 sin2-t))])
                                   (v-add (v-scale v eta)
                                          (v-scale n (- (* eta cos-i) cos-t))))))))]))


;;; generate-vec3-comparison : Generate equal?, nearly-equal?, zero?, unit?
(define-syntax generate-vec3-comparison
  (syntax-rules ()
                [(_ get-x get-y get-z v-mag-sq
                    v-equal? v-nearly-equal? v-zero? v-unit?)
                 (begin
                  (define (v-equal? a b)
                    (and (= (get-x a) (get-x b))
                         (= (get-y a) (get-y b))
                         (= (get-z a) (get-z b))))
                  
                  (define (v-nearly-equal? a b epsilon)
                    (and (< (abs (- (get-x a) (get-x b))) epsilon)
                         (< (abs (- (get-y a) (get-y b))) epsilon)
                         (< (abs (- (get-z a) (get-z b))) epsilon)))
                  
                  (define (v-zero? v)
                    (and (< (abs (get-x v)) 1e-10)
                         (< (abs (get-y v)) 1e-10)
                         (< (abs (get-z v)) 1e-10)))
                  
                  (define (v-unit? v)
                    (< (abs (- (v-mag-sq v) 1.0)) 1e-10)))]))


;;; generate-vec3-utilities : Generate min, max, abs, floor, ceil, clamp
(define-syntax generate-vec3-utilities
  (syntax-rules ()
                [(_ ctor get-x get-y get-z v-mag v-set-mag
                    v-min v-max v-clamp v-abs v-floor v-ceil v-clamp-mag)
                 (begin
                  (define (v-min a b)
                    (ctor (min (get-x a) (get-x b))
                          (min (get-y a) (get-y b))
                          (min (get-z a) (get-z b))))
                  
                  (define (v-max a b)
                    (ctor (max (get-x a) (get-x b))
                          (max (get-y a) (get-y b))
                          (max (get-z a) (get-z b))))
                  
                  (define (v-clamp v vmin vmax)
                    (ctor (max (get-x vmin) (min (get-x v) (get-x vmax)))
                          (max (get-y vmin) (min (get-y v) (get-y vmax)))
                          (max (get-z vmin) (min (get-z v) (get-z vmax)))))
                  
                  (define (v-abs v)
                    (ctor (abs (get-x v))
                          (abs (get-y v))
                          (abs (get-z v))))
                  
                  (define (v-floor v)
                    (ctor (floor (get-x v))
                          (floor (get-y v))
                          (floor (get-z v))))
                  
                  (define (v-ceil v)
                    (ctor (ceiling (get-x v))
                          (ceiling (get-y v))
                          (ceiling (get-z v))))
                  
                  (define (v-clamp-mag v min-mag max-mag)
                    (let ([mag (v-mag v)])
                         (cond
                          [(< mag min-mag) (v-set-mag v min-mag)]
                          [(> mag max-mag) (v-set-mag v max-mag)]
                          [else v]))))]))


;;; generate-vec3-angles : Generate angle operations
(define-syntax generate-vec3-angles
  (syntax-rules ()
                [(_ v-dot v-cross v-mag
                    v-angle v-signed-angle)
                 (begin
                  (define (v-angle a b)
                    (let* ([dot (v-dot a b)]
                           [mags (* (v-mag a) (v-mag b))])
                          (if (< mags 1e-10)
                              0
                              (acos (max -1.0 (min 1.0 (/ dot mags)))))))
                  
                  (define (v-signed-angle a b axis)
                    (let* ([cross (v-cross a b)]
                           [angle (v-angle a b)]
                           [sign (if (< (v-dot cross axis) 0) -1 1)])
                          (* sign angle))))]))


;;; generate-vec3-rotation : Generate rotation around axes
(define-syntax generate-vec3-rotation
  (syntax-rules ()
                [(_ ctor get-x get-y get-z v-add v-scale v-cross v-dot
                    v-rotate-x v-rotate-y v-rotate-z v-rotate-axis)
                 (begin
                  (define (v-rotate-x v angle)
                    (let ([c (cos angle)]
                          [s (sin angle)]
                          [y (get-y v)]
                          [z (get-z v)])
                         (ctor (get-x v)
                               (- (* c y) (* s z))
                               (+ (* s y) (* c z)))))
                  
                  (define (v-rotate-y v angle)
                    (let ([c (cos angle)]
                          [s (sin angle)]
                          [x (get-x v)]
                          [z (get-z v)])
                         (ctor (+ (* c x) (* s z))
                               (get-y v)
                               (- (* c z) (* s x)))))
                  
                  (define (v-rotate-z v angle)
                    (let ([c (cos angle)]
                          [s (sin angle)]
                          [x (get-x v)]
                          [y (get-y v)])
                         (ctor (- (* c x) (* s y))
                               (+ (* s x) (* c y))
                               (get-z v))))
                  
                  ;; Rodrigues' rotation formula
                  (define (v-rotate-axis v axis angle)
                    (let* ([c (cos angle)]
                           [s (sin angle)]
                           [k axis]
                           [term1 (v-scale v c)]
                           [term2 (v-scale (v-cross k v) s)]
                           [term3 (v-scale k (* (v-dot k v) (- 1 c)))])
                          (v-add (v-add term1 term2) term3))))]))


;;; generate-vec3-basis : Generate orthonormal basis operations
(define-syntax generate-vec3-basis
  (syntax-rules ()
                [(_ ctor get-x get-y get-z v-norm v-cross v-reject
                    v-orthonormal-basis v-gram-schmidt)
                 (begin
                  (define (v-orthonormal-basis v)
                    (let* ([n (v-norm v)]
                           [up (if (< (abs (get-y n)) 0.9)
                                   (ctor 0 1 0)
                                   (ctor 1 0 0))]
                           [t1 (v-norm (v-cross n up))]
                           [t2 (v-cross n t1)])
                          (list t1 t2 n)))
                  
                  (define (v-gram-schmidt v1 v2)
                    (let* ([u1 (v-norm v1)]
                           [u2 (v-norm (v-reject v2 u1))])
                          (list u1 u2))))]))


;;; generate-vec3-coords : Generate coordinate conversions
(define-syntax generate-vec3-coords
  (syntax-rules ()
                [(_ ctor get-x get-y get-z v-mag
                    v-from-spherical v-from-cylindrical v-to-spherical v-to-cylindrical)
                 (begin
                  (define (v-from-spherical r theta phi)
                    (let ([sin-theta (sin theta)])
                         (ctor (* r sin-theta (cos phi))
                               (* r sin-theta (sin phi))
                               (* r (cos theta)))))
                  
                  (define (v-from-cylindrical r theta z)
                    (ctor (* r (cos theta))
                          (* r (sin theta))
                          z))
                  
                  (define (v-to-spherical v)
                    (let* ([x (get-x v)]
                           [y (get-y v)]
                           [z (get-z v)]
                           [r (v-mag v)]
                           [theta (if (< r 1e-10) 0 (acos (/ z r)))]
                           [phi (atan y x)])
                          (list r theta phi)))
                  
                  (define (v-to-cylindrical v)
                    (let* ([x (get-x v)]
                           [y (get-y v)]
                           [r (sqrt (+ (* x x) (* y y)))]
                           [theta (atan y x)])
                          (list r theta (get-z v)))))]))
