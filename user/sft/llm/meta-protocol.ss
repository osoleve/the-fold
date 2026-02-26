;;; user/sft/llm/meta-protocol.ss — Protocol system SFT task generator
;;;
;;; Generates SFT samples teaching the protocol dispatch system:
;;;   - Define + implement + dispatch (single protocol, 2+ types)
;;;   - Default fallback (define-protocol/default, test unknown type)
;;;   - Introspection (protocol-implementations, type-implements?)
;;;   - Multi-protocol (one type implements 2+ protocols)
;;;
;;; Seeds are mechanically constructed. Verification is pure within
;;; subprocess isolation (protocol registry is global mutable state).
;;;
;;; Usage:
;;;   (load "user/sft/llm/meta-protocol.ss")
;;;   (define seeds (meta-protocol/generate-seeds))

(load "user/sft/extract/emit.ss")

;;; ====
;;; Protocol Archetypes
;;; ====
;;; Each archetype defines a domain with concrete types, protocol(s),
;;; and verifiable behavior.

(define *mp-archetypes*
  `(;; --- Geometry: area + perimeter ---
    ((domain . "geometry")
     (protocols . (area perimeter))
     (types . ((circle
                 (constructor . (lambda (r) (list 'circle r)))
                 (area-impl . (lambda (c) (* 3.14159 (cadr c) (cadr c))))
                 (perimeter-impl . (lambda (c) (* 2 3.14159 (cadr c))))
                 (test-val . 5)
                 (expected-area . 78.53975)
                 (expected-perimeter . 31.4159))
               (rectangle
                 (constructor . (lambda (w h) (list 'rectangle w h)))
                 (area-impl . (lambda (r) (* (cadr r) (caddr r))))
                 (perimeter-impl . (lambda (r) (* 2 (+ (cadr r) (caddr r)))))
                 (test-val . (3 4))
                 (expected-area . 12)
                 (expected-perimeter . 14)))))

    ;; --- Algebra: combine + identity ---
    ((domain . "algebra")
     (protocols . (combine identity))
     (types . ((sum-monoid
                 (constructor . (lambda (v) (list 'sum-monoid v)))
                 (combine-impl . (lambda (a b) (list 'sum-monoid (+ (cadr a) (cadr b)))))
                 (identity-impl . (lambda (a) (list 'sum-monoid 0)))
                 (test-val . 10)
                 (expected-combine . (sum-monoid 30))
                 (expected-identity . (sum-monoid 0)))
               (product-monoid
                 (constructor . (lambda (v) (list 'product-monoid v)))
                 (combine-impl . (lambda (a b) (list 'product-monoid (* (cadr a) (cadr b)))))
                 (identity-impl . (lambda (a) (list 'product-monoid 1)))
                 (test-val . 5)
                 (expected-combine . (product-monoid 25))
                 (expected-identity . (product-monoid 1))))))

    ;; --- Display: show + pretty ---
    ((domain . "display")
     (protocols . (show))
     (types . ((point
                 (constructor . (lambda (x y) (list 'point x y)))
                 (show-impl . (lambda (p) (format "(~a, ~a)" (cadr p) (caddr p))))
                 (test-val . (3 4))
                 (expected-show . "(3, 4)"))
               (color
                 (constructor . (lambda (r g b) (list 'color r g b)))
                 (show-impl . (lambda (c) (format "rgb(~a,~a,~a)" (cadr c) (caddr c) (cadddr c))))
                 (test-val . (255 128 0))
                 (expected-show . "rgb(255,128,0)")))))

    ;; --- Numeric: distance ---
    ((domain . "numeric")
     (protocols . (distance))
     (types . ((vec2
                 (constructor . (lambda (x y) (list 'vec2 x y)))
                 (distance-impl . (lambda (a b)
                   (let ([dx (- (cadr b) (cadr a))]
                         [dy (- (caddr b) (caddr a))])
                     (sqrt (+ (* dx dx) (* dy dy))))))
                 (test-val . (0 0))
                 (expected-distance . 5.0))
               (scalar
                 (constructor . (lambda (v) (list 'scalar v)))
                 (distance-impl . (lambda (a b) (abs (- (cadr b) (cadr a)))))
                 (test-val . 3)
                 (expected-distance . 7)))))))

;;; ====
;;; Task Type Generators
;;; ====

(define *mp-counter* 0)

(define (mp/next-id)
  (set! *mp-counter* (+ *mp-counter* 1))
  (format "meta_protocol_~3,'0d" *mp-counter*))

(define (mp/archetype-ref archetype key)
  (cdr (assq key archetype)))

(define (mp/type-ref type-spec key)
  (let ([entry (assq key (cdr type-spec))])
    (if entry (cdr entry) #f)))

(define (mp/generate-define-dispatch archetype)
  (doc 'description
    "Generate: define protocol + implement for 2 types + dispatch")
  (let* ([domain (mp/archetype-ref archetype 'domain)]
         [protocols (mp/archetype-ref archetype 'protocols)]
         [types (mp/archetype-ref archetype 'types)]
         [proto (car protocols)]  ; use first protocol
         [type-names (map car types)]
         [first-type (car types)]
         [second-type (cadr types)]
         [t1-name (car first-type)]
         [t2-name (car second-type)])
    (let* ([prompt-body
            (format "Define a protocol called `~a` that dispatches on the first argument's type tag.

Implement it for two types:
- `~a`: tagged list `(~a ...)`.
- `~a`: tagged list `(~a ...)`.

Use `define-protocol` to define the dispatch function, then `implement-protocol!`
to register each type's implementation. Show a dispatch call for each type."
                    proto t1-name t1-name t2-name t2-name)]
           [impl1-str (let ([p (open-output-string)])
                        (write (mp/type-ref first-type
                                 (string->symbol
                                   (format "~a-impl" proto)))
                               p)
                        (get-output-string p))]
           [impl2-str (let ([p (open-output-string)])
                        (write (mp/type-ref second-type
                                 (string->symbol
                                   (format "~a-impl" proto)))
                               p)
                        (get-output-string p))]
           [ground-truth
            (format "(define-protocol (~a obj) \"~a dispatch\")

(implement-protocol! '~a '~a
  ~a)

(implement-protocol! '~a '~a
  ~a)"
                    proto (string-titlecase domain)
                    proto t1-name impl1-str
                    proto t2-name impl2-str)]
           ;; Verify: dispatch produces expected values
           [verify-sexp
            `(and (equal? (protocol-exists? ',proto) #t)
                  (equal? (type-implements? ',t1-name ',proto) #t)
                  (equal? (type-implements? ',t2-name ',proto) #t))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mp/next-id)]
           [tags (list domain (symbol->string proto)
                       "meta_protocol" "define_dispatch")])
      (make-sample id "meta_protocol" "usage" "easy"
                   "lattice/fp/protocol.ss" ""
                   (symbol->string proto)
                   prompt-body ground-truth verify-str tags))))

(define (mp/generate-default-fallback archetype)
  (doc 'description
    "Generate: define-protocol/default + test with unknown type")
  (let* ([domain (mp/archetype-ref archetype 'domain)]
         [protocols (mp/archetype-ref archetype 'protocols)]
         [proto (car protocols)]
         [default-name (string->symbol (format "~a-with-default" proto))])
    (let* ([prompt-body
            (format "Define a protocol called `~a` using `define-protocol/default`.
The default handler should return the string \"unknown\".

Implement it for the `point` type as `(lambda (p) \"point-~a\")`.

Then dispatch with:
1. A `point` object — should use the registered implementation
2. An `unknown-thing` object — should use the default"
                    default-name proto)]
           [ground-truth
            (format "(define-protocol/default (~a obj)
  (lambda (obj) \"unknown\"))

(implement-protocol! '~a 'point
  (lambda (p) \"point-~a\"))"
                    default-name default-name proto)]
           [verify-sexp
            `(and (equal? (,default-name '(point 1 2)) ,(format "point-~a" proto))
                  (equal? (,default-name '(unknown-thing)) "unknown"))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mp/next-id)]
           [tags (list domain (symbol->string proto)
                       "meta_protocol" "default_fallback")])
      (make-sample id "meta_protocol" "usage" "medium"
                   "lattice/fp/protocol.ss" ""
                   (symbol->string default-name)
                   prompt-body ground-truth verify-str tags))))

(define (mp/generate-introspection archetype)
  (doc 'description
    "Generate: query protocol registry after implementations")
  (let* ([domain (mp/archetype-ref archetype 'domain)]
         [protocols (mp/archetype-ref archetype 'protocols)]
         [types (mp/archetype-ref archetype 'types)]
         [proto (car protocols)]
         [type-names (map car types)])
    (let* ([prompt-body
            (format "After defining a protocol `~a` and implementing it for types ~a,
use the introspection API to:
1. Check `protocol-exists?` returns #t
2. Check `type-implements?` for each type
3. List all implementations with `protocol-implementations`
4. List all registered protocols with `list-protocols`"
                    proto type-names)]
           [ground-truth
            (format ";; After defining and implementing '~a for ~a:
(protocol-exists? '~a)           ;; => #t
~a
(protocol-implementations '~a)   ;; => ~a (order may vary)"
                    proto type-names proto
                    (apply string-append
                      (map (lambda (tn)
                             (format "(type-implements? '~a '~a)  ;; => #t\n"
                                     tn proto))
                           type-names))
                    proto type-names)]
           [verify-sexp
            `(and (equal? (protocol-exists? ',proto) #t)
                  ,@(map (lambda (tn)
                           `(equal? (type-implements? ',tn ',proto) #t))
                         type-names))]
           [verify-str (let ([p (open-output-string)])
                         (write verify-sexp p)
                         (get-output-string p))]
           [id (mp/next-id)]
           [tags (list domain (symbol->string proto)
                       "meta_protocol" "introspection")])
      (make-sample id "meta_protocol" "analysis" "medium"
                   "lattice/fp/protocol.ss" ""
                   (symbol->string proto)
                   prompt-body ground-truth verify-str tags))))

(define (mp/generate-multi-protocol archetype)
  (doc 'description
    "Generate: single type implementing 2+ protocols")
  (let* ([domain (mp/archetype-ref archetype 'domain)]
         [protocols (mp/archetype-ref archetype 'protocols)]
         [types (mp/archetype-ref archetype 'types)])
    ;; Only works for archetypes with 2+ protocols
    (if (< (length protocols) 2)
        #f
        (let* ([proto1 (car protocols)]
               [proto2 (cadr protocols)]
               [first-type (car types)]
               [t-name (car first-type)])
          (let* ([prompt-body
                  (format "Show that a single type can implement multiple protocols.

Define two protocols: `~a` and `~a`.
Implement both for the `~a` type.
Then dispatch both protocols on the same object."
                          proto1 proto2 t-name)]
                 [impl1-str (let ([p (open-output-string)])
                              (write (mp/type-ref first-type
                                       (string->symbol
                                         (format "~a-impl" proto1)))
                                     p)
                              (get-output-string p))]
                 [impl2-str (let ([p (open-output-string)])
                              (write (mp/type-ref first-type
                                       (string->symbol
                                         (format "~a-impl" proto2)))
                                     p)
                              (get-output-string p))]
                 [ground-truth
                  (format "(define-protocol (~a obj) \"~a\")
(define-protocol (~a obj) \"~a\")

(implement-protocol! '~a '~a ~a)
(implement-protocol! '~a '~a ~a)"
                          proto1 (symbol->string proto1)
                          proto2 (symbol->string proto2)
                          proto1 t-name impl1-str
                          proto2 t-name impl2-str)]
                 [verify-sexp
                  `(and (type-implements? ',t-name ',proto1)
                        (type-implements? ',t-name ',proto2))]
                 [verify-str (let ([p (open-output-string)])
                               (write verify-sexp p)
                               (get-output-string p))]
                 [id (mp/next-id)]
                 [tags (list domain
                             (symbol->string proto1)
                             (symbol->string proto2)
                             "meta_protocol" "multi_protocol")])
            (make-sample id "meta_protocol" "usage" "hard"
                         "lattice/fp/protocol.ss" ""
                         (symbol->string t-name)
                         prompt-body ground-truth verify-str tags))))))

;;; ====
;;; Main Generator
;;; ====

(define (meta-protocol/generate-seeds)
  (doc 'description
    "Generate protocol system SFT samples from archetypes.
     Returns list of sample alists in standard schema.")
  (set! *mp-counter* 0)
  (printf "Generating protocol system seeds from ~a archetypes...\\n"
          (length *mp-archetypes*))
  (let loop ([archetypes *mp-archetypes*] [acc '()])
    (if (null? archetypes)
        (begin
          (printf "Generated ~a protocol system seeds\\n" (length acc))
          (reverse acc))
        (let* ([arch (car archetypes)]
               ;; Each archetype generates 3-4 task types
               [s1 (guard (ex [else #f])
                     (mp/generate-define-dispatch arch))]
               [s2 (guard (ex [else #f])
                     (mp/generate-default-fallback arch))]
               [s3 (guard (ex [else #f])
                     (mp/generate-introspection arch))]
               [s4 (guard (ex [else #f])
                     (mp/generate-multi-protocol arch))]
               [new-samples (filter (lambda (x) x) (list s1 s2 s3 s4))])
          (loop (cdr archetypes)
                (append (reverse new-samples) acc))))))

;;; ====
;;; REPL interface
;;; ====

(printf "meta-protocol.ss loaded.\\n")
(printf "  (meta-protocol/generate-seeds) - Generate protocol system seeds\\n")
