;;; lattice/fp/pretty-instances.ss — Pretty Type Class Instances for Lattice Types
;;; @module pretty-instances
;;; @requires pretty-class linalg numeric algebra
;;;
;;; Provides Pretty type class instances for:
;;;   - Vectors (Vec, Vec2, Vec3, Vec4)
;;;   - Matrices
;;;   - Complex numbers
;;;   - Polynomials
;;;   - Symbolic expressions
;;;
;;; This module extends the core Pretty type class to lattice types,
;;; enabling unified pretty-printing across the system.
;;;
;;; Dependencies:
;;;   - core/util/pretty-class.ss
;;;   - Various lattice modules

(load "core/util/pretty-class.ss")

;;; ====
;;; Vec2 Pretty (tagged list: (vec2 x y))
;;; ====
;;;
;;; Note: Scheme vectors use scheme-vec-pretty from core/util/pretty-class.ss
;;; Vec2/Vec3 are tagged lists like (vec2 x y), not Scheme vectors.

;;; vec2-pretty : Vec2 → Doc
;;; Pretty-print a 2D vector as [x, y]
(define (vec2-pretty v)
  (brackets
   (<> (text (number->string (cadr v)))
       (<> (text ", ")
           (text (number->string (caddr v)))))))

;;; vec2-pretty-prec : Int → Vec2 → Doc
(define (vec2-pretty-prec prec v)
  (vec2-pretty v))

;;; ====
;;; Vec3 Pretty
;;; ====

;;; vec3-pretty : Vec3 → Doc
;;; Pretty-print a 3D vector as [x, y, z]
(define (vec3-pretty v)
  (brackets
   (<> (text (number->string (cadr v)))
       (<> (text ", ")
           (<> (text (number->string (caddr v)))
               (<> (text ", ")
                   (text (number->string (cadddr v)))))))))

;;; vec3-pretty-prec : Int → Vec3 → Doc
(define (vec3-pretty-prec prec v)
  (vec3-pretty v))

;;; ====
;;; Matrix Pretty
;;; ====

;;; matrix-pretty : Matrix → Doc
;;; Pretty-print a matrix with aligned columns.
;;; For small matrices, show full content; for large, show dimensions.
(define (matrix-pretty m)
  (let* ([rows (cadr m)]
         [cols (caddr m)]
         [data (cadddr m)])
    (if (or (> rows 10) (> cols 10))
        ;; Large matrix: just show dimensions
        (angles (<> (text (number->string rows))
                    (<> (text "×")
                        (text (number->string cols)))))
        ;; Small matrix: show content
        (let ([row-docs
               (let build-rows ([r 0] [acc '()])
                 (if (>= r rows)
                     (reverse acc)
                     (let ([row-elems
                            (let build-cols ([c 0] [cols-acc '()])
                              (if (>= c cols)
                                  (reverse cols-acc)
                                  (let ([idx (+ (* r cols) c)]
                                        [val (vector-ref data (+ (* r cols) c))])
                                    (build-cols (+ c 1)
                                                (cons (text (number->string val)) cols-acc)))))])
                       (build-rows (+ r 1)
                                   (cons (brackets (hsep row-elems)) acc)))))])
          (group (vsep row-docs))))))

;;; matrix-pretty-prec : Int → Matrix → Doc
(define (matrix-pretty-prec prec m)
  (matrix-pretty m))

;;; ====
;;; Complex Number Pretty
;;; ====

;;; complex-pretty : Complex → Doc
;;; Pretty-print a complex number as a + bi
(define (complex-pretty c)
  (let ([real (cadr c)]
        [imag (caddr c)])
    (cond
     [(= imag 0)
      (text (number->string real))]
     [(= real 0)
      (<> (text (number->string imag)) (text "i"))]
     [(< imag 0)
      (<> (text (number->string real))
          (<> (text " - ")
              (<> (text (number->string (- imag))) (text "i"))))]
     [else
      (<> (text (number->string real))
          (<> (text " + ")
              (<> (text (number->string imag)) (text "i"))))])))

;;; complex-pretty-prec : Int → Complex → Doc
(define (complex-pretty-prec prec c)
  ;; Complex literals are atoms, no precedence needed
  (complex-pretty c))

;;; ====
;;; Polynomial Pretty
;;; ====

;;; polynomial-pretty : Polynomial → Doc
;;; Pretty-print a polynomial in standard mathematical notation.
;;; e.g., x^2 + 3x + 5
(define (polynomial-pretty p)
  (let* ([coeffs (caddr p)]
         [degree (- (length coeffs) 1)])
    (if (or (null? coeffs) (= degree -1))
        (text "0")
        (let ([terms
               (let build-terms ([i degree] [acc '()])
                 (if (< i 0)
                     acc
                     (let ([coeff (list-ref coeffs i)])
                       (if (= coeff 0)
                           (build-terms (- i 1) acc)
                           (let ([term-doc
                                  (cond
                                   [(= i 0) (text (number->string coeff))]
                                   [(= i 1)
                                    (if (= coeff 1)
                                        (text "x")
                                        (<> (text (number->string coeff)) (text "x")))]
                                   [else
                                    (if (= coeff 1)
                                        (<> (text "x^") (text (number->string i)))
                                        (<> (text (number->string coeff))
                                            (<> (text "x^") (text (number->string i)))))])])
                             (build-terms (- i 1) (cons term-doc acc)))))))])
          (if (null? terms)
              (text "0")
              (hsep (punctuate (text " +") terms)))))))

;;; polynomial-pretty-prec : Int → Polynomial → Doc
(define (polynomial-pretty-prec prec p)
  ;; Add parens if we're in a tighter context
  (parens-if (> prec prec-add) (polynomial-pretty p)))

;;; ====
;;; Symbolic Expression Pretty (with precedence)
;;; ====

;;; expr-pretty : Expr → Doc
;;; Pretty-print a symbolic expression.
(define (expr-pretty e)
  (expr-pretty-prec prec-top e))  ; Start with lowest precedence (0)

;;; expr-pretty-prec : Int → Expr → Doc
;;; Pretty-print with precedence tracking for minimal parentheses.
;;; Higher prec = tighter binding. Add parens when context is tighter than operator.
(define (expr-pretty-prec prec e)
  (cond
   ;; Numeric constant
   [(and (pair? e) (eq? (car e) 'num))
    (text (number->string (cadr e)))]
   ;; Variable
   [(and (pair? e) (eq? (car e) 'var))
    (text (symbol->string (cadr e)))]
   ;; Sum
   [(and (pair? e) (eq? (car e) '+))
    (let ([terms (cdr e)])
      (parens-if (> prec prec-add)
        (group (hsep (punctuate (text " +") (map (lambda (t) (expr-pretty-prec prec-add t)) terms))))))]
   ;; Product
   [(and (pair? e) (eq? (car e) '*))
    (let ([factors (cdr e)])
      (parens-if (> prec prec-mul)
        (group (hsep (punctuate (text " *") (map (lambda (f) (expr-pretty-prec prec-mul f)) factors))))))]
   ;; Difference
   [(and (pair? e) (eq? (car e) '-))
    (if (= (length e) 2)
        ;; Unary negation
        (<> (text "-") (expr-pretty-prec prec-atom (cadr e)))
        ;; Binary subtraction
        (parens-if (> prec prec-add)
          (<> (expr-pretty-prec prec-add (cadr e))
              (<> (text " - ")
                  (expr-pretty-prec (+ prec-add 1) (caddr e))))))]
   ;; Division
   [(and (pair? e) (eq? (car e) '/))
    (parens-if (> prec prec-mul)
      (<> (expr-pretty-prec prec-mul (cadr e))
          (<> (text " / ")
              (expr-pretty-prec (+ prec-mul 1) (caddr e)))))]
   ;; Power
   [(and (pair? e) (eq? (car e) '^))
    (parens-if (> prec prec-exp)
      (<> (expr-pretty-prec prec-exp (cadr e))
          (<> (text "^")
              (expr-pretty-prec prec-exp (caddr e)))))]
   ;; Function application (handles multi-arg: (fn arg1 arg2 ...))
   [(and (pair? e) (symbol? (car e)))
    (let ([args (cdr e)])
      (<> (text (symbol->string (car e)))
          (parens
           (if (null? args)
               empty
               (hsep (punctuate (text ",")
                                (map (lambda (a) (expr-pretty-prec prec-top a)) args)))))))]
   ;; Fallback
   [else (text "?")]))

;;; ====
;;; Block Pretty (CAS primitive)
;;; ====

;;; block-pretty : Block → Doc
;;; Pretty-print a CAS block as {tag payload refs...}
(define (block-pretty b)
  (if (and (pair? b) (eq? (car b) 'block))
      (let ([tag (cadr b)]
            [payload (caddr b)]
            [refs (cadddr b)])
        (braces
         (group
          (<> (text (symbol->string tag))
              (<> (text " ")
                  (<> (text (if (bytevector? payload)
                                (string-append "#vu8[" (number->string (bytevector-length payload)) "]")
                                "?"))
                      (if (null? refs)
                          empty
                          (<> (text " ")
                              (brackets
                               (hsep (map (lambda (r) (text (if (string? r) (substring r 0 8) "?"))) refs)))))))))))
      (text "<invalid-block>")))

;;; block-pretty-prec : Int → Block → Doc
(define (block-pretty-prec prec b)
  (block-pretty b))

;;; ====
;;; Pretty Dispatch Registration
;;; ====
;;;
;;; Register the lattice-aware dispatch function so list-pretty and
;;; scheme-vec-pretty can use Pretty instances for lattice types.

;;; lattice-pretty-dispatch : Any → Doc
;;; Dispatch to appropriate Pretty instance based on value type.
(define (lattice-pretty-dispatch x)
  (cond
   ;; Primitives (from core)
   [(number? x) (text (number->string x))]
   [(boolean? x) (bool-pretty x)]
   [(char? x) (char-pretty x)]
   [(string? x) (string-pretty x)]
   [(symbol? x) (symbol-pretty x)]
   ;; Tagged lattice types
   [(and (pair? x) (symbol? (car x)))
    (case (car x)
      [(vec2) (vec2-pretty x)]
      [(vec3) (vec3-pretty x)]
      [(matrix) (matrix-pretty x)]
      [(complex) (complex-pretty x)]
      [(polynomial) (polynomial-pretty x)]
      [(num var) (expr-pretty x)]
      [(+ * - / ^) (expr-pretty x)]
      [(block) (block-pretty x)]
      [else (sexp->doc x)])]
   ;; Scheme vectors
   [(vector? x) (scheme-vec-pretty x)]
   ;; Nested lists
   [(list? x) (list-pretty x)]
   ;; Fallback
   [else (sexp->doc x)]))

;; Register the dispatch function
(set-pretty-dispatch! lattice-pretty-dispatch)

;;; ====
;;; Module Loading Message
;;; ====

(display "pretty-instances.ss loaded.\n")
(display "  Pretty instances for Vec2, Vec3, Matrix, Complex, Polynomial, Expr, Block.\n")
(display "  Use (vec2-pretty v), (matrix-pretty m), (complex-pretty c), etc.\n")
(display "  Use (expr-pretty e) for symbolic expressions with minimal parens.\n")
(display "  Dispatch registered: list-pretty now uses lattice Pretty instances.\n")
