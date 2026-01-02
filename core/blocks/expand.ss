;;; core/blocks/expand.ss — De Bruijn expansion with symbol supply
;;; @module expand
;;; @requires prelude
;;;
;;; The inverse of normalize: given a de Bruijn form and a supply
;;; of symbols, produce an S-expression with named variables.
;;;
;;; (expand '(fn (dv 0)) '(x))       → (fn (x) x)
;;; (expand '(fn (fn (dv 1))) '(x y)) → (fn (x) (fn (y) x))
;;;
;;; Capture avoidance: expansion must not choose binder names that
;;; capture free variables in the body. The symbol supply should
;;; avoid names that appear free in the expression.
;;;
;;; This is Core code: pure, total, assumes perfect input.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ============================================================
;;; Symbol Supply
;;; ============================================================

;;; A symbol supply is a list. We consume symbols as we encounter binders.

;;; supply-next : Supply → (Values Symbol Supply)
;;; Take the next symbol from the supply.
(define (supply-next supply)
  (values (car supply) (cdr supply)))

;;; ============================================================
;;; Expansion
;;; ============================================================

;;; expand : S-expr × (List Symbol) → S-expr
;;; Convert de Bruijn form back to named form.
(define (expand expr symbols)
  (let-values ([(result _) (expand-with-ctx expr '() symbols)])
              result))

;;; expand-with-ctx : S-expr × (List Symbol) × Supply → (Values S-expr Supply)
;;; Expand expression using context and symbol supply. Context is a list mapping de Bruijn index to symbol (innermost first).
(define (expand-with-ctx expr ctx supply)
  (cond
   ;; (dv n) → the symbol at index n in context
   [(and (pair? expr) (eq? (car expr) 'dv) (number? (cadr expr)))
    (values (list-ref ctx (cadr expr)) supply)]
   
   ;; Symbols (free variables) pass through
   [(symbol? expr) (values expr supply)]
   
   ;; Non-list atoms pass through
   [(not (pair? expr)) (values expr supply)]
   
   ;; (fn body) → (fn (var) expanded-body)
   [(and (eq? (car expr) 'fn)
         (pair? (cdr expr))
         (null? (cddr expr)))
    (let-values ([(var new-supply) (supply-next supply)])
                (let-values ([(body final-supply)
                              (expand-with-ctx (cadr expr) (cons var ctx) new-supply)])
                            (values `(fn (,var) ,body) final-supply)))]
   
   ;; (let (val) body) → (let ((var val)) expanded-body)
   [(and (eq? (car expr) 'let)
         (pair? (cdr expr))
         (pair? (cadr expr))
         (null? (cdadr expr)))
    (let-values ([(var new-supply) (supply-next supply)])
                (let-values ([(val supply2) (expand-with-ctx (caadr expr) ctx new-supply)])
                            (let-values ([(body final-supply)
                                          (expand-with-ctx (caddr expr) (cons var ctx) supply2)])
                                        (values `(let ((,var ,val)) ,body) final-supply))))]
   
   ;; (fix body) → (fix (f) expanded-body)
   [(and (eq? (car expr) 'fix)
         (pair? (cdr expr))
         (null? (cddr expr)))
    (let-values ([(f new-supply) (supply-next supply)])
                (let-values ([(body final-supply)
                              (expand-with-ctx (cadr expr) (cons f ctx) new-supply)])
                            (values `(fix (,f) ,body) final-supply)))]
   
   ;; (quote datum) → unchanged
   [(eq? (car expr) 'quote) (values expr supply)]
   
   ;; General list: expand each element
   [else
    (let loop ([items expr] [acc '()] [sup supply])
         (if (null? items)
             (values (reverse acc) sup)
             (let-values ([(expanded new-sup) (expand-with-ctx (car items) ctx sup)])
                         (loop (cdr items) (cons expanded acc) new-sup))))]))

;;; ============================================================
;;; Default Symbol Generator
;;; ============================================================

;;; make-symbol-supply : Nat → (List Symbol)
;;; Generate a list of symbols: x, y, z, x1, y1, z1, ...
(define (make-symbol-supply n)
  (let ([bases '("x" "y" "z" "a" "b" "c" "f" "g" "h")])
       (let loop ([i 0] [acc '()])
            (if (>= i n)
                (reverse acc)
                (let* ([cycle (quotient i (length bases))]
                       [base (list-ref bases (modulo i (length bases)))]
                       [name (if (= cycle 0)
                                 base
                                 (string-append base (number->string cycle)))])
                      (loop (+ i 1) (cons (string->symbol name) acc)))))))

;;; expand-fresh : S-expr → S-expr
;;; Expand using a fresh symbol supply (convenience function).
(define (expand-fresh expr)
  (expand expr (make-symbol-supply 100)))
