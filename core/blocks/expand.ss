(load "core/base/prelude.ss")

(doc 'module 'expand)
(doc 'description "De Bruijn expansion with symbol supply. Inverse of normalize.")
(doc 'layer 'core)

(doc 'section 'symbol-supply)

(define (supply-next supply)
  (doc 'type (-> Supply (Values Symbol Supply)))
  (doc 'description "Take the next symbol from the supply.")
  (doc 'export #t)
  (values (car supply) (cdr supply)))

(doc 'section 'expansion)

(define (expand expr symbols)
  (doc 'type (-> Any (List Symbol) Any))
  (doc 'description "Convert de Bruijn form back to named form.")
  (doc 'export #t)
  (let-values ([(result _) (expand-with-ctx expr '() symbols)])
              result))

(define (expand-with-ctx expr ctx supply)
  (doc 'type (-> Any (List Symbol) Supply (Values Any Supply)))
  (doc 'description "Expand expression using context and symbol supply.")
  (doc 'export #t)
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

(doc 'section 'symbol-generator)

(define (make-symbol-supply n)
  (doc 'type (-> Nat (List Symbol)))
  (doc 'description "Generate list of symbols: x, y, z, x1, y1, z1, ...")
  (doc 'export #t)
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

(define (expand-fresh expr)
  (doc 'type (-> Any Any))
  (doc 'description "Expand using a fresh symbol supply (convenience function).")
  (doc 'export #t)
  (expand expr (make-symbol-supply 100)))
