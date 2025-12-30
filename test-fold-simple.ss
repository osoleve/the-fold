(load "fabric/stitches/prelude.ss")

(display "Testing fold-left...\n")

(define result
  (fold-left (lambda (acc elem)
                     (display "acc: ") (display acc) (newline)
                     (display "elem: ") (display elem) (newline)
                     (cons elem acc))
             '()
             '(1 2 3)))

(display "Result: ") (display result) (newline)

; Now test with pairs
(define var-names '(x y))
(define values '(5 10))

(define pairs
  (let loop ([names var-names] [vals values])
       (if (null? names)
           '()
           (cons (cons (car names) (car vals))
                 (loop (cdr names) (cdr vals))))))

(display "\nPairs: ") (display pairs) (newline)

(define result2
  (fold-left (lambda (acc pair)
                     (display "Processing pair: ") (display pair) (newline)
                     (cons pair acc))
             '()
             pairs))

(display "Result2: ") (display result2) (newline)
