(define (test-func x)
  (let ([n (modulo x 3)])
       (case n
             [(0) 'zero]
             [(1) 'one]
             [(2) 'two])))

(display (test-func 5))
(newline)
(display (test-func 6))
(newline)
(display (test-func 7))
(newline)
