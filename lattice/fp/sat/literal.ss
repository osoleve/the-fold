;;; @module sat/literal
;;; @requires prelude

(require 'prelude)

(doc 'module 'literal)
(doc 'description "SAT literal representation - a variable with polarity")
(doc 'layer 'lattice)
(doc 'purity 'total)
(doc 'note "Literals are represented as signed integers: positive = true, negative = false")
(doc 'note "Variable 0 is reserved. Variables are numbered 1, 2, 3, ...")

(doc 'section 'literal-constructors)

(define (make-lit var positive?)
  (doc 'export #t)
  (doc 'type '(-> VarId Bool Literal))
  (doc 'description "Create a literal from variable id and polarity")
  (if positive? var (- var)))

(define (pos-lit var)
  (doc 'export #t)
  (doc 'type '(-> VarId Literal))
  (doc 'description "Create positive literal x")
  var)

(define (neg-lit var)
  (doc 'export #t)
  (doc 'type '(-> VarId Literal))
  (doc 'description "Create negative literal ~x")
  (- var))

(doc 'section 'literal-accessors)

(define (lit-var lit)
  (doc 'export #t)
  (doc 'type '(-> Literal VarId))
  (doc 'description "Get the variable id from a literal")
  (abs lit))

(define (lit-positive? lit)
  (doc 'export #t)
  (doc 'type '(-> Literal Bool))
  (doc 'description "Check if literal is positive (unnegated)")
  (> lit 0))

(define (lit-negative? lit)
  (doc 'export #t)
  (doc 'type '(-> Literal Bool))
  (doc 'description "Check if literal is negative (negated)")
  (< lit 0))

(define (lit-negate lit)
  (doc 'export #t)
  (doc 'type '(-> Literal Literal))
  (doc 'description "Negate a literal: x -> ~x, ~x -> x")
  (- lit))

(doc 'section 'literal-comparison)

(define (lit=? l1 l2)
  (doc 'export #t)
  (doc 'type '(-> Literal Literal Bool))
  (doc 'description "Check if two literals are equal")
  (= l1 l2))

(define (lit-complement? l1 l2)
  (doc 'export #t)
  (doc 'type '(-> Literal Literal Bool))
  (doc 'description "Check if literals are complements (x and ~x)")
  (= l1 (- l2)))

(doc 'section 'literal-display)

(define (lit->string lit)
  (doc 'export #t)
  (doc 'type '(-> Literal String))
  (doc 'description "Convert literal to string representation")
  (if (lit-positive? lit)
      (string-append "x" (number->string lit))
      (string-append "~x" (number->string (- lit)))))

(define (lit->symbol-string lit var-names)
  (doc 'export #t)
  (doc 'type '(-> Literal (Vector String) String))
  (doc 'description "Convert literal to string using variable names")
  (let* ([var (lit-var lit)]
         [name (if (and var-names (< var (vector-length var-names)))
                   (vector-ref var-names var)
                   (string-append "x" (number->string var)))])
        (if (lit-positive? lit)
            name
            (string-append "~" name))))
