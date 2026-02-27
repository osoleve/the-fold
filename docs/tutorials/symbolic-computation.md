# Symbolic Computation Tutorial

This tutorial introduces The Fold's symbolic computation library, which provides
tools for manipulating mathematical expressions symbolically rather than
numerically.

## Overview

The symbolic computation library consists of five main modules:

| Module | Purpose |
|----|----|
| `expr.ss` | Expression representation and manipulation |
| `diff.ss` | Symbolic differentiation |
| `simplify.ss` | Algebraic simplification |
| `integrate.ss` | Symbolic integration |
| `solve.ss` | Symbolic equation solving |

## Getting Started

Load the modules you need:

```scheme
;; For basic expression manipulation
(load "lattice/symbolic/expr.ss")

;; For differentiation (includes expr.ss)
(load "lattice/symbolic/diff.ss")

;; For simplification (includes expr.ss)
(load "lattice/symbolic/simplify.ss")

;; For integration (includes diff.ss and simplify.ss)
(load "lattice/symbolic/integrate.ss")

;; For equation solving (includes expr.ss and simplify.ss)
(load "lattice/symbolic/solve.ss")
```

## Building Expressions

Expressions are built using constructors:

```scheme
;; Numeric constants
(num 5)           ; => (num 5)
(num 3.14)        ; => (num 3.14)

;; Variables
(var 'x)          ; => (var x)
(var 'theta)      ; => (var theta)

;; Arithmetic operations
(sum (var 'x) (num 1))              ; x + 1
(product (num 2) (var 'x))          ; 2 * x
(difference (var 'x) (var 'y))      ; x - y
(quotient (var 'x) (var 'y))        ; x / y
(power (var 'x) (num 2))            ; x^2

;; Functions
(sym-sin (var 'x))    ; sin(x)
(sym-cos (var 'x))    ; cos(x)
(sym-exp (var 'x))    ; e^x
(sym-log (var 'x))    ; ln(x)
(sym-sqrt (var 'x))   ; sqrt(x)
```

### Smart Constructors

The constructors automatically apply basic simplifications:

```scheme
(sum (num 0) (var 'x))        ; => (var x)     [0 + x = x]
(product (num 1) (var 'x))    ; => (var x)     [1 * x = x]
(product (num 0) (var 'x))    ; => (num 0)     [0 * x = 0]
(power (var 'x) (num 0))      ; => (num 1)     [x^0 = 1]
(power (var 'x) (num 1))      ; => (var x)     [x^1 = x]
```

## Symbolic Differentiation

The `diff.ss` module provides the `deriv` function:

```scheme
(load "lattice/symbolic/diff.ss")

;; Basic derivative
(deriv (power (var 'x) (num 2)) 'x)
;; => (* (num 2) (var x))   [d/dx(x^2) = 2x]

;; Chain rule
(deriv (sym-sin (power (var 'x) (num 2))) 'x)
;; => (* (cos (^ (var x) (num 2))) (* (num 2) (var x)))
;; [d/dx(sin(x^2)) = cos(x^2) * 2x]

;; Product rule
(deriv (product (var 'x) (sym-sin (var 'x))) 'x)
;; => (+ (* (num 1) (sin (var x))) (* (var x) (cos (var x))))
;; [d/dx(x*sin(x)) = sin(x) + x*cos(x)]
```

### Higher-Order Derivatives

```scheme
;; Second derivative
(deriv (deriv (power (var 'x) (num 3)) 'x) 'x)
;; => (* (num 6) (var x))   [d^2/dx^2(x^3) = 6x]

;; Or use deriv-n
(deriv-n (power (var 'x) (num 4)) 'x 2)
;; => (* (num 12) (^ (var x) (num 2)))   [d^2/dx^2(x^4) = 12x^2]
```

### Gradients and Jacobians

```scheme
;; Gradient of f(x,y) = x^2 + y^2
(gradient (sum (power (var 'x) (num 2))
               (power (var 'y) (num 2)))
          '(x y))
;; => ((* (num 2) (var x)) (* (num 2) (var y)))

;; Jacobian of vector function
(jacobian (list (sum (var 'x) (var 'y))
                (product (var 'x) (var 'y)))
          '(x y))
;; => (((num 1) (num 1))
;;     ((var y) (var x)))
```

## Algebraic Simplification

The `simplify.ss` module provides comprehensive simplification:

```scheme
(load "lattice/symbolic/simplify.ss")

;; Collect like terms
(simplify (sum (var 'x) (var 'x)))
;; => (* (num 2) (var x))   [x + x = 2x]

(simplify (sum (product (num 2) (var 'x))
               (product (num 3) (var 'x))))
;; => (* (num 5) (var x))   [2x + 3x = 5x]

;; Combine powers
(simplify (product (var 'x) (var 'x)))
;; => (^ (var x) (num 2))   [x * x = x^2]

(simplify (product (power (var 'x) (num 2))
                   (power (var 'x) (num 3))))
;; => (^ (var x) (num 5))   [x^2 * x^3 = x^5]

;; Numeric evaluation
(simplify (sum (num 2) (num 3)))
;; => (num 5)

(simplify (power (num 2) (num 3)))
;; => (num 8)
```

### Trigonometric Identities

```scheme
;; Pythagorean identity
(simplify-trig (sum (power (sym-sin (var 'x)) (num 2))
                    (power (sym-cos (var 'x)) (num 2))))
;; => (num 1)   [sin^2(x) + cos^2(x) = 1]

;; sin/cos = tan
(simplify-trig (quotient (sym-sin (var 'x))
                         (sym-cos (var 'x))))
;; => (tan (var x))
```

### Exponential and Logarithm Rules

```scheme
;; exp(log(x)) = x
(simplify (sym-exp (sym-log (var 'x))))
;; => (var x)

;; log(x^n) = n*log(x)
(simplify (sym-log (power (var 'x) (num 3))))
;; => (* (num 3) (log (var x)))
```

### Expansion and Factoring

```scheme
;; Expand products
(expand (product (sum (var 'a) (var 'b))
                 (sum (var 'c) (var 'd))))
;; Expands to ac + ad + bc + bd

;; Factor common terms
(factor (sum (product (num 2) (var 'x))
             (product (num 2) (var 'y))))
;; => (* (num 2) (+ (var x) (var y)))   [2x + 2y = 2(x+y)]

;; Difference of squares
(factor (difference (power (var 'x) (num 2))
                    (power (var 'y) (num 2))))
;; => (* (+ (var x) (var y)) (- (var x) (var y)))
;; [x^2 - y^2 = (x+y)(x-y)]
```

## Symbolic Integration

The `integrate.ss` module computes antiderivatives:

```scheme
(load "lattice/symbolic/integrate.ss")

;; Power rule
(integrate (power (var 'x) (num 2)) 'x)
;; => (/ (^ (var x) (num 3)) (num 3))   [integral of x^2 = x^3/3]

;; Trigonometric functions
(integrate (sym-sin (var 'x)) 'x)
;; => (- (cos (var x)))   [integral of sin(x) = -cos(x)]

(integrate (sym-cos (var 'x)) 'x)
;; => (sin (var x))   [integral of cos(x) = sin(x)]

;; Exponential
(integrate (sym-exp (var 'x)) 'x)
;; => (exp (var x))   [integral of e^x = e^x]

;; Logarithm
(integrate (quotient (num 1) (var 'x)) 'x)
;; => (log (abs (var x)))   [integral of 1/x = ln|x|]

;; Natural log
(integrate (sym-log (var 'x)) 'x)
;; => (- (* (var x) (log (var x))) (var x))
;; [integral of ln(x) = x*ln(x) - x]
```

### Definite Integrals

```scheme
;; Definite integral from 0 to 1
(definite-integral (var 'x) 'x (num 0) (num 1))
;; => (num 1/2)   [integral from 0 to 1 of x dx = 1/2]

;; With symbolic bounds
(definite-integral (power (var 'x) (num 2)) 'x (num 0) (var 'a))
;; => (/ (^ (var a) (num 3)) (num 3))
```

### Integration Rules

The integrator applies several rules:

```scheme
;; Sum rule
(integrate (sum (var 'x) (power (var 'x) (num 2))) 'x)
;; Integrates each term separately

;; Constant multiple
(integrate (product (num 5) (sym-sin (var 'x))) 'x)
;; => (* (num 5) (- (cos (var x))))

;; Linear substitution
(integrate (power (sum (product (num 2) (var 'x)) (num 1)) (num 3)) 'x)
;; Handles f(ax+b) => F(ax+b)/a
```

## Symbolic Equation Solving

The `solve.ss` module provides symbolic equation solving:

```scheme
(load "lattice/symbolic/solve.ss")

;; Solve linear equation: 2x + 4 = 0
(solve-for (sum (product (num 2) (var 'x)) (num 4)) 'x)
;; => (num -2)   [x = -2]

;; Solve quadratic: x^2 - 5x + 6 = 0
(solve (sum (power (var 'x) (num 2))
            (sum (product (num -5) (var 'x)) (num 6)))
       'x)
;; => ((num 2) (num 3))   [x = 2 or x = 3]

;; Create equations explicitly
(define eq (make-equation (product (num 2) (var 'x)) (num 6)))
(solve-for eq 'x)
;; => (num 3)   [2x = 6 => x = 3]
```

### Polynomial Analysis

```scheme
;; Check if expression is polynomial
(is-polynomial? (sum (var 'x) (num 1)) 'x)
;; => #t

(is-polynomial? (sym-sin (var 'x)) 'x)
;; => #f

;; Get polynomial degree
(polynomial-degree (power (var 'x) (num 3)) 'x)
;; => 3

;; Extract coefficients (constant term first)
(extract-poly-coefficients
  (sum (power (var 'x) (num 2))
       (sum (product (num 2) (var 'x)) (num 3)))
  'x)
;; => ((num 3) (num 2) (num 1))   [3 + 2x + x^2]
```

### Linear Systems

Solve systems of linear equations via Gaussian elimination:

```scheme
;; System: x + y = 3, x - y = 1
(solve-linear-system
  (list (difference (sum (var 'x) (var 'y)) (num 3))
        (difference (difference (var 'x) (var 'y)) (num 1)))
  '(x y))
;; => ((x . (num 2)) (y . (num 1)))
;; [x = 2, y = 1]

;; System: 2x + 3y = 8, 4x - y = 2
(solve-linear-system
  (list (difference (sum (product (num 2) (var 'x))
                         (product (num 3) (var 'y)))
                    (num 8))
        (difference (difference (product (num 4) (var 'x))
                                (var 'y))
                    (num 2)))
  '(x y))
;; => ((x . (num 1)) (y . (num 2)))
```

### Cubic Equations

The solver uses Cardano's formula for cubic equations:

```scheme
;; x^3 - 6x^2 + 11x - 6 = 0  [roots: 1, 2, 3]
(solve (sum (power (var 'x) (num 3))
            (sum (product (num -6) (power (var 'x) (num 2)))
                 (sum (product (num 11) (var 'x))
                      (num -6))))
       'x)
;; Returns list of solutions (may include symbolic sqrt expressions)
```

## Verification: Differentiate the Integral

You can verify integration results by differentiating:

```scheme
(let* ([f (power (var 'x) (num 2))]
       [F (integrate f 'x)]
       [dF (deriv F 'x)]
       [simplified (simplify dF)])
      (expr=? simplified f))
;; => #t   [d/dx(integral of x^2) = x^2]
```

## Complete Example: Taylor Series

Here's a practical example computing Taylor series terms:

```scheme
(load "lattice/symbolic/integrate.ss")

;; Taylor series term: f^(n)(a) * (x-a)^n / n!
(define (taylor-term f x-var a n)
  (let* ([nth-deriv (deriv-n f x-var n)]
         [at-a (subst nth-deriv x-var a)]
         [x-minus-a (difference (var x-var) a)]
         [term (quotient (product at-a (power x-minus-a (num n)))
                         (num (factorial n)))])
        (simplify term)))

(define (factorial n)
  (if (<= n 1) 1 (* n (factorial (- n 1)))))

;; Taylor series of e^x around 0 (first 4 terms)
;; e^x = 1 + x + x^2/2 + x^3/6 + ...
(define ex (sym-exp (var 'x)))
(taylor-term ex 'x (num 0) 0)  ; => (num 1)
(taylor-term ex 'x (num 0) 1)  ; => (var x)
(taylor-term ex 'x (num 0) 2)  ; => (/ (^ (var x) (num 2)) (num 2))
(taylor-term ex 'x (num 0) 3)  ; => (/ (^ (var x) (num 3)) (num 6))
```

## Tips and Best Practices

1. **Use simplify liberally**: After complex operations, call `simplify` to keep
   expressions manageable.

2. **Check for #f**: The `integrate` function returns `#f` when it can't find
   an antiderivative.

3. **Combine with autodiff**: Use `lattice/autodiff/symbolic-diff.ss` to bridge
   symbolic and numeric computation.

4. **Test with verification**: Always verify integration by differentiating the
   result when developing.

## Further Reading

- `lattice/symbolic/README.sexp` - Complete API reference
- `lattice/symbolic/test-*.ss` - Test files show many usage examples
- `lattice/autodiff/` - Automatic differentiation for numeric gradients
