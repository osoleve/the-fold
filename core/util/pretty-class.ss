;;; core/util/pretty-class.ss — Pretty Type Class Implementation
;;; @module pretty-class
;;; @requires prelude pretty
;;;
;;; Implementation functions for the Pretty type class instances.
;;; Pretty returns Doc (from pretty.ss) for width-aware layout,
;;; unlike Show which returns flat String.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - pretty.ss (for Doc type and combinators)

(load "core/base/prelude.ss")
(load "core/util/pretty.ss")

;;; ====
;;; Default Width
;;; ====

(define *default-pretty-width* 80)

;;; ====
;;; Primitive Pretty Implementations
;;; ====

;;; nat-pretty : Nat → Doc
(define (nat-pretty n)
  (text (number->string n)))

;;; nat-pretty-prec : Int → Nat → Doc
;;; Precedence doesn't affect atoms
(define (nat-pretty-prec prec n)
  (nat-pretty n))

;;; int-pretty : Int → Doc
(define (int-pretty n)
  (text (number->string n)))

;;; int-pretty-prec : Int → Int → Doc
(define (int-pretty-prec prec n)
  (int-pretty n))

;;; bool-pretty : Bool → Doc
(define (bool-pretty b)
  (text (if b "#t" "#f")))

;;; bool-pretty-prec : Int → Bool → Doc
(define (bool-pretty-prec prec b)
  (bool-pretty b))

;;; char-pretty : Char → Doc
(define (char-pretty c)
  (text (string-append "#\\" (string c))))

;;; char-pretty-prec : Int → Char → Doc
(define (char-pretty-prec prec c)
  (char-pretty c))

;;; string-pretty : String → Doc
(define (string-pretty s)
  (double-quotes (text s)))

;;; string-pretty-prec : Int → String → Doc
(define (string-pretty-prec prec s)
  (string-pretty s))

;;; symbol-pretty : Symbol → Doc
(define (symbol-pretty s)
  (text (symbol->string s)))

;;; symbol-pretty-prec : Int → Symbol → Doc
(define (symbol-pretty-prec prec s)
  (symbol-pretty s))

;;; ====
;;; List Pretty Implementation
;;; ====

;;; list-pretty : (a → Doc) → (List a) → Doc
;;; Pretty-print a list using element pretty-printer.
(define (list-pretty-with elem-pretty lst)
  (if (null? lst)
      (text "()")
      (parens (sep (map elem-pretty lst)))))

;;; list-pretty : (List a) → Doc
;;; Default implementation using generic sexp->doc fallback.
(define (list-pretty lst)
  (list-pretty-with sexp->doc lst))

;;; list-pretty-prec : Int → (List a) → Doc
(define (list-pretty-prec prec lst)
  (list-pretty lst))

;;; ====
;;; Precedence Helpers (for expressions)
;;; ====
;;;
;;; Precedence levels (higher binds tighter):
;;;   10: atoms (numbers, variables) - tightest
;;;    9: function application
;;;    8: exponentiation (^)
;;;    7: multiplication, division (*, /)
;;;    6: addition, subtraction (+, -)
;;;    5: comparisons (<, >, ==, etc.)
;;;    4: logical and (&&)
;;;    3: logical or (||)
;;;    2: conditional expressions
;;;    0: top level (no context) - loosest

(define prec-atom 10)
(define prec-app 9)
(define prec-exp 8)
(define prec-mul 7)
(define prec-add 6)
(define prec-cmp 5)
(define prec-and 4)
(define prec-or 3)
(define prec-cond 2)
(define prec-top 0)

;;; parens-if : Bool → Doc → Doc
;;; Wrap doc in parens if condition is true.
(define (parens-if cond doc)
  (if cond (parens doc) doc))

;;; pretty-binop : Int → Int → Doc → String → Doc → Doc
;;; Pretty-print a binary operator with precedence handling.
;;; prec: current context precedence
;;; op-prec: operator precedence
;;; left, right: operand Docs
;;; op: operator string
(define (pretty-binop prec op-prec left op right)
  (parens-if (> prec op-prec)
    (group (<> left (<> (text (string-append " " op " ")) right)))))

;;; pretty-unop : Int → Int → String → Doc → Doc
;;; Pretty-print a unary operator with precedence handling.
(define (pretty-unop prec op-prec op operand)
  (parens-if (> prec op-prec)
    (<> (text op) operand)))

;;; ====
;;; Convenience Functions
;;; ====

;;; pretty-render : Doc → String
;;; Render a Doc to string with default width.
(define (pretty-render doc)
  (pretty *default-pretty-width* doc))

;;; pretty-render-width : Nat → Doc → String
;;; Render a Doc to string with given width.
(define (pretty-render-width width doc)
  (pretty width doc))

;;; pretty-display : Doc → Void
;;; Display a Doc to stdout with default width.
(define (pretty-display doc)
  (display (pretty-render doc))
  (newline))

;;; pretty-display-width : Nat → Doc → Void
;;; Display a Doc to stdout with given width.
(define (pretty-display-width width doc)
  (display (pretty-render-width width doc))
  (newline))

;;; ====
;;; S-expression fallback (already in pretty.ss as sexp->doc)
;;; ====
;;; sexp->doc provides a fallback for any S-expression when
;;; no specific Pretty instance is available.

;;; ====
;;; Module Loading Message
;;; ====

(display "pretty-class.ss loaded.\n")
(display "  Pretty type class implementations for Nat, Int, Bool, Char, String, Symbol, List.\n")
(display "  Use (pretty-render doc) to render a Doc to string.\n")
(display "  Use (parens-if cond doc) for precedence-aware wrapping.\n")
