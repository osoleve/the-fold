(load "core/base/prelude.ss")
(load "core/util/pretty.ss")

(doc 'module 'pretty-class)
(doc 'description "Pretty Type Class Implementation. Implementation functions for the Pretty type class instances. Pretty returns Doc (from pretty.ss) for width-aware layout, unlike Show which returns flat String. This module bridges the type class system with the Wadler-Lindig layout algorithm. Architecture: pretty.ss provides the layout layer with Doc type and combinators; pretty-class.ss provides the type class layer with instance functions and dispatch hooks; lattice/fp/pretty-instances.ss extends with lattice types.")
(doc 'layer 'core)

(doc 'section 'default-width)

(doc *default-pretty-width* 'type Nat)
(doc *default-pretty-width* 'description "Default width for pretty printing values.")
(doc *default-pretty-width* 'export #t)
(define *default-pretty-width* 80)

(doc 'section 'primitive-pretty-implementations)

(define (nat-pretty n)
  (doc 'type (-> Nat Doc))
  (doc 'description "Pretty-print a natural number.")
  (doc 'export #t)
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

(doc 'section 'list-pretty-implementation)

(define (list-pretty-with elem-pretty lst)
  (doc 'type (-> (-> a Doc) (List a) Doc))
  (doc 'description "Pretty-print a list using element pretty-printer.")
  (doc 'export #t)
  (if (null? lst)
      (text "()")
      (parens (sep (map elem-pretty lst)))))

(doc *pretty-dispatch* 'type (Maybe (-> Any Doc)))
(doc *pretty-dispatch* 'description "Hook for lattice modules to register a dispatch function. When set, list-pretty uses this to infer element Pretty instances. Lattice code sets this via set-pretty-dispatch!.")
(doc *pretty-dispatch* 'export #t)
(define *pretty-dispatch* #f)

(define (set-pretty-dispatch! fn)
  (doc 'type (-> (-> Any Doc) Void))
  (doc 'description "Register a dispatch function for Pretty instance inference.")
  (doc 'export #t)
  (set! *pretty-dispatch* fn))

;;; core-elem-pretty : Any → Doc
;;; Core-only element pretty-printer (primitives only).
(define (core-elem-pretty x)
  (cond
   [(number? x) (text (number->string x))]
   [(boolean? x) (bool-pretty x)]
   [(char? x) (char-pretty x)]
   [(string? x) (string-pretty x)]
   [(symbol? x) (symbol-pretty x)]
   [(vector? x) (scheme-vec-pretty x)]
   [(list? x) (list-pretty x)]
   [else (sexp->doc x)]))

;;; infer-elem-pretty : Any → Doc
;;; Infer the appropriate pretty-printer for a value.
;;; Uses *pretty-dispatch* if set (by lattice), otherwise core-only.
(define (infer-elem-pretty x)
  (if *pretty-dispatch*
      (*pretty-dispatch* x)
      (core-elem-pretty x)))

(define (list-pretty lst)
  (doc 'type (-> (List a) Doc))
  (doc 'description "Pretty-print a list, inferring element Pretty instances.")
  (doc 'export #t)
  (if (null? lst)
      (text "()")
      (parens (sep (map infer-elem-pretty lst)))))

;;; list-pretty-prec : Int → (List a) → Doc
(define (list-pretty-prec prec lst)
  (list-pretty lst))

;;; scheme-vec-pretty : Vector → Doc
;;; Pretty-print a Scheme vector as [a, b, c, ...]
(define (scheme-vec-pretty v)
  (let ([elems (vector->list v)])
    (brackets
     (group
      (hsep (punctuate (text ",")
                       (map infer-elem-pretty elems)))))))

;;; sexp->doc-dispatch : Sexp → Doc
;;; Like sexp->doc but uses infer-elem-pretty for nested elements.
;;; This ensures lattice types nested in unknown structures still pretty-print.
(define (sexp->doc-dispatch sexp)
  (cond
   [(null? sexp) (text "()")]
   [(pair? sexp)
    (parens (group (nest 1 (sep (map infer-elem-pretty sexp)))))]
   ;; For atoms, use infer-elem-pretty to get proper Pretty instance
   [else (infer-elem-pretty sexp)]))

(doc 'section 'precedence-helpers)

(doc prec-atom 'type Int)
(doc prec-atom 'description "Precedence level for atoms (numbers, variables) - tightest binding.")
(doc prec-atom 'export #t)
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

(define (parens-if cond doc)
  (doc 'type (-> Bool Doc Doc))
  (doc 'description "Wrap doc in parens if condition is true.")
  (doc 'export #t)
  (if cond (parens doc) doc))

(define (pretty-binop prec op-prec left op right)
  (doc 'type (-> Int Int Doc String Doc Doc))
  (doc 'description "Pretty-print a binary operator with precedence handling. prec is current context precedence, op-prec is operator precedence, left and right are operand Docs, op is operator string.")
  (doc 'export #t)
  (parens-if (> prec op-prec)
    (group (<> left (<> (text (string-append " " op " ")) right)))))

;;; pretty-unop : Int → Int → String → Doc → Doc
;;; Pretty-print a unary operator with precedence handling.
(define (pretty-unop prec op-prec op operand)
  (parens-if (> prec op-prec)
    (<> (text op) operand)))

(doc 'section 'convenience-functions)

(define (pretty-render doc)
  (doc 'type (-> Doc String))
  (doc 'description "Render a Doc to string with default width.")
  (doc 'export #t)
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

(doc 'section 'value-to-string-convenience)

(define (pp-value x)
  (doc 'type (-> Any String))
  (doc 'description "Pretty-print any value to string with default width. Uses dispatch hook if set (for lattice types).")
  (doc 'export #t)
  (pretty-render (infer-elem-pretty x)))

;;; pp-value-width : Nat → Any → String
;;; Pretty-print any value to string with given width.
(define (pp-value-width width x)
  (pretty-render-width width (infer-elem-pretty x)))

;;; pprint-value : Any → Void
;;; Display any value to stdout with default width.
(define (pprint-value x)
  (display (pp-value x))
  (newline))

;;; pprint-value-width : Nat → Any → Void
;;; Display any value to stdout with given width.
(define (pprint-value-width width x)
  (display (pp-value-width width x))
  (newline))

;;; ====
;;; S-expression fallback (already in pretty.ss as sexp->doc)
;;; ====
;;; sexp->doc provides a fallback for any S-expression when
;;; no specific Pretty instance is available.

(display "pretty-class.ss loaded.\n")
(display "  Pretty type class: Nat, Int, Bool, Char, String, Symbol, List.\n")
(display "  Value → String: (pp-value x), (pprint-value x)\n")
(display "  Doc → String: (pretty-render doc), (pretty-display doc)\n")
(display "  Extension: (set-pretty-dispatch! fn) for lattice types.\n")
