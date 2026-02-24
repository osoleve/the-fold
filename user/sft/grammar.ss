;;; user/sft/grammar.ss — Generative grammar data types
;;;
;;; A grammar is an alist mapping rule names (symbols) to expansion bodies.
;;; Bodies are s-expressions built from combinators:
;;;   "literal"           → text
;;;   (ref rule)          → expand named rule
;;;   (slot key)          → look up key in context alist
;;;   (seq a b ...)       → concatenate expansions
;;;   (alt a b ...)       → uniform random choice
;;;   (weighted (p a) ..) → weighted random choice
;;;   (maybe prob body)   → include with probability prob
;;;   (when key body)     → include only if key is truthy in context
;;;   (case-on key (v1 b1) (v2 b2) ...) → switch on context value
;;;   (join sep rule)     → expand rule (must yield list), join with sep

(load "core/lang/module.ss")

;;; --- Data type ---

(define (make-grammar rules)
  (doc 'type '(-> Alist Grammar))
  (doc 'description "Create a grammar from an alist of (name . body) rules")
  (list 'grammar rules))

(define (grammar? x)
  (and (pair? x) (eq? (car x) 'grammar)))

(define (grammar-rules g)
  (doc 'type '(-> Grammar Alist))
  (cadr g))

(define (grammar-ref g rule-name)
  (doc 'type '(-> Grammar Symbol Body))
  (doc 'description "Look up a rule by name; error if missing")
  (let ([entry (assq rule-name (grammar-rules g))])
    (if entry
        (cdr entry)
        (error 'grammar-ref "undefined rule" rule-name))))

(define (grammar-has-rule? g rule-name)
  (doc 'type '(-> Grammar Symbol Boolean))
  (if (assq rule-name (grammar-rules g)) #t #f))

(define (grammar-rule-names g)
  (doc 'type '(-> Grammar (List Symbol)))
  (map car (grammar-rules g)))

;;; --- Inheritance ---

(define (grammar-merge parent child)
  (doc 'type '(-> Grammar Grammar Grammar))
  (doc 'description "Merge grammars; child rules shadow parent rules")
  (let ([child-rules (grammar-rules child)]
        [parent-rules (grammar-rules parent)])
    (make-grammar
      (append child-rules
              (filter (lambda (r) (not (assq (car r) child-rules)))
                      parent-rules)))))
