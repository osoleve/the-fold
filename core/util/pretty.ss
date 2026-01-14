;;; fabric/stitches/pretty.ss -- Pretty Printing Combinators
;;;
;;; Wadler-Lindig style pretty printer for optimal document layout.
;;; Based on "A Prettier Printer" by Philip Wadler.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Key concepts:
;;;   Doc - Abstract document type
;;;   text - Literal text (no newlines)
;;;   line - Line break (or space when grouped)
;;;   nest - Increase indentation for following lines
;;;   group - Try to flatten to single line if it fits
;;;
;;; The layout algorithm chooses line breaks to fit within a page width.
;;;
;;; Dependencies:
;;;   - prelude.ss

(load "core/base/prelude.ss")

;;; ====
;;; Document Type
;;; ====
;;;
;;; A Doc is one of:
;;;   (doc-empty)           - Empty document
;;;   (doc-text str)        - Literal text (must not contain newlines)
;;;   (doc-line)            - Line break, or space if grouped
;;;   (doc-hardline)        - Line break, never flattened
;;;   (doc-nest n doc)      - Add n spaces to indentation
;;;   (doc-concat d1 d2)    - Concatenate two documents
;;;   (doc-group doc)       - Try to fit on one line
;;;   (doc-union d1 d2)     - Choice: d1 if fits, else d2

;;; --- Constructors ---

;;; doc-empty : → Doc
(define (doc-empty)
  (vector 'doc-empty))

;;; doc-text : String → Doc
(define (doc-text str)
  (vector 'doc-text str))

;;; doc-line : → Doc
(define (doc-line)
  (vector 'doc-line))

;;; doc-hardline : → Doc
(define (doc-hardline)
  (vector 'doc-hardline))

;;; doc-nest : Nat × Doc → Doc
(define (doc-nest n doc)
  (vector 'doc-nest n doc))

;;; doc-concat : Doc × Doc → Doc
(define (doc-concat d1 d2)
  (vector 'doc-concat d1 d2))

;;; doc-union : Doc × Doc → Doc
(define (doc-union d1 d2)
  (vector 'doc-union d1 d2))

;;; --- Predicates ---

;;; doc? : α → Bool
(define (doc? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (if (memq (vector-ref x 0)
                 '(doc-empty doc-text doc-line doc-hardline
                   doc-nest doc-concat doc-group doc-union))
           #t #f)))

;;; doc-empty? : Doc → Bool
(define (doc-empty? d) (eq? (vector-ref d 0) 'doc-empty))
;;; doc-text? : Doc → Bool
(define (doc-text? d) (eq? (vector-ref d 0) 'doc-text))
;;; doc-line? : Doc → Bool
(define (doc-line? d) (eq? (vector-ref d 0) 'doc-line))
;;; doc-hardline? : Doc → Bool
(define (doc-hardline? d) (eq? (vector-ref d 0) 'doc-hardline))
;;; doc-nest? : Doc → Bool
(define (doc-nest? d) (eq? (vector-ref d 0) 'doc-nest))
;;; doc-concat? : Doc → Bool
(define (doc-concat? d) (eq? (vector-ref d 0) 'doc-concat))
;;; doc-group? : Doc → Bool
(define (doc-group? d) (eq? (vector-ref d 0) 'doc-group))
;;; doc-union? : Doc → Bool
(define (doc-union? d) (eq? (vector-ref d 0) 'doc-union))

;;; --- Accessors ---

;;; doc-text-str : Doc → String
(define (doc-text-str d) (vector-ref d 1))
;;; doc-nest-n : Doc → Nat
(define (doc-nest-n d) (vector-ref d 1))
;;; doc-nest-doc : Doc → Doc
(define (doc-nest-doc d) (vector-ref d 2))
;;; doc-concat-left : Doc → Doc
(define (doc-concat-left d) (vector-ref d 1))
;;; doc-concat-right : Doc → Doc
(define (doc-concat-right d) (vector-ref d 2))
;;; doc-group-doc : Doc → Doc
(define (doc-group-doc d) (vector-ref d 1))
;;; doc-group-flat : Doc → Doc
(define (doc-group-flat d) (vector-ref d 2))  ; pre-computed flattened version
;;; doc-union-left : Doc → Doc
(define (doc-union-left d) (vector-ref d 1))
;;; doc-union-right : Doc → Doc
(define (doc-union-right d) (vector-ref d 2))

;;; ====
;;; Flattening Helper (for doc-group construction)
;;; ====
;;;
;;; flatten-inner is used during doc-group construction to pre-compute
;;; the flattened version. It differs from flatten in that it retrieves
;;; the pre-computed flattened version from doc-group nodes rather than
;;; recursing, avoiding exponential re-traversal.

;;; flatten-inner : Doc → Doc
(define (flatten-inner doc)
  (cond
   [(doc-empty? doc) doc]
   [(doc-text? doc) doc]
   [(doc-line? doc) (vector 'doc-text " ")]  ; line becomes space
   [(doc-hardline? doc) doc]  ; hardline never flattens
   [(doc-nest? doc) (vector 'doc-nest (doc-nest-n doc) (flatten-inner (doc-nest-doc doc)))]
   [(doc-concat? doc) (vector 'doc-concat (flatten-inner (doc-concat-left doc))
                              (flatten-inner (doc-concat-right doc)))]
   [(doc-group? doc) (doc-group-flat doc)]  ; use pre-computed flattened version
   [(doc-union? doc) (doc-union-left doc)]  ; take flattened version
   [else doc]))

;;; doc-group : Doc → Doc
;;; Create a group with pre-computed flattened version.
(define (doc-group doc)
  (vector 'doc-group doc (flatten-inner doc)))

;;; ====
;;; Primitive Documents
;;; ====

;;; empty : Doc
;;; The empty document.
(define empty (doc-empty))

;;; text : String → Doc
;;; A literal string (should not contain newlines).
(define (text s)
  (if (string=? s "")
      empty
      (doc-text s)))

;;; line : Doc
;;; A line break. When flattened (in a group), becomes a space.
(define line (doc-line))

;;; hardline : Doc
;;; A line break that never flattens.
(define hardline (doc-hardline))

;;; softline : Doc
;;; A space that can become a line break if needed.
(define softline (doc-group line))

;;; linebreak : Doc
;;; A line break that becomes empty when flattened.
(define linebreak (doc-union (text "") (doc-line)))

;;; ====
;;; Combinators
;;; ====

;;; <> : Doc × Doc → Doc
(define (<> d1 d2)
  (cond
   [(doc-empty? d1) d2]
   [(doc-empty? d2) d1]
   [else (doc-concat d1 d2)]))

;;; <+> : Doc × Doc → Doc
(define (<+> d1 d2)
  (<> d1 (<> (text " ") d2)))

;;; </> : Doc × Doc → Doc
(define (</> d1 d2)
  (<> d1 (<> line d2)))

;;; <//> : Doc × Doc → Doc
(define (<//> d1 d2)
  (<> d1 (<> linebreak d2)))

;;; nest : Nat × Doc → Doc
(define (nest n doc)
  (doc-nest n doc))

;;; group : Doc → Doc
(define (group doc)
  (doc-group doc))

;;; align : Doc → Doc
(define (align doc)
  (nest 0 doc))

;;; hang : Nat × Doc → Doc
(define (hang n doc)
  (align (nest n doc)))

;;; indent : Nat × Doc → Doc
(define (indent n doc)
  (<> (text (make-string n #\space)) (nest n doc)))

;;; ====
;;; List Combinators
;;; ====

;;; concat : (List Doc) → Doc
(define (concat docs)
  (if (null? docs)
      empty
      (fold-left <> (car docs) (cdr docs))))

;;; hcat : (List Doc) → Doc
(define hcat concat)

;;; vcat : (List Doc) → Doc
(define (vcat docs)
  (if (null? docs)
      empty
      (fold-left (lambda (acc d) (<> acc (<> hardline d)))
                 (car docs)
                 (cdr docs))))

;;; hsep : (List Doc) → Doc
(define (hsep docs)
  (if (null? docs)
      empty
      (fold-left <+> (car docs) (cdr docs))))

;;; vsep : (List Doc) → Doc
(define (vsep docs)
  (if (null? docs)
      empty
      (fold-left </> (car docs) (cdr docs))))

;;; sep : (List Doc) → Doc
(define (sep docs)
  (group (vsep docs)))

;;; cat : (List Doc) → Doc
(define (cat docs)
  (group (vcat docs)))

;;; fill-sep : (List Doc) → Doc
(define (fill-sep docs)
  (if (null? docs)
      empty
      (fold-left (lambda (acc d) (<> acc (<> softline d)))
                 (car docs)
                 (cdr docs))))

;;; punctuate : Doc × (List Doc) → (List Doc)
(define (punctuate sep docs)
  (if (or (null? docs) (null? (cdr docs)))
      docs
      (cons (<> (car docs) sep)
            (punctuate sep (cdr docs)))))

;;; ====
;;; Bracketing Combinators
;;; ====

;;; enclose : Doc × Doc × Doc → Doc
(define (enclose left right doc)
  (<> left (<> doc right)))

;;; parens : Doc → Doc
(define (parens doc)
  (enclose (text "(") (text ")") doc))

;;; brackets : Doc → Doc
(define (brackets doc)
  (enclose (text "[") (text "]") doc))

;;; braces : Doc → Doc
(define (braces doc)
  (enclose (text "{") (text "}") doc))

;;; angles : Doc → Doc
(define (angles doc)
  (enclose (text "<") (text ">") doc))

;;; quotes : Doc → Doc
(define (quotes doc)
  (enclose (text "'") (text "'") doc))

;;; double-quotes : Doc → Doc
(define (double-quotes doc)
  (enclose (text "\"") (text "\"") doc))

;;; ====
;;; Flattening (for group)
;;; ====

;;; flatten : Doc → Doc
(define (flatten doc)
  (cond
   [(doc-empty? doc) empty]
   [(doc-text? doc) doc]
   [(doc-line? doc) (text " ")]
   [(doc-hardline? doc) doc]  ; hardline never flattens
   [(doc-nest? doc) (doc-nest (doc-nest-n doc) (flatten (doc-nest-doc doc)))]
   [(doc-concat? doc) (doc-concat (flatten (doc-concat-left doc))
                                  (flatten (doc-concat-right doc)))]
   [(doc-group? doc) (doc-group-flat doc)]  ; use pre-computed flattened version
   [(doc-union? doc) (doc-union-left doc)]  ; take flattened version
   [else doc]))

;;; ====
;;; Layout Algorithm (Simple Mode)
;;; ====
;;;
;;; Simple Doc (for rendering):
;;;   (sdoc-empty)
;;;   (sdoc-text str sdoc)   - text followed by more
;;;   (sdoc-line n sdoc)     - newline + n spaces, followed by more

;;; sdoc-empty : → SDoc
(define (sdoc-empty) (vector 'sdoc-empty))
;;; sdoc-text : String × SDoc → SDoc
(define (sdoc-text str rest) (vector 'sdoc-text str rest))
;;; sdoc-line : Nat × SDoc → SDoc
(define (sdoc-line n rest) (vector 'sdoc-line n rest))

;;; sdoc-empty? : SDoc → Bool
(define (sdoc-empty? sd) (eq? (vector-ref sd 0) 'sdoc-empty))
;;; sdoc-text? : SDoc → Bool
(define (sdoc-text? sd) (eq? (vector-ref sd 0) 'sdoc-text))
;;; sdoc-line? : SDoc → Bool
(define (sdoc-line? sd) (eq? (vector-ref sd 0) 'sdoc-line))

;;; sdoc-text-str : SDoc → String
(define (sdoc-text-str sd) (vector-ref sd 1))
;;; sdoc-text-rest : SDoc → SDoc
(define (sdoc-text-rest sd) (vector-ref sd 2))
;;; sdoc-line-n : SDoc → Nat
(define (sdoc-line-n sd) (vector-ref sd 1))
;;; sdoc-line-rest : SDoc → SDoc
(define (sdoc-line-rest sd) (vector-ref sd 2))

;;; best : Nat × Nat × Doc → SDoc
(define (best w k doc)
  (be w k (list (cons 0 doc))))

;;; be : Nat × Nat × (List (Nat × Doc)) → SDoc
(define (be w k stack)
  (if (null? stack)
      (sdoc-empty)
      (let ([i (car (car stack))]
            [doc (cdr (car stack))]
            [rest (cdr stack)])
           (cond
            [(doc-empty? doc)
             (be w k rest)]
            [(doc-text? doc)
             (let ([s (doc-text-str doc)])
                  (sdoc-text s (be w (+ k (string-length s)) rest)))]
            [(doc-line? doc)
             (sdoc-line i (be w i rest))]
            [(doc-hardline? doc)
             (sdoc-line i (be w i rest))]
            [(doc-nest? doc)
             (be w k (cons (cons (+ i (doc-nest-n doc)) (doc-nest-doc doc)) rest))]
            [(doc-concat? doc)
             (be w k (cons (cons i (doc-concat-left doc))
                           (cons (cons i (doc-concat-right doc)) rest)))]
            [(doc-group? doc)
             (let ([flattened (doc-group-flat doc)])  ; use pre-computed flattened version
                  (if (fits? (- w k) (list (cons i flattened)))
                      (be w k (cons (cons i flattened) rest))
                      (be w k (cons (cons i (doc-group-doc doc)) rest))))]
            [(doc-union? doc)
             (if (fits? (- w k) (list (cons i (doc-union-left doc))))
                 (be w k (cons (cons i (doc-union-left doc)) rest))
                 (be w k (cons (cons i (doc-union-right doc)) rest)))]
            [else (sdoc-empty)]))))

;;; fits? : Nat × (List (Nat × Doc)) → Boolean
(define (fits? w stack)
  (cond
   [(< w 0) #f]
   [(null? stack) #t]
   [else
    (let ([i (car (car stack))]
          [doc (cdr (car stack))]
          [rest (cdr stack)])
         (cond
          [(doc-empty? doc) (fits? w rest)]
          [(doc-text? doc)
           (fits? (- w (string-length (doc-text-str doc))) rest)]
          [(doc-line? doc) #t]  ; line always fits (we stop here)
          [(doc-hardline? doc) #t]
          [(doc-nest? doc)
           (fits? w (cons (cons (+ i (doc-nest-n doc)) (doc-nest-doc doc)) rest))]
          [(doc-concat? doc)
           (fits? w (cons (cons i (doc-concat-left doc))
                          (cons (cons i (doc-concat-right doc)) rest)))]
          [(doc-group? doc)
           (fits? w (cons (cons i (doc-group-flat doc)) rest))]  ; use pre-computed
          [(doc-union? doc)
           (fits? w (cons (cons i (doc-union-left doc)) rest))]
          [else #t]))]))

;;; doc-width : Doc → Nat
(define (doc-width doc)
  (cond
   [(doc-empty? doc) 0]
   [(doc-text? doc) (string-length (doc-text-str doc))]
   [(doc-line? doc) 1]  ; space when flattened
   [(doc-hardline? doc) 0]  ; doesn't flatten
   [(doc-nest? doc) (doc-width (doc-nest-doc doc))]
   [(doc-concat? doc) (+ (doc-width (doc-concat-left doc))
                         (doc-width (doc-concat-right doc)))]
   [(doc-group? doc) (doc-width (doc-group-doc doc))]
   [(doc-union? doc) (doc-width (doc-union-left doc))]
   [else 0]))

;;; ====
;;; Rendering
;;; ====

;;; sdoc->string : SDoc → String
(define (sdoc->string sd)
  (let loop ([sd sd] [acc '()])
       (cond
        [(sdoc-empty? sd) (apply string-append (reverse acc))]
        [(sdoc-text? sd)
         (loop (sdoc-text-rest sd)
               (cons (sdoc-text-str sd) acc))]
        [(sdoc-line? sd)
         (loop (sdoc-line-rest sd)
               (cons (string-append "
" (make-string (sdoc-line-n sd) #\space))
                     acc))]
        [else (apply string-append (reverse acc))])))

;;; pretty : Nat × Doc → String
(define (pretty width doc)
  (sdoc->string (best width 0 doc)))

;;; pretty-print : Nat × Doc → Void
(define (pretty-print width doc)
  (display (pretty width doc))
  (newline))

;;; ====
;;; Default Width
;;; ====

(define *default-width* 80)

;;; pp : Doc → String
(define (pp doc)
  (pretty *default-width* doc))

;;; pprint : Doc → Void
(define (pprint doc)
  (pretty-print *default-width* doc))

;;; ====
;;; Convenience: S-expression Pretty Printing
;;; ====

;;; sexp->doc : Sexp → Doc
(define (sexp->doc sexp)
  (cond
   [(null? sexp) (text "()")]
   [(pair? sexp)
    (parens (group (nest 1 (sep (map sexp->doc sexp)))))]
   [(symbol? sexp) (text (symbol->string sexp))]
   [(number? sexp) (text (number->string sexp))]
   [(string? sexp) (double-quotes (text sexp))]
   [(boolean? sexp) (text (if sexp "#t" "#f"))]
   [(vector? sexp) (text "#(...)")]  ; simplified
   [else (text "?")]))

;;; pretty-sexp : Nat × Sexp → String
(define (pretty-sexp width sexp)
  (pretty width (sexp->doc sexp)))

;;; ====
;;; Export Summary
;;; ====
;;;
;;; Types:
;;;   doc?, doc-empty?, doc-text?, doc-line?, doc-nest?, etc.
;;;
;;; Primitives:
;;;   empty, text, line, hardline, softline, linebreak
;;;
;;; Combinators:
;;;   <>, <+>, </>, <//>, nest, group, align, hang, indent
;;;
;;; List Combinators:
;;;   concat, hcat, vcat, hsep, vsep, sep, cat, fill-sep, punctuate
;;;
;;; Bracketing:
;;;   enclose, parens, brackets, braces, angles, quotes, double-quotes
;;;
;;; Rendering:
;;;   pretty, pretty-print, pp, pprint
;;;
;;; S-expression:
;;;   sexp->doc, pretty-sexp
