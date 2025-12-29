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

(load "prelude.ss")

;;; ============================================================
;;; Document Type
;;; ============================================================
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

(define (doc-empty)
  (vector 'doc-empty))

(define (doc-text str)
  (vector 'doc-text str))

(define (doc-line)
  (vector 'doc-line))

(define (doc-hardline)
  (vector 'doc-hardline))

(define (doc-nest n doc)
  (vector 'doc-nest n doc))

(define (doc-concat d1 d2)
  (vector 'doc-concat d1 d2))

(define (doc-group doc)
  (vector 'doc-group doc))

(define (doc-union d1 d2)
  (vector 'doc-union d1 d2))

;;; --- Predicates ---

(define (doc? x)
  (and (vector? x)
       (> (vector-length x) 0)
       (if (memq (vector-ref x 0)
                 '(doc-empty doc-text doc-line doc-hardline
                   doc-nest doc-concat doc-group doc-union))
           #t #f)))

(define (doc-empty? d) (eq? (vector-ref d 0) 'doc-empty))
(define (doc-text? d) (eq? (vector-ref d 0) 'doc-text))
(define (doc-line? d) (eq? (vector-ref d 0) 'doc-line))
(define (doc-hardline? d) (eq? (vector-ref d 0) 'doc-hardline))
(define (doc-nest? d) (eq? (vector-ref d 0) 'doc-nest))
(define (doc-concat? d) (eq? (vector-ref d 0) 'doc-concat))
(define (doc-group? d) (eq? (vector-ref d 0) 'doc-group))
(define (doc-union? d) (eq? (vector-ref d 0) 'doc-union))

;;; --- Accessors ---

(define (doc-text-str d) (vector-ref d 1))
(define (doc-nest-n d) (vector-ref d 1))
(define (doc-nest-doc d) (vector-ref d 2))
(define (doc-concat-left d) (vector-ref d 1))
(define (doc-concat-right d) (vector-ref d 2))
(define (doc-group-doc d) (vector-ref d 1))
(define (doc-union-left d) (vector-ref d 1))
(define (doc-union-right d) (vector-ref d 2))

;;; ============================================================
;;; Primitive Documents
;;; ============================================================

;;; empty : Doc
;;; The empty document.
(define empty (doc-empty))

;;; text : String -> Doc
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

;;; ============================================================
;;; Combinators
;;; ============================================================

;;; <> : Doc -> Doc -> Doc
;;; Concatenate two documents.
(define (<> d1 d2)
  (cond
   [(doc-empty? d1) d2]
   [(doc-empty? d2) d1]
   [else (doc-concat d1 d2)]))

;;; <+> : Doc -> Doc -> Doc
;;; Concatenate with a space.
(define (<+> d1 d2)
  (<> d1 (<> (text " ") d2)))

;;; </> : Doc -> Doc -> Doc
;;; Concatenate with a line (space or newline).
(define (</> d1 d2)
  (<> d1 (<> line d2)))

;;; <//> : Doc -> Doc -> Doc
;;; Concatenate with a linebreak (empty or newline).
(define (<//> d1 d2)
  (<> d1 (<> linebreak d2)))

;;; nest : Int -> Doc -> Doc
;;; Increase indentation for nested content.
(define (nest n doc)
  (doc-nest n doc))

;;; group : Doc -> Doc
;;; Try to fit on one line; if not, use line breaks.
(define (group doc)
  (doc-group doc))

;;; align : Doc -> Doc
;;; Align to current column (implemented via nest 0 for simplicity).
(define (align doc)
  (nest 0 doc))

;;; hang : Int -> Doc -> Doc
;;; Hang with indentation.
(define (hang n doc)
  (align (nest n doc)))

;;; indent : Int -> Doc -> Doc
;;; Indent the document.
(define (indent n doc)
  (<> (text (make-string n #\space)) (nest n doc)))

;;; ============================================================
;;; List Combinators
;;; ============================================================

;;; concat : (List Doc) -> Doc
;;; Concatenate a list of documents.
(define (concat docs)
  (if (null? docs)
      empty
      (fold-left <> (car docs) (cdr docs))))

;;; hcat : (List Doc) -> Doc
;;; Horizontal concatenation (no separators).
(define hcat concat)

;;; vcat : (List Doc) -> Doc
;;; Vertical concatenation (hardlines between).
(define (vcat docs)
  (if (null? docs)
      empty
      (fold-left (lambda (acc d) (<> acc (<> hardline d)))
                 (car docs)
                 (cdr docs))))

;;; hsep : (List Doc) -> Doc
;;; Horizontal with spaces.
(define (hsep docs)
  (if (null? docs)
      empty
      (fold-left <+> (car docs) (cdr docs))))

;;; vsep : (List Doc) -> Doc
;;; Vertical with line breaks.
(define (vsep docs)
  (if (null? docs)
      empty
      (fold-left </> (car docs) (cdr docs))))

;;; sep : (List Doc) -> Doc
;;; Either all on one line (hsep) or each on own line (vsep).
(define (sep docs)
  (group (vsep docs)))

;;; cat : (List Doc) -> Doc
;;; Either all on one line (hcat) or each on own line (vcat).
(define (cat docs)
  (group (vcat docs)))

;;; fill-sep : (List Doc) -> Doc
;;; Fill as many on each line as possible.
(define (fill-sep docs)
  (if (null? docs)
      empty
      (fold-left (lambda (acc d) (<> acc (<> softline d)))
                 (car docs)
                 (cdr docs))))

;;; punctuate : Doc -> (List Doc) -> (List Doc)
;;; Add punctuation between elements.
(define (punctuate sep docs)
  (if (or (null? docs) (null? (cdr docs)))
      docs
      (cons (<> (car docs) sep)
            (punctuate sep (cdr docs)))))

;;; ============================================================
;;; Bracketing Combinators
;;; ============================================================

;;; enclose : Doc -> Doc -> Doc -> Doc
;;; Wrap document with left and right delimiters.
(define (enclose left right doc)
  (<> left (<> doc right)))

;;; parens : Doc -> Doc
(define (parens doc)
  (enclose (text "(") (text ")") doc))

;;; brackets : Doc -> Doc
(define (brackets doc)
  (enclose (text "[") (text "]") doc))

;;; braces : Doc -> Doc
(define (braces doc)
  (enclose (text "{") (text "}") doc))

;;; angles : Doc -> Doc
(define (angles doc)
  (enclose (text "<") (text ">") doc))

;;; quotes : Doc -> Doc
(define (quotes doc)
  (enclose (text "'") (text "'") doc))

;;; double-quotes : Doc -> Doc
(define (double-quotes doc)
  (enclose (text "\"") (text "\"") doc))

;;; ============================================================
;;; Flattening (for group)
;;; ============================================================

;;; flatten : Doc -> Doc
;;; Replace line breaks with spaces.
(define (flatten doc)
  (cond
   [(doc-empty? doc) empty]
   [(doc-text? doc) doc]
   [(doc-line? doc) (text " ")]
   [(doc-hardline? doc) doc]  ; hardline never flattens
   [(doc-nest? doc) (doc-nest (doc-nest-n doc) (flatten (doc-nest-doc doc)))]
   [(doc-concat? doc) (doc-concat (flatten (doc-concat-left doc))
                                  (flatten (doc-concat-right doc)))]
   [(doc-group? doc) (flatten (doc-group-doc doc))]
   [(doc-union? doc) (doc-union-left doc)]  ; take flattened version
   [else doc]))

;;; ============================================================
;;; Layout Algorithm (Simple Mode)
;;; ============================================================
;;;
;;; Simple Doc (for rendering):
;;;   (sdoc-empty)
;;;   (sdoc-text str sdoc)   - text followed by more
;;;   (sdoc-line n sdoc)     - newline + n spaces, followed by more

(define (sdoc-empty) (vector 'sdoc-empty))
(define (sdoc-text str rest) (vector 'sdoc-text str rest))
(define (sdoc-line n rest) (vector 'sdoc-line n rest))

(define (sdoc-empty? sd) (eq? (vector-ref sd 0) 'sdoc-empty))
(define (sdoc-text? sd) (eq? (vector-ref sd 0) 'sdoc-text))
(define (sdoc-line? sd) (eq? (vector-ref sd 0) 'sdoc-line))

(define (sdoc-text-str sd) (vector-ref sd 1))
(define (sdoc-text-rest sd) (vector-ref sd 2))
(define (sdoc-line-n sd) (vector-ref sd 1))
(define (sdoc-line-rest sd) (vector-ref sd 2))

;;; best : Int -> Int -> Doc -> SimpleDoc
;;; Layout algorithm. Returns SimpleDoc.
;;; w = page width, k = current column, doc = document
(define (best w k doc)
  (be w k (list (cons 0 doc))))

;;; be : Int -> Int -> (List (Int . Doc)) -> SimpleDoc
;;; Main layout worker. Stack of (indent . doc) pairs.
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
             (let ([flattened (flatten (doc-group-doc doc))])
                  (if (fits? (- w k) (list (cons i flattened)))
                      (be w k (cons (cons i flattened) rest))
                      (be w k (cons (cons i (doc-group-doc doc)) rest))))]
            [(doc-union? doc)
             (if (fits? (- w k) (list (cons i (doc-union-left doc))))
                 (be w k (cons (cons i (doc-union-left doc)) rest))
                 (be w k (cons (cons i (doc-union-right doc)) rest)))]
            [else (sdoc-empty)]))))

;;; fits? : Int -> (List (Int . Doc)) -> Bool
;;; Does document stack fit in remaining width?
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
           (fits? w (cons (cons i (flatten (doc-group-doc doc))) rest))]
          [(doc-union? doc)
           (fits? w (cons (cons i (doc-union-left doc)) rest))]
          [else #t]))]))

;;; doc-width : Doc -> Int
;;; Width of flattened document (for fits? calculation).
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

;;; ============================================================
;;; Rendering
;;; ============================================================

;;; sdoc->string : SimpleDoc -> String
;;; Render SimpleDoc to a string.
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

;;; pretty : Int -> Doc -> String
;;; Render document to string with given page width.
(define (pretty width doc)
  (sdoc->string (best width 0 doc)))

;;; pretty-print : Int -> Doc -> Void
;;; Render and display with newline.
(define (pretty-print width doc)
  (display (pretty width doc))
  (newline))

;;; ============================================================
;;; Default Width
;;; ============================================================

(define *default-width* 80)

;;; pp : Doc -> String
;;; Pretty print with default width.
(define (pp doc)
  (pretty *default-width* doc))

;;; pprint : Doc -> Void
;;; Pretty print and display with default width.
(define (pprint doc)
  (pretty-print *default-width* doc))

;;; ============================================================
;;; Convenience: S-expression Pretty Printing
;;; ============================================================

;;; sexp->doc : Sexp -> Doc
;;; Convert S-expression to document.
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

;;; pretty-sexp : Int -> Sexp -> String
;;; Pretty print an S-expression.
(define (pretty-sexp width sexp)
  (pretty width (sexp->doc sexp)))

;;; ============================================================
;;; Export Summary
;;; ============================================================
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
