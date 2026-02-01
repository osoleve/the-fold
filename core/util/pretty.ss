(load "core/base/prelude.ss")

(doc 'module 'pretty)
(doc 'description "Pretty Printing Combinators. Wadler-Lindig style pretty printer for optimal document layout, based on A Prettier Printer by Philip Wadler. Key concepts: Doc (abstract document type), text (literal text with no newlines), line (line break or space when grouped), nest (increase indentation), group (try to flatten to single line if it fits). The layout algorithm chooses line breaks to fit within a page width.")
(doc 'layer 'core)

(doc 'section 'document-type)

(define (doc-empty)
  (doc 'type (-> Doc))
  (doc 'description "Create an empty document.")
  (doc 'export #t)
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

;;; doc-linebreak : → Doc
;;; A line break that flattens to empty (not space).
;;; Unlike doc-union, this is handled specially by group.
(define (doc-linebreak)
  (vector 'doc-linebreak))

;;; doc-nest : Nat × Doc → Doc
(define (doc-nest n doc)
  (vector 'doc-nest n doc))

;;; doc-concat : Doc × Doc → Doc
(define (doc-concat d1 d2)
  (vector 'doc-concat d1 d2))

;;; doc-union : Doc × Doc → Doc
(define (doc-union d1 d2)
  (vector 'doc-union d1 d2))

(doc 'section 'document-predicates)

(define (doc? x)
  (doc 'type (-> Any Bool))
  (doc 'description "Test if value is a Doc.")
  (doc 'export #t)
  (and (vector? x)
       (> (vector-length x) 0)
       (if (memq (vector-ref x 0)
                 '(doc-empty doc-text doc-line doc-hardline doc-linebreak
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
;;; doc-linebreak? : Doc → Bool
(define (doc-linebreak? d) (eq? (vector-ref d 0) 'doc-linebreak))
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
   [(doc-linebreak? doc) (doc-empty)]  ; linebreak becomes empty (not space)
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

(doc 'section 'primitive-documents)

(doc empty 'type Doc)
(doc empty 'description "The empty document.")
(doc empty 'export #t)
(define empty (doc-empty))

(define (text s)
  (doc 'type (-> String Doc))
  (doc 'description "A literal string (should not contain newlines).")
  (doc 'export #t)
  (if (string=? s "")
      empty
      (doc-text s)))

(doc line 'type Doc)
(doc line 'description "A line break. When flattened (in a group), becomes a space.")
(doc line 'export #t)
(define line (doc-line))

(doc hardline 'type Doc)
(doc hardline 'description "A line break that never flattens.")
(doc hardline 'export #t)
(define hardline (doc-hardline))

(doc softline 'type Doc)
(doc softline 'description "A space that can become a line break if needed.")
(doc softline 'export #t)
(define softline (doc-group line))

(doc linebreak 'type Doc)
(doc linebreak 'description "A line break that becomes empty when flattened.")
(doc linebreak 'export #t)
(define linebreak (doc-linebreak))

(doc 'section 'combinators)

(define (<> d1 d2)
  (doc 'type (-> Doc Doc Doc))
  (doc 'description "Concatenate two documents.")
  (doc 'export #t)
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

(define (nest n d)
  (doc 'type (-> Nat Doc Doc))
  (doc 'description "Increase indentation by n spaces for following lines.")
  (doc 'export #t)
  (doc-nest n d))

(define (group d)
  (doc 'type (-> Doc Doc))
  (doc 'description "Try to flatten document to single line if it fits within page width.")
  (doc 'export #t)
  (doc-group d))

;;; align : Doc → Doc
(define (align doc)
  (nest 0 doc))

;;; hang : Nat × Doc → Doc
(define (hang n doc)
  (align (nest n doc)))

;;; indent : Nat × Doc → Doc
(define (indent n doc)
  (<> (text (make-string n #\space)) (nest n doc)))

(doc 'section 'list-combinators)

(define (concat docs)
  (doc 'type (-> (List Doc) Doc))
  (doc 'description "Concatenate a list of documents.")
  (doc 'export #t)
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

(define (sep docs)
  (doc 'type (-> (List Doc) Doc))
  (doc 'description "Separate documents with line breaks, grouped to try fitting on one line.")
  (doc 'export #t)
  (group (vsep docs)))

(define (cat docs)
  (doc 'type (-> (List Doc) Doc))
  (doc 'description "Concatenate documents vertically, grouped to try fitting on one line.")
  (doc 'export #t)
  (group (vcat docs)))

;;; fill-sep : (List Doc) → Doc
;;; Like fill-cat but with space between items when on same line.
(define (fill-sep docs)
  (if (null? docs)
      empty
      ;; Group must encompass line AND next doc so fits? checks the actual content
      (fold-left (lambda (acc d) (<> acc (group (<> line d))))
                 (car docs)
                 (cdr docs))))

(define (fill-cat docs)
  (doc 'type (-> (List Doc) Doc))
  (doc 'description "Concatenate documents, trying to fit as many as possible on each line with no space between them. When a document doesn't fit, inserts a line break before it.")
  (doc 'export #t)
  (if (null? docs)
      empty
      ;; Group must encompass linebreak AND next doc so fits? checks the actual content
      (fold-left (lambda (acc d) (<> acc (group (<> linebreak d))))
                 (car docs)
                 (cdr docs))))

(define (fill n d)
  (doc 'type (-> Nat Doc Doc))
  (doc 'description "Pad document to at least n characters wide by appending spaces. If document already exceeds n, returns unchanged.")
  (doc 'export #t)
  (let ([w (doc-width d)])
    (if (>= w n)
        d
        (<> d (text (make-string (- n w) #\space))))))

(define (fill-break n d)
  (doc 'type (-> Nat Doc Doc))
  (doc 'description "Pad document to n characters, or insert line break and indent if it exceeds n. Useful for aligning columns that may overflow.")
  (doc 'export #t)
  (let ([w (doc-width d)])
    (if (> w n)
        (<> d (nest n line))
        (<> d (text (make-string (- n w) #\space))))))

;;; punctuate : Doc × (List Doc) → (List Doc)
(define (punctuate sep docs)
  (if (or (null? docs) (null? (cdr docs)))
      docs
      (cons (<> (car docs) sep)
            (punctuate sep (cdr docs)))))

(doc 'section 'bracketing-combinators)

(define (enclose left right d)
  (doc 'type (-> Doc Doc Doc Doc))
  (doc 'description "Enclose a document between left and right brackets.")
  (doc 'export #t)
  (<> left (<> d right)))

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

(doc 'section 'layout-algorithm)

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
;;; Layout with default ribbon width.
(define (best w k doc)
  (best-ribbon w *default-ribbon* k doc))

;;; best-ribbon : Nat × Nat × Nat × Doc → SDoc
;;; Layout with explicit ribbon width.
(define (best-ribbon w r k doc)
  (be-ribbon w r k (list (cons 0 doc))))

;;; be-ribbon : Nat × Nat × Nat × (List (Nat × Doc)) → SDoc
;;; Core layout with ribbon width support.
;;; w = page width, r = ribbon width, k = current column, stack = (indent . doc) pairs
(define (be-ribbon w r k stack)
  (if (null? stack)
      (sdoc-empty)
      (let ([i (car (car stack))]
            [doc (cdr (car stack))]
            [rest (cdr stack)])
           (cond
            [(doc-empty? doc)
             (be-ribbon w r k rest)]
            [(doc-text? doc)
             (let ([s (doc-text-str doc)])
                  (sdoc-text s (be-ribbon w r (+ k (string-length s)) rest)))]
            [(doc-line? doc)
             (sdoc-line i (be-ribbon w r i rest))]
            [(doc-hardline? doc)
             (sdoc-line i (be-ribbon w r i rest))]
            [(doc-linebreak? doc)
             (sdoc-line i (be-ribbon w r i rest))]
            [(doc-nest? doc)
             (be-ribbon w r k (cons (cons (+ i (doc-nest-n doc)) (doc-nest-doc doc)) rest))]
            [(doc-concat? doc)
             (be-ribbon w r k (cons (cons i (doc-concat-left doc))
                           (cons (cons i (doc-concat-right doc)) rest)))]
            [(doc-group? doc)
             (let* ([flattened (doc-group-flat doc)]
                    ;; Check both page and ribbon constraints
                    [page-remaining (- w k)]
                    [ribbon-remaining (- (+ i r) k)])  ; chars left in ribbon from current column
               (if (fits? (min page-remaining ribbon-remaining) (list (cons i flattened)))
                   (be-ribbon w r k (cons (cons i flattened) rest))
                   (be-ribbon w r k (cons (cons i (doc-group-doc doc)) rest))))]
            [(doc-union? doc)
             (let ([page-remaining (- w k)]
                   [ribbon-remaining (- (+ i r) k)])
               (if (fits? (min page-remaining ribbon-remaining) (list (cons i (doc-union-left doc))))
                   (be-ribbon w r k (cons (cons i (doc-union-left doc)) rest))
                   (be-ribbon w r k (cons (cons i (doc-union-right doc)) rest))))]
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
          [(doc-linebreak? doc) #t]  ; linebreak also ends the line
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

(doc 'section 'rendering)

(define (sdoc->string sd)
  (doc 'type (-> SDoc String))
  (doc 'description "Convert a simple doc to a string.")
  (doc 'export #t)
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

;;; --- HTML Rendering ---

;;; html-escape : String → String
;;; Escape HTML special characters (< > & " ').
(define (html-escape str)
  (let loop ([chars (string->list str)] [acc '()])
    (if (null? chars)
        (apply string-append (reverse acc))
        (let ([c (car chars)])
          (loop (cdr chars)
                (cons (cond
                       [(char=? c #\<) "&lt;"]
                       [(char=? c #\>) "&gt;"]
                       [(char=? c #\&) "&amp;"]
                       [(char=? c #\") "&quot;"]
                       [(char=? c #\') "&#39;"]
                       [else (string c)])
                      acc))))))

(define (sdoc->html sd)
  (doc 'type (-> SDoc String))
  (doc 'description "Convert a simple doc to HTML. Wraps output in <pre> for whitespace preservation.")
  (doc 'export #t)
  (let loop ([sd sd] [acc '()])
    (cond
     [(sdoc-empty? sd)
      (string-append "<pre>" (apply string-append (reverse acc)) "</pre>")]
     [(sdoc-text? sd)
      (loop (sdoc-text-rest sd)
            (cons (html-escape (sdoc-text-str sd)) acc))]
     [(sdoc-line? sd)
      (loop (sdoc-line-rest sd)
            (cons (string-append "\n" (make-string (sdoc-line-n sd) #\space))
                  acc))]
     [else
      (string-append "<pre>" (apply string-append (reverse acc)) "</pre>")])))

;;; --- Markdown Rendering ---

(define (sdoc->markdown sd)
  (doc 'type (-> SDoc String))
  (doc 'description "Convert a simple doc to Markdown. Uses fenced code block for whitespace preservation.")
  (doc 'export #t)
  (let loop ([sd sd] [acc '()])
    (cond
     [(sdoc-empty? sd)
      (string-append "```\n" (apply string-append (reverse acc)) "\n```")]
     [(sdoc-text? sd)
      (loop (sdoc-text-rest sd)
            (cons (sdoc-text-str sd) acc))]
     [(sdoc-line? sd)
      (loop (sdoc-line-rest sd)
            (cons (string-append "\n" (make-string (sdoc-line-n sd) #\space))
                  acc))]
     [else
      (string-append "```\n" (apply string-append (reverse acc)) "\n```")])))

;;; --- Plain Text Rendering (default) ---

(define (pretty width d)
  (doc 'type (-> Nat Doc String))
  (doc 'description "Render a document to a string with the given page width.")
  (doc 'export #t)
  (sdoc->string (best width 0 d)))

(define (pretty-ribbon width ribbon d)
  (doc 'type (-> Nat Nat Doc String))
  (doc 'description "Render a document with explicit page width and ribbon width. Ribbon width limits characters per line excluding indentation, improving readability for nested code.")
  (doc 'export #t)
  (sdoc->string (best-ribbon width ribbon 0 d)))

(define (pretty-html width d)
  (doc 'type (-> Nat Doc String))
  (doc 'description "Render a document to HTML with the given page width. Output is wrapped in <pre> for whitespace preservation.")
  (doc 'export #t)
  (sdoc->html (best width 0 d)))

(define (pretty-markdown width d)
  (doc 'type (-> Nat Doc String))
  (doc 'description "Render a document to Markdown with the given page width. Output is wrapped in a fenced code block.")
  (doc 'export #t)
  (sdoc->markdown (best width 0 d)))

(define (pretty-print width d)
  (doc 'type (-> Nat Doc Void))
  (doc 'description "Print a document to stdout with the given page width.")
  (doc 'export #t)
  (display (pretty width d))
  (newline))

(doc 'section 'default-width)

(doc *default-width* 'type Nat)
(doc *default-width* 'description "Default page width for pretty printing.")
(doc *default-width* 'export #t)
(define *default-width* 80)

(doc *default-ribbon* 'type Nat)
(doc *default-ribbon* 'description "Default ribbon width (max characters per line excluding indentation). When equal to page width, ribbon has no effect.")
(doc *default-ribbon* 'export #t)
(define *default-ribbon* 60)

(define (pp d)
  (doc 'type (-> Doc String))
  (doc 'description "Pretty-print a document using the default width.")
  (doc 'export #t)
  (pretty *default-width* d))

;;; pprint : Doc → Void
(define (pprint doc)
  (pretty-print *default-width* doc))

(doc 'section 'sexp-pretty-printing)

(define (sexp->doc sexp)
  (doc 'type (-> Sexp Doc))
  (doc 'description "Convert an S-expression to a Doc for pretty printing.")
  (doc 'export #t)
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

(define (pretty-sexp width sexp)
  (doc 'type (-> Nat Sexp String))
  (doc 'description "Pretty-print an S-expression to a string with the given page width.")
  (doc 'export #t)
  (pretty width (sexp->doc sexp)))
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
;;;   sdoc->string, sdoc->html, sdoc->markdown
;;;   pretty, pretty-html, pretty-markdown, pretty-ribbon
;;;   pretty-print, pp, pprint
;;;
;;; S-expression:
;;;   sexp->doc, pretty-sexp
