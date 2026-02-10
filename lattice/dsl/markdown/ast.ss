;;; @module markdown-ast
;;; @requires prelude
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'prelude)

(doc 'module 'markdown-ast)
(doc 'description "Markdown Abstract Syntax Tree - Defines the AST types for parsed markdown documents. All nodes are tagged S-expressions for easy pattern matching")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Block-level nodes:
  (document block...)           - Root document
  (h1 inline...)                - Heading level 1
  (h2 inline...)                - Heading level 2
  (h3 inline...)                - Heading level 3
  (h4 inline...)                - Heading level 4
  (h5 inline...)                - Heading level 5
  (h6 inline...)                - Heading level 6
  (p inline...)                 - Paragraph
  (code-block lang code)        - Fenced code block
  (blockquote block...)         - Block quote
  (ul li...)                    - Unordered list
  (ol li...)                    - Ordered list
  (li inline...)                - List item
  (hr)                          - Horizontal rule")

(doc 'note "Inline-level nodes:
  (text \"string\")               - Plain text
  (strong inline...)            - Bold text
  (em inline...)                - Italic text
  (code \"string\")               - Inline code
  (link url title inline...)    - Hyperlink
  (br)                          - Line break")

(doc 'section 'block-node-constructors)

;;; md-document : (List Block) -> AST
;;; Create a document node containing block elements.
(define (md-document blocks)
  (cons 'document blocks))

;;; md-heading : Nat × (List Inline) -> AST
;;; Create a heading node (level 1-6).
(define (md-heading level inlines)
  (let ([tag (case level
               [(1) 'h1] [(2) 'h2] [(3) 'h3]
               [(4) 'h4] [(5) 'h5] [(6) 'h6]
               [else 'h6])])
    (cons tag inlines)))

;;; md-h1 ... md-h6 : (List Inline) -> AST
;;; Convenience constructors for each heading level.
(define (md-h1 inlines) (cons 'h1 inlines))
(define (md-h2 inlines) (cons 'h2 inlines))
(define (md-h3 inlines) (cons 'h3 inlines))
(define (md-h4 inlines) (cons 'h4 inlines))
(define (md-h5 inlines) (cons 'h5 inlines))
(define (md-h6 inlines) (cons 'h6 inlines))

;;; md-paragraph : (List Inline) -> AST
;;; Create a paragraph node.
(define (md-paragraph inlines)
  (cons 'p inlines))

;;; md-code-block : String × String -> AST
;;; Create a fenced code block with optional language.
(define (md-code-block lang code)
  (list 'code-block lang code))

;;; md-blockquote : (List Block) -> AST
;;; Create a blockquote containing block elements.
(define (md-blockquote blocks)
  (cons 'blockquote blocks))

;;; md-unordered-list : (List LI) -> AST
;;; Create an unordered list.
(define (md-unordered-list items)
  (cons 'ul items))

;;; md-ordered-list : (List LI) -> AST
;;; Create an ordered list.
(define (md-ordered-list items)
  (cons 'ol items))

;;; md-list-item : (List Inline) -> AST
;;; Create a list item.
(define (md-list-item inlines)
  (cons 'li inlines))

;;; md-hr : -> AST
;;; Create a horizontal rule.
(define (md-hr)
  '(hr))

;;; md-table : (List (List Inline)) × (List (List Inline)) -> AST
;;; Create a table with header row and data rows.
;;; Each row is a list of cells, each cell is a list of inlines.
(define (md-table header rows)
  (list 'table header rows))

;;; md-table-row : (List (List Inline)) -> AST
;;; Create a table row.
(define (md-table-row cells)
  (cons 'tr cells))

;;; md-table-cell : (List Inline) -> AST
;;; Create a table cell.
(define (md-table-cell inlines)
  (cons 'td inlines))

;;; md-table-header-cell : (List Inline) -> AST
;;; Create a table header cell.
(define (md-table-header-cell inlines)
  (cons 'th inlines))

(doc 'section 'inline-node-constructors)

;;; md-text : String -> AST
;;; Create a plain text node.
(define (md-text str)
  (list 'text str))

;;; md-strong : (List Inline) -> AST
;;; Create bold text.
(define (md-strong inlines)
  (cons 'strong inlines))

;;; md-em : (List Inline) -> AST
;;; Create italic text.
(define (md-em inlines)
  (cons 'em inlines))

;;; md-code : String -> AST
;;; Create inline code.
(define (md-code str)
  (list 'code str))

;;; md-link : String × String × (List Inline) -> AST
;;; Create a hyperlink with URL, optional title, and link text.
(define (md-link url title inlines)
  (list 'link url title inlines))

;;; md-br : -> AST
;;; Create a line break.
(define (md-br)
  '(br))

(doc 'section 'predicates)

;;; md-node? : Any -> Boolean
;;; Check if x is any markdown node.
(define (md-node? x)
  (and (pair? x)
       (symbol? (car x))
       (if (memq (car x) '(document h1 h2 h3 h4 h5 h6 p code-block
                           blockquote ul ol li hr table tr td th
                           text strong em code link br))
           #t #f)))

;;; md-block? : Any -> Boolean
;;; Check if x is a block-level node.
(define (md-block? x)
  (and (pair? x)
       (if (memq (car x) '(document h1 h2 h3 h4 h5 h6 p code-block
                           blockquote ul ol hr table))
           #t #f)))

;;; md-inline? : Any -> Boolean
;;; Check if x is an inline-level node.
(define (md-inline? x)
  (and (pair? x)
       (if (memq (car x) '(text strong em code link br))
           #t #f)))

;;; md-heading? : Any -> Boolean
(define (md-heading? x)
  (and (pair? x)
       (if (memq (car x) '(h1 h2 h3 h4 h5 h6))
           #t #f)))

;;; md-list? : Any -> Boolean
(define (md-list? x)
  (and (pair? x)
       (memq (car x) '(ul ol))))

(doc 'section 'accessors)

;;; md-tag : AST -> Symbol
;;; Get the tag of a markdown node.
(define (md-tag node)
  (car node))

;;; md-children : AST -> (List AST)
;;; Get the children of a markdown node.
(define (md-children node)
  (cdr node))

;;; md-heading-level : AST -> Nat
;;; Get the level of a heading node (1-6).
(define (md-heading-level node)
  (case (car node)
    [(h1) 1] [(h2) 2] [(h3) 3]
    [(h4) 4] [(h5) 5] [(h6) 6]
    [else 0]))

;;; md-code-block-lang : AST -> String
;;; Get the language of a code block.
(define (md-code-block-lang node)
  (if (eq? (car node) 'code-block)
      (cadr node)
      ""))

;;; md-code-block-content : AST -> String
;;; Get the content of a code block.
(define (md-code-block-content node)
  (if (eq? (car node) 'code-block)
      (caddr node)
      ""))

;;; md-text-content : AST -> String
;;; Get the string content of a text node.
(define (md-text-content node)
  (if (eq? (car node) 'text)
      (cadr node)
      ""))

;;; md-code-content : AST -> String
;;; Get the string content of an inline code node.
(define (md-code-content node)
  (if (eq? (car node) 'code)
      (cadr node)
      ""))

;;; md-link-url : AST -> String
(define (md-link-url node)
  (if (eq? (car node) 'link)
      (cadr node)
      ""))

;;; md-link-title : AST -> String
(define (md-link-title node)
  (if (eq? (car node) 'link)
      (caddr node)
      ""))

;;; md-link-text : AST -> (List Inline)
(define (md-link-text node)
  (if (eq? (car node) 'link)
      (cadddr node)
      '()))

(doc 'section 'utilities)

;;; md-flatten-text : AST -> String
;;; Extract all text content from a node, discarding formatting.
(define (md-flatten-text node)
  (cond
    [(string? node) node]
    [(not (pair? node)) ""]
    [(eq? (car node) 'text) (cadr node)]
    [(eq? (car node) 'code) (cadr node)]
    [(eq? (car node) 'code-block) (caddr node)]
    [(eq? (car node) 'br) "\n"]
    [(eq? (car node) 'hr) ""]
    [else (apply string-append (map md-flatten-text (cdr node)))]))

;;; md-walk : (AST -> AST) × AST -> AST
;;; Walk the AST, applying f to each node (post-order).
(define (md-walk f node)
  (if (or (not (pair? node)) (null? node))
      (f node)
      (f (cons (car node)
               (map (lambda (child) (md-walk f child))
                    (cdr node))))))
