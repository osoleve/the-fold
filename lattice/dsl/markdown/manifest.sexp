(skill markdown
  (version "0.1.0")
  (tier 1)
  (path "lattice/dsl/markdown")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n) for parsing, O(n) for rendering")
  (deps (fp dsl))
  (description "Markdown parser and HTML renderer using parser combinators.
    Parses CommonMark-subset markdown into an S-expression AST,
    then renders to HTML. Dogfooding the FP toolkit.")
  (keywords (markdown parser html renderer documentation dsl))
  (aliases (md markdown-parser))
  (exports
    (ast md-document md-heading md-h1 md-h2 md-h3 md-h4 md-h5 md-h6
         md-paragraph md-code-block md-blockquote md-unordered-list
         md-ordered-list md-list-item md-hr md-text md-strong md-em
         md-code md-link md-br md-node? md-block? md-inline? md-heading?
         md-list? md-tag md-children md-heading-level md-flatten-text md-walk)
    (inline-parser parse-inline parse-inline-or-text)
    (block-parser parse-markdown parse-markdown-or-error)
    (html render-html render-page html-escape markdown-to-html))
  (modules
    (ast "ast.ss" "Markdown AST node constructors and predicates")
    (inline-parser "inline-parser.ss" "Inline element parser (emphasis, code, links)")
    (block-parser "block-parser.ss" "Block element parser (headings, paragraphs, lists)")
    (html "html.ss" "HTML renderer for markdown AST")))
