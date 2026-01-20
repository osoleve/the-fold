(load "core/base/prelude.ss")

(doc 'module 'build-report)
(doc 'description "Builds the technical report HTML from markdown chapter files")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Dogfoods the lattice/dsl/markdown parser and renderer")
(doc 'usage "scheme --script boundary/tools/build-report.ss")
(doc 'usage "Or from REPL: (load \"boundary/tools/build-report.ss\") (build-report)")
(load "lattice/dsl/markdown/block-parser.ss")
(load "lattice/dsl/markdown/html.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *report-source-dir* "docs/technical-report")
(define *report-output-file* "/home/oso/fold/docs/technical-report.html")
(define *manifest-file* "docs/technical-report/manifest.sexp")

;;; ============================================================
;;; Manifest Reader
;;; ============================================================

;;; read-manifest : String -> Sexp
;;; Read the manifest file.
(define (read-manifest path)
  (call-with-input-file path read))

;;; get-chapters : Sexp -> (List String)
;;; Extract chapter list from manifest, excluding frontmatter.
(define (get-chapters manifest)
  (let ([chapters-entry (assq 'chapters (cdr manifest))])
    (if chapters-entry
        ;; Filter out frontmatter files
        (filter (lambda (ch) (not (string=? ch "00-frontmatter.md")))
                (cdr chapters-entry))
        (error 'get-chapters "No chapters found in manifest"))))

;;; Load chapters from manifest
(define *chapters* (get-chapters (read-manifest *manifest-file*)))

;;; ============================================================
;;; File I/O
;;; ============================================================

;;; read-file : String -> String
;;; Read entire file contents.
(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([chars '()])
        (let ([c (read-char port)])
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

;;; write-file : String × String -> Unit
;;; Write content to file.
(define (write-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))
    '(replace)))

;;; ============================================================
;;; Page Template
;;; ============================================================

(define *page-css* "
:root {
    --bg-primary: #0d1117;
    --bg-secondary: #161b22;
    --bg-tertiary: #21262d;
    --text-primary: #e6edf3;
    --text-secondary: #8b949e;
    --text-muted: #6e7681;
    --accent: #7c3aed;
    --border: #30363d;
    --code-bg: #1c2128;
    --link: #a78bfa;
}

* { box-sizing: border-box; margin: 0; padding: 0; }

html { scroll-behavior: smooth; }

body {
    font-family: 'Inter', -apple-system, BlinkMacSystemFont, system-ui, sans-serif;
    background: var(--bg-primary);
    color: var(--text-primary);
    line-height: 1.7;
    font-size: 16px;
}

.container {
    max-width: 800px;
    margin: 0 auto;
    padding: 3rem 2rem 6rem;
}

h1 {
    font-size: 2.5rem;
    font-weight: 700;
    line-height: 1.2;
    margin-bottom: 0.5rem;
    background: linear-gradient(135deg, #a78bfa 0%, #7c3aed 50%, #5b21b6 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
}

h2 {
    font-size: 1.75rem;
    font-weight: 600;
    margin-top: 3rem;
    margin-bottom: 1rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border);
}

h3 {
    font-size: 1.25rem;
    font-weight: 600;
    margin-top: 2rem;
    margin-bottom: 0.75rem;
}

h4 {
    font-size: 1rem;
    font-weight: 600;
    margin-top: 1.5rem;
    margin-bottom: 0.5rem;
    color: var(--text-secondary);
}

p {
    margin-bottom: 1rem;
    color: var(--text-secondary);
}

strong { color: var(--text-primary); font-weight: 600; }
em { font-style: italic; color: var(--text-primary); }

a { color: var(--link); text-decoration: none; }
a:hover { text-decoration: underline; }

ul, ol {
    margin-bottom: 1rem;
    padding-left: 1.5rem;
    color: var(--text-secondary);
}

li { margin-bottom: 0.5rem; }
li::marker { color: var(--text-muted); }

code {
    font-family: 'JetBrains Mono', 'Fira Code', monospace;
    font-size: 0.875em;
    background: var(--code-bg);
    padding: 0.2em 0.4em;
    border-radius: 4px;
    color: #f0abfc;
}

pre {
    background: var(--code-bg);
    border: 1px solid var(--border);
    border-radius: 8px;
    padding: 1rem 1.25rem;
    overflow-x: auto;
    margin-bottom: 1.5rem;
    font-size: 0.875rem;
    line-height: 1.6;
}

pre code {
    background: none;
    padding: 0;
    color: var(--text-primary);
}

blockquote {
    border-left: 3px solid var(--accent);
    margin: 1.5rem 0;
    padding: 0.5rem 0 0.5rem 1.25rem;
    color: var(--text-secondary);
    background: rgba(124, 58, 237, 0.05);
    border-radius: 0 8px 8px 0;
}

blockquote p:last-child { margin-bottom: 0; }

hr {
    border: none;
    border-top: 1px solid var(--border);
    margin: 2rem 0;
}

.meta {
    color: var(--text-muted);
    font-size: 0.875rem;
    margin-bottom: 2rem;
    padding-bottom: 2rem;
    border-bottom: 1px solid var(--border);
}

.generated {
    margin-top: 3rem;
    padding-top: 1rem;
    border-top: 1px solid var(--border);
    font-size: 0.75rem;
    color: var(--text-muted);
    text-align: center;
}
")

;;; wrap-page : String -> String
;;; Wrap content in the page template.
(define (wrap-page content)
  (string-append
   "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>The Fold: Technical Report</title>
    <style>" *page-css* "</style>
    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
    <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400&display=swap\" rel=\"stylesheet\">
</head>
<body>
<div class=\"container\">
<h1>The Fold: A Content-Addressable Homoiconic Universe</h1>
<div class=\"meta\">Technical Report</div>
"
   content
   "
<div class=\"generated\">Generated by The Fold's markdown parser (dogfooding the lattice/dsl/markdown skill)</div>
</div>
</body>
</html>
"))

;;; ============================================================
;;; Build Process
;;; ============================================================

;;; parse-chapter : String -> String
;;; Parse a single chapter file and return HTML.
(define (parse-chapter filename)
  (let* ([path (string-append *report-source-dir* "/" filename)]
         [content (read-file path)]
         [result (parse-markdown content)])
    (if (right? result)
        (render-html (from-right result))
        (string-append "<p class=\"error\">Error parsing " filename ": "
                       (format-error (from-left result))
                       "</p>\n"))))

;;; build-report : -> Unit
;;; Build the complete technical report.
(define (build-report)
  (printf "Building technical report from markdown...\n")
  (printf "  Source: ~a/\n" *report-source-dir*)
  (printf "  Output: ~a\n" *report-output-file*)
  (printf "\n")

  (let* ([chapter-htmls (map (lambda (ch)
                               (printf "  Parsing ~a...\n" ch)
                               (parse-chapter ch))
                             *chapters*)]
         [body (apply string-append chapter-htmls)]
         [page (wrap-page body)])

    (printf "\n")
    (printf "  Writing output...\n")
    (write-file *report-output-file* page)
    (printf "  Done! ~a bytes written.\n" (string-length page))
    (printf "\n")
    (printf "View at: https://oso.rocks/report-fold.html\n")))

;;; ============================================================
;;; CLI Entry Point
;;; ============================================================

;;; Run if executed as script
(build-report)
