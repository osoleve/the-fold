(load "core/base/prelude.ss")

(doc 'module 'build-nav-report)
(doc 'description "Builds the chapter-navigable technical report by parsing markdown chapters and injecting content into the template")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'note "Dogfooding the lattice/dsl/markdown parser")
(doc 'usage "scheme --script boundary/tools/build-nav-report.ss")
(load "lattice/dsl/markdown/block-parser.ss")
(load "lattice/dsl/markdown/html.ss")

(doc 'section 'configuration)

(define *report-source-dir* "docs/technical-report")
(define *report-output-file* "docs/technical-report-nav.html")

;;; Chapter manifest mapping section IDs to filenames
;;; The section IDs match the nav links in report.html
(define *chapter-map*
  '(("block-machine"    "03-the-block-machine.md")
    ("block-calculus"   "04-the-block-calculus.md")
    ("type-theory"      "05-the-type-theory.md")
    ("module-system"    "06-the-module-system.md")
    ("implementation"   "07-implementation.md")
    ("egraph-core"      "07b-egraph-core.md")
    ("equality-sat"     "07c-equality-saturation.md")
    ("meta-tooling"     "08-developer-and-meta-tooling.md")
    ("agent-substrate"  "09-agent-substrate.md")
    ("evaluation"       "10-evaluation.md")
    ("related-work"     "11-related-work.md")
    ("limitations"      "12-limitations-and-non-goals.md")
    ("future-work"      "13-future-work.md")
    ("conclusion"       "14-conclusion.md")
    ("appendix-a"       "appendix-a-block-calculus-formal-syntax.md")
    ("appendix-b"       "appendix-b-type-grammar.md")
    ("appendix-c"       "appendix-c-kind-grammar.md")
    ("appendix-d"       "appendix-d-manifest-schema.md")
    ("appendix-e"       "appendix-e-comparison-with-unison.md")
    ("references"       "99-references.md")))

(doc 'section 'file-io)

(define (read-file path)
  (doc 'type "Path -> String")
  (call-with-input-file path
    (lambda (port)
      (let loop ([chars '()])
        (let ([c (read-char port)])
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

(define (write-file path content)
  (doc 'type "Path × String -> Unit")
  (call-with-output-file path
    (lambda (port)
      (display content port))
    '(replace)))

(doc 'section 'markdown-processing)

(define (render-body ast)
  (doc 'type "AST -> String")
  (doc 'description "Render markdown AST to HTML, skipping the first H2 (already in template)")
  (if (and (pair? ast) (eq? (car ast) 'document))
      (let* ([blocks (cdr ast)]
             ;; Skip leading H2 if present
             [body-blocks (if (and (pair? blocks)
                                   (pair? (car blocks))
                                   (eq? (caar blocks) 'h2))
                              (cdr blocks)
                              blocks)])
        ;; Render remaining blocks
        (apply string-append
               (map render-html body-blocks)))
      (render-html ast)))

(define (parse-chapter-body filename)
  (doc 'type "String -> String")
  (doc 'description "Parse markdown file and return just the body HTML (no H2)")
  (let* ([path (string-append *report-source-dir* "/" filename)]
         [content (read-file path)]
         [result (parse-markdown content)])
    (if (right? result)
        (render-body (from-right result))
        (string-append "<p class=\"error\">Error parsing " filename ": "
                       (format-error (from-left result))
                       "</p>\n"))))

(doc 'section 'template-generation)

(define (generate-section section-id content)
  (doc 'type "String × String -> String")
  (doc 'description "Generate a complete section with content")
  (string-append
   "            <section id=\"" section-id "\">\n"
   content
   "            </section>\n\n"))

(doc 'section 'html-template)

(define *page-header* "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>The Fold: Technical Report</title>
    <meta name=\"description\" content=\"Technical report for The Fold: A Content-Addressable Homoiconic Universe built on Chez Scheme\">
    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
    <link href=\"https://fonts.googleapis.com/css2?family=Cormorant+Garamond:ital,wght@0,400;0,500;0,600;0,700;1,400;1,500&family=IBM+Plex+Mono:ital,wght@0,400;0,500;0,600;1,400&family=DM+Sans:ital,wght@0,400;0,500;0,600;1,400&display=swap\" rel=\"stylesheet\">
    <style>
        /* ===============================================================
           THE FOLD — Research Terminal Aesthetic
           A scholarly document viewer with terminal undertones
           =============================================================== */

        :root {
            /* Dark theme (default) */
            --bg-deep: #0a0c10;
            --bg-primary: #0d1117;
            --bg-secondary: #151b23;
            --bg-tertiary: #1c2430;
            --bg-elevated: #212a36;

            --text-primary: #e2e8f0;
            --text-secondary: #94a3b8;
            --text-muted: #64748b;
            --text-faint: #475569;

            --accent-primary: #c084fc;
            --accent-secondary: #a855f7;
            --accent-tertiary: #7c3aed;
            --accent-glow: rgba(168, 85, 247, 0.15);
            --accent-subtle: rgba(168, 85, 247, 0.08);

            --border-primary: #2d3748;
            --border-secondary: #1e293b;
            --border-accent: rgba(168, 85, 247, 0.3);

            --code-bg: #0f1419;
            --code-border: #1e293b;
            --code-text: #e879f9;

            --link: #c084fc;
            --link-hover: #e879f9;

            --success: #4ade80;
            --warning: #fbbf24;
            --error: #f87171;

            /* Layout */
            --sidebar-width: 320px;
            --content-max: 780px;
            --header-height: 64px;

            /* Typography scale */
            --text-xs: 0.75rem;
            --text-sm: 0.875rem;
            --text-base: 1rem;
            --text-lg: 1.125rem;
            --text-xl: 1.25rem;
            --text-2xl: 1.5rem;
            --text-3xl: 1.875rem;
            --text-4xl: 2.25rem;

            /* Transitions */
            --transition-fast: 0.15s ease;
            --transition-base: 0.25s ease;
            --transition-slow: 0.4s ease;
        }

        /* Light theme */
        [data-theme=\"light\"] {
            --bg-deep: #f8fafc;
            --bg-primary: #ffffff;
            --bg-secondary: #f1f5f9;
            --bg-tertiary: #e2e8f0;
            --bg-elevated: #ffffff;

            --text-primary: #0f172a;
            --text-secondary: #334155;
            --text-muted: #64748b;
            --text-faint: #94a3b8;

            --accent-primary: #7c3aed;
            --accent-secondary: #6d28d9;
            --accent-tertiary: #5b21b6;
            --accent-glow: rgba(124, 58, 237, 0.12);
            --accent-subtle: rgba(124, 58, 237, 0.06);

            --border-primary: #e2e8f0;
            --border-secondary: #cbd5e1;
            --border-accent: rgba(124, 58, 237, 0.25);

            --code-bg: #f8fafc;
            --code-border: #e2e8f0;
            --code-text: #7c3aed;

            --link: #7c3aed;
            --link-hover: #6d28d9;
        }

        /* Reset & base */
        *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }

        html {
            scroll-behavior: smooth;
            scroll-padding-top: calc(var(--header-height) + 2rem);
            font-size: 16px;
        }

        body {
            font-family: 'DM Sans', -apple-system, BlinkMacSystemFont, sans-serif;
            background: var(--bg-deep);
            color: var(--text-primary);
            line-height: 1.7;
            min-height: 100vh;
            overflow-x: hidden;
            -webkit-font-smoothing: antialiased;
            -moz-osx-font-smoothing: grayscale;
        }

        /* Ambient background texture */
        body::before {
            content: '';
            position: fixed;
            inset: 0;
            background:
                radial-gradient(ellipse 80% 50% at 50% -20%, var(--accent-glow) 0%, transparent 50%),
                radial-gradient(ellipse 60% 40% at 100% 100%, rgba(124, 58, 237, 0.05) 0%, transparent 40%);
            pointer-events: none;
            z-index: -1;
        }

        /* Noise texture overlay */
        body::after {
            content: '';
            position: fixed;
            inset: 0;
            background-image: url(\"data:image/svg+xml,%3Csvg viewBox='0 0 400 400' xmlns='http://www.w3.org/2000/svg'%3E%3Cfilter id='noiseFilter'%3E%3CfeTurbulence type='fractalNoise' baseFrequency='0.9' numOctaves='4' stitchTiles='stitch'/%3E%3C/filter%3E%3Crect width='100%25' height='100%25' filter='url(%23noiseFilter)'/%3E%3C/svg%3E\");
            opacity: 0.015;
            pointer-events: none;
            z-index: 1000;
        }

        /* ===============================================================
           HEADER BAR
           =============================================================== */

        .header {
            position: fixed;
            top: 0;
            left: var(--sidebar-width);
            right: 0;
            height: var(--header-height);
            background: rgba(13, 17, 23, 0.85);
            backdrop-filter: blur(12px);
            -webkit-backdrop-filter: blur(12px);
            border-bottom: 1px solid var(--border-secondary);
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 0 2rem;
            z-index: 90;
            transition: left var(--transition-base);
        }

        [data-theme=\"light\"] .header {
            background: rgba(255, 255, 255, 0.9);
        }

        .header-left {
            display: flex;
            align-items: center;
            gap: 1rem;
        }

        .breadcrumb {
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-sm);
            color: var(--text-muted);
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .breadcrumb-sep {
            color: var(--text-faint);
        }

        .breadcrumb-current {
            color: var(--accent-primary);
            font-weight: 500;
        }

        .header-actions {
            display: flex;
            align-items: center;
            gap: 0.75rem;
        }

        /* Search */
        .search-container {
            position: relative;
        }

        .search-toggle {
            display: flex;
            align-items: center;
            gap: 0.5rem;
            padding: 0.5rem 1rem;
            background: var(--bg-tertiary);
            border: 1px solid var(--border-primary);
            border-radius: 8px;
            color: var(--text-muted);
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-sm);
            cursor: pointer;
            transition: all var(--transition-fast);
        }

        .search-toggle:hover {
            background: var(--bg-elevated);
            border-color: var(--border-accent);
            color: var(--text-secondary);
        }

        .search-toggle kbd {
            font-family: inherit;
            font-size: var(--text-xs);
            padding: 0.125rem 0.375rem;
            background: var(--bg-secondary);
            border: 1px solid var(--border-primary);
            border-radius: 4px;
            color: var(--text-faint);
        }

        /* Theme toggle */
        .theme-toggle {
            width: 40px;
            height: 40px;
            display: flex;
            align-items: center;
            justify-content: center;
            background: transparent;
            border: 1px solid var(--border-primary);
            border-radius: 8px;
            color: var(--text-muted);
            cursor: pointer;
            transition: all var(--transition-fast);
        }

        .theme-toggle:hover {
            background: var(--bg-tertiary);
            border-color: var(--border-accent);
            color: var(--accent-primary);
        }

        .theme-toggle svg {
            width: 20px;
            height: 20px;
        }

        .theme-toggle .icon-sun { display: none; }
        .theme-toggle .icon-moon { display: block; }
        [data-theme=\"light\"] .theme-toggle .icon-sun { display: block; }
        [data-theme=\"light\"] .theme-toggle .icon-moon { display: none; }

        /* ===============================================================
           SIDEBAR NAVIGATION
           =============================================================== */

        .sidebar {
            position: fixed;
            top: 0;
            left: 0;
            width: var(--sidebar-width);
            height: 100vh;
            background: var(--bg-secondary);
            border-right: 1px solid var(--border-secondary);
            display: flex;
            flex-direction: column;
            z-index: 100;
            transition: transform var(--transition-base);
        }

        .sidebar-header {
            padding: 1.5rem;
            border-bottom: 1px solid var(--border-secondary);
        }

        .sidebar-brand {
            display: flex;
            align-items: center;
            gap: 0.75rem;
            text-decoration: none;
            margin-bottom: 0.75rem;
        }

        .sidebar-logo {
            width: 36px;
            height: 36px;
            background: linear-gradient(135deg, var(--accent-primary) 0%, var(--accent-tertiary) 100%);
            border-radius: 8px;
            display: flex;
            align-items: center;
            justify-content: center;
            font-family: 'Cormorant Garamond', serif;
            font-weight: 700;
            font-size: 1.25rem;
            color: white;
            box-shadow: 0 2px 8px var(--accent-glow);
        }

        .sidebar-title {
            font-family: 'Cormorant Garamond', serif;
            font-size: var(--text-xl);
            font-weight: 600;
            color: var(--text-primary);
            letter-spacing: -0.01em;
        }

        .sidebar-subtitle {
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            color: var(--text-muted);
            text-transform: uppercase;
            letter-spacing: 0.1em;
        }

        /* Navigation */
        .nav-container {
            flex: 1;
            overflow-y: auto;
            padding: 1rem 0;
            scrollbar-width: thin;
            scrollbar-color: var(--border-primary) transparent;
        }

        .nav-container::-webkit-scrollbar {
            width: 6px;
        }

        .nav-container::-webkit-scrollbar-track {
            background: transparent;
        }

        .nav-container::-webkit-scrollbar-thumb {
            background: var(--border-primary);
            border-radius: 3px;
        }

        .nav-section {
            margin-bottom: 0.5rem;
        }

        .nav-section-header {
            display: flex;
            align-items: center;
            justify-content: space-between;
            padding: 0.625rem 1.5rem;
            cursor: pointer;
            user-select: none;
        }

        .nav-section-title {
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            font-weight: 600;
            color: var(--text-faint);
            text-transform: uppercase;
            letter-spacing: 0.1em;
        }

        .nav-section-toggle {
            width: 16px;
            height: 16px;
            color: var(--text-faint);
            transition: transform var(--transition-fast);
        }

        .nav-section.collapsed .nav-section-toggle {
            transform: rotate(-90deg);
        }

        .nav-section-items {
            overflow: hidden;
            transition: max-height var(--transition-base);
        }

        .nav-section.collapsed .nav-section-items {
            max-height: 0 !important;
        }

        .nav-link {
            display: flex;
            align-items: baseline;
            gap: 0.625rem;
            padding: 0.5rem 1.5rem 0.5rem 1.75rem;
            color: var(--text-secondary);
            text-decoration: none;
            font-size: var(--text-sm);
            border-left: 2px solid transparent;
            transition: all var(--transition-fast);
            position: relative;
        }

        .nav-link::before {
            content: '';
            position: absolute;
            left: 0;
            top: 0;
            bottom: 0;
            width: 2px;
            background: transparent;
            transition: background var(--transition-fast);
        }

        .nav-link:hover {
            color: var(--text-primary);
            background: var(--accent-subtle);
        }

        .nav-link:hover::before {
            background: var(--accent-primary);
        }

        .nav-link.active {
            color: var(--accent-primary);
            background: var(--accent-subtle);
            font-weight: 500;
        }

        .nav-link.active::before {
            background: var(--accent-primary);
        }

        .nav-link .chapter-num {
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            color: var(--text-faint);
            min-width: 1.75rem;
            font-variant-numeric: tabular-nums;
        }

        .nav-link.active .chapter-num {
            color: var(--accent-secondary);
        }

        /* Subsection links */
        .nav-subsection {
            padding-left: 3rem;
        }

        .nav-subsection .nav-link {
            font-size: var(--text-xs);
            padding: 0.375rem 1rem 0.375rem 0;
            color: var(--text-muted);
            border-left: none;
        }

        .nav-subsection .nav-link::before {
            display: none;
        }

        .nav-subsection .nav-link:hover {
            color: var(--text-secondary);
            background: transparent;
        }

        .nav-subsection .nav-link.active {
            color: var(--accent-primary);
            background: transparent;
        }

        /* ===============================================================
           MAIN CONTENT AREA
           =============================================================== */

        .main {
            margin-left: var(--sidebar-width);
            min-height: 100vh;
            padding-top: var(--header-height);
            transition: margin-left var(--transition-base);
        }

        .content {
            max-width: var(--content-max);
            margin: 0 auto;
            padding: 3rem 2.5rem 6rem;
        }

        /* Document header */
        .doc-header {
            margin-bottom: 3rem;
            padding-bottom: 2rem;
            border-bottom: 1px solid var(--border-secondary);
        }

        .doc-header h1 {
            font-family: 'Cormorant Garamond', serif;
            font-size: var(--text-4xl);
            font-weight: 600;
            line-height: 1.2;
            color: var(--text-primary);
            letter-spacing: -0.02em;
            margin-bottom: 0.75rem;
        }

        .doc-header h1 span {
            background: linear-gradient(135deg, var(--accent-primary) 0%, var(--accent-tertiary) 100%);
            -webkit-background-clip: text;
            -webkit-text-fill-color: transparent;
            background-clip: text;
        }

        .doc-meta {
            display: flex;
            align-items: center;
            gap: 1.5rem;
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-sm);
            color: var(--text-muted);
        }

        .doc-meta-item {
            display: flex;
            align-items: center;
            gap: 0.5rem;
        }

        .doc-meta-item svg {
            width: 16px;
            height: 16px;
            color: var(--text-faint);
        }

        /* ===============================================================
           TYPOGRAPHY
           =============================================================== */

        h2 {
            font-family: 'Cormorant Garamond', serif;
            font-size: var(--text-2xl);
            font-weight: 600;
            color: var(--text-primary);
            margin-top: 3.5rem;
            margin-bottom: 1rem;
            padding-top: 1.5rem;
            letter-spacing: -0.01em;
            position: relative;
        }

        h2:first-of-type {
            margin-top: 0;
            padding-top: 0;
        }

        h3 {
            font-family: 'DM Sans', sans-serif;
            font-size: var(--text-lg);
            font-weight: 600;
            color: var(--text-primary);
            margin-top: 2.5rem;
            margin-bottom: 0.75rem;
        }

        h4 {
            font-family: 'DM Sans', sans-serif;
            font-size: var(--text-base);
            font-weight: 600;
            color: var(--text-secondary);
            margin-top: 2rem;
            margin-bottom: 0.5rem;
        }

        p {
            margin-bottom: 1.25rem;
            color: var(--text-secondary);
        }

        strong {
            color: var(--text-primary);
            font-weight: 600;
        }

        em {
            font-style: italic;
            color: var(--text-primary);
        }

        a {
            color: var(--link);
            text-decoration: none;
            transition: color var(--transition-fast);
        }

        a:hover {
            color: var(--link-hover);
        }

        /* Section anchor links */
        .section-anchor {
            color: var(--text-faint);
            margin-left: 0.5rem;
            opacity: 0;
            font-weight: 400;
            transition: opacity var(--transition-fast), color var(--transition-fast);
            text-decoration: none;
        }

        h2:hover .section-anchor,
        h3:hover .section-anchor {
            opacity: 1;
        }

        .section-anchor:hover {
            color: var(--accent-primary);
        }

        /* Lists */
        ul, ol {
            margin-bottom: 1.25rem;
            padding-left: 1.5rem;
            color: var(--text-secondary);
        }

        li {
            margin-bottom: 0.5rem;
        }

        li::marker {
            color: var(--text-faint);
        }

        /* Code */
        code {
            font-family: 'IBM Plex Mono', monospace;
            font-size: 0.9em;
            background: var(--code-bg);
            padding: 0.2em 0.4em;
            border-radius: 4px;
            color: var(--code-text);
            border: 1px solid var(--code-border);
        }

        pre {
            position: relative;
            background: var(--code-bg);
            border: 1px solid var(--code-border);
            border-radius: 10px;
            padding: 1.25rem 1.5rem;
            overflow-x: auto;
            margin-bottom: 1.5rem;
            font-size: var(--text-sm);
            line-height: 1.6;
        }

        pre::before {
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            height: 3px;
            background: linear-gradient(90deg, var(--accent-primary), var(--accent-tertiary));
            border-radius: 10px 10px 0 0;
            opacity: 0.5;
        }

        pre code {
            background: none;
            padding: 0;
            color: var(--text-primary);
            border: none;
            font-size: inherit;
        }

        /* Blockquotes */
        blockquote {
            position: relative;
            margin: 1.5rem 0;
            padding: 1rem 1.5rem;
            background: var(--accent-subtle);
            border-radius: 0 10px 10px 0;
            color: var(--text-secondary);
        }

        blockquote::before {
            content: '';
            position: absolute;
            left: 0;
            top: 0;
            bottom: 0;
            width: 3px;
            background: linear-gradient(180deg, var(--accent-primary), var(--accent-tertiary));
            border-radius: 3px;
        }

        blockquote p:last-child {
            margin-bottom: 0;
        }

        /* Tables */
        table {
            width: 100%;
            border-collapse: collapse;
            margin: 1.5rem 0;
            font-size: var(--text-sm);
        }

        th, td {
            text-align: left;
            padding: 0.875rem 1rem;
            border: 1px solid var(--border-primary);
        }

        th {
            background: var(--bg-tertiary);
            font-weight: 600;
            color: var(--text-primary);
            font-family: 'DM Sans', sans-serif;
        }

        td {
            color: var(--text-secondary);
        }

        tr:nth-child(even) td {
            background: var(--bg-secondary);
        }

        /* Horizontal rules */
        hr {
            border: none;
            border-top: 1px solid var(--border-secondary);
            margin: 3rem 0;
        }

        /* Abstract */
        .abstract {
            background: linear-gradient(135deg, var(--bg-secondary) 0%, var(--bg-tertiary) 100%);
            border: 1px solid var(--border-primary);
            border-radius: 12px;
            padding: 2rem 2.5rem;
            margin-bottom: 2.5rem;
            position: relative;
            overflow: hidden;
        }

        .abstract::before {
            content: '';
            position: absolute;
            top: 0;
            left: 0;
            right: 0;
            height: 3px;
            background: linear-gradient(90deg, var(--accent-primary), var(--accent-tertiary), var(--accent-primary));
        }

        .abstract-title {
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            font-weight: 600;
            color: var(--accent-primary);
            text-transform: uppercase;
            letter-spacing: 0.15em;
            margin-bottom: 1rem;
        }

        .abstract p {
            font-size: var(--text-sm);
            line-height: 1.8;
        }

        /* ===============================================================
           SEARCH MODAL
           =============================================================== */

        .search-modal {
            display: none;
            position: fixed;
            inset: 0;
            z-index: 1000;
            padding: 15vh 1rem 1rem;
            background: rgba(0, 0, 0, 0.7);
            backdrop-filter: blur(4px);
            -webkit-backdrop-filter: blur(4px);
        }

        .search-modal.open {
            display: block;
            animation: fadeIn 0.2s ease;
        }

        @keyframes fadeIn {
            from { opacity: 0; }
            to { opacity: 1; }
        }

        .search-dialog {
            max-width: 600px;
            margin: 0 auto;
            background: var(--bg-secondary);
            border: 1px solid var(--border-primary);
            border-radius: 12px;
            box-shadow: 0 25px 50px -12px rgba(0, 0, 0, 0.5);
            overflow: hidden;
            animation: slideUp 0.2s ease;
        }

        @keyframes slideUp {
            from { transform: translateY(10px); opacity: 0; }
            to { transform: translateY(0); opacity: 1; }
        }

        .search-input-container {
            display: flex;
            align-items: center;
            padding: 1rem 1.25rem;
            border-bottom: 1px solid var(--border-secondary);
        }

        .search-input-container svg {
            width: 20px;
            height: 20px;
            color: var(--text-muted);
            margin-right: 0.75rem;
            flex-shrink: 0;
        }

        .search-input {
            flex: 1;
            background: transparent;
            border: none;
            outline: none;
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-base);
            color: var(--text-primary);
        }

        .search-input::placeholder {
            color: var(--text-muted);
        }

        .search-results {
            max-height: 50vh;
            overflow-y: auto;
            padding: 0.5rem;
        }

        .search-result {
            display: block;
            padding: 0.875rem 1rem;
            border-radius: 8px;
            text-decoration: none;
            transition: background var(--transition-fast);
        }

        .search-result:hover,
        .search-result.selected {
            background: var(--bg-tertiary);
        }

        .search-result-title {
            font-weight: 500;
            color: var(--text-primary);
            margin-bottom: 0.25rem;
        }

        .search-result-context {
            font-size: var(--text-sm);
            color: var(--text-muted);
        }

        .search-result mark {
            background: var(--accent-glow);
            color: var(--accent-primary);
            padding: 0.1em 0.2em;
            border-radius: 2px;
        }

        .search-footer {
            padding: 0.75rem 1rem;
            border-top: 1px solid var(--border-secondary);
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            color: var(--text-muted);
            display: flex;
            gap: 1.5rem;
        }

        .search-footer kbd {
            font-family: inherit;
            padding: 0.125rem 0.375rem;
            background: var(--bg-tertiary);
            border: 1px solid var(--border-primary);
            border-radius: 4px;
            margin-right: 0.25rem;
        }

        /* ===============================================================
           PROGRESS BAR
           =============================================================== */

        .progress {
            position: fixed;
            top: 0;
            left: var(--sidebar-width);
            right: 0;
            height: 2px;
            background: var(--bg-tertiary);
            z-index: 95;
            transition: left var(--transition-base);
        }

        .progress-bar {
            height: 100%;
            background: linear-gradient(90deg, var(--accent-primary), var(--accent-secondary));
            width: 0%;
            transition: width 0.1s ease;
        }

        /* ===============================================================
           MOBILE NAVIGATION
           =============================================================== */

        .menu-toggle {
            display: none;
            position: fixed;
            top: 1rem;
            left: 1rem;
            z-index: 200;
            width: 44px;
            height: 44px;
            background: var(--bg-secondary);
            border: 1px solid var(--border-primary);
            border-radius: 10px;
            cursor: pointer;
            color: var(--text-primary);
            align-items: center;
            justify-content: center;
        }

        .menu-toggle svg {
            width: 24px;
            height: 24px;
        }

        .overlay {
            display: none;
            position: fixed;
            inset: 0;
            background: rgba(0, 0, 0, 0.6);
            z-index: 50;
            backdrop-filter: blur(2px);
            -webkit-backdrop-filter: blur(2px);
        }

        .overlay.open {
            display: block;
        }

        /* ===============================================================
           RESPONSIVE DESIGN
           =============================================================== */

        @media (max-width: 1024px) {
            .sidebar {
                transform: translateX(-100%);
            }

            .sidebar.open {
                transform: translateX(0);
            }

            .main {
                margin-left: 0;
            }

            .header {
                left: 0;
            }

            .progress {
                left: 0;
            }

            .menu-toggle {
                display: flex;
            }

            .content {
                padding: calc(var(--header-height) + 2rem) 1.5rem 4rem;
            }

            .doc-header h1 {
                font-size: var(--text-3xl);
            }

            .doc-meta {
                flex-wrap: wrap;
                gap: 0.75rem;
            }
        }

        @media (max-width: 640px) {
            .content {
                padding: calc(var(--header-height) + 1.5rem) 1rem 3rem;
            }

            .doc-header h1 {
                font-size: var(--text-2xl);
            }

            h2 {
                font-size: var(--text-xl);
            }

            .abstract {
                padding: 1.5rem;
            }

            pre {
                padding: 1rem;
                font-size: var(--text-xs);
            }

            .search-toggle span {
                display: none;
            }

            .header-actions {
                gap: 0.5rem;
            }
        }

        /* ===============================================================
           FOOTER
           =============================================================== */

        .generated {
            margin-top: 4rem;
            padding-top: 1.5rem;
            border-top: 1px solid var(--border-secondary);
            font-family: 'IBM Plex Mono', monospace;
            font-size: var(--text-xs);
            color: var(--text-faint);
            text-align: center;
        }

        .generated a {
            color: var(--text-muted);
        }

        .generated a:hover {
            color: var(--accent-primary);
        }
    </style>
</head>
<body>
    <!-- Mobile menu toggle -->
    <button class=\"menu-toggle\" onclick=\"toggleMenu()\" aria-label=\"Toggle navigation\">
        <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
            <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M4 6h16M4 12h16M4 18h16\"/>
        </svg>
    </button>

    <div class=\"overlay\" onclick=\"toggleMenu()\"></div>

    <!-- Sidebar -->
    <nav class=\"sidebar\">
        <div class=\"sidebar-header\">
            <a href=\"#\" class=\"sidebar-brand\">
                <div class=\"sidebar-logo\">λ</div>
                <div class=\"sidebar-title\">The Fold</div>
            </a>
            <div class=\"sidebar-subtitle\">Technical Report · 2026</div>
        </div>

        <div class=\"nav-container\">
            <div class=\"nav-section\">
                <a href=\"#abstract\" class=\"nav-link active\">Abstract</a>
            </div>

            <div class=\"nav-section\">
                <div class=\"nav-section-header\" onclick=\"toggleSection(this.parentElement)\">
                    <span class=\"nav-section-title\">Foundations</span>
                    <svg class=\"nav-section-toggle\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                        <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 9l-7 7-7-7\"/>
                    </svg>
                </div>
                <div class=\"nav-section-items\">
                    <a href=\"#introduction\" class=\"nav-link\"><span class=\"chapter-num\">1</span>Introduction</a>
                    <a href=\"#architecture\" class=\"nav-link\"><span class=\"chapter-num\">2</span>System Architecture</a>
                    <a href=\"#block-machine\" class=\"nav-link\"><span class=\"chapter-num\">3</span>The Block Machine</a>
                    <a href=\"#block-calculus\" class=\"nav-link\"><span class=\"chapter-num\">4</span>The Block Calculus</a>
                </div>
            </div>

            <div class=\"nav-section\">
                <div class=\"nav-section-header\" onclick=\"toggleSection(this.parentElement)\">
                    <span class=\"nav-section-title\">Type System</span>
                    <svg class=\"nav-section-toggle\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                        <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 9l-7 7-7-7\"/>
                    </svg>
                </div>
                <div class=\"nav-section-items\">
                    <a href=\"#type-theory\" class=\"nav-link\"><span class=\"chapter-num\">5</span>The Type Theory</a>
                    <a href=\"#module-system\" class=\"nav-link\"><span class=\"chapter-num\">6</span>The Module System</a>
                </div>
            </div>

            <div class=\"nav-section\">
                <div class=\"nav-section-header\" onclick=\"toggleSection(this.parentElement)\">
                    <span class=\"nav-section-title\">Implementation</span>
                    <svg class=\"nav-section-toggle\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                        <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 9l-7 7-7-7\"/>
                    </svg>
                </div>
                <div class=\"nav-section-items\">
                    <a href=\"#implementation\" class=\"nav-link\"><span class=\"chapter-num\">7</span>Implementation</a>
                    <a href=\"#egraph-core\" class=\"nav-link\"><span class=\"chapter-num\">7b</span>E-Graph Core</a>
                    <a href=\"#equality-sat\" class=\"nav-link\"><span class=\"chapter-num\">7c</span>Equality Saturation</a>
                    <a href=\"#meta-tooling\" class=\"nav-link\"><span class=\"chapter-num\">8</span>Meta-Tooling</a>
                    <a href=\"#agent-substrate\" class=\"nav-link\"><span class=\"chapter-num\">9</span>Agent Substrate</a>
                    <a href=\"#evaluation\" class=\"nav-link\"><span class=\"chapter-num\">10</span>Evaluation</a>
                </div>
            </div>

            <div class=\"nav-section\">
                <div class=\"nav-section-header\" onclick=\"toggleSection(this.parentElement)\">
                    <span class=\"nav-section-title\">Context</span>
                    <svg class=\"nav-section-toggle\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                        <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 9l-7 7-7-7\"/>
                    </svg>
                </div>
                <div class=\"nav-section-items\">
                    <a href=\"#related-work\" class=\"nav-link\"><span class=\"chapter-num\">11</span>Related Work</a>
                    <a href=\"#limitations\" class=\"nav-link\"><span class=\"chapter-num\">12</span>Limitations</a>
                    <a href=\"#future-work\" class=\"nav-link\"><span class=\"chapter-num\">13</span>Future Work</a>
                    <a href=\"#conclusion\" class=\"nav-link\"><span class=\"chapter-num\">14</span>Conclusion</a>
                </div>
            </div>

            <div class=\"nav-section\">
                <div class=\"nav-section-header\" onclick=\"toggleSection(this.parentElement)\">
                    <span class=\"nav-section-title\">Appendices</span>
                    <svg class=\"nav-section-toggle\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                        <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M19 9l-7 7-7-7\"/>
                    </svg>
                </div>
                <div class=\"nav-section-items\">
                    <a href=\"#appendix-a\" class=\"nav-link\"><span class=\"chapter-num\">A</span>Block Calculus Syntax</a>
                    <a href=\"#appendix-b\" class=\"nav-link\"><span class=\"chapter-num\">B</span>Type Grammar</a>
                    <a href=\"#appendix-c\" class=\"nav-link\"><span class=\"chapter-num\">C</span>Kind Grammar</a>
                    <a href=\"#appendix-d\" class=\"nav-link\"><span class=\"chapter-num\">D</span>Manifest Schema</a>
                    <a href=\"#appendix-e\" class=\"nav-link\"><span class=\"chapter-num\">E</span>Comparison with Unison</a>
                    <a href=\"#references\" class=\"nav-link\">References</a>
                </div>
            </div>
        </div>
    </nav>

    <!-- Progress bar -->
    <div class=\"progress\">
        <div class=\"progress-bar\"></div>
    </div>

    <!-- Header -->
    <header class=\"header\">
        <div class=\"header-left\">
            <div class=\"breadcrumb\">
                <span>docs</span>
                <span class=\"breadcrumb-sep\">/</span>
                <span class=\"breadcrumb-current\">technical-report</span>
            </div>
        </div>
        <div class=\"header-actions\">
            <button class=\"search-toggle\" onclick=\"openSearch()\">
                <svg width=\"16\" height=\"16\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                    <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z\"/>
                </svg>
                <span>Search...</span>
                <kbd>⌘K</kbd>
            </button>
            <button class=\"theme-toggle\" onclick=\"toggleTheme()\" aria-label=\"Toggle theme\">
                <svg class=\"icon-moon\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                    <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M20.354 15.354A9 9 0 018.646 3.646 9.003 9.003 0 0012 21a9.003 9.003 0 008.354-5.646z\"/>
                </svg>
                <svg class=\"icon-sun\" fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                    <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M12 3v1m0 16v1m9-9h-1M4 12H3m15.364 6.364l-.707-.707M6.343 6.343l-.707-.707m12.728 0l-.707.707M6.343 17.657l-.707.707M16 12a4 4 0 11-8 0 4 4 0 018 0z\"/>
                </svg>
            </button>
        </div>
    </header>

    <!-- Search modal -->
    <div class=\"search-modal\" id=\"searchModal\" onclick=\"if(event.target === this) closeSearch()\">
        <div class=\"search-dialog\">
            <div class=\"search-input-container\">
                <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                    <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M21 21l-6-6m2-5a7 7 0 11-14 0 7 7 0 0114 0z\"/>
                </svg>
                <input type=\"text\" class=\"search-input\" id=\"searchInput\" placeholder=\"Search documentation...\" oninput=\"handleSearch(this.value)\">
            </div>
            <div class=\"search-results\" id=\"searchResults\">
                <div style=\"padding: 2rem; text-align: center; color: var(--text-muted);\">
                    Type to search chapters, sections, and keywords
                </div>
            </div>
            <div class=\"search-footer\">
                <span><kbd>↵</kbd> to select</span>
                <span><kbd>↑</kbd><kbd>↓</kbd> to navigate</span>
                <span><kbd>esc</kbd> to close</span>
            </div>
        </div>
    </div>

    <!-- Main content -->
    <main class=\"main\">
        <div class=\"content\">
            <div class=\"doc-header\">
                <h1>The Fold: <span>A Content-Addressable Homoiconic Universe</span></h1>
                <div class=\"doc-meta\">
                    <div class=\"doc-meta-item\">
                        <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                            <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M9 12h6m-6 4h6m2 5H7a2 2 0 01-2-2V5a2 2 0 012-2h5.586a1 1 0 01.707.293l5.414 5.414a1 1 0 01.293.707V19a2 2 0 01-2 2z\"/>
                        </svg>
                        Technical Report
                    </div>
                    <div class=\"doc-meta-item\">
                        <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                            <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M8 7V3m8 4V3m-9 8h10M5 21h14a2 2 0 002-2V7a2 2 0 00-2-2H5a2 2 0 00-2 2v12a2 2 0 002 2z\"/>
                        </svg>
                        January 2026
                    </div>
                    <div class=\"doc-meta-item\">
                        <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
                            <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M10 20l4-16m4 4l4 4-4 4M6 16l-4-4 4-4\"/>
                        </svg>
                        Chez Scheme
                    </div>
                </div>
            </div>

")

;;; Abstract and first two sections (hand-crafted in original)
(define *abstract-section* "            <section id=\"abstract\">
                <div class=\"abstract\">
                    <div class=\"abstract-title\">Abstract</div>
                    <p>We present <strong>The Fold</strong>, a programming system built on a content-addressable homoiconic foundation. At its core lies a <em>block machine</em> where every computational unit&mdash;code, data, and types&mdash;is represented as a cryptographically-addressed immutable structure. Through a two-phase normalization process&mdash;&alpha;-normalization via de Bruijn indices and algebraic canonicalization (commutative sorting, associative flattening)&mdash;semantically equivalent expressions produce identical hashes, achieving true <em>semantic identity</em>: two functions that behave identically are the same function, regardless of variable naming or argument order in commutative operations.</p>

                    <p>The Fold implements a <em>gradual dependent type system</em> combining bidirectional type checking (following Dunfield &amp; Krishnaswami), dependent function and pair types (&Pi;, &Sigma;), higher-kinded types, type classes via dictionary-passing, and GADTs with pattern refinement. Gradual typing through holes enables incremental specification without sacrificing soundness where types are known.</p>

                    <p>The system organizes verified code into a <em>module DAG</em> (internally called the \"skill lattice\")&mdash;a tiered directed acyclic graph where modules declare dependencies, purity guarantees, and complexity bounds. Functions are bounded rather than structurally total&mdash;fuel limits guarantee termination of any execution, though this is weaker than type-theoretic totality. This structure enables compositional verification: if dependencies are verified and a module is verified against those dependencies, the module is verified. A BM25-powered semantic search engine enables discovery across thousands of exports.</p>

                    <p><strong>Key contributions:</strong> (1) a block calculus formalizing content-addressed computation with &alpha;-equivalence, (2) a dependent type system integrated with gradual typing, (3) a compositional module system with fuel-bounded complexity guarantees. The implementation, built entirely in Chez Scheme with no third-party dependencies, demonstrates that reproducible, verifiable computation can emerge from simple foundations.</p>
                </div>
            </section>

")

(define *intro-section* "            <section id=\"introduction\">
                <h2>1. Introduction<a href=\"#introduction\" class=\"section-anchor\">#</a></h2>

                <h3>1.1 The Problem with File-Based Programming</h3>
                <p>Traditional programming systems identify code by <em>location</em>: file paths, module names, package versions. This conflation of identity with storage creates fundamental problems:</p>

                <ol>
                    <li><strong>Semantic drift</strong>: The same file path can refer to different code at different times</li>
                    <li><strong>Dependency hell</strong>: Version conflicts arise from name-based resolution</li>
                    <li><strong>&alpha;-equivalence violation</strong>: <code>(&lambda; x. x)</code> and <code>(&lambda; y. y)</code> are stored differently despite identical semantics</li>
                    <li><strong>Non-reproducibility</strong>: Builds depend on mutable external state</li>
                </ol>

                <p>Consider two developers who independently write the identity function:</p>

<pre><code>;; Developer A
(define id-a (lambda (x) x))

;; Developer B
(define id-b (lambda (y) y))</code></pre>

                <p>In file-based systems, these are distinct entities requiring coordination. Yet semantically, they are the same function. This gap between syntax and semantics pervades software engineering.</p>

                <h3>1.2 The Proposal: Content-Addressed Homoiconic Computation</h3>
                <p>The Fold addresses these problems through three interlocking mechanisms:</p>

                <ol>
                    <li><strong>Content Addressing</strong>: Every value's identity is its cryptographic hash. Two values with the same content have the same identity&mdash;automatically, universally, permanently.</li>
                    <li><strong>&alpha;-Normalization</strong>: Before hashing, expressions are normalized using de Bruijn indices, eliminating variable naming from identity. <code>(&lambda; x. x)</code> and <code>(&lambda; y. y)</code> normalize to <code>(&lambda; (dv 0))</code> and hash identically.</li>
                    <li><strong>Homoiconicity</strong>: Code is data. Programs are S-expressions that serialize to blocks, enabling introspection, metaprogramming, and uniform treatment of all computational artifacts.</li>
                </ol>

                <p>The result is a system where <em>semantic identity replaces syntactic identity</em>. Functions that behave the same are the same. Verified code stays verified. Dependencies are content, not names.</p>

                <h3>1.3 Contributions</h3>

                <h4>Contribution 1: Block Calculus with Multi-Phase Normalization</h4>
                <p>We formalize a calculus where computation operates over content-addressed blocks. The key innovation is integrating a two-phase normalization pipeline with cryptographic hashing:</p>

                <ol>
                    <li><strong>Algebraic canonicalization</strong>: Sort arguments of commutative operations, flatten associative operations, reorder independent bindings</li>
                    <li><strong>&alpha;-normalization</strong>: Convert to de Bruijn indices, eliminating variable naming</li>
                </ol>

                <p>This yields the semantic identity property:</p>

<pre><code>&alpha;-equiv(e&#x2081;, e&#x2082;) &#x27F9; hash(normalize(e&#x2081;)) = hash(normalize(e&#x2082;))
(+ a b) &#x2261; (+ b a)           ; Commutative equivalence
(+ (+ a b) c) &#x2261; (+ a b c)   ; Associative equivalence</code></pre>

                <h4>Contribution 2: Gradual Dependent Type System</h4>
                <p>We implement a type system combining:</p>
                <ul>
                    <li>Bidirectional type checking for predictable inference</li>
                    <li>Dependent types (&Pi;, &Sigma;) for precise specifications</li>
                    <li>Higher-kinded types and type classes for abstraction</li>
                    <li>Gradual typing through holes for incremental development</li>
                </ul>

                <h4>Contribution 3: Compositional Module System</h4>
                <p>We organize code into a tiered DAG where each module declares:</p>
                <ul>
                    <li>Dependencies (other modules)</li>
                    <li>Purity (total, partial, effectful)</li>
                    <li>Complexity bounds (fuel consumption)</li>
                </ul>

                <p>This enables <em>compositional verification</em>: verifying a module requires only verifying its code against already-verified dependencies, not the entire transitive closure.</p>
            </section>

")

(define *arch-section* "            <section id=\"architecture\">
                <h2>2. System Architecture<a href=\"#architecture\" class=\"section-anchor\">#</a></h2>

                <p>The Fold employs a <em>three-layer architecture</em> separating pure computation from effectful boundaries:</p>

<pre><code>&#x250C;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2510;
&#x2502;                         User Layer                          &#x2502;
&#x2502;              Applications, experiments, scripts             &#x2502;
&#x251C;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2524;
&#x2502;                      Boundary Layer                         &#x2502;
&#x2502;         IO, validation, capability minting, effects         &#x2502;
&#x251C;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2524;
&#x2502;                        Core Layer                           &#x2502;
&#x2502;          Pure, total, content-addressed, verified           &#x2502;
&#x2514;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2500;&#x2518;</code></pre>

                <h3>2.1 The Core Layer</h3>
                <p>The Core is the mathematical heart of The Fold. Code in Core satisfies three properties:</p>

                <p><strong>Purity</strong>: No side effects. Functions depend only on their arguments and produce only their return values. This enables equational reasoning&mdash;if <code>f(x) = y</code>, then <code>f(x)</code> can always be replaced with <code>y</code>.</p>

                <p><strong>Bounded Computation</strong>: Every computation terminates within a declared resource bound. This is enforced via <em>fuel-bounded execution</em>: every computation receives a fuel budget that decrements with each reduction step. Exhausting fuel yields an <code>out-of-fuel</code> error rather than infinite looping.</p>

                <blockquote>
                    <p><strong>Important distinction</strong>: This is <em>not</em> totality in the type-theoretic sense. True totality (as in Agda or Idris) proves termination for all inputs via structural recursion checks or sized types&mdash;a property of the function itself. Fuel bounds instead guarantee that any particular execution completes&mdash;a property of the runtime.</p>
                </blockquote>

                <p><strong>Trust</strong>: Core assumes <em>perfect input</em>. It performs no validation, no defensive checks, no error recovery. If you pass malformed data to Core, behavior is undefined. This simplicity enables formal verification.</p>

                <h3>2.2 The Boundary Layer</h3>
                <p>The Boundary is \"fallen\"&mdash;it interacts with the impure outside world:</p>
                <ul>
                    <li>File I/O, network, user input</li>
                    <li>Validation and error handling</li>
                    <li>Capability minting from external resources</li>
                    <li>Session management and persistence</li>
                </ul>

                <h3>2.3 The User Layer</h3>
                <p>Applications and experiments that compose Shell and Core functionality. This layer has maximum freedom and minimum guarantees.</p>
            </section>

")

(define *page-footer* "
            <div class=\"generated\">
                Generated by The Fold's markdown parser &mdash; dogfooding <code>lattice/dsl/markdown</code>
            </div>
        </div>
    </main>

    <script>
        // ===============================================================
        // INTERACTIVE FUNCTIONALITY
        // ===============================================================

        // Theme management
        function getPreferredTheme() {
            const saved = localStorage.getItem('theme');
            if (saved) return saved;
            return window.matchMedia('(prefers-color-scheme: light)').matches ? 'light' : 'dark';
        }

        function setTheme(theme) {
            document.documentElement.setAttribute('data-theme', theme);
            localStorage.setItem('theme', theme);
        }

        function toggleTheme() {
            const current = document.documentElement.getAttribute('data-theme') || 'dark';
            setTheme(current === 'dark' ? 'light' : 'dark');
        }

        // Initialize theme
        setTheme(getPreferredTheme());

        // Mobile menu
        function toggleMenu() {
            document.querySelector('.sidebar').classList.toggle('open');
            document.querySelector('.overlay').classList.toggle('open');
        }

        // Section collapse
        function toggleSection(section) {
            section.classList.toggle('collapsed');
            const items = section.querySelector('.nav-section-items');
            if (items) {
                if (section.classList.contains('collapsed')) {
                    items.style.maxHeight = '0';
                } else {
                    items.style.maxHeight = items.scrollHeight + 'px';
                }
            }
        }

        // Initialize section heights
        document.querySelectorAll('.nav-section-items').forEach(items => {
            items.style.maxHeight = items.scrollHeight + 'px';
        });

        // Active section tracking
        const sections = document.querySelectorAll('section[id]');
        const navLinks = document.querySelectorAll('.nav-link');
        let currentSection = '';

        function updateActiveLink() {
            const scrollY = window.scrollY;
            const headerOffset = 100;

            sections.forEach(section => {
                const sectionTop = section.offsetTop - headerOffset;
                const sectionHeight = section.offsetHeight;
                const sectionId = section.getAttribute('id');

                if (scrollY >= sectionTop && scrollY < sectionTop + sectionHeight) {
                    if (currentSection !== sectionId) {
                        currentSection = sectionId;
                        navLinks.forEach(link => {
                            link.classList.remove('active');
                            if (link.getAttribute('href') === '#' + sectionId) {
                                link.classList.add('active');
                                // Update breadcrumb
                                const breadcrumb = document.querySelector('.breadcrumb-current');
                                if (breadcrumb) {
                                    breadcrumb.textContent = link.textContent.trim() || sectionId;
                                }
                            }
                        });
                    }
                }
            });
        }

        // Progress bar
        function updateProgress() {
            const scrollTop = window.scrollY;
            const docHeight = document.documentElement.scrollHeight - window.innerHeight;
            const progress = Math.min((scrollTop / docHeight) * 100, 100);
            document.querySelector('.progress-bar').style.width = progress + '%';
        }

        // Scroll event listeners
        let ticking = false;
        window.addEventListener('scroll', () => {
            if (!ticking) {
                window.requestAnimationFrame(() => {
                    updateActiveLink();
                    updateProgress();
                    ticking = false;
                });
                ticking = true;
            }
        });

        // Close mobile menu on link click
        navLinks.forEach(link => {
            link.addEventListener('click', () => {
                document.querySelector('.sidebar').classList.remove('open');
                document.querySelector('.overlay').classList.remove('open');
            });
        });

        // Search functionality
        const searchData = [];
        sections.forEach(section => {
            const id = section.getAttribute('id');
            const h2 = section.querySelector('h2');
            const title = h2 ? h2.textContent.replace('#', '').trim() : id;
            const text = section.textContent.substring(0, 500);
            searchData.push({ id, title, text });
        });

        function openSearch() {
            document.getElementById('searchModal').classList.add('open');
            document.getElementById('searchInput').focus();
        }

        function closeSearch() {
            document.getElementById('searchModal').classList.remove('open');
            document.getElementById('searchInput').value = '';
            document.getElementById('searchResults').innerHTML = '<div style=\"padding: 2rem; text-align: center; color: var(--text-muted);\">Type to search chapters, sections, and keywords</div>';
        }

        function handleSearch(query) {
            const resultsContainer = document.getElementById('searchResults');

            if (!query.trim()) {
                resultsContainer.innerHTML = '<div style=\"padding: 2rem; text-align: center; color: var(--text-muted);\">Type to search chapters, sections, and keywords</div>';
                return;
            }

            const lowerQuery = query.toLowerCase();
            const results = searchData.filter(item =>
                item.title.toLowerCase().includes(lowerQuery) ||
                item.text.toLowerCase().includes(lowerQuery)
            ).slice(0, 8);

            if (results.length === 0) {
                resultsContainer.innerHTML = '<div style=\"padding: 2rem; text-align: center; color: var(--text-muted);\">No results found</div>';
                return;
            }

            resultsContainer.innerHTML = results.map(result => {
                const contextStart = Math.max(0, result.text.toLowerCase().indexOf(lowerQuery) - 40);
                const context = result.text.substring(contextStart, contextStart + 100).replace(/\\s+/g, ' ').trim();
                return `
                    <a href=\"#${result.id}\" class=\"search-result\" onclick=\"closeSearch()\">
                        <div class=\"search-result-title\">${result.title}</div>
                        <div class=\"search-result-context\">${context}...</div>
                    </a>
                `;
            }).join('');
        }

        // Keyboard shortcuts
        document.addEventListener('keydown', (e) => {
            // Cmd/Ctrl + K to open search
            if ((e.metaKey || e.ctrlKey) && e.key === 'k') {
                e.preventDefault();
                openSearch();
            }
            // Escape to close search
            if (e.key === 'Escape') {
                closeSearch();
            }
        });

        // Initialize
        updateActiveLink();
        updateProgress();
    </script>
</body>
</html>
")

(doc 'section 'section-title-map)

(define *section-titles*
  '(("block-machine"    "3. The Block Machine")
    ("block-calculus"   "4. The Block Calculus")
    ("type-theory"      "5. The Type Theory")
    ("module-system"    "6. The Module System")
    ("implementation"   "7. Implementation")
    ("egraph-core"      "7b. E-Graph Data Structures")
    ("equality-sat"     "7c. Equality Saturation")
    ("meta-tooling"     "8. Developer and Meta-Tooling")
    ("agent-substrate"  "9. The Fold as Agent Substrate")
    ("evaluation"       "10. Evaluation")
    ("related-work"     "11. Related Work")
    ("limitations"      "12. Limitations and Non-Goals")
    ("future-work"      "13. Future Work")
    ("conclusion"       "14. Conclusion")
    ("appendix-a"       "Appendix A: Block Calculus Formal Syntax")
    ("appendix-b"       "Appendix B: Type Grammar")
    ("appendix-c"       "Appendix C: Kind Grammar")
    ("appendix-d"       "Appendix D: Manifest Schema")
    ("appendix-e"       "Appendix E: Comparison with Unison")
    ("references"       "References")))

(define (get-section-title section-id)
  (let ([entry (assoc section-id *section-titles*)])
    (if entry (cadr entry) section-id)))

(doc 'section 'build-process)

(define (build-section entry)
  (doc 'type "(String × String) -> String")
  (doc 'description "Build a complete section from section-id and filename")
  (let* ([section-id (car entry)]
         [filename (cadr entry)]
         [title (get-section-title section-id)]
         [body-content (parse-chapter-body filename)])
    (printf "  Parsing ~a (~a)...\n" section-id filename)
    (string-append
     "            <section id=\"" section-id "\">\n"
     "                <h2>" title "<a href=\"#" section-id "\" class=\"section-anchor\">#</a></h2>\n\n"
     body-content
     "            </section>\n\n")))

;;; build-report : -> Unit
(define (build-report)
  (printf "Building navigable technical report from markdown...\n")
  (printf "  Source: ~a/\n" *report-source-dir*)
  (printf "  Output: ~a\n" *report-output-file*)
  (printf "\n")

  ;; Build all generated sections
  (let* ([generated-sections (map build-section *chapter-map*)]
         [all-sections (string-append
                        *abstract-section*
                        *intro-section*
                        *arch-section*
                        (apply string-append generated-sections))]
         [page (string-append *page-header* all-sections *page-footer*)])

    (printf "\n")
    (printf "  Writing output...\n")
    (write-file *report-output-file* page)
    (printf "  Done! ~a bytes written.\n" (string-length page))
    (printf "\n")
    (printf "Output: ~a\n" *report-output-file*)))

;;; ============================================================
;;; Entry Point
;;; ============================================================

(build-report)
