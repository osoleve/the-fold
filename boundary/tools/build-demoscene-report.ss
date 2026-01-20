(load "core/base/prelude.ss")

(doc 'module 'build-demoscene-report)
(doc 'description "Builds the technical report with refined CRT/demoscene aesthetics")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(load "lattice/dsl/markdown/block-parser.ss")
(load "lattice/dsl/markdown/html.ss")

(define *report-source-dir* "docs/technical-report")
(define *report-output-file* "/home/oso/fold/docs/technical-report-demo.html")

(define *chapter-map*
  '(("block-machine"    "03-the-block-machine.md")
    ("block-calculus"   "04-the-block-calculus.md")
    ("type-theory"      "05-the-type-theory.md")
    ("module-system"    "06-the-module-system.md")
    ("implementation"   "07-implementation.md")
    ("evaluation"       "08-evaluation.md")
    ("related-work"     "09-related-work.md")
    ("limitations"      "10-limitations-and-non-goals.md")
    ("future-work"      "11-future-work.md")
    ("conclusion"       "12-conclusion.md")
    ("appendix-a"       "appendix-a-block-calculus-formal-syntax.md")
    ("appendix-b"       "appendix-b-type-grammar.md")
    ("appendix-c"       "appendix-c-kind-grammar.md")
    ("appendix-d"       "appendix-d-manifest-schema.md")
    ("appendix-e"       "appendix-e-comparison-with-unison.md")
    ("references"       "99-references.md")))

(define *section-titles*
  '(("block-machine"    "3. The Block Machine")
    ("block-calculus"   "4. The Block Calculus")
    ("type-theory"      "5. The Type Theory")
    ("module-system"    "6. The Module System")
    ("implementation"   "7. Implementation")
    ("evaluation"       "8. Evaluation")
    ("related-work"     "9. Related Work")
    ("limitations"      "10. Limitations and Non-Goals")
    ("future-work"      "11. Future Work")
    ("conclusion"       "12. Conclusion")
    ("appendix-a"       "Appendix A: Block Calculus Formal Syntax")
    ("appendix-b"       "Appendix B: Type Grammar")
    ("appendix-c"       "Appendix C: Kind Grammar")
    ("appendix-d"       "Appendix D: Manifest Schema")
    ("appendix-e"       "Appendix E: Comparison with Unison")
    ("references"       "References")))

(define (read-file path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([chars '()])
        (let ([c (read-char port)])
          (if (eof-object? c)
              (list->string (reverse chars))
              (loop (cons c chars))))))))

(define (write-file path content)
  (call-with-output-file path
    (lambda (port)
      (display content port))
    '(replace)))

(define (render-body ast)
  (if (and (pair? ast) (eq? (car ast) 'document))
      (let* ([blocks (cdr ast)]
             [body-blocks (if (and (pair? blocks)
                                   (pair? (car blocks))
                                   (eq? (caar blocks) 'h2))
                              (cdr blocks)
                              blocks)])
        (apply string-append (map render-html body-blocks)))
      (render-html ast)))

(define (parse-chapter-body filename)
  (let* ([path (string-append *report-source-dir* "/" filename)]
         [content (read-file path)]
         [result (parse-markdown content)])
    (if (right? result)
        (render-body (from-right result))
        (string-append "<p class=\"error\">Error parsing " filename "</p>\n"))))

(define (get-section-title section-id)
  (let ([entry (assoc section-id *section-titles*)])
    (if entry (cadr entry) section-id)))

(define (build-section entry)
  (let* ([section-id (car entry)]
         [filename (cadr entry)]
         [title (get-section-title section-id)]
         [body-content (parse-chapter-body filename)])
    (printf "  ~a\n" section-id)
    (string-append
     "            <section id=\"" section-id "\">\n"
     "                <h2>" title "<a href=\"#" section-id "\" class=\"section-anchor\">#</a></h2>\n\n"
     body-content
     "            </section>\n\n")))

;;; ============================================================
;;; Demoscene Template
;;; ============================================================

(define *page-header* "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>The Fold: Technical Report</title>
    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
    <link href=\"https://fonts.googleapis.com/css2?family=IBM+Plex+Mono:wght@400;500;600&family=Space+Mono:wght@400;700&display=swap\" rel=\"stylesheet\">
    <style>
        :root {
            --bg-void: #0a0a0a;
            --bg-primary: #0e0e0e;
            --bg-secondary: #141414;
            --bg-tertiary: #1a1a1a;
            --bg-elevated: #1e1e1e;
            --green: #98c379;
            --green-dim: #5a7a48;
            --amber: #e5c07b;
            --amber-dim: #a8894a;
            --red: #e06c75;
            --text-bright: #c8c8c8;
            --text-primary: #909090;
            --text-secondary: #686868;
            --text-muted: #404040;
            --glow-green: rgba(152, 195, 121, 0.12);
            --glow-amber: rgba(229, 192, 123, 0.10);
            --border: #252525;
            --border-subtle: #1a1a1a;
            --sidebar-width: 260px;
            --content-max: 780px;
        }

        @keyframes scanline {
            0% { transform: translateY(-100%); }
            100% { transform: translateY(100vh); }
        }

        * { box-sizing: border-box; margin: 0; padding: 0; }
        html { scroll-behavior: smooth; scroll-padding-top: 2rem; }

        body {
            font-family: 'IBM Plex Mono', monospace;
            background: var(--bg-void);
            color: var(--text-primary);
            line-height: 1.75;
            font-size: 14px;
            -webkit-font-smoothing: antialiased;
        }

        .crt-effect {
            position: fixed;
            top: 0; left: 0;
            width: 100%; height: 100%;
            pointer-events: none;
            z-index: 9999;
        }

        .crt-effect::before {
            content: '';
            position: absolute;
            top: 0; left: 0;
            width: 100%; height: 100%;
            background: repeating-linear-gradient(
                0deg,
                transparent 0px, transparent 1px,
                rgba(0, 0, 0, 0.03) 1px, rgba(0, 0, 0, 0.03) 2px
            );
        }

        .crt-effect::after {
            content: '';
            position: absolute;
            top: 0; left: 0;
            width: 100%; height: 100%;
            background: radial-gradient(
                ellipse at center,
                transparent 60%,
                rgba(0, 0, 0, 0.3) 100%
            );
        }

        .scanline {
            position: fixed;
            top: 0; left: 0;
            width: 100%; height: 4px;
            background: linear-gradient(180deg, transparent, rgba(152, 195, 121, 0.02), transparent);
            animation: scanline 12s linear infinite;
            pointer-events: none;
            z-index: 9998;
        }

        .sidebar {
            position: fixed;
            top: 0; left: 0;
            width: var(--sidebar-width);
            height: 100vh;
            background: var(--bg-primary);
            border-right: 1px solid var(--border);
            overflow-y: auto;
            padding: 2rem 0;
            z-index: 100;
        }

        .sidebar-header {
            padding: 0 1.25rem 1.5rem;
            margin-bottom: 1rem;
        }

        .sidebar-title {
            font-family: 'Space Mono', monospace;
            font-size: 0.95rem;
            font-weight: 700;
            color: var(--green);
            letter-spacing: 0.12em;
            text-transform: uppercase;
            margin-bottom: 0.25rem;
        }

        .sidebar-subtitle {
            font-size: 0.65rem;
            color: var(--text-muted);
            letter-spacing: 0.08em;
            text-transform: uppercase;
        }

        .nav-section { padding: 0.75rem 0; }

        .nav-section-title {
            font-size: 0.6rem;
            font-weight: 600;
            color: var(--text-muted);
            text-transform: uppercase;
            letter-spacing: 0.12em;
            padding: 0.5rem 1.25rem;
            border-left: 2px solid var(--border);
        }

        .nav-link {
            display: block;
            padding: 0.4rem 1.25rem;
            color: var(--text-secondary);
            text-decoration: none;
            font-size: 0.8rem;
            transition: all 0.15s ease;
            border-left: 2px solid transparent;
        }

        .nav-link:hover {
            color: var(--text-bright);
            background: rgba(152, 195, 121, 0.04);
            border-left-color: var(--green-dim);
        }

        .nav-link.active {
            color: var(--green);
            border-left-color: var(--green);
            background: rgba(152, 195, 121, 0.06);
        }

        .nav-link .chapter-num {
            display: inline-block;
            width: 1.5rem;
            color: var(--text-muted);
            font-size: 0.7rem;
            font-variant-numeric: tabular-nums;
        }

        .main { margin-left: var(--sidebar-width); min-height: 100vh; }
        .content { max-width: var(--content-max); margin: 0 auto; padding: 3rem 2.5rem 6rem; }

        h1 {
            font-family: 'Space Mono', monospace;
            font-size: 1.6rem;
            font-weight: 700;
            line-height: 1.3;
            margin-bottom: 0.75rem;
            color: var(--text-bright);
            letter-spacing: -0.01em;
        }

        h1::before { content: '> '; color: var(--green); opacity: 0.7; }

        .doc-meta {
            font-size: 0.7rem;
            color: var(--text-muted);
            margin-bottom: 2.5rem;
            padding-bottom: 2rem;
            border-bottom: 1px solid var(--border);
            letter-spacing: 0.05em;
        }

        h2 {
            font-family: 'Space Mono', monospace;
            font-size: 1.15rem;
            font-weight: 700;
            margin-top: 3.5rem;
            margin-bottom: 1.25rem;
            padding-top: 1.5rem;
            color: var(--amber);
            border-top: 1px solid var(--border);
            letter-spacing: 0.02em;
        }

        h2:first-of-type { margin-top: 0; border-top: none; padding-top: 0; }

        h3 {
            font-family: 'Space Mono', monospace;
            font-size: 0.95rem;
            font-weight: 700;
            margin-top: 2.5rem;
            margin-bottom: 0.75rem;
            color: var(--green);
            letter-spacing: 0.01em;
        }

        h4 {
            font-size: 0.85rem;
            font-weight: 600;
            margin-top: 1.75rem;
            margin-bottom: 0.5rem;
            color: var(--amber-dim);
        }

        p { margin-bottom: 1rem; }
        strong { color: var(--text-bright); font-weight: 600; }
        em { color: var(--green); font-style: normal; }

        a {
            color: var(--amber);
            text-decoration: none;
            border-bottom: 1px solid transparent;
            transition: border-color 0.15s ease;
        }

        a:hover { border-bottom-color: var(--amber); }

        ul, ol { margin-bottom: 1rem; padding-left: 0; list-style: none; }
        li { margin-bottom: 0.5rem; padding-left: 1.25rem; position: relative; }
        ul li::before { content: '·'; position: absolute; left: 0; color: var(--green); font-weight: bold; }

        ol { counter-reset: ol-counter; }
        ol li { counter-increment: ol-counter; }
        ol li::before {
            content: counter(ol-counter) '.';
            position: absolute; left: 0;
            color: var(--text-muted);
            font-size: 0.75rem;
            font-weight: 600;
        }

        code {
            font-family: 'IBM Plex Mono', monospace;
            font-size: 0.85em;
            background: var(--bg-elevated);
            padding: 0.15em 0.35em;
            border-radius: 3px;
            color: var(--green);
            border: 1px solid var(--border);
        }

        pre {
            background: var(--bg-secondary);
            border: 1px solid var(--border);
            border-left: 3px solid var(--green-dim);
            border-radius: 2px;
            padding: 1rem 1.25rem;
            overflow-x: auto;
            margin-bottom: 1.5rem;
            font-size: 0.8rem;
            line-height: 1.6;
        }

        pre code { background: none; padding: 0; border: none; color: var(--text-primary); }

        blockquote {
            border-left: 2px solid var(--amber-dim);
            margin: 1.5rem 0;
            padding: 0.5rem 0 0.5rem 1.25rem;
            background: rgba(229, 192, 123, 0.03);
        }

        blockquote p:last-child { margin-bottom: 0; }

        table {
            width: 100%;
            border-collapse: collapse;
            margin: 1.5rem 0;
            font-size: 0.8rem;
        }

        th, td {
            text-align: left;
            padding: 0.6rem 0.75rem;
            border-bottom: 1px solid var(--border);
        }

        th {
            font-weight: 600;
            font-size: 0.7rem;
            text-transform: uppercase;
            letter-spacing: 0.05em;
            color: var(--amber);
            border-bottom: 2px solid var(--border);
        }

        td { color: var(--text-secondary); }
        tr:hover td { background: rgba(152, 195, 121, 0.03); }

        hr {
            border: none;
            height: 1px;
            margin: 2.5rem 0;
            background: linear-gradient(90deg, transparent, var(--border) 20%, var(--green-dim) 50%, var(--border) 80%, transparent);
        }

        .abstract {
            background: var(--bg-secondary);
            border: 1px solid var(--border);
            border-left: 3px solid var(--green);
            padding: 1.5rem 1.75rem;
            margin-bottom: 2rem;
        }

        .abstract-title {
            font-size: 0.65rem;
            font-weight: 600;
            color: var(--green);
            text-transform: uppercase;
            letter-spacing: 0.1em;
            margin-bottom: 1rem;
        }

        .abstract p { font-size: 0.9rem; color: var(--text-secondary); }

        .section-anchor {
            color: var(--text-muted);
            margin-left: 0.5rem;
            opacity: 0;
            transition: opacity 0.15s ease;
            text-decoration: none;
            font-weight: normal;
        }

        h2:hover .section-anchor, h3:hover .section-anchor { opacity: 0.5; }
        .section-anchor:hover { opacity: 1; color: var(--green); }

        .progress {
            position: fixed;
            top: 0;
            left: var(--sidebar-width);
            right: 0;
            height: 2px;
            background: var(--bg-tertiary);
            z-index: 100;
        }

        .progress-bar {
            height: 100%;
            background: var(--green);
            width: 0%;
            transition: width 0.1s ease;
        }

        .menu-toggle {
            display: none;
            position: fixed;
            top: 1rem; left: 1rem;
            z-index: 200;
            background: var(--bg-secondary);
            border: 1px solid var(--border);
            border-radius: 4px;
            padding: 0.6rem;
            cursor: pointer;
            color: var(--green);
        }

        .menu-toggle svg { display: block; width: 20px; height: 20px; }
        .overlay { display: none; position: fixed; inset: 0; background: rgba(5, 8, 12, 0.9); z-index: 50; }
        .overlay.open { display: block; }

        @media (max-width: 900px) {
            .sidebar { transform: translateX(-100%); transition: transform 0.25s ease; }
            .sidebar.open { transform: translateX(0); }
            .main { margin-left: 0; }
            .menu-toggle { display: block; }
            .content { padding: 4rem 1.5rem 4rem; }
            h1 { font-size: 1.3rem; }
            h2 { font-size: 1rem; }
            .progress { left: 0; }
        }

        .generated {
            margin-top: 3rem;
            padding-top: 1.25rem;
            border-top: 1px solid var(--border);
            font-size: 0.65rem;
            color: var(--text-muted);
            text-align: center;
            letter-spacing: 0.05em;
        }

        ::-webkit-scrollbar { width: 6px; height: 6px; }
        ::-webkit-scrollbar-track { background: var(--bg-void); }
        ::-webkit-scrollbar-thumb { background: var(--border); border-radius: 3px; }
        ::-webkit-scrollbar-thumb:hover { background: var(--text-muted); }
        ::selection { background: var(--green); color: var(--bg-void); }
    </style>
</head>
<body>
    <div class=\"crt-effect\"></div>
    <div class=\"scanline\"></div>

    <button class=\"menu-toggle\" onclick=\"toggleMenu()\" aria-label=\"Toggle navigation\">
        <svg fill=\"none\" stroke=\"currentColor\" viewBox=\"0 0 24 24\">
            <path stroke-linecap=\"round\" stroke-linejoin=\"round\" stroke-width=\"2\" d=\"M4 6h16M4 12h16M4 18h16\"/>
        </svg>
    </button>

    <div class=\"overlay\" onclick=\"toggleMenu()\"></div>

    <nav class=\"sidebar\">
        <div class=\"sidebar-header\">
            <div class=\"sidebar-title\">The Fold</div>
            <div class=\"sidebar-subtitle\">Technical Report</div>
        </div>

        <div class=\"nav-section\">
            <a href=\"#abstract\" class=\"nav-link active\">Abstract</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Foundations</div>
            <a href=\"#introduction\" class=\"nav-link\"><span class=\"chapter-num\">1</span>Introduction</a>
            <a href=\"#architecture\" class=\"nav-link\"><span class=\"chapter-num\">2</span>System Architecture</a>
            <a href=\"#block-machine\" class=\"nav-link\"><span class=\"chapter-num\">3</span>The Block Machine</a>
            <a href=\"#block-calculus\" class=\"nav-link\"><span class=\"chapter-num\">4</span>The Block Calculus</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Type System</div>
            <a href=\"#type-theory\" class=\"nav-link\"><span class=\"chapter-num\">5</span>The Type Theory</a>
            <a href=\"#module-system\" class=\"nav-link\"><span class=\"chapter-num\">6</span>The Module System</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Practice</div>
            <a href=\"#implementation\" class=\"nav-link\"><span class=\"chapter-num\">7</span>Implementation</a>
            <a href=\"#evaluation\" class=\"nav-link\"><span class=\"chapter-num\">8</span>Evaluation</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Context</div>
            <a href=\"#related-work\" class=\"nav-link\"><span class=\"chapter-num\">9</span>Related Work</a>
            <a href=\"#limitations\" class=\"nav-link\"><span class=\"chapter-num\">10</span>Limitations</a>
            <a href=\"#future-work\" class=\"nav-link\"><span class=\"chapter-num\">11</span>Future Work</a>
            <a href=\"#conclusion\" class=\"nav-link\"><span class=\"chapter-num\">12</span>Conclusion</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Appendices</div>
            <a href=\"#appendix-a\" class=\"nav-link\"><span class=\"chapter-num\">A</span>Block Calculus Syntax</a>
            <a href=\"#appendix-b\" class=\"nav-link\"><span class=\"chapter-num\">B</span>Type Grammar</a>
            <a href=\"#appendix-c\" class=\"nav-link\"><span class=\"chapter-num\">C</span>Kind Grammar</a>
            <a href=\"#appendix-d\" class=\"nav-link\"><span class=\"chapter-num\">D</span>Manifest Schema</a>
            <a href=\"#appendix-e\" class=\"nav-link\"><span class=\"chapter-num\">E</span>Comparison with Unison</a>
            <a href=\"#references\" class=\"nav-link\">References</a>
        </div>
    </nav>

    <div class=\"progress\">
        <div class=\"progress-bar\"></div>
    </div>

    <main class=\"main\">
        <div class=\"content\">
            <h1>The Fold: A Content-Addressable Homoiconic Universe</h1>
            <div class=\"doc-meta\">Technical Report &middot; January 2026</div>

")

(define *abstract-section* "            <section id=\"abstract\">
                <div class=\"abstract\">
                    <div class=\"abstract-title\">Abstract</div>
                    <p>We present <strong>The Fold</strong>, a programming system built on a content-addressable homoiconic foundation. At its core lies a <em>block machine</em> where every computational unit&mdash;code, data, and types&mdash;is represented as a cryptographically-addressed immutable structure. Through a two-phase normalization process&mdash;&alpha;-normalization via de Bruijn indices and algebraic canonicalization (commutative sorting, associative flattening)&mdash;semantically equivalent expressions produce identical hashes, achieving true <em>semantic identity</em>.</p>

                    <p>The Fold implements a <em>gradual dependent type system</em> combining bidirectional type checking, dependent function and pair types (&Pi;, &Sigma;), higher-kinded types, type classes via dictionary-passing, and GADTs with pattern refinement.</p>

                    <p>The system organizes verified code into a <em>module DAG</em>&mdash;a tiered directed acyclic graph where modules declare dependencies, purity guarantees, and complexity bounds. A BM25-powered semantic search engine enables discovery across thousands of exports.</p>

                    <p><strong>Key contributions:</strong> (1) a block calculus formalizing content-addressed computation with &alpha;-equivalence, (2) a dependent type system integrated with gradual typing, (3) a compositional module system with fuel-bounded complexity guarantees.</p>
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

                <p>In file-based systems, these are distinct entities requiring coordination. Yet semantically, they are the same function.</p>

                <h3>1.2 The Proposal: Content-Addressed Homoiconic Computation</h3>
                <p>The Fold addresses these problems through three interlocking mechanisms:</p>

                <ol>
                    <li><strong>Content Addressing</strong>: Every value's identity is its cryptographic hash.</li>
                    <li><strong>&alpha;-Normalization</strong>: Before hashing, expressions are normalized using de Bruijn indices.</li>
                    <li><strong>Homoiconicity</strong>: Code is data. Programs are S-expressions that serialize to blocks.</li>
                </ol>

                <p>The result is a system where <em>semantic identity replaces syntactic identity</em>.</p>

                <h3>1.3 Contributions</h3>

                <h4>Contribution 1: Block Calculus with Multi-Phase Normalization</h4>
                <p>We formalize a calculus where computation operates over content-addressed blocks. The key innovation is integrating a two-phase normalization pipeline with cryptographic hashing.</p>

                <h4>Contribution 2: Gradual Dependent Type System</h4>
                <p>We implement a type system combining bidirectional type checking for predictable inference, dependent types (&Pi;, &Sigma;) for precise specifications, and higher-kinded types and type classes for abstraction.</p>

                <h4>Contribution 3: Compositional Module System</h4>
                <p>We organize code into a tiered DAG where each module declares dependencies, purity, and complexity bounds.</p>
            </section>

")

(define *arch-section* "            <section id=\"architecture\">
                <h2>2. System Architecture<a href=\"#architecture\" class=\"section-anchor\">#</a></h2>

                <p>The Fold employs a <em>three-layer architecture</em> separating pure computation from effectful boundaries:</p>

<pre><code>+-------------------------------------------------------------+
|                         User Layer                          |
|              Applications, experiments, scripts             |
+-------------------------------------------------------------+
|                      Boundary Layer                         |
|         IO, validation, capability minting, effects         |
+-------------------------------------------------------------+
|                        Core Layer                           |
|          Pure, total, content-addressed, verified           |
+-------------------------------------------------------------+</code></pre>

                <h3>2.1 The Core Layer</h3>
                <p>The Core is the mathematical heart of The Fold. Code in Core satisfies three properties:</p>

                <p><strong>Purity</strong>: No side effects. Functions depend only on their arguments and produce only their return values.</p>

                <p><strong>Bounded Computation</strong>: Every computation terminates within a declared resource bound via <em>fuel-bounded execution</em>.</p>

                <blockquote>
                    <p><strong>Important distinction</strong>: This is <em>not</em> totality in the type-theoretic sense. True totality proves termination for all inputs. Fuel bounds guarantee that any particular execution completes.</p>
                </blockquote>

                <p><strong>Trust</strong>: Core assumes <em>perfect input</em>. It performs no validation, no defensive checks, no error recovery.</p>

                <h3>2.2 The Boundary Layer</h3>
                <p>The Boundary is \"fallen\"&mdash;it interacts with the impure outside world: File I/O, network, user input, validation and error handling, capability minting from external resources.</p>

                <h3>2.3 The User Layer</h3>
                <p>Applications and experiments that compose Boundary and Core functionality. This layer has maximum freedom and minimum guarantees.</p>
            </section>

")

(define *page-footer* "
            <div class=\"generated\">Generated by The Fold's markdown parser &middot; lattice/dsl/markdown</div>
        </div>
    </main>

    <script>
        function toggleMenu() {
            document.querySelector('.sidebar').classList.toggle('open');
            document.querySelector('.overlay').classList.toggle('open');
        }

        const sections = document.querySelectorAll('section[id]');
        const navLinks = document.querySelectorAll('.nav-link');

        function updateActiveLink() {
            const scrollY = window.scrollY;
            sections.forEach(section => {
                const sectionTop = section.offsetTop - 100;
                const sectionHeight = section.offsetHeight;
                const sectionId = section.getAttribute('id');
                if (scrollY >= sectionTop && scrollY < sectionTop + sectionHeight) {
                    navLinks.forEach(link => {
                        link.classList.remove('active');
                        if (link.getAttribute('href') === '#' + sectionId) {
                            link.classList.add('active');
                        }
                    });
                }
            });
        }

        function updateProgress() {
            const scrollTop = window.scrollY;
            const docHeight = document.documentElement.scrollHeight - window.innerHeight;
            const progress = (scrollTop / docHeight) * 100;
            document.querySelector('.progress-bar').style.width = progress + '%';
        }

        window.addEventListener('scroll', () => { updateActiveLink(); updateProgress(); });

        navLinks.forEach(link => {
            link.addEventListener('click', () => {
                document.querySelector('.sidebar').classList.remove('open');
                document.querySelector('.overlay').classList.remove('open');
            });
        });
    </script>
</body>
</html>
")

(define (build-report)
  (printf "Building demoscene-styled report...\n")
  (let* ([generated-sections (map build-section *chapter-map*)]
         [all-sections (string-append
                        *abstract-section*
                        *intro-section*
                        *arch-section*
                        (apply string-append generated-sections))]
         [page (string-append *page-header* all-sections *page-footer*)])
    (printf "  Writing output...\n")
    (write-file *report-output-file* page)
    (printf "  Done! ~a bytes\n" (string-length page))))

(build-report)
