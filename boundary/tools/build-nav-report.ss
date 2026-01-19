;;; boundary/tools/build-nav-report.ss — Navigable Technical Report Generator
;;;
;;; Builds the chapter-navigable technical report by parsing markdown
;;; chapters and injecting content into the template.
;;;
;;; Dogfoods the lattice/dsl/markdown parser.
;;;
;;; Usage:
;;;   scheme --script boundary/tools/build-nav-report.ss

(load "core/base/prelude.ss")
(load "lattice/dsl/markdown/block-parser.ss")
(load "lattice/dsl/markdown/html.ss")

;;; ============================================================
;;; Configuration
;;; ============================================================

(define *report-source-dir* "docs/technical-report")
(define *report-output-file* "/home/oso/fold/docs/technical-report-nav.html")

;;; Chapter manifest mapping section IDs to filenames
;;; The section IDs match the nav links in report.html
(define *chapter-map*
  '(("block-machine"    "03-the-block-machine.md")
    ("block-calculus"   "04-the-block-calculus.md")
    ("type-theory"      "05-the-type-theory.md")
    ("module-system"    "06-the-module-system.md")
    ("implementation"   "07-implementation.md")
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

;;; ============================================================
;;; File I/O
;;; ============================================================

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

;;; ============================================================
;;; Markdown Processing
;;; ============================================================

;;; render-body : AST -> String
;;; Render markdown AST to HTML, skipping the first H2 (already in template).
(define (render-body ast)
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

;;; parse-chapter-body : String -> String
;;; Parse markdown file and return just the body HTML (no H2).
(define (parse-chapter-body filename)
  (let* ([path (string-append *report-source-dir* "/" filename)]
         [content (read-file path)]
         [result (parse-markdown content)])
    (if (right? result)
        (render-body (from-right result))
        (string-append "<p class=\"error\">Error parsing " filename ": "
                       (format-error (from-left result))
                       "</p>\n"))))

;;; ============================================================
;;; Template Generation
;;; ============================================================

;;; generate-section : String × String -> String
;;; Generate a complete section with content.
(define (generate-section section-id content)
  (string-append
   "            <section id=\"" section-id "\">\n"
   content
   "            </section>\n\n"))

;;; ============================================================
;;; HTML Template
;;; ============================================================

(define *page-header* "<!DOCTYPE html>
<html lang=\"en\">
<head>
    <meta charset=\"UTF-8\">
    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">
    <title>The Fold: Technical Report</title>
    <style>
        :root {
            --bg-primary: #0d1117;
            --bg-secondary: #161b22;
            --bg-tertiary: #21262d;
            --text-primary: #e6edf3;
            --text-secondary: #8b949e;
            --text-muted: #6e7681;
            --accent: #7c3aed;
            --accent-dim: #5b21b6;
            --border: #30363d;
            --code-bg: #1c2128;
            --link: #a78bfa;
            --link-hover: #c4b5fd;
            --sidebar-width: 280px;
            --content-max: 800px;
        }

        * { box-sizing: border-box; margin: 0; padding: 0; }
        html { scroll-behavior: smooth; scroll-padding-top: 2rem; }

        body {
            font-family: 'Inter', -apple-system, BlinkMacSystemFont, 'Segoe UI', system-ui, sans-serif;
            background: var(--bg-primary);
            color: var(--text-primary);
            line-height: 1.7;
            font-size: 16px;
        }

        /* Sidebar */
        .sidebar {
            position: fixed;
            top: 0; left: 0;
            width: var(--sidebar-width);
            height: 100vh;
            background: var(--bg-secondary);
            border-right: 1px solid var(--border);
            overflow-y: auto;
            padding: 1.5rem 0;
            z-index: 100;
        }

        .sidebar-header {
            padding: 0 1.5rem 1.5rem;
            border-bottom: 1px solid var(--border);
            margin-bottom: 1rem;
        }

        .sidebar-title { font-size: 1.1rem; font-weight: 600; color: var(--text-primary); margin-bottom: 0.25rem; }
        .sidebar-subtitle { font-size: 0.75rem; color: var(--text-muted); text-transform: uppercase; letter-spacing: 0.05em; }

        .nav-section { padding: 0.5rem 0; }
        .nav-section-title {
            font-size: 0.7rem; font-weight: 600; color: var(--text-muted);
            text-transform: uppercase; letter-spacing: 0.08em;
            padding: 0.75rem 1.5rem 0.5rem;
        }

        .nav-link {
            display: block; padding: 0.5rem 1.5rem;
            color: var(--text-secondary); text-decoration: none;
            font-size: 0.875rem; transition: all 0.15s ease;
            border-left: 2px solid transparent;
        }

        .nav-link:hover { color: var(--text-primary); background: var(--bg-tertiary); }
        .nav-link.active { color: var(--accent); border-left-color: var(--accent); background: rgba(124, 58, 237, 0.1); }
        .nav-link .chapter-num { color: var(--text-muted); font-size: 0.75rem; margin-right: 0.5rem; font-variant-numeric: tabular-nums; }

        /* Main content */
        .main { margin-left: var(--sidebar-width); min-height: 100vh; }
        .content { max-width: var(--content-max); margin: 0 auto; padding: 3rem 2rem 6rem; }

        /* Typography */
        h1 {
            font-size: 2.5rem; font-weight: 700; line-height: 1.2; margin-bottom: 0.5rem;
            background: linear-gradient(135deg, #a78bfa 0%, #7c3aed 50%, #5b21b6 100%);
            -webkit-background-clip: text; -webkit-text-fill-color: transparent; background-clip: text;
        }

        .doc-meta { color: var(--text-muted); font-size: 0.875rem; margin-bottom: 2rem; padding-bottom: 2rem; border-bottom: 1px solid var(--border); }

        h2 { font-size: 1.75rem; font-weight: 600; margin-top: 3rem; margin-bottom: 1rem; padding-top: 1rem; color: var(--text-primary); }
        h2:first-of-type { margin-top: 0; }
        h3 { font-size: 1.25rem; font-weight: 600; margin-top: 2rem; margin-bottom: 0.75rem; color: var(--text-primary); }
        h4 { font-size: 1rem; font-weight: 600; margin-top: 1.5rem; margin-bottom: 0.5rem; color: var(--text-secondary); }

        p { margin-bottom: 1rem; color: var(--text-secondary); }
        strong { color: var(--text-primary); font-weight: 600; }
        em { font-style: italic; color: var(--text-primary); }
        a { color: var(--link); text-decoration: none; }
        a:hover { color: var(--link-hover); text-decoration: underline; }

        ul, ol { margin-bottom: 1rem; padding-left: 1.5rem; color: var(--text-secondary); }
        li { margin-bottom: 0.5rem; }
        li::marker { color: var(--text-muted); }

        code {
            font-family: 'JetBrains Mono', 'Fira Code', 'Consolas', monospace;
            font-size: 0.875em; background: var(--code-bg);
            padding: 0.2em 0.4em; border-radius: 4px; color: #f0abfc;
        }

        pre {
            background: var(--code-bg); border: 1px solid var(--border);
            border-radius: 8px; padding: 1rem 1.25rem; overflow-x: auto;
            margin-bottom: 1.5rem; font-size: 0.875rem; line-height: 1.6;
        }

        pre code { background: none; padding: 0; color: var(--text-primary); }

        blockquote {
            border-left: 3px solid var(--accent); margin: 1.5rem 0;
            padding: 0.5rem 0 0.5rem 1.25rem; color: var(--text-secondary);
            background: rgba(124, 58, 237, 0.05); border-radius: 0 8px 8px 0;
        }
        blockquote p:last-child { margin-bottom: 0; }

        table { width: 100%; border-collapse: collapse; margin: 1.5rem 0; font-size: 0.875rem; }
        th, td { text-align: left; padding: 0.75rem 1rem; border: 1px solid var(--border); }
        th { background: var(--bg-tertiary); font-weight: 600; color: var(--text-primary); }
        td { color: var(--text-secondary); }
        tr:nth-child(even) td { background: var(--bg-secondary); }

        hr { border: none; border-top: 1px solid var(--border); margin: 2rem 0; }

        .section-anchor { color: var(--text-muted); margin-left: 0.5rem; opacity: 0; transition: opacity 0.15s ease; text-decoration: none; }
        h2:hover .section-anchor, h3:hover .section-anchor { opacity: 1; }
        .section-anchor:hover { color: var(--accent); }

        .abstract { background: var(--bg-secondary); border: 1px solid var(--border); border-radius: 12px; padding: 1.5rem 2rem; margin-bottom: 2rem; }
        .abstract-title { font-size: 0.75rem; font-weight: 600; color: var(--accent); text-transform: uppercase; letter-spacing: 0.1em; margin-bottom: 1rem; }
        .abstract p { font-size: 0.95rem; }

        .menu-toggle { display: none; position: fixed; top: 1rem; left: 1rem; z-index: 200; background: var(--bg-secondary); border: 1px solid var(--border); border-radius: 8px; padding: 0.75rem; cursor: pointer; color: var(--text-primary); }
        .menu-toggle svg { display: block; width: 24px; height: 24px; }

        @media (max-width: 900px) {
            .sidebar { transform: translateX(-100%); transition: transform 0.3s ease; }
            .sidebar.open { transform: translateX(0); }
            .main { margin-left: 0; }
            .menu-toggle { display: block; }
            .content { padding: 4rem 1.5rem 4rem; }
            h1 { font-size: 2rem; }
            h2 { font-size: 1.5rem; }
        }

        .overlay { display: none; position: fixed; inset: 0; background: rgba(0, 0, 0, 0.5); z-index: 50; }
        .overlay.open { display: block; }

        .progress { position: fixed; top: 0; left: var(--sidebar-width); right: 0; height: 3px; background: var(--bg-tertiary); z-index: 100; }
        .progress-bar { height: 100%; background: linear-gradient(90deg, var(--accent), #a78bfa); width: 0%; transition: width 0.1s ease; }
        @media (max-width: 900px) { .progress { left: 0; } }

        .generated { margin-top: 3rem; padding-top: 1rem; border-top: 1px solid var(--border); font-size: 0.75rem; color: var(--text-muted); text-align: center; }
    </style>
    <link rel=\"preconnect\" href=\"https://fonts.googleapis.com\">
    <link rel=\"preconnect\" href=\"https://fonts.gstatic.com\" crossorigin>
    <link href=\"https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap\" rel=\"stylesheet\">
</head>
<body>
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
            <a href=\"#meta-tooling\" class=\"nav-link\"><span class=\"chapter-num\">8</span>Meta-Tooling</a>
            <a href=\"#agent-substrate\" class=\"nav-link\"><span class=\"chapter-num\">9</span>Agent Substrate</a>
            <a href=\"#evaluation\" class=\"nav-link\"><span class=\"chapter-num\">10</span>Evaluation</a>
        </div>

        <div class=\"nav-section\">
            <div class=\"nav-section-title\">Context</div>
            <a href=\"#related-work\" class=\"nav-link\"><span class=\"chapter-num\">11</span>Related Work</a>
            <a href=\"#limitations\" class=\"nav-link\"><span class=\"chapter-num\">12</span>Limitations</a>
            <a href=\"#future-work\" class=\"nav-link\"><span class=\"chapter-num\">13</span>Future Work</a>
            <a href=\"#conclusion\" class=\"nav-link\"><span class=\"chapter-num\">14</span>Conclusion</a>
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
            <div class=\"generated\">Generated by The Fold's markdown parser (dogfooding lattice/dsl/markdown)</div>
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

;;; ============================================================
;;; Section Title Map
;;; ============================================================

(define *section-titles*
  '(("block-machine"    "3. The Block Machine")
    ("block-calculus"   "4. The Block Calculus")
    ("type-theory"      "5. The Type Theory")
    ("module-system"    "6. The Module System")
    ("implementation"   "7. Implementation")
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

;;; ============================================================
;;; Build Process
;;; ============================================================

;;; build-section : (String × String) -> String
;;; Build a complete section from section-id and filename.
(define (build-section entry)
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
