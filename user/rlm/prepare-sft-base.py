#!/usr/bin/env python3
"""Prepare SFT training data for Qwen3.5-4B-Base Scheme fluency.

Three data sources:
  1. Raw Scheme source files (lattice/ + core/) — language distribution
  2. Synthgen instruction→code pairs (sft-all-phases.jsonl)
  3. Tier0/Tier1 completion tasks — exact RL env format

Output: train.jsonl + eval.jsonl with {"text": "...", "source": "..."} format.

Usage:
  python3 prepare-sft-base.py [--source-dir ~/fold] [--synthgen PATH] [--output-dir PATH]
"""

import argparse
import json
import os
import random
import sys
from pathlib import Path


# ---------------------------------------------------------------------------
# Tier 0 task data — comment → expression completion
# Same data as fold_base_tier0 env (copied here to avoid Python import path gymnastics)
# ---------------------------------------------------------------------------

TIER0_HEADER = ";; Scheme expressions"

TIER0_TASKS = [
    # list-access
    {"task_text": "Get the first element from (apple banana cherry)", "answer": "(first '(apple banana cherry))", "category": "list-access"},
    {"task_text": "Get the second element of (x y z)", "answer": "(second '(x y z))", "category": "list-access"},
    {"task_text": "Get the third element of (10 20 30 40)", "answer": "(third '(10 20 30 40))", "category": "list-access"},
    {"task_text": "Get the fourth element of (a b c d e)", "answer": "(fourth '(a b c d e))", "category": "list-access"},
    {"task_text": "Get the fifth element of (1 2 3 4 5)", "answer": "(fifth '(1 2 3 4 5))", "category": "list-access"},
    {"task_text": "Get the last element of (red green blue)", "answer": "(last '(red green blue))", "category": "list-access"},
    {"task_text": "Get elements after the first from (1 2 3 4)", "answer": "(rest '(1 2 3 4))", "category": "list-access"},
    {"task_text": "Get the length of the list (a b c d e f)", "answer": "(length '(a b c d e f))", "category": "list-access"},
    {"task_text": "Reverse the list (1 2 3 4 5)", "answer": "(reverse '(1 2 3 4 5))", "category": "list-access"},
    {"task_text": "Append (1 2) and (3 4)", "answer": "(append '(1 2) '(3 4))", "category": "list-access"},
    {"task_text": "Take the first 3 elements of (a b c d e)", "answer": "(take '(a b c d e) 3)", "category": "list-access"},
    {"task_text": "Drop the first 2 elements of (10 20 30 40 50)", "answer": "(drop '(10 20 30 40 50) 2)", "category": "list-access"},
    # string-ops
    {"task_text": "Get the length of the string \"hello world\"", "answer": "(string-length \"hello world\")", "category": "string-ops"},
    {"task_text": "Convert the string \"hello\" to uppercase", "answer": "(string-upcase \"hello\")", "category": "string-ops"},
    {"task_text": "Convert \"WORLD\" to lowercase", "answer": "(string-downcase \"WORLD\")", "category": "string-ops"},
    {"task_text": "Concatenate \"hello\" and \" world\"", "answer": "(string-append \"hello\" \" world\")", "category": "string-ops"},
    {"task_text": "Get the character at index 0 of \"scheme\"", "answer": "(string-ref \"scheme\" 0)", "category": "string-ops"},
    {"task_text": "Check if \"hello\" contains \"ell\"", "answer": "(string-contains \"hello\" \"ell\")", "category": "string-ops"},
    {"task_text": "Get substring of \"abcdef\" from index 2 to 4", "answer": "(substring \"abcdef\" 2 4)", "category": "string-ops"},
    {"task_text": "Convert the number 42 to a string", "answer": "(number->string 42)", "category": "string-ops"},
    {"task_text": "Convert the string \"42\" to a number", "answer": "(string->number \"42\")", "category": "string-ops"},
    {"task_text": "Convert the symbol 'hello to a string", "answer": "(symbol->string 'hello)", "category": "string-ops"},
    {"task_text": "Convert the string \"hello\" to a symbol", "answer": "(string->symbol \"hello\")", "category": "string-ops"},
    {"task_text": "Check if \"hello\" starts with \"hel\"", "answer": "(string-prefix? \"hel\" \"hello\")", "category": "string-ops"},
    {"task_text": "Check if \"world\" ends with \"rld\"", "answer": "(string-suffix? \"rld\" \"world\")", "category": "string-ops"},
    {"task_text": "Split the string \"a,b,c\" by comma", "answer": "(string-split \"a,b,c\" \",\")", "category": "string-ops"},
    {"task_text": "Join the list (\"a\" \"b\" \"c\") with \"-\"", "answer": "(string-join '(\"a\" \"b\" \"c\") \"-\")", "category": "string-ops"},
    # conditionals
    {"task_text": "Return \"yes\" if 5 > 3, otherwise \"no\"", "answer": "(if (> 5 3) \"yes\" \"no\")", "category": "conditionals"},
    {"task_text": "Return \"even\" if 4 is even, \"odd\" otherwise", "answer": "(if (even? 4) \"even\" \"odd\")", "category": "conditionals"},
    {"task_text": "Use cond to classify 15 as \"small\" (<10), \"medium\" (<20), or \"large\"", "answer": "(cond [(< 15 10) \"small\"] [(< 15 20) \"medium\"] [else \"large\"])", "category": "conditionals"},
    {"task_text": "Check if the value 'red is one of (red green blue) using memq", "answer": "(memq 'red '(red green blue))", "category": "conditionals"},
    {"task_text": "Return #t if both 3 > 2 and 5 > 4", "answer": "(and (> 3 2) (> 5 4))", "category": "conditionals"},
    {"task_text": "Return #t if either 1 > 2 or 3 > 2", "answer": "(or (> 1 2) (> 3 2))", "category": "conditionals"},
    {"task_text": "Negate the boolean #t", "answer": "(not #t)", "category": "conditionals"},
    # arithmetic
    {"task_text": "Add 17 and 25", "answer": "(+ 17 25)", "category": "arithmetic"},
    {"task_text": "Compute the remainder of 17 divided by 5", "answer": "(remainder 17 5)", "category": "arithmetic"},
    {"task_text": "Compute the absolute value of -42", "answer": "(abs -42)", "category": "arithmetic"},
    {"task_text": "Find the maximum of 3 7 2 9 1", "answer": "(max 3 7 2 9 1)", "category": "arithmetic"},
    # filter-map
    {"task_text": "Filter even numbers from (1 2 3 4 5 6)", "answer": "(filter even? '(1 2 3 4 5 6))", "category": "filter-map"},
    {"task_text": "Double every element in (1 2 3 4)", "answer": "(map (lambda (x) (* x 2)) '(1 2 3 4))", "category": "filter-map"},
    {"task_text": "Square every number in (1 2 3 4 5)", "answer": "(map (lambda (x) (* x x)) '(1 2 3 4 5))", "category": "filter-map"},
    {"task_text": "Remove strings shorter than 4 characters from (\"hi\" \"hello\" \"hey\" \"world\")", "answer": "(filter (lambda (s) (>= (string-length s) 4)) '(\"hi\" \"hello\" \"hey\" \"world\"))", "category": "filter-map"},
    # sort-aggregate
    {"task_text": "Sort the list (3 1 4 1 5 9 2 6) in ascending order", "answer": "(sort < '(3 1 4 1 5 9 2 6))", "category": "sort-aggregate"},
    {"task_text": "Sum all elements in (1 2 3 4 5) using fold-left", "answer": "(fold-left + 0 '(1 2 3 4 5))", "category": "sort-aggregate"},
    {"task_text": "Find the product of all elements in (1 2 3 4 5) using fold-left", "answer": "(fold-left * 1 '(1 2 3 4 5))", "category": "sort-aggregate"},
    {"task_text": "Count the elements in (a b c d e) using fold-left", "answer": "(fold-left (lambda (acc _) (+ acc 1)) 0 '(a b c d e))", "category": "sort-aggregate"},
    # let-bindings
    {"task_text": "Bind x to 10 and y to 20, then compute (+ x y)", "answer": "(let ([x 10] [y 20]) (+ x y))", "category": "let-bindings"},
    {"task_text": "Use let* to bind x to 5 and y to (* x 2), return y", "answer": "(let* ([x 5] [y (* x 2)]) y)", "category": "let-bindings"},
    {"task_text": "Use letrec to define even?/odd? and test (even? 4)", "answer": "(letrec ([even? (lambda (n) (if (= n 0) #t (odd? (- n 1))))] [odd? (lambda (n) (if (= n 0) #f (even? (- n 1))))]) (even? 4))", "category": "let-bindings"},
    # cons-pairs
    {"task_text": "Create a pair of 1 and 2", "answer": "(cons 1 2)", "category": "cons-pairs"},
    {"task_text": "Get the car of the pair (hello . world)", "answer": "(car '(hello . world))", "category": "cons-pairs"},
    {"task_text": "Get the cdr of the pair (hello . world)", "answer": "(cdr '(hello . world))", "category": "cons-pairs"},
    # higher-order
    {"task_text": "Apply the function + to arguments 3 and 4", "answer": "(apply + '(3 4))", "category": "higher-order"},
    {"task_text": "Use map with string-length on (\"hi\" \"hello\" \"hey\")", "answer": "(map string-length '(\"hi\" \"hello\" \"hey\"))", "category": "higher-order"},
    {"task_text": "Use for-each to process each element (printing not needed, just show the call)", "answer": "(for-each (lambda (x) x) '(1 2 3))", "category": "higher-order"},
    # composition
    {"task_text": "Get the first element of (rest '(10 20 30 40))", "answer": "(first (rest '(10 20 30 40)))", "category": "composition"},
    {"task_text": "Get the length of the result of filtering even? from (1 2 3 4 5 6)", "answer": "(length (filter even? '(1 2 3 4 5 6)))", "category": "composition"},
    # recursion
    {"task_text": "Define a recursive factorial function and compute (fact 6)", "answer": "(begin (define (fact n) (if (<= n 1) 1 (* n (fact (- n 1))))) (fact 6))", "category": "recursion"},
    {"task_text": "Define a recursive fibonacci function and compute (fib 7)", "answer": "(begin (define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (fib 7))", "category": "recursion"},
    {"task_text": "Define a recursive sum-list function and sum (1 2 3 4 5)", "answer": "(begin (define (sum-list lst) (if (null? lst) 0 (+ (car lst) (sum-list (cdr lst))))) (sum-list '(1 2 3 4 5)))", "category": "recursion"},
    {"task_text": "Define a recursive length function and compute the length of (a b c d)", "answer": "(begin (define (my-length lst) (if (null? lst) 0 (+ 1 (my-length (cdr lst))))) (my-length '(a b c d)))", "category": "recursion"},
    {"task_text": "Define a recursive list-contains? and check if 3 is in (1 2 3 4 5)", "answer": "(begin (define (list-contains? lst x) (cond [(null? lst) #f] [(equal? (car lst) x) #t] [else (list-contains? (cdr lst) x)])) (list-contains? '(1 2 3 4 5) 3))", "category": "recursion"},
    {"task_text": "Define a recursive list-reverse and reverse (1 2 3)", "answer": "(begin (define (list-reverse lst) (if (null? lst) '() (append (list-reverse (cdr lst)) (list (car lst))))) (list-reverse '(1 2 3)))", "category": "recursion"},
    {"task_text": "Define recursive count-occurrences and count 'a in (a b a c a)", "answer": "(begin (define (count-occ lst x) (cond [(null? lst) 0] [(equal? (car lst) x) (+ 1 (count-occ (cdr lst) x))] [else (count-occ (cdr lst) x)])) (count-occ '(a b a c a) 'a))", "category": "recursion"},
    {"task_text": "Define recursive flatten and flatten ((1 2) (3 (4 5)) 6)", "answer": "(begin (define (flatten lst) (cond [(null? lst) '()] [(pair? (car lst)) (append (flatten (car lst)) (flatten (cdr lst)))] [else (cons (car lst) (flatten (cdr lst)))])) (flatten '((1 2) (3 (4 5)) 6)))", "category": "recursion"},
    # tail-recursion
    {"task_text": "Define a tail-recursive sum using an accumulator and sum (1 2 3 4 5)", "answer": "(begin (define (sum-tr lst acc) (if (null? lst) acc (sum-tr (cdr lst) (+ acc (car lst))))) (sum-tr '(1 2 3 4 5) 0))", "category": "tail-recursion"},
    # anti-patterns (model should learn correct idioms)
    {"task_text": "Create a proper list containing 1 2 3 using cons and null", "answer": "(cons 1 (cons 2 (cons 3 '())))", "category": "anti-patterns"},
    {"task_text": "Use quasiquote with unquote: x=10, create (result 10)", "answer": "(let ([x 10]) `(result ,x))", "category": "anti-patterns"},
    {"task_text": "Use case to match the symbol 'circle and return \"round\"", "answer": "(case 'circle [(square) \"angular\"] [(circle) \"round\"] [else \"unknown\"])", "category": "anti-patterns"},
    {"task_text": "Use when to print only if x > 0 (x is 5)", "answer": "(let ([x 5]) (when (> x 0) \"positive\"))", "category": "anti-patterns"},
    {"task_text": "Use unless to signal only if x is not positive (x is -3)", "answer": "(let ([x -3]) (unless (> x 0) \"not positive\"))", "category": "anti-patterns"},
    {"task_text": "Demonstrate proper tail position: return the last cdr result", "answer": "(let loop ([lst '(a b c)]) (if (null? (cdr lst)) (car lst) (loop (cdr lst))))", "category": "anti-patterns"},
]

TIER0_FEW_SHOTS = [
    ("Get the second element of (x y z)", "(second '(x y z))"),
    ("Filter even numbers from (1 2 3 4 5 6)", "(filter even? '(1 2 3 4 5 6))"),
    ("Add 17 and 25", "(+ 17 25)"),
    ("Reverse the list (1 2 3 4 5)", "(reverse '(1 2 3 4 5))"),
    ("Concatenate \"hello\" and \" world\"", "(string-append \"hello\" \" world\")"),
    ("Return \"yes\" if 5 > 3, otherwise \"no\"", "(if (> 5 3) \"yes\" \"no\")"),
    ("Sort the list (3 1 4 1 5 9 2 6)", "(sort < '(3 1 4 1 5 9 2 6))"),
    ("Bind x to 10 and y to 20, then compute (+ x y)", "(let ([x 10] [y 20]) (+ x y))"),
    ("Create a pair of 1 and 2", "(cons 1 2)"),
    ("Apply the function + to arguments 3 and 4", "(apply + '(3 4))"),
    ("Sum a list recursively", "(begin (define (sum lst) (if (null? lst) 0 (+ (car lst) (sum (cdr lst))))) (sum '(1 2 3)))"),
    ("Use quasiquote with unquote: x=10, create (result 10)", "(let ([x 10]) `(result ,x))"),
    ("Get the length of the string \"hello world\"", "(string-length \"hello world\")"),
    ("Double every element in (1 2 3 4)", "(map (lambda (x) (* x 2)) '(1 2 3 4))"),
    ("Get the car of the pair (hello . world)", "(car '(hello . world))"),
]


# ---------------------------------------------------------------------------
# Tier 1 task data — module discovery
# ---------------------------------------------------------------------------

TIER1_HEADER_REQUIRE = ";; Load Fold lattice skills"
TIER1_HEADER_SEARCH = ";; Search the Fold lattice"

TIER1_TASKS = [
    {"task_text": "Solve a system of linear equations", "answer": "linalg"},
    {"task_text": "Multiply two matrices together", "answer": "linalg"},
    {"task_text": "Compute the determinant of a matrix", "answer": "linalg"},
    {"task_text": "Find eigenvalues and eigenvectors", "answer": "linalg"},
    {"task_text": "Compute the dot product of two vectors", "answer": "linalg"},
    {"task_text": "Perform LU decomposition on a matrix", "answer": "linalg"},
    {"task_text": "Solve a least-squares regression problem", "answer": "linalg"},
    {"task_text": "Compute polynomial GCD", "answer": "algebra"},
    {"task_text": "Factor a polynomial over the integers", "answer": "algebra"},
    {"task_text": "Work with finite fields and modular arithmetic", "answer": "algebra"},
    {"task_text": "Multiply polynomials together", "answer": "algebra"},
    {"task_text": "Compute a Gröbner basis", "answer": "algebra"},
    {"task_text": "Simplify an algebraic expression", "answer": "algebra"},
    {"task_text": "Generate a random integer in a range", "answer": "random"},
    {"task_text": "Sample from a normal distribution", "answer": "random"},
    {"task_text": "Shuffle a list randomly", "answer": "random"},
    {"task_text": "Generate random bytes for testing", "answer": "random"},
    {"task_text": "Compute SHA-256 hash of data", "answer": "crypto"},
    {"task_text": "Generate a cryptographic HMAC", "answer": "crypto"},
    {"task_text": "Encrypt data with a symmetric cipher", "answer": "crypto"},
    {"task_text": "Perform forward-mode automatic differentiation", "answer": "autodiff"},
    {"task_text": "Compute gradients with reverse-mode autodiff", "answer": "autodiff"},
    {"task_text": "Differentiate a composition of functions", "answer": "autodiff"},
    {"task_text": "Compute a Jacobian matrix", "answer": "autodiff"},
    {"task_text": "Minimize a function using gradient descent", "answer": "optimization"},
    {"task_text": "Solve a constrained optimization problem", "answer": "optimization"},
    {"task_text": "Find the root of a nonlinear equation", "answer": "optimization"},
    {"task_text": "Perform line search optimization", "answer": "optimization"},
    {"task_text": "Compute descriptive statistics (mean, variance)", "answer": "statistics"},
    {"task_text": "Perform a hypothesis test", "answer": "statistics"},
    {"task_text": "Fit a linear regression model", "answer": "statistics"},
    {"task_text": "Compute correlation between two datasets", "answer": "statistics"},
    {"task_text": "Compute Bayesian posterior updates", "answer": "statistics"},
    {"task_text": "Parse a context-free grammar", "answer": "fp"},
    {"task_text": "Use monadic parser combinators", "answer": "fp"},
    {"task_text": "Compose parsers for a DSL", "answer": "fp"},
    {"task_text": "Build a lazy stream of values", "answer": "fp"},
    {"task_text": "Use the Maybe monad for error handling", "answer": "fp"},
    {"task_text": "Solve a SAT problem", "answer": "fp"},
    {"task_text": "Model a constraint satisfaction problem", "answer": "fp"},
    {"task_text": "Simulate a physics system", "answer": "physics"},
    {"task_text": "Compute rigid body dynamics", "answer": "physics"},
    {"task_text": "Model gravitational forces between bodies", "answer": "physics"},
    {"task_text": "Simulate projectile motion with drag", "answer": "physics"},
    {"task_text": "Compute geodesics on a manifold", "answer": "diffgeo"},
    {"task_text": "Compute Christoffel symbols", "answer": "diffgeo"},
    {"task_text": "Work with differential forms", "answer": "diffgeo"},
    {"task_text": "Compute curvature tensors", "answer": "diffgeo"},
    {"task_text": "Build a directed graph and find paths", "answer": "data"},
    {"task_text": "Implement a priority queue", "answer": "data"},
    {"task_text": "Traverse a tree data structure", "answer": "data"},
    {"task_text": "Use a hash map for key-value storage", "answer": "data"},
    {"task_text": "Compute shortest paths in a graph", "answer": "data"},
    {"task_text": "Build and query a trie", "answer": "data"},
    {"task_text": "Compute the convex hull of a point set", "answer": "geometry"},
    {"task_text": "Find the intersection of two line segments", "answer": "geometry"},
    {"task_text": "Test if a point is inside a polygon", "answer": "geometry"},
    {"task_text": "Compute Delaunay triangulation", "answer": "geometry"},
    {"task_text": "Solve a partial differential equation", "answer": "pde"},
    {"task_text": "Solve the heat equation on a grid", "answer": "pde"},
    {"task_text": "Compute a numerical PDE solution", "answer": "pde"},
    {"task_text": "Build a hexagonal tile grid", "answer": "tiles"},
    {"task_text": "Render a tile map", "answer": "tiles"},
    {"task_text": "Navigate tile coordinates", "answer": "tiles"},
    {"task_text": "Create a square tile grid", "answer": "tiles"},
    {"task_text": "Compute Shannon entropy", "answer": "info"},
    {"task_text": "Calculate mutual information between variables", "answer": "info"},
    {"task_text": "Compute KL divergence between distributions", "answer": "info"},
    {"task_text": "Apply the Fourier transform to a signal", "answer": "signal"},
    {"task_text": "Filter a signal with a low-pass filter", "answer": "signal"},
    {"task_text": "Compute a windowed FFT (STFT)", "answer": "signal"},
    {"task_text": "Use lenses to access nested data", "answer": "optics"},
    {"task_text": "Compose optics for deep field access", "answer": "optics"},
    {"task_text": "Use a prism to handle sum types", "answer": "optics"},
    {"task_text": "Traverse all elements with a traversal optic", "answer": "optics"},
    {"task_text": "Perform equality saturation on expressions", "answer": "egraph"},
    {"task_text": "Extract optimal form from an e-graph", "answer": "egraph"},
    {"task_text": "Add rewrite rules to an e-graph", "answer": "egraph"},
    {"task_text": "Compute topological invariants", "answer": "topology"},
    {"task_text": "Build a simplicial complex", "answer": "topology"},
    {"task_text": "Compute homology groups", "answer": "topology"},
    {"task_text": "Compute Betti numbers", "answer": "topology"},
    {"task_text": "Interpolate data points with cubic splines", "answer": "interpolate"},
    {"task_text": "Perform Lagrange interpolation", "answer": "interpolate"},
    {"task_text": "Do polynomial interpolation of a dataset", "answer": "interpolate"},
    {"task_text": "Compute interval arithmetic bounds", "answer": "interval"},
    {"task_text": "Check if two intervals overlap", "answer": "interval"},
    {"task_text": "Run a discrete event simulation", "answer": "sim"},
    {"task_text": "Model a queueing system", "answer": "sim"},
    {"task_text": "Simulate a Markov chain", "answer": "sim"},
    {"task_text": "Build an NFA and convert to DFA", "answer": "automata"},
    {"task_text": "Test if a string matches a regular expression via automaton", "answer": "automata"},
    {"task_text": "Minimize a DFA", "answer": "automata"},
    {"task_text": "Factor a large integer", "answer": "number-theory"},
    {"task_text": "Check if a number is prime", "answer": "number-theory"},
    {"task_text": "Compute the GCD of two numbers", "answer": "number-theory"},
    {"task_text": "Solve a game theory payoff matrix", "answer": "fp"},
    {"task_text": "Find Nash equilibria", "answer": "fp"},
    {"task_text": "Compute minimax for a two-player game", "answer": "fp"},
    {"task_text": "Query structured data with combinators", "answer": "query"},
    {"task_text": "Filter and project tabular data", "answer": "query"},
    {"task_text": "Apply a DSL template to generate code", "answer": "dsl"},
    {"task_text": "Build a reader macro for custom syntax", "answer": "dsl"},
]

REQUIRE_FEW_SHOTS = [
    ("Solve a system of linear equations", "linalg"),
    ("Generate cryptographic hashes", "crypto"),
    ("Build and query a data structure", "data"),
    ("Simulate classical physics", "physics"),
    ("Compute derivatives automatically", "autodiff"),
    ("Generate random numbers", "random"),
    ("Solve a boolean satisfiability problem", "fp"),
    ("Find Nash equilibria in a game", "fp"),
    ("Compute topological invariants", "topology"),
    ("Parse structured text with combinators", "fp"),
    ("Minimize a cost function", "optimization"),
    ("Compute Christoffel symbols on a manifold", "diffgeo"),
    ("Build a hexagonal tile grid", "tiles"),
    ("Compute Shannon entropy of a distribution", "info"),
    ("Optimize expressions via equality saturation", "egraph"),
]

SEARCH_FEW_SHOTS = [
    ("Find tools for matrix operations", "matrix linear algebra eigenvalue"),
    ("Find tools for cryptographic hashing", "crypto hash encryption"),
    ("Find tools for graph algorithms", "graph shortest path tree traversal"),
    ("Find tools for physics simulation", "physics rigid body dynamics force"),
    ("Find tools for automatic differentiation", "autodiff gradient jacobian"),
    ("Find tools for random number generation", "random sample distribution"),
    ("Find tools for constraint solving", "sat constraint satisfaction boolean"),
    ("Find tools for game theory", "game theory nash equilibrium payoff"),
    ("Find tools for topology", "topology simplicial complex homology"),
    ("Find tools for parsing", "parser combinator grammar context-free"),
    ("Find tools for optimization", "minimize gradient descent optimizer"),
    ("Find tools for differential geometry", "manifold christoffel geodesic curvature"),
    ("Find tools for tile grids", "tile hex grid coordinate"),
    ("Find tools for information theory", "entropy mutual information divergence"),
    ("Find tools for term rewriting", "egraph equality saturation rewrite"),
]


# ---------------------------------------------------------------------------
# Source file processing
# ---------------------------------------------------------------------------

def collect_source_files(source_dir: str) -> list[dict]:
    """Walk lattice/ and core/, read all .ss files, chunk into ~2048-token documents."""
    samples = []
    dirs = [os.path.join(source_dir, d) for d in ("lattice", "core")]

    for base_dir in dirs:
        if not os.path.isdir(base_dir):
            print(f"  Warning: {base_dir} not found, skipping", file=sys.stderr)
            continue

        for root, _dirs, files in os.walk(base_dir):
            for fname in sorted(files):
                if not fname.endswith(".ss"):
                    continue
                fpath = os.path.join(root, fname)
                try:
                    with open(fpath, "r", encoding="utf-8", errors="replace") as f:
                        content = f.read()
                except Exception as e:
                    print(f"  Warning: could not read {fpath}: {e}", file=sys.stderr)
                    continue

                if not content.strip():
                    continue

                # Chunk long files at ~8000 chars (~2048 tokens at ~4 chars/token)
                # Split on blank lines to preserve S-expression boundaries
                chunks = chunk_text(content, max_chars=8000)
                for chunk in chunks:
                    if chunk.strip():
                        rel_path = os.path.relpath(fpath, source_dir)
                        # Add a file header comment so the model learns file context
                        text = f";;; {rel_path}\n\n{chunk.strip()}"
                        samples.append({"text": text, "source": "scheme-source"})

    return samples


def chunk_text(text: str, max_chars: int = 8000) -> list[str]:
    """Split text into chunks at blank-line boundaries, respecting max_chars."""
    if len(text) <= max_chars:
        return [text]

    chunks = []
    paragraphs = text.split("\n\n")
    current = []
    current_len = 0

    for para in paragraphs:
        para_len = len(para) + 2  # +2 for the \n\n separator
        if current_len + para_len > max_chars and current:
            chunks.append("\n\n".join(current))
            current = [para]
            current_len = para_len
        else:
            current.append(para)
            current_len += para_len

    if current:
        chunks.append("\n\n".join(current))

    return chunks


# ---------------------------------------------------------------------------
# Synthgen processing
# ---------------------------------------------------------------------------

def collect_synthgen(synthgen_path: str) -> list[dict]:
    """Read sft-all-phases.jsonl, format as prompt + ground_truth raw text."""
    samples = []
    with open(synthgen_path, "r") as f:
        for line in f:
            entry = json.loads(line)
            prompt = entry.get("prompt", "").strip()
            ground_truth = entry.get("ground_truth", "").strip()

            if not prompt or not ground_truth:
                continue

            text = f"{prompt}\n\n{ground_truth}"
            samples.append({"text": text, "source": "synthgen"})

    return samples


# ---------------------------------------------------------------------------
# Tier0/Tier1 task processing
# ---------------------------------------------------------------------------

def build_tier0_completion(task: dict, n_shots: int = 0) -> str:
    """Build a tier0 completion training example."""
    parts = [TIER0_HEADER, ""]

    if n_shots > 0:
        # Deterministic few-shot selection
        idx = hash(task["task_text"]) % len(TIER0_FEW_SHOTS)
        for i in range(n_shots):
            shot_idx = (idx + i) % len(TIER0_FEW_SHOTS)
            comment, expr = TIER0_FEW_SHOTS[shot_idx]
            parts.append(f";; {comment}")
            parts.append(expr)
            parts.append("")

    parts.append(f";; {task['task_text']}")
    parts.append(task["answer"])
    return "\n".join(parts)


def build_tier1_require(task: dict, n_shots: int = 0) -> str:
    """Build a tier1 require-mode completion training example."""
    parts = [TIER1_HEADER_REQUIRE, ""]

    if n_shots > 0:
        idx = hash(task["task_text"]) % len(REQUIRE_FEW_SHOTS)
        for i in range(n_shots):
            shot_idx = (idx + i) % len(REQUIRE_FEW_SHOTS)
            desc, skill = REQUIRE_FEW_SHOTS[shot_idx]
            parts.append(f";; {desc}")
            parts.append(f"(require '{skill})")
            parts.append("")

    parts.append(f";; {task['task_text']}")
    parts.append(f"(require '{task['answer']})")
    return "\n".join(parts)


def build_tier1_search(task: dict, n_shots: int = 0) -> str:
    """Build a tier1 search-mode completion training example."""
    parts = [TIER1_HEADER_SEARCH, ""]

    if n_shots > 0:
        idx = hash(task["task_text"]) % len(SEARCH_FEW_SHOTS)
        for i in range(n_shots):
            shot_idx = (idx + i) % len(SEARCH_FEW_SHOTS)
            desc, query = SEARCH_FEW_SHOTS[shot_idx]
            parts.append(f";; {desc}")
            parts.append(f'(lf "{query}")')
            parts.append("")

    # For search mode, create a keyword query from the task text
    keywords = task["task_text"].lower().replace(",", " ").split()
    # Keep meaningful keywords, drop common words
    stopwords = {"a", "an", "the", "of", "for", "and", "to", "in", "on", "with", "is", "it", "by"}
    keywords = [w for w in keywords if w not in stopwords and len(w) > 2]
    query_str = " ".join(keywords[:5])

    parts.append(f";; Find tools for: {task['task_text'].lower()}")
    parts.append(f'(lf "{query_str}")')
    return "\n".join(parts)


def collect_tier_tasks() -> list[dict]:
    """Generate completion examples from tier0 and tier1 tasks."""
    samples = []

    # Tier 0: generate at 0-shot and 3-shot
    for task in TIER0_TASKS:
        samples.append({"text": build_tier0_completion(task, n_shots=0), "source": "tier0"})
        samples.append({"text": build_tier0_completion(task, n_shots=3), "source": "tier0"})

    # Tier 1: require and search modes
    for task in TIER1_TASKS:
        samples.append({"text": build_tier1_require(task, n_shots=0), "source": "tier1-require"})
        samples.append({"text": build_tier1_require(task, n_shots=3), "source": "tier1-require"})
        samples.append({"text": build_tier1_search(task, n_shots=0), "source": "tier1-search"})
        samples.append({"text": build_tier1_search(task, n_shots=3), "source": "tier1-search"})

    return samples


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(description="Prepare SFT data for Qwen3.5-4B-Base")
    parser.add_argument("--source-dir", default=os.path.expanduser("~/fold"),
                        help="Root of Fold project (for lattice/*.ss, core/*.ss)")
    parser.add_argument("--synthgen", default=os.path.expanduser("~/fold/data/sft-all-phases.jsonl"),
                        help="Path to synthgen JSONL")
    parser.add_argument("--output-dir", default=os.path.expanduser("~/fold/user/rlm/training/base"),
                        help="Output directory for train/eval JSONL")
    parser.add_argument("--eval-ratio", type=float, default=0.05,
                        help="Fraction of data to hold out for eval")
    parser.add_argument("--seed", type=int, default=42)
    args = parser.parse_args()

    random.seed(args.seed)
    os.makedirs(args.output_dir, exist_ok=True)

    # Collect all data sources
    print("Collecting Scheme source files...")
    source_samples = collect_source_files(args.source_dir)
    print(f"  {len(source_samples)} chunks from source files")

    print("Collecting synthgen data...")
    synthgen_samples = collect_synthgen(args.synthgen)
    print(f"  {len(synthgen_samples)} samples from synthgen")

    print("Generating tier0/tier1 completion tasks...")
    tier_samples = collect_tier_tasks()
    print(f"  {len(tier_samples)} samples from tier tasks")

    # Combine
    all_samples = source_samples + synthgen_samples + tier_samples
    print(f"\nTotal: {len(all_samples)} samples")

    # Stratified split: keep source file chunks together by grouping consecutive same-source
    # For simplicity, just shuffle and split (source chunks are independent enough)
    random.shuffle(all_samples)

    n_eval = max(1, int(len(all_samples) * args.eval_ratio))
    eval_samples = all_samples[:n_eval]
    train_samples = all_samples[n_eval:]

    # Write output
    train_path = os.path.join(args.output_dir, "train.jsonl")
    eval_path = os.path.join(args.output_dir, "eval.jsonl")

    with open(train_path, "w") as f:
        for sample in train_samples:
            f.write(json.dumps(sample) + "\n")

    with open(eval_path, "w") as f:
        for sample in eval_samples:
            f.write(json.dumps(sample) + "\n")

    # Summary
    source_counts = {}
    for s in all_samples:
        src = s["source"]
        source_counts[src] = source_counts.get(src, 0) + 1

    print(f"\nOutput:")
    print(f"  Train: {len(train_samples)} → {train_path}")
    print(f"  Eval:  {len(eval_samples)} → {eval_path}")
    print(f"\nBy source:")
    for src, count in sorted(source_counts.items()):
        print(f"  {src}: {count}")


if __name__ == "__main__":
    main()
