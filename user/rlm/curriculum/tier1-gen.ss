#!/usr/bin/env scheme-script
;;; tier1-gen.ss — Generate Tier 1 module discovery training data as JSONL
;;;
;;; For each lattice skill, generate natural-language task descriptions that
;;; would lead an agent to search for and load that skill. The reward function
;;; in Prime Lab evaluates whether the model's search query would surface the
;;; correct skill via BM25 search.
;;;
;;; Output: user/rlm/curriculum/tier1-data.jsonl
;;; Format: {"prompt": "...", "target_module": "...", "keywords": [...]}

(load "lattice/meta/meta.ss")

;; ---- Minimal JSON emitter ----

(define (json-escape s)
  (if (not (string? s)) (json-escape (format "~a" s))
      (let loop ([i 0] [acc '()])
        (if (>= i (string-length s))
            (list->string (reverse acc))
            (let ([c (string-ref s i)])
              (cond
                [(char=? c #\") (loop (+ i 1) (append '(#\" #\\) acc))]
                [(char=? c #\\) (loop (+ i 1) (append '(#\\ #\\) acc))]
                [(char=? c #\newline) (loop (+ i 1) (append '(#\n #\\) acc))]
                [(char=? c #\tab) (loop (+ i 1) (append '(#\t #\\) acc))]
                [(char=? c #\return) (loop (+ i 1) (append '(#\r #\\) acc))]
                [else (loop (+ i 1) (cons c acc))]))))))

(define (emit-jsonl port prompt target-module keywords)
  (display "{" port)
  (display (format "\"prompt\": \"~a\", " (json-escape prompt)) port)
  (display (format "\"target_module\": \"~a\", " (json-escape target-module)) port)
  (display "\"keywords\": [" port)
  (let loop ([k keywords] [first? #t])
    (unless (null? k)
      (unless first? (display ", " port))
      (display (format "\"~a\"" (json-escape (car k))) port)
      (loop (cdr k) #f)))
  (display "]}\n" port))

;; ---- Prompt template ----

(define *system-context*
  (string-append
    "You are an RLM agent that can search and load modules from the Fold lattice. "
    "Given a task description, output a search query that would help you find "
    "the right module to load. Output ONLY the search query string, nothing else."))

(define (make-prompt task)
  (string-append *system-context* "\n\nTask: " task "\n\nSearch query:"))

;; ---- Hand-crafted task descriptions per skill ----
;; Each entry: (skill-name . ((task-description keyword1 keyword2 ...) ...))
;; We deliberately phrase tasks in ways a user/agent might naturally ask,
;; WITHOUT using the skill name directly.

(define *skill-tasks*
  '(;; --- linalg ---
    (linalg
      ("Compute the eigenvalues of a 3x3 matrix" "eigenvalue" "matrix")
      ("Multiply two matrices together" "matrix" "multiply")
      ("Solve a system of linear equations Ax=b" "solve" "linear" "equations")
      ("Decompose a matrix using QR factorization" "QR" "decomposition" "matrix")
      ("Find the singular value decomposition of a matrix" "SVD" "singular" "value"))

    ;; --- algebra ---
    (algebra
      ("Compute the greatest common divisor of two polynomials" "polynomial" "GCD")
      ("Multiply polynomials in a ring" "polynomial" "ring" "multiply")
      ("Compute a Groebner basis for a polynomial ideal" "Groebner" "basis" "ideal")
      ("Factor a polynomial over the integers" "polynomial" "factor"))

    ;; --- geometry ---
    (geometry
      ("Compute the intersection of a ray with a triangle" "ray" "triangle" "intersection")
      ("Build a BVH acceleration structure for a mesh" "BVH" "mesh" "acceleration")
      ("Perform marching cubes on an SDF" "marching" "cubes" "SDF" "isosurface")
      ("Compute Delaunay triangulation of a point set" "Delaunay" "triangulation")
      ("Generate a Voronoi diagram" "Voronoi" "diagram" "cells"))

    ;; --- numeric ---
    (numeric
      ("Compute the FFT of a signal" "FFT" "Fourier" "transform")
      ("Perform interval arithmetic operations" "interval" "arithmetic")
      ("Work with complex numbers" "complex" "numbers" "arithmetic")
      ("Do a discrete Fourier transform" "DFT" "frequency" "spectrum"))

    ;; --- optimization ---
    (optimization
      ("Minimize a function using gradient descent" "gradient" "descent" "minimize")
      ("Solve a linear programming problem" "linear" "programming" "simplex")
      ("Find the global minimum using interval methods" "global" "minimum" "interval")
      ("Solve an integer linear programming problem" "integer" "programming" "ILP")
      ("Use L-BFGS to optimize a function" "L-BFGS" "quasi-Newton" "optimize"))

    ;; --- fp ---
    (fp
      ("Parse a string using parser combinators" "parser" "combinator" "parse")
      ("Use monadic composition to chain computations" "monad" "bind" "composition")
      ("Build a game theory payoff matrix and find Nash equilibrium" "game" "theory" "Nash")
      ("Design a PID controller for a feedback system" "PID" "controller" "feedback")
      ("Use protocol dispatch to define extensible functions" "protocol" "dispatch"))

    ;; --- optics ---
    (optics
      ("Use a lens to focus on a nested field in a data structure" "lens" "focus" "nested")
      ("Traverse all elements in a container with a traversal" "traversal" "traverse" "container")
      ("Use a prism to access a variant in a sum type" "prism" "variant" "sum" "type")
      ("Compose lenses to access deeply nested data" "lens" "compose" "nested"))

    ;; --- data ---
    (data
      ("Find the shortest path in a weighted graph" "shortest" "path" "graph" "Dijkstra")
      ("Detect communities in a social network graph" "community" "detection" "graph")
      ("Use a priority queue for scheduling" "priority" "queue" "heap")
      ("Build and query a trie data structure" "trie" "prefix" "tree"))

    ;; --- statistics ---
    (statistics
      ("Fit a linear regression model to data" "regression" "linear" "fit")
      ("Perform a hypothesis test on two samples" "hypothesis" "test" "t-test")
      ("Fit a generalized linear model with Poisson family" "GLM" "Poisson" "regression")
      ("Compute descriptive statistics for a dataset" "mean" "median" "variance" "statistics"))

    ;; --- autodiff ---
    (autodiff
      ("Compute gradients using reverse-mode automatic differentiation" "gradient" "reverse" "mode" "autodiff")
      ("Build a computational graph for differentiation" "computation" "graph" "differentiation")
      ("Differentiate a function using dual numbers" "dual" "numbers" "forward" "mode"))

    ;; --- info ---
    (info
      ("Compute the Shannon entropy of a distribution" "entropy" "Shannon" "information")
      ("Encode a message using Huffman coding" "Huffman" "coding" "compression")
      ("Calculate mutual information between two variables" "mutual" "information"))

    ;; --- crypto ---
    (crypto
      ("Compute the SHA-512 hash of a message" "SHA-512" "hash" "digest")
      ("Generate an HMAC authentication code" "HMAC" "authentication" "MAC")
      ("Hash data using BLAKE2b" "BLAKE2b" "hash" "digest"))

    ;; --- number-theory ---
    (number-theory
      ("Check if a number is prime" "prime" "primality" "test")
      ("Compute modular exponentiation" "modular" "exponentiation" "power")
      ("Find the prime factorization of a number" "prime" "factorization"))

    ;; --- topology ---
    (topology
      ("Compute the Betti numbers of a simplicial complex" "Betti" "numbers" "homology")
      ("Build a simplicial complex from data" "simplicial" "complex" "topology")
      ("Compute the Euler characteristic" "Euler" "characteristic"))

    ;; --- random ---
    (random
      ("Generate random numbers from a normal distribution" "random" "normal" "distribution")
      ("Sample from a uniform distribution" "random" "uniform" "sample")
      ("Create a seeded PRNG for reproducibility" "PRNG" "seed" "random"))

    ;; --- diffgeo ---
    (diffgeo
      ("Compute Christoffel symbols on a Riemannian manifold" "Christoffel" "Riemannian" "manifold")
      ("Define a chart and atlas for a manifold" "chart" "atlas" "manifold")
      ("Compute curvature tensors" "curvature" "tensor" "Riemann"))

    ;; --- egraph ---
    (egraph
      ("Use equality saturation to optimize an expression" "equality" "saturation" "e-graph")
      ("Extract the lowest-cost expression from an e-graph" "cost" "extraction" "e-graph")
      ("Apply rewrite rules to simplify an expression" "rewrite" "rules" "simplify"))

    ;; --- sat ---
    (sat
      ("Solve a Boolean satisfiability problem" "SAT" "Boolean" "satisfiability")
      ("Find the maximum satisfiable subset of clauses" "MaxSAT" "optimization")
      ("Convert a problem to CNF form" "CNF" "conjunctive" "normal" "form"))

    ;; --- clp ---
    (clp
      ("Solve a constraint satisfaction problem with finite domains" "constraint" "satisfaction" "CSP")
      ("Use constraint logic programming to find valid assignments" "constraint" "logic" "programming")
      ("Define domain constraints and propagate" "domain" "constraint" "propagate"))

    ;; --- query ---
    (query
      ("Parse and execute a SQL query" "SQL" "query" "parse")
      ("Pattern match against structured data" "pattern" "match" "query"))

    ;; --- dsl ---
    (dsl
      ("Define a domain-specific language using tagless final" "tagless" "final" "DSL")
      ("Use staging to generate optimized code" "staging" "code" "generation")
      ("Build templates for S-expression generation" "template" "S-expression"))

    ;; --- tiles ---
    (tiles
      ("Create a hexagonal game board" "hex" "hexagonal" "board" "game")
      ("Build a square grid map for a strategy game" "grid" "square" "map" "strategy")
      ("Render a tile-based game board" "render" "tiles" "board"))

    ;; --- sim ---
    (sim
      ("Simulate a dynamical system over time" "simulate" "dynamics" "time" "step")
      ("Run an agent-based simulation" "agent" "simulation" "model"))

    ;; --- automata ---
    (automata
      ("Build a deterministic finite automaton" "DFA" "automaton" "state" "machine")
      ("Convert an NFA to a DFA" "NFA" "DFA" "conversion")
      ("Check if a string is accepted by a finite automaton" "automaton" "accept" "string"))

    ;; --- ui ---
    (ui
      ("Lay out UI components using combinators" "layout" "UI" "combinator")
      ("Compose visual elements horizontally and vertically" "compose" "horizontal" "vertical" "layout"))

    ;; --- meta ---
    (meta
      ("Search the lattice for available modules" "search" "lattice" "module" "find")
      ("Get information about a specific skill" "inspect" "skill" "info")
      ("List all exports from a skill" "exports" "list" "skill"))

    ;; --- physics ---
    (physics
      ("Simulate rigid body dynamics in 2D" "rigid" "body" "physics" "2D")
      ("Model gravitational attraction between particles" "gravity" "particles" "force")
      ("Run a 3D physics simulation" "physics" "3D" "simulation" "dynamics"))
    ))

;; ---- Generate ----

(define (generate-all output-file)
  (display "Initializing lattice...\n")
  (lattice-init-quiet!)

  (let ([skills (kg-skills)])
    (display (format "Lattice has ~a skills\n" (length skills)))

    (call-with-output-file output-file
      (lambda (port)
        (let ([count 0]
              [skills-covered 0])
          (for-each
            (lambda (skill-entry)
              (let ([skill-name (symbol->string (car skill-entry))]
                    [tasks (cdr skill-entry)])
                ;; Verify skill exists in lattice
                (when (memq (car skill-entry) skills)
                  (set! skills-covered (+ skills-covered 1))
                  (for-each
                    (lambda (task-entry)
                      (let ([task-desc (car task-entry)]
                            [keywords (cdr task-entry)])
                        (emit-jsonl port (make-prompt task-desc) skill-name keywords)
                        (set! count (+ count 1))))
                    tasks))))
            *skill-tasks*)

          (display (format "Generated ~a tasks covering ~a skills to ~a\n"
                           count skills-covered output-file))
          ;; Report uncovered skills
          (let ([covered-names (map car *skill-tasks*)])
            (let ([uncovered (filter (lambda (s) (not (memq s covered-names))) skills)])
              (when (not (null? uncovered))
                (display (format "Uncovered skills (~a): ~a\n"
                                 (length uncovered) uncovered))))))))))

(generate-all "user/rlm/curriculum/tier1-data.jsonl")
