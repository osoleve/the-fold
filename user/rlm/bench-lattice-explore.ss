;;; user/rlm/bench-lattice-explore.ss — Lattice Exploration Trace Collector
;;;
;;; Generates diverse traces of a capable model navigating the Fold lattice.
;;; Only trajectories where the model reaches (submit ...) are marked correct.
;;;
;;; Tasks may specify setup actions — pre-loaded modules and pre-bound variables
;;; that let the model focus on lattice navigation rather than API memorization.
;;; Setup actions execute silently before the model starts, at zero fuel cost.
;;;
;;; Run: RLM_INTEGRATION=1 scheme --script user/rlm/bench-lattice-explore.ss
;;; Env: RLM_MODEL, RLM_PORT, RLM_HOST, RLM_TEMPERATURE

(unless (top-level-bound? 'rlm2-run)
  (load "boundary/pipeline/rlm2-drive.ss"))

(unless (top-level-bound? 'rlm2-fit-prompt)
  (load "boundary/pipeline/rlm2-prompt-fit.ss"))

;;; ====
;;; System prompt — action grammar + few-shot examples
;;; ====

(define *explore-system-prompt*
  (string-append
    "## Example\n\n"
    "Task: Compute the determinant of matrix ((1 2) (3 4))\n\n"
    "Turn 1 → (search \"matrix determinant\")\n"
    "Turn 2 → (load 'matrix)\n"
    "Turn 3 → (store 'M (matrix-from-lists '((1 2) (3 4))))\n"
    "Turn 4 → (store 'det (matrix-det M))\n"
    "Turn 5 → (submit det)\n"))

;;; ====
;;; Task definitions
;;; ====
;;; Each task: (question expected-answer mode setup)
;;;   mode: 'explore (always "correct") | 'compute (check answer)
;;;   setup: list of RLM actions to replay silently before the model starts
;;;          (load ...), (store ...), (eval ...) — at zero fuel cost
;;;   setup defaults to '() when omitted (3-element tasks still work).
;;; For 'explore tasks, expected-answer is ignored.

(define *lattice-tasks*
  (list
    ;; ================================================================
    ;; CRYPTO — pre-loaded modules, model calls hash functions directly
    ;; ================================================================

    (list "Module sha512 is loaded. Compute (sha512-hex (string->utf8 \"hello world\")) and submit the hex string."
          "309ecc489c12d6eb4cc40f50c902f2b4d0ed77ee511a7c7a9bcd3ca86d4cd86f989dd35bc5ff499670da34255b45b0cfd830e81f605dcf7dc5542e93ae9cd76f" 'compute
          '((load sha512)))

    (list "Search for BLAKE2b hashing. Load the blake2b module and compute the hash of (string->utf8 \"hello world\"). Submit the hex string."
          "021ced8799296ceca557832ab941a50b4a11f83478cf141f51f933f653ab9fbcc05a037cddbed06e309bf334942c4e58cdf1a46e237911ccd7fcf9787cbc7fd0" 'compute)

    (list "Search for HMAC in the lattice. Load the hmac module and compute hmac-sha256-hex with key (string->utf8 \"secret\") and message (string->utf8 \"message\"). Submit the hex result."
          "8b5f48702995c1598c573db1e21866a9b825d4a794d169d7060a03605796360b" 'compute)

    ;; ================================================================
    ;; LINALG — pre-loaded modules and pre-bound matrix M
    ;; The model navigates to the right function and calls it.
    ;; ================================================================

    (list "The variable M holds a 3x3 matrix ((2 1 3) (4 -1 2) (1 5 -2)). Modules matrix and matrix-decomp are loaded. Compute the determinant of M. Submit the numeric result."
          "57" 'compute
          '((load matrix) (load matrix-decomp)
            (store M (matrix-from-lists '((2 1 3) (4 -1 2) (1 5 -2))))))

    (list "The variable M holds a 3x3 matrix ((7 2 1) (0 3 -1) (-3 4 2)). Module matrix is loaded. Compute the trace of M. Submit the numeric result."
          "12" 'compute
          '((load matrix)
            (store M (matrix-from-lists '((7 2 1) (0 3 -1) (-3 4 2))))))

    (list "The variable M holds a 2x2 matrix ((2 1) (5 3)). Modules matrix and matrix-solvers are loaded. Compute the inverse of M. Submit the result matrix."
          "3 -1 -5 2" 'compute
          '((load matrix) (load matrix-solvers)
            (store M (matrix-from-lists '((2 1) (5 3))))))

    (list "The variable M holds a 3x3 matrix ((1 2 3) (4 5 6) (7 8 10)). Modules matrix and matrix-decomp are loaded. Compute the determinant of M. Submit the number."
          "-3" 'compute
          '((load matrix) (load matrix-decomp)
            (store M (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10))))))

    ;; ================================================================
    ;; NUMBER THEORY — no setup needed, simple function calls
    ;; ================================================================

    (list "Search for modular arithmetic in the lattice. Load the modular module. Compute (mod-expt 7 256 1000003). Submit the numeric result."
          "357979" 'compute)

    (list "Search for modular arithmetic. Load the modular module. Compute the modular inverse of 17 modulo 1000003. Submit the numeric result."
          "411766" 'compute)

    (list "Module modular is loaded. The CRT function is (crt residues moduli) where both args are lists. Find x such that x ≡ 2 (mod 3), x ≡ 3 (mod 5), x ≡ 2 (mod 7). Submit the number."
          "23" 'compute
          '((load modular)))

    (list "Search for prime factorization. Load the primality module. Compute (factorize 2310) to get prime factors as a flat list. Submit the result."
          "(2 3 5 7 11)" 'compute)

    (list "Search for Euler's totient function. Load the primality module. Compute euler-totient of 2310. Submit the numeric result."
          "480" 'compute)

    ;; ================================================================
    ;; OPTICS — pre-loaded module with pre-composed lenses
    ;; Model just calls view/over — no lens composition required.
    ;; ================================================================

    (list "The variable grid holds a 3x3x3 nested list and L is a pre-composed lens targeting position [2][1][0]. The optics module is loaded. Functions: (view lens data), (over lens f data). Compute (view L grid) and submit the number."
          "22" 'compute
          '((load optics)
            (store grid '(((1 2 3) (4 5 6) (7 8 9))
                          ((10 11 12) (13 14 15) (16 17 18))
                          ((19 20 21) (22 23 24) (25 26 27))))
            (store L (lens-compose (lens-nth 2) (lens-compose (lens-nth 1) (lens-nth 0))))))

    (list "The variable grid holds a 3x3x3 nested list and L is a pre-composed lens targeting position [1][2][1] (value is 17). The optics module is loaded. Use (over L f data) to square the element: (over L (lambda (x) (* x x)) grid). Submit the value at that position after modification."
          "289" 'compute
          '((load optics)
            (store grid '(((1 2 3) (4 5 6) (7 8 9))
                          ((10 11 12) (13 14 15) (16 17 18))
                          ((19 20 21) (22 23 24) (25 26 27))))
            (store L (lens-compose (lens-nth 1) (lens-compose (lens-nth 2) (lens-nth 1))))))

    (list "The variable data holds '(10 20 30 40 50). The optics module is loaded. Use (view (lens-nth 3) data) to get the element at index 3. Submit the number."
          "40" 'compute
          '((load optics)
            (store data '(10 20 30 40 50))))

    ;; ================================================================
    ;; TOPOLOGY — pre-loaded modules and pre-bound complexes
    ;; ================================================================

    (list "The variable T holds a simplicial complex representing a torus (via make-torus). Modules simplicial-complex and homology are loaded. Compute the Betti numbers of T. Submit the list."
          "(1 2 1)" 'compute
          '((load simplicial-complex) (load homology)
            (store T (make-torus))))

    (list "The variable C holds a simplicial complex (triangle boundary — 3 edges, no filled face). Modules simplicial-complex and homology are loaded. Compute (sc-betti-numbers C). Submit the list."
          "(1 1)" 'compute
          '((load simplicial-complex) (load homology)
            (store C (sc-from-simplices
                       (list (make-simplex '(0 1))
                             (make-simplex '(1 2))
                             (make-simplex '(0 2)))))))

    ;; ================================================================
    ;; POLYNOMIAL ALGEBRA — pre-loaded with polynomials bound
    ;; ================================================================

    (list "The variable product holds the result of multiplying two polynomials: (1 - x^2) * (1 + x). Modules algebra/polynomial and field are loaded. Extract its coefficients with (poly-coeffs product). Submit the list."
          "(1 1 -1 -1)" 'compute
          '((load algebra/polynomial) (load field)
            (store product (poly-mul (make-polynomial Q-field '(1 0 -1))
                                     (make-polynomial Q-field '(1 1))))))

    ;; ================================================================
    ;; AUTODIFF — pre-loaded with traced arithmetic available
    ;; ================================================================

    (list "Module higher-order-diff is loaded (which provides grad, traced-mul, traced-add). Compute the gradient of f(x) = x^3 at x = 4.0. Note: grad takes a function and a list of arguments, returns a list. Use traced-mul to build x*x*x. Submit the numeric result."
          "48" 'compute
          '((load higher-order-diff)))

    ;; ================================================================
    ;; INFORMATION THEORY — no setup needed, simple function calls
    ;; ================================================================

    (list "Search for entropy or information theory. Load the entropy module. Compute the Shannon entropy of the distribution (0.1 0.2 0.3 0.4). Submit the numeric result."
          "1.846" 'compute)

    (list "Search for KL divergence. Load the entropy module. Compute the KL divergence from a uniform distribution (0.25 0.25 0.25 0.25) to the distribution (0.1 0.2 0.3 0.4). Submit the numeric result."
          "0.175" 'compute)

    ;; ================================================================
    ;; STATISTICS — pre-loaded with data bound
    ;; ================================================================

    (list "The variable data holds the list (2 4 4 4 5 5 7 9). Module summary-stats is loaded. Compute the variance of data. Submit the result."
          "32/7" 'compute
          '((load summary-stats)
            (store data '(2 4 4 4 5 5 7 9))))

    ;; ================================================================
    ;; CROSS-MODULE CHAINS — pre-loaded linalg, model chains to crypto
    ;; ================================================================

    (list "The variable M holds matrix ((1 2 3) (4 5 6) (7 8 10)). Modules matrix and matrix-decomp are loaded. Compute the determinant of M, then load sha512 and compute the SHA-512 hash of that number as a string. Submit the hex hash."
          "023b573307a786e687c7a85d0a2ea74e74f77d16989f18fc7c2e6dd60f7218c027c77def8ac9a1a3fb1dbcd66cb8d3c3c4bd840f8f70b8b82b7a692f6f274631" 'compute
          '((load matrix) (load matrix-decomp)
            (store M (matrix-from-lists '((1 2 3) (4 5 6) (7 8 10))))))

    ;; ================================================================
    ;; META-TOOLING — lattice structure queries (no setup needed)
    ;; ================================================================

    (list "Search for information about the lattice itself. Use (inspect 'crypto) to find out how many modules the crypto skill has. Submit the number of modules."
          "4" 'compute)

    (list "Use (inspect 'autodiff) to find the lattice depth (tier) of the autodiff skill. Submit the tier number."
          "8" 'compute)

    (list "Use (inspect 'statistics) to find its tier in the lattice. Submit the tier number."
          "6" 'compute)

    ;; ================================================================
    ;; GAME THEORY — pre-loaded with game bound (new category)
    ;; ================================================================

    (list "The variable G holds a Prisoner's Dilemma game. Module game-theory/normal-form is loaded. Call (best-response-p1 G 0) to find player 1's best response when player 2 cooperates (index 0). Submit the result."
          "(1)" 'compute
          '((load game-theory/normal-form)
            (store G (make-game '(cooperate defect) '(cooperate defect)
                       '(((3 . 3) (0 . 5)) ((5 . 0) (1 . 1)))))))

    (list "The variable G holds a Prisoner's Dilemma game. Module game-theory/normal-form is loaded. Call (find-pure-nash G) to find all pure Nash equilibria. Submit the result list."
          "((1 . 1))" 'compute
          '((load game-theory/normal-form)
            (store G (make-game '(cooperate defect) '(cooperate defect)
                       '(((3 . 3) (0 . 5)) ((5 . 0) (1 . 1)))))))

    ;; ================================================================
    ;; ZIPPERS — pre-loaded with zipper bound (new category)
    ;; ================================================================

    (list "The variable z holds a list zipper focused on element 3 (the third element of (1 2 3 4 5)). Module fp/data/zipper is loaded. Use (zipper-set z 99) to replace the focus with 99, then (zipper->list ...) to convert back. Submit the resulting list."
          "(1 2 99 4 5)" 'compute
          '((load fp/data/zipper)
            (store z (from-just (zipper-right!
                       (from-just (zipper-right!
                         (list->zipper '(1 2 3 4 5)))))))))

    ;; ================================================================
    ;; HEX GRIDS — pre-loaded tiles/hex (new category from exercises)
    ;; ================================================================

    (list "Module tiles/hex is loaded. The variable A holds hex coordinate (0 . 0) and B holds (3 . 2), created via (axial-coord q r). Compute (hex-distance A B). Submit the number."
          "5" 'compute
          '((load tiles/hex)
            (store A (axial-coord 0 0))
            (store B (axial-coord 3 2))))

    (list "Module tiles/hex is loaded. The variable C holds hex coordinate (0 . 0). Compute (length (hex-neighbors C)) to find how many neighbors a hex cell has. Submit the number."
          "6" 'compute
          '((load tiles/hex)
            (store C (axial-coord 0 0))))

    (list "Search for hex grids or tile systems. Load the tiles/hex module. Coordinates are created with (axial-coord q r). Compute the number of hexes in a ring of radius 2 around the origin: (length (hex-ring (axial-coord 0 0) 2)). Submit the number."
          "12" 'compute)

    ;; ================================================================
    ;; SYMBOLIC COMPUTATION — pre-loaded expr/diff/simplify (new category)
    ;; ================================================================

    (list "Modules symbolic/expr, symbolic/diff, and symbolic/simplify are loaded. Symbolic expressions use constructors: (var 'x), (num n), (sum a b), (product a b), (make-pow base exp). Compute the derivative of x^3 with respect to x: (deriv (make-pow (var 'x) (num 3)) 'x). Submit the result."
          "(* (num 3) (^ (var x) (num 2)))" 'compute
          '((load symbolic/expr) (load symbolic/diff) (load symbolic/simplify)))

    (list "Modules symbolic/expr, symbolic/diff, and symbolic/simplify are loaded. The variable f holds the expression 3*x, built as (product (num 3) (var 'x)). Compute (simplify (deriv f 'x)). Submit the simplified result."
          "(num 3)" 'compute
          '((load symbolic/expr) (load symbolic/diff) (load symbolic/simplify)
            (store f (product (num 3) (var 'x)))))

    ;; ================================================================
    ;; LINALG (additional) — matrix multiply, vec dot, transpose, solve
    ;; ================================================================

    (list "The variables A and B hold 2x2 matrices. Module matrix is loaded. A = ((1 2) (3 4)), B = ((5 6) (7 8)). Compute (matrix-mul A B). Submit the result matrix."
          "19 22 43 50" 'compute
          '((load matrix)
            (store A (matrix-from-lists '((1 2) (3 4))))
            (store B (matrix-from-lists '((5 6) (7 8))))))

    (list "The variable M holds a 2x3 matrix ((1 2 3) (4 5 6)). Module matrix is loaded. Compute (matrix-transpose M). Submit the result matrix."
          "1 4 2 5 3 6" 'compute
          '((load matrix)
            (store M (matrix-from-lists '((1 2 3) (4 5 6))))))

    (list "The variables u and v hold vectors. Modules vec is loaded. u = (1 2 3), v = (4 5 6), created via (vec-from-list ...). Compute (vec-dot u v). Submit the number."
          "32" 'compute
          '((load vec)
            (store u (vec-from-list '(1 2 3)))
            (store v (vec-from-list '(4 5 6)))))

    (list "The variable A holds matrix ((2 1) (5 3)) and b holds vector #(4 7). Modules matrix and matrix-solvers are loaded. Solve Ax = b using (matrix-solve A b). Submit the result vector."
          "5 -6" 'compute
          '((load matrix) (load matrix-solvers) (load vec)
            (store A (matrix-from-lists '((2 1) (5 3))))
            (store b (vec-from-list '(4 7)))))

    ;; ================================================================
    ;; INFORMATION THEORY (additional) — binary entropy, cross entropy
    ;; ================================================================

    (list "Module entropy is loaded. Compute (binary-entropy 0.5) — the entropy of a fair coin flip. Submit the numeric result."
          "1.0" 'compute
          '((load entropy)))

    (list "Module entropy is loaded. Compute the cross-entropy from P = (0.25 0.25 0.25 0.25) to Q = (0.1 0.2 0.3 0.4): (cross-entropy '(0.25 0.25 0.25 0.25) '(0.1 0.2 0.3 0.4)). Submit the numeric result."
          "2.175" 'compute
          '((load entropy)))

    (list "Search for max entropy or information theory. Load the entropy module. Compute (max-entropy 6) — the maximum entropy of a distribution over 6 outcomes. Submit the numeric result."
          "2.584" 'compute)

    ;; ================================================================
    ;; 3D VECTORS & QUATERNIONS — pre-loaded with vectors/quats bound
    ;; ================================================================

    (list "The variables u and v hold 3D vectors. Module vec3 is loaded. u = (vec3 1 0 0), v = (vec3 0 1 0). Compute (vec3-cross u v). Submit the result."
          "(vec3 0 0 1)" 'compute
          '((load vec3)
            (store u (vec3 1 0 0))
            (store v (vec3 0 1 0))))

    (list "The variable w holds a 3D vector (vec3 3 4 0). Module vec3 is loaded. Compute (vec3-magnitude w). Submit the number."
          "5" 'compute
          '((load vec3)
            (store w (vec3 3 4 0))))

    (list "The variables p and q hold quaternions. Module quaternion is loaded. p = (quat 0 1 0 0), q = (quat 0 0 1 0). Compute (quat-mul p q). Submit the result."
          "(quat 0 0 0 1)" 'compute
          '((load quaternion)
            (store p (quat 0 1 0 0))
            (store q (quat 0 0 1 0))))

    ;; ================================================================
    ;; CONVOLUTION — pre-loaded with signal vectors bound
    ;; ================================================================

    (list "The variables sig and ker hold vectors for convolution. Module numeric/convolution and vec are loaded. sig = #(1 2 3), ker = #(4 5). Compute (convolve-direct-full sig ker). Submit the result vector."
          "#(4 13 22 15)" 'compute
          '((load numeric/convolution) (load vec)
            (store sig (vec-from-list '(1 2 3)))
            (store ker (vec-from-list '(4 5)))))

    ;; ================================================================
    ;; INTERPOLATION — search or pre-loaded
    ;; ================================================================

    (list "Search for interpolation in the lattice. Load the interpolate module. Compute (interp-lagrange '(0 1 2) '(1 3 7) 1.5). Submit the numeric result."
          "4.75" 'compute)

    (list "Module interpolate is loaded. Fit a degree-1 polynomial to points (0,1), (1,3), (2,5): (polyfit '(0 1 2) '(1 3 5) 1). Submit the result."
          "(poly #(2 1))" 'compute
          '((load interpolate)))

    ;; ================================================================
    ;; GEOMETRY — convex hull with pre-loaded points
    ;; ================================================================

    (list "Module convex-hull is loaded. The variable pts holds 5 points: #(0 0), #(4 0), #(0 3), #(4 3), #(2 1). Points are vectors. Compute (length (convex-hull pts)) to count hull vertices. Submit the number."
          "4" 'compute
          '((load convex-hull)
            (store pts (list (vector 0 0) (vector 4 0) (vector 0 3) (vector 4 3) (vector 2 1)))))

    (list "Module convex-hull is loaded. The variable tri holds 3 points forming a triangle: #(0 0), #(4 0), #(0 3). Compute (convex-hull-area tri). Submit the number."
          "6" 'compute
          '((load convex-hull)
            (store tri (list (vector 0 0) (vector 4 0) (vector 0 3)))))

    ;; ================================================================
    ;; SAT SOLVER — search or pre-loaded
    ;; ================================================================

    (list "Search for SAT or satisfiability in the lattice. Load the sat module. Check if the clauses ((1 2) (-1 2) (1 -2)) are satisfiable using (sat-satisfiable? '((1 2) (-1 2) (1 -2))). Submit the boolean result."
          "#t" 'compute)

    (list "Module sat is loaded. Check if the clauses ((1) (-1)) are satisfiable: (sat-satisfiable? '((1) (-1))). Submit the boolean result."
          "#f" 'compute
          '((load sat)))

    ;; ================================================================
    ;; STATISTICAL DISTRIBUTIONS — pre-loaded
    ;; ================================================================

    (list "Module distributions is loaded. Compute (standard-normal-pdf 0) — the PDF of the standard normal at x=0. Submit the numeric result."
          "0.398" 'compute
          '((load distributions)))

    (list "Module summary-stats is loaded. The variable data holds '(1 3 5 7 9). Compute (median data). Submit the number."
          "5" 'compute
          '((load summary-stats)
            (store data '(1 3 5 7 9))))

    ;; ================================================================
    ;; OPEN EXPLORATION — correct if submitted (diverse trajectories)
    ;; ================================================================

    (list "Explore the Fold's crypto skill. Search for it, inspect what modules are available, load one, and demonstrate a function from it. Submit any result you compute."
          "" 'explore)

    (list "Explore the Fold's linalg skill. Search for it, find what matrix operations are available, load a module, and perform a computation. Submit any result."
          "" 'explore)

    (list "Explore the Fold's optics skill. Search for lenses or optics, find what's available, load the module, and demonstrate composing two optics. Submit any result."
          "" 'explore)

    (list "Explore the Fold's topology skill. Search for it, find what constructions are available, load a module, and build something. Submit any result."
          "" 'explore)

    (list "Explore the Fold's information theory capabilities. Search for entropy, find what modules exist, load one, and compute something. Submit any result."
          "" 'explore)
    ))

;;; ====
;;; Runner
;;; ====

(define (run-lattice-task task-entry provider system-prompt i total)
  (let* ([question (car task-entry)]
         [expected (cadr task-entry)]
         [mode (caddr task-entry)]
         [setup (if (> (length task-entry) 3) (cadddr task-entry) '())]
         [max-steps 15]
         [max-fuel 60000]
         [label (format "lattice-~a-~a" mode i)])

    (display (format "\n=== [~a/~a] ~a ===\n" (+ i 1) total label))
    (display (format "Task: ~a\n"
                     (if (> (string-length question) 120)
                         (string-append (substring question 0 120) "...")
                         question)))
    (when (not (null? setup))
      (display (format "  Setup: ~a action~a\n"
                       (length setup)
                       (if (= (length setup) 1) "" "s"))))
    (flush-output-port)

    (guard (ex [else
                (display (format "  ERROR: ~a\n"
                          (if (message-condition? ex)
                              (condition-message ex)
                              ex)))
                `((label . ,label)
                  (correct . #f)
                  (status . "error")
                  (trajectory . #f))])
      (let-values ([(result ms)
                    (wall-clock-ms
                      (lambda ()
                        ;; Build config, appending empty few-shot + setup
                        (let ([config (append
                                        (make-rlm2-config
                                          provider system-prompt
                                          max-steps max-fuel
                                          2000  ; chunk-size
                                          1     ; max-depth
                                          3     ; loop-window
                                          8000  ; context-budget
                                          #f    ; no verifier
                                          512)  ; max-tokens
                                        (list '()     ; few-shot (empty)
                                              setup))])  ; setup actions
                          (rlm2-run config question ""))))])
        (let* ([status (rlm2-run-result-status result)]
               [output (format "~a" (rlm2-run-result-output result))]
               [traj (rlm2-run-result-trajectory-hash result)]
               ;; Gate: model must have submitted (status = completed)
               ;; Compute tasks additionally check the answer
               [submitted? (eq? status 'completed)]
               [correct? (and submitted?
                              (or (eq? mode 'explore)
                                  (and (string? expected)
                                       (> (string-length expected) 0)
                                       (string-contains output expected))))])
          (display (format "  Status: ~a | Time: ~a ms | Correct: ~a\n"
                           status ms correct?))
          (display (format "  Output: ~a\n"
                           (if (> (string-length output) 200)
                               (string-append (substring output 0 200) "...")
                               output)))
          (flush-output-port)

          `((label . ,label)
            (question . ,question)
            (expected . ,expected)
            (mode . ,(symbol->string mode))
            (setup . ,(length setup))
            (status . ,(symbol->string status))
            (time-ms . ,ms)
            (correct . ,correct?)
            (output . ,output)
            (trajectory . ,traj)))))))

;;; string-contains : String String -> Bool
(define (string-contains haystack needle)
  (let ([hlen (string-length haystack)]
        [nlen (string-length needle)])
    (if (> nlen hlen) #f
        (let loop ([i 0])
          (cond
            [(> (+ i nlen) hlen) #f]
            [(string=? (substring haystack i (+ i nlen)) needle) #t]
            [else (loop (+ i 1))])))))

;;; wall-clock-ms : (-> a) -> (values a Nat)
(define (wall-clock-ms thunk)
  (let* ([t0 (current-time)]
         [result (thunk)]
         [t1 (current-time)]
         [ms (+ (* 1000 (- (time-second t1) (time-second t0)))
                (quotient (- (time-nanosecond t1) (time-nanosecond t0))
                          1000000))])
    (values result ms)))

;;; ====
;;; Main
;;; ====

(define (run-lattice-suite)
  (let* ([model-id (or (getenv "RLM_MODEL")
                       "/models/Qwen3.5-27B-NVFP4")]
         [host (or (getenv "RLM_HOST") "localhost")]
         [port (or (and (getenv "RLM_PORT")
                        (string->number (getenv "RLM_PORT")))
                   8000)]
         [provider (make-rlm-provider
                     (format "http://~a:~a/v1/chat/completions" host port)
                     model-id #f 'openai)]
         ;; Optionally fit the system prompt to the target model
         [system-prompt (if (getenv "RLM_FIT_PROMPT")
                            (rlm2-get-fitted-prompt provider
                              *explore-system-prompt*)
                            *explore-system-prompt*)]
         [total (length *lattice-tasks*)]
         [n-with-setup (length (filter (lambda (t) (and (> (length t) 3)
                                                        (not (null? (cadddr t)))))
                                       *lattice-tasks*))])

    (display (format "Lattice Exploration Trace Collector\n"))
    (display (format "===================================\n"))
    (display (format "Model: ~a | Host: ~a:~a | Tasks: ~a (~a with setup)~a\n\n"
                     model-id host port total n-with-setup
                     (if (getenv "RLM_FIT_PROMPT") " | Fitted prompt" "")))
    (flush-output-port)

    (let loop ([tasks *lattice-tasks*] [i 0] [results '()])
      (if (null? tasks)
          ;; Done — report and save
          (let* ([results (reverse results)]
                 [n-correct (length (filter (lambda (r)
                                             (let ([c (assq 'correct r)])
                                               (and c (cdr c))))
                                           results))]
                 [n-explore (length (filter (lambda (r)
                                             (let ([m (assq 'mode r)])
                                               (and m (string=? (cdr m) "explore"))))
                                           results))]
                 [n-compute (- total n-explore)]
                 [n-setup (length (filter (lambda (r)
                                            (let ([s (assq 'setup r)])
                                              (and s (> (cdr s) 0))))
                                          results))]
                 [total-ms (apply + (map (lambda (r)
                                           (let ([t (assq 'time-ms r)])
                                             (if t (cdr t) 0)))
                                         results))]
                 [results-file (format "user/rlm/bench-results-lattice-~a.sexp"
                                       (rlm2-current-iso8601))])

            (display (format "\n\n========================================\n"))
            (display (format "RESULTS: ~a/~a correct (~a explore, ~a compute, ~a with setup)\n"
                             n-correct total n-explore n-compute n-setup))
            (display (format "Total time: ~a ms (~a ms avg)\n"
                             total-ms (if (> total 0) (quotient total-ms total) 0)))

            (call-with-output-file results-file
              (lambda (port)
                (pretty-print `(benchmark-results
                                 (model ,model-id)
                                 (mode "lattice-explore")
                                 (fitted-prompt ,(if (getenv "RLM_FIT_PROMPT") #t #f))
                                 (timestamp ,(rlm2-current-iso8601))
                                 (problems ,results))
                              port))
              'replace)
            (display (format "Results saved to ~a\n" results-file))
            (display "Run 'scheme --script user/rlm/collect-traces.ss' to extract JSONL\n"))

          ;; Run next task
          (let ([result (run-lattice-task (car tasks) provider system-prompt i total)])
            (loop (cdr tasks) (+ i 1) (cons result results)))))))

;; Auto-run when invoked as script
(when (getenv "RLM_INTEGRATION")
  (run-lattice-suite))
