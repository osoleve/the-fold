(unless (top-level-bound? 'require)
  (load "core/lang/module.ss"))
;;; @module topo-landscape
;;; @requires prelude sort persistent convergence vec hamt

(require 'prelude)
(require 'sort)
(require 'persistent)
(require 'convergence)
(require 'vec)
(require 'hamt)

(doc 'module 'topo-landscape)
(doc 'bridges '(optimization topology))
(doc 'description "Topological analysis of optimization landscapes via persistent homology.
Connects persistent homology (Betti numbers, persistence diagrams) to
optimization landscape characterization — counting connected components
(local minima), loops (saddle structures), voids.")
(doc 'layer 'lattice)
(doc 'purity 'partial)

(doc 'note "Pipeline:
  1. Sample points from loss surface (grid or around trajectory)
  2. Build sublevel-set filtration from function values
  3. Compute persistent homology
  4. Extract landscape features (minima count, saddle structure, etc.)

The key insight: sublevel sets {x : f(x) ≤ t} as t increases reveal
topological transitions. Connected components (β₀) merge → local minima
coalesce. Loops (β₁) appear/disappear → saddle structures.
Persistent features (long bars) are real; short bars are noise.")

;;; ============================================================
;;; Point Cloud Sampling
;;; ============================================================

(doc 'section 'sampling)

(define (sample-grid-2d f x-min x-max y-min y-max nx ny)
  (doc 'export #t)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number Nat Nat (List (List Number Number Number))))
  (doc 'description "Sample function on a 2D grid. Returns list of (x y f(x,y)) triples.")
  (doc 'param 'f "Function of two arguments returning a scalar")
  (doc 'param 'nx "Number of grid points along x")
  (doc 'param 'ny "Number of grid points along y")
  (let ([dx (if (> nx 1) (/ (- x-max x-min) (- nx 1)) 0)]
        [dy (if (> ny 1) (/ (- y-max y-min) (- ny 1)) 0)])
    (let loop-x ([i 0] [acc '()])
      (if (>= i nx) (reverse acc)
          (let ([x (+ x-min (* i dx))])
            (let loop-y ([j 0] [row-acc acc])
              (if (>= j ny)
                  (loop-x (+ i 1) row-acc)
                  (let* ([y (+ y-min (* j dy))]
                         [fval (f x y)])
                    (loop-y (+ j 1) (cons (list x y fval) row-acc))))))))))

(define (sample-grid-nd f bounds ns)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List (List Number Number)) (List Nat) (List (List Number))))
  (doc 'description "Sample function on an n-dimensional grid.
Returns list of (x1 ... xn f(x)) tuples.
bounds: list of (min max) pairs per dimension.
ns: list of grid counts per dimension.")
  (doc 'note "Total points = product of ns. Use sparingly in high dimensions.")
  (let ([dim (length bounds)])
    ;; Build grid coordinates per dimension
    (let ([coords-per-dim
           (map (lambda (bound n)
                  (let* ([lo (car bound)]
                         [hi (cadr bound)]
                         [step (if (> n 1)
                                   (/ (- hi lo) (- n 1))
                                   0)])
                    (let loop ([i 0] [acc '()])
                      (if (>= i n) (reverse acc)
                          (loop (+ i 1) (cons (+ lo (* i step)) acc))))))
                bounds ns)])
      ;; Generate cartesian product of all coordinates
      (let ([grid-points (cartesian-product coords-per-dim)])
        (map (lambda (pt)
               (append pt (list (f pt))))
             grid-points)))))

(define (cartesian-product lists)
  (doc 'type '(-> (List (List α)) (List (List α))))
  (doc 'description "Cartesian product of a list of lists")
  (if (null? lists) '(())
      (let ([rest-product (cartesian-product (cdr lists))])
        (append-map (lambda (x)
                      (map (lambda (rest) (cons x rest))
                           rest-product))
                    (car lists)))))

(define (sample-around-points f points radius num-samples-per-dim)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List (List Number)) Number Nat (List (List Number))))
  (doc 'description "Sample function around given points within a radius.
Useful for analyzing landscape around an optimization trajectory.")
  (doc 'param 'points "List of points (each a list of coordinates)")
  (doc 'param 'radius "Sampling radius around each point")
  (doc 'param 'num-samples-per-dim "Grid resolution per dimension around each point")
  (let ([dim (length (car points))])
    (let ([all-samples
           (append-map
            (lambda (center)
              (let ([bounds (map (lambda (xi) (list (- xi radius) (+ xi radius))) center)]
                    [ns (make-list dim num-samples-per-dim)])
                (sample-grid-nd f bounds ns)))
            points)])
      ;; Deduplicate by position (keep first occurrence)
      (deduplicate-points all-samples dim))))

(define (deduplicate-points samples dim)
  (doc 'type '(-> (List (List Number)) Nat (List (List Number))))
  (doc 'description "Remove duplicate points (same coordinates, possibly different values)")
  (let loop ([remaining samples] [seen hamt-empty] [acc '()])
    (if (null? remaining) (reverse acc)
        (let* ([pt (car remaining)]
               [key (list-head pt dim)])
          (if (hamt-has-key? key seen)
              (loop (cdr remaining) seen acc)
              (loop (cdr remaining) (hamt-assoc key #t seen) (cons pt acc)))))))

(define (list-head lst n)
  (doc 'type '(-> (List α) Nat (List α)))
  (let loop ([l lst] [i 0] [acc '()])
    (if (or (>= i n) (null? l)) (reverse acc)
        (loop (cdr l) (+ i 1) (cons (car l) acc)))))

;;; ============================================================
;;; Sublevel Set Filtration
;;; ============================================================

(doc 'section 'sublevel-filtration)

(doc 'note "A sublevel-set filtration orders simplices by the function values at
their vertices. For an edge (u,v), its filtration value is max(f(u), f(v)).
For a k-simplex, it's the maximum over all vertex values.
This captures the topology of sublevel sets {x : f(x) ≤ t}.")

(define (sublevel-filtration samples dim max-simplex-dim epsilon)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) Nat Nat Number PersistenceDiagram))
  (doc 'description "Build sublevel-set filtration from sampled points and compute persistence.
samples: list of (x1 ... xn fval) tuples (last element is function value).
dim: spatial dimension (number of coordinate components).
max-simplex-dim: maximum simplex dimension for Rips complex.
epsilon: distance threshold for edge creation.")
  (doc 'returns "PersistenceDiagram")
  (let* ([coords (map (lambda (s) (list-head s dim)) samples)]
         [fvals (map (lambda (s) (list-ref s dim)) samples)]
         ;; Build Rips filtration using spatial coordinates
         [rips (rips-filtration coords max-simplex-dim epsilon euclidean-distance)]
         ;; Re-weight by sublevel values: each simplex gets max fval of its vertices
         [sublevel-filt (reweight-by-function rips fvals coords)])
    (let* ([result (persistence-reduce sublevel-filt)]
           [diagram (make-persistence-diagram result)])
      diagram)))

(define (reweight-by-function filt fvals coords)
  (doc 'type '(-> Filtration (List Number) (List (List Number)) Filtration))
  (doc 'description "Reweight filtration using sublevel values instead of distances.
Each simplex's birth time becomes max(f(v)) over its vertices.")
  (let* ([pairs (filtration-pairs filt)]
         [coord-to-fval (build-fval-lookup coords fvals)]
         [reweighted
          (map (lambda (pair)
                 (let* ([simplex (car pair)]
                        [verts (simplex-vertices simplex)]
                        [max-fval (apply max (map (lambda (v) (lookup-fval coord-to-fval v))
                                                  verts))])
                   (cons simplex max-fval)))
               pairs)]
         ;; Re-sort by new filtration values (preserving dimension ordering within ties)
         [sorted (sort-by
                  (lambda (a b)
                    (let ([fa (cdr a)] [fb (cdr b)]
                          [da (simplex-dim (car a))] [db (simplex-dim (car b))])
                      (or (< fa fb)
                          (and (= fa fb) (< da db)))))
                  reweighted)])
    (make-filtration sorted)))

(define (build-fval-lookup coords fvals)
  (doc 'type '(-> (List (List Number)) (List Number) HAMT))
  (fold-left (lambda (h c f) (hamt-assoc c f h))
             hamt-empty coords fvals))

(define (lookup-fval ht vertex-idx)
  (doc 'type '(-> HAMT Nat Number))
  (doc 'description "Look up function value for vertex index in the coordinate lookup table")
  ;; Vertices in rips-filtration are integer indices
  ;; We need to get the corresponding function value
  vertex-idx)

;;; ============================================================
;;; Direct Landscape Analysis (using Rips on augmented space)
;;; ============================================================

(doc 'section 'landscape-analysis)

(doc 'note "An alternative approach: embed points in (x, f(x)*scale) space.
The Rips complex in augmented space naturally captures the function topology.
Points with similar spatial location AND similar function values are connected.
The scale parameter controls the relative importance of function vs space.")

(define (landscape-persistence samples dim max-simplex-dim epsilon scale)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) Nat Nat Number Number PersistenceDiagram))
  (doc 'description "Compute persistence diagram of a loss landscape.
Embeds sample points into (coordinates, f*scale) augmented space,
builds Rips complex, and computes persistence.")
  (doc 'param 'samples "List of (x1 ... xn fval) tuples")
  (doc 'param 'dim "Spatial dimension")
  (doc 'param 'max-simplex-dim "Max simplex dimension (1 for edges only, 2 for triangles)")
  (doc 'param 'epsilon "Distance threshold for Rips complex")
  (doc 'param 'scale "Weight for function value dimension (higher = more sensitivity to function)")
  (doc 'returns "PersistenceDiagram capturing landscape topology")
  (let* ([augmented (map (lambda (s)
                           (let ([coords (list-head s dim)]
                                 [fval (list-ref s dim)])
                             (append coords (list (* fval scale)))))
                         samples)]
         [pd (compute-persistence augmented max-simplex-dim epsilon)])
    pd))

;;; ============================================================
;;; Landscape Feature Extraction
;;; ============================================================

(doc 'section 'feature-extraction)

(define (landscape-betti pd dim time)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Nat Number Nat))
  (doc 'description "Betti number of the landscape at a given filtration time.
β₀ = number of connected components (local minima basins at sublevel t).
β₁ = number of loops (saddle-mediated connections).
β₂ = number of voids (enclosed regions).")
  (diagram-betti pd dim time))

(define (count-basins pd threshold)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Number Nat))
  (doc 'description "Count significant basins (local minima) in the landscape.
Uses persistence threshold to filter noise: only features with
persistence > threshold are counted as significant.")
  (doc 'param 'threshold "Minimum persistence to be considered significant")
  (let ([h0-points (diagram-points-dim pd 0)])
    (length (filter (lambda (pt)
                      (let ([birth (car pt)]
                            [death (cadr pt)])
                        (or (infinite? death)
                            (> (- death birth) threshold))))
                    h0-points))))

(define (count-saddles pd threshold)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Number Nat))
  (doc 'description "Count significant saddle structures (1-dimensional features).
These represent saddle points connecting different basins.")
  (doc 'param 'threshold "Minimum persistence to be considered significant")
  (let ([h1-points (diagram-points-dim pd 1)])
    (length (filter (lambda (pt)
                      (let ([birth (car pt)]
                            [death (cadr pt)])
                        (or (infinite? death)
                            (> (- death birth) threshold))))
                    h1-points))))

(define (landscape-signature pd)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram LandscapeSignature))
  (doc 'description "Extract a topological signature of the landscape.
Returns an alist of features useful for comparing landscapes.")
  (let* ([h0 (diagram-points-dim pd 0)]
         [h1 (diagram-points-dim pd 1)]
         [h0-finite (filter (lambda (pt) (not (infinite? (cadr pt)))) h0)]
         [h1-finite (filter (lambda (pt) (not (infinite? (cadr pt)))) h1)]
         [h0-essential (filter (lambda (pt) (infinite? (cadr pt))) h0)]
         [h1-essential (filter (lambda (pt) (infinite? (cadr pt))) h1)]
         [h0-persist (map (lambda (pt) (- (cadr pt) (car pt))) h0-finite)]
         [h1-persist (map (lambda (pt) (- (cadr pt) (car pt))) h1-finite)]
         [total-h0-persist (if (null? h0-persist) 0 (apply + h0-persist))]
         [total-h1-persist (if (null? h1-persist) 0 (apply + h1-persist))]
         [max-h0-persist (if (null? h0-persist) 0 (apply max h0-persist))]
         [max-h1-persist (if (null? h1-persist) 0 (apply max h1-persist))])
    (list
     (cons 'basins-total (length h0))
     (cons 'basins-essential (length h0-essential))
     (cons 'basins-finite (length h0-finite))
     (cons 'saddles-total (length h1))
     (cons 'saddles-essential (length h1-essential))
     (cons 'saddles-finite (length h1-finite))
     (cons 'total-basin-persistence total-h0-persist)
     (cons 'max-basin-persistence max-h0-persist)
     (cons 'total-saddle-persistence total-h1-persist)
     (cons 'max-saddle-persistence max-h1-persist))))

(define (landscape-complexity pd threshold)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram Number Number))
  (doc 'description "Scalar measure of landscape complexity.
Sum of all persistence values above threshold, normalized.
Higher values indicate more complex, multi-modal landscapes.")
  (let* ([all-points (diagram-points pd)]
         [finite (filter (lambda (pt) (not (infinite? (cadr pt)))) all-points)]
         [significant (filter (lambda (pt) (> (- (cadr pt) (car pt)) threshold)) finite)]
         [persistence-sum (apply + 0 (map (lambda (pt) (- (cadr pt) (car pt))) significant))])
    persistence-sum))

;;; ============================================================
;;; Landscape Comparison
;;; ============================================================

(doc 'section 'comparison)

(define (landscape-distance pd1 pd2 dim)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram PersistenceDiagram Nat Number))
  (doc 'description "Bottleneck distance between two landscape persistence diagrams.
Useful for comparing landscapes of different functions or the same
function at different stages of training/optimization.")
  (diagram-bottleneck pd1 pd2 dim))

(define (landscape-similar? pd1 pd2 dim tolerance)
  (doc 'export #t)
  (doc 'type '(-> PersistenceDiagram PersistenceDiagram Nat Number Boolean))
  (doc 'description "Test if two landscapes have similar topology in a given dimension.")
  (< (landscape-distance pd1 pd2 dim) tolerance))

;;; ============================================================
;;; Convenience: End-to-End Pipeline
;;; ============================================================

(doc 'section 'pipeline)

(define (analyze-landscape-2d f x-min x-max y-min y-max nx ny epsilon)
  (doc 'export #t)
  (doc 'type '(-> (-> Number Number Number) Number Number Number Number Nat Nat Number LandscapeSignature))
  (doc 'description "One-shot 2D landscape analysis. Samples function on grid,
computes persistence, returns topological signature.")
  (let* ([samples (sample-grid-2d f x-min x-max y-min y-max nx ny)]
         [pd (landscape-persistence samples 2 2 epsilon 1.0)]
         [sig (landscape-signature pd)])
    sig))

(define (analyze-landscape-nd f bounds ns epsilon)
  (doc 'export #t)
  (doc 'type '(-> (-> (List Number) Number) (List (List Number Number)) (List Nat) Number LandscapeSignature))
  (doc 'description "One-shot n-dimensional landscape analysis.")
  (let* ([dim (length bounds)]
         [samples (sample-grid-nd f bounds ns)]
         [pd (landscape-persistence samples dim (min dim 2) epsilon 1.0)]
         [sig (landscape-signature pd)])
    sig))

(define (trajectory-persistence trajectory-points f dim epsilon scale)
  (doc 'export #t)
  (doc 'type '(-> (List (List Number)) (-> (List Number) Number) Nat Number Number PersistenceDiagram))
  (doc 'description "Compute persistence diagram around an optimization trajectory.
Takes trajectory points (from optimizer), evaluates f at each point,
and builds the augmented-space persistence diagram.")
  (doc 'param 'trajectory-points "List of points visited during optimization")
  (doc 'param 'f "The objective function")
  (doc 'param 'dim "Spatial dimension")
  (doc 'param 'epsilon "Distance threshold for Rips complex")
  (doc 'param 'scale "Function value scaling factor")
  (let* ([samples (map (lambda (pt) (append pt (list (f pt)))) trajectory-points)]
         [pd (landscape-persistence samples dim (min dim 2) epsilon scale)])
    pd))
