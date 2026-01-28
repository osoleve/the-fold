# Automatic Bifurcation Diagram Generation

**Issue:** fold-zxuh
**Date:** 2026-01-28
**Status:** Design complete, ready for implementation

## Problem

Manual bifurcation analysis requires:
1. Running continuation from a starting point
2. Manually inspecting for bifurcations
3. Switching branches at each bifurcation
4. Repeating until the diagram is complete

This is tedious and error-prone for complex systems.

## Solution

Automatic tracer that explores the full bifurcation diagram from a starting point.

### Behavior
- Starts from a known fixed point
- Continues until hitting a bifurcation
- At bifurcations, spawns continuation on ALL new branches
- Stops when parameter exits `[p_min, p_max]` OR after max steps per branch
- Returns flat list with branch IDs for easy analysis

## Data Structures

### Bifurcation Diagram

```scheme
(make-bifurcation-diagram
  branches: '((branch-id . continuation-data) ...)
  bifurcations: '((bif-type param fp parent-branch child-branches) ...)
  metadata: '((param-range . (p-min . p-max))
              (total-steps . n)
              (branch-count . m) ...))
```

Where:
- `branch-id`: Symbol like `'branch-0`, `'branch-1`, etc.
- `continuation-data`: `((param fp stability eigenvalues) ...)` - standard format
- `child-branches`: `((branch-id . direction) ...)` - includes direction (upper/lower)

### Work Item (internal)

```scheme
(make-work-item
  id: branch-id
  param: starting-parameter
  fp: starting-fixed-point
  direction: 'forward | 'backward)
```

### Spatial Hash (internal)

Grid-based collision detection for finding branch merges:
- Cell size: `δp = (p_max - p_min) / 100`, `δx` based on typical spacing
- Maps `(cell-i, cell-j)` → `(branch-id point-index)`

## Algorithm

```scheme
(define (trace-bifurcation-diagram psys start-param start-fp p-min p-max max-steps-per-branch)
  ;; 1. Initialize
  (let ([diagram (make-empty-diagram p-min p-max)]
        [spatial-hash (make-spatial-hash grid-size)]
        [queue (list (make-work-item 'branch-0 start-param start-fp 'forward))])

    ;; 2. Process queue
    (let loop ([q queue] [diag diagram])
      (if (null? q)
          diag
          (let* ([item (car q)]
                 [branch-id (work-item-id item)]
                 [p-start (work-item-param item)]
                 [fp-start (work-item-fp item)]
                 [dir (work-item-direction item)])

            ;; 3. Continue this branch
            (let* ([cont-data (continue-fixed-point-arclength-adaptive
                               psys p-start fp-start max-steps-per-branch
                               (if (eq? dir 'forward) 0.01 -0.01))]
                   [filtered (filter-within-bounds cont-data p-min p-max)]
                   [bifs (detect-bifurcations filtered psys)])

              ;; 4. Register branch and points in spatial hash
              (add-branch! diag branch-id filtered)
              (register-points! spatial-hash filtered)

              ;; 5. Spawn child branches at bifurcations
              (let ([new-items (spawn-branches psys bifs branch-id p-min p-max spatial-hash)])
                (loop (append (cdr q) new-items)
                      diag))))))))
```

### Branch Spawning

**Critical:** `switch-branch-adaptive` returns `(new-fp . new-param)` where `new-param` is offset from the bifurcation. Must start continuation from that offset point.

```scheme
(define (spawn-branches psys bifs parent-id p-min p-max spatial-hash)
  (apply append
    (map (lambda (bif)
           (let* ([bif-type (car bif)]
                  [bif-param (cadr bif)]
                  [bif-fp (caddr bif)])
             ;; Try both directions
             (filter-map
               (lambda (dir)
                 (let ([switched (switch-branch-adaptive psys bif-param bif-fp dir)])
                   (and switched
                        (let ([new-param (cdr switched)]
                              [new-fp (car switched)])
                          ;; Check bounds
                          (and (<= p-min new-param p-max)
                               ;; Check not already traced
                               (not (spatial-hash-occupied? spatial-hash new-param new-fp))
                               ;; Create work item
                               (make-work-item (gensym 'branch-) new-param new-fp
                                              (if (> new-param bif-param) 'forward 'backward)))))))
               '(upper lower))))
         bifs)))
```

## Termination Conditions

| Condition | Status Symbol | Meaning |
|-----------|---------------|---------|
| Parameter exits bounds | `'boundary` | Normal termination |
| Max steps reached | `'max-steps` | Budget exhausted |
| Continuation fails | `'failed` | Newton didn't converge |
| Step size collapses | `'stalled` | Numerical difficulty |
| Hits existing branch | `'merged` | Branch reconnection |

## Public API

```scheme
;; Main entry point
(define (trace-bifurcation-diagram psys start-param start-fp p-min p-max . opts)
  ...)

;; Accessors
(define (diagram-branches diag) ...)
(define (diagram-branch diag id) ...)
(define (diagram-bifurcations diag) ...)
(define (diagram-bifurcations-of-type diag type) ...)

;; Queries
(define (diagram-stable-points diag) ...)
(define (diagram-unstable-points diag) ...)
(define (diagram-points-at-param diag p tol) ...)

;; Statistics
(define (diagram-summary diag) ...)
```

## Edge Cases

### Bifurcations near bounds
- Check if `bif-param ± switch-step` within bounds before spawning
- Record bifurcation with `child-branches: '()` if skipped

### Branch merging
- Use spatial hash to detect when continuation lands on existing branch
- Record merge point for topology reconstruction

### Domain errors
- Wrap continuation in exception handler
- Mark branch as `'failed` with error info in metadata

### Isolas (isolated loops)
- V1: Not handled (require global search)
- Only handle reconnections within connected component

## Testing

1. **Pitchfork normal form** - Should find 3 branches meeting at r=0
2. **Transcritical normal form** - Should find 2 branches exchanging stability
3. **Saddle-node normal form** - Should traverse fold via arc-length
4. **Lorenz system** - Stress test on real 3D system
5. **Boundary conditions** - Verify proper termination at p_min/p_max

## References

- AUTO-07p documentation (Doedel et al.)
- MATCONT continuation package (Dhooge et al.)
- Kuznetsov, "Elements of Applied Bifurcation Theory"
