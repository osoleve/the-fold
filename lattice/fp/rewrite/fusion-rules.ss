;;; core/fp/rewrite/fusion-rules.ss — Fusion Rewrite Rules
;;;
;;; Fusion rules for combining traversals into single passes.
;;; These rules eliminate intermediate data structures by fusing
;;; consecutive operations (deforestation/short-cut fusion).
;;;
;;; Key Fusion Categories:
;;;   - map-map fusion: Combine consecutive maps into single map
;;;   - filter-map fusion: Fuse filter with map into filter-map
;;;   - fold-map fusion: Absorb map into fold's combining function
;;;   - flatMap fusion: Fuse flatten/concat with map
;;;   - stream fusion: Fuse stream operations
;;;
;;; Cycle Prevention:
;;;   All rules are strictly forward-directed (unfused -> fused).
;;;   The fused form is canonical; no reverse rules exist.
;;;   This ensures termination and prevents oscillation.
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - core/base/prelude.ss
;;;   - core/fp/rewrite/rule.ss
;;;   - core/fp/rewrite/engine.ss
;;;   - core/fp/rewrite/laws.ss (for register-law!)

(load "core/base/prelude.ss")
(load "lattice/fp/rewrite/rule.ss")
(load "lattice/fp/rewrite/engine.ss")
(load "lattice/fp/rewrite/laws.ss")

;;; ====
;;; Map-Map Fusion
;;; ====

;;; map-map-fuse : map f (map g xs) = map (compose f g) xs
;;; Two consecutive maps fuse into one map with composed function.
;;; Eliminates intermediate list allocation.
(define map-map-fuse
  (make-rule 'map-map-fuse
             '(map (?f) (map (?g) (?xs)))
             '(map (compose (?f) (?g)) (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Filter-Map Fusion
;;; ====

;;; filter-map-fuse : map f (filter p xs) = filter-map p f xs
;;; Map over filtered list becomes single filter-map pass.
;;; filter-map applies f only to elements satisfying p.
(define filter-map-fuse
  (make-rule 'filter-map-fuse
             '(map (?f) (filter (?p) (?xs)))
             '(filter-map (?p) (?f) (?xs))
             'category 'fusion
             'direction 'forward))

;;; map-filter-fuse : filter p (map f xs) = filter-map (compose p f) f xs
;;; Filter after map: predicate must be composed with the mapping function.
;;; The composed predicate tests (f x), but we still apply f to passing elements.
(define map-filter-fuse
  (make-rule 'map-filter-fuse
             '(filter (?p) (map (?f) (?xs)))
             '(filter-map (compose (?p) (?f)) (?f) (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Fold-Map Fusion (foldl variants)
;;; ====

;;; fold-map-fuse : foldl f z (map g xs) = foldl (fn (acc x) (f acc (g x))) z xs
;;; Absorb map into the fold's combining function.
;;; Eliminates the intermediate mapped list.
(define fold-map-fuse
  (make-rule 'fold-map-fuse
             '(foldl (?f) (?z) (map (?g) (?xs)))
             '(foldl (fn (acc x) ((?f) acc ((?g) x))) (?z) (?xs))
             'category 'fusion
             'direction 'forward))

;;; fold-filter-fuse : foldl f z (filter p xs) = foldl (fn (acc x) (if (p x) (f acc x) acc)) z xs
;;; Absorb filter into the fold's combining function.
;;; Only accumulate when predicate passes.
(define fold-filter-fuse
  (make-rule 'fold-filter-fuse
             '(foldl (?f) (?z) (filter (?p) (?xs)))
             '(foldl (fn (acc x) (if ((?p) x) ((?f) acc x) acc)) (?z) (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Fold-Map Fusion (foldr variants)
;;; ====

;;; foldr-map-fuse : foldr f z (map g xs) = foldr (fn (x acc) (f (g x) acc)) z xs
;;; Same as foldl variant but for right folds.
;;; Note: foldr has (element, accumulator) argument order.
(define foldr-map-fuse
  (make-rule 'foldr-map-fuse
             '(foldr (?f) (?z) (map (?g) (?xs)))
             '(foldr (fn (x acc) ((?f) ((?g) x) acc)) (?z) (?xs))
             'category 'fusion
             'direction 'forward))

;;; foldr-filter-fuse : foldr f z (filter p xs) = foldr (fn (x acc) (if (p x) (f x acc) acc)) z xs
;;; Absorb filter into foldr's combining function.
(define foldr-filter-fuse
  (make-rule 'foldr-filter-fuse
             '(foldr (?f) (?z) (filter (?p) (?xs)))
             '(foldr (fn (x acc) (if ((?p) x) ((?f) x acc) acc)) (?z) (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; FlatMap / Concat-Map Fusion
;;; ====

;;; concat-map-fuse : flatten (map f xs) = flatMap f xs
;;; Also known as concatMap or bind for lists.
;;; Fuses list flattening with the preceding map.
(define concat-map-fuse
  (make-rule 'concat-map-fuse
             '(flatten (map (?f) (?xs)))
             '(flatMap (?f) (?xs))
             'category 'fusion
             'direction 'forward))

;;; append-map-fuse : append (map f xs) = flatMap f xs
;;; Variant using append* / apply append semantics.
(define append-map-fuse
  (make-rule 'append-map-fuse
             '(apply append (map (?f) (?xs)))
             '(flatMap (?f) (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Stream Fusion
;;; ====

;;; stream-map-map-fuse : stream-map f (stream-map g s) = stream-map (compose f g) s
;;; Lazy stream variant of map-map fusion.
(define stream-map-map-fuse
  (make-rule 'stream-map-map-fuse
             '(stream-map (?f) (stream-map (?g) (?s)))
             '(stream-map (compose (?f) (?g)) (?s))
             'category 'fusion
             'direction 'forward))

;;; stream-filter-map-fuse : stream-map f (stream-filter p s) = stream-filter-map p f s
;;; Stream variant of filter-map fusion.
(define stream-filter-map-fuse
  (make-rule 'stream-filter-map-fuse
             '(stream-map (?f) (stream-filter (?p) (?s)))
             '(stream-filter-map (?p) (?f) (?s))
             'category 'fusion
             'direction 'forward))

;;; stream-take-map-fuse : stream-take n (stream-map f s) = stream-map f (stream-take n s)
;;; Take commutes with map (take first, then transform).
;;; This reduces the number of function applications.
(define stream-take-map-fuse
  (make-rule 'stream-take-map-fuse
             '(stream-take (?n) (stream-map (?f) (?s)))
             '(stream-map (?f) (stream-take (?n) (?s)))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Additional List Fusion Rules
;;; ====

;;; take-map-fuse : take n (map f xs) = map f (take n xs)
;;; Same principle as stream variant but for strict lists.
(define take-map-fuse
  (make-rule 'take-map-fuse
             '(take (?n) (map (?f) (?xs)))
             '(map (?f) (take (?n) (?xs)))
             'category 'fusion
             'direction 'forward))

;;; reverse-map-fuse : reverse (map f xs) = map f (reverse xs)
;;; Reverse commutes with map.
(define reverse-map-fuse
  (make-rule 'reverse-map-fuse
             '(reverse (map (?f) (?xs)))
             '(map (?f) (reverse (?xs)))
             'category 'fusion
             'direction 'forward))

;;; length-map-elim : length (map f xs) = length xs
;;; Map does not change length; eliminate the map entirely.
(define length-map-elim
  (make-rule 'length-map-elim
             '(length (map (?f) (?xs)))
             '(length (?xs))
             'category 'fusion
             'direction 'forward))

;;; null-map-elim : null? (map f xs) = null? xs
;;; Map does not change emptiness; eliminate the map.
(define null-map-elim
  (make-rule 'null-map-elim
             '(null? (map (?f) (?xs)))
             '(null? (?xs))
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Compose Chain Fusion
;;; ====

;;; compose-assoc : compose (compose f g) h = compose f (compose g h)
;;; Right-associative composition is canonical (matches function application order).
(define compose-assoc
  (make-rule 'compose-assoc
             '(compose (compose (?f) (?g)) (?h))
             '(compose (?f) (compose (?g) (?h)))
             'category 'fusion
             'direction 'forward))

;;; compose-id-left : compose id f = f
;;; Left identity elimination.
(define compose-id-left
  (make-rule 'compose-id-left
             '(compose id (?f))
             '(?f)
             'category 'fusion
             'direction 'forward))

;;; compose-id-right : compose f id = f
;;; Right identity elimination.
(define compose-id-right
  (make-rule 'compose-id-right
             '(compose (?f) id)
             '(?f)
             'category 'fusion
             'direction 'forward))

;;; ====
;;; Initialize Fusion Laws
;;; ====

;;; init-fusion-laws! : -> void
;;; Register all fusion rules in the law registry.
(define (init-fusion-laws!)
  ;; Map-Map Fusion
  (register-law! map-map-fuse)
  
  ;; Filter-Map Fusion
  (register-law! filter-map-fuse)
  (register-law! map-filter-fuse)
  
  ;; Fold-Map Fusion (foldl)
  (register-law! fold-map-fuse)
  (register-law! fold-filter-fuse)
  
  ;; Fold-Map Fusion (foldr)
  (register-law! foldr-map-fuse)
  (register-law! foldr-filter-fuse)
  
  ;; FlatMap Fusion
  (register-law! concat-map-fuse)
  (register-law! append-map-fuse)
  
  ;; Stream Fusion
  (register-law! stream-map-map-fuse)
  (register-law! stream-filter-map-fuse)
  (register-law! stream-take-map-fuse)
  
  ;; Additional List Fusion
  (register-law! take-map-fuse)
  (register-law! reverse-map-fuse)
  (register-law! length-map-elim)
  (register-law! null-map-elim)
  
  ;; Compose Chain Fusion
  (register-law! compose-assoc)
  (register-law! compose-id-left)
  (register-law! compose-id-right))

;;; Initialize on load
(init-fusion-laws!)

;;; ====
;;; Convenience Strategies
;;; ====

;;; fusion-rules : -> (List Rule)
;;; Get all fusion rules.
(define (fusion-rules)
  (laws-by-category 'fusion))

;;; fusion-simplify : Strategy
;;; Apply fusion rules exhaustively using innermost strategy.
;;; Innermost ensures we fuse from the inside out, which is
;;; more likely to find all fusion opportunities.
(define fusion-simplify
  (innermost (rules->strategy (fusion-rules))))

;;; fusion-simplify-once : Strategy
;;; Apply fusion rules exactly once (for debugging/stepping).
(define fusion-simplify-once
  (bottomup (try (rules->strategy (fusion-rules)))))

;;; aggressive-fusion : Strategy
;;; Combines fusion with compose simplification for maximum effect.
;;; Runs multiple passes to handle newly exposed opportunities.
(define aggressive-fusion
  (repeat (seq fusion-simplify-once
               (try (rules->strategy (list compose-id-left compose-id-right))))))

;;; ====
;;; Fusion Analysis
;;; ====

;;; count-traversals : Expr -> Nat
;;; Count the number of list traversals in an expression.
;;; Useful for measuring fusion effectiveness.
(define (count-traversals expr)
  (define traversal-ops '(map filter foldl foldr flatten reverse
                          stream-map stream-filter stream-take))
  (cond
   [(null? expr) 0]
   [(not (pair? expr)) 0]
   [(memq (car expr) traversal-ops)
    (+ 1 (apply + (map count-traversals (cdr expr))))]
   [else
    (apply + (map count-traversals expr))]))

;;; fusion-potential : Expr -> (Values Nat Nat)
;;; Returns (before-count . after-count) showing traversal reduction.
(define (fusion-potential expr)
  (let* ([before (count-traversals expr)]
         [after-expr (fusion-simplify expr)]
         [after (count-traversals (or after-expr expr))])
        (cons before after)))
