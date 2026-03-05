;;; @module template-zipper
;;; @requires dsl/template rewrite/sexp-zipper
(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
(require 'dsl/template)
(require 'rewrite/sexp-zipper)

(doc 'module 'template-zipper)
(doc 'description "Hole-Focused Navigation for Templates - Provides zipper-based navigation between holes in template expressions")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'note "Key features:
  - Focus on \"current hole\" being filled
  - Navigate between holes in document order (preorder)
  - Track position (\"hole 2 of 5\")
  - Fill current hole and automatically move to next
  - Undo support via hole history")

(doc 'section 'template-hole-zipper-type)
;;;
;;; A hole-zipper tracks:
;;;   - The underlying sexp-zipper for navigation
;;;   - All hole positions in document order (cached)
;;;   - Current hole index
;;;   - Fill history for undo

;;; hole-zipper-tag : Symbol
(define hole-zipper-tag 'hole-zipper)

;;; make-hole-zipper : SexpZipper x (List Position) x Nat x (List HoleZipper) -> HoleZipper
(define (make-hole-zipper sexp-z hole-positions current-idx history)
  (doc 'export #t)
  (list hole-zipper-tag sexp-z hole-positions current-idx history))

;;; hole-zipper? : a -> Bool
(define (hole-zipper? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? (car x) hole-zipper-tag)))

;;; hole-zipper-sexp-z : HoleZipper -> SexpZipper
(define (hole-zipper-sexp-z hz)
  (doc 'export #t)
  (list-ref hz 1))

;;; hole-zipper-positions : HoleZipper -> (List Position)
(define (hole-zipper-positions hz)
  (doc 'export #t)
  (list-ref hz 2))

;;; hole-zipper-index : HoleZipper -> Nat
(define (hole-zipper-index hz)
  (doc 'export #t)
  (list-ref hz 3))

;;; hole-zipper-history : HoleZipper -> (List HoleZipper)
(define (hole-zipper-history hz)
  (doc 'export #t)
  (list-ref hz 4))

(doc 'section 'hole-detection)

;;; find-hole-positions : Sexp -> (List Position)
;;; Find all positions containing holes, in preorder (document order).
(define (find-hole-positions expr)
  (doc 'export #t)
  (let* ([sz (sexp->zipper expr)]
         [all-zippers (sexp-zipper-preorder sz)])
    (filter-map-idx
     (lambda (z)
       (let ([val (sexp-zipper-get z)])
         (if (hole? val)
             (sexp-zipper-position z)
             #f)))
     all-zippers)))

;;; filter-map-idx : (a -> b | #f) x (List a) -> (List b)
;;; Filter and map, keeping only non-#f results.
(define (filter-map-idx f lst)
  (doc 'export #t)
  (let loop ([lst lst] [acc '()])
    (if (null? lst)
        (reverse acc)
        (let ([result (f (car lst))])
          (if result
              (loop (cdr lst) (cons result acc))
              (loop (cdr lst) acc))))))

(doc 'section 'constructors)

;;; template->hole-zipper : Template -> HoleZipper | #f
;;; Create a hole-zipper from a template.
;;; Returns #f if template has no holes.
(define (template->hole-zipper t)
  (doc 'export #t)
  (let* ([expr (template-expr t)]
         [positions (find-hole-positions expr)])
    (if (null? positions)
        #f
        (let ([sz (sexp->zipper expr)])
          (make-hole-zipper sz positions 0 '())))))

;;; expr->hole-zipper : Expr -> HoleZipper | #f
;;; Create a hole-zipper directly from an expression.
(define (expr->hole-zipper expr)
  (doc 'export #t)
  (template->hole-zipper (new-template expr)))

;;; hole-zipper->template : HoleZipper -> Template
;;; Convert hole-zipper back to a template.
(define (hole-zipper->template hz)
  (doc 'export #t)
  (new-template (zipper->sexp (sexp-zipper-root (hole-zipper-sexp-z hz)))))

;;; hole-zipper->expr : HoleZipper -> Expr
;;; Get the current expression from hole-zipper.
(define (hole-zipper->expr hz)
  (doc 'export #t)
  (zipper->sexp (sexp-zipper-root (hole-zipper-sexp-z hz))))

(doc 'section 'focus-operations)

;;; hole-zipper-current : HoleZipper -> Symbol | #f
;;; Get the current hole symbol, or #f if no holes.
(define (hole-zipper-current hz)
  (doc 'export #t)
  (let ([positions (hole-zipper-positions hz)]
        [idx (hole-zipper-index hz)])
    (if (or (null? positions) (>= idx (length positions)))
        #f
        (let* ([pos (list-ref positions idx)]
               [root-z (sexp-zipper-root (hole-zipper-sexp-z hz))]
               [at-pos (sexp-zipper-goto root-z pos)])
          (if (nothing? at-pos)
              #f
              (sexp-zipper-get (from-just at-pos)))))))

;;; hole-zipper-current-position : HoleZipper -> Position | #f
;;; Get the position of the current hole.
(define (hole-zipper-current-position hz)
  (doc 'export #t)
  (let ([positions (hole-zipper-positions hz)]
        [idx (hole-zipper-index hz)])
    (if (or (null? positions) (>= idx (length positions)))
        #f
        (list-ref positions idx))))

(doc 'section 'position-information)

;;; hole-zipper-count : HoleZipper -> Nat
;;; Total number of holes.
(define (hole-zipper-count hz)
  (doc 'export #t)
  (length (hole-zipper-positions hz)))

;;; hole-zipper-position-string : HoleZipper -> String
;;; Get position string like "hole 2 of 5".
(define (hole-zipper-position-string hz)
  (doc 'export #t)
  (let ([count (hole-zipper-count hz)]
        [idx (hole-zipper-index hz)])
    (if (= count 0)
        "no holes"
        (string-append "hole "
                       (number->string (+ idx 1))
                       " of "
                       (number->string count)))))

;;; hole-zipper-complete? : HoleZipper -> Bool
;;; True if all holes have been filled.
(define (hole-zipper-complete? hz)
  (doc 'export #t)
  (= (hole-zipper-count hz) 0))

;;; hole-zipper-at-last? : HoleZipper -> Bool
;;; True if at the last hole.
(define (hole-zipper-at-last? hz)
  (doc 'export #t)
  (>= (+ (hole-zipper-index hz) 1) (hole-zipper-count hz)))

;;; hole-zipper-at-first? : HoleZipper -> Bool
;;; True if at the first hole.
(define (hole-zipper-at-first? hz)
  (doc 'export #t)
  (= (hole-zipper-index hz) 0))

(doc 'section 'navigation)

;;; hole-zipper-next : HoleZipper -> HoleZipper | #f
;;; Move to the next hole. Returns #f if at last hole.
(define (hole-zipper-next hz)
  (doc 'export #t)
  (let ([idx (hole-zipper-index hz)]
        [count (hole-zipper-count hz)])
    (if (>= (+ idx 1) count)
        #f
        (make-hole-zipper
         (hole-zipper-sexp-z hz)
         (hole-zipper-positions hz)
         (+ idx 1)
         (hole-zipper-history hz)))))

;;; hole-zipper-prev : HoleZipper -> HoleZipper | #f
;;; Move to the previous hole. Returns #f if at first hole.
(define (hole-zipper-prev hz)
  (doc 'export #t)
  (let ([idx (hole-zipper-index hz)])
    (if (= idx 0)
        #f
        (make-hole-zipper
         (hole-zipper-sexp-z hz)
         (hole-zipper-positions hz)
         (- idx 1)
         (hole-zipper-history hz)))))

;;; hole-zipper-goto-index : HoleZipper x Nat -> HoleZipper | #f
;;; Go to hole at specific index. Returns #f if out of bounds.
(define (hole-zipper-goto-index hz idx)
  (doc 'export #t)
  (if (or (< idx 0) (>= idx (hole-zipper-count hz)))
      #f
      (make-hole-zipper
       (hole-zipper-sexp-z hz)
       (hole-zipper-positions hz)
       idx
       (hole-zipper-history hz))))

;;; hole-zipper-first : HoleZipper -> HoleZipper
;;; Go to the first hole.
(define (hole-zipper-first hz)
  (doc 'export #t)
  (make-hole-zipper
   (hole-zipper-sexp-z hz)
   (hole-zipper-positions hz)
   0
   (hole-zipper-history hz)))

;;; hole-zipper-last : HoleZipper -> HoleZipper
;;; Go to the last hole.
(define (hole-zipper-last hz)
  (doc 'export #t)
  (let ([count (hole-zipper-count hz)])
    (make-hole-zipper
     (hole-zipper-sexp-z hz)
     (hole-zipper-positions hz)
     (max 0 (- count 1))
     (hole-zipper-history hz))))

(doc 'section 'filling-holes)

;;; hole-zipper-fill : HoleZipper x Expr -> HoleZipper
;;; Fill the current hole with a value.
;;; - Substitutes the hole with the value
;;; - Recomputes hole positions (value may contain new holes)
;;; - Moves to first new hole in filled region, or stays at same index
;;; - Saves current state to history for undo
(define (hole-zipper-fill hz value)
  (doc 'export #t)
  (let* ([current-hole (hole-zipper-current hz)]
         [expr (hole-zipper->expr hz)]
         ;; Substitute the hole
         [new-expr (subst-hole expr current-hole value)]
         ;; Recompute hole positions
         [new-positions (find-hole-positions new-expr)]
         [new-sexp-z (sexp->zipper new-expr)]
         ;; Determine new index
         [old-idx (hole-zipper-index hz)]
         [new-idx (min old-idx (max 0 (- (length new-positions) 1)))]
         ;; Save to history (full current state)
         [new-history (cons hz (hole-zipper-history hz))])
    (make-hole-zipper new-sexp-z new-positions new-idx new-history)))

;;; hole-zipper-fill-current : HoleZipper x Symbol x Expr -> HoleZipper
;;; Fill a specific hole by name (must match current hole).
;;; Errors if hole doesn't match.
(define (hole-zipper-fill-current hz hole-sym value)
  (doc 'export #t)
  (let ([current (hole-zipper-current hz)])
    (unless (eq? current hole-sym)
      (error 'hole-zipper-fill-current
             (string-append "Current hole is " (symbol->string current)
                           ", not " (symbol->string hole-sym))))
    (hole-zipper-fill hz value)))

;;; hole-zipper-fill-by-name : HoleZipper x Symbol x Expr -> HoleZipper
;;; Fill a hole by name anywhere in the template.
;;; Uses the underlying template's subst-hole.
(define (hole-zipper-fill-by-name hz hole-sym value)
  (doc 'export #t)
  (let* ([expr (hole-zipper->expr hz)]
         [new-expr (subst-hole expr hole-sym value)]
         [new-positions (find-hole-positions new-expr)]
         [new-sexp-z (sexp->zipper new-expr)]
         ;; Adjust index if needed
         [old-idx (hole-zipper-index hz)]
         [new-idx (min old-idx (max 0 (- (length new-positions) 1)))]
         [new-history (cons hz (hole-zipper-history hz))])
    (make-hole-zipper new-sexp-z new-positions new-idx new-history)))

(doc 'section 'undo)

;;; hole-zipper-undo : HoleZipper -> HoleZipper | #f
;;; Undo the last fill operation. Returns #f if nothing to undo.
(define (hole-zipper-undo hz)
  (doc 'export #t)
  (let ([history (hole-zipper-history hz)])
    (if (null? history)
        #f
        (car history))))

;;; hole-zipper-can-undo? : HoleZipper -> Bool
(define (hole-zipper-can-undo? hz)
  (doc 'export #t)
  (not (null? (hole-zipper-history hz))))

;;; hole-zipper-undo-count : HoleZipper -> Nat
;;; Number of undo steps available.
(define (hole-zipper-undo-count hz)
  (doc 'export #t)
  (length (hole-zipper-history hz)))

(doc 'section 'context-information)

;;; hole-zipper-context : HoleZipper -> (List Sexp)
;;; Get the path of parent expressions leading to current hole.
;;; Useful for showing context: "Filling $body in (define (foo x) ...)"
(define (hole-zipper-context hz)
  (doc 'export #t)
  (let* ([pos (hole-zipper-current-position hz)]
         [root-z (sexp-zipper-root (hole-zipper-sexp-z hz))]
         [at-pos (if pos (sexp-zipper-goto root-z pos) nothing)])
    (if (nothing? at-pos)
        '()
        ;; Walk up collecting parent expressions
        (let loop ([z (from-just at-pos)] [acc '()])
          (let ([up (sexp-zipper-up z)])
            (if (nothing? up)
                (reverse acc)
                (loop (from-just up)
                      (cons (sexp-zipper-get (from-just up)) acc))))))))

;;; hole-zipper-immediate-context : HoleZipper -> Sexp | #f
;;; Get the immediate parent expression containing the current hole.
(define (hole-zipper-immediate-context hz)
  (doc 'export #t)
  (let ([ctx (hole-zipper-context hz)])
    (if (null? ctx)
        #f
        (car ctx))))

(doc 'section 'all-holes)

;;; hole-zipper-all-holes : HoleZipper -> (List Symbol)
;;; Get all remaining holes in document order.
(define (hole-zipper-all-holes hz)
  (doc 'export #t)
  (let ([expr (hole-zipper->expr hz)])
    (find-holes expr)))

;;; hole-zipper-remaining-holes : HoleZipper -> (List Symbol)
;;; Get holes from current position onwards.
(define (hole-zipper-remaining-holes hz)
  (doc 'export #t)
  (let* ([all-positions (hole-zipper-positions hz)]
         [idx (hole-zipper-index hz)]
         [remaining-positions (list-tail-safe all-positions idx)]
         [root-z (sexp-zipper-root (hole-zipper-sexp-z hz))])
    (filter-map-idx
     (lambda (pos)
       (let ([at-pos (sexp-zipper-goto root-z pos)])
         (if (nothing? at-pos)
             #f
             (sexp-zipper-get (from-just at-pos)))))
     remaining-positions)))

;;; list-tail-safe : (List a) x Nat -> (List a)
;;; Safe list-tail that returns empty list if n >= length.
(define (list-tail-safe lst n)
  (doc 'export #t)
  (cond
    [(null? lst) '()]
    [(= n 0) lst]
    [else (list-tail-safe (cdr lst) (- n 1))]))

(doc 'section 'status-and-display)

;;; hole-zipper-status : HoleZipper -> String
;;; Get a status string describing current state.
(define (hole-zipper-status hz)
  (doc 'export #t)
  (let ([count (hole-zipper-count hz)]
        [current (hole-zipper-current hz)]
        [pos-str (hole-zipper-position-string hz)])
    (if (= count 0)
        "Complete!"
        (string-append pos-str
                       ": "
                       (if current (symbol->string current) "???")))))

;;; hole-zipper-summary : HoleZipper -> String
;;; Get a summary including remaining holes.
(define (hole-zipper-summary hz)
  (doc 'export #t)
  (let ([status (hole-zipper-status hz)]
        [remaining (hole-zipper-remaining-holes hz)])
    (if (null? remaining)
        status
        (string-append status
                       " (remaining: "
                       (holes->string remaining)
                       ")"))))
