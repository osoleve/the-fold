;;; fabric/stitches/dsl/quasi.ss — Quasiquotation and Syntax Templates
;;;
;;; Pattern-based code generation for DSLs and macros.
;;;
;;; Core Operations:
;;;   `expr           → (quasiquote expr)    ; quote with holes
;;;   ,expr           → (unquote expr)       ; fill hole with value
;;;   ,@expr          → (unquote-splicing expr) ; splice list
;;;
;;; The qq-expand function transforms quasiquoted forms into
;;; runtime list construction code:
;;;
;;;   `(a ,b c)     → (list 'a b 'c)
;;;   `(a ,@xs b)   → (append (list 'a) xs (list 'b))
;;;   `(a . ,b)     → (cons 'a b)
;;;
;;; Nested quasiquotes work correctly:
;;;   ``(,a ,,b)    → (list 'quasiquote (list (list 'unquote 'a) (list 'unquote b)))
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/combinators.ss (for Maybe)

(load "core/prelude.ss")
(load "core/fp/meta/combinators.ss")

;;; ============================================================
;;; Quasiquote Detection
;;; ============================================================

;;; quasiquote? : Any -> Boolean
(define (quasiquote? x)
  (and (pair? x)
       (eq? (car x) 'quasiquote)
       (pair? (cdr x))
       (null? (cddr x))))

;;; unquote? : Any -> Boolean
(define (unquote? x)
  (and (pair? x)
       (eq? (car x) 'unquote)
       (pair? (cdr x))
       (null? (cddr x))))

;;; unquote-splicing? : Any -> Boolean
(define (unquote-splicing? x)
  (and (pair? x)
       (eq? (car x) 'unquote-splicing)
       (pair? (cdr x))
       (null? (cddr x))))

;;; ============================================================
;;; Quasiquote Expansion
;;; ============================================================
;;;
;;; qq-expand transforms a quasiquoted expression into code that
;;; constructs the result at runtime.
;;;
;;; The depth parameter tracks nesting:
;;;   depth = 1: we're inside one quasiquote, unquotes are active
;;;   depth = 2: we're inside ``, unquotes are data
;;;   etc.

;;; qq-expand : Expr -> Expr
;;; Expand a (quasiquote expr) form.
(define (qq-expand expr)
  (if (quasiquote? expr)
      (qq-expand-depth (cadr expr) 1)
      (error 'qq-expand "expected quasiquote form" expr)))

;;; qq-expand-depth : Expr x Int -> Expr
;;; Expand expression at given quasiquote depth.
(define (qq-expand-depth expr depth)
  (cond
    ;; Unquote at depth 1 → return the expression
    [(and (unquote? expr) (= depth 1))
     (cadr expr)]

    ;; Unquote at deeper depth → reconstruct unquote, decrement depth
    [(and (unquote? expr) (> depth 1))
     `(list 'unquote ,(qq-expand-depth (cadr expr) (- depth 1)))]

    ;; Unquote-splicing at depth 1 → error (should be in list context)
    [(and (unquote-splicing? expr) (= depth 1))
     (error 'qq-expand "unquote-splicing not in list context" expr)]

    ;; Unquote-splicing at deeper depth → reconstruct
    [(and (unquote-splicing? expr) (> depth 1))
     `(list 'unquote-splicing ,(qq-expand-depth (cadr expr) (- depth 1)))]

    ;; Nested quasiquote → reconstruct quasiquote, increment depth
    [(quasiquote? expr)
     `(list 'quasiquote ,(qq-expand-depth (cadr expr) (+ depth 1)))]

    ;; Pair/list → handle elements with possible splicing
    [(pair? expr)
     (qq-expand-pair expr depth)]

    ;; Vector → expand elements (no splicing in vectors for simplicity)
    [(vector? expr)
     `(list->vector ,(qq-expand-depth (vector->list expr) depth))]

    ;; Atom → quote it
    [else
     `(quote ,expr)]))

;;; qq-expand-pair : Pair x Int -> Expr
;;; Expand a pair, handling unquote-splicing in list context.
(define (qq-expand-pair expr depth)
  (let ([head (car expr)]
        [tail (cdr expr)])
    (cond
      ;; (unquote e) in car position at depth 1
      [(and (unquote? head) (= depth 1))
       (if (null? tail)
           (cadr head)  ; just ,e → e
           `(cons ,(cadr head) ,(qq-expand-depth tail depth)))]

      ;; (unquote-splicing e) in car position at depth 1
      [(and (unquote-splicing? head) (= depth 1))
       (if (null? tail)
           (cadr head)  ; just ,@e → e
           `(append ,(cadr head) ,(qq-expand-depth tail depth)))]

      ;; Tail is (unquote e) at depth 1 → improper list with unquote in cdr
      [(and (unquote? tail) (= depth 1))
       `(cons ,(qq-expand-depth head depth) ,(cadr tail))]

      ;; Regular pair with null tail → (list ...)
      [(null? tail)
       `(list ,(qq-expand-depth head depth))]

      ;; Regular pair with pair tail → collect into list or cons
      [(pair? tail)
       (qq-expand-list expr depth)]

      ;; Improper list (a . b) where b is not null or pair
      [else
       `(cons ,(qq-expand-depth head depth) ,(qq-expand-depth tail depth))])))

;;; qq-expand-list : List x Int -> Expr
;;; Expand a proper list, optimizing for common cases.
(define (qq-expand-list lst depth)
  ;; Collect segments: either (list ...) or splice expressions
  (let ([segments (qq-collect-segments lst depth)])
    (cond
      ;; Single list segment → just (list ...)
      [(and (= (length segments) 1)
            (pair? (car segments))
            (eq? (caar segments) 'list))
       (car segments)]

      ;; Single splice → just the expression
      [(and (= (length segments) 1)
            (pair? (car segments))
            (eq? (caar segments) 'splice))
       (cadar segments)]

      ;; Multiple segments → use append
      [else
       (let ([args (map (lambda (seg)
                          (if (and (pair? seg) (eq? (car seg) 'splice))
                              (cadr seg)
                              seg))
                        segments)])
         `(append ,@args))])))

;;; qq-collect-segments : List x Int -> (List Segment)
;;; Collect list into segments for efficient construction.
;;; A segment is either (list elem ...) or (splice expr).
(define (qq-collect-segments lst depth)
  (let loop ([remaining lst] [current-list '()] [segments '()])
    (cond
      ;; End of list
      [(null? remaining)
       (if (null? current-list)
           (reverse segments)
           (reverse (cons `(list ,@(reverse current-list)) segments)))]

      ;; Improper list tail
      [(not (pair? remaining))
       ;; Flush current list, add tail as splice-like
       (let ([segs (if (null? current-list)
                       segments
                       (cons `(list ,@(reverse current-list)) segments))])
         (reverse (cons `(splice (list ,(qq-expand-depth remaining depth))) segs)))]

      ;; Unquote-splicing at depth 1 → start new segment
      [(and (unquote-splicing? (car remaining)) (= depth 1))
       (let ([segs (if (null? current-list)
                       segments
                       (cons `(list ,@(reverse current-list)) segments))])
         (loop (cdr remaining)
               '()
               (cons `(splice ,(cadar remaining)) segs)))]

      ;; Regular element → add to current list
      [else
       (loop (cdr remaining)
             (cons (qq-expand-depth (car remaining) depth) current-list)
             segments)])))

;;; ============================================================
;;; Convenience: expand-quasiquote
;;; ============================================================

;;; expand-quasiquote : Expr -> Expr
;;; Top-level entry point. If not a quasiquote, returns as-is.
(define (expand-quasiquote expr)
  (if (quasiquote? expr)
      (qq-expand expr)
      expr))

;;; ============================================================
;;; Syntax Objects (Source Location Tracking)
;;; ============================================================
;;;
;;; A syntax object wraps a datum with source location information.
;;; This enables error messages that point to the original DSL code.

;;; make-syntax : Datum x SourceLoc -> Syntax
(define (make-syntax datum source-loc)
  (list 'syntax datum source-loc))

;;; syntax? : Any -> Boolean
(define (syntax? x)
  (and (pair? x)
       (eq? (car x) 'syntax)
       (= (length x) 3)))

;;; syntax-datum : Syntax -> Datum
(define (syntax-datum stx)
  (cadr stx))

;;; syntax-source : Syntax -> SourceLoc
(define (syntax-source stx)
  (caddr stx))

;;; make-source-loc : String x Int x Int -> SourceLoc
(define (make-source-loc file line column)
  (list 'source-loc file line column))

;;; source-loc? : Any -> Boolean
(define (source-loc? x)
  (and (pair? x)
       (eq? (car x) 'source-loc)
       (= (length x) 4)))

;;; source-loc-file : SourceLoc -> String
(define (source-loc-file loc)
  (cadr loc))

;;; source-loc-line : SourceLoc -> Int
(define (source-loc-line loc)
  (caddr loc))

;;; source-loc-column : SourceLoc -> Int
(define (source-loc-column loc)
  (cadddr loc))

;;; no-source : SourceLoc
;;; A placeholder for unknown source locations.
(define no-source
  (make-source-loc "<unknown>" 0 0))

;;; datum->syntax : Datum x Syntax -> Syntax
;;; Wrap a datum with the source location from another syntax object.
(define (datum->syntax datum stx)
  (make-syntax datum (if (syntax? stx) (syntax-source stx) no-source)))

;;; syntax->datum : Syntax -> Datum
;;; Strip syntax wrapper, recursively.
(define (syntax->datum stx)
  (cond
    [(syntax? stx) (syntax->datum (syntax-datum stx))]
    [(pair? stx) (cons (syntax->datum (car stx))
                       (syntax->datum (cdr stx)))]
    [(vector? stx) (list->vector (map syntax->datum (vector->list stx)))]
    [else stx]))

;;; ============================================================
;;; Syntax Templates with Source Tracking
;;; ============================================================
;;;
;;; qq-expand-syntax: like qq-expand but preserves source locations.

;;; qq-expand-syntax : Syntax -> Expr
;;; Expand a quasiquoted syntax object, preserving locations.
(define (qq-expand-syntax stx)
  (let ([datum (if (syntax? stx) (syntax-datum stx) stx)]
        [loc (if (syntax? stx) (syntax-source stx) no-source)])
    (if (quasiquote? datum)
        (qq-expand-syntax-depth (cadr datum) 1 loc)
        (error 'qq-expand-syntax "expected quasiquote form" datum))))

;;; qq-expand-syntax-depth : Expr x Int x SourceLoc -> Expr
(define (qq-expand-syntax-depth expr depth loc)
  ;; For now, delegate to qq-expand-depth
  ;; Full implementation would track locations through expansion
  (qq-expand-depth (if (syntax? expr) (syntax-datum expr) expr) depth))

;;; ============================================================
;;; Pattern Matching on Syntax (syntax-case style)
;;; ============================================================
;;;
;;; syntax-match provides pattern matching on syntax objects.
;;; Patterns can include:
;;;   - literals: match exactly
;;;   - pattern variables: bind to matched syntax
;;;   - (pattern ...) : match zero or more
;;;   - _ : wildcard

;;; syntax-match : Syntax x Pattern -> (Maybe Bindings)
;;; Match syntax against pattern, returning bindings or nothing.
(define (syntax-match stx pattern)
  (let ([datum (if (syntax? stx) (syntax-datum stx) stx)])
    (pattern-match datum pattern '())))

;;; pattern-match : Datum x Pattern x Bindings -> (Maybe Bindings)
(define (pattern-match datum pattern bindings)
  (cond
    ;; Wildcard matches anything
    [(eq? pattern '_)
     (just bindings)]

    ;; Pattern variable (symbols not in literals)
    [(and (symbol? pattern) (not (literal-pattern? pattern)))
     (just (cons (cons pattern datum) bindings))]

    ;; Literal match
    [(literal-pattern? pattern)
     (if (equal? datum pattern)
         (just bindings)
         nothing)]

    ;; List pattern with ellipsis
    [(and (pair? pattern)
          (pair? (cdr pattern))
          (eq? (cadr pattern) '...))
     (pattern-match-ellipsis datum (car pattern) (cddr pattern) bindings)]

    ;; Regular list pattern
    [(pair? pattern)
     (if (pair? datum)
         (maybe-bind (pattern-match (car datum) (car pattern) bindings)
                     (lambda (bindings1)
                       (pattern-match (cdr datum) (cdr pattern) bindings1)))
         nothing)]

    ;; Atom pattern
    [(equal? datum pattern)
     (just bindings)]

    [else nothing]))

;;; literal-pattern? : Any -> Boolean
;;; Check if pattern should be matched literally.
(define (literal-pattern? x)
  (or (number? x)
      (string? x)
      (boolean? x)
      (null? x)
      (memq x '(quote quasiquote unquote unquote-splicing))))

;;; pattern-match-ellipsis : Datum x Pattern x Pattern x Bindings -> (Maybe Bindings)
;;; Match pattern with ... (zero or more).
(define (pattern-match-ellipsis datum elem-pattern rest-pattern bindings)
  ;; Try to match as many elements as possible with elem-pattern,
  ;; then match remainder with rest-pattern
  (let loop ([items datum] [matches '()])
    (cond
      ;; Try matching rest
      [(let ([rest-result (pattern-match items rest-pattern bindings)])
         (and (just? rest-result)
              ;; Found valid split
              (just (merge-ellipsis-bindings elem-pattern (reverse matches)
                                             (from-just rest-result)))))]

      ;; Can't match rest, try consuming one more element
      [(and (pair? items)
            (just? (pattern-match (car items) elem-pattern '())))
       (loop (cdr items) (cons (car items) matches))]

      ;; Can't match at all
      [else nothing])))

;;; merge-ellipsis-bindings : Pattern x (List Datum) x Bindings -> Bindings
;;; Add ellipsis-matched items to bindings (as lists).
(define (merge-ellipsis-bindings pattern matches bindings)
  (if (symbol? pattern)
      (cons (cons pattern matches) bindings)
      ;; For complex patterns, would need to extract and merge
      bindings))

;;; maybe-bind : (Maybe a) x (a -> Maybe b) -> (Maybe b)
(define (maybe-bind m f)
  (if (just? m)
      (f (from-just m))
      nothing))

;;; ============================================================
;;; Utility: Template Instantiation
;;; ============================================================

;;; instantiate-template : Template x Bindings -> Expr
;;; Fill in a template with bindings from pattern match.
(define (instantiate-template template bindings)
  (cond
    [(symbol? template)
     (let ([binding (assq template bindings)])
       (if binding
           (cdr binding)
           template))]

    [(pair? template)
     (cons (instantiate-template (car template) bindings)
           (instantiate-template (cdr template) bindings))]

    [else template]))
