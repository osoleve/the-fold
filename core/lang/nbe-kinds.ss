;;; @module nbe-kinds
;;; @description Kind NbE — kind normalization, equivalence checking, dependent kinds (K-pi).
;;; Loaded by nbe.ss — requires core NbE values and evaluation infrastructure.

;;; ====
;;; Kind NbE - Phase 2: Kind Normalization
;;; ====
;;;
;;; Extends NbE to handle kinds. This enables:
;;;   - Normalizing kind expressions
;;;   - Checking kind equivalence
;;;   - Handling dependent kinds (K-pi)
;;;
;;; Kind semantic values mirror type semantic values:
;;;   KV-star    : The kind * as a value
;;;   KV-arrow   : Kind arrow as a closure
;;;   KV-pi      : Dependent kind Πκ as a closure
;;;   KV-sort    : Sort □ (kind of kinds) as a value
;;;   KV-neutral : Stuck kind computation

;;; ====
;;; Kind Semantic Values
;;; ====

;;; KV-star : → KindValue
;;; The base kind * as a semantic value.
(define (KV-star)
  '(KV-star))

(define (KV-star? v)
  (and (pair? v) (eq? (car v) 'KV-star)))

;;; KV-constraint : → KindValue
;;; The Constraint kind as a semantic value.
(define (KV-constraint)
  '(KV-constraint))

(define (KV-constraint? v)
  (and (pair? v) (eq? (car v) 'KV-constraint)))

;;; KV-row : → KindValue
;;; The Row kind as a semantic value.
(define (KV-row)
  '(KV-row))

(define (KV-row? v)
  (and (pair? v) (eq? (car v) 'KV-row)))

;;; KV-arrow : KindValue × KindClosure → KindValue
;;; A kind arrow ⇒. The codomain is a closure to handle potential
;;; dependency (though standard arrows are non-dependent).
(define (KV-arrow domain codomain-clo)
  `(KV-arrow ,domain ,codomain-clo))

(define (KV-arrow? v)
  (and (pair? v) (eq? (car v) 'KV-arrow)))

(define (KV-arrow-domain v) (cadr v))
(define (KV-arrow-codomain-clo v) (caddr v))

;;; KV-pi : KindValue × KindClosure → KindValue
;;; A dependent kind Πκ. The codomain depends on the input.
(define (KV-pi domain codomain-clo)
  `(KV-pi ,domain ,codomain-clo))

(define (KV-pi? v)
  (and (pair? v) (eq? (car v) 'KV-pi)))

(define (KV-pi-domain v) (cadr v))
(define (KV-pi-codomain-clo v) (caddr v))

;;; KV-sort : Nat → KindValue
;;; The sort □ (kind of kinds) at a given level.
(define (KV-sort level)
  `(KV-sort ,level))

(define (KV-sort? v)
  (and (pair? v) (eq? (car v) 'KV-sort)))

(define (KV-sort-level v) (cadr v))

;;; KV-neutral : KindNeutral → KindValue
;;; A stuck kind computation.
(define (KV-neutral neutral)
  `(KV-neutral ,neutral))

(define (KV-neutral? v)
  (and (pair? v) (eq? (car v) 'KV-neutral)))

(define (KV-neutral-term v) (cadr v))

;;; ====
;;; Kind Neutral Values
;;; ====

;;; KN-var : Level → KindNeutral
;;; A kind variable (stuck).
(define (KN-var level)
  `(KN-var ,level))

(define (KN-var? n)
  (and (pair? n) (eq? (car n) 'KN-var)))

(define (KN-var-level n) (cadr n))

;;; KN-app : KindNeutral × KindValue → KindNeutral
;;; Application of a neutral kind to a value.
(define (KN-app func arg)
  `(KN-app ,func ,arg))

(define (KN-app? n)
  (and (pair? n) (eq? (car n) 'KN-app)))

;;; ====
;;; Kind Closures
;;; ====

;;; make-kind-closure : Symbol × KindExpr × KindEnv → KindClosure
(define (make-kind-closure param body env)
  `(kind-closure ,param ,body ,env))

(define (kind-closure? c)
  (and (pair? c) (eq? (car c) 'kind-closure)))

(define (kind-closure-param c) (cadr c))
(define (kind-closure-body c) (caddr c))
(define (kind-closure-env c) (cadddr c))

;;; make-kind-const-closure : KindValue → KindClosure
;;; A constant closure that ignores its argument.
(define (make-kind-const-closure value)
  `(kind-const-closure ,value))

(define (kind-const-closure? c)
  (and (pair? c) (eq? (car c) 'kind-const-closure)))

(define (kind-const-closure-value c) (cadr c))

;;; apply-kind-closure : KindClosure × KindValue → KindValue
;;; Apply a kind closure to a kind value.
(define (apply-kind-closure clo val)
  (cond
   [(kind-const-closure? clo)
    (kind-const-closure-value clo)]
   [(kind-closure? clo)
    (let ([param (kind-closure-param clo)]
          [body (kind-closure-body clo)]
          [env (kind-closure-env clo)])
         (eval-kind body (kind-env-extend env param val)))]
   [else
    (error 'apply-kind-closure "invalid closure" clo)]))

;;; ====
;;; Kind Environments
;;; ====

(define kind-empty-env '())

(define (kind-env-extend env name val)
  (cons (cons name val) env))

(define (kind-env-lookup env name)
  (let ([entry (assq name env)])
       (if entry
           (cdr entry)
           ;; Unknown kind variable becomes a neutral
           (KV-neutral (KN-var name)))))

;;; ====
;;; Kind Evaluation (Kind → KindValue)
;;; ====

;;; eval-kind : Kind × KindEnv → KindValue
;;; Evaluate a kind expression to a kind semantic value.
(define (eval-kind kind env)
  (cond
   ;; Base kind *
   [(eq? kind '*)
    (KV-star)]
   
   ;; Constraint kind
   [(eq? kind 'Constraint)
    (KV-constraint)]
   
   ;; Row kind
   [(eq? kind 'Row)
    (KV-row)]
   
   ;; Sort □ (bare)
   [(eq? kind '□)
    (KV-sort 0)]
   
   ;; Kind variable — look up or become neutral
   [(symbol? kind)
    (kind-env-lookup env kind)]
   
   [(not (pair? kind))
    (error 'eval-kind "invalid kind" kind)]
   
   ;; Leveled sort: (□ n)
   [(eq? (car kind) '□)
    (KV-sort (cadr kind))]
   
   ;; Kind arrow: (⇒ K1 K2)
   [(eq? (car kind) '⇒)
    (let* ([domain-kind (cadr kind)]
           [codomain-kind (caddr kind)]
           [domain-val (eval-kind domain-kind env)]
           ;; Non-dependent: create constant closure
           [codomain-clo (make-kind-const-closure (eval-kind codomain-kind env))])
          (KV-arrow domain-val codomain-clo))]
   
   ;; Dependent kind: (Πκ ((var : domain)) codomain)
   [(eq? (car kind) 'Πκ)
    (let* ([binding (car (cadr kind))]
           [var (car binding)]
           [domain-kind (caddr binding)]
           [codomain-kind (caddr kind)]
           [domain-val (eval-kind domain-kind env)]
           [codomain-clo (make-kind-closure var codomain-kind env)])
          (KV-pi domain-val codomain-clo))]
   
   ;; Kind polymorphism: (κ∀ (vars) body)
   ;; For now, treat as the body (assuming vars are in scope)
   [(eq? (car kind) 'κ∀)
    (let ([vars (cadr kind)]
          [body (caddr kind)])
         ;; Extend env with neutral variables for each bound var
         (let loop ([vs vars] [e env])
              (if (null? vs)
                  (eval-kind body e)
                  (loop (cdr vs)
                        (kind-env-extend e (car vs)
                                         (KV-neutral (KN-var (car vs))))))))]
   
   [else
    (error 'eval-kind "unknown kind form" kind)]))

;;; ====
;;; Kind Readback (KindValue → Kind)
;;; ====

;;; kind-readback : Level × KindValue → Kind
;;; Convert a kind semantic value back to a normal form kind expression.
(define (kind-readback level kval)
  (cond
   [(KV-star? kval)
    '*]
   
   [(KV-constraint? kval)
    'Constraint]
   
   [(KV-row? kval)
    'Row]
   
   [(KV-sort? kval)
    (let ([n (KV-sort-level kval)])
         (if (= n 0) '□ `(□ ,n)))]
   
   [(KV-arrow? kval)
    (let* ([x-name (kind-fresh-name level)]
           [x-val (KV-neutral (KN-var level))]
           [domain-nf (kind-readback level (KV-arrow-domain kval))]
           [codomain-val (apply-kind-closure (KV-arrow-codomain-clo kval) x-val)]
           [codomain-nf (kind-readback (+ level 1) codomain-val)])
          ;; Check if codomain mentions the variable
          (if (kind-mentions-var? codomain-nf x-name)
              ;; Dependent: use Πκ syntax
              `(Πκ ((,x-name : ,domain-nf)) ,codomain-nf)
              ;; Non-dependent: use ⇒ syntax
              `(⇒ ,domain-nf ,codomain-nf)))]
   
   [(KV-pi? kval)
    (let* ([x-name (kind-fresh-name level)]
           [x-val (KV-neutral (KN-var level))]
           [domain-nf (kind-readback level (KV-pi-domain kval))]
           [codomain-val (apply-kind-closure (KV-pi-codomain-clo kval) x-val)]
           [codomain-nf (kind-readback (+ level 1) codomain-val)])
          `(Πκ ((,x-name : ,domain-nf)) ,codomain-nf))]
   
   [(KV-neutral? kval)
    (kind-readback-neutral level (KV-neutral-term kval))]
   
   [else
    (error 'kind-readback "unknown kind value" kval)]))

;;; kind-readback-neutral : Level × KindNeutral → Kind
(define (kind-readback-neutral level neutral)
  (cond
   [(KN-var? neutral)
    (kind-level->name (KN-var-level neutral))]
   
   [(KN-app? neutral)
    (let ([func-nf (kind-readback-neutral level (cadr neutral))]
          [arg-nf (kind-readback level (caddr neutral))])
         `(,func-nf ,arg-nf))]
   
   [else neutral]))

;;; kind-fresh-name : Level → Symbol
;;; Generate a fresh kind variable name from a de Bruijn level.
(define (kind-fresh-name level)
  (string->symbol (string-append "k" (number->string level))))

;;; kind-level->name : Level|Symbol → Symbol
;;; Convert a de Bruijn level to a name (or pass through symbol).
(define (kind-level->name level)
  (if (symbol? level)
      level
      (string->symbol (string-append "k" (number->string level)))))

;;; kind-mentions-var? : Kind × Symbol → Boolean
;;; Check if a kind expression mentions a variable.
(define (kind-mentions-var? kind var)
  (cond
   [(symbol? kind) (eq? kind var)]
   [(not (pair? kind)) #f]
   [else (ormap (lambda (k) (kind-mentions-var? k var)) kind)]))

;;; ====
;;; Kind Normalization
;;; ====

;;; kind-normalize : Kind × KindEnv → Kind
;;; Normalize a kind expression.
(define (kind-normalize kind env)
  (kind-readback 0 (eval-kind kind env)))

;;; kind-normalize-closed : Kind → Kind
;;; Normalize a closed kind expression.
(define (kind-normalize-closed kind)
  (kind-normalize kind kind-empty-env))

;;; ====
;;; Kind Equivalence (Conversion Checking)
;;; ====

;;; kind-equiv? : Level × KindValue × KindValue → Boolean
;;; Check if two kind values are definitionally equal.
(define (kind-equiv? level kv1 kv2)
  (cond
   ;; Both are *
   [(and (KV-star? kv1) (KV-star? kv2))
    #t]
   
   ;; Both are Constraint
   [(and (KV-constraint? kv1) (KV-constraint? kv2))
    #t]
   
   ;; Both are Row
   [(and (KV-row? kv1) (KV-row? kv2))
    #t]
   
   ;; Both are sorts: compare levels
   [(and (KV-sort? kv1) (KV-sort? kv2))
    (= (KV-sort-level kv1) (KV-sort-level kv2))]
   
   ;; Both are arrows: compare domains and codomains
   [(and (KV-arrow? kv1) (KV-arrow? kv2))
    (and (kind-equiv? level (KV-arrow-domain kv1) (KV-arrow-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-arrow-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-arrow-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   
   ;; Both are dependent kinds: compare domains and codomains
   [(and (KV-pi? kv1) (KV-pi? kv2))
    (and (kind-equiv? level (KV-pi-domain kv1) (KV-pi-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-pi-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-pi-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   
   ;; Arrow vs Pi: they can be equivalent if Pi is non-dependent
   [(and (KV-arrow? kv1) (KV-pi? kv2))
    (and (kind-equiv? level (KV-arrow-domain kv1) (KV-pi-domain kv2))
         (let* ([x (KV-neutral (KN-var level))]
                [c1 (apply-kind-closure (KV-arrow-codomain-clo kv1) x)]
                [c2 (apply-kind-closure (KV-pi-codomain-clo kv2) x)])
               (kind-equiv? (+ level 1) c1 c2)))]
   [(and (KV-pi? kv1) (KV-arrow? kv2))
    (kind-equiv? level kv2 kv1)]  ; Symmetric case
   
   ;; Both are neutrals: compare structurally
   [(and (KV-neutral? kv1) (KV-neutral? kv2))
    (kind-convert-neutral? level (KV-neutral-term kv1) (KV-neutral-term kv2))]
   
   [else #f]))

;;; kind-convert-neutral? : Level × KindNeutral × KindNeutral → Boolean
;;; Check if two neutral kind terms are structurally equal.
(define (kind-convert-neutral? level n1 n2)
  (cond
   [(and (KN-var? n1) (KN-var? n2))
    (equal? (KN-var-level n1) (KN-var-level n2))]
   
   [(and (KN-app? n1) (KN-app? n2))
    (and (kind-convert-neutral? level (cadr n1) (cadr n2))
         (kind-equiv? level (caddr n1) (caddr n2)))]
   
   [else #f]))

;;; ====
;;; Convenience Functions
;;; ====

;;; kinds-equal? : Kind × Kind → Boolean
;;; Check if two kinds are definitionally equal.
(define (kinds-equal? k1 k2)
  (let ([v1 (eval-kind k1 kind-empty-env)]
        [v2 (eval-kind k2 kind-empty-env)])
       (kind-equiv? 0 v1 v2)))

;;; kind-nf : Kind → Kind
;;; Get the normal form of a kind.
(define (kind-nf k)
  (kind-normalize-closed k))

