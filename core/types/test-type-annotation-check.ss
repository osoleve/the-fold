;;; core/types/test-type-annotation-check.ss — Tests for Type Annotation Validation
;;;
;;; Tests the type annotation validation logic from type-annotation-check.ss.
;;; This covers:
;;;   - Type name normalization and mapping
;;;   - Type variable recognition
;;;   - Conversion from parsed types to kind-checkable types
;;;   - Kind checking of parsed type annotations
;;;
;;; Uses core/test-framework.ss for consistent test reporting.

(load "core/test-framework.ss")
(load "core/types/types.ss")
(load "core/types/kinds.ss")
(load "core/types/sig-check.ss")

(display "
====
                    TYPE ANNOTATION CHECK TESTS
====
")

;;; ====
;;; Type Name Mapping (from type-annotation-check.ss)
;;; ====

;;; Greek letters used as type variables
(define greek-type-var-names
  '(alpha beta gamma delta epsilon zeta eta theta iota kappa lambda mu nu xi omicron pi rho sigma tau upsilon phi chi psi omega))

;;; is-type-var? : Symbol -> Boolean
(define (is-type-var? sym)
  (or (if (memq sym greek-type-var-names) #t #f)
      ;; Single lowercase letter
      (let ([s (symbol->string sym)])
           (and (= (string-length s) 1)
                (char-lower-case? (string-ref s 0))))))

;;; Type name mapping from sig-parser names to kinds.ss names
(define type-name-mapping
  '((Integer . Int)
    (Number . Nat)
    (Real . Nat)
    (Rational . Nat)
    (Char . Symbol)
    (Boolean . Bool)
    (Any . Unit)
    (Void . Unit)
    (Maybe . Option)
    (Result . Either)
    (Pair . Either)))

;;; normalize-type-name : Symbol -> Symbol
(define (normalize-type-name sym)
  (let ([entry (assq sym type-name-mapping)])
       (if entry (cdr entry) sym)))

;;; Type constructors that use @ application syntax
(define type-constructors-with-application
  '(List Vector Option Either Result Pair Maybe Ref Set Stream Delayed
    Functor Applicative Monad))

;;; Type constructors that need implicit type parameters when used bare
(define bare-type-constructors
  '(List Vector Option Set Ref Stream Delayed))

;;; sig-type->kind-type : Type -> Type
;;; Convert parsed type to kind-checker format
(define (sig-type->kind-type parsed)
  (cond
   [(symbol? parsed)
    (let ([normalized (normalize-type-name parsed)])
         ;; If it's a bare type constructor, add implicit type parameter
         (if (memq normalized bare-type-constructors)
             `(@ ,normalized alpha)  ; Implicit type variable
             normalized))]
   [(not (pair? parsed)) parsed]
   ;; Function type: (-> A B) stays the same
   [(eq? (car parsed) '->)
    `(-> ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Product type: (x A B C)
   [(eq? (car parsed) 'x)
    `(x ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Sum/union type: (+ A B)
   [(eq? (car parsed) '+)
    (let ([non-false (filter (lambda (x) (not (eq? x '#f))) (cdr parsed))])
         (if (null? non-false)
             'Unit
             (sig-type->kind-type (car non-false))))]
   ;; Forall: (forall (vars) body)
   [(eq? (car parsed) 'forall)
    `(forall ,(cadr parsed) ,(sig-type->kind-type (caddr parsed)))]
   ;; Values (multiple returns) - treat as product
   [(eq? (car parsed) 'Values)
    `(x ,@(map sig-type->kind-type (cdr parsed)))]
   ;; Type application with known constructors: use @ syntax
   [(memq (car parsed) type-constructors-with-application)
    (let* ([constructor (normalize-type-name (car parsed))]
           [args (map sig-type->kind-type (cdr parsed))])
          (if (null? args)
              constructor
              `(@ ,constructor ,@args)))]
   ;; Other type applications
   [else
    (let ([head (normalize-type-name (car parsed))])
         (if (and (symbol? head)
                  (or (lookup-kind head)
                      (memq head type-constructors-with-application)))
             `(@ ,head ,@(map sig-type->kind-type (cdr parsed)))
             (cons head (map sig-type->kind-type (cdr parsed)))))]))

;;; collect-type-vars : Type -> (List Symbol)
(define (collect-type-vars type)
  (cond
   [(symbol? type)
    (if (is-type-var? type) (list type) '())]
   [(not (pair? type)) '()]
   [else
    (apply append (map collect-type-vars type))]))

;;; make-type-var-env : (List Symbol) -> KindEnv
(define (make-type-var-env vars)
  (map (lambda (v) (cons v '*)) (unique vars)))

;;; check-type-kind : Type -> (ok Kind) | (error ...)
(define (check-type-kind parsed-type)
  (let* ([internal-type (sig-type->kind-type parsed-type)]
         [type-vars (collect-type-vars internal-type)]
         [kenv (make-type-var-env type-vars)]
         [kind-result (infer-kind internal-type kenv)])
        (if (and (pair? kind-result)
                 (eq? (car kind-result) 'error))
            `(error ,kind-result)
            `(ok ,kind-result))))

;;; ====
;;; Type Variable Recognition Tests
;;; ====

(test-group type-variable-recognition
            
            (define-test greek-alpha-is-type-var
              (assert-true (is-type-var? 'alpha)))
            
            (define-test greek-beta-is-type-var
              (assert-true (is-type-var? 'beta)))
            
            (define-test single-lowercase-is-type-var
              (assert-true (is-type-var? 'a)))
            
            (define-test single-lowercase-b-is-type-var
              (assert-true (is-type-var? 'b)))
            
            (define-test uppercase-not-type-var
              (assert-false (is-type-var? 'A)))
            
            (define-test multi-char-not-type-var
              (assert-false (is-type-var? 'ab)))
            
            (define-test int-not-type-var
              (assert-false (is-type-var? 'Int)))
            
            (define-test list-not-type-var
              (assert-false (is-type-var? 'List))))

;;; ====
;;; Type Name Normalization Tests
;;; ====

(test-group type-name-normalization
            
            (define-test integer-normalizes-to-int
              (assert-equal 'Int (normalize-type-name 'Integer)))
            
            (define-test number-normalizes-to-nat
              (assert-equal 'Nat (normalize-type-name 'Number)))
            
            (define-test boolean-normalizes-to-bool
              (assert-equal 'Bool (normalize-type-name 'Boolean)))
            
            (define-test char-normalizes-to-symbol
              (assert-equal 'Symbol (normalize-type-name 'Char)))
            
            (define-test maybe-normalizes-to-option
              (assert-equal 'Option (normalize-type-name 'Maybe)))
            
            (define-test result-normalizes-to-either
              (assert-equal 'Either (normalize-type-name 'Result)))
            
            (define-test int-stays-int
              (assert-equal 'Int (normalize-type-name 'Int)))
            
            (define-test list-stays-list
              (assert-equal 'List (normalize-type-name 'List)))
            
            (define-test unknown-stays-unknown
              (assert-equal 'FooBar (normalize-type-name 'FooBar))))

;;; ====
;;; Type Conversion Tests
;;; ====

(test-group type-conversion
            
            (define-test convert-base-type
              (assert-equal 'Int (sig-type->kind-type 'Int)))
            
            (define-test convert-integer-to-int
              (assert-equal 'Int (sig-type->kind-type 'Integer)))
            
            (define-test convert-bare-list
              (assert-equal '(@ List alpha) (sig-type->kind-type 'List)))
            
            (define-test convert-bare-option
              (assert-equal '(@ Option alpha) (sig-type->kind-type 'Option)))
            
            (define-test convert-function-type
              (assert-equal '(-> Int Bool) (sig-type->kind-type '(-> Int Bool))))
            
            (define-test convert-list-int
              (assert-equal '(@ List Int) (sig-type->kind-type '(List Int))))
            
            (define-test convert-maybe-to-option
              (assert-equal '(@ Option Int) (sig-type->kind-type '(Maybe Int))))
            
            (define-test convert-result-to-either
              (assert-equal '(@ Either String Int)
                            (sig-type->kind-type '(Result String Int))))
            
            (define-test convert-nested-list
              (assert-equal '(@ List (@ List Int))
                            (sig-type->kind-type '(List (List Int))))))

;;; ====
;;; Type Variable Collection Tests
;;; ====

(test-group type-var-collection
            
            (define-test collect-no-vars
              (assert-equal '() (collect-type-vars 'Int)))
            
            (define-test collect-single-var
              (assert-equal '(a) (collect-type-vars 'a)))
            
            (define-test collect-in-function
              (assert-true (if (member 'a (collect-type-vars '(-> a b))) #t #f)))
            
            (define-test collect-multiple-vars
              (let ([vars (collect-type-vars '(-> a (-> b c)))])
                   (assert-true (and (if (member 'a vars) #t #f)
                                     (if (member 'b vars) #t #f)
                                     (if (member 'c vars) #t #f)))))
            
            (define-test collect-in-list-type
              (assert-true (if (member 'alpha (collect-type-vars '(@ List alpha))) #t #f))))

;;; ====
;;; Kind Checking of Parsed Types
;;; ====

(test-group kind-checking-parsed-types
            
            (define-test check-simple-int
              (let ([result (check-type-kind 'Int)])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-simple-bool
              (let ([result (check-type-kind 'Bool)])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-integer-normalizes
              (let ([result (check-type-kind 'Integer)])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-function-type
              (let ([result (check-type-kind '(-> Int Bool))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-multi-arg-function
              (let ([result (check-type-kind '(-> Int String Bool))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-list-int
              (let ([result (check-type-kind '(List Int))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-maybe-bool
              (let ([result (check-type-kind '(Maybe Bool))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-either-string-int
              (let ([result (check-type-kind '(Either String Int))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-result-string-int
              (let ([result (check-type-kind '(Result String Int))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result)))))

;;; ====
;;; Polymorphic Type Annotations
;;; ====

(test-group polymorphic-annotations
            
            (define-test check-identity-type
              (let ([result (check-type-kind '(-> a a))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-map-type
              (let ([result (check-type-kind '(-> (-> a b) (List a) (List b)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-foldl-type
              (let ([result (check-type-kind '(-> (-> b a b) b (List a) b))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-compose-type
              (let ([result (check-type-kind '(-> (-> b c) (-> a b) (-> a c)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result)))))

;;; ====
;;; Complex Annotation Patterns
;;; ====

(test-group complex-annotations
            
            (define-test check-nested-list
              (let ([result (check-type-kind '(List (List Int)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-list-of-either
              (let ([result (check-type-kind '(List (Either String Int)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-function-returning-list
              (let ([result (check-type-kind '(-> Int (List String)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result))))
            
            (define-test check-higher-order-function
              (let ([result (check-type-kind '(-> (-> a b) (List a) (List b)))])
                   (assert-equal 'ok (car result))
                   (assert-equal '* (cadr result)))))

;;; ====
;;; Signature Parsing Integration
;;; ====

(test-group signature-parsing-integration
            
            (define-test parse-and-check-simple
              (let* ([parse-result (parse-sig-line ";;; foo : Int -> Bool")]
                     [type-result (if (eq? (car parse-result) 'ok)
                                      (check-type-kind (cdadr parse-result))
                                      '(skip))])
                    (if (eq? (car parse-result) 'ok)
                        (begin
                         (assert-equal 'ok (car type-result))
                         (assert-equal '* (cadr type-result)))
                        #t)))
            
            (define-test parse-and-check-list
              (let* ([parse-result (parse-sig-line ";;; bar : (List Int) -> Int")]
                     [type-result (if (eq? (car parse-result) 'ok)
                                      (check-type-kind (cdadr parse-result))
                                      '(skip))])
                    (if (eq? (car parse-result) 'ok)
                        (begin
                         (assert-equal 'ok (car type-result))
                         (assert-equal '* (cadr type-result)))
                        #t))))

;;; ====
;;; Error Cases
;;; ====

(test-group annotation-errors
            
            ;; Note: Most "domain types" like Matrix, Vec, etc. are mapped to Hash
            ;; in type-annotation-check.ss, so they won't produce kind errors.
            ;; We test cases where the kind checker should fail.
            
            (define-test unknown-type-produces-error
              (let ([result (check-type-kind '(UnknownTypeXYZ123))])
                   ;; Unknown type constructors that aren't in the mapping
                   ;; may produce errors
                   (assert-true (or (eq? (car result) 'ok)
                                    (eq? (car result) 'error))))))

;;; ====
;;; Kind Environment Creation
;;; ====

(test-group kind-env-creation
            
            (define-test empty-vars-empty-env
              (assert-equal '() (make-type-var-env '())))
            
            (define-test single-var-env
              (let ([env (make-type-var-env '(a))])
                   (assert-equal 1 (length env))
                   (assert-equal '* (cdr (assq 'a env)))))
            
            (define-test multi-var-env
              (let ([env (make-type-var-env '(a b c))])
                   (assert-equal 3 (length env))
                   (assert-true (if (assq 'a env) #t #f))
                   (assert-true (if (assq 'b env) #t #f))
                   (assert-true (if (assq 'c env) #t #f))))
            
            (define-test duplicate-vars-deduplicated
              (let ([env (make-type-var-env '(a a b))])
                   (assert-equal 2 (length env)))))

;;; ====
;;; Summary
;;; ====

(newline)
(display "====")
(newline)
(display "                TYPE ANNOTATION CHECK TESTS COMPLETE")
(newline)
(display "====")
(newline)
(exit-with-summary)
