(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module contract
;;; @requires prelude sha256 sort
(require 'prelude)
(require 'sha256)
(require 'sort)

(doc 'module 'contract)
(doc 'description "Content-addressed validation contracts. Contracts are declarative
specifications of data shape expressed as S-expressions. Each contract gets a
SHA256 hash from its canonical form, enabling versioning, provenance tracking,
and CAS storage.")
(doc 'layer 'lattice)
(doc 'purity 'total)

;;; ============================================================
;;; Contract Spec Language
;;; ============================================================
;;;
;;; A spec is a tagged S-expression describing valid data shape:
;;;
;;;   (prim symbol?)           — primitive type check
;;;   (and spec1 spec2 ...)    — all must pass
;;;   (or spec1 spec2 ...)     — at least one must pass
;;;   (not spec)               — negation
;;;   (list-of spec)           — list with all elements matching
;;;   (vector-of spec)         — vector with all elements matching
;;;   (pair-of spec1 spec2)    — pair with car/cdr matching
;;;   (range lo hi)            — number in [lo, hi]
;;;   (length= n)              — collection of exact length
;;;   (length>= n)             — collection of minimum length
;;;   (fields (name . spec) ...) — alist with required fields
;;;   (optional name spec)     — field that may be absent
;;;   (ref name)               — reference to registry entry
;;;   (any)                    — always passes
;;;   (none)                   — always fails

;;; ============================================================
;;; Primitive Specs
;;; ============================================================

(doc 'section 'primitives)

(define spec/symbol   '(prim symbol?))
(doc 'spec/symbol 'export #t)
(define spec/string   '(prim string?))
(doc 'spec/string 'export #t)
(define spec/number   '(prim number?))
(doc 'spec/number 'export #t)
(define spec/integer  '(prim integer?))
(define spec/boolean  '(prim boolean?))
(doc 'spec/boolean 'export #t)
(define spec/bytevector '(prim bytevector?))
(define spec/pair     '(prim pair?))
(define spec/null     '(prim null?))
(doc 'spec/null 'export #t)
(define spec/vector   '(prim vector?))
(define spec/char     '(prim char?))
(define spec/list     '(prim list?))

;;; ============================================================
;;; Combinators
;;; ============================================================

(doc 'section 'combinators)

(doc 'type '(-> Spec ... Spec))
(doc 'description "Combine specs with logical AND — all must pass.")
(define (spec/and . specs)
  (doc 'export #t)
  (cons 'and specs))

(doc 'type '(-> Spec ... Spec))
(doc 'description "Combine specs with logical OR — at least one must pass.")
(define (spec/or . specs)
  (doc 'export #t)
  (cons 'or specs))

(doc 'type '(-> Spec Spec))
(doc 'description "Negate a spec.")
(define (spec/not spec) (list 'not spec))

;;; ============================================================
;;; Collections
;;; ============================================================

(doc 'section 'collections)

(doc 'type '(-> Spec Spec))
(doc 'description "Spec for a list where every element matches the given spec.")
(define (spec/list-of elem-spec)
  (doc 'export #t)
  (list 'list-of elem-spec))

(doc 'type '(-> Spec Spec))
(doc 'description "Spec for a vector where every element matches the given spec.")
(define (spec/vector-of elem-spec) (list 'vector-of elem-spec))

(doc 'type '(-> Spec Spec Spec))
(doc 'description "Spec for a pair where car matches spec1 and cdr matches spec2.")
(define (spec/pair-of car-spec cdr-spec)
  (doc 'export #t)
  (list 'pair-of car-spec cdr-spec))

;;; ============================================================
;;; Numeric / Size Constraints
;;; ============================================================

(doc 'section 'constraints)

(doc 'type '(-> Number Number Spec))
(doc 'description "Spec for a number in [lo, hi] inclusive.")
(define (spec/range lo hi)
  (doc 'export #t)
  (list 'range lo hi))

(doc 'type '(-> Nat Spec))
(doc 'description "Spec for a collection of exactly n elements.")
(define (spec/length= n) (list 'length= n))

(doc 'type '(-> Nat Spec))
(doc 'description "Spec for a collection of at least n elements.")
(define (spec/length>= n) (list 'length>= n))

;;; ============================================================
;;; Structural (Fields)
;;; ============================================================

(doc 'section 'structural)

(doc 'type '(-> (Pair Symbol Spec) ... Spec))
(doc 'description "Spec for an alist with required fields. Each field is (name . spec).")
(define (spec/fields . field-specs) (cons 'fields field-specs))

(doc 'type '(-> Symbol Spec Spec))
(doc 'description "Spec for an optional field in a fields spec.")
(define (spec/optional name spec)
  (doc 'export #t)
  (list 'optional name spec))

;;; ============================================================
;;; References
;;; ============================================================

(doc 'section 'references)

(doc 'type '(-> Symbol Spec))
(doc 'description "Reference to a named contract in the registry.")
(define (spec/ref name) (list 'ref name))

;;; ============================================================
;;; Contract Record
;;; ============================================================

(doc 'section 'contract)

(doc 'type '(-> Symbol Spec Contract))
(doc 'description "Create a named, content-addressed validation contract.")
(define (make-contract name spec)
  (doc 'export #t)
  (let ([hash (sha256-hex (string->utf8 (contract-serialize-spec spec)))])
    (list 'contract
          (list 'name name)
          (list 'spec spec)
          (list 'hash hash))))

(doc 'type '(-> Any Boolean))
(doc 'description "Check if a value is a contract.")
(define (contract? x)
  (doc 'export #t)
  (and (pair? x)
       (eq? 'contract (car x))
       (pair? (cdr x))
       (pair? (assq 'name (cdr x)))
       (pair? (assq 'spec (cdr x)))
       (pair? (assq 'hash (cdr x)))))

(doc 'type '(-> Contract Symbol))
(define (contract-name c)
  (doc 'export #t)
  (cadr (assq 'name (cdr c))))

(doc 'type '(-> Contract Spec))
(define (contract-spec c)
  (doc 'export #t)
  (cadr (assq 'spec (cdr c))))

(doc 'type '(-> Contract String))
(define (contract-hash c)
  (doc 'export #t)
  (cadr (assq 'hash (cdr c))))

;;; ============================================================
;;; Spec Checking (Predicate Mode)
;;; ============================================================

(doc 'section 'checking)

(doc 'type '(-> Spec Any Boolean))
(doc 'description "Check a value against a spec. Returns #t if the value matches.")
(define (contract-check spec value)
  (doc 'export #t)
  (contract-check-with spec value #f))

;;; Internal: check with optional registry for (ref ...) resolution.
(define (contract-check-with spec value registry)
  (doc 'export #t)
  (cond
    ;; Primitives
    [(and (pair? spec) (eq? 'prim (car spec)))
     (let ([pred-name (cadr spec)])
       (case pred-name
         [(symbol?)     (symbol? value)]
         [(string?)     (string? value)]
         [(number?)     (number? value)]
         [(integer?)    (integer? value)]
         [(boolean?)    (boolean? value)]
         [(bytevector?) (bytevector? value)]
         [(pair?)       (pair? value)]
         [(null?)       (null? value)]
         [(vector?)     (vector? value)]
         [(char?)       (char? value)]
         [(list?)       (list? value)]
         [else #f]))]

    ;; And
    [(and (pair? spec) (eq? 'and (car spec)))
     (let loop ([specs (cdr spec)])
       (or (null? specs)
           (and (contract-check-with (car specs) value registry)
                (loop (cdr specs)))))]

    ;; Or
    [(and (pair? spec) (eq? 'or (car spec)))
     (let loop ([specs (cdr spec)])
       (and (pair? specs)
            (or (contract-check-with (car specs) value registry)
                (loop (cdr specs)))))]

    ;; Not
    [(and (pair? spec) (eq? 'not (car spec)))
     (not (contract-check-with (cadr spec) value registry))]

    ;; List-of
    [(and (pair? spec) (eq? 'list-of (car spec)))
     (and (list? value)
          (let loop ([items value])
            (or (null? items)
                (and (contract-check-with (cadr spec) (car items) registry)
                     (loop (cdr items))))))]

    ;; Vector-of
    [(and (pair? spec) (eq? 'vector-of (car spec)))
     (and (vector? value)
          (let loop ([i 0])
            (or (>= i (vector-length value))
                (and (contract-check-with (cadr spec) (vector-ref value i) registry)
                     (loop (+ i 1))))))]

    ;; Pair-of
    [(and (pair? spec) (eq? 'pair-of (car spec)))
     (and (pair? value)
          (contract-check-with (cadr spec) (car value) registry)
          (contract-check-with (caddr spec) (cdr value) registry))]

    ;; Range
    [(and (pair? spec) (eq? 'range (car spec)))
     (and (number? value)
          (>= value (cadr spec))
          (<= value (caddr spec)))]

    ;; Length=
    [(and (pair? spec) (eq? 'length= (car spec)))
     (let ([n (cadr spec)])
       (cond
         [(list? value) (= (length value) n)]
         [(vector? value) (= (vector-length value) n)]
         [(string? value) (= (string-length value) n)]
         [(bytevector? value) (= (bytevector-length value) n)]
         [else #f]))]

    ;; Length>=
    [(and (pair? spec) (eq? 'length>= (car spec)))
     (let ([n (cadr spec)])
       (cond
         [(list? value) (>= (length value) n)]
         [(vector? value) (>= (vector-length value) n)]
         [(string? value) (>= (string-length value) n)]
         [(bytevector? value) (>= (bytevector-length value) n)]
         [else #f]))]

    ;; Fields (alist)
    [(and (pair? spec) (eq? 'fields (car spec)))
     (and (list? value)
          (let loop ([field-specs (cdr spec)])
            (or (null? field-specs)
                (let ([fs (car field-specs)])
                  (cond
                    ;; Optional field
                    [(and (pair? fs) (eq? 'optional (car fs)))
                     (let ([fname (cadr fs)] [fspec (caddr fs)])
                       (let ([entry (assq fname value)])
                         (and (or (not entry)
                                  (contract-check-with fspec (cdr entry) registry))
                              (loop (cdr field-specs)))))]
                    ;; Required field
                    [(pair? fs)
                     (let ([fname (car fs)] [fspec (cdr fs)])
                       (let ([entry (assq fname value)])
                         (and entry
                              (contract-check-with fspec (cdr entry) registry)
                              (loop (cdr field-specs)))))]
                    [else #f])))))]

    ;; Ref (registry lookup)
    [(and (pair? spec) (eq? 'ref (car spec)))
     (if registry
         (let ([contract (registry-lookup registry (cadr spec))])
           (if contract
               (contract-check-with (contract-spec contract) value registry)
               #f))
         #f)]

    ;; Any / None
    [(and (pair? spec) (eq? 'any (car spec))) #t]
    [(and (pair? spec) (eq? 'none (car spec))) #f]

    [else #f]))

;;; ============================================================
;;; Spec Validation (Error Mode)
;;; ============================================================

(doc 'section 'validation)

(doc 'type '(-> Spec Any (Or (ok #t) (error String String))))
(doc 'description "Validate a value against a spec. Returns (ok #t) on success,
or (error path message) with the location and description of the first failure.")
(define (contract-validate spec value)
  (doc 'export #t)
  (contract-validate-at spec value "" #f))

;;; Internal: validate with path tracking and optional registry.
(define (contract-validate-at spec value path registry)
  (doc 'export #t)
  (cond
    ;; Primitives
    [(and (pair? spec) (eq? 'prim (car spec)))
     (if (contract-check-with spec value registry)
         '(ok #t)
         `(error ,path ,(string-append "expected " (symbol->string (cadr spec))
                                       ", got " (brief-type value))))]

    ;; And
    [(and (pair? spec) (eq? 'and (car spec)))
     (let loop ([specs (cdr spec)] [i 0])
       (if (null? specs)
           '(ok #t)
           (let ([result (contract-validate-at (car specs) value
                           (string-append path "/and[" (number->string i) "]")
                           registry)])
             (if (eq? 'ok (car result))
                 (loop (cdr specs) (+ i 1))
                 result))))]

    ;; Or
    [(and (pair? spec) (eq? 'or (car spec)))
     (let loop ([specs (cdr spec)])
       (if (null? specs)
           `(error ,path "none of the alternatives matched")
           (let ([result (contract-validate-at (car specs) value path registry)])
             (if (eq? 'ok (car result))
                 result
                 (loop (cdr specs))))))]

    ;; Not
    [(and (pair? spec) (eq? 'not (car spec)))
     (if (contract-check-with (cadr spec) value registry)
         `(error ,path "value matched excluded spec")
         '(ok #t))]

    ;; List-of
    [(and (pair? spec) (eq? 'list-of (car spec)))
     (if (not (list? value))
         `(error ,path ,(string-append "expected list, got " (brief-type value)))
         (let loop ([items value] [i 0])
           (if (null? items)
               '(ok #t)
               (let ([result (contract-validate-at (cadr spec) (car items)
                               (string-append path "[" (number->string i) "]")
                               registry)])
                 (if (eq? 'ok (car result))
                     (loop (cdr items) (+ i 1))
                     result)))))]

    ;; Vector-of
    [(and (pair? spec) (eq? 'vector-of (car spec)))
     (if (not (vector? value))
         `(error ,path ,(string-append "expected vector, got " (brief-type value)))
         (let loop ([i 0])
           (if (>= i (vector-length value))
               '(ok #t)
               (let ([result (contract-validate-at (cadr spec) (vector-ref value i)
                               (string-append path "[" (number->string i) "]")
                               registry)])
                 (if (eq? 'ok (car result))
                     (loop (+ i 1))
                     result)))))]

    ;; Pair-of
    [(and (pair? spec) (eq? 'pair-of (car spec)))
     (if (not (pair? value))
         `(error ,path ,(string-append "expected pair, got " (brief-type value)))
         (let ([car-result (contract-validate-at (cadr spec) (car value)
                             (string-append path ".car") registry)])
           (if (eq? 'ok (car car-result))
               (contract-validate-at (caddr spec) (cdr value)
                 (string-append path ".cdr") registry)
               car-result)))]

    ;; Range
    [(and (pair? spec) (eq? 'range (car spec)))
     (cond
       [(not (number? value))
        `(error ,path ,(string-append "expected number, got " (brief-type value)))]
       [(< value (cadr spec))
        `(error ,path ,(string-append "value " (number->string value)
                                      " below minimum " (number->string (cadr spec))))]
       [(> value (caddr spec))
        `(error ,path ,(string-append "value " (number->string value)
                                      " above maximum " (number->string (caddr spec))))]
       [else '(ok #t)])]

    ;; Length=
    [(and (pair? spec) (eq? 'length= (car spec)))
     (if (contract-check-with spec value registry)
         '(ok #t)
         `(error ,path ,(string-append "expected length " (number->string (cadr spec)))))]

    ;; Length>=
    [(and (pair? spec) (eq? 'length>= (car spec)))
     (if (contract-check-with spec value registry)
         '(ok #t)
         `(error ,path ,(string-append "expected length >= " (number->string (cadr spec)))))]

    ;; Fields
    [(and (pair? spec) (eq? 'fields (car spec)))
     (if (not (list? value))
         `(error ,path ,(string-append "expected alist, got " (brief-type value)))
         (let loop ([field-specs (cdr spec)])
           (if (null? field-specs)
               '(ok #t)
               (let ([fs (car field-specs)])
                 (cond
                   ;; Optional field
                   [(and (pair? fs) (eq? 'optional (car fs)))
                    (let ([fname (cadr fs)] [fspec (caddr fs)])
                      (let ([entry (assq fname value)])
                        (if (not entry)
                            (loop (cdr field-specs))
                            (let ([result (contract-validate-at fspec (cdr entry)
                                            (string-append path "." (symbol->string fname))
                                            registry)])
                              (if (eq? 'ok (car result))
                                  (loop (cdr field-specs))
                                  result)))))]
                   ;; Required field
                   [(pair? fs)
                    (let ([fname (car fs)] [fspec (cdr fs)])
                      (let ([entry (assq fname value)])
                        (if (not entry)
                            `(error ,path ,(string-append "missing required field: "
                                                          (symbol->string fname)))
                            (let ([result (contract-validate-at fspec (cdr entry)
                                            (string-append path "." (symbol->string fname))
                                            registry)])
                              (if (eq? 'ok (car result))
                                  (loop (cdr field-specs))
                                  result)))))]
                   [else `(error ,path "malformed field spec")])))))]

    ;; Ref
    [(and (pair? spec) (eq? 'ref (car spec)))
     (if (not registry)
         `(error ,path "registry reference used without registry")
         (let ([contract (registry-lookup registry (cadr spec))])
           (if (not contract)
               `(error ,path ,(string-append "unknown contract: "
                                             (symbol->string (cadr spec))))
               (contract-validate-at (contract-spec contract) value path registry))))]

    ;; Any / None
    [(and (pair? spec) (eq? 'any (car spec))) '(ok #t)]
    [(and (pair? spec) (eq? 'none (car spec)))
     `(error ,path "spec (none) always fails")]

    [else `(error ,path "unknown spec form")]))

;;; Brief type description for error messages.
(define (brief-type value)
  (doc 'export #t)
  (cond
    [(symbol? value) "symbol"]
    [(string? value) "string"]
    [(number? value) "number"]
    [(boolean? value) "boolean"]
    [(bytevector? value) "bytevector"]
    [(null? value) "null"]
    [(pair? value) "pair"]
    [(vector? value) "vector"]
    [(char? value) "char"]
    [else "unknown"]))

;;; ============================================================
;;; Contract Registry
;;; ============================================================

(doc 'section 'registry)

(doc 'type '(-> ContractRegistry))
(doc 'description "Create an empty contract registry.")
(define (make-contract-registry)
  (doc 'export #t)
  (list 'contract-registry))

(doc 'type '(-> ContractRegistry Boolean))
(define (contract-registry? x)
  (doc 'export #t)
  (and (pair? x) (eq? 'contract-registry (car x))))

(doc 'type '(-> ContractRegistry Contract ContractRegistry))
(doc 'description "Add a contract to the registry. Returns updated registry.")
(define (registry-add! reg contract)
  (doc 'export #t)
  (let ([existing (assq (contract-name contract) (cdr reg))])
    (if existing
        ;; Replace
        (cons 'contract-registry
              (map (lambda (entry)
                     (if (eq? (car entry) (contract-name contract))
                         (cons (contract-name contract) contract)
                         entry))
                   (cdr reg)))
        ;; Add
        (cons 'contract-registry
              (cons (cons (contract-name contract) contract)
                    (cdr reg))))))

(doc 'type '(-> ContractRegistry Symbol (Maybe Contract)))
(doc 'description "Look up a contract by name.")
(define (registry-lookup reg name)
  (doc 'export #t)
  (let ([entry (assq name (cdr reg))])
    (if entry (cdr entry) #f)))

(doc 'type '(-> ContractRegistry (List Contract)))
(doc 'description "List all contracts in the registry.")
(define (registry-contracts reg)
  (doc 'export #t)
  (map cdr (cdr reg)))

(doc 'type '(-> ContractRegistry String))
(doc 'description "Content-address the entire registry. Hash of sorted contract hashes.")
(define (registry-hash reg)
  (doc 'export #t)
  (let* ([contracts (registry-contracts reg)]
         [sorted-hashes (sort-by string<? (map contract-hash contracts))]
         [combined (apply string-append sorted-hashes)])
    (sha256-hex (string->utf8 combined))))

;;; ============================================================
;;; Serialization
;;; ============================================================

(doc 'section 'serialization)

(doc 'type '(-> Spec String))
(doc 'description "Serialize a contract spec to a canonical string form for hashing.")
(define (contract-serialize-spec spec)
  (doc 'export #t)
  (cond
    [(null? spec) "()"]
    [(symbol? spec) (symbol->string spec)]
    [(number? spec) (number->string spec)]
    [(string? spec) (string-append "\"" spec "\"")]
    [(boolean? spec) (if spec "#t" "#f")]
    [(pair? spec)
     (string-append "("
       (let loop ([items spec] [first #t])
         (cond
           [(null? items) ")"]
           [(not (pair? items))
            (string-append " . " (contract-serialize-spec items) ")")]
           [else
            (string-append
              (if first "" " ")
              (contract-serialize-spec (car items))
              (loop (cdr items) #f))])))]
    [else "?"]))

(doc 'type '(-> Contract String))
(doc 'description "Serialize an entire contract (name + spec) for display.")
(define (contract-serialize contract)
  (doc 'export #t)
  (string-append "(contract " (symbol->string (contract-name contract))
                 " " (contract-serialize-spec (contract-spec contract)) ")"))

;;; ============================================================
;;; Convenience: Check/Validate with Registry
;;; ============================================================

(doc 'section 'convenience)

(doc 'type '(-> ContractRegistry Spec Any Boolean))
(doc 'description "Check a value against a spec, resolving (ref ...) via registry.")
(define (contract-check-in registry spec value)
  (doc 'export #t)
  (contract-check-with spec value registry))

(doc 'type '(-> ContractRegistry Spec Any (Or (ok #t) (error String String))))
(doc 'description "Validate a value against a spec, resolving (ref ...) via registry.")
(define (contract-validate-in registry spec value)
  (doc 'export #t)
  (contract-validate-at spec value "" registry))
