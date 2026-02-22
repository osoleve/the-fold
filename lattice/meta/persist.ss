(load "core/base/prelude.ss")
(load "core/base/sha256.ss")

(doc 'module 'persist)
(doc 'description "Lattice index persistence — pure serialization and deserialization logic")
(doc 'layer 'lattice)
(doc 'purity 'partial)

;;; ====
;;; Cache Configuration
;;; ====

(define LATTICE-CACHE-PATH ".fold-repl/lattice-cache.sexp")
(define LATTICE-CACHE-VERSION 3)  ; Bump: module-aware source location resolution

;;; ====
;;; Pure Helpers
;;; ====

;;; path-directory : String -> String
(define (path-directory path)
  (let ([idx (string-rindex path #\/)])
       (if idx
           (substring path 0 idx)
           ".")))

;;; string-rindex : String Char -> Int | #f
(define (string-rindex str char)
  (let loop ([i (- (string-length str) 1)])
       (cond
        [(< i 0) #f]
        [(char=? (string-ref str i) char) i]
        [else (loop (- i 1))])))

;;; lattice-cache-path : -> String
(define (lattice-cache-path)
  LATTICE-CACHE-PATH)

;;; ====
;;; State Serialization (pure — reads from global hashtables)
;;; ====

;;; serialize-kg-state : -> SExp
;;; Serialize current KG state for search index restoration
(define (serialize-kg-state)
  `(kg-state
    (skill-names ,(map car *kg-skills*))
    (skill-data ,*kg-skill-data*)
    (module-names ,(map car *kg-modules*))
    (export-names ,(map car *kg-exports*))))

;;; serialize-docstrings : -> (List (Symbol . String))
;;; Convert *docstrings* hashtable to alist for serialization
(define (serialize-docstrings)
  (let-values ([(keys vals) (hashtable-entries *docstrings*)])
              (map cons (vector->list keys) (vector->list vals))))

;;; serialize-source-locs : -> (List (Symbol File Line))
;;; Convert *source-locations* hashtable to list for serialization
(define (serialize-source-locs)
  (let-values ([(keys vals) (hashtable-entries *source-locations*)])
              (map (lambda (k v) (list k (car v) (cdr v)))
                   (vector->list keys) (vector->list vals))))

;;; serialize-cache : String -> SExp
;;; Create full cache S-expression with fingerprint
(define (serialize-cache fingerprint)
  `(lattice-cache
    (version ,LATTICE-CACHE-VERSION)
    (fingerprint ,fingerprint)
    ,(serialize-kg-state)
    (docstrings ,(serialize-docstrings))
    (source-locs ,(serialize-source-locs))))

;;; ====
;;; Cache Parsing (pure — operates on S-expression data)
;;; ====

;;; cache-field : SExp Symbol -> Any | #f
;;; For simple fields like (version 1), returns the value (1)
;;; For nested fields like (kg-state ...), returns the whole field
(define (cache-field cache field-name)
  (if (and (pair? cache) (eq? (car cache) 'lattice-cache))
      (let loop ([fields (cdr cache)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             ;; For kg-state, return the whole structure; for others return value
             (if (eq? field-name 'kg-state)
                 (car fields)  ; Return (kg-state ...)
                 (cadar fields))]  ; Return just the value
            [else (loop (cdr fields))]))
      #f))

;;; kg-state-field : SExp Symbol -> Any | #f
(define (kg-state-field kg-state field-name)
  (if (and (pair? kg-state) (eq? (car kg-state) 'kg-state))
      (let loop ([fields (cdr kg-state)])
           (cond
            [(null? fields) #f]
            [(and (pair? (car fields))
                  (eq? (caar fields) field-name))
             (cadar fields)]
            [else (loop (cdr fields))]))
      #f))

;;; ====
;;; State Restoration (pure — writes to global hashtables from parsed data)
;;; ====

;;; restore-kg-state! : SExp -> void
;;; Restore KG globals from cached state
(define (restore-kg-state! kg-state)
  (let ([skill-names (kg-state-field kg-state 'skill-names)]
        [skill-data (kg-state-field kg-state 'skill-data)]
        [module-names (kg-state-field kg-state 'module-names)]
        [export-names (kg-state-field kg-state 'export-names)])
       ;; Restore skills as (name . #f) since we only need names for search
       (set! *kg-skills* (map (lambda (n) (cons n #f)) (or skill-names '())))
       (set! *kg-skill-data* (or skill-data '()))
       ;; Restore modules and exports as (name . #f)
       (set! *kg-modules* (map (lambda (n) (cons n #f)) (or module-names '())))
       (set! *kg-exports* (map (lambda (n) (cons n #f)) (or export-names '())))
       (set! *kg-deps* '())  ; Deps can be recomputed from skill-data if needed
       (set! *kg-loaded* #t)))

;;; restore-docstrings! : (List (Symbol . String)) -> void
;;; Restore docstrings cache from alist
(define (restore-docstrings! alist)
  (set! *docstrings* (make-hashtable symbol-hash eq?))
  (for-each (lambda (pair)
                    (hashtable-set! *docstrings* (car pair) (cdr pair)))
            (or alist '())))

;;; restore-source-locs! : (List (Symbol File Line)) -> void
;;; Restore source locations cache from list
(define (restore-source-locs! locs)
  (set! *source-locations* (make-hashtable symbol-hash eq?))
  (for-each (lambda (entry)
                    (hashtable-set! *source-locations*
                                    (car entry)
                                    (cons (cadr entry) (caddr entry))))
            (or locs '())))

;;; ====
;;; REPL Interface
;;; ====

(meta-printf "persist.ss loaded.\n")
