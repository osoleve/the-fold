(load "boundary/io/fs.ss")
(load "boundary/ui/text.ss")
(load "boundary/tools/edit.ss")
(load "core/base/prelude.ss")

(doc 'module 'capability-lens)
(doc 'description "Static analysis of Scheme sources to surface capability mint/use")
(doc 'layer 'boundary)
(doc 'purity 'partial)
(doc 'usage "(capability-report (fs) \"shell\" \"forum\")")
(doc 'usage "(capability-scan (fs) \"shell\") ; returns structured data")

(doc 'section 'small-utilities)

(doc 'note "string utilities (string-starts-with?, string-ends-with?, string-downcase, string-upcase, string-join) are provided by core/prelude.ss")

(doc 'note "string-prefix? and string-suffix? wrap prelude functions with consistent (pattern str) order")
(define (string-prefix? prefix str) (string-starts-with? str prefix))
(define (string-suffix? suffix str) (string-ends-with? str suffix))

;;; NOTE: unique is provided by core/base/prelude.ss

(define (set-diff xs ys)
  (filter (lambda (x) (not (member x ys))) xs))

(define (append-map f xs)
  (if (null? xs)
      '()
      (apply append (map f xs))))

(define (sort-strings xs)
  (list-sort string<? xs))

(define (sort-symbols xs)
  (list-sort (lambda (a b)
                     (string<? (symbol->string a) (symbol->string b)))
             xs))

(define (capability->symbol cap-str)
  (string->symbol (string-upcase cap-str)))

;;; ====
;;; File Discovery and Parsing
;;; ====

(define (scheme-file? path)
  (let ([len (string-length path)])
       (and (>= len 3)
            (string=? (substring path (- len 3) len) ".ss"))))

(define (skip-dir? name)
  (or (string=? name ".")
      (string=? name "..")
      (string=? name ".git")
      (string=? name ".fold-repl")
      (string=? name ".fold-sessions")
      (string=? name ".store")
      (string=? name "node_modules")))

(define (list-scheme-files path)
  (cond
   [(not (file-exists? path)) '()]
   [(and (file-exists? path) (not (file-directory? path)))
    (if (scheme-file? path) (list path) '())]
   [else
    (let loop ([to-process (list path)] [acc '()])
         (if (null? to-process)
             acc
             (let* ([current (car to-process)]
                    [rest (cdr to-process)])
                   (cond
                    [(not (file-exists? current))
                     (loop rest acc)]
                    [(and (file-directory? current)
                          (not (scheme-file? current)))
                     (let ([entries (guard (e [else '()])
                                           (directory-list current))])
                          (let ([full (map (lambda (e) (string-append current "/" e))
                                           (filter (lambda (e) (not (skip-dir? e)))
                                                   entries))])
                               (loop (append full rest) acc)))]
                    [(scheme-file? current)
                     (loop rest (cons current acc))]
                    [else
                     (loop rest acc)]))))]))

(define (read-file-exprs fs path)
  (guard (e [else '()])
         (let* ([content (read-text-file fs path)]
                [port (open-input-string content)])
               (let loop ([exprs '()])
                    (let ([expr (guard (e [else 'read-error]) (read port))])
                         (cond
                          [(eof-object? expr) (reverse exprs)]
                          [(eq? expr 'read-error) (reverse exprs)]
                          [else (loop (cons expr exprs))]))))))

;;; ====
;;; Capability Discovery
;;; ====

(define (capability-from-record-type expr)
  (and (pair? expr)
       (eq? (car expr) 'define-record-type)
       (pair? (cdr expr))
       (let ([name (cadr expr)])
            (and (symbol? name)
                 (let* ([str (symbol->string name)]
                        [suffix "-capability"])
                       (if (string-suffix? suffix str)
                           (substring str 0 (- (string-length str)
                                               (string-length suffix)))
                           #f))))))

(define (capability-from-mint-name name)
  (let ([prefix "mint-"] [suffix "-capability"])
       (if (and (string-prefix? prefix name) (string-suffix? suffix name))
           (substring name (string-length prefix)
                      (- (string-length name) (string-length suffix)))
           #f)))

(define (capability-from-make-name name)
  (let ([prefix "make-"] [suffix "-capability"])
       (if (and (string-prefix? prefix name) (string-suffix? suffix name))
           (substring name (string-length prefix)
                      (- (string-length name) (string-length suffix)))
           #f)))

(define (collect-mint-ops exprs)
  (let loop ([xs exprs] [acc '()])
       (if (null? xs)
           acc
           (let* ([ops (collect-operators (car xs))]
                  [caps (filter (lambda (c) c)
                                (map (lambda (op)
                                             (let ([name (symbol->string op)])
                                                  (or (capability-from-mint-name name)
                                                      (capability-from-make-name name))))
                                     ops))])
                 (loop (cdr xs) (append caps acc))))))

(define (discover-capabilities fs files)
  (let loop ([paths files] [caps '()])
       (if (null? paths)
           (unique (map string-downcase caps))
           (let* ([exprs (read-file-exprs fs (car paths))]
                  [record-caps (filter (lambda (c) c)
                                       (map capability-from-record-type exprs))]
                  [mint-caps (collect-mint-ops exprs)])
                 (loop (cdr paths) (append record-caps mint-caps caps))))))

;;; ====
;;; Operator Collection (skip quoted and binding positions)
;;; ====

(define (collect-operators expr)
  (cond
   [(pair? expr)
    (let ([op (car expr)] [rest (cdr expr)])
         (cond
          [(and (symbol? op)
                (memq op '(quote quasiquote syntax quasisyntax)))
           '()]
          [(and (symbol? op) (memq op '(lambda λ)))
           (collect-operators-list (cddr expr))]
          [(and (symbol? op) (eq? op 'case-lambda))
           (collect-operators-case-lambda rest)]
          [(and (symbol? op) (memq op '(let let* letrec letrec*)))
           (collect-operators-let rest)]
          [else
           (append (if (symbol? op) (list op) '())
                   (collect-operators op)
                   (collect-operators-list rest))]))]
   [else '()]))

(define (collect-operators-list exprs)
  (let loop ([xs exprs] [acc '()])
       (cond
        [(null? xs) acc]
        [(pair? xs)
         (loop (cdr xs) (append acc (collect-operators (car xs))))]
        [else (append acc (collect-operators xs))])))

(define (collect-operators-case-lambda clauses)
  (append-map
   (lambda (clause)
           (if (pair? clause)
               (collect-operators-list (cdr clause))
               '()))
   clauses))

(define (collect-operators-let rest)
  (cond
   [(null? rest) '()]
   [(symbol? (car rest))
    (let ([bindings (cadr rest)]
          [body (cddr rest)])
         (append (collect-operators-bindings bindings)
                 (collect-operators-list body)))]
   [else
    (let ([bindings (car rest)]
          [body (cdr rest)])
         (append (collect-operators-bindings bindings)
                 (collect-operators-list body)))]))

(define (collect-operators-bindings bindings)
  (if (not (list? bindings))
      '()
      (append-map
       (lambda (binding)
               (if (and (pair? binding) (pair? (cdr binding)))
                   (collect-operators-list (cdr binding))
                   '()))
       bindings)))

;;; ====
;;; Capability Classification
;;; ====

(define (capability-from-prefix name known-caps)
  (let loop ([caps known-caps])
       (cond
        [(null? caps) #f]
        [(string-prefix? (string-append (car caps) "-") name) (car caps)]
        [else (loop (cdr caps))])))

(define (capability-from-operator op known-caps)
  (let* ([name (symbol->string op)]
         [mint (or (capability-from-mint-name name)
                   (capability-from-make-name name))]
         [prefix-cap (capability-from-prefix name known-caps)]
         [direct-cap (and (member name known-caps) name)])
        (cond
         [mint (cons (string-downcase mint) 'mint)]
         [direct-cap (cons (string-downcase direct-cap) 'mint)]
         [prefix-cap (cons (string-downcase prefix-cap) 'use)]
         [else #f])))

(define (capabilities-from-operators ops known-caps)
  (let loop ([xs ops] [mints '()] [uses '()])
       (if (null? xs)
           (list (unique mints) (unique uses))
           (let ([cap (capability-from-operator (car xs) known-caps)])
                (cond
                 [(not cap) (loop (cdr xs) mints uses)]
                 [(eq? (cdr cap) 'mint)
                  (loop (cdr xs)
                        (cons (capability->symbol (car cap)) mints)
                        uses)]
                 [else
                  (loop (cdr xs)
                        mints
                        (cons (capability->symbol (car cap)) uses))])))))

;;; ====
;;; Definition Analysis
;;; ====

(define (define-form? expr)
  (and (pair? expr) (eq? (car expr) 'define)))

(define (extract-define-name expr)
  (and (define-form? expr)
       (pair? (cdr expr))
       (let ([head (cadr expr)])
            (cond
             [(symbol? head) head]
             [(pair? head) (car head)]
             [else #f]))))

(define (extract-define-body expr)
  (if (define-form? expr)
      (let ([head (cadr expr)]
            [body (cddr expr)])
           (cond
            [(symbol? head) body]
            [(pair? head) body]
            [else body]))
      '()))

(define (analyze-definition expr known-caps)
  (let* ([name (extract-define-name expr)]
         [body (extract-define-body expr)]
         [ops (collect-operators-list body)]
         [caps (capabilities-from-operators ops known-caps)]
         [mints (car caps)]
         [uses (cadr caps)]
         [requires (set-diff uses mints)])
        (and name
             (or (not (null? mints)) (not (null? uses)))
             `((name . ,name)
               (mints . ,(sort-symbols mints))
               (uses . ,(sort-symbols uses))
               (requires . ,(sort-symbols requires))))))

(define (analyze-top-level expr known-caps)
  (let* ([ops (collect-operators expr)]
         [caps (capabilities-from-operators ops known-caps)]
         [mints (car caps)]
         [uses (cadr caps)]
         [requires (set-diff uses mints)])
        (and (or (not (null? mints)) (not (null? uses)))
             `((name . *top-level*)
               (mints . ,(sort-symbols mints))
               (uses . ,(sort-symbols uses))
               (requires . ,(sort-symbols requires))))))

(define (analyze-file fs path known-caps)
  (let* ([exprs (read-file-exprs fs path)]
         [defs (filter (lambda (d) d)
                       (map (lambda (expr)
                                    (if (define-form? expr)
                                        (analyze-definition expr known-caps)
                                        #f))
                            exprs))]
         [tops (filter (lambda (d) d)
                       (map (lambda (expr)
                                    (if (define-form? expr)
                                        #f
                                        (analyze-top-level expr known-caps)))
                            exprs))]
         [all-defs (append defs tops)]
         [mints (unique (append-map (lambda (d) (cdr (assq 'mints d))) all-defs))]
         [uses (unique (append-map (lambda (d) (cdr (assq 'uses d))) all-defs))]
         [requires (unique (append-map (lambda (d) (cdr (assq 'requires d))) all-defs))])
        (and (or (not (null? mints)) (not (null? uses)))
             `((file . ,path)
               (mints . ,(sort-symbols mints))
               (uses . ,(sort-symbols uses))
               (requires . ,(sort-symbols requires))
               (definitions . ,all-defs)))))

;;; ====
;;; Public API
;;; ====

(define (capability-scan fs . paths)
  (let* ([roots (if (null? paths) (list "shell") paths)]
         [files (sort-strings (unique (apply append (map list-scheme-files roots))))]
         [known (discover-capabilities fs files)]
         [known (if (null? known) (list "fs") known)])
        (filter (lambda (r) r)
                (map (lambda (path) (analyze-file fs path known)) files))))

(define (format-cap-list caps)
  (if (null? caps)
      "none"
      (string-join (map symbol->string caps) ", ")))

(define (capability-report fs . paths)
  (let* ([roots (if (null? paths) (list "shell") paths)]
         [reports (apply capability-scan fs roots)]
         [caps (if (null? reports)
                   '()
                   (unique (append-map (lambda (r)
                                               (append (cdr (assq 'mints r))
                                                       (cdr (assq 'uses r))))
                                       reports)))]
         [caps (sort-symbols caps)])
        (display "CAPABILITY LENS\n")
        (display "Paths: ")
        (display (string-join roots ", "))
        (display "\n")
        (display "Capabilities: ")
        (display (format-cap-list caps))
        (display "\n\n")
        (if (null? reports)
            (display "No capability usage found.\n")
            (begin
             (display "Summary:\n")
             (for-each
              (lambda (cap)
                      (let* ([file-count (length (filter (lambda (r)
                                                                 (or (member cap (cdr (assq 'mints r)))
                                                                     (member cap (cdr (assq 'uses r)))))
                                                         reports))]
                             [defs (apply append (map (lambda (r)
                                                              (cdr (assq 'definitions r)))
                                                      reports))]
                             [def-count (length (filter (lambda (d)
                                                                (or (member cap (cdr (assq 'mints d)))
                                                                    (member cap (cdr (assq 'uses d)))))
                                                        defs))]
                             [mint-count (length (filter (lambda (d)
                                                                 (member cap (cdr (assq 'mints d))))
                                                         defs))]
                             [use-count (length (filter (lambda (d)
                                                                (member cap (cdr (assq 'uses d))))
                                                        defs))]
                             [req-count (length (filter (lambda (d)
                                                                (member cap (cdr (assq 'requires d))))
                                                        defs))])
                            (display (format "  ~a: files=~a defs=~a mints=~a uses=~a requires=~a\n"
                                             cap file-count def-count mint-count use-count req-count))))
              caps)
             (display "\nBy file:\n")
             (for-each
              (lambda (r)
                      (display (format "  ~a\n" (cdr (assq 'file r))))
                      (display (format "    mints: ~a\n" (format-cap-list (cdr (assq 'mints r)))))
                      (display (format "    uses: ~a\n" (format-cap-list (cdr (assq 'uses r)))))
                      (display (format "    requires: ~a\n" (format-cap-list (cdr (assq 'requires r)))))
                      (let ([defs (cdr (assq 'definitions r))])
                           (when (not (null? defs))
                                 (display "    definitions:\n")
                                 (for-each
                                  (lambda (d)
                                          (display (format "      - ~a: mints [~a], uses [~a], requires [~a]\n"
                                                           (cdr (assq 'name d))
                                                           (format-cap-list (cdr (assq 'mints d)))
                                                           (format-cap-list (cdr (assq 'uses d)))
                                                           (format-cap-list (cdr (assq 'requires d))))))
                                  defs))))
              reports)))))
