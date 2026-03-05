(unless (top-level-bound? 'require) (load "core/lang/module.ss"))
;;; @module module-meta
;;; @requires prelude
;;; @description Module metadata scanner. Combines header annotations with doc-form export scanning to produce complete module metadata alists.
;;; @purity total
;;; @stability experimental

(require 'prelude)
(unless (top-level-bound? 'extract-exports-from-sexps)
  (load "lattice/meta/exports.ss"))

(doc 'module 'module-meta)
(doc 'description "Combines parse-module-metadata (header annotations) with export scanning (doc 'export #t) to produce complete module metadata alists for KG build.")
(doc 'layer 'lattice)

;;; scan-module-metadata : String -> (Option Alist)
;;; Parse all available metadata from a module file:
;;; - Header annotations: @module, @requires, @description, @keywords, @concepts, @purity, @stability
;;; - Source exports: (doc sym 'export #t) annotations
;;; Returns an alist with keys: name, requires, description, keywords, concepts,
;;; purity, stability, exports, path. Returns #f if no @module annotation found.
(define (scan-module-metadata filepath)
  (doc 'type (-> String (Option (List (Pair Symbol Any)))))
  (doc 'description "Complete metadata scan: header annotations + doc-form export extraction.")
  (doc 'export #t)
  (let ([header (parse-module-metadata filepath)])
    (and header
         (let ([exports-from-source (scan-source-exports filepath)])
           (append header
                   (list (cons 'exports exports-from-source)
                         (cons 'path filepath)))))))

;;; scan-source-exports : String -> (List Symbol)
;;; Extract exported symbols from a source file via (doc sym 'export #t) annotations.
;;; Uses the pure export pattern matcher from lattice/meta/exports.ss.
(define (scan-source-exports filepath)
  (doc 'type (-> String (List Symbol)))
  (doc 'description "Extract exported symbols from doc annotations in source file.")
  (doc 'export #t)
  (guard (exn [else '()])
    (let ([sexps (call-with-input-file filepath
                   (lambda (port)
                     (let loop ([acc '()])
                       (let ([sexp (read port)])
                         (if (eof-object? sexp)
                             (reverse acc)
                             (loop (cons sexp acc)))))))])
      (let ([inline (extract-exports-from-sexps sexps)]
            [targeted (find-targeted-exports sexps)])
        ;; Deduplicate
        (let loop ([all (append inline targeted)] [seen '()] [acc '()])
          (cond
            [(null? all) (reverse acc)]
            [(memq (car all) seen) (loop (cdr all) seen acc)]
            [else (loop (cdr all) (cons (car all) seen) (cons (car all) acc))]))))))

;;; scan-all-module-metadata : -> (List Alist)
;;; Scan metadata for all registered modules.
;;; Returns a list of metadata alists, one per module.
(define (scan-all-module-metadata)
  (doc 'type (-> (List (List (Pair Symbol Any)))))
  (doc 'description "Scan metadata for all registered modules in *module-paths*.")
  (doc 'export #t)
  (let ([paths (hashtable->alist *module-paths*)])
    (let loop ([entries paths] [acc '()])
      (if (null? entries)
          (reverse acc)
          (let* ([entry (car entries)]
                 [path (cdr entry)]
                 [meta (scan-module-metadata path)])
            (loop (cdr entries)
                  (if meta (cons meta acc) acc)))))))
