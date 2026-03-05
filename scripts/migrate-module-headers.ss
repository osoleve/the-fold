;;; scripts/migrate-module-headers.ss
;;; One-time migration: extract module metadata from manifest.sexp files
;;; and inject @module, @description, @purity, @stability annotations.
;;;
;;; Usage: scheme --script scripts/migrate-module-headers.ss [--dry-run]

(load "core/base/prelude.ss")

;;; ---- Manifest parsing ----

(define (read-manifest path)
  (guard (exn [else #f])
    (call-with-input-file path read)))

(define (manifest-field sexp key)
  (and (pair? sexp)
    (let loop ([rest (cddr sexp)])
      (cond
        [(null? rest) #f]
        [(and (pair? (car rest)) (eq? (caar rest) key)) (cdar rest)]
        [else (loop (cdr rest))]))))

(define (manifest-field-atom sexp key)
  (let ([field (manifest-field sexp key)])
    (and field (pair? field) (car field))))

;;; ---- Module extraction ----

(define (extract-subdir-modules skill-path groups)
  (let loop ([groups groups] [acc '()])
    (if (null? groups) (reverse acc)
      (let* ([group (car groups)]
             [subdir (let ([e (assq 'subdir group)]) (if e (cadr e) ""))]
             [files (let ([e (assq 'files group)]) (if e (cadr e) '()))]
             [base (if (string=? subdir "") skill-path
                     (string-append skill-path "/" subdir))])
        (let floop ([fs files] [inner '()])
          (if (null? fs)
            (loop (cdr groups) (append (reverse inner) acc))
            (let* ([fname (if (string? (car fs)) (car fs) (caar fs))]
                   [name (string->symbol (substring fname 0 (- (string-length fname) 3)))]
                   [path (string-append base "/" fname)])
              (floop (cdr fs) (cons (list name path #f) inner)))))))))

(define (extract-modules manifest)
  (let* ([skill-path (manifest-field-atom manifest 'path)]
         [modules-raw (manifest-field manifest 'modules)])
    (if (not modules-raw) '()
      (let ([first-entry (and (pair? modules-raw) (car modules-raw))])
        (cond
          ;; Standard: ((name "file.ss" "Desc") ...)
          [(and (pair? first-entry) (symbol? (car first-entry)))
           (map (lambda (m)
                  (list (car m)
                        (string-append skill-path "/" (cadr m))
                        (if (> (length m) 2) (caddr m) #f)))
                modules-raw)]
          ;; Subdir-grouped: (((subdir "control") ...) ...)
          ;; modules-raw = ((group1 group2 ...)) — extra wrapping level
          [(and (pair? first-entry) (pair? (car first-entry)) (pair? (caar first-entry)))
           (extract-subdir-modules skill-path first-entry)]
          [else '()])))))

;;; ---- File utilities ----

(define (file-exists? path)
  (guard (exn [else #f])
    (let ([p (open-input-file path)]) (close-input-port p) #t)))

(define (read-file-lines path)
  (call-with-input-file path
    (lambda (port)
      (let loop ([lines '()])
        (let ([line (get-line port)])
          (if (eof-object? line) (reverse lines)
            (loop (cons line lines))))))))

(define (write-file-lines path lines)
  (call-with-output-file path
    (lambda (port)
      (for-each (lambda (line) (put-string port line) (put-string port "\n")) lines))
    'replace))

(define (string-starts-with? str prefix)
  (and (>= (string-length str) (string-length prefix))
       (string=? (substring str 0 (string-length prefix)) prefix)))

(define (string-trim-left str)
  (let loop ([i 0])
    (cond [(>= i (string-length str)) ""]
          [(char-whitespace? (string-ref str i)) (loop (+ i 1))]
          [else (substring str i (string-length str))])))

(define (list-take lst n)
  (let loop ([l lst] [n n] [acc '()])
    (if (= n 0) (reverse acc) (loop (cdr l) (- n 1) (cons (car l) acc)))))

(define (list-drop lst n)
  (if (= n 0) lst (list-drop (cdr lst) (- n 1))))

(define (insert-at lst idx items)
  (append (list-take lst idx) items (list-drop lst idx)))

;;; ---- Annotation detection ----

(define (line-is-annotation? line)
  (let ([trimmed (string-trim-left line)])
    (and (string-starts-with? trimmed ";;;")
         (let ([after (string-trim-left (substring trimmed 3 (string-length trimmed)))])
           (string-starts-with? after "@")))))

(define (line-has-annotation? line prefix)
  (let ([trimmed (string-trim-left line)])
    (and (string-starts-with? trimmed ";;;")
         (let ([after (string-trim-left (substring trimmed 3 (string-length trimmed)))])
           (string-starts-with? after prefix)))))

(define (has-annotation? lines prefix)
  (let loop ([ls lines] [i 0])
    (cond [(or (null? ls) (>= i 80)) #f]
          [(line-has-annotation? (car ls) prefix) #t]
          [else (loop (cdr ls) (+ i 1))])))

(define (find-first-annotation lines prefix)
  (let loop ([ls lines] [i 0])
    (cond [(or (null? ls) (>= i 80)) #f]
          [(line-has-annotation? (car ls) prefix) i]
          [else (loop (cdr ls) (+ i 1))])))

;;; ---- Extract module name from (doc 'module 'name) ----

(define (find-doc-module-name lines)
  (let loop ([ls lines] [i 0])
    (cond
      [(or (null? ls) (>= i 80)) #f]
      [else
       (let ([trimmed (string-trim-left (car ls))])
         (if (not (string-starts-with? trimmed "(doc 'module '"))
           (loop (cdr ls) (+ i 1))
           (let ([rest (substring trimmed 14 (string-length trimmed))])
             (let idx-loop ([j 0])
               (cond
                 [(>= j (string-length rest)) (loop (cdr ls) (+ i 1))]
                 [(char=? (string-ref rest j) #\))
                  (if (> j 0) (substring rest 0 j)
                    (loop (cdr ls) (+ i 1)))]
                 [else (idx-loop (+ j 1))])))))])))

;;; ---- Insertion point ----

(define (find-insertion-point lines)
  (let loop ([i 0] [last-ann -1])
    (cond
      [(>= i (min (length lines) 80))
       (if (>= last-ann 0) (+ last-ann 1) #f)]
      [(line-is-annotation? (list-ref lines i))
       (loop (+ i 1) i)]
      [else
       (let ([trimmed (string-trim-left (list-ref lines i))])
         (if (or (string=? trimmed "")
                 (and (string-starts-with? trimmed ";;;") (< i 10)))
           (loop (+ i 1) last-ann)
           (if (>= last-ann 0) (+ last-ann 1) #f)))])))

;;; ---- Build annotation lines ----

(define (collapse-to-single-line str)
  "Replace newlines and excess whitespace with single spaces."
  (if (not (string? str)) str
    (let loop ([i 0] [acc '()] [last-was-space? #f])
      (if (>= i (string-length str))
        (list->string (reverse acc))
        (let ([c (string-ref str i)])
          (cond
            [(or (char=? c #\newline) (char=? c #\return) (char=? c #\tab))
             (if last-was-space? (loop (+ i 1) acc #t)
               (loop (+ i 1) (cons #\space acc) #t))]
            [(char=? c #\space)
             (if last-was-space? (loop (+ i 1) acc #t)
               (loop (+ i 1) (cons c acc) #t))]
            [else (loop (+ i 1) (cons c acc) #f)]))))))

(define (build-new-annotations lines description purity stability)
  (let* ([desc (and description (collapse-to-single-line description))]
         [new '()]
         [new (if (and stability (not (has-annotation? lines "@stability ")))
                (cons (format ";;; @stability ~a" stability) new) new)]
         [new (if (and purity (not (has-annotation? lines "@purity ")))
                (cons (format ";;; @purity ~a" purity) new) new)]
         [new (if (and desc (not (has-annotation? lines "@description ")))
                (cons (format ";;; @description ~a" desc) new) new)])
    new))

;;; ---- Maybe add @module ----

(define (maybe-add-module lines module-name)
  "Returns (cons updated-lines module-added?)"
  (let ([needs? (and (has-annotation? lines "@requires ")
                     (not (has-annotation? lines "@module ")))])
    (if (not needs?) (cons lines #f)
      (let* ([doc-name (find-doc-module-name lines)]
             [eff-name (or doc-name (and module-name (symbol->string module-name)))]
             [idx (and eff-name (find-first-annotation lines "@requires "))])
        (if (not idx) (cons lines #f)
          (cons (insert-at lines idx (list (format ";;; @module ~a" eff-name))) #t))))))

;;; ---- Main injection ----

(define (inject-annotations! path module-name description purity stability dry-run?)
  (if (not (file-exists? path))
    (begin (display (format "  SKIP (not found): ~a\n" path)) #f)
    (let* ([result (maybe-add-module (read-file-lines path) module-name)]
           [lines (car result)]
           [module-added? (cdr result)]
           [ins-idx (find-insertion-point lines)])
      (if (not ins-idx)
        (begin (display (format "  SKIP (no annotations): ~a\n" path)) #f)
        (let* ([new (build-new-annotations lines description purity stability)]
               [nothing-to-do? (and (null? new) (not module-added?))])
          (if nothing-to-do?
            (begin (display (format "  SKIP (already annotated): ~a\n" path)) #f)
            (let ([final (insert-at lines ins-idx new)]
                  [total (+ (length new) (if module-added? 1 0))]
                  [note (if module-added? " (incl @module)" "")])
              (if dry-run?
                (display (format "  DRY-RUN ~a: +~a~a\n" path total note))
                (begin (write-file-lines path final)
                       (display (format "  WROTE ~a: +~a~a\n" path total note))))
              #t)))))))

;;; ---- Discovery ----

(define (find-manifests)
  (let ([manifests '()])
    (define (walk dir)
      (guard (exn [else #f])
        (let ([entries (directory-list dir)])
          (for-each
            (lambda (entry)
              (let ([full-path (string-append dir "/" entry)])
                (cond
                  [(string=? entry "manifest.sexp")
                   (set! manifests (cons full-path manifests))]
                  [(and (not (string-starts-with? entry "."))
                        (file-directory? full-path))
                   (walk full-path)])))
            entries))))
    (walk "lattice")
    (reverse manifests)))

(define (process-manifest! manifest-path dry-run?)
  (let ([sexp (read-manifest manifest-path)])
    (if (not sexp) (begin (display (format "SKIP manifest: ~a\n" manifest-path)) 0)
      (let* ([purity (manifest-field-atom sexp 'purity)]
             [stability (manifest-field-atom sexp 'stability)]
             [modules (extract-modules sexp)]
             [skill-name (cadr sexp)])
        (display (format "\n=== ~a (purity: ~a, stability: ~a, ~a modules) ===\n"
                         skill-name purity stability (length modules)))
        (let ([count 0])
          (for-each
            (lambda (mod)
              (when (inject-annotations! (cadr mod) (car mod) (caddr mod) purity stability dry-run?)
                (set! count (+ count 1))))
            modules)
          count)))))

(define (main)
  (let* ([args (command-line-arguments)]
         [dry-run? (member "--dry-run" args)])
    (when dry-run? (display "=== DRY RUN MODE ===\n\n"))
    (let ([manifests (find-manifests)] [total 0])
      (display (format "Found ~a manifests\n" (length manifests)))
      (for-each
        (lambda (m)
          (let ([n (process-manifest! m dry-run?)])
            (when (number? n) (set! total (+ total n)))))
        manifests)
      (display (format "\n=== Total files modified: ~a ===\n" total)))))

(main)
