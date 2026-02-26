#!/usr/bin/env scheme-script
;;; user/rlm/extract-curriculum.ss — Extract RL curriculum from lattice
;;; I/O driver: reads manifests, processes source files, writes curriculum.sexp

(load "core/base/prelude.ss")
(load "boundary/meta/file-io.ss")
(load "lattice/meta/manifest.ss")
(load "lattice/pipeline/curriculum.ss")
(load "lattice/meta/meta.ss")

;; ====
;; Topological Sort of Skills by Dependencies
;; ====

(define (topo-sort manifests)
  ;; Simple Kahn's algorithm on the ~30 skills
  (let* ([name->manifest (map (lambda (m) (cons (manifest-name m) m)) manifests)]
         [all-names (map manifest-name manifests)]
         ;; Build adjacency: dep -> list of dependents
         [dep->dependents (make-hashtable symbol-hash eq?)]
         ;; In-degree count
         [in-degree (make-hashtable symbol-hash eq?)])
    ;; Initialize
    (for-each (lambda (n) (hashtable-set! in-degree n 0)) all-names)
    ;; Build graph
    (for-each
      (lambda (m)
        (let ([name (manifest-name m)])
          (for-each
            (lambda (dep)
              (when (memq dep all-names)  ; only count deps we have manifests for
                (hashtable-set! in-degree name
                  (+ 1 (hashtable-ref in-degree name 0)))
                (hashtable-set! dep->dependents dep
                  (cons name (hashtable-ref dep->dependents dep '())))))
            (manifest-deps m))))
      manifests)
    ;; Kahn's
    (let loop ([queue (filter (lambda (n) (= 0 (hashtable-ref in-degree n 0))) all-names)]
               [result '()])
      (if (null? queue)
          (reverse result)
          (let* ([node (car queue)]
                 [rest-queue (cdr queue)]
                 [dependents (hashtable-ref dep->dependents node '())]
                 [new-ready '()])
            ;; Decrement in-degree for dependents
            (for-each
              (lambda (d)
                (let ([new-deg (- (hashtable-ref in-degree d 0) 1)])
                  (hashtable-set! in-degree d new-deg)
                  (when (= new-deg 0)
                    (set! new-ready (cons d new-ready)))))
              dependents)
            (loop (append rest-queue (reverse new-ready))
                  (cons node result)))))))

;; ====
;; Process a Single Module File
;; ====

(define (process-module-file filepath skill-name module-name tier)
  (let ([sexps (read-all-sexps filepath)])
    (if (null? sexps)
        '()
        (extract-defines sexps skill-name module-name tier))))

;; ====
;; Process a Single Skill (all its modules)
;; ====

(define (process-skill manifest skill-order)
  (let* ([skill-name (manifest-name manifest)]
         [tier (lattice-depth skill-name)]
         [module-index (manifest->module-index manifest)]
         [entries '()])
    (for-each
      (lambda (mod-entry)
        (let* ([mod-name (car mod-entry)]
               [mod-path (cdr mod-entry)]
               ;; Skip test files
               [is-test? (let ([name-str (symbol->string mod-name)])
                           (and (>= (string-length name-str) 5)
                                (string=? (substring name-str 0 5) "test-")))])
          (unless is-test?
            (when (file-exists? mod-path)
              (let ([mod-entries (process-module-file mod-path skill-name mod-name tier)])
                ;; Assign difficulty scores
                (for-each
                  (lambda (te)
                    (let ([difficulty (compute-difficulty tier skill-order
                                                         (task-entry-position te))])
                      (set! entries (cons (cons te difficulty) entries))))
                  mod-entries))))))
      module-index)
    (reverse entries)))

;; ====
;; Main Pipeline
;; ====

(define (run-extraction)
  ;; 0. Initialize lattice KG so lattice-depth is available
  (display "Initializing lattice...\n")
  (lattice-init!)
  ;; 1. Find and parse all lattice manifests
  (display "Finding lattice manifests...\n")
  (let* ([manifest-paths (find-manifests "lattice")]
         [raw-manifests (filter-map
                          (lambda (path)
                            (let ([sexp (read-manifest-sexp path)])
                              (and sexp (parse-manifest sexp))))
                          manifest-paths)]
         [valid-manifests (filter (lambda (m) (and m (valid-manifest? m))) raw-manifests)])

    (printf "  Found ~a manifests (~a valid)\n" (length raw-manifests) (length valid-manifests))

    ;; 2. Topological sort
    (display "Topological sorting by dependencies...\n")
    (let* ([skill-order (topo-sort valid-manifests)]
           [order-map (make-hashtable symbol-hash eq?)])
      ;; Build order lookup
      (let loop ([names skill-order] [idx 0])
        (unless (null? names)
          (hashtable-set! order-map (car names) idx)
          (loop (cdr names) (+ idx 1))))

      (printf "  Skill order: ~a\n" skill-order)

      ;; 3. Process each skill in order
      (display "Extracting task-answer pairs...\n")
      (let ([all-entries '()]
            [skill-count 0]
            [module-count 0])

        (for-each
          (lambda (skill-name)
            (let ([manifest (find (lambda (m) (eq? (manifest-name m) skill-name))
                                  valid-manifests)])
              (when manifest
                (let* ([idx (hashtable-ref order-map skill-name 0)]
                       [entries (process-skill manifest idx)])
                  (set! skill-count (+ skill-count 1))
                  (set! module-count (+ module-count
                                        (length (manifest->module-index manifest))))
                  (set! all-entries (append all-entries entries))
                  (printf "  ~a: ~a entries\n" skill-name (length entries))))))
          skill-order)

        ;; 4. Sort all entries by difficulty
        (let* ([sorted (list-sort (lambda (a b) (< (cdr a) (cdr b))) all-entries)]
               [timestamp (date-and-time)]
               [output-sexps
                 (map (lambda (pair)
                        (task-entry->sexp/difficulty (car pair) (cdr pair)))
                      sorted)])

          ;; 5. Write output
          (let ([output-path "user/rlm/curriculum.sexp"])
            (printf "\nWriting ~a entries to ~a...\n" (length output-sexps) output-path)
            (call-with-output-file output-path
              (lambda (port)
                (fprintf port ";; Fold RL Curriculum — auto-generated\n")
                (fprintf port ";; ~a\n\n" timestamp)
                (write `(meta
                          (generated ,timestamp)
                          (skills ,skill-count)
                          (modules ,module-count)
                          (tasks ,(length output-sexps)))
                       port)
                (newline port)
                (newline port)
                (for-each
                  (lambda (entry-sexp)
                    (write entry-sexp port)
                    (newline port))
                  output-sexps))
              'replace)

            (printf "Done. ~a tasks across ~a skills, ~a modules.\n"
                    (length output-sexps) skill-count module-count)

            ;; Quick sanity report
            (display "\nSample entries (first 3):\n")
            (let loop ([entries (take-n 3 output-sexps)])
              (unless (null? entries)
                (let ([e (car entries)])
                  (printf "  ~a — ~a\n"
                          (cadr (assq 'id (cdr e)))
                          (cadr (assq 'description (cdr e)))))
                (loop (cdr entries))))

            (display "\nSample entries (last 3):\n")
            (let loop ([entries (take-n 3 (reverse output-sexps))])
              (unless (null? entries)
                (let ([e (car entries)])
                  (printf "  ~a — ~a\n"
                          (cadr (assq 'id (cdr e)))
                          (cadr (assq 'description (cdr e)))))
                (loop (cdr entries))))))))))

;; Helpers

(define (find pred lst)
  (cond
    [(null? lst) #f]
    [(pred (car lst)) (car lst)]
    [else (find pred (cdr lst))]))

(define (take-n n lst)
  (if (or (= n 0) (null? lst))
      '()
      (cons (car lst) (take-n (- n 1) (cdr lst)))))

(define (filter-map f lst)
  (if (null? lst)
      '()
      (let ([result (f (car lst))])
        (if result
            (cons result (filter-map f (cdr lst)))
            (filter-map f (cdr lst))))))

;; Run
(run-extraction)
