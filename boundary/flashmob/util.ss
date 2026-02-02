(doc 'module 'flashmob-util)
(doc 'description "Shared utilities for flashmob modules")
(doc 'layer 'boundary)
(doc 'purity 'pure)

(doc 'section 'keyword-arguments)

(doc fm-get-keyword 'type '(-> (List Any) Symbol Any Any))
(doc fm-get-keyword 'description "Extract keyword argument from rest args list")
(doc fm-get-keyword 'example "(fm-get-keyword args 'count 10) ; Get 'count or default to 10")
(define (fm-get-keyword args key default)
  (let loop ([lst args])
    (cond
     [(null? lst) default]
     [(null? (cdr lst)) default]
     [(eq? (car lst) key) (cadr lst)]
     [else (loop (cdr lst))])))

(doc 'section 'string-utilities)

(doc fm-truncate 'type '(-> String Int String))
(doc fm-truncate 'description "Truncate string to max length with ellipsis")
(define (fm-truncate str max-len)
  (if (<= (string-length str) max-len)
      str
      (string-append (substring str 0 (- max-len 3)) "...")))

(doc fm-pad-right 'type '(-> String Int String))
(doc fm-pad-right 'description "Pad string on right to width")
(define (fm-pad-right str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append str (make-string (- width len) #\space)))))

(doc fm-pad-left 'type '(-> String Int String))
(doc fm-pad-left 'description "Pad string on left to width")
(define (fm-pad-left str width)
  (let ([len (string-length str)])
    (if (>= len width)
        str
        (string-append (make-string (- width len) #\space) str))))
