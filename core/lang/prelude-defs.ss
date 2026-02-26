;;; @module prelude-defs
;;; @description Standard prelude definitions for The Fold's evaluator.
;;; Functions defined as S-expressions evaluated in The Fold's own language.
;;; Loaded by eval.ss — requires env-extend, eval-expr to be defined first.

(doc 'section 'standard-prelude)

(doc 'note "Some useful functions defined in The Fold's own language. These are defined as expressions to be evaluated, ensuring proper closure semantics.")

(doc prelude-defs 'type (List (Pair Symbol Expr)))
(doc prelude-defs 'description "Standard prelude definitions.")
(doc prelude-defs 'export #t)
(define prelude-defs
  '((id      . (fn (x) x))
    (const   . (fn (x) (fn (y) x)))
    (compose . (fn (f) (fn (g) (fn (x) (f (g x))))))
    (flip    . (fn (f) (fn (x) (fn (y) ((f y) x)))))
    (on      . (fn (f) (fn (g) (fn (x) (fn (y) ((f (g x)) (g y)))))))

    (fst     . (fn (p) (prim 'car p)))
    (snd     . (fn (p) (prim 'car (prim 'cdr p))))
    (pair    . (fn (a) (fn (b) (prim 'list a b))))

    (bool-not . (fn (b) (if b #f #t)))
    (bool-and . (fn (a) (fn (b) (if a b #f))))
    (bool-or  . (fn (a) (fn (b) (if a #t b))))

    (map     . (fix map (fn (f xs)
                            (if (prim 'null? xs)
                                '()
                                (prim 'cons (f (prim 'car xs))
                                      (map f (prim 'cdr xs)))))))

    (filter  . (fix filter (fn (p xs)
                               (if (prim 'null? xs)
                                   '()
                                   (let ((x (prim 'car xs))
                                         (rest (filter p (prim 'cdr xs))))
                                        (if (p x)
                                            (prim 'cons x rest)
                                            rest))))))

    (foldl   . (fix foldl (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (foldl f (f acc (prim 'car xs)) (prim 'cdr xs))))))

    (foldr   . (fix foldr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  acc
                                  (f (prim 'car xs) (foldr f acc (prim 'cdr xs)))))))

    (scanl   . (fix scanl (fn (f acc xs)
                              (prim 'cons acc
                                    (if (prim 'null? xs)
                                        '()
                                        (let ((new-acc (f acc (prim 'car xs))))
                                             (scanl f new-acc (prim 'cdr xs))))))))

    (scanr   . (fix scanr (fn (f acc xs)
                              (if (prim 'null? xs)
                                  (prim 'list acc)
                                  (let ((rest (scanr f acc (prim 'cdr xs))))
                                       (prim 'cons (f (prim 'car xs) (prim 'car rest))
                                             rest))))))

    (take    . (fix take (fn (n xs)
                             (if (prim 'zero? n)
                                 '()
                                 (if (prim 'null? xs)
                                     '()
                                     (prim 'cons (prim 'car xs)
                                           (take (prim 'sub n 1) (prim 'cdr xs))))))))

    (drop    . (fix drop (fn (n xs)
                             (if (prim 'zero? n)
                                 xs
                                 (if (prim 'null? xs)
                                     '()
                                     (drop (prim 'sub n 1) (prim 'cdr xs)))))))

    (zip     . (fix zip (fn (xs ys)
                            (if (prim 'null? xs)
                                '()
                                (if (prim 'null? ys)
                                    '()
                                    (prim 'cons (prim 'list (prim 'car xs) (prim 'car ys))
                                          (zip (prim 'cdr xs) (prim 'cdr ys))))))))

    (range   . (fix range (fn (start end)
                              (if (prim 'ge? start end)
                                  '()
                                  (prim 'cons start (range (prim 'add start 1) end))))))

    (sum     . (fix sum (fn (xs)
                            (if (prim 'null? xs)
                                0
                                (prim 'add (prim 'car xs) (sum (prim 'cdr xs)))))))

    (product . (fix product (fn (xs)
                                (if (prim 'null? xs)
                                    1
                                    (prim 'mul (prim 'car xs) (product (prim 'cdr xs)))))))

    (flatten . (fix flatten (fn (xss)
                                (if (prim 'null? xss)
                                    '()
                                    (prim 'append (prim 'car xss)
                                          (flatten (prim 'cdr xss)))))))

    (flatMap . (fn (f)
                   (fix flatMap-rec (fn (xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (prim 'append (f (prim 'car xs))
                                                  (flatMap-rec (prim 'cdr xs))))))))

    (any     . (fix any (fn (p xs)
                            (if (prim 'null? xs)
                                #f
                                (if (p (prim 'car xs))
                                    #t
                                    (any p (prim 'cdr xs)))))))

    (all     . (fix all (fn (p xs)
                            (if (prim 'null? xs)
                                #t
                                (if (p (prim 'car xs))
                                    (all p (prim 'cdr xs))
                                    #f)))))

    (elem    . (fix elem (fn (x xs)
                             (if (prim 'null? xs)
                                 #f
                                 (if (prim 'eq? x (prim 'car xs))
                                     #t
                                     (elem x (prim 'cdr xs)))))))

    (replicate . (fix replicate (fn (n x)
                                    (if (prim 'zero? n)
                                        '()
                                        (prim 'cons x (replicate (prim 'sub n 1) x))))))

    (takeWhile . (fix takeWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (prim 'cons (prim 'car xs)
                                                  (takeWhile p (prim 'cdr xs)))
                                            '())))))

    (dropWhile . (fix dropWhile (fn (p xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (if (p (prim 'car xs))
                                            (dropWhile p (prim 'cdr xs))
                                            xs)))))

    (span    . (fix span (fn (p xs)
                             (if (prim 'null? xs)
                                 (prim 'list '() '())
                                 (if (p (prim 'car xs))
                                     (let ((rest (span p (prim 'cdr xs))))
                                          (prim 'list
                                                (prim 'cons (prim 'car xs) (prim 'car rest))
                                                (prim 'car (prim 'cdr rest))))
                                     (prim 'list '() xs))))))

    (break   . (fn (p)
                   (fix break-rec (fn (xs)
                                      (if (prim 'null? xs)
                                          (prim 'list '() '())
                                          (if (p (prim 'car xs))
                                              (prim 'list '() xs)
                                              (let ((rest (break-rec (prim 'cdr xs))))
                                                   (prim 'list
                                                         (prim 'cons (prim 'car xs) (prim 'car rest))
                                                         (prim 'car (prim 'cdr rest))))))))))

    (partition . (fix partition (fn (p xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((x (prim 'car xs))
                                              (rest (partition p (prim 'cdr xs))))
                                             (if (p x)
                                                 (prim 'list
                                                       (prim 'cons x (prim 'car rest))
                                                       (prim 'car (prim 'cdr rest)))
                                                 (prim 'list
                                                       (prim 'car rest)
                                                       (prim 'cons x (prim 'car (prim 'cdr rest))))))))))

    (zipWith . (fix zipWith (fn (f xs ys)
                                (if (prim 'null? xs)
                                    '()
                                    (if (prim 'null? ys)
                                        '()
                                        (prim 'cons (f (prim 'car xs) (prim 'car ys))
                                              (zipWith f (prim 'cdr xs) (prim 'cdr ys))))))))

    (unzip   . (fix unzip (fn (pairs)
                              (if (prim 'null? pairs)
                                  (prim 'list '() '())
                                  (let ((p (prim 'car pairs))
                                        (rest (unzip (prim 'cdr pairs))))
                                       (prim 'list
                                             (prim 'cons (prim 'car p)
                                                   (prim 'car rest))
                                             (prim 'cons (prim 'car (prim 'cdr p))
                                                   (prim 'car (prim 'cdr rest)))))))))

    (intersperse . (fix intersperse (fn (sep xs)
                                        (if (prim 'null? xs)
                                            '()
                                            (if (prim 'null? (prim 'cdr xs))
                                                xs
                                                (prim 'cons (prim 'car xs)
                                                      (prim 'cons sep
                                                            (intersperse sep (prim 'cdr xs)))))))))

    (group   . (fix group (fn (xs)
                              (if (prim 'null? xs)
                                  '()
                                  (let ((x (prim 'car xs)))
                                       (let ((rest-result (span (fn (y) (prim 'eq? x y)) (prim 'cdr xs))))
                                            (prim 'cons
                                                  (prim 'cons x (prim 'car rest-result))
                                                  (group (prim 'car (prim 'cdr rest-result))))))))))

    (nub     . (fix nub (fn (xs)
                            (if (prim 'null? xs)
                                '()
                                (let ((x (prim 'car xs)))
                                     (prim 'cons x (nub (filter (fn (y) (prim 'not (prim 'eq? x y))) (prim 'cdr xs)))))))))

    (find    . (fix find (fn (p xs)
                             (if (prim 'null? xs)
                                 'none
                                 (if (p (prim 'car xs))
                                     (prim 'cons 'some (prim 'car xs))
                                     (find p (prim 'cdr xs)))))))

    (splitAt . (fix splitAt (fn (n xs)
                                (if (prim 'zero? n)
                                    (prim 'list '() xs)
                                    (if (prim 'null? xs)
                                        (prim 'list '() '())
                                        (let ((rest (splitAt (prim 'sub n 1) (prim 'cdr xs))))
                                             (prim 'list
                                                   (prim 'cons (prim 'car xs) (prim 'car rest))
                                                   (prim 'car (prim 'cdr rest)))))))))

    (doc 'section 'type-class-implementations)

    (list-fmap . (fix list-fmap (fn (f xs)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'cons (f (prim 'car xs))
                                              (list-fmap f (prim 'cdr xs)))))))

    (option-fmap . (fn (f opt)
                       (if (prim 'eq? opt 'none)
                           'none
                           (prim 'cons 'some (f (prim 'cdr opt))))))

    (either-fmap . (fn (f e)
                       (if (prim 'eq? (prim 'car e) 'left)
                           e
                           (prim 'cons 'right (f (prim 'cdr e))))))

    (list-pure . (fn (x) (prim 'list x)))

    (list-ap . (fix list-ap (fn (fs xs)
                                (if (prim 'null? fs)
                                    '()
                                    (prim 'append
                                          (map (prim 'car fs) xs)
                                          (list-ap (prim 'cdr fs) xs))))))

    (option-pure . (fn (x) (prim 'cons 'some x)))

    (option-ap . (fn (mf mx)
                     (if (prim 'eq? mf 'none)
                         'none
                         (if (prim 'eq? mx 'none)
                             'none
                             (prim 'cons 'some
                                   ((prim 'cdr mf) (prim 'cdr mx)))))))

    (list-bind . (fix list-bind (fn (xs f)
                                    (if (prim 'null? xs)
                                        '()
                                        (prim 'append (f (prim 'car xs))
                                              (list-bind (prim 'cdr xs) f))))))

    (list-return . (fn (x) (prim 'list x)))

    (option-bind . (fn (mx f)
                       (if (prim 'eq? mx 'none)
                           'none
                           (f (prim 'cdr mx)))))

    (option-return . (fn (x) (prim 'cons 'some x)))

    (either-bind . (fn (mx f)
                       (if (prim 'eq? (prim 'car mx) 'left)
                           mx
                           (f (prim 'cdr mx)))))

    (either-return . (fn (x) (prim 'cons 'right x)))

    (nat-eq . (fn (a b) (prim 'eq? a b)))
    (nat-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (int-eq . (fn (a b) (prim 'eq? a b)))
    (int-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (bool-eq . (fn (a b) (prim 'eq? a b)))
    (bool-neq . (fn (a b) (prim 'not (prim 'eq? a b))))
    (char-eq . (fn (a b) (prim 'char=? a b)))
    (char-neq . (fn (a b) (prim 'not (prim 'char=? a b))))
    (string-eq . (fn (a b) (prim 'string=? a b)))
    (string-neq . (fn (a b) (prim 'not (prim 'string=? a b))))
    (symbol-eq . (fn (a b) (prim 'eq? a b)))
    (symbol-neq . (fn (a b) (prim 'not (prim 'eq? a b))))

    (list-eq . (fix list-eq (fn (xs ys)
                                (if (prim 'null? xs)
                                    (prim 'null? ys)
                                    (if (prim 'null? ys)
                                        #f
                                        (if (prim 'eq? (prim 'car xs) (prim 'car ys))
                                            (list-eq (prim 'cdr xs) (prim 'cdr ys))
                                            #f))))))

    (list-neq . (fn (xs ys) (prim 'not (list-eq xs ys))))

    (option-eq . (fn (a b)
                     (if (prim 'eq? a 'none)
                         (prim 'eq? b 'none)
                         (if (prim 'eq? b 'none)
                             #f
                             (prim 'eq? (prim 'cdr a) (prim 'cdr b))))))

    (nat-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (nat-lt . (fn (a b) (prim 'lt? a b)))
    (nat-lte . (fn (a b) (prim 'le? a b)))
    (nat-gt . (fn (a b) (prim 'gt? a b)))
    (nat-gte . (fn (a b) (prim 'ge? a b)))

    (int-compare . (fn (a b)
                       (if (prim 'lt? a b) 'LT
                           (if (prim 'eq? a b) 'EQ 'GT))))
    (int-lt . (fn (a b) (prim 'lt? a b)))
    (int-lte . (fn (a b) (prim 'le? a b)))
    (int-gt . (fn (a b) (prim 'gt? a b)))
    (int-gte . (fn (a b) (prim 'ge? a b)))

    (char-compare . (fn (a b)
                        (if (prim 'char<? a b) 'LT
                            (if (prim 'char=? a b) 'EQ 'GT))))
    (char-lt . (fn (a b) (prim 'char<? a b)))
    (char-lte . (fn (a b) (prim 'or (prim 'char<? a b) (prim 'char=? a b))))
    (char-gt . (fn (a b) (prim 'char<? b a)))
    (char-gte . (fn (a b) (prim 'or (prim 'char<? b a) (prim 'char=? a b))))

    (string-compare . (fn (a b)
                          (if (prim 'string<? a b) 'LT
                              (if (prim 'string=? a b) 'EQ 'GT))))
    (string-lt . (fn (a b) (prim 'string<? a b)))
    (string-lte . (fn (a b) (prim 'or (prim 'string<? a b) (prim 'string=? a b))))
    (string-gt . (fn (a b) (prim 'string>? a b)))
    (string-gte . (fn (a b) (prim 'or (prim 'string>? a b) (prim 'string=? a b))))

    (nat-show . (fn (n) (prim 'number->string n)))
    (int-show . (fn (n) (prim 'number->string n)))
    (bool-show . (fn (b) (if b "#t" "#f")))
    (char-show . (fn (c) (prim 'list->string (prim 'list c))))
    (string-show . (fn (s) s))
    (symbol-show . (fn (s) (prim 'symbol->string s)))

    (list-show . (fix list-show (fn (xs)
                                    (if (prim 'null? xs)
                                        "()"
                                        (prim 'string-append
                                              "("
                                              (prim 'string-append
                                                    (nat-show (prim 'car xs))
                                                    (prim 'string-append
                                                          (list-show-rest (prim 'cdr xs))
                                                          ")")))))))

    (list-show-rest . (fix list-show-rest (fn (xs)
                                              (if (prim 'null? xs)
                                                  ""
                                                  (prim 'string-append
                                                        " "
                                                        (prim 'string-append
                                                              (nat-show (prim 'car xs))
                                                              (list-show-rest (prim 'cdr xs))))))))

    (list-append . (fn (xs ys) (prim 'append xs ys)))

    (list-empty . '())))

(define (build-prelude-env fuel)
  (doc 'type (-> Fuel Env))
  (doc 'description "Build the prelude environment by evaluating definitions.")
  (doc 'export #t)
  (let loop ([defs prelude-defs] [env empty-env] [remaining fuel])
       (if (null? defs)
           env
           (let* ([def (car defs)]
                  [name (car def)]
                  [expr (cdr def)]
                  [result (eval-expr expr env remaining)])
                 (if (eq? (car result) 'ok)
                     (loop (cdr defs)
                           (env-extend env name (cadr result))
                           (caddr result))
                     env)))))

(define (run-prelude expr fuel)
  (doc 'type (-> Expr Fuel Result))
  (doc 'description "Evaluate with the standard prelude.")
  (doc 'export #t)
  (let ([prelude-env (build-prelude-env 1000)])
       (eval-expr expr prelude-env fuel)))

