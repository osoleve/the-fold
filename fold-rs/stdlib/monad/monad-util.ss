; ============================================================
; Additional Monad Utilities
; Maybe and Either monad operations
; ============================================================

; bind-maybe: Monadic bind for Maybe
(bind-maybe (fn (m f)
               (if (nothing? m)
                   m
                   (f (from-just m)))))

; sequence-maybes: Sequence list of maybes into maybe of list
(sequence-maybes (fn (ms)
                    (foldl (fn (acc m)
                               (if (nothing? acc)
                                   acc
                                   (if (nothing? m)
                                       m
                                       (just (append (from-just acc) (list (from-just m)))))))
                           (just '())
                           ms)))

; traverse-maybe: Map and sequence for Maybe
(traverse-maybe (fn (f lst)
                   (sequence-maybes (map f lst))))

; ap-maybe: Applicative apply for Maybe
(ap-maybe (fn (mf mx)
             (if (nothing? mf)
                 mf
                 (if (nothing? mx)
                     mx
                     (just ((from-just mf) (from-just mx)))))))

; maybe-to-list: Convert Maybe to list
(maybe-to-list (fn (m)
                  (if (nothing? m)
                      '()
                      (list (from-just m)))))

; list-to-maybe: Convert list to Maybe
(list-to-maybe (fn (lst)
                  (if (null? lst)
                      nothing
                      (just (car lst)))))

; cat-maybes: Filter and extract Just values
(cat-maybes (fn (lst)
               (map from-just (filter (fn (m) (not (nothing? m))) lst))))

; bind-either: Monadic bind for Either
(bind-either (fn (m f)
                (if (left? m)
                    m
                    (f (from-right m)))))

; sequence-eithers: Sequence list of eithers into either of list
(sequence-eithers (fn (es)
                     (foldl (fn (acc e)
                                (if (left? acc)
                                    acc
                                    (if (left? e)
                                        e
                                        (right (append (from-right acc) (list (from-right e)))))))
                            (right '())
                            es)))

; traverse-either: Map and sequence for Either
(traverse-either (fn (f lst)
                    (sequence-eithers (map f lst))))

; ap-either: Applicative apply for Either
(ap-either (fn (ef ex)
              (if (left? ef)
                  ef
                  (if (left? ex)
                      ex
                      (right ((from-right ef) (from-right ex)))))))

; either-to-maybe: Convert Either to Maybe
(either-to-maybe (fn (e)
                    (if (left? e)
                        nothing
                        (just (from-right e)))))

; maybe-to-either: Convert Maybe to Either
(maybe-to-either (fn (err m)
                    (if (nothing? m)
                        (left err)
                        (right (from-just m)))))
