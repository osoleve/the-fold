(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")

(doc 'module 'optics)
(doc 'description "Comprehensive Optics Tower - A complete hierarchy of optics for composable data access")
(doc 'layer 'lattice)
(doc 'purity 'total)

(doc 'section 'iso)
(doc 'description "An isomorphism represents a reversible transformation. Iso s t a b means: s ≅ a and t ≅ b with consistent transformations. For simple isos: Iso' s a means s ≅ a. Laws: forward . backward = id, backward . forward = id")

(define (make-iso forward backward)
  (doc 'type '(-> (-> s a) (-> b t) (Iso s t a b)))
  (doc 'description "For simple isos, s=t and a=b, so: (s → a) × (a → s)")
  (list 'iso forward backward))

;;; iso? : α → Boolean
(define (iso? x)
  (and (pair? x) (eq? (car x) 'iso)))

;;; iso-forward : Iso s t a b → (s → a)
(define (iso-forward i)
  (cadr i))

;;; iso-backward : Iso s t a b → (b → t)
(define (iso-backward i)
  (caddr i))

;;; iso-view : Iso × s → a
(define (iso-view iso s)
  ((iso-forward iso) s))

;;; iso-review : Iso × b → t
(define (iso-review iso b)
  ((iso-backward iso) b))

;;; iso-over : Iso × (a → b) × s → t
(define (iso-over iso f s)
  ((iso-backward iso) (f ((iso-forward iso) s))))

;;; iso-flip : Iso s t a b → Iso b a t s
;;; Reverse the direction of an iso.
(define (iso-flip iso)
  (make-iso (iso-backward iso) (iso-forward iso)))

;;; iso-compose : Iso s t a b × Iso a b c d → Iso s t c d
(define (iso-compose outer inner)
  (make-iso
   (compose2 (iso-forward inner) (iso-forward outer))
   (compose2 (iso-backward outer) (iso-backward inner))))

;;; ====
;;; Common Isomorphisms
;;; ====

;;; iso-id : Iso a a a a
;;; Identity isomorphism.
(define iso-id
  (make-iso identity identity))

;;; iso-curried : Iso ((a, b) → c) (a → b → c)
;;; Curry/uncurry isomorphism.
(define iso-curried
  (make-iso
   (lambda (f) (lambda (a) (lambda (b) (f (cons a b)))))
   (lambda (f) (lambda (ab) ((f (car ab)) (cdr ab))))))

;;; iso-flipped : Iso (a → b → c) (b → a → c)
;;; Argument flip isomorphism.
(define iso-flipped
  (make-iso flip flip))

;;; iso-swapped : Iso (a, b) (b, a)
;;; Swap pair components.
(define iso-swapped
  (make-iso
   (lambda (p) (cons (cdr p) (car p)))
   (lambda (p) (cons (cdr p) (car p)))))

;;; iso-reversed : Iso (List a) (List a)
;;; List reversal (self-inverse).
(define iso-reversed
  (make-iso reverse reverse))

;;; iso-assoc-list : Iso ((a, b), c) (a, (b, c))
;;; Pair association.
(define iso-assoc-list
  (make-iso
   (lambda (abc) (cons (caar abc) (cons (cdar abc) (cdr abc))))
   (lambda (abc) (cons (cons (car abc) (cadr abc)) (cddr abc)))))

;;; iso-maybe-either : Iso (Maybe a) (Either () a)
;;; Maybe as Either with unit left.
(define iso-maybe-either
  (make-iso
   (lambda (m) (if (just? m) (right (from-just m)) (left '())))
   (lambda (e) (if (right? e) (just (from-right e)) nothing))))

;;; iso-cons : Iso (a, List a) (NonEmpty a)
;;; Pair of head+tail as non-empty list (represented as regular list).
(define iso-cons
  (make-iso
   (lambda (p) (cons (car p) (cdr p)))
   (lambda (xs) (cons (car xs) (cdr xs)))))

;;; ============================================================
;;; Part 2: Lenses (re-export from templates for completeness)
;;; ============================================================
;;;
;;; A lens focuses on exactly one part of a product type.
;;; Lens s t a b means: s contains an a, and replacing with b gives t.
;;; For simple lenses: Lens' s a means s contains exactly one a.
;;;
;;; Laws:
;;;   - Get-Put: set l (view l s) s = s
;;;   - Put-Get: view l (set l a s) = a
;;;   - Put-Put: set l a' (set l a s) = set l a' s

;;; Re-export lens primitives (already in templates.ss)
;;; make-lens, lens?, lens-getter, lens-setter
;;; view, set-lens, over, lens-compose
;;; lens-fst, lens-snd, lens-head, lens-tail, lens-nth, lens-key

;;; lens-id : Lens a a
;;; Identity lens.
(define lens-id
  (make-lens identity (lambda (a _s) a)))

;;; ============================================================
;;; Part 3: Prisms (re-export and extend from templates)
;;; ============================================================
;;;
;;; A prism focuses on one variant of a sum type.
;;; Prism s t a b means: s might contain an a; can build t from b.
;;;
;;; Laws:
;;;   - Preview-Review: preview p (review p a) = Just a
;;;   - Review-Preview: preview p s = Just a implies review p a = s

;;; Re-export prism primitives (already in templates.ss)
;;; make-prism, prism?, prism-match, prism-build
;;; preview, review
;;; prism-just, prism-left, prism-right

;;; prism-over : Prism × (a → b) × s → t
;;; Modify through a prism if target exists.
(define (prism-over prism f s)
  (let ([maybe-a (preview prism s)])
    (if (nothing? maybe-a)
        s
        (review prism (f (from-just maybe-a))))))

;;; prism-set : Prism × b × s → t
;;; Set through a prism if target exists.
(define (prism-set prism b s)
  (prism-over prism (const b) s))

;;; prism-compose : Prism s t a b × Prism a b c d → Prism s t c d
(define (prism-compose outer inner)
  (make-prism
   ;; match: s → Maybe c
   (lambda (s)
     (let ([maybe-a ((prism-match outer) s)])
       (if (nothing? maybe-a)
           nothing
           ((prism-match inner) (from-just maybe-a)))))
   ;; build: d → t
   (lambda (d)
     ((prism-build outer) ((prism-build inner) d)))))

;;; prism-id : Prism a a
;;; Identity prism.
(define prism-id
  (make-prism just identity))

;;; prism-nil : Prism (List a) ()
;;; Prism matching empty list.
(define prism-nil
  (make-prism
   (lambda (xs) (if (null? xs) (just '()) nothing))
   (lambda (_) '())))

;;; prism-cons : Prism (List a) (a, List a)
;;; Prism matching non-empty list as head-tail pair.
(define prism-cons
  (make-prism
   (lambda (xs) (if (null? xs) nothing (just (cons (car xs) (cdr xs)))))
   (lambda (p) (cons (car p) (cdr p)))))

;;; affine-nth : Nat → Affine (List a) a
;;; Affine focusing on nth element if it exists.
;;; Note: This is an Affine, not a Prism, because we cannot lawfully
;;; "review" a single element back into a list without knowing the context.
(define (affine-nth n)
  (make-affine
   (lambda (xs)
     (if (< n (length xs))
         (just (list-ref xs n))
         nothing))
   (lambda (a xs)
     (if (< n (length xs))
         (list-set-at xs n a)
         xs))))

;;; list-set-at : List × Nat × a → List
;;; Set element at index.
(define (list-set-at xs n a)
  (let loop ([i 0] [remaining xs] [acc '()])
    (if (null? remaining)
        (reverse acc)
        (if (= i n)
            (loop (+ i 1) (cdr remaining) (cons a acc))
            (loop (+ i 1) (cdr remaining) (cons (car remaining) acc))))))

;;; ============================================================
;;; Part 4: Affines (Partial Lenses)
;;; ============================================================
;;;
;;; An affine focuses on at most one target. It's the intersection
;;; of Lens and Prism capabilities: can get (maybe) and set.
;;;
;;; Affine s t a b: s might contain an a; can set b to get t.
;;;
;;; Laws:
;;;   - Get-Set: set l (preview l s) s = s (when preview returns Just)
;;;   - Set-Get: preview l (set l a s) = Just a (when preview returns Just)

;;; make-affine : (s → Maybe a) × (b → s → t) → Affine s t a b
(define (make-affine getter setter)
  (list 'affine getter setter))

;;; affine? : α → Boolean
(define (affine? x)
  (and (pair? x) (eq? (car x) 'affine)))

;;; affine-getter : Affine → (s → Maybe a)
(define (affine-getter a)
  (cadr a))

;;; affine-setter : Affine → (b → s → t)
(define (affine-setter a)
  (caddr a))

;;; affine-preview : Affine × s → Maybe a
(define (affine-preview affine s)
  ((affine-getter affine) s))

;;; affine-set : Affine × b × s → t
(define (affine-set affine b s)
  ((affine-setter affine) b s))

;;; affine-over : Affine × (a → b) × s → t
;;; Modify if target exists.
(define (affine-over affine f s)
  (let ([maybe-a ((affine-getter affine) s)])
    (if (nothing? maybe-a)
        s
        ((affine-setter affine) (f (from-just maybe-a)) s))))

;;; affine-compose : Affine s t a b × Affine a b c d → Affine s t c d
(define (affine-compose outer inner)
  (make-affine
   ;; getter: s → Maybe c
   (lambda (s)
     (let ([maybe-a ((affine-getter outer) s)])
       (if (nothing? maybe-a)
           nothing
           ((affine-getter inner) (from-just maybe-a)))))
   ;; setter: d → s → t
   (lambda (d s)
     (let ([maybe-a ((affine-getter outer) s)])
       (if (nothing? maybe-a)
           s
           ((affine-setter outer)
            ((affine-setter inner) d (from-just maybe-a))
            s))))))

;;; affine-id : Affine a a
(define affine-id
  (make-affine just (lambda (a _s) a)))

;;; ====
;;; Conversions to Affine
;;; ====

;;; lens->affine : Lens s t a b → Affine s t a b
(define (lens->affine lens)
  (make-affine
   (lambda (s) (just ((lens-getter lens) s)))
   (lens-setter lens)))

;;; prism->affine : Prism s t a b → Affine s t a b
(define (prism->affine prism)
  (make-affine
   (prism-match prism)
   (lambda (b s)
     (let ([maybe-a ((prism-match prism) s)])
       (if (nothing? maybe-a)
           s
           ((prism-build prism) b))))))

;;; ============================================================
;;; Part 5: Traversals
;;; ============================================================
;;;
;;; A traversal focuses on zero or more targets within a structure.
;;; It generalizes lenses (one target) and folds (read-only).
;;;
;;; Traversal s t a b: s contains zero or more a's; replacing all
;;; with b's gives t.
;;;
;;; We represent traversals as functions that work with applicative
;;; functors to maintain the structure while transforming elements.

;;; make-traversal : ((a → F b) → s → F t) → Traversal s t a b
;;; The function takes an applicative-lifted transformation.
;;; For concrete ops, we provide direct list-based accessors.
(define (make-traversal traverse-fn fold-targets)
  (list 'traversal traverse-fn fold-targets))

;;; traversal? : α → Boolean
(define (traversal? x)
  (and (pair? x) (eq? (car x) 'traversal)))

;;; traversal-traverse : Traversal → ((a → F b) → s → F t)
(define (traversal-traverse t)
  (cadr t))

;;; traversal-fold : Traversal → (s → List a)
;;; Get all targets as a list.
(define (traversal-fold t)
  (caddr t))

;;; traversal-to-list : Traversal × s → List a
(define (traversal-to-list trav s)
  ((traversal-fold trav) s))

;;; traversal-over : Traversal × (a → b) × s → t
;;; Modify all targets.
(define (traversal-over trav f s)
  ;; Use identity applicative for pure mapping
  ((traversal-traverse trav)
   (lambda (a) (f a))
   s))

;;; traversal-set : Traversal × b × s → t
;;; Set all targets to same value.
(define (traversal-set trav b s)
  (traversal-over trav (const b) s))

;;; traversal-compose : Traversal s t a b × Traversal a b c d → Traversal s t c d
(define (traversal-compose outer inner)
  (make-traversal
   ;; traverse: (c → F d) → s → F t
   (lambda (f s)
     ((traversal-traverse outer)
      (lambda (a) ((traversal-traverse inner) f a))
      s))
   ;; fold: s → List c
   (lambda (s)
     (apply append
            (map (traversal-fold inner)
                 ((traversal-fold outer) s))))))

;;; ====
;;; Common Traversals
;;; ====

;;; traversal-each : Traversal (List a) (List b) a b
;;; Traverse each element of a list.
(define traversal-each
  (make-traversal
   (lambda (f xs) (map f xs))
   identity))

;;; traversal-filtered : (a → Boolean) → Traversal (List a) (List a) a a
;;; Traverse only elements matching predicate.
(define (traversal-filtered pred)
  (make-traversal
   (lambda (f xs)
     (map (lambda (x) (if (pred x) (f x) x)) xs))
   (lambda (xs) (filter pred xs))))

;;; traversal-both : Traversal (a, a) (b, b) a b
;;; Traverse both elements of a pair of same type.
(define traversal-both
  (make-traversal
   (lambda (f p) (cons (f (car p)) (f (cdr p))))
   (lambda (p) (list (car p) (cdr p)))))

;;; traversal-left : Traversal (Either a c) (Either b c) a b
;;; Traverse Left values.
(define traversal-left
  (make-traversal
   (lambda (f e) (if (left? e) (left (f (from-left e))) e))
   (lambda (e) (if (left? e) (list (from-left e)) '()))))

;;; traversal-right : Traversal (Either c a) (Either c b) a b
;;; Traverse Right values.
(define traversal-right
  (make-traversal
   (lambda (f e) (if (right? e) (right (f (from-right e))) e))
   (lambda (e) (if (right? e) (list (from-right e)) '()))))

;;; traversal-just : Traversal (Maybe a) (Maybe b) a b
;;; Traverse Just values.
(define traversal-just
  (make-traversal
   (lambda (f m) (if (just? m) (just (f (from-just m))) nothing))
   (lambda (m) (if (just? m) (list (from-just m)) '()))))

;;; ====
;;; Conversions to Traversal
;;; ====

;;; lens->traversal : Lens s t a b → Traversal s t a b
(define (lens->traversal lens)
  (make-traversal
   (lambda (f s) (set-lens lens (f (view lens s)) s))
   (lambda (s) (list (view lens s)))))

;;; prism->traversal : Prism s t a b → Traversal s t a b
(define (prism->traversal prism)
  (make-traversal
   (lambda (f s)
     (let ([maybe-a (preview prism s)])
       (if (nothing? maybe-a)
           s
           (review prism (f (from-just maybe-a))))))
   (lambda (s)
     (let ([maybe-a (preview prism s)])
       (if (nothing? maybe-a) '() (list (from-just maybe-a)))))))

;;; affine->traversal : Affine s t a b → Traversal s t a b
(define (affine->traversal affine)
  (make-traversal
   (lambda (f s)
     (let ([maybe-a (affine-preview affine s)])
       (if (nothing? maybe-a)
           s
           (affine-set affine (f (from-just maybe-a)) s))))
   (lambda (s)
     (let ([maybe-a (affine-preview affine s)])
       (if (nothing? maybe-a) '() (list (from-just maybe-a)))))))

;;; ============================================================
;;; Part 6: Folds (Read-Only Traversals)
;;; ============================================================
;;;
;;; A fold provides read-only access to zero or more targets.
;;; It's the read-only version of a traversal.

;;; make-fold : (s → List a) → Fold s a
(define (make-fold fold-fn)
  (list 'fold fold-fn))

;;; fold-optic? : α → Boolean
(define (fold-optic? x)
  (and (pair? x) (eq? (car x) 'fold)))

;;; fold-optic-fn : Fold → (s → List a)
(define (fold-optic-fn f)
  (cadr f))

;;; fold-to-list : Fold × s → List a
(define (fold-to-list fold s)
  ((fold-optic-fn fold) s))

;;; fold-preview : Fold × s → Maybe a
;;; Get first target if any.
(define (fold-preview fold s)
  (let ([targets ((fold-optic-fn fold) s)])
    (if (null? targets) nothing (just (car targets)))))

;;; fold-has : Fold × s → Boolean
;;; Check if any target exists.
(define (fold-has fold s)
  (not (null? ((fold-optic-fn fold) s))))

;;; fold-length : Fold × s → Nat
;;; Count targets.
(define (fold-length fold s)
  (length ((fold-optic-fn fold) s)))

;;; fold-all : Fold × (a → Boolean) × s → Boolean
;;; Check if all targets satisfy predicate.
(define (fold-all fold pred s)
  (andmap pred ((fold-optic-fn fold) s)))

;;; fold-any : Fold × (a → Boolean) × s → Boolean
;;; Check if any target satisfies predicate.
(define (fold-any fold pred s)
  (ormap pred ((fold-optic-fn fold) s)))

;;; fold-sum : Fold × s → Number
;;; Sum numeric targets.
(define (fold-sum fold s)
  (apply + ((fold-optic-fn fold) s)))

;;; fold-compose : Fold s a × Fold a b → Fold s b
(define (fold-compose outer inner)
  (make-fold
   (lambda (s)
     (apply append
            (map (fold-optic-fn inner)
                 ((fold-optic-fn outer) s))))))

;;; ====
;;; Common Folds
;;; ====

;;; fold-each : Fold (List a) a
(define fold-each
  (make-fold identity))

;;; fold-filtered : (a → Boolean) → Fold (List a) a
(define (fold-filtered pred)
  (make-fold (lambda (xs) (filter pred xs))))

;;; fold-taking : Nat → Fold (List a) a
(define (fold-taking n)
  (make-fold (lambda (xs) (take-up-to n xs))))

;;; take-up-to : Nat × List → List
(define (take-up-to n xs)
  (if (or (<= n 0) (null? xs))
      '()
      (cons (car xs) (take-up-to (- n 1) (cdr xs)))))

;;; fold-dropping : Nat → Fold (List a) a
(define (fold-dropping n)
  (make-fold (lambda (xs) (drop-up-to n xs))))

;;; drop-up-to : Nat × List → List
(define (drop-up-to n xs)
  (if (or (<= n 0) (null? xs))
      xs
      (drop-up-to (- n 1) (cdr xs))))

;;; ====
;;; Conversions to Fold
;;; ====

;;; traversal->fold : Traversal s t a b → Fold s a
(define (traversal->fold trav)
  (make-fold (traversal-fold trav)))

;;; lens->fold : Lens s a → Fold s a
(define (lens->fold lens)
  (make-fold (lambda (s) (list (view lens s)))))

;;; prism->fold : Prism s a → Fold s a
(define (prism->fold prism)
  (make-fold (lambda (s)
    (let ([maybe-a (preview prism s)])
      (if (nothing? maybe-a) '() (list (from-just maybe-a)))))))

;;; ============================================================
;;; Part 7: Getters (Read-Only Lenses)
;;; ============================================================
;;;
;;; A getter provides read-only access to exactly one target.

;;; make-getter : (s → a) → Getter s a
(define (make-getter get-fn)
  (list 'getter get-fn))

;;; getter? : α → Boolean
(define (getter? x)
  (and (pair? x) (eq? (car x) 'getter)))

;;; getter-fn : Getter → (s → a)
(define (getter-fn g)
  (cadr g))

;;; getter-view : Getter × s → a
(define (getter-view getter s)
  ((getter-fn getter) s))

;;; getter-compose : Getter s a × Getter a b → Getter s b
(define (getter-compose outer inner)
  (make-getter
   (compose2 (getter-fn inner) (getter-fn outer))))

;;; ====
;;; Common Getters
;;; ====

;;; getter-id : Getter a a
(define getter-id
  (make-getter identity))

;;; getter-fst : Getter (a, b) a
(define getter-fst
  (make-getter car))

;;; getter-snd : Getter (a, b) b
(define getter-snd
  (make-getter cdr))

;;; getter-to : (s → a) → Getter s a
;;; Lift any function to a getter.
(define getter-to make-getter)

;;; ====
;;; Conversions to Getter
;;; ====

;;; lens->getter : Lens s a → Getter s a
(define (lens->getter lens)
  (make-getter (lens-getter lens)))

;;; iso->getter : Iso s a → Getter s a
(define (iso->getter iso)
  (make-getter (iso-forward iso)))

;;; ============================================================
;;; Part 8: Setters (Write-Only Traversals)
;;; ============================================================
;;;
;;; A setter provides write-only modification of zero or more targets.

;;; make-setter : ((a → b) → s → t) → Setter s t a b
(define (make-setter over-fn)
  (list 'setter over-fn))

;;; setter? : α → Boolean
(define (setter? x)
  (and (pair? x) (eq? (car x) 'setter)))

;;; setter-over-fn : Setter → ((a → b) → s → t)
(define (setter-over-fn s)
  (cadr s))

;;; setter-over : Setter × (a → b) × s → t
(define (setter-over setter f s)
  ((setter-over-fn setter) f s))

;;; setter-set : Setter × b × s → t
(define (setter-set setter b s)
  ((setter-over-fn setter) (const b) s))

;;; setter-compose : Setter s t a b × Setter a b c d → Setter s t c d
(define (setter-compose outer inner)
  (make-setter
   (lambda (f s)
     ((setter-over-fn outer)
      (lambda (a) ((setter-over-fn inner) f a))
      s))))

;;; ====
;;; Common Setters
;;; ====

;;; setter-mapped : Setter (List a) (List b) a b
;;; Set over list elements.
(define setter-mapped
  (make-setter map))

;;; setter-arg : Setter (a → r) (b → r) a b
;;; Modify function argument (contravariant).
(define setter-arg
  (make-setter
   (lambda (f g) (compose2 g f))))

;;; setter-result : Setter (r → a) (r → b) a b
;;; Modify function result (covariant).
(define setter-result
  (make-setter
   (lambda (f g) (compose2 f g))))

;;; ====
;;; Conversions to Setter
;;; ====

;;; lens->setter : Lens s t a b → Setter s t a b
(define (lens->setter lens)
  (make-setter (lambda (f s) (over lens f s))))

;;; traversal->setter : Traversal s t a b → Setter s t a b
(define (traversal->setter trav)
  (make-setter (lambda (f s) (traversal-over trav f s))))

;;; iso->setter : Iso s t a b → Setter s t a b
(define (iso->setter iso)
  (make-setter (lambda (f s) (iso-over iso f s))))

;;; ============================================================
;;; Part 9: Grates (Dual of Lenses)
;;; ============================================================
;;;
;;; A grate focuses on "closed" structures where you can zip
;;; multiple instances together. It's the categorical dual of a lens.
;;;
;;; Grate s t a b: Given any way to extract 'a' from 's' and combine
;;; the results into 'b', produce a 't'.
;;;
;;; The representation is: ((s → a) → b) → t
;;;
;;; Key operations:
;;;   - zipWithOf: Combine two structures element-wise
;;;   - cotraverse: Dual of traverse, works with functors
;;;   - review: Inject a constant value
;;;
;;; Laws (for Grate' s a where s=t and a=b):
;;;   - Identity: grate-over id = id
;;;   - Composition: grate-over g . grate-over f = grate-over (g . f)

;;; make-grate : (((s → a) → b) → t) → Grate s t a b
(define (make-grate cotraverse-fn)
  (list 'grate cotraverse-fn))

;;; grate? : α → Boolean
(define (grate? x)
  (and (pair? x) (eq? (car x) 'grate)))

;;; grate-cotraverse-fn : Grate → (((s → a) → b) → t)
(define (grate-cotraverse-fn g)
  (cadr g))

;;; grate-review : Grate × b → t
;;; Inject a constant value through the grate.
;;; review g b = cotraverse (\_ -> b)
(define (grate-review grate b)
  ((grate-cotraverse-fn grate) (lambda (_) b)))

;;; grate-over : Grate × (a → b) × s → t
;;; Modify through the grate.
;;; over g f s = cotraverse (\sa -> f (sa s))
(define (grate-over grate f s)
  ((grate-cotraverse-fn grate)
   (lambda (sa) (f (sa s)))))

;;; grate-set : Grate × b × s → t
;;; Set through the grate (ignores structure, just reviews).
(define (grate-set grate b _s)
  (grate-review grate b))

;;; grate-zipWith : Grate × (a → a → b) × s × s → t
;;; The defining operation of grates: zip two structures together.
;;; zipWithOf g f s1 s2 = cotraverse (\sa -> f (sa s1) (sa s2))
(define (grate-zipWith grate f s1 s2)
  ((grate-cotraverse-fn grate)
   (lambda (sa) (f (sa s1) (sa s2)))))

;;; grate-zipWith3 : Grate × (a → a → a → b) × s × s × s → t
;;; Zip three structures together.
(define (grate-zipWith3 grate f s1 s2 s3)
  ((grate-cotraverse-fn grate)
   (lambda (sa) (f (sa s1) (sa s2) (sa s3)))))

;;; grate-zip : Grate × s × s → t
;;; Zip two structures into pairs (when a = b = pair type).
;;; Specialized zipWith with cons.
(define (grate-zip grate s1 s2)
  (grate-zipWith grate cons s1 s2))

;;; grate-compose : Grate s t a b × Grate a b c d → Grate s t c d
(define (grate-compose outer inner)
  (make-grate
   (lambda (sctod)
     ;; outer : ((s → a) → b) → t
     ;; inner : ((a → c) → d) → b
     ;; need: ((s → c) → d) → t
     ((grate-cotraverse-fn outer)
      (lambda (sa)
        ;; sa : s → a
        ;; need: b
        ((grate-cotraverse-fn inner)
         (lambda (ac)
           ;; ac : a → c
           ;; need: d
           (sctod (compose2 ac sa)))))))))

;;; ====
;;; Common Grates
;;; ====

;;; grate-id : Grate a a
;;; Identity grate.
(define grate-id
  (make-grate
   (lambda (satob) (satob identity))))

;;; grate-fn : Grate (x → a) (x → b) a b
;;; The canonical grate for functions.
;;; Functions can be "zipped" by applying multiple functions to the same input.
(define grate-fn
  (make-grate
   (lambda (satob)
     ;; satob : ((x → a) → b)
     ;; result: x → b
     (lambda (x)
       (satob (lambda (f) (f x)))))))

;;; grate-pair-same : Grate (a . a) (b . b) a b
;;; Grate for pairs where both elements have the same type.
;;; Allows zipping pairs together.
(define grate-pair-same
  (make-grate
   (lambda (satob)
     (cons
      (satob car)
      (satob cdr)))))

;;; grate-list-rep : Nat → Grate (List a) (List b) a b
;;; Grate for fixed-length lists (representable functor).
;;; Only valid when all lists have exactly length n.
(define (grate-list-rep n)
  (make-grate
   (lambda (satob)
     (let loop ([i 0] [acc '()])
       (if (>= i n)
           (reverse acc)
           (loop (+ i 1)
                 (cons (satob (lambda (xs) (list-ref xs i))) acc)))))))

;;; ====
;;; Conversions to/from Grate
;;; ====

;;; iso->grate : Iso s t a b → Grate s t a b
;;; Every iso is a grate.
(define (iso->grate iso)
  (make-grate
   (lambda (satob)
     ((iso-backward iso)
      (satob (iso-forward iso))))))

;;; grate->setter : Grate s t a b → Setter s t a b
;;; Grates can be used as setters.
(define (grate->setter grate)
  (make-setter
   (lambda (f s)
     (grate-over grate f s))))

;;; ============================================================
;;; Part 10: Unified Composition
;;; ============================================================
;;;
;;; Compose any two optics, returning the most specific result type.
;;; The hierarchy determines the result:
;;;
;;;   Iso + Iso = Iso
;;;   Iso + Lens = Lens
;;;   Iso + Prism = Prism
;;;   Iso + Grate = Grate
;;;   Lens + Lens = Lens
;;;   Prism + Prism = Prism
;;;   Grate + Grate = Grate
;;;   Lens + Prism = Affine
;;;   Prism + Lens = Affine
;;;   Grate + Setter = Setter
;;;   * + Traversal = Traversal
;;;   * + Fold = Fold (if fold involved)
;;;   * + Getter = Getter/Fold (read-only)
;;;   * + Setter = Setter

;;; optic-type : Optic → Symbol
(define (optic-type o)
  (if (pair? o) (car o) 'unknown))

;;; optic-compose : Optic × Optic → Optic
;;; Unified composition that returns appropriate type.
(define (optic-compose outer inner)
  (let ([t1 (optic-type outer)]
        [t2 (optic-type inner)])
    (cond
      ;; Iso compositions
      [(and (eq? t1 'iso) (eq? t2 'iso))
       (iso-compose outer inner)]
      [(and (eq? t1 'iso) (eq? t2 'lens))
       (lens-compose (iso->lens outer) inner)]
      [(and (eq? t1 'lens) (eq? t2 'iso))
       (lens-compose outer (iso->lens inner))]
      [(and (eq? t1 'iso) (eq? t2 'prism))
       (prism-compose (iso->prism outer) inner)]
      [(and (eq? t1 'prism) (eq? t2 'iso))
       (prism-compose outer (iso->prism inner))]
      [(and (eq? t1 'iso) (eq? t2 'grate))
       (grate-compose (iso->grate outer) inner)]
      [(and (eq? t1 'grate) (eq? t2 'iso))
       (grate-compose outer (iso->grate inner))]

      ;; Grate compositions
      [(and (eq? t1 'grate) (eq? t2 'grate))
       (grate-compose outer inner)]

      ;; Lens compositions
      [(and (eq? t1 'lens) (eq? t2 'lens))
       (lens-compose outer inner)]
      [(and (eq? t1 'lens) (eq? t2 'prism))
       (affine-compose (lens->affine outer) (prism->affine inner))]
      [(and (eq? t1 'prism) (eq? t2 'lens))
       (affine-compose (prism->affine outer) (lens->affine inner))]

      ;; Prism compositions
      [(and (eq? t1 'prism) (eq? t2 'prism))
       (prism-compose outer inner)]

      ;; Affine compositions
      [(and (eq? t1 'affine) (eq? t2 'affine))
       (affine-compose outer inner)]
      [(and (eq? t1 'affine) (eq? t2 'lens))
       (affine-compose outer (lens->affine inner))]
      [(and (eq? t1 'lens) (eq? t2 'affine))
       (affine-compose (lens->affine outer) inner)]
      [(and (eq? t1 'affine) (eq? t2 'prism))
       (affine-compose outer (prism->affine inner))]
      [(and (eq? t1 'prism) (eq? t2 'affine))
       (affine-compose (prism->affine outer) inner)]
      [(and (eq? t1 'affine) (eq? t2 'iso))
       (affine-compose outer (lens->affine (iso->lens inner)))]
      [(and (eq? t1 'iso) (eq? t2 'affine))
       (affine-compose (lens->affine (iso->lens outer)) inner)]

      ;; Traversal compositions
      [(or (eq? t1 'traversal) (eq? t2 'traversal))
       (traversal-compose (->traversal outer) (->traversal inner))]

      ;; Fold compositions (read-only)
      [(or (eq? t1 'fold) (eq? t2 'fold))
       (fold-compose (->fold outer) (->fold inner))]

      ;; Getter compositions
      [(and (eq? t1 'getter) (eq? t2 'getter))
       (getter-compose outer inner)]
      [(eq? t1 'getter)
       (fold-compose (getter->fold outer) (->fold inner))]
      [(eq? t2 'getter)
       (fold-compose (->fold outer) (getter->fold inner))]

      ;; Setter compositions
      [(and (eq? t1 'setter) (eq? t2 'setter))
       (setter-compose outer inner)]
      [(eq? t1 'setter)
       (setter-compose outer (->setter inner))]
      [(eq? t2 'setter)
       (setter-compose (->setter outer) inner)]

      ;; Grate + Setter = Setter
      [(and (eq? t1 'grate) (eq? t2 'setter))
       (setter-compose (grate->setter outer) inner)]
      [(and (eq? t1 'setter) (eq? t2 'grate))
       (setter-compose outer (grate->setter inner))]

      ;; Default: convert to traversals
      [else
       (traversal-compose (->traversal outer) (->traversal inner))])))

;;; >>> : Optic -> Optic -> Optic
;;; Left-to-right optic composition.
;;; (>>> outer inner) focuses first through outer, then inner within that.
;;; "Outer" and "inner" refer to structural nesting: outer contains inner.
(define (>>> outer inner)
  (optic-compose outer inner))

;;; ====
;;; Universal Conversion Helpers
;;; ====

;;; ->traversal : Optic → Traversal
(define (->traversal o)
  (case (optic-type o)
    [(traversal) o]
    [(lens) (lens->traversal o)]
    [(prism) (prism->traversal o)]
    [(affine) (affine->traversal o)]
    [(iso) (lens->traversal (iso->lens o))]
    [(fold) (error '->traversal "Cannot convert fold to traversal (read-only)")]
    [(getter) (error '->traversal "Cannot convert getter to traversal (read-only)")]
    [(setter) (error '->traversal "Cannot convert setter to traversal (write-only)")]
    [else (error '->traversal "Unknown optic type")]))

;;; ->fold : Optic → Fold
(define (->fold o)
  (case (optic-type o)
    [(fold) o]
    [(getter) (getter->fold o)]
    [(lens) (lens->fold o)]
    [(prism) (prism->fold o)]
    [(traversal) (traversal->fold o)]
    [(affine) (traversal->fold (affine->traversal o))]
    [(iso) (lens->fold (iso->lens o))]
    [(setter) (error '->fold "Cannot convert setter to fold (write-only)")]
    [else (error '->fold "Unknown optic type")]))

;;; ->setter : Optic → Setter
(define (->setter o)
  (case (optic-type o)
    [(setter) o]
    [(lens) (lens->setter o)]
    [(traversal) (traversal->setter o)]
    [(iso) (iso->setter o)]
    [(grate) (grate->setter o)]
    [(prism) (traversal->setter (prism->traversal o))]
    [(affine) (traversal->setter (affine->traversal o))]
    [(fold) (error '->setter "Cannot convert fold to setter (read-only)")]
    [(getter) (error '->setter "Cannot convert getter to setter (read-only)")]
    [else (error '->setter "Unknown optic type")]))

;;; iso->lens : Iso s t a b → Lens s t a b
(define (iso->lens iso)
  (make-lens
   (iso-forward iso)
   (lambda (b _s) ((iso-backward iso) b))))

;;; iso->prism : Iso s t a b → Prism s t a b
(define (iso->prism iso)
  (make-prism
   (lambda (s) (just ((iso-forward iso) s)))
   (iso-backward iso)))

;;; getter->fold : Getter s a → Fold s a
(define (getter->fold getter)
  (make-fold (lambda (s) (list ((getter-fn getter) s)))))

;;; ============================================================
;;; Part 10: Optic Operators (Infix-style)
;;; ============================================================

;;; ^. : s × Optic → a (view through any readable optic)
(define (^. s optic)
  (case (optic-type optic)
    [(lens) (view optic s)]
    [(getter) (getter-view optic s)]
    [(iso) (iso-view optic s)]
    [(prism affine)
     (let ([m (if (eq? (optic-type optic) 'prism)
                  (preview optic s)
                  (affine-preview optic s))])
       (if (nothing? m)
           (error '^. "No target in prism/affine")
           (from-just m)))]
    [else (error '^. "Optic not readable as single value")]))

;;; ^? : s × Optic → Maybe a (preview through any optic)
(define (^? s optic)
  (case (optic-type optic)
    [(lens) (just (view optic s))]
    [(getter) (just (getter-view optic s))]
    [(iso) (just (iso-view optic s))]
    [(prism) (preview optic s)]
    [(affine) (affine-preview optic s)]
    [(fold traversal)
     (let ([targets (if (eq? (optic-type optic) 'fold)
                        (fold-to-list optic s)
                        (traversal-to-list optic s))])
       (if (null? targets) nothing (just (car targets))))]
    [else (error '^? "Unknown optic type")]))

;;; ^.. : s × Optic → List a (get all targets)
(define (^.. s optic)
  (case (optic-type optic)
    [(lens) (list (view optic s))]
    [(getter) (list (getter-view optic s))]
    [(iso) (list (iso-view optic s))]
    [(prism) (let ([m (preview optic s)])
               (if (nothing? m) '() (list (from-just m))))]
    [(affine) (let ([m (affine-preview optic s)])
                (if (nothing? m) '() (list (from-just m))))]
    [(fold) (fold-to-list optic s)]
    [(traversal) (traversal-to-list optic s)]
    [else (error '^.. "Unknown optic type")]))

;;; .~ : Optic × b → (s → t) (set through optic)
(define (.~ optic val)
  (lambda (s)
    (case (optic-type optic)
      [(lens) (set-lens optic val s)]
      [(iso) (iso-review optic val)]
      [(prism) (prism-set optic val s)]
      [(affine) (affine-set optic val s)]
      [(traversal) (traversal-set optic val s)]
      [(setter) (setter-set optic val s)]
      [(grate) (grate-set optic val s)]
      [else (error '.~ "Optic not settable")])))

;;; %~ : Optic × (a → b) → (s → t) (modify through optic)
(define (%~ optic f)
  (lambda (s)
    (case (optic-type optic)
      [(lens) (over optic f s)]
      [(iso) (iso-over optic f s)]
      [(prism) (prism-over optic f s)]
      [(affine) (affine-over optic f s)]
      [(traversal) (traversal-over optic f s)]
      [(setter) (setter-over optic f s)]
      [(grate) (grate-over optic f s)]
      [else (error '%~ "Optic not modifiable")])))

;;; & : s × (s → t) → t (reverse application for chaining)
(define (& s f) (f s))

;;; ============================================================
;;; Part 11: Law Verification
;;; ============================================================

;;; verify-iso-laws : Iso × (List a) × (List b) → Boolean
(define (verify-iso-laws iso test-as test-bs)
  (and
   ;; forward . backward = id (for b values)
   (andmap (lambda (b)
             (equal? ((iso-forward iso) ((iso-backward iso) b)) b))
           test-bs)
   ;; backward . forward = id (for a values/sources)
   (andmap (lambda (a)
             (equal? ((iso-backward iso) ((iso-forward iso) a)) a))
           test-as)))

;;; verify-lens-laws : Lens × (List (s, a)) → Boolean
;;; Test values are pairs of (source, new-value).
(define (verify-lens-laws lens test-pairs)
  (andmap
   (lambda (pair)
     (let ([s (car pair)]
           [a (cdr pair)])
       (and
        ;; Get-Put: set (view s) s = s
        (equal? (set-lens lens (view lens s) s) s)
        ;; Put-Get: view (set a s) = a
        (equal? (view lens (set-lens lens a s)) a)
        ;; Put-Put: set a' (set a s) = set a' s
        (equal? (set-lens lens a (set-lens lens (view lens s) s))
                (set-lens lens a s)))))
   test-pairs))

;;; verify-prism-laws : Prism × (List a) × (List s) → Boolean
(define (verify-prism-laws prism test-as test-ss)
  (and
   ;; Preview-Review: preview (review a) = Just a
   (andmap (lambda (a)
             (let ([result (preview prism (review prism a))])
               (and (just? result) (equal? (from-just result) a))))
           test-as)
   ;; Review-Preview: if preview s = Just a, then review a should match
   (andmap (lambda (s)
             (let ([maybe-a (preview prism s)])
               (or (nothing? maybe-a)
                   (equal? (review prism (from-just maybe-a)) s))))
           test-ss)))

;;; verify-traversal-laws : Traversal × (List s) → Boolean
(define (verify-traversal-laws trav test-ss)
  (andmap
   (lambda (s)
     (and
      ;; Identity: traversal-over id = id
      (equal? (traversal-over trav identity s) s)
      ;; Composition: traversal-over (g . f) = traversal-over g . traversal-over f
      (let ([f (lambda (x) (if (number? x) (+ x 1) x))]
            [g (lambda (x) (if (number? x) (* x 2) x))])
        (equal? (traversal-over trav (compose2 g f) s)
                (traversal-over trav g (traversal-over trav f s))))))
   test-ss))

;;; verify-grate-laws : Grate × (List s) → Boolean
;;; Verify grate laws:
;;;   - Identity: grate-over id = id
;;;   - Composition: grate-over g . grate-over f = grate-over (g . f)
(define (verify-grate-laws grate test-ss)
  (andmap
   (lambda (s)
     (and
      ;; Identity: grate-over id s = s
      (equal? (grate-over grate identity s) s)
      ;; Composition
      (let ([f (lambda (x) (if (number? x) (+ x 1) x))]
            [g (lambda (x) (if (number? x) (* x 2) x))])
        (equal? (grate-over grate (compose2 g f) s)
                (grate-over grate g (grate-over grate f s))))))
   test-ss))

;;; ============================================================
;;; Exports
;;; ============================================================
;;;
;;; Iso:
;;;   make-iso, iso?, iso-forward, iso-backward
;;;   iso-view, iso-review, iso-over, iso-flip, iso-compose
;;;   iso-id, iso-curried, iso-flipped, iso-swapped, iso-reversed
;;;   iso-assoc-list, iso-maybe-either, iso-cons
;;;
;;; Lens (extended):
;;;   lens-id
;;;
;;; Prism (extended):
;;;   prism-over, prism-set, prism-compose
;;;   prism-id, prism-nil, prism-cons
;;;   affine-nth (demoted from prism - can't lawfully review single element)
;;;
;;; Affine:
;;;   make-affine, affine?, affine-getter, affine-setter
;;;   affine-preview, affine-set, affine-over, affine-compose
;;;   affine-id, lens->affine, prism->affine
;;;
;;; Traversal:
;;;   make-traversal, traversal?, traversal-traverse, traversal-fold
;;;   traversal-to-list, traversal-over, traversal-set, traversal-compose
;;;   traversal-each, traversal-filtered, traversal-both
;;;   traversal-left, traversal-right, traversal-just
;;;   lens->traversal, prism->traversal, affine->traversal
;;;
;;; Fold:
;;;   make-fold, fold-optic?, fold-optic-fn
;;;   fold-to-list, fold-preview, fold-has, fold-length
;;;   fold-all, fold-any, fold-sum, fold-compose
;;;   fold-each, fold-filtered, fold-taking, fold-dropping
;;;   traversal->fold, lens->fold, prism->fold
;;;
;;; Getter:
;;;   make-getter, getter?, getter-fn
;;;   getter-view, getter-compose
;;;   getter-id, getter-fst, getter-snd, getter-to
;;;   lens->getter, iso->getter
;;;
;;; Setter:
;;;   make-setter, setter?, setter-over-fn
;;;   setter-over, setter-set, setter-compose
;;;   setter-mapped, setter-arg, setter-result
;;;   lens->setter, traversal->setter, iso->setter
;;;
;;; Grate:
;;;   make-grate, grate?, grate-cotraverse-fn
;;;   grate-review, grate-over, grate-set
;;;   grate-zipWith, grate-zipWith3, grate-zip
;;;   grate-compose
;;;   grate-id, grate-fn, grate-pair-same, grate-list-rep
;;;   iso->grate, grate->setter
;;;
;;; Unified Composition:
;;;   optic-type, optic-compose
;;;   ->traversal, ->fold, ->setter
;;;   iso->lens, iso->prism, iso->grate, getter->fold
;;;
;;; Operators:
;;;   ^. (view), ^? (preview), ^.. (to-list)
;;;   .~ (set), %~ (modify), & (reverse apply)
;;;
;;; Law Verification:
;;;   verify-iso-laws, verify-lens-laws, verify-prism-laws
;;;   verify-traversal-laws, verify-grate-laws
