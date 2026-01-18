;;; lattice/fp/category/comonad.ss — Comonad Type Class and Instances
;;;
;;; A comonad is the categorical dual of a monad. Where a monad allows
;;; composing effectful computations (sequencing with context), a comonad
;;; allows decomposing context-dependent values (observing with context).
;;;
;;; Comonad W has three operations:
;;;   - extract  : W a → a           (observe the focused value)
;;;   - extend   : (W a → b) → W a → W b  (apply contextual function everywhere)
;;;   - duplicate: W a → W (W a)     (derived: duplicate = extend id)
;;;
;;; Comonad laws:
;;;   1. extend extract = id         (extending observation is identity)
;;;   2. extract . extend f = f      (observing extended = applying function)
;;;   3. extend f . extend g = extend (f . extend g)  (associativity)
;;;
;;; Key insight: Every adjunction F ⊣ G yields a comonad F∘G with:
;;;   - extract = ε (counit)
;;;   - duplicate = F(η_G) (lift unit into F)
;;;
;;; This is Core code: pure, total, assumes reasonable input.
;;;
;;; Features:
;;;   - Comonad type class definition
;;;   - Comonad derivation from adjunctions
;;;   - Store comonad (dual of State monad)
;;;   - Env comonad (dual of Reader monad)
;;;   - Traced comonad (dual of Writer monad)
;;;   - Law verification utilities
;;;
;;; Dependencies:
;;;   - prelude.ss
;;;   - fp/category/adjunction.ss

(load "core/base/prelude.ss")
(load "lattice/fp/meta/combinators.ss")
(load "lattice/fp/templates.ss")
(load "lattice/fp/category/adjunction.ss")

;;; ====
;;; Comonad Type Class
;;; ====

;;; make-comonad : Functor × (W a → a) × ((W a → b) → W a → W b) → Comonad
;;; Create a comonad from its functor and core operations.
;;; Note: duplicate is derived from extend via (extend id).
(define (make-comonad functor extract-fn extend-fn)
  (list 'comonad functor extract-fn extend-fn))

;;; comonad? : Any → Boolean
(define (comonad? x)
  (and (pair? x)
       (eq? (car x) 'comonad)
       (= (length x) 4)))

;;; comonad-functor : Comonad → Functor
;;; Errors if not a valid comonad (consistent with comonad-extend).
(define (comonad-functor w)
  (if (comonad? w)
      (cadr w)
      (error 'comonad-functor "not a valid comonad")))

;;; comonad-extract : Comonad → (W a → a)
;;; Errors if not a valid comonad (consistent with comonad-extend).
(define (comonad-extract w)
  (if (comonad? w)
      (caddr w)
      (error 'comonad-extract "not a valid comonad")))

;;; comonad-extend : Comonad → ((W a → b) → W a → W b)
;;; Note: Default fallback is identity; caller should always provide a valid comonad.
(define (comonad-extend w)
  (if (comonad? w)
      (cadddr w)
      ;; Fallback should preserve W structure. The minimal valid extend
      ;; that doesn't know W's structure is to just return the input unchanged.
      ;; This only type-checks if f returns the same type, which it generally doesn't.
      ;; Better to error than silently return wrong type.
      (error 'comonad-extend "not a valid comonad")))

;;; ====
;;; Derived Operations
;;; ====

;;; duplicate-with : Comonad × W a → W (W a)
;;; Duplicate structure: every position sees itself in context.
;;; Derived as: duplicate = extend id
(define (duplicate-with comonad wa)
  ((comonad-extend comonad) id wa))

;;; extract-with : Comonad × W a → a
;;; Apply comonad's extract operation.
(define (extract-with comonad wa)
  ((comonad-extract comonad) wa))

;;; extend-with : Comonad × (W a → b) × W a → W b
;;; Apply comonad's extend operation.
(define (extend-with comonad f wa)
  ((comonad-extend comonad) f wa))

;;; coflatmap : Comonad × (W a → b) × W a → W b
;;; Alias for extend-with (mirrors Scala naming).
(define coflatmap extend-with)

;;; (=>>) : W a × (W a → b) → W b
;;; Comonadic extension operator (flip of extend).
;;; wa =>> f = extend f wa
(define (=>> wa f comonad)
  (extend-with comonad f wa))

;;; ====
;;; Comonad Derivation from Adjunction
;;; ====
;;;
;;; Given F ⊣ G (F left adjoint to G):
;;;   - The composite F∘G forms a comonad
;;;   - extract = ε (counit of adjunction)
;;;   - duplicate = F(η_G) where η is unit

;;; comonad-from-adjunction : Adjunction → Comonad
;;; Derive comonad from adjunction F ⊣ G.
;;; The comonad acts on F∘G.
;;;
;;; For w : F(G(a)), the operations are:
;;;   - extract = ε : F(G(a)) → a
;;;   - duplicate = F(η_G) : F(G(a)) → F(G(F(G(a))))
;;;   - extend f w = F(G(f))(duplicate w) where f : F(G(a)) → b
;;;
;;; The extend operation applies f at each "position" in the duplicated structure.
(define (comonad-from-adjunction adj)
  (let* ([F (adjunction-left adj)]
         [G (adjunction-right adj)]
         [η (adjunction-unit adj)]
         [ε (adjunction-counit adj)]
         [F-fmap (functor-fmap F)]
         [G-fmap (functor-fmap G)]
         [η-comp (nat-transform-component η)]
         [ε-comp (nat-transform-component ε)])
    ;; Functor for F∘G: fmap f = F(G(f))
    (let ([FG-functor (make-functor
                       (lambda (f x)
                         (F-fmap (lambda (y) (G-fmap f y)) x)))])
      (make-comonad
       FG-functor
       ;; extract = ε : F(G(a)) → a
       ε-comp
       ;; extend f w where f : F(G(a)) → b, w : F(G(a))
       ;; 1. duplicate w = F-fmap η-comp w : F(G(F(G(a))))
       ;; 2. Map f over the inner F(G(a)) via G-fmap
       ;; Result: F(G(b))
       (lambda (f w)
         (let ([duplicated (F-fmap η-comp w)])  ; F(G(F(G(a))))
           (F-fmap (lambda (gfga)               ; gfga : G(F(G(a)))
                     (G-fmap f gfga))           ; G-fmap f : G(F(G(a))) → G(b)
                   duplicated)))))))

;;; ====
;;; Store Comonad
;;; ====
;;;
;;; Store s a = (s → a, s)
;;;
;;; A position s in a space, plus a function to get a value at any position.
;;; This is the comonad dual of State s a = s → (a, s).
;;;
;;; Store arises from the product-exponential adjunction:
;;;   (−) × S ⊣ (−)^S
;;;
;;; The comonad is (−) × S ∘ (−)^S = (S → −) × S = Store S
;;;
;;; Key uses:
;;;   - Profunctor optics (lenses are Store coalgebras)
;;;   - Cellular automata (Store over grid positions)
;;;   - Memoization with position tracking

;;; store-tag : Symbol
(define store-tag 'store)

;;; make-store : (s → a) × s → Store s a
;;; Create a Store from an accessor function and current position.
(define (make-store accessor position)
  (list store-tag accessor position))

;;; store? : Any → Boolean
(define (store? x)
  (and (pair? x)
       (eq? (car x) store-tag)
       (= (length x) 3)))

;;; store-accessor : Store s a → (s → a)
;;; Get the accessor function.
(define (store-accessor st)
  (if (store? st) (cadr st) (error 'store-accessor "not a store")))

;;; store-position : Store s a → s
;;; Get the current position.
(define (store-position st)
  (if (store? st) (caddr st) (error 'store-position "not a store")))

;;; store-peek : Store s a × s → a
;;; Peek at a value at a different position.
(define (store-peek st pos)
  ((store-accessor st) pos))

;;; store-seek : Store s a × s → Store s a
;;; Move to a new position.
(define (store-seek st new-pos)
  (make-store (store-accessor st) new-pos))

;;; store-seeks : Store s a × (s → s) → Store s a
;;; Move position by applying a function.
(define (store-seeks st f)
  (store-seek st (f (store-position st))))

;;; store-experiment : Functor × Store s a × (s → f s) → f a
;;; Apply a functor-valued function to position and extract values.
(define (store-experiment functor st f)
  (let ([accessor (store-accessor st)]
        [pos (store-position st)])
    ((functor-fmap functor) accessor (f pos))))

;;; ====
;;; Store Comonad Operations
;;; ====

;;; store-extract : Store s a → a
;;; Extract the value at the current position.
(define (store-extract st)
  ((store-accessor st) (store-position st)))

;;; store-extend : (Store s a → b) × Store s a → Store s b
;;; Extend a function over all positions.
;;; The new accessor at position p runs f on Store focused at p.
(define (store-extend f st)
  (let ([accessor (store-accessor st)]
        [pos (store-position st)])
    (make-store
     ;; New accessor: for any position p, run f with store focused at p
     (lambda (p)
       (f (make-store accessor p)))
     pos)))

;;; store-duplicate : Store s a → Store s (Store s a)
;;; Duplicate: each position maps to the store focused at that position.
(define (store-duplicate st)
  (store-extend id st))

;;; store-functor : Functor Store
;;; Functor instance for Store.
(define store-functor
  (make-functor
   (lambda (f st)
     (make-store
      (lambda (s) (f ((store-accessor st) s)))
      (store-position st)))))

;;; store-comonad : Comonad Store
;;; The Store comonad instance.
(define store-comonad
  (make-comonad store-functor store-extract store-extend))

;;; store-copeek : Store s a → Store s a → a
;;; Copeek operation for Store: extract from second store at first store's position.
;;; Use with compose-comonads-with-dist* for Store-based composition.
(define (store-copeek pos-store val-store)
  (store-peek val-store (store-position pos-store)))

;;; ====
;;; Env Comonad (CoReader)
;;; ====
;;;
;;; Env e a = (e, a)
;;;
;;; A value paired with an environment. Dual of Reader e a = e → a.
;;; This is the simplest comonad: extract gets the value, extend
;;; distributes the environment.

;;; env-tag : Symbol
(define env-tag 'env)

;;; make-env : e × a → Env e a
(define (make-env environment value)
  (list env-tag environment value))

;;; env? : Any → Boolean
(define (env? x)
  (and (pair? x)
       (eq? (car x) env-tag)
       (= (length x) 3)))

;;; env-environment : Env e a → e
(define (env-environment e)
  (if (env? e) (cadr e) (error 'env-environment "not an env")))

;;; env-value : Env e a → a
(define (env-value e)
  (if (env? e) (caddr e) (error 'env-value "not an env")))

;;; env-ask : Env e a → e
;;; Get the environment (alias for env-environment).
(define env-ask env-environment)

;;; env-local : (e → e) × Env e a → Env e a
;;; Modify the environment.
(define (env-local f e)
  (make-env (f (env-environment e)) (env-value e)))

;;; ====
;;; Env Comonad Operations
;;; ====

;;; env-extract : Env e a → a
;;; Extract the value (ignoring environment).
(define (env-extract e)
  (env-value e))

;;; env-extend : (Env e a → b) × Env e a → Env e b
;;; Extend: apply function, keeping environment.
(define (env-extend f e)
  (make-env (env-environment e) (f e)))

;;; env-duplicate : Env e a → Env e (Env e a)
(define (env-duplicate e)
  (env-extend id e))

;;; env-functor : Functor Env
(define env-functor
  (make-functor
   (lambda (f e)
     (make-env (env-environment e) (f (env-value e))))))

;;; env-comonad : Comonad Env
(define env-comonad
  (make-comonad env-functor env-extract env-extend))

;;; env-copeek : Env e a → Env e a → a
;;; Copeek operation for Env: position is trivial, so just extract from value source.
;;; Use with compose-comonads-with-dist* (though the default works for Env).
(define (env-copeek pos-env val-env)
  (env-value val-env))

;;; ====
;;; Traced Comonad (CoWriter)
;;; ====
;;;
;;; Traced m a = m → a (where m is a Monoid)
;;;
;;; A function from accumulated trace to value. Dual of Writer m a = (a, m).
;;; The comonad threads a monoid through, allowing dependency tracking.

;;; traced-tag : Symbol
(define traced-tag 'traced)

;;; make-traced : (m → a) × Monoid → Traced m a
;;; Create a Traced from a function and its monoid.
(define (make-traced run-traced monoid)
  (list traced-tag run-traced monoid))

;;; traced? : Any → Boolean
(define (traced? x)
  (and (pair? x)
       (eq? (car x) traced-tag)
       (= (length x) 3)))

;;; traced-run : Traced m a → (m → a)
(define (traced-run t)
  (if (traced? t) (cadr t) (error 'traced-run "not a traced")))

;;; traced-monoid : Traced m a → Monoid
(define (traced-monoid t)
  (if (traced? t) (caddr t) (error 'traced-monoid "not a traced")))

;;; run-traced : Traced m a × m → a
;;; Run the traced computation with accumulated value.
(define (run-traced t m)
  ((traced-run t) m))

;;; ====
;;; Traced Comonad Operations
;;; ====

;;; traced-extract : Traced m a → a
;;; Extract: run with monoid identity.
(define (traced-extract t)
  (let ([monoid (traced-monoid t)])
    ((traced-run t) (monoid-mempty monoid))))

;;; traced-extend : (Traced m a → b) × Traced m a → Traced m b
;;; Extend: the new traced at position m1 runs f on traced shifted by m1.
(define (traced-extend f t)
  (let ([monoid (traced-monoid t)]
        [original-run (traced-run t)])
    (make-traced
     (lambda (m1)
       (f (make-traced
           (lambda (m2)
             (original-run ((monoid-mappend monoid) m1 m2)))
           monoid)))
     monoid)))

;;; traced-duplicate : Traced m a → Traced m (Traced m a)
(define (traced-duplicate t)
  (traced-extend id t))

;;; traced-functor : Monoid → Functor (Traced m)
;;; Note: Traced's functor depends on the monoid.
(define (traced-functor monoid)
  (make-functor
   (lambda (f t)
     (make-traced
      (lambda (m) (f ((traced-run t) m)))
      monoid))))

;;; traced-comonad : Monoid → Comonad (Traced m)
;;; Note: Traced comonad is parameterized by monoid.
(define (traced-comonad monoid)
  (make-comonad
   (traced-functor monoid)
   traced-extract
   traced-extend))

;;; ====
;;; Comonad Law Verification
;;; ====

;;; verify-comonad-law-1 : Comonad × W a → Boolean
;;; Law 1: extend extract = id
;;; Tests: extend extract wa = wa
(define (verify-comonad-law-1 comonad wa eq?)
  (let* ([ext (comonad-extend comonad)]
         [extr (comonad-extract comonad)]
         [result (ext extr wa)])
    (eq? result wa)))

;;; verify-comonad-law-2 : Comonad × (W a → b) × W a → Boolean
;;; Law 2: extract . extend f = f
;;; Tests: extract (extend f wa) = f wa
(define (verify-comonad-law-2 comonad f wa)
  (let* ([ext (comonad-extend comonad)]
         [extr (comonad-extract comonad)]
         [result (extr (ext f wa))])
    (equal? result (f wa))))

;;; verify-comonad-law-3 : Comonad × (W b → c) × (W a → b) × W a × eq? → Boolean
;;; Law 3: extend f . extend g = extend (f . extend g)
;;; Tests: extend f (extend g wa) = extend (λw. f (extend g w)) wa
(define (verify-comonad-law-3 comonad f g wa eq?)
  (let* ([ext (comonad-extend comonad)]
         [left (ext f (ext g wa))]
         [right (ext (lambda (w) (f (ext g w))) wa)])
    (eq? left right)))

;;; verify-comonad-laws : Comonad × (W a → b) × (W b → c) × W a × eq? → Boolean
;;; Verify all three comonad laws.
(define (verify-comonad-laws comonad f g wa eq?)
  (and (verify-comonad-law-1 comonad wa eq?)
       (verify-comonad-law-2 comonad f wa)
       (verify-comonad-law-3 comonad f g wa eq?)))

;;; ====
;;; Comonad Composition
;;; ====
;;;
;;; IMPORTANT: Unlike the common misconception, comonads do NOT always compose.
;;; Comonad composition requires a "distributive law" δ : W2(W1(a)) → W1(W2(a))
;;; satisfying coherence conditions (dual to monad distributive laws).
;;;
;;; Without a distributive law, the extend operation cannot be defined correctly
;;; because there's no canonical way to "push" the outer comonad structure
;;; through the inner one.
;;;
;;; Specific comonad pairs that DO compose naturally:
;;;   - Env E with any comonad (Env is "left-distributive")
;;;   - Any comonad with Store S (Store has special structure)
;;;   - Comonads from the same adjunction

;;; compose-comonads-with-dist* : Comonad × Comonad × DistLaw × Copeek → Comonad
;;; Compose two comonads given a distributive law δ : W2(W1(a)) → W1(W2(a))
;;; and a "copeek" operation for W2.
;;;
;;; The distributive law must satisfy:
;;;   - δ ∘ W2(extract1) = extract1
;;;   - δ ∘ extract2 = W1(extract2)
;;;   - δ ∘ W2(duplicate1) = duplicate1 ∘ δ ∘ W2(δ)
;;;   - δ ∘ duplicate2 = W1(duplicate2) ∘ δ
;;;
;;; The copeek operation: W2(a) → W2(a) → a
;;; Given (w2-pos, w2-val), extracts from w2-val at w2-pos's focus position.
;;;   - For Store: (λ pos val. store-peek val (store-position pos))
;;;   - For Env:   (λ _ val. env-value val)
;;;   - For Traced: (λ _ val. traced-extract val)
;;;
;;; This is the fully general composition that works for all comonads.
(define (compose-comonads-with-dist* w1 w2 dist copeek)
  (let* ([f1 (comonad-functor w1)]
         [f2 (comonad-functor w2)]
         [ext1 (comonad-extend w1)]
         [ext2 (comonad-extend w2)]
         [extr1 (comonad-extract w1)]
         [extr2 (comonad-extract w2)]
         [fmap1 (functor-fmap f1)]
         [fmap2 (functor-fmap f2)])
    ;; Composed functor: (W1 ∘ W2)(f) = W1(W2(f))
    (let ([composed-functor
           (make-functor
            (lambda (f x)
              (fmap1 (lambda (w2a) (fmap2 f w2a)) x)))])
      (make-comonad
       composed-functor
       ;; extract: extract2 ∘ extract1
       ;; W1(W2(a)) → W2(a) → a
       (lambda (w1w2a)
         (extr2 (extr1 w1w2a)))
       ;; extend f w
       ;; At each (W1-position, W2-position) pair, apply f to the whole
       ;; W1(W2(a)) structure "focused" at that position pair.
       (lambda (f w1w2a)
         (ext1
          (lambda (w1-pos)
            ;; w1-pos : W1(W2(a)) - the whole structure focused at current W1 position
            (ext2
             (lambda (w2-foc)
               ;; w2-foc : W2(a) - the W2 at this W1 position, focused at current W2 position
               ;; Build W2(W1(a)) where at each W2 position p, we have W1(a) with
               ;; values extracted from w1-pos's W2 values at position p.
               ;; Use ext2 to iterate with proper focus information.
               (let ([w2w1a
                      (ext2
                       (lambda (w2-iter)
                         ;; w2-iter : W2(a) focused at iteration position
                         ;; Build W1(a) by extracting from each W1's W2 at this position
                         (fmap1
                          (lambda (w2-in-w1)
                            ;; w2-in-w1 : W2(a) at some W1 position
                            ;; Extract at w2-iter's focus position using copeek
                            (copeek w2-iter w2-in-w1))
                          w1-pos))
                       w2-foc)])
                 ;; Apply distributive law to swap W2(W1(a)) → W1(W2(a))
                 ;; Then apply f to get b
                 (f (dist w2w1a))))
             (extr1 w1-pos)))
          w1w2a))))))

;;; compose-comonads-with-dist : Comonad × Comonad × DistLaw → Comonad
;;; Compose two comonads given a distributive law δ : W2(W1(a)) → W1(W2(a)).
;;;
;;; Uses extr2 as the default copeek operation, which is correct for:
;;;   - Env (CoReader): No meaningful position, extract always works
;;;   - Any comonad where "position" is trivial
;;;
;;; For position-based comonads like Store, use compose-comonads-with-dist*
;;; with an appropriate copeek (e.g., store-peek-based).
;;;
;;; The distributive law must satisfy:
;;;   - δ ∘ W2(extract1) = extract1
;;;   - δ ∘ extract2 = W1(extract2)
;;;   - δ ∘ W2(duplicate1) = duplicate1 ∘ δ ∘ W2(δ)
;;;   - δ ∘ duplicate2 = W1(duplicate2) ∘ δ
(define (compose-comonads-with-dist w1 w2 dist)
  (compose-comonads-with-dist* w1 w2 dist
    ;; Default copeek: just extract from value source
    ;; Correct for Env and other position-trivial comonads
    (lambda (w2-pos w2-val) ((comonad-extract w2) w2-val))))

;;; compose-comonads : Comonad × Comonad → Comonad
;;;
;;; DEPRECATED: Use compose-comonads-with-dist with an explicit distributive law.
;;;
;;; WARNING: Comonads do NOT compose in general! This function provides a
;;; fallback extend implementation that:
;;;   - Does NOT satisfy the comonad laws for arbitrary comonad pairs
;;;   - Uses constant fmap to create synthetic structure (discards context)
;;;   - Only produces correct results for special cases (Env ∘ Any, Any ∘ Store)
;;;
;;; The fundamental issue: extend needs to apply a contextual function at
;;; every position, but without a distributive law there's no canonical way
;;; to "thread" the outer comonad through the inner one.
;;;
;;; For correct composition, use:
;;;   (compose-comonads-with-dist w1 w2 distributive-law)
;;;
;;; Or work with each comonad separately via comonad transformers.
(define (compose-comonads w1 w2)
  (let* ([f1 (comonad-functor w1)]
         [f2 (comonad-functor w2)]
         [extr1 (comonad-extract w1)]
         [extr2 (comonad-extract w2)]
         [fmap1 (functor-fmap f1)]
         [fmap2 (functor-fmap f2)])
    ;; Composed functor: (W1 ∘ W2)(f) = W1(W2(f))
    (let ([composed-functor
           (make-functor
            (lambda (f x)
              (fmap1 (lambda (w2a) (fmap2 f w2a)) x)))])
      (make-comonad
       composed-functor
       ;; extract: extract2 ∘ extract1 (this IS correct)
       (lambda (w1w2a)
         (extr2 (extr1 w1w2a)))
       ;; extend: INCORRECT for general comonads.
       ;; This uses constant fmap, which creates synthetic structure that
       ;; replaces all positions with a single extracted value `a`.
       ;; Result: every position "sees" the same context instead of its own.
       ;;
       ;; Known to work correctly only when:
       ;;   - W1 is Env (environment just gets passed through)
       ;;   - W2 is Store (position-indexed access still works)
       (lambda (f w1w2a)
         ;; For each position in W1, for each position in W2:
         ;; Apply f to a synthetic structure where all of W1 contains
         ;; copies of the current W2 value. This is wrong but type-preserving.
         (fmap1 (lambda (w2a)
                  (fmap2 (lambda (a)
                           ;; BUG: This creates W1(W2(a)) where every W1-position
                           ;; contains the same w2a (filled with constant a).
                           ;; The real context from w1w2a is discarded.
                           (f (fmap1 (lambda (_) (fmap2 (lambda (_) a) w2a)) w1w2a)))
                         w2a))
                w1w2a))))))

;;; ====
;;; Display
;;; ====

;;; comonad->string : Comonad → String
(define (comonad->string w)
  (if (comonad? w)
      "Comonad"
      "Not a comonad"))

;;; store->string : Store s a → String
(define (store->string st)
  (if (store? st)
      (format "Store(pos=~a)" (store-position st))
      "Not a store"))

;;; env->string : Env e a → String
(define (env->string e)
  (if (env? e)
      (format "Env(~a, ~a)" (env-environment e) (env-value e))
      "Not an env"))
