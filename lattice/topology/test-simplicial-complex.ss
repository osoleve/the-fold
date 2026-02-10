(load "core/lang/module.ss")
(load "core/testing/test-framework.ss")
(require 'simplicial-complex)

(doc 'module 'test-simplicial-complex)
(doc 'description "Comprehensive tests for simplicial complex data structures and operations")
(doc 'layer 'lattice)

(doc 'section 'simplex-tests)

(test-group 'simplex

  (define-test "make-simplex sorts vertices"
    (let ([s (make-simplex '(3 1 2))])
      (assert-equal '(1 2 3) (simplex-vertices s))))

  (define-test "simplex? predicate"
    (assert-true (simplex? (make-simplex '(1 2 3))))
    (assert-false (simplex? '(1 2 3)))
    (assert-false (simplex? 42)))

  (define-test "simplex-dim"
    (assert-equal 0 (simplex-dim (vertex 'a)))
    (assert-equal 1 (simplex-dim (edge 'a 'b)))
    (assert-equal 2 (simplex-dim (triangle 'a 'b 'c)))
    (assert-equal 3 (simplex-dim (tetrahedron 'a 'b 'c 'd))))

  (define-test "simplex-equal? canonical comparison"
    (assert-true (simplex-equal? (make-simplex '(1 2 3))
                                  (make-simplex '(3 2 1))))
    (assert-false (simplex-equal? (make-simplex '(1 2 3))
                                   (make-simplex '(1 2 4)))))

  (define-test "simplex-contains-vertex?"
    (let ([s (triangle 'a 'b 'c)])
      (assert-true (simplex-contains-vertex? s 'a))
      (assert-true (simplex-contains-vertex? s 'b))
      (assert-false (simplex-contains-vertex? s 'd))))

  (define-test "simplex-face?"
    (let ([t (triangle 'a 'b 'c)]
          [e (edge 'a 'b)]
          [v (vertex 'a)]
          [other (edge 'a 'd)])
      (assert-true (simplex-face? v t))
      (assert-true (simplex-face? e t))
      (assert-true (simplex-face? t t))  ; A simplex is a face of itself
      (assert-false (simplex-face? other t))))

  (define-test "duplicate vertices are removed"
    ; Degenerate simplices should be normalized
    (let ([s (make-simplex '(a a b))])
      (assert-equal 1 (simplex-dim s))  ; Edge, not triangle
      (assert-equal '(a b) (simplex-vertices s)))
    ; Same vertex repeated many times
    (let ([s (make-simplex '(x x x))])
      (assert-equal 0 (simplex-dim s))  ; Vertex
      (assert-equal '(x) (simplex-vertices s))))

  (define-test "bytevector vertices are sorted correctly"
    ; Critical for block graph homology where vertices are hashes
    (let* ([bv1 #vu8(0 1 2)]
           [bv2 #vu8(0 1 3)]
           [bv3 #vu8(1 0 0)]
           [s (make-simplex (list bv3 bv1 bv2))])
      ; Should be sorted: bv1 < bv2 < bv3 (lexicographic)
      (assert-equal (list bv1 bv2 bv3) (simplex-vertices s))))

  (define-test "bytevector edge canonical regardless of order"
    ; Edge (A,B) should equal edge (B,A) after canonicalization
    (let* ([hash-a #vu8(0 0 0 1)]
           [hash-b #vu8(0 0 0 2)]
           [edge-ab (make-simplex (list hash-a hash-b))]
           [edge-ba (make-simplex (list hash-b hash-a))])
      (assert-true (simplex-equal? edge-ab edge-ba))
      (assert-equal (simplex-vertices edge-ab) (simplex-vertices edge-ba))))

  (define-test "bytevector<? comparison"
    ; Test the underlying comparison function
    (assert-true (bytevector<? #vu8(0 0) #vu8(0 1)))
    (assert-true (bytevector<? #vu8(0 0) #vu8(1 0)))
    (assert-true (bytevector<? #vu8(0) #vu8(0 0)))     ; shorter < longer when prefix matches
    (assert-false (bytevector<? #vu8(1 0) #vu8(0 1)))
    (assert-false (bytevector<? #vu8(0 0) #vu8(0 0)))  ; equal, not less
    (assert-false (bytevector<? #vu8(0 0 0) #vu8(0 0)))) ; longer >= shorter

  (define-test "mixed type ordering includes bytevectors"
    ; Bytevectors should have consistent ordering with other types
    (let* ([num 42]
           [sym 'foo]
           [str "bar"]
           [bv #vu8(1 2 3)]
           [lst '(1 2)]
           [s (make-simplex (list lst bv str sym num))])
      ; Order: number < symbol < string < bytevector < list
      (assert-equal (list num sym str bv lst) (simplex-vertices s)))))

(doc 'section 'face-enumeration-tests)

(test-group 'face-enumeration

  (define-test "simplex-facets of triangle"
    (let* ([t (triangle 'a 'b 'c)]
           [facets (simplex-facets t)])
      (assert-equal 3 (length facets))
      ; Each facet is an edge
      (assert-true (every (lambda (f) (= 1 (simplex-dim f))) facets))))

  (define-test "simplex-facets of edge"
    (let* ([e (edge 'a 'b)]
           [facets (simplex-facets e)])
      (assert-equal 2 (length facets))
      ; Each facet is a vertex
      (assert-true (every (lambda (f) (= 0 (simplex-dim f))) facets))))

  (define-test "simplex-facets of vertex"
    (let ([v (vertex 'a)])
      (assert-equal '() (simplex-facets v))))

  (define-test "simplex-faces-of-dim"
    (let ([t (tetrahedron 'a 'b 'c 'd)])
      ; 0-faces (vertices): C(4,1) = 4
      (assert-equal 4 (length (simplex-faces-of-dim t 0)))
      ; 1-faces (edges): C(4,2) = 6
      (assert-equal 6 (length (simplex-faces-of-dim t 1)))
      ; 2-faces (triangles): C(4,3) = 4
      (assert-equal 4 (length (simplex-faces-of-dim t 2)))
      ; 3-faces (the tetrahedron itself): C(4,4) = 1
      (assert-equal 1 (length (simplex-faces-of-dim t 3)))
      ; Invalid dimensions
      (assert-equal '() (simplex-faces-of-dim t -1))
      (assert-equal '() (simplex-faces-of-dim t 4))))

  (define-test "simplex-all-faces count"
    (let ([t (triangle 'a 'b 'c)])
      ; A triangle has 3 vertices + 3 edges + 1 triangle = 7 faces
      (assert-equal 7 (length (simplex-all-faces t)))))

  (define-test "simplex-proper-faces excludes simplex itself"
    (let* ([t (triangle 'a 'b 'c)]
           [proper (simplex-proper-faces t)])
      ; 3 vertices + 3 edges = 6 proper faces
      (assert-equal 6 (length proper))
      (assert-false (simplex-in-list? t proper)))))

(doc 'section 'simplicial-complex-tests)

(test-group 'simplicial-complex

  (define-test "sc-empty is empty"
    (assert-equal '() (sc-simplices sc-empty))
    (assert-equal -1 (sc-max-dim sc-empty)))

  (define-test "sc-from-simplices with closure"
    (let ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))])
      ; Should contain: 3 vertices + 3 edges + 1 triangle = 7 simplices
      (assert-equal 7 (sc-count sc))
      (assert-equal 3 (sc-count-dim sc 0))
      (assert-equal 3 (sc-count-dim sc 1))
      (assert-equal 1 (sc-count-dim sc 2))))

  (define-test "sc-contains?"
    (let ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))])
      (assert-true (sc-contains? sc (vertex 'a)))
      (assert-true (sc-contains? sc (edge 'a 'b)))
      (assert-true (sc-contains? sc (triangle 'a 'b 'c)))
      (assert-false (sc-contains? sc (vertex 'd)))
      (assert-false (sc-contains? sc (edge 'a 'd)))))

  (define-test "sc-vertices, sc-edges, sc-faces"
    (let ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))])
      (assert-equal 3 (length (sc-vertices sc)))
      (assert-equal 3 (length (sc-edges sc)))
      (assert-equal 1 (length (sc-faces sc)))))

  (define-test "sc-add-simplex maintains closure"
    (let* ([sc1 (sc-add-simplex sc-empty (triangle 'a 'b 'c))]
           [sc2 (sc-add-simplex sc1 (triangle 'b 'c 'd))])
      ; Two triangles sharing an edge
      ; Vertices: a, b, c, d = 4
      ; Edges: ab, ac, bc, bd, cd = 5
      ; Triangles: abc, bcd = 2
      (assert-equal 4 (sc-count-dim sc2 0))
      (assert-equal 5 (sc-count-dim sc2 1))
      (assert-equal 2 (sc-count-dim sc2 2))))

  (define-test "adding same simplex twice is idempotent"
    (let* ([sc1 (sc-from-simplices (list (triangle 'a 'b 'c)))]
           [sc2 (sc-add-simplex sc1 (triangle 'a 'b 'c))])
      (assert-equal (sc-count sc1) (sc-count sc2)))))

(doc 'section 'skeleton-star-link-tests)

(test-group 'skeleton-star-link

  (define-test "sc-skeleton extracts lower dimensions"
    (let* ([sc (sc-from-simplices (list (tetrahedron 'a 'b 'c 'd)))]
           [sk1 (sc-skeleton sc 1)])
      ; 1-skeleton has only vertices and edges
      (assert-equal 4 (sc-count-dim sk1 0))
      (assert-equal 6 (sc-count-dim sk1 1))
      (assert-equal 0 (sc-count-dim sk1 2))))

  (define-test "sc-star finds cofaces"
    (let* ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))]
           [v (vertex 'a)]
           [star (sc-star sc v)])
      ; Star of vertex a: vertex a, edges ab, ac, and triangle abc
      (assert-equal 4 (length star))))

  (define-test "sc-link of vertex in triangle"
    (let* ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))]
           [link (sc-link sc (vertex 'a))])
      ; Link of a in triangle abc is the edge bc (and its vertices)
      (assert-true (sc-contains? link (vertex 'b)))
      (assert-true (sc-contains? link (vertex 'c)))
      (assert-true (sc-contains? link (edge 'b 'c)))
      (assert-false (sc-contains? link (vertex 'a))))))

(doc 'section 'boundary-tests)

(test-group 'boundary

  (define-test "boundary of vertex is empty"
    (let ([bd (simplex-boundary (vertex 'a))])
      (assert-equal chain-empty bd)))

  (define-test "boundary of edge has two terms"
    (let ([bd (simplex-boundary (edge 'a 'b))])
      ; ∂[a,b] = [b] - [a]  (with sorted vertices)
      (assert-equal 2 (length bd))))

  (define-test "boundary of triangle has three terms"
    (let ([bd (simplex-boundary (triangle 'a 'b 'c))])
      ; ∂[a,b,c] = [b,c] - [a,c] + [a,b]
      (assert-equal 3 (length bd))))

  (define-test "boundary squared is zero (∂² = 0)"
    ; This is the fundamental lemma of homology!
    (let* ([t (triangle 'a 'b 'c)]
           [bd1 (simplex-boundary t)]
           [bd2 (chain-boundary bd1)])
      ; The boundary of the boundary should be empty (all terms cancel)
      (assert-equal chain-empty bd2)))

  (define-test "∂² = 0 for tetrahedron"
    (let* ([tet (tetrahedron 'a 'b 'c 'd)]
           [bd1 (simplex-boundary tet)]
           [bd2 (chain-boundary bd1)])
      (assert-equal chain-empty bd2))))

(doc 'section 'invariants-tests)

(test-group 'invariants

  (define-test "Euler characteristic of triangle"
    (let ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))])
      ; χ = V - E + F = 3 - 3 + 1 = 1
      (assert-equal 1 (sc-euler sc))))

  (define-test "Euler characteristic of tetrahedron"
    (let ([sc (sc-from-simplices (list (tetrahedron 'a 'b 'c 'd)))])
      ; χ = V - E + F - T = 4 - 6 + 4 - 1 = 1
      (assert-equal 1 (sc-euler sc))))

  (define-test "Euler characteristic of boundary of tetrahedron (sphere)"
    ; The boundary of a tetrahedron is topologically a 2-sphere
    (let ([sc (sc-boundary-of-simplex 3)])
      ; χ(S²) = 2
      (assert-equal 2 (sc-euler sc))))

  (define-test "f-vector of triangle"
    (let ([sc (sc-from-simplices (list (triangle 'a 'b 'c)))])
      (assert-equal '(3 3 1) (sc-f-vector sc))))

  (define-test "f-vector of tetrahedron"
    (let ([sc (sc-from-simplices (list (tetrahedron 'a 'b 'c 'd)))])
      (assert-equal '(4 6 4 1) (sc-f-vector sc)))))

(doc 'section 'standard-complexes-tests)

(test-group 'standard-complexes

  (define-test "sc-simplex 0 is a single vertex"
    (let ([sc (sc-simplex 0)])
      (assert-equal 1 (sc-count sc))
      (assert-equal 0 (sc-max-dim sc))))

  (define-test "sc-simplex 2 is a triangle with all faces"
    (let ([sc (sc-simplex 2)])
      ; 3 vertices + 3 edges + 1 triangle
      (assert-equal 7 (sc-count sc))))

  (define-test "sc-boundary-of-simplex 2 is a circle"
    ; Boundary of a 2-simplex is 3 edges forming a triangle boundary
    (let ([sc (sc-boundary-of-simplex 2)])
      (assert-equal 3 (sc-count-dim sc 0))  ; 3 vertices
      (assert-equal 3 (sc-count-dim sc 1))  ; 3 edges
      (assert-equal 0 (sc-count-dim sc 2))  ; no 2-faces
      ; χ(S¹) = 0
      (assert-equal 0 (sc-euler sc))))

  (define-test "sc-discrete creates only vertices"
    (let ([sc (sc-discrete '(a b c d e))])
      (assert-equal 5 (sc-count sc))
      (assert-equal 0 (sc-max-dim sc)))))

(doc 'section 'helpers-tests)

(test-group 'helpers

  (define-test "combinations generates correct counts"
    ; C(4,2) = 6
    (assert-equal 6 (length (combinations '(1 2 3 4) 2)))
    ; C(5,3) = 10
    (assert-equal 10 (length (combinations '(1 2 3 4 5) 3)))
    ; C(n,0) = 1
    (assert-equal 1 (length (combinations '(1 2 3) 0)))
    ; C(n,n) = 1
    (assert-equal 1 (length (combinations '(1 2 3) 3))))

  (define-test "iota generates sequence"
    (assert-equal '() (iota 0))
    (assert-equal '(0) (iota 1))
    (assert-equal '(0 1 2 3 4) (iota 5))))

(doc 'section 'filtration-tests)

(test-group 'filtration

  (define-test "filtered-simplex construction"
    (let* ([s (triangle 'a 'b 'c)]
           [fs (make-filtered-simplex s 0.5)])
      (assert-true (filtered-simplex? fs))
      (assert-true (simplex-equal? s (filtered-simplex-base fs)))
      (assert-equal 0.5 (filtered-simplex-value fs))))

  (define-test "filtered-simplex predicate"
    (assert-false (filtered-simplex? (triangle 'a 'b 'c)))
    (assert-false (filtered-simplex? '(1 2 3)))))

(define (every pred lst)
  (doc 'description "Helper for test output")
  (cond
    [(null? lst) #t]
    [(pred (car lst)) (every pred (cdr lst))]
    [else #f]))

(doc 'section 'run-tests)

(display "\n=== Simplicial Complex Tests ===\n\n")
(run-all-tests)
