;;; core/algebra/test-group.ss — Tests for Group Theory Library
;;;
;;; Run: scheme --script core/algebra/test-group.ss

(load "core/test-framework.ss")
(load "lattice/algebra/group.ss")

(set! *current-group* 'group-theory)

;;; ============================================================
;;; Basic Group Construction Tests
;;; ============================================================

(define-test "make-group creates valid group structure"
  (let ([g (make-cyclic-group 4)])
       (assert-true (group? g))
       (assert-equal 4 (group-order g))
       (assert-equal 0 (group-identity g))))

(define-test "group? identifies groups"
  (assert-true (group? (make-cyclic-group 3)))
  (assert-false (group? '(1 2 3)))
  (assert-false (group? 42)))

;;; ============================================================
;;; Cyclic Group Tests
;;; ============================================================

(define-test "Z_3 has correct elements"
  (let ([g (Z 3)])
       (assert-equal '(0 1 2) (group-elements g))
       (assert-equal 0 (group-identity g))))

(define-test "Z_4 addition works correctly"
  (let ([g (Z 4)])
       (assert-equal 0 (group-compose g 1 3))
       (assert-equal 2 (group-compose g 1 1))
       (assert-equal 3 (group-compose g 2 1))))

(define-test "Z_5 inverses are correct"
  (let ([g (Z 5)])
       (assert-equal 4 (group-inverse g 1))
       (assert-equal 3 (group-inverse g 2))
       (assert-equal 0 (group-inverse g 0))))

(define-test "Z_n satisfies group axioms"
  (assert-true (verify-group-axioms (Z 2)))
  (assert-true (verify-group-axioms (Z 3)))
  (assert-true (verify-group-axioms (Z 5))))

(define-test "cyclic group element order"
  (let ([g (Z 6)])
       (assert-equal 6 (element-order g 1))
       (assert-equal 3 (element-order g 2))
       (assert-equal 2 (element-order g 3))
       (assert-equal 1 (element-order g 0))))

;;; ============================================================
;;; Permutation Group Tests
;;; ============================================================

(define-test "permutation-identity is identity"
  (let ([id (permutation-identity 3)])
       (assert-equal '(0 1 2) id)
       (assert-equal 0 (permutation-apply id 0))
       (assert-equal 1 (permutation-apply id 1))
       (assert-equal 2 (permutation-apply id 2))))

(define-test "permutation-compose works correctly"
  (let* ([p1 '(1 0 2)]  ; swap 0 and 1
         [p2 '(0 2 1)]  ; swap 1 and 2
         [composed (permutation-compose p1 p2)])
        ;; (p1 ∘ p2)(i) = p1(p2(i))
        ;; p2: 0→0, 1→2, 2→1
        ;; p1: 0→1, 1→0, 2→2
        ;; composed: 0→1, 1→2, 2→0
        (assert-equal '(1 2 0) composed)))

(define-test "permutation-inverse inverts correctly"
  (let* ([p '(1 2 0)]
         [inv (permutation-inverse p)])
        ;; p: 0→1, 1→2, 2→0
        ;; inv: 0→2, 1→0, 2→1
        (assert-equal '(2 0 1) inv)
        ;; Composing with inverse gives identity
        (assert-equal '(0 1 2) (permutation-compose p inv))))

(define-test "S_2 has 2 elements"
  (let ([g (S 2)])
       (assert-equal 2 (group-order g))
       (assert-true (verify-group-axioms g))))

(define-test "S_3 has 6 elements"
  (let ([g (S 3)])
       (assert-equal 6 (group-order g))
       (assert-true (verify-group-axioms g))))

(define-test "cycle-to-permutation creates correct permutation"
  (let ([p (cycle-to-permutation 3 '(0 1 2))])
       ;; 3-cycle: 0→1, 1→2, 2→0
       (assert-equal '(1 2 0) p)))

(define-test "transposition creates swap"
  (let ([t (transposition 3 0 1)])
       (assert-equal '(1 0 2) t)))

(define-test "permutation-parity identifies even/odd"
  (assert-equal 0 (permutation-parity '(0 1 2)))  ; identity is even
  (assert-equal 1 (permutation-parity '(1 0 2)))  ; transposition is odd
  (assert-equal 0 (permutation-parity '(1 2 0)))) ; 3-cycle is even

;;; ============================================================
;;; Dihedral Group Tests
;;; ============================================================

(define-test "D_3 has 6 elements"
  (let ([g (D 3)])
       (assert-equal 6 (group-order g))))

(define-test "D_4 has 8 elements"
  (let ([g (D 4)])
       (assert-equal 8 (group-order g))))

(define-test "D_n identity is r^0"
  (let ([g (D 4)])
       (assert-equal (cons 'r 0) (group-identity g))))

(define-test "D_3 satisfies group axioms"
  (assert-true (verify-group-axioms (D 3))))

(define-test "D_4 rotation composition"
  (let* ([g (D 4)]
         [r1 (cons 'r 1)]
         [r2 (cons 'r 2)])
        (assert-equal (cons 'r 3) (group-compose g r1 r2))
        (assert-equal (cons 'r 0) (group-compose g r2 r2))))

(define-test "D_n reflections are self-inverse"
  (let* ([g (D 4)]
         [s0 (cons 's 0)]
         [s1 (cons 's 1)])
        (assert-equal (group-identity g) (group-compose g s0 s0))
        (assert-equal (group-identity g) (group-compose g s1 s1))))

;;; ============================================================
;;; Group Power Tests
;;; ============================================================

(define-test "group-power computes powers correctly"
  (let ([g (Z 7)])
       (assert-equal 0 (group-power g 3 0))
       (assert-equal 3 (group-power g 3 1))
       (assert-equal 6 (group-power g 3 2))
       (assert-equal 2 (group-power g 3 3))))

(define-test "group-power with negative exponent"
  (let ([g (Z 5)])
       ;; 2^(-1) in Z_5 is 3 (since 2+3=5≡0, additive inverse)
       (assert-equal 3 (group-power g 2 -1))
       ;; 2^(-2) = 3+3 = 6 ≡ 1 (mod 5)
       (assert-equal 1 (group-power g 2 -2))))

;;; ============================================================
;;; Subgroup Tests
;;; ============================================================

(define-test "trivial subgroup is subgroup"
  (let ([g (Z 6)])
       (assert-true (is-subgroup? g '(0)))))

(define-test "whole group is subgroup"
  (let ([g (Z 4)])
       (assert-true (is-subgroup? g (group-elements g)))))

(define-test "even elements form subgroup of Z_6"
  (let ([g (Z 6)])
       ;; {0, 2, 4} forms a subgroup isomorphic to Z_3
       (assert-true (is-subgroup? g '(0 2 4)))))

(define-test "non-subgroup detected"
  (let ([g (Z 6)])
       ;; {0, 1} is not closed: 1+1=2 not in set
       (assert-false (is-subgroup? g '(0 1)))))

(define-test "generate-subgroup from generator"
  (let* ([g (Z 6)]
         [sub (generate-subgroup g '(2))])
        ;; <2> = {0, 2, 4}
        (assert-equal 3 (length sub))))

;;; ============================================================
;;; Homomorphism Tests
;;; ============================================================

(define-test "identity homomorphism is valid"
  (let* ([g (Z 4)]
         [h (make-homomorphism g g (lambda (x) x))])
        (assert-true (verify-homomorphism h))
        (assert-true (is-isomorphism? h))))

(define-test "projection Z_4 -> Z_2 is homomorphism"
  (let* ([z4 (Z 4)]
         [z2 (Z 2)]
         [phi (lambda (x) (modulo x 2))]
         [h (make-homomorphism z4 z2 phi)])
        (assert-true (verify-homomorphism h))))

(define-test "kernel of projection is {0, 2}"
  (let* ([z4 (Z 4)]
         [z2 (Z 2)]
         [phi (lambda (x) (modulo x 2))]
         [h (make-homomorphism z4 z2 phi)]
         [ker (kernel h)])
        (assert-equal 2 (length ker))
        (assert-true (and (member 0 ker) #t))
        (assert-true (and (member 2 ker) #t))))

(define-test "image of projection is Z_2"
  (let* ([z4 (Z 4)]
         [z2 (Z 2)]
         [phi (lambda (x) (modulo x 2))]
         [h (make-homomorphism z4 z2 phi)]
         [img (image h)])
        (assert-equal 2 (length img))))

(define-test "isomorphism Z_3 -> Z_3"
  (let* ([z3 (Z 3)]
         [phi (lambda (x) (modulo (* 2 x) 3))]  ; multiplication by 2
         [h (make-homomorphism z3 z3 phi)])
        (assert-true (is-isomorphism? h))))

;;; ============================================================
;;; Cayley Table Tests
;;; ============================================================

(define-test "cayley-table for Z_2"
  (let* ([g (Z 2)]
         [table (cayley-table g)])
        (assert-equal '((0 1) (1 0)) table)))

(define-test "cayley-table for Z_3"
  (let* ([g (Z 3)]
         [table (cayley-table g)])
        (assert-equal 3 (length table))
        (assert-equal 3 (length (car table)))))

;;; ============================================================
;;; Common Groups Tests
;;; ============================================================

(define-test "trivial group has one element"
  (let ([g (trivial-group)])
       (assert-equal 1 (group-order g))
       (assert-true (verify-group-axioms g))))

(define-test "klein-four-group has order 4"
  (let ([g (klein-four-group)])
       (assert-equal 4 (group-order g))))

(define-test "klein-four-group all elements self-inverse"
  (let ([g (klein-four-group)])
       (let loop ([elems (group-elements g)])
            (if (null? elems)
                (assert-true #t)
                (begin
                 (assert-equal (car elems) (group-inverse g (car elems)))
                 (loop (cdr elems)))))))

(define-test "quaternion-group has order 8"
  (let ([g (quaternion-group)])
       (assert-equal 8 (group-order g))))

(define-test "quaternion-group identity"
  (let ([g (quaternion-group)])
       (assert-equal 'e1 (group-identity g))))

(define-test "quaternion qi*qj=qk"
  (let ([g (quaternion-group)])
       (assert-equal 'qk (group-compose g 'qi 'qj))))

(define-test "quaternion qj*qk=qi"
  (let ([g (quaternion-group)])
       (assert-equal 'qi (group-compose g 'qj 'qk))))

(define-test "quaternion qk*qi=qj"
  (let ([g (quaternion-group)])
       (assert-equal 'qj (group-compose g 'qk 'qi))))

(define-test "quaternion qi^2=qj^2=qk^2=e-1"
  (let ([g (quaternion-group)])
       (assert-equal 'e-1 (group-compose g 'qi 'qi))
       (assert-equal 'e-1 (group-compose g 'qj 'qj))
       (assert-equal 'e-1 (group-compose g 'qk 'qk))))

;;; ============================================================
;;; Run Tests
;;; ============================================================

(display "Running Group Theory Tests...\n")
(display "============================\n\n")
(run-tests 'group-theory)
