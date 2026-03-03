;;; lattice/linalg/matrix-blocked-decomp.ss — Parallelizable Decomposition Kernels
;;; @module matrix-blocked-decomp
;;; @requires prelude

(require 'prelude)

(doc 'module 'matrix-blocked-decomp)
(doc 'description "Range-parameterized kernels extracted from LU and QR for parallel dispatch.")
(doc 'layer 'lattice)
(doc 'purity 'partial)
(doc 'note "Client must load prelude.ss, vec.ss, matrix.ss before this file. Does NOT require matrix-decomp.ss.")

;;; ====
;;; LU Column Update Kernel
;;; ====

;;; lu-column-update : Vector × Nat × Nat × Nat × Nat → Void
;;; Row elimination for rows [from-row, to-row) at pivot column k.
;;; Operates on the flat data vector of an in-place LU working copy.
;;; Each row i in [from-row, to-row):
;;;   factor = data[i,k] / data[k,k]
;;;   data[i,k] = factor  (store L factor)
;;;   data[i,j] -= factor * data[k,j]  for j in (k+1, n)
;;;
;;; Workers process disjoint row ranges → no write conflicts.
(define (lu-column-update data n k from-row to-row)
  (let ([pivot (vector-ref data (+ (* k n) k))])
    (do ([i from-row (+ i 1)])
        ((= i to-row))
      (let ([factor (/ (vector-ref data (+ (* i n) k)) pivot)])
        (vector-set! data (+ (* i n) k) factor)
        (do ([j (+ k 1) (+ j 1)])
            ((= j n))
          (vector-set! data (+ (* i n) j)
            (- (vector-ref data (+ (* i n) j))
               (* factor (vector-ref data (+ (* k n) j))))))))))

;;; ====
;;; QR Column Orthogonalize Kernel
;;; ====

;;; qr-column-orthogonalize : Vector × Vector × Nat × Nat × Nat × Nat × Nat → Void
;;; Orthogonalize columns [from-col, to-col) against column j in Q.
;;; q-data: flat row-major data vector of Q (m × n)
;;; r-data: flat row-major data vector of R (n × n)
;;; m: number of rows in Q
;;; n: number of columns in Q (and size of R)
;;; j: the reference column that cols are being orthogonalized against
;;;
;;; For each column i in [from-col, to-col):
;;;   proj = dot(Q[:,j], Q[:,i])
;;;   R[j,i] = proj
;;;   Q[:,i] -= proj * Q[:,j]
;;;
;;; Workers process disjoint column ranges → no write conflicts on Q or R.
(define (qr-column-orthogonalize q-data r-data m n j from-col to-col)
  (do ([i from-col (+ i 1)])
      ((= i to-col))
    ;; Compute dot product of column j and column i of Q
    (let ([proj (let dot-loop ([row 0] [sum 0])
                  (if (= row m)
                      sum
                      (dot-loop (+ row 1)
                                (+ sum (* (vector-ref q-data (+ (* row n) j))
                                          (vector-ref q-data (+ (* row n) i)))))))])
      ;; Store in R
      (vector-set! r-data (+ (* j n) i) proj)
      ;; Update column i of Q: Q[:,i] -= proj * Q[:,j]
      (do ([row 0 (+ row 1)])
          ((= row m))
        (let ([idx (+ (* row n) i)])
          (vector-set! q-data idx
            (- (vector-ref q-data idx)
               (* proj (vector-ref q-data (+ (* row n) j))))))))))
