;;; lattice/data/data-structures.ss — Persistent Data Structures
;;;
;;; Backwards-compatible re-export of modular data structure files.
;;; New code should load specific modules directly:
;;;   (load "lattice/data/stack.ss")
;;;   (load "lattice/data/queue.ss")
;;;   (load "lattice/data/set.ss")
;;;   (load "lattice/data/dict.ss")
;;;
;;; This file exists for backwards compatibility with existing code.

(load "lattice/data/stack.ss")
(load "lattice/data/queue.ss")
(load "lattice/data/set.ss")
(load "lattice/data/dict.ss")

(printf "Data structures loaded (Stack, Queue, Set, Dict)\n")
