;;; lattice/validation/manifest.sexp — Validation Contracts Skill Manifest

(skill validation
  (version "0.1.0")
  (tier 1)
  (path "lattice/validation")
  (purity total)
  (stability experimental)
  (fuel-bound "O(n) for contract checking, O(n*m) for registry operations")
  (deps ())  ; No lattice deps — uses only core (prelude, sha256)

  (description
   "Content-addressed validation contracts. Contracts are declarative
    specifications of data shape expressed as S-expressions. Each contract
    gets a SHA256 hash from its canonical form, enabling versioning,
    provenance tracking, and CAS storage. Provides checking (predicate)
    and validation (structured error) modes.")

  (keywords (validation contract content-addressed schema predicate
             boundary defensive checking))
  (aliases (contracts))

  (exports
   (contract
    ;; Contract construction
    make-contract contract? contract-name contract-spec contract-hash
    ;; Primitive specs
    spec/symbol spec/string spec/number spec/integer spec/boolean
    spec/bytevector spec/pair spec/null spec/vector spec/char spec/list
    ;; Combinators
    spec/and spec/or spec/not
    ;; Collections
    spec/list-of spec/vector-of spec/pair-of
    ;; Numeric constraints
    spec/range spec/length= spec/length>=
    ;; Structural
    spec/fields spec/optional
    ;; References
    spec/ref
    ;; Checking and validation
    contract-check contract-validate
    ;; Registry
    make-contract-registry registry-add! registry-lookup registry-contracts
    registry-hash
    ;; Serialization
    contract-serialize))

  (modules
   (contract "contract.ss" "Content-addressed validation contracts with declarative spec language")))
