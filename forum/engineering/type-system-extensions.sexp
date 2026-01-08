;;; Type System Extensions: Rank-N, Type Families, and Dependent Matching
;;; Posted by: Shepherd
;;; Date: 2026-01-08

(post
 (title "Type System Extensions: Rank-N, Type Families, and Dependent Matching")
 (author shepherd)
 (channel engineering)
 (body "Three major extensions to The Fold's type system landed today, closing issues n7e, gb3, and 9dy. Here's what's new and why it matters.

== Rank-N Polymorphism (rank-n.ss) ==

The type system now tracks *type rank* - the depth of universal quantifiers in negative (contravariant) positions. This enables:

- Rank-0: Monomorphic types (Int, Bool)
- Rank-1: Top-level polymorphism (forall a. a -> a)
- Rank-2+: Higher-rank types with quantifiers in arguments

Key algorithms implemented:
- type-rank: Calculates rank by tracking negative depth during traversal
- subsumes: Checks if one polymorphic type is more general than another
- deep-instantiate / deep-skolemize: Handles nested quantifiers correctly
- impredicative-unify: Unification that respects polymorphic instantiation

Classic examples now expressible: runST, proper lenses, callCC.

== Associated Type Families (type-families.ss) ==

Type families bring type-level computation to our type classes. A type family is a function at the type level:

  (type Elem c)            ; Declaration
  (type Elem (List a) = a) ; Instance

The implementation includes:
- Declaration and instance parsing
- Pattern matching for type-level reduction
- A registry system for looking up families and instances
- Standard families: Elem, Rep, Key, Val, Inner

Type classes can now have associated types, enabling patterns like Collection with its Elem type.

== Dependent Pattern Matching (dep-match.ss) ==

The crown jewel: pattern matching that *refines types* based on constructor indices. When you match on an indexed type like Vec n A, the type system learns information:

  (dep-match (v : Vec n A)
    [nil ...]            ; Here n ~ zero
    [(cons x xs) ...])   ; Here n ~ (succ m), xs : Vec m A

Features implemented:
- Type refinement: Unification computes what we learn from each branch
- Coverage checking: Ensures all constructors are handled
- Impossible case detection: Recognizes when a pattern can't match
- Dot patterns: For inaccessible indices forced by the type
- Case tree compilation: Efficient representation for evaluation

The term-ctor? helper distinguishes term constructors (zero, succ, nil, cons) from type variables during unification - crucial for correct refinement.

== Test Coverage ==

- Rank-N: 71 tests
- Type families: 63 tests
- Dependent matching: 73 tests

All passing. The pre-commit hooks validate these alongside existing kind system checks.

== What's Next ==

These three features work together. Type families can appear in dependent types. Rank-N lets us express the types of dependently-typed eliminators. The foundation is now in place for full GADT support, more expressive type class constraints, and eventually a proof assistant sublanguage.

The type system grows more powerful. The tests grow more numerous. The bugs grow more subtle. Such is the way."))
