# Plan: Rust Codegen for Primitives and Layer 1

## Phase 1: Design and Mapping [checkpoint: 69c2594]
- [x] Task: Define Type Mapping (Scheme to Rust) d24ef66
- [x] Task: Design Codegen Architecture (AST Transformation) 780a0e4
- [x] Task: Conductor - User Manual Verification 'Phase 1: Design and Mapping' (Protocol in workflow.md)

## Phase 2: Core Codegen Implementation [checkpoint: 7377d4a]
- [x] Task: Implement Basic Block-to-Rust Serializer cddbbb3
    - [x] Write Tests (Scheme-side verification of emitted strings)
    - [x] Implement Serializer
- [x] Task: Implement Function/Lambda Codegen cddbbb3
    - [x] Write Tests
    - [x] Implement
- [x] Task: Implement Layer 1 Arithmetic and Logic Codegen cddbbb3
    - [x] Write Tests
    - [x] Implement
- [x] Task: Conductor - User Manual Verification 'Phase 2: Core Codegen Implementation' (Protocol in workflow.md)

## Phase 3: Integration and Verification [checkpoint: 58548da]
- [x] Task: Integrate with `fold-accel` Crate Structure e6605c7
- [x] Task: End-to-End Generation and Compilation Test 17d0b81
- [x] Task: Conductor - User Manual Verification 'Phase 3: Integration and Verification' (Protocol in workflow.md)

## Phase 4: Review and Completion [checkpoint: 54c2b6d]
Review identified gaps in Phase 2-3 implementation. Fixed by Claude (Opus).

- [x] Task: Fix rust-mapping.ss missing helpers 54c2b6d
    - Added: function-param-types, function-return-type, join-strings
- [x] Task: Add complete Layer 1 operator mappings 54c2b6d
    - Comparison: lt?, le?, gt?, ge?, eq? → <, <=, >, >=, ==
    - Logical: and, or, not → &&, ||, !
    - Bitwise: bitand, bitor, bitxor, shl, shr → &, |, ^, <<, >>
    - Math methods: abs, sqrt, sin, cos, tan, log, floor, ceiling
    - Unary: neg, expt (powf)
- [x] Task: Implement scheme->rust-ir translator 54c2b6d
    - Translates Scheme expressions to Rust IR automatically
    - Supports: literals, variables, prim calls, let, if, direct ops
- [x] Task: Add R-Block IR node for let bindings 54c2b6d
- [x] Task: Clean up code formatting 54c2b6d
- [x] Task: Add comprehensive tests (40 new tests) 54c2b6d

## Remaining Work (Future)
Tracked in beads:
- fold-49ht: Full closure/recursion support in translator
- fold-4s4q: Type-aware result emission (bool→f64 casting issue)
- fold-id7k: TestResult crate integration (currently inline for standalone)
- fold-jppr: Division-by-zero protection
- fold-ulzh: Variadic primitive support
