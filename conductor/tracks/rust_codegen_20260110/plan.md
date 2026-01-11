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

## Phase 3: Integration and Verification
- [ ] Task: Integrate with `fold-accel` Crate Structure
- [ ] Task: End-to-End Generation and Compilation Test
- [ ] Task: Conductor - User Manual Verification 'Phase 3: Integration and Verification' (Protocol in workflow.md)
