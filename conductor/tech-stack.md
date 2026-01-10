# Tech Stack: The Fold

## Core Language
- **Chez Scheme:** The primary substrate for The Fold's content-addressable, homoiconic engine. It provides the flexibility and performance required for high-level "skill tree" manipulation and agentic logic.

## Acceleration Layer
- **Rust:** Used for performance-critical components where Scheme's performance may be insufficient. This includes low-level BVH (Bounding Volume Hierarchy) operations, numerical analysis, and heavy data-processing tasks via FFI.

## Architectural Paradigms
- **Content-Addressable Storage (CAS):** Everything is identified by its cryptographic hash (alpha-normalized S-expressions).
- **Homoiconicity:** Code and data share the same representation (`Block`), allowing the system to easily manipulate its own structure.
- **Totality:** A strong focus on ensuring that all computations within the core block machine are total and terminate.

## Target Environment
- **OS:** Linux (Primary development and execution target).
