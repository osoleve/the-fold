## 11. Future Work


**Distributed CAS**: Extend the CAS to peer-to-peer networks, enabling decentralized code sharing with content verification.

**Concurrent Access**: Add MVCC (Multi-Version Concurrency Control) for safe concurrent reads/writes to the CAS.

**Algebraic Effects**: Integrate algebraic effects more deeply, replacing the current capability/monad approach.

**Linear Types**: Add linear/affine types for safe resource management in Shell.

**Incremental Type Checking**: Cache type derivations in the CAS, enabling O(changed) re-checking instead of O(total).

**Formal Verification**: Mechanize the Core semantics in a proof assistant, proving type soundness and other properties.

---
