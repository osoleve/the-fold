# File Locking Implementation QA Review

**Reviewer:** Gemini 3 Pro Preview
**Date:** 2026-01-16
**Files Reviewed:**
- `boundary/io/file-lock.ss` (new)
- `boundary/io/atomic.ss` (updated)
- `boundary/bbs/counter.ss` (updated)
- `boundary/bbs/heads.ss` (updated)

---

## Findings

### 1. Critical Race Condition in Stale Lock Handling

**File:** `boundary/io/file-lock.ss:90`

**Issue:** Breaking a stale lock is not atomic.

If two processes (P1, P2) detect a stale lock simultaneously:

1. P1 checks `lock-file-stale?` -> true.
2. P2 checks `lock-file-stale?` -> true.
3. P1 deletes the lock file (`remove-lock-file`).
4. P1 successfully creates a new lock (`try-create-lock-file`).
5. P2 deletes the lock file (deleting P1's valid lock).
6. P2 successfully creates a new lock.

**Result:** Both P1 and P2 hold the lock concurrently.

**Recommendation:** Breaking stale locks safely with only `O_EXCL` is difficult. Ideally, include a random token in the lock filename or content and verify identity before deletion, or use `flock()` which handles process termination automatically.

---

### 2. Lock Bypassing in Public Writers

**File:** `boundary/bbs/counter.ss:73` (`bbs-write-counter!`)
**File:** `boundary/bbs/heads.ss:67` (`bbs-write-head!`)
**File:** `boundary/bbs/heads.ss:77` (`bbs-delete-head!`)

**Issue:** These functions modify protected resources without acquiring the corresponding lock.

`bbs-next-id!` and `bbs-cas-head!` rely on `with-file-lock` for safety. However, `bbs-write-counter!` (used by `bbs-sync-counter-from-heads!`) and `bbs-delete-head!` ignore the lock file entirely.

**Scenario:**

- P1 calls `bbs-next-id!`, acquires lock, reads value X.
- P2 calls `bbs-sync-counter-from-heads!`, calculates Y, calls `bbs-write-counter!(Y)`.
- `bbs-write-counter!` overwrites the counter file immediately via `atomic-write-file` (which doesn't check for `.lock`).
- P1 writes X+1, potentially overwriting P2's update or basing it on stale data.

---

### 3. Temporary File Collision

**File:** `boundary/io/atomic.ss:28`

**Issue:** Fixed temporary filename suffix `.tmp` causes collisions for concurrent writers.

`atomic-write-file` uses `(string-append path ".tmp")`. If two processes attempt to write to `path` concurrently (e.g., one holding a lock and one bypassing it, as per Issue #2), they will both write to `path.tmp`.

**Result:** One process's data will corrupt the other's temp file before rename, or the `rename` will effectively commit a mix of data/metadata from two processes.

---

### 4. Unreliable Process ID

**File:** `boundary/io/file-lock.ss:54`

**Issue:** `current-process-id` uses `(modulo addr 1000000)` of a pointer address.

This is not a valid PID. It is not unique across processes (collisions are likely) and it does not correspond to the OS PID. If the stale lock logic is ever enhanced to check if the process is alive (common practice), this will fail.

---

### 5. Redundant TOCTOU Check

**File:** `boundary/io/file-lock.ss:40`

**Issue:** `(if (file-exists? lock-path) ...)` before `call-with-output-file ... '(exclusive)`.

The existence check is a Time-of-Check-Time-of-Use race. While the subsequent `exclusive` open handles the safety correctly, this check is redundant and can be misleading during debugging or if the filesystem state changes rapidly.

---

## Summary

The current implementation relies on cooperative locking, but the API exposes functions that break this cooperation. The stale lock handling mechanism introduces a race condition that can lead to dual ownership of a lock. The `current-process-id` implementation is functionally incorrect for any OS-level validation.

---

## Severity Assessment

| Issue | Severity | Likelihood | Impact |
|-------|----------|------------|--------|
| Stale lock race condition | High | Medium | Data corruption, dual lock ownership |
| Lock bypassing in writers | High | High | Counter/head state corruption |
| Temp file collision | Medium | Medium | Data corruption during concurrent writes |
| Unreliable process ID | Low | Low | Incorrect stale lock detection |
| Redundant TOCTOU check | Low | N/A | Code clarity, no functional impact |
