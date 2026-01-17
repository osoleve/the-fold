//! POSIX FFI primitives for The Fold
//!
//! Provides system-level operations that Chez Scheme cannot access directly:
//! - getpid: true OS process ID
//! - flock: advisory file locking with automatic cleanup on process death
//! - open/close: low-level file descriptor operations with O_EXCL
//!
//! Design principles:
//! - All functions use out-pointer pattern for FFI safety
//! - Non-blocking flock (LOCK_NB) to avoid hanging Chez runtime
//! - No panics - all errors return status codes

use std::os::unix::io::RawFd;

// ====
// Result structs for FFI
// ====

/// Result struct for operations returning an integer (fd, pid, etc.)
#[repr(C)]
pub struct IntResult {
    /// Status: 0=success, 1=error
    pub status: u8,
    /// Result value (fd, pid, etc.)
    pub value: i32,
    /// Error code (errno) if status != 0
    pub error_code: i32,
}

/// Result struct for operations returning only status
#[repr(C)]
pub struct StatusResult {
    /// Status: 0=success, 1=error
    pub status: u8,
    /// Error code (errno) if status != 0
    pub error_code: i32,
}

// ====
// flock constants (match POSIX values)
// ====

/// Shared lock (read lock)
pub const LOCK_SH: i32 = libc::LOCK_SH;
/// Exclusive lock (write lock)
pub const LOCK_EX: i32 = libc::LOCK_EX;
/// Non-blocking (CRITICAL: prevents Chez runtime hang)
pub const LOCK_NB: i32 = libc::LOCK_NB;
/// Unlock
pub const LOCK_UN: i32 = libc::LOCK_UN;

// ====
// open flags
// ====

/// Create file if not exists
pub const O_CREAT: i32 = libc::O_CREAT;
/// Fail if file exists (for atomic creation)
pub const O_EXCL: i32 = libc::O_EXCL;
/// Read-write access
pub const O_RDWR: i32 = libc::O_RDWR;
/// Read-only access
pub const O_RDONLY: i32 = libc::O_RDONLY;
/// Write-only access
pub const O_WRONLY: i32 = libc::O_WRONLY;
/// Close on exec (prevents FD inheritance to child processes)
pub const O_CLOEXEC: i32 = libc::O_CLOEXEC;

// ====
// POSIX operations
// ====

/// Get the current process ID
///
/// # Safety
/// Caller must provide valid pointer to IntResult
#[no_mangle]
pub extern "C" fn fold_posix_getpid(out: *mut IntResult) {
    if out.is_null() {
        return;
    }

    let result = unsafe { &mut *out };
    // getpid() always succeeds
    result.status = 0;
    result.value = unsafe { libc::getpid() };
    result.error_code = 0;
}

/// Open a file and return file descriptor
///
/// # Arguments
/// * `path_ptr` - Pointer to UTF-8 path string
/// * `path_len` - Length of path string (not including null terminator)
/// * `flags` - Open flags (O_CREAT, O_EXCL, O_RDWR, etc.)
/// * `mode` - File mode if creating (e.g., 0o644)
/// * `out` - Output pointer for result
///
/// # Safety
/// Caller must provide valid pointers and ensure path_len is correct
#[no_mangle]
pub extern "C" fn fold_posix_open(
    path_ptr: *const u8,
    path_len: usize,
    flags: i32,
    mode: u32,
    out: *mut IntResult,
) {
    if out.is_null() || path_ptr.is_null() {
        return;
    }

    let result = unsafe { &mut *out };

    // Convert path bytes to CString for libc
    let path_slice = unsafe { std::slice::from_raw_parts(path_ptr, path_len) };
    let path_str = match std::str::from_utf8(path_slice) {
        Ok(s) => s,
        Err(_) => {
            result.status = 1;
            result.value = -1;
            result.error_code = libc::EINVAL;
            return;
        }
    };

    // Create null-terminated string for libc
    let mut path_cstr = path_str.as_bytes().to_vec();
    path_cstr.push(0);

    let fd = unsafe { libc::open(path_cstr.as_ptr() as *const libc::c_char, flags, mode) };

    if fd < 0 {
        result.status = 1;
        result.value = -1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.value = fd;
        result.error_code = 0;
    }
}

/// Close a file descriptor
///
/// # Safety
/// Caller must provide valid fd and result pointer
#[no_mangle]
pub extern "C" fn fold_posix_close(fd: RawFd, out: *mut StatusResult) {
    if out.is_null() {
        return;
    }

    let result = unsafe { &mut *out };
    let ret = unsafe { libc::close(fd) };

    if ret < 0 {
        result.status = 1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.error_code = 0;
    }
}

/// Apply advisory lock to file descriptor
///
/// IMPORTANT: Always use LOCK_NB (non-blocking) from Scheme!
/// Blocking flock would hang the entire Chez runtime since it uses
/// cooperative threading.
///
/// # Arguments
/// * `fd` - File descriptor to lock
/// * `operation` - LOCK_SH, LOCK_EX, LOCK_UN, optionally OR'd with LOCK_NB
/// * `out` - Output pointer for result
///
/// # Returns
/// Status 0 on success, 1 on error (check error_code for EWOULDBLOCK)
///
/// # Safety
/// Caller must provide valid fd and result pointer
#[no_mangle]
pub extern "C" fn fold_posix_flock(fd: RawFd, operation: i32, out: *mut StatusResult) {
    if out.is_null() {
        return;
    }

    let result = unsafe { &mut *out };
    let ret = unsafe { libc::flock(fd, operation) };

    if ret < 0 {
        result.status = 1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.error_code = 0;
    }
}

// ====
// Constant accessors for Scheme
// ====

/// Get LOCK_SH constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_lock_sh() -> i32 {
    LOCK_SH
}

/// Get LOCK_EX constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_lock_ex() -> i32 {
    LOCK_EX
}

/// Get LOCK_NB constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_lock_nb() -> i32 {
    LOCK_NB
}

/// Get LOCK_UN constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_lock_un() -> i32 {
    LOCK_UN
}

/// Get O_CREAT constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_o_creat() -> i32 {
    O_CREAT
}

/// Get O_EXCL constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_o_excl() -> i32 {
    O_EXCL
}

/// Get O_RDWR constant for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_o_rdwr() -> i32 {
    O_RDWR
}

/// Get O_CLOEXEC constant for Scheme (close on exec - prevents FD inheritance)
#[no_mangle]
pub extern "C" fn fold_posix_o_cloexec() -> i32 {
    libc::O_CLOEXEC
}

/// Get EWOULDBLOCK constant for Scheme (non-blocking lock would block)
#[no_mangle]
pub extern "C" fn fold_posix_ewouldblock() -> i32 {
    libc::EWOULDBLOCK
}

/// Get EEXIST constant for Scheme (O_EXCL file exists)
#[no_mangle]
pub extern "C" fn fold_posix_eexist() -> i32 {
    libc::EEXIST
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_getpid() {
        let mut result = IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };
        fold_posix_getpid(&mut result);
        assert_eq!(result.status, 0);
        assert!(result.value > 0);
    }

    #[test]
    fn test_open_close() {
        let path = "/tmp/fold_posix_test.txt";
        let path_bytes = path.as_bytes();

        // Clean up any existing file
        let _ = fs::remove_file(path);

        let mut open_result = IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };

        // Open with O_CREAT | O_RDWR
        fold_posix_open(
            path_bytes.as_ptr(),
            path_bytes.len(),
            O_CREAT | O_RDWR,
            0o644,
            &mut open_result,
        );
        assert_eq!(open_result.status, 0);
        assert!(open_result.value >= 0);

        let fd = open_result.value;

        // Close
        let mut close_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_close(fd, &mut close_result);
        assert_eq!(close_result.status, 0);

        // Clean up
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_open_excl_fails_if_exists() {
        let path = "/tmp/fold_posix_excl_test.txt";
        let path_bytes = path.as_bytes();

        // Create the file first
        fs::write(path, "exists").unwrap();

        let mut result = IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };

        // Try to open with O_CREAT | O_EXCL - should fail
        fold_posix_open(
            path_bytes.as_ptr(),
            path_bytes.len(),
            O_CREAT | O_EXCL | O_RDWR,
            0o644,
            &mut result,
        );
        assert_eq!(result.status, 1);
        assert_eq!(result.error_code, libc::EEXIST);

        // Clean up
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_flock_nonblocking() {
        let path = "/tmp/fold_flock_test.txt";
        let path_bytes = path.as_bytes();

        // Clean up and create file
        let _ = fs::remove_file(path);

        let mut open_result = IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };
        fold_posix_open(
            path_bytes.as_ptr(),
            path_bytes.len(),
            O_CREAT | O_RDWR,
            0o644,
            &mut open_result,
        );
        assert_eq!(open_result.status, 0);
        let fd = open_result.value;

        // Acquire exclusive lock (non-blocking)
        let mut lock_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_flock(fd, LOCK_EX | LOCK_NB, &mut lock_result);
        assert_eq!(lock_result.status, 0);

        // Unlock
        let mut unlock_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_flock(fd, LOCK_UN, &mut unlock_result);
        assert_eq!(unlock_result.status, 0);

        // Close
        let mut close_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_close(fd, &mut close_result);

        // Clean up
        let _ = fs::remove_file(path);
    }
}
