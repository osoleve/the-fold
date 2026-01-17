//! POSIX FFI primitives for The Fold
//!
//! Provides system-level operations that Chez Scheme cannot access directly:
//! - getpid: true OS process ID
//! - flock: advisory file locking with automatic cleanup on process death
//! - open/close: low-level file descriptor operations with O_EXCL
//! - fsync/fdatasync: force data to disk for durability
//! - stat: file metadata (size, mtime, mode, type)
//!
//! Design principles:
//! - All functions use out-pointer pattern for FFI safety
//! - Non-blocking flock (LOCK_NB) to avoid hanging Chez runtime
//! - No panics - all errors return status codes
//! - stat uses buffer pattern to avoid Scheme-side allocation

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
// Durability operations
// ====

/// Force all buffered data for fd to be written to disk
///
/// WARNING: This is a blocking operation that can take 10-100ms on rotating media.
/// Use sparingly, only for critical data that must survive power loss.
///
/// # Safety
/// Caller must provide valid fd and result pointer
#[no_mangle]
pub extern "C" fn fold_posix_fsync(fd: RawFd, out: *mut StatusResult) {
    if out.is_null() {
        return;
    }

    let result = unsafe { &mut *out };
    let ret = unsafe { libc::fsync(fd) };

    if ret < 0 {
        result.status = 1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.error_code = 0;
    }
}

/// Force buffered data (but not metadata) for fd to be written to disk
///
/// Faster than fsync() because it skips non-essential metadata like atime.
/// Preferred for atomic writes where only data integrity matters.
///
/// # Safety
/// Caller must provide valid fd and result pointer
#[no_mangle]
pub extern "C" fn fold_posix_fdatasync(fd: RawFd, out: *mut StatusResult) {
    if out.is_null() {
        return;
    }

    let result = unsafe { &mut *out };
    let ret = unsafe { libc::fdatasync(fd) };

    if ret < 0 {
        result.status = 1;
        result.error_code = unsafe { *libc::__errno_location() };
    } else {
        result.status = 0;
        result.error_code = 0;
    }
}

// ====
// File metadata operations
// ====

/// Stat buffer layout (all values in native byte order):
/// - offset 0:  size (u64) - file size in bytes
/// - offset 8:  mtime_sec (i64) - modification time seconds since epoch
/// - offset 16: mtime_nsec (i64) - modification time nanoseconds
/// - offset 24: mode (u32) - file mode/permissions
/// - offset 28: file_type (u32) - 1=regular, 2=directory, 3=symlink, 0=other
///
/// Total buffer size: 32 bytes
pub const STAT_BUFFER_SIZE: usize = 32;

/// File type constants for stat buffer
pub const FILE_TYPE_OTHER: u32 = 0;
pub const FILE_TYPE_REGULAR: u32 = 1;
pub const FILE_TYPE_DIRECTORY: u32 = 2;
pub const FILE_TYPE_SYMLINK: u32 = 3;

/// Get file metadata and write to pre-allocated buffer
///
/// This uses the buffer pattern to avoid Scheme-side allocation per call.
/// Scheme passes a pre-allocated 32-byte bytevector, Rust fills it.
///
/// # Arguments
/// * `path_ptr` - Pointer to UTF-8 path string
/// * `path_len` - Length of path string
/// * `out_buf` - Pointer to 32-byte output buffer (must be aligned to 8 bytes)
/// * `out_status` - Pointer to StatusResult for error reporting
///
/// # Buffer Layout
/// See STAT_BUFFER_SIZE documentation above.
///
/// # Safety
/// Caller must provide valid pointers and ensure buffer is at least 32 bytes
#[no_mangle]
pub extern "C" fn fold_posix_stat(
    path_ptr: *const u8,
    path_len: usize,
    out_buf: *mut u8,
    out_status: *mut StatusResult,
) {
    if out_status.is_null() || path_ptr.is_null() || out_buf.is_null() {
        return;
    }

    let status = unsafe { &mut *out_status };

    // Convert path bytes to CString for libc
    let path_slice = unsafe { std::slice::from_raw_parts(path_ptr, path_len) };
    let path_str = match std::str::from_utf8(path_slice) {
        Ok(s) => s,
        Err(_) => {
            status.status = 1;
            status.error_code = libc::EINVAL;
            return;
        }
    };

    // Create null-terminated string for libc
    let mut path_cstr = path_str.as_bytes().to_vec();
    path_cstr.push(0);

    // Call stat
    let mut stat_buf: libc::stat = unsafe { std::mem::zeroed() };
    let ret = unsafe { libc::stat(path_cstr.as_ptr() as *const libc::c_char, &mut stat_buf) };

    if ret < 0 {
        status.status = 1;
        status.error_code = unsafe { *libc::__errno_location() };
        return;
    }

    // Determine file type
    let file_type = if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFREG {
        FILE_TYPE_REGULAR
    } else if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFDIR {
        FILE_TYPE_DIRECTORY
    } else if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFLNK {
        FILE_TYPE_SYMLINK
    } else {
        FILE_TYPE_OTHER
    };

    // Write to output buffer
    // Note: We write each field individually to handle alignment correctly
    unsafe {
        // size (u64) at offset 0
        let size = stat_buf.st_size as u64;
        std::ptr::copy_nonoverlapping(size.to_ne_bytes().as_ptr(), out_buf, 8);

        // mtime_sec (i64) at offset 8
        let mtime_sec = stat_buf.st_mtime as i64;
        std::ptr::copy_nonoverlapping(mtime_sec.to_ne_bytes().as_ptr(), out_buf.add(8), 8);

        // mtime_nsec (i64) at offset 16
        let mtime_nsec = stat_buf.st_mtime_nsec as i64;
        std::ptr::copy_nonoverlapping(mtime_nsec.to_ne_bytes().as_ptr(), out_buf.add(16), 8);

        // mode (u32) at offset 24
        let mode = stat_buf.st_mode as u32;
        std::ptr::copy_nonoverlapping(mode.to_ne_bytes().as_ptr(), out_buf.add(24), 4);

        // file_type (u32) at offset 28
        std::ptr::copy_nonoverlapping(file_type.to_ne_bytes().as_ptr(), out_buf.add(28), 4);
    }

    status.status = 0;
    status.error_code = 0;
}

/// Get file metadata via file descriptor (fstat)
///
/// Same as fold_posix_stat but operates on an open file descriptor.
///
/// # Safety
/// Caller must provide valid fd and buffer pointers
#[no_mangle]
pub extern "C" fn fold_posix_fstat(fd: RawFd, out_buf: *mut u8, out_status: *mut StatusResult) {
    if out_status.is_null() || out_buf.is_null() {
        return;
    }

    let status = unsafe { &mut *out_status };

    // Call fstat
    let mut stat_buf: libc::stat = unsafe { std::mem::zeroed() };
    let ret = unsafe { libc::fstat(fd, &mut stat_buf) };

    if ret < 0 {
        status.status = 1;
        status.error_code = unsafe { *libc::__errno_location() };
        return;
    }

    // Determine file type
    let file_type = if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFREG {
        FILE_TYPE_REGULAR
    } else if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFDIR {
        FILE_TYPE_DIRECTORY
    } else if (stat_buf.st_mode & libc::S_IFMT) == libc::S_IFLNK {
        FILE_TYPE_SYMLINK
    } else {
        FILE_TYPE_OTHER
    };

    // Write to output buffer (same layout as fold_posix_stat)
    unsafe {
        let size = stat_buf.st_size as u64;
        std::ptr::copy_nonoverlapping(size.to_ne_bytes().as_ptr(), out_buf, 8);

        let mtime_sec = stat_buf.st_mtime as i64;
        std::ptr::copy_nonoverlapping(mtime_sec.to_ne_bytes().as_ptr(), out_buf.add(8), 8);

        let mtime_nsec = stat_buf.st_mtime_nsec as i64;
        std::ptr::copy_nonoverlapping(mtime_nsec.to_ne_bytes().as_ptr(), out_buf.add(16), 8);

        let mode = stat_buf.st_mode as u32;
        std::ptr::copy_nonoverlapping(mode.to_ne_bytes().as_ptr(), out_buf.add(24), 4);

        std::ptr::copy_nonoverlapping(file_type.to_ne_bytes().as_ptr(), out_buf.add(28), 4);
    }

    status.status = 0;
    status.error_code = 0;
}

// ====
// Constant accessors for Scheme
// ====

/// Get stat buffer size for Scheme
#[no_mangle]
pub extern "C" fn fold_posix_stat_buffer_size() -> usize {
    STAT_BUFFER_SIZE
}

/// Get file type constant: regular file
#[no_mangle]
pub extern "C" fn fold_posix_file_type_regular() -> u32 {
    FILE_TYPE_REGULAR
}

/// Get file type constant: directory
#[no_mangle]
pub extern "C" fn fold_posix_file_type_directory() -> u32 {
    FILE_TYPE_DIRECTORY
}

/// Get file type constant: symlink
#[no_mangle]
pub extern "C" fn fold_posix_file_type_symlink() -> u32 {
    FILE_TYPE_SYMLINK
}

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

    #[test]
    fn test_fsync_fdatasync() {
        let path = "/tmp/fold_fsync_test.txt";
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

        // Write some data using libc
        let data = b"test data for fsync";
        unsafe {
            libc::write(fd, data.as_ptr() as *const libc::c_void, data.len());
        }

        // Test fdatasync (faster, preferred)
        let mut sync_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_fdatasync(fd, &mut sync_result);
        assert_eq!(sync_result.status, 0);

        // Test fsync
        let mut fsync_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_fsync(fd, &mut fsync_result);
        assert_eq!(fsync_result.status, 0);

        // Close
        let mut close_result = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_close(fd, &mut close_result);

        // Clean up
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_stat() {
        let path = "/tmp/fold_stat_test.txt";
        let path_bytes = path.as_bytes();

        // Create file with known content
        let content = "hello stat test";
        fs::write(path, content).unwrap();

        // Allocate stat buffer
        let mut stat_buf = [0u8; STAT_BUFFER_SIZE];
        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        fold_posix_stat(
            path_bytes.as_ptr(),
            path_bytes.len(),
            stat_buf.as_mut_ptr(),
            &mut status,
        );
        assert_eq!(status.status, 0);

        // Check size (offset 0, u64)
        let size = u64::from_ne_bytes(stat_buf[0..8].try_into().unwrap());
        assert_eq!(size, content.len() as u64);

        // Check mtime_sec is reasonable (offset 8, i64)
        let mtime_sec = i64::from_ne_bytes(stat_buf[8..16].try_into().unwrap());
        assert!(mtime_sec > 1700000000); // After 2023

        // Check file_type is regular (offset 28, u32)
        let file_type = u32::from_ne_bytes(stat_buf[28..32].try_into().unwrap());
        assert_eq!(file_type, FILE_TYPE_REGULAR);

        // Clean up
        let _ = fs::remove_file(path);
    }

    #[test]
    fn test_stat_directory() {
        let path = "/tmp";
        let path_bytes = path.as_bytes();

        let mut stat_buf = [0u8; STAT_BUFFER_SIZE];
        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        fold_posix_stat(
            path_bytes.as_ptr(),
            path_bytes.len(),
            stat_buf.as_mut_ptr(),
            &mut status,
        );
        assert_eq!(status.status, 0);

        // Check file_type is directory
        let file_type = u32::from_ne_bytes(stat_buf[28..32].try_into().unwrap());
        assert_eq!(file_type, FILE_TYPE_DIRECTORY);
    }

    #[test]
    fn test_stat_nonexistent() {
        let path = "/tmp/this_file_should_not_exist_fold_test.txt";
        let path_bytes = path.as_bytes();

        let mut stat_buf = [0u8; STAT_BUFFER_SIZE];
        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };

        fold_posix_stat(
            path_bytes.as_ptr(),
            path_bytes.len(),
            stat_buf.as_mut_ptr(),
            &mut status,
        );
        assert_eq!(status.status, 1);
        assert_eq!(status.error_code, libc::ENOENT);
    }

    #[test]
    fn test_fstat() {
        let path = "/tmp/fold_fstat_test.txt";
        let path_bytes = path.as_bytes();

        // Create file with known content
        let content = "hello fstat test";
        fs::write(path, content).unwrap();

        // Open file
        let mut open_result = IntResult {
            status: 0,
            value: 0,
            error_code: 0,
        };
        fold_posix_open(
            path_bytes.as_ptr(),
            path_bytes.len(),
            O_RDONLY,
            0,
            &mut open_result,
        );
        assert_eq!(open_result.status, 0);
        let fd = open_result.value;

        // fstat
        let mut stat_buf = [0u8; STAT_BUFFER_SIZE];
        let mut status = StatusResult {
            status: 0,
            error_code: 0,
        };
        fold_posix_fstat(fd, stat_buf.as_mut_ptr(), &mut status);
        assert_eq!(status.status, 0);

        // Check size
        let size = u64::from_ne_bytes(stat_buf[0..8].try_into().unwrap());
        assert_eq!(size, content.len() as u64);

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
